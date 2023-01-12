use std::cell::RefCell;
use std::collections::hash_map::RandomState;
use std::collections::HashMap;
use std::fmt::{Debug, Formatter, Pointer};
use std::hash::{BuildHasher, Hash, Hasher};
use std::marker::PhantomData;
use std::num::NonZeroUsize;
use std::ops::Deref;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering::SeqCst;
use std::sync::Mutex;
use std::vec::IntoIter;
use bumpalo_herd::Herd;
use dashmap::{DashMap, DashSet};
use serde::{de, Deserialize, Deserializer, ser, Serialize, Serializer};
use serde::ser::Error as SerializeError;
use serde::de::Error as DeserializeError;

const DEFAULT_BUCKET_CAPACITY: usize = 512;

#[derive(Debug, Eq, PartialEq, Hash, Ord, PartialOrd, Copy, Clone, Serialize, Deserialize)]
#[repr(transparent)]
pub struct Key {
    val: NonZeroUsize,
}

impl Key {
    pub fn new(val: NonZeroUsize) -> Self {
        Self { val }
    }
}


#[derive(Debug)]
pub struct Interner<'interned, T: Eq + Hash, S: Clone + BuildHasher> {
    interner: DashMap<&'interned T, Key, S>,
    resolver: DashMap<Key, &'interned T, S>,
    counter: AtomicUsize,
    arena: Arena,
}

impl<'interned, T, S> Interner<'interned, T, S>
    where T: Sized + PartialEq + Eq + Hash + Debug + Clone,
          S: Clone + BuildHasher + Default {

    pub fn new(bucket_capacity: usize) -> Self {
        Self {
            interner: DashMap::with_capacity_and_hasher(DEFAULT_BUCKET_CAPACITY, S::default()),
            resolver: DashMap::with_capacity_and_hasher(DEFAULT_BUCKET_CAPACITY, S::default()),
            counter: AtomicUsize::new(1),
            arena: Mutex::new(Buckets::new(bucket_capacity)),
        }
    }

    pub fn intern(&self, item: T) -> Key {
        if let Some(key) = self.interner.get(&item) {
            *key.value()
        } else {
            let arena = self.arena.lock().unwrap();
            let index = self.counter.fetch_add(1, SeqCst);
            // This should be safe because the buckets are never reallocated.
            // The reference should be stable for the lifetime of the interner.
            let val_ref = arena.insert(item);
            let key = unsafe {
                Key::new(NonZeroUsize::new_unchecked(index))
            };
            self.interner.insert(val_ref, key);
            self.resolver.insert(key, val_ref);
            key
        }
    }

    pub fn resolve(&self, key: &Key) -> Option<&'interned T> {
        self.resolver.get(key).map(|entry| *entry.value())
    }
}

impl<'interned, T> Default for Interner<'interned, T, RandomState>
    where T: Sized + PartialEq + Eq + Hash + Debug + Clone {
    fn default() -> Self {
        Interner::new(DEFAULT_BUCKET_CAPACITY)
    }
}

impl<T, S> PartialEq<Self> for Interner<'_, T, S>
    where T: Sized + PartialEq + Eq + Hash + Debug,
          S: Clone + BuildHasher {

    fn eq(&self, other: &Self) -> bool {
        self.resolver.len() == other.resolver.len()
            && self.resolver.iter().all(|left| {
            other.resolver
                .get(left.key())
                .map(|s| s.value() == left.value())
                == Some(true)
        })
    }
}

impl<T, S> Eq for Interner<'_, T, S>
    where T: Sized + PartialEq + Eq + Hash + Debug,
          S: Clone + BuildHasher {}

impl<T, H> Serialize for Interner<'_, T, H>
    where T: Sized + PartialEq + Eq + Hash + Debug + Serialize + Clone,
          H: Clone + BuildHasher {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer {
        // Serialize all of self as a `Vec<K>`
        if let Ok(buckets) = self.arena.lock() {
            buckets.flatten().serialize(serializer)
        } else {
            Err(SerializeError::custom("Serialization failure"))
        }
    }
}

impl<'de, T, S> Deserialize<'de> for Interner<'de, T, S>
    where
        T: Sized + PartialEq + Eq + Hash + Debug + Deserialize<'de> + Clone,
        S: BuildHasher + Clone + Default,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: Deserializer<'de>,
    {
        let deserialized_values: Vec<T> = Vec::deserialize(deserializer)?;
        let counter = AtomicUsize::new(deserialized_values.len() + 1);
        let hasher = S::default();
        let interner = DashMap::with_capacity_and_hasher(deserialized_values.len(), hasher.clone());
        let resolver = DashMap::with_capacity_and_hasher(deserialized_values.len(), hasher);
        let buckets = Buckets::default();
        for (index, item) in deserialized_values.into_iter().enumerate() {
            let key = Key::new(NonZeroUsize::new(index + 1).unwrap());
            let val = buckets.insert(item);
            interner.insert(val, key);
            resolver.insert(key, val);
        }
        Ok(Self {
            interner,
            resolver,
            counter,
            arena: Mutex::new(buckets)
        })
    }
}

#[derive(Debug)]
struct Buckets<'a, T> {
    buckets: RefCell<Vec<Bucket<'a, T>>>,
    bucket_capacity: usize,
}

impl<'a, T> Buckets<'a, T> where T: Clone {
    pub fn new(bucket_capacity: usize) -> Self {
        Self {
            buckets: RefCell::new(Vec::new()),
            bucket_capacity,
        }
    }

    pub fn insert(&self, item: T) -> &'a T {
        let mut buckets = self.buckets.borrow_mut();
        let has_space = buckets.last().map(|item| !item.full()).unwrap_or(false);
        if !has_space {
            buckets.push(Bucket::new(self.bucket_capacity));
        }
        buckets.last_mut().map(|bucket| bucket.push(item)).unwrap()
    }

    fn flatten(&self) -> Vec<T> {
        self.buckets.borrow().iter().flat_map(|bucket| bucket.buffer.clone()).collect()
    }
}

impl<'a, T> Default for Buckets<'a, T> where T: Clone {
    fn default() -> Self {
        Buckets::new(512)
    }
}

#[derive(Debug, Clone)]
#[repr(transparent)]
struct Bucket<'a, T> {
    buffer: Vec<T>,
    marker: PhantomData<&'a T>,
}

impl<'a, T> Bucket<'a, T> {

    fn new(capacity: usize) -> Self {
        Self {
            buffer: Vec::with_capacity(capacity),
            marker: PhantomData::default(),
        }
    }

    fn full(&self) -> bool {
        self.buffer.capacity() <= self.buffer.len()
    }

    fn push(&mut self, item: T) -> &'a T {
        debug_assert!(!self.full());
        self.buffer.push(item);
        unsafe {
            std::mem::transmute(self.buffer.last().unwrap())
        }
    }
}

impl<'a, T> Default for Bucket<'a, T> {
    fn default() -> Self {
        Bucket::new(512)
    }
}


mod tests {
    use std::collections::hash_map::RandomState;
    use std::num::NonZeroUsize;
    use crate::compiler::interner::{Interner, Key};

    #[test]
    pub fn sanity_check() {
        let interner = Interner::default();
        let key = interner.intern(123i128);

        assert_eq!(Key::new(NonZeroUsize::new(1).unwrap()), key);

        let ptr = interner.resolve(&key).unwrap();
        assert_eq!(123, *ptr);
    }

    #[test]
    pub fn stable_references() {
        let interner = Interner::<usize, RandomState>::new(8);
        let keys = (0..128).into_iter().map(|num| interner.intern(num)).collect::<Vec<Key>>();
        for (index, key) in keys.iter().enumerate() {
            assert_eq!(index, *interner.resolve(key).unwrap())
        }
    }
}
