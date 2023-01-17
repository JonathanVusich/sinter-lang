use std::cell::RefCell;
use std::collections::hash_map::{Entry, RandomState};
use std::collections::{HashMap, HashSet};
use std::fmt::{Debug, Formatter, Pointer};
use std::hash::{BuildHasher, Hash, Hasher};
use std::marker::PhantomData;
use std::mem;
use std::num::NonZeroUsize;
use std::ops::Deref;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering::SeqCst;
use std::sync::Mutex;
use std::vec::IntoIter;
use dashmap::{DashMap, DashSet};
use serde::{de, Deserialize, Deserializer, ser, Serialize, Serializer};
use serde::ser::Error as SerializeError;
use serde::de::Error as DeserializeError;
use typed_arena::Arena;

const DEFAULT_CAPACITY: usize = 512;

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

pub struct Interner<T: 'static> {
    interner: HashMap<&'static T, Key>,
    resolver: HashMap<Key, &'static T>,
    counter: NonZeroUsize,
    arena: Arena<T>,
}

impl<T> Interner<T>
    where T: PartialEq + Eq + Hash + Debug {

    pub fn new(bucket_capacity: usize) -> Self {
        Self {
            interner: HashMap::default(),
            resolver: HashMap::default(),
            counter: NonZeroUsize::new(1).unwrap(),
            arena: Arena::new(),
        }
    }

    pub fn intern(&mut self, item: T) -> Key {
        let entry = self.interner.entry(&item);
        match entry {
            Entry::Occupied(entry) => *entry.get(),
            Entry::Vacant(entry) => {
                let key = Key::new(self.counter);
                entry.insert(key);
                self.counter = self.counter.checked_add(1).unwrap();
                key
            }
        }
    }

    pub fn resolve(&self, item: &Key) -> Option<&T> {
        self.resolver.get(&item).copied()
    }

    unsafe fn alloc(&mut self, item: T) -> &'static T {
        let interned_ref = self.arena.alloc(item);
        interned_ref
    }
}

impl<T> PartialEq<Self> for Interner<T>
    where T: PartialEq + Eq + Hash + Debug {

    fn eq(&self, other: &Self) -> bool {
        self.interner.eq(&other.interner)
    }
}

impl<T: Debug> Debug for Interner<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl<T> Eq for Interner<T>
    where T: PartialEq + Eq + Hash + Debug {}

impl<T> Default for Interner<T>
    where T: PartialEq + Eq + Hash + Debug {
    fn default() -> Self {
        Self::new(DEFAULT_CAPACITY)
    }
}

impl<T> Serialize for Interner<T>
    where T: PartialEq + Eq + Hash + Debug + Serialize {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
        where S: Serializer {
        // Serialize all of self as a `Vec<K>`
        todo!()
    }
}

impl<'de, T> Deserialize<'de> for Interner<T>
    where T: PartialEq + Eq + Hash + Debug + Deserialize<'de> + Clone
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where D: Deserializer<'de>,
    {
        let deserialized_values: Vec<T> = Vec::deserialize(deserializer)?;
        let mut interner = Self::new(DEFAULT_CAPACITY);
        for item in deserialized_values.into_iter() {
            interner.intern(item);
        }
        Ok(interner)
    }
}


mod tests {
    use std::collections::hash_map::RandomState;
    use std::num::NonZeroUsize;
    use crate::compiler::interner::{Interner, Key};

    #[test]
    pub fn sanity_check() {
        let mut interner = Interner::default();
        let key = interner.intern(123i128);

        assert_eq!(Key::new(NonZeroUsize::new(1).unwrap()), key);

        let ptr = interner.resolve(&key).unwrap();
        assert_eq!(123, *ptr);
    }

    #[test]
    pub fn stable_references() {
        let mut interner = Interner::<usize>::new(8);
        let keys = (0..128).into_iter().map(|num| interner.intern(num)).collect::<Vec<Key>>();
        for (index, key) in keys.iter().enumerate() {
            assert_eq!(index, *interner.resolve(key).unwrap())
        }
    }
}