use std::cell::RefCell;
use std::collections::hash_map::{Entry, RandomState};
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
use hashbrown::hash_map::{HashMap, RawEntryMut};
use serde::de::Error as DeserializeError;
use serde::ser::Error as SerializeError;
use serde::{de, ser, Deserialize, Deserializer, Serialize, Serializer};
use typed_arena::Arena;

const DEFAULT_CAPACITY: usize = 512;

#[derive(Debug, Eq, PartialEq, Hash, Ord, PartialOrd, Copy, Clone, Serialize, Deserialize)]
#[repr(transparent)]
pub struct Key {
    val: NonZeroUsize,
}

impl Key {
    pub unsafe fn new(index: usize) -> Self {
        let val = NonZeroUsize::new_unchecked(index + 1);
        Self { val }
    }
}

impl From<Key> for usize {
    fn from(value: Key) -> Self {
        value.val.get() - 1
    }
}

pub struct Interner<T, H = RandomState> {
    interner: HashMap<Key, (), ()>,
    hasher: H,
    values: Vec<T>,
}

impl<T, H> Interner<T, H>
where
    T: PartialEq + Eq + Hash + Debug,
    H: BuildHasher + Default,
{
    pub fn new() -> Self {
        Self {
            interner: HashMap::default(),
            hasher: H::default(),
            values: Vec::default(),
        }
    }

    pub fn with_hasher(hasher: H) -> Self {
        Self {
            interner: HashMap::default(),
            hasher,
            values: Vec::default(),
        }
    }

    pub fn intern(&mut self, item: T) -> Key {
        // Compute hash of item
        let mut hasher = self.hasher.build_hasher();
        item.hash(&mut hasher);
        let hash = hasher.finish();

        let entry = self.interner.raw_entry_mut().from_hash(hash, |key| {
            let val: usize = (*key).into();
            let interned = self.values.get(val).unwrap();
            item == *interned
        });

        let key = match entry {
            RawEntryMut::Occupied(entry) => *entry.into_key(),
            RawEntryMut::Vacant(entry) => {
                let key = unsafe { Key::new(self.values.len()) };

                self.values.push(item);

                entry.insert_with_hasher(hash, key, (), |key| {
                    let key_val: &T = self.values.get::<usize>((*key).into()).unwrap();

                    let mut state = self.hasher.build_hasher();
                    key_val.hash(&mut state);

                    state.finish()
                });
                key
            }
        };
        key
    }

    pub fn resolve(&self, item: Key) -> Option<&T> {
        self.values.get(usize::from(item))
    }
}

impl<T> PartialEq<Self> for Interner<T>
where
    T: PartialEq + Eq + Hash + Debug,
{
    fn eq(&self, other: &Self) -> bool {
        self.values == other.values
    }
}

impl<T: Debug> Debug for Interner<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl<T> Eq for Interner<T> where T: PartialEq + Eq + Hash + Debug {}

impl<T> Default for Interner<T, RandomState>
where
    T: PartialEq + Eq + Hash + Debug,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<T> Serialize for Interner<T>
where
    T: PartialEq + Eq + Hash + Debug + Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        // Serialize all of self as a `Vec<K>`
        self.values.serialize(serializer)
    }
}

impl<'de, T> Deserialize<'de> for Interner<T>
where
    T: PartialEq + Eq + Hash + Debug + Deserialize<'de> + Clone,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let deserialized_values: Vec<T> = Vec::deserialize(deserializer)?;
        let mut interner = Self::new();
        for item in deserialized_values.into_iter() {
            interner.intern(item);
        }
        Ok(interner)
    }
}

mod tests {
    use rustc_hash::FxHasher;
    use std::collections::hash_map::RandomState;
    use std::hash::{BuildHasherDefault, Hasher};
    use std::num::NonZeroUsize;

    use snap::snapshot;

    use crate::compiler::interner::{Interner, Key};
    use crate::compiler::SeedableHasher;

    #[test]
    pub fn sanity_check() {
        let mut interner = Interner::default();
        let key = interner.intern(123i128);

        let expected_key = unsafe { Key::new(0) };

        assert_eq!(expected_key, key);

        let ptr = interner.resolve(key).unwrap();
        assert_eq!(123, *ptr);
    }

    #[test]
    pub fn stable_references() {
        let mut interner = Interner::default();
        let keys = (0..128)
            .into_iter()
            .map(|num| interner.intern(num))
            .collect::<Vec<Key>>();
        for (index, key) in keys.iter().enumerate() {
            assert_eq!(index, *interner.resolve(*key).unwrap())
        }
    }

    #[test]
    #[snapshot]
    pub fn snapshot_serialization() -> Interner<usize> {
        let mut interner = Interner::default();
        for i in 0..10 {
            interner.intern(i);
        }
        interner
    }

    #[test]
    pub fn same_hash() {
        struct SameHash {}
        impl Hasher for SameHash {
            fn finish(&self) -> u64 {
                0
            }

            fn write(&mut self, bytes: &[u8]) {}
        }
        impl Default for SameHash {
            fn default() -> Self {
                SameHash {}
            }
        }

        let mut interner = Interner::<usize, BuildHasherDefault<SameHash>>::new();
        let first_key = interner.intern(128);
        let second_key = interner.intern(256);
        assert_ne!(first_key, second_key);

        assert_eq!(128, *interner.resolve(first_key).unwrap());
        assert_eq!(256, *interner.resolve(second_key).unwrap());
    }
}
