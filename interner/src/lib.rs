use serde::{Serialize, Serializer};

use std::borrow::Borrow;

use std::borrow::Cow;
use std::cell::RefCell;
use std::cmp::Ordering;

use std::collections::HashSet;

use std::ffi::OsStr;

use std::fmt;
use std::fmt::Formatter;
use std::hash::{BuildHasher, Hash, Hasher};

use std::ops::Deref;

use std::path::Path;

use std::ptr;

use std::str;

use std::sync::Mutex;

use hashbrown::hash_table::Entry;
use hashbrown::DefaultHashBuilder;
use hashbrown::HashTable;
use serde::de::Visitor;
use std::sync::OnceLock;

fn interned_storage() -> std::sync::MutexGuard<'static, HashSet<&'static str>> {
    static STRING_CACHE: OnceLock<Mutex<HashSet<&'static str>>> = OnceLock::new();

    STRING_CACHE
        .get_or_init(|| {
            let out: HashSet<&'static str> = Default::default();
            Mutex::new(out)
        })
        .lock()
        .unwrap()
}

#[derive(Clone, Copy)]
pub struct InternedStr {
    inner: &'static str,
}

impl<'a> From<&'a str> for InternedStr {
    fn from(item: &'a str) -> Self {
        InternedStr::new(item)
    }
}

impl<'a> From<&'a String> for InternedStr {
    fn from(item: &'a String) -> Self {
        InternedStr::new(item)
    }
}

impl From<String> for InternedStr {
    fn from(item: String) -> Self {
        InternedStr::from_cow(item.into())
    }
}

impl PartialEq for InternedStr {
    fn eq(&self, other: &InternedStr) -> bool {
        ptr::eq(self.as_str(), other.as_str())
    }
}

impl PartialEq<str> for InternedStr {
    fn eq(&self, other: &str) -> bool {
        *self == other
    }
}

impl<'a> PartialEq<&'a str> for InternedStr {
    fn eq(&self, other: &&str) -> bool {
        **self == **other
    }
}

impl Eq for InternedStr {}

impl InternedStr {
    pub fn new(s: &str) -> InternedStr {
        InternedStr::from_cow(s.into())
    }

    fn from_cow(cs: Cow<str>) -> InternedStr {
        let mut cache = interned_storage();

        let s = cache.get(cs.as_ref()).copied().unwrap_or_else(|| {
            let s = cs.into_owned().leak();

            cache.insert(s);

            s
        });

        InternedStr { inner: s }
    }

    pub fn as_str(&self) -> &'static str {
        self.inner
    }
}

impl Deref for InternedStr {
    type Target = str;

    fn deref(&self) -> &'static str {
        self.as_str()
    }
}

impl AsRef<str> for InternedStr {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl AsRef<OsStr> for InternedStr {
    fn as_ref(&self) -> &OsStr {
        self.as_str().as_ref()
    }
}

impl AsRef<Path> for InternedStr {
    fn as_ref(&self) -> &Path {
        self.as_str().as_ref()
    }
}

impl Hash for InternedStr {
    // N.B., we can't implement this as `identity(self).hash(state)`,

    // because we use this for on-disk fingerprints and so need

    // stability across Cargo invocations.

    fn hash<H: Hasher>(&self, state: &mut H) {
        self.as_str().hash(state);
    }
}

impl Borrow<str> for InternedStr {
    // If we implement Hash as `identity(self).hash(state)`,

    // then this will need to be removed.

    fn borrow(&self) -> &str {
        self.as_str()
    }
}

impl fmt::Debug for InternedStr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self.as_str(), f)
    }
}

impl fmt::Display for InternedStr {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self.as_str(), f)
    }
}

impl Ord for InternedStr {
    fn cmp(&self, other: &InternedStr) -> Ordering {
        self.as_str().cmp(other.as_str())
    }
}

impl PartialOrd for InternedStr {
    fn partial_cmp(&self, other: &InternedStr) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Serialize for InternedStr {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(self.inner)
    }
}

#[derive(Debug)]
pub struct Interner<'a, T> {
    interned: RefCell<HashTable<&'a T>>,
}

impl<'a, T> Default for Interner<'a, T> {
    fn default() -> Self {
        Self {
            interned: RefCell::default(),
        }
    }
}

impl<'a, T> Interner<'a, T>
where
    T: Eq + Hash,
{
    pub fn intern<F: Fn(T) -> &'a T>(&self, val: T, alloc_fn: F) -> &'a T {
        let mut interned = self.interned.borrow_mut();
        let hash_fn = DefaultHashBuilder::default();
        let hash = hash_fn.hash_one(&val);

        let entry = interned.entry(hash, |v| (**v) == val, |val| hash_fn.hash_one(val));
        match entry {
            Entry::Occupied(occupied) => occupied.get(),
            Entry::Vacant(vacant) => {
                let interned_val = alloc_fn(val);
                vacant.insert(interned_val);
                interned_val
            }
        }
    }
}

impl<'de> serde::Deserialize<'de> for InternedStr {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct StringVisitor;

        impl<'de> Visitor<'de> for StringVisitor {
            type Value = InternedStr;

            fn expecting(&self, formatter: &mut Formatter) -> fmt::Result {
                formatter.write_str("a string")
            }

            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E> {
                Ok(InternedStr::from(v))
            }
        }
        deserializer.deserialize_str(StringVisitor)
    }
}
