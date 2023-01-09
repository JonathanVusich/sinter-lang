use std::collections::hash_map::RandomState;
use std::fmt::{Debug, Formatter, Pointer};
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::num::NonZeroUsize;
use std::ops::Deref;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering::SeqCst;
use std::sync::Mutex;
use dashmap::{DashMap, DashSet};
use typed_arena::Arena;

#[derive(Debug, Eq, PartialEq, Hash, Ord, PartialOrd, Copy, Clone)]
#[repr(transparent)]
pub struct Key {
    val: NonZeroUsize,
}

impl Key {
    pub fn new(val: NonZeroUsize) -> Self {
        Self { val }
    }
}

pub struct Interner<'interned, T: Sized + PartialEq + Eq + Hash + Debug, S = RandomState> {
    interner: DashMap<&'interned T, Key, S>,
    resolver: DashMap<Key, &'interned T, S>,
    counter: AtomicUsize,
    arena: Mutex<Arena<T>>,
}

impl<'interned, T: Sized + PartialEq + Eq + Hash + Debug> Interner<'interned, T> {

    pub fn new() -> Self {
        Self {
            interner: DashMap::new(),
            resolver: DashMap::new(),
            counter: AtomicUsize::new(1),
            arena: Mutex::new(Arena::new()),
        }
    }
    fn intern(&self, item: T) -> Key {
        if let Some(key) = self.interner.get(&item) {
            *key.value()
        } else {
            let arena = self.arena.lock().unwrap();
            let index = self.counter.fetch_add(1, SeqCst);
            let val_ref: &'interned T = unsafe { std::mem::transmute(arena.alloc(item)) };
            let key = unsafe {
                Key::new(NonZeroUsize::new_unchecked(index))
            };
            self.interner.insert(val_ref, key);
            self.resolver.insert(key, val_ref);
            key
        }
    }

    fn resolve(&self, key: Key) -> Option<&'interned T> {
        self.resolver.get(&key).map(|entry| *entry.value())
    }
}

impl<'a, T: Sized + PartialEq + Eq + Hash + Debug> Interner<'a, T> {}

mod tests {
    use std::num::NonZeroUsize;
    use crate::compiler::interner::{Interner, Key};

    #[test]
    pub fn sanity_check() {
        let interner = Interner::<i128>::new();
        let key = interner.intern(123);

        assert_eq!(Key::new(NonZeroUsize::new(1).unwrap()), key);

        let ptr = interner.resolve(key).unwrap();
        assert_eq!(123, *ptr);
    }
}
