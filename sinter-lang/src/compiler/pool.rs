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

pub struct Interner<'a, T: Sized + PartialEq + Eq  + Hash + Debug, S = RandomState> {
    interner: DashMap<&'a T, Key, S>,
    resolver: DashMap<Key, &'a T, S>,
    counter: AtomicUsize,
    arena: Mutex<Arena<T>>,
}

impl<'a, T: Sized + PartialEq + Eq + Hash + Debug> Interner<'a, T> {

    pub fn new() -> Self {
        Self {
            interner: DashMap::new(),
            resolver: DashMap::new(),
            counter: AtomicUsize::new(1),
            arena: Mutex::new(Arena::new()),
        }
    }

    pub fn get_or_intern(&'a self, item: T) -> Key {
        if let Some(key) = self.interner.get(&item) {
            *key.value()
        } else {
            let guard = self.arena.lock().unwrap();
            let val : &'a mut T = guard.alloc(item);
            let index = self.counter.fetch_add(1, SeqCst);
            let key = unsafe {
                Key::new(NonZeroUsize::new_unchecked(index))
            };
            self.interner.insert(val, key);
            self.resolver.insert(key, val);
            key
        }
    }

    pub fn resolve(&'a self, key: Key) -> Option<&'a T> {
        self.resolver.get(&key).map(|reff| reff.value().clone())
    }
}
