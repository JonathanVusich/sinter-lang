use std::collections::{HashMap, HashSet};
use std::collections::hash_map::Entry;
use std::hash::Hash;
use std::marker::PhantomData;
use std::ops::Deref;
use crate::compiler::types::types::{Type, TypeDescription};
use crate::pointers::pointer::Pointer;

pub struct Arena<'ctx, T: Eq + Hash> {
    map: HashMap<T, ()>,
    marker: PhantomData<&'ctx T>,
}

#[repr(transparent)]
pub struct Interned<'ctx, T: Eq + Hash> {
    ptr: &'ctx T,
}

impl<'ctx, T> Interned<'ctx, T> where T: Eq + Hash {

    pub fn new(ptr: &'ctx T) -> Self {
        Self {
            ptr,
        }
    }
}

impl<'ctx, T> Deref for Interned<'ctx, T> where T: Eq + Hash {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.ptr
    }
}

impl<'ctx, T> Arena<'ctx, T> where T: Eq + Hash {

    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    pub fn intern(&mut self, value: T) -> Interned<'ctx, T> {
        match self.map.entry(value) {
            Entry::Occupied(entry) => {
                Interned::new(entry.key())
            }
            Entry::Vacant(entry) => {
                entry.insert_entry(());
                Interned::new(&value)
            }
        }
    }
}

mod tests {
    use crate::util::arena::Arena;

    #[test]
    pub fn interning() {
        let mut arena: Arena<usize> = Arena::new();

        let interned_val = arena.intern(100);
        assert_eq!(interned_val, 100);
    }
}