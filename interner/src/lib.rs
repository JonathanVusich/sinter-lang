#![feature(hash_raw_entry)]

use bumpalo::Bump;
use lasso::{Rodeo, Spur};
use serde::{Deserialize, Serialize};
use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::Hash;

#[repr(transparent)]
#[derive(
    Copy, Clone, PartialEq, Eq, Default, Debug, PartialOrd, Ord, Hash, Serialize, Deserialize,
)]
#[serde(transparent)]
pub struct InternedStr {
    str: Spur,
}

impl From<InternedStr> for Spur {
    fn from(interned_str: InternedStr) -> Self {
        interned_str.str
    }
}

impl From<Spur> for InternedStr {
    fn from(value: Spur) -> Self {
        InternedStr { str: value }
    }
}

impl InternedStr {
    pub fn new(str: Spur) -> Self {
        Self { str }
    }

    pub fn into_inner(self) -> u32 {
        self.str.into_inner().get()
    }
}

#[derive(PartialEq, Debug, Default, Serialize, Deserialize)]
#[serde(transparent)]
pub struct StringInterner {
    interner: Rodeo,
}

impl StringInterner {
    pub fn intern(&mut self, str: &str) -> InternedStr {
        self.interner.get_or_intern(str).into()
    }
    pub fn resolve(&self, str: InternedStr) -> &str {
        self.interner.resolve(&str.into())
    }
}

#[derive(Debug)]
pub struct Interner<'a, T> {
    allocator: &'a Bump,
    interned: RefCell<HashMap<&'a T, ()>>,
}

impl<'a, T> Interner<'a, T>
where
    T: Eq + Hash,
{
    pub fn intern(&self, val: T) -> &'a T {
        let mut set = self.interned.borrow_mut();
        set.raw_entry_mut()
            .from_key(&val)
            .or_insert_with(|| (self.allocator.alloc(val), ()))
            .0
    }
}
