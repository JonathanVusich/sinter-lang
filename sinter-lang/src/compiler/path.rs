use itertools::Itertools;
use lasso::Spur;
use radix_trie::TrieKey;
use std::borrow::Borrow;
use std::collections::VecDeque;
use std::fmt::Debug;
use std::hash::Hash;
use std::ops::{Deref, DerefMut};

use serde::de::DeserializeOwned;
use serde::{Deserialize, Serialize};

use nibble_vec::Nibblet;

use crate::compiler::ast::{Ident, IdentType, QualifiedIdent, Segment};
use crate::compiler::tokens::tokenized_file::Span;
use crate::compiler::types::InternedStr;

use crate::gc::block::BLOCK_SIZE;

#[derive(PartialEq, Eq, Default, Hash, Debug, Clone, Serialize, Deserialize)]
pub struct ModulePath {
    module_path: VecDeque<InternedStr>,
}

impl ModulePath {
    pub fn from_iter<T: IntoIterator<Item = InternedStr>>(module_path: T) -> Self {
        Self {
            module_path: VecDeque::from_iter(module_path),
        }
    }

    pub fn concat(&self, mut other: Self) -> Self {
        self.module_path
            .iter()
            .rev()
            .for_each(|seg| other.module_path.push_front(*seg));
        other
    }

    pub fn front(&self) -> Option<InternedStr> {
        self.module_path.front().copied()
    }

    pub fn back(&self) -> Option<InternedStr> {
        self.module_path.back().copied()
    }

    pub fn pop_front(&mut self) -> Option<InternedStr> {
        self.module_path.pop_front()
    }

    pub fn pop_back(&mut self) -> Option<InternedStr> {
        self.module_path.pop_back()
    }

    fn push_front(&mut self, value: InternedStr) {
        self.module_path.push_front(value)
    }

    fn push_back(&mut self, value: InternedStr) {
        self.module_path.push_back(value)
    }

    fn append(&mut self, deque: VecDeque<InternedStr>) {
        self.module_path.extend(deque)
    }
}

impl TrieKey for ModulePath {
    #[inline]
    fn encode(&self) -> Nibblet {
        let mut nibblet = Nibblet::new();
        for seg in self.module_path.iter().copied() {
            let bytes = Spur::from(seg).into_inner().get().to_be_bytes();
            for byte in bytes {
                nibblet.push(byte);
            }
        }
        nibblet
    }
}

impl<T> From<T> for ModulePath
where
    T: Borrow<QualifiedIdent>,
{
    fn from(value: T) -> Self {
        let qualified_ident = value.borrow();
        let inner = qualified_ident
            .idents
            .iter()
            .map(|ident| ident.ident)
            .collect();
        Self { module_path: inner }
    }
}
