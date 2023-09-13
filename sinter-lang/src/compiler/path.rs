use lasso::Spur;
use radix_trie::TrieKey;
use std::borrow::Borrow;
use std::collections::VecDeque;
use std::fmt::Debug;
use std::hash::Hash;
use std::ops::{Deref, DerefMut};
use itertools::Itertools;

use serde::de::DeserializeOwned;
use serde::{Deserialize, Serialize};

use nibble_vec::Nibblet;

use crate::compiler::ast::{Ident, IdentType, QualifiedIdent, Segment};
use crate::compiler::tokens::tokenized_file::Span;
use crate::compiler::types::InternedStr;

use crate::gc::block::BLOCK_SIZE;

#[derive(PartialEq, Eq, Hash, Debug, Clone, Serialize, Deserialize)]
pub enum ModulePath {
    Empty,
    Single(InternedStr),
    Multiple(VecDeque<InternedStr>),
}

impl Default for ModulePath {
    fn default() -> Self {
        Self::Empty
    }
}

impl ModulePath {
    pub fn from_iter<T: IntoIterator<Item = InternedStr>>(module_path: T) -> Self {
        Self::Multiple(VecDeque::from_iter(module_path))
    }

    pub fn concat(&self, mut other: ModulePath) -> ModulePath {
        match &mut other {
            ModulePath::Empty => self.clone(),
            ModulePath::Single(istr) => {
                let mut clone = self.clone();
                clone.push_back(*istr);
                clone
            }
            ModulePath::Multiple(deque) => {
                // Reuse already allocated deque
                match self {
                    ModulePath::Empty => other,
                    ModulePath::Single(istr) => {
                        deque.push_front(*istr);
                        other
                    }
                    ModulePath::Multiple(front_deque) => {
                        for val in front_deque.iter().rev().copied() {
                            deque.push_front(val);
                        }
                        other
                    }
                }
            },
        }
    }

    pub fn front(&self) -> Option<InternedStr> {
        match self {
            ModulePath::Empty => None,
            ModulePath::Single(istr) => Some(*istr),
            ModulePath::Multiple(deque) => deque.front().copied(),
        }
    }

    pub fn back(&self) -> Option<InternedStr> {
        match self {
            ModulePath::Empty => None,
            ModulePath::Single(istr) => Some(*istr),
            ModulePath::Multiple(deque) => deque.back().copied(),
        }
    }

    pub fn pop_front(&mut self) -> Option<InternedStr> {
        match self {
            ModulePath::Empty => None,
            ModulePath::Single(istr) => {
                let ret_val = Some(*istr);
                *self = ModulePath::Empty;
                ret_val
            }
            ModulePath::Multiple(deque) => deque.pop_front(),
        }
    }

    pub fn pop_back(&mut self) -> Option<InternedStr> {
        match self {
            ModulePath::Empty => None,
            ModulePath::Single(istr) => {
                let ret_val = Some(*istr);
                *self = ModulePath::Empty;
                ret_val
            }
            ModulePath::Multiple(deque) => deque.pop_back(),
        }
    }

    fn push_front(&mut self, value: InternedStr) {
        match self {
            ModulePath::Empty => *self = ModulePath::Single(value),
            ModulePath::Single(istr) => {
                *self = ModulePath::Multiple(VecDeque::from_iter([value, *istr]))
            }
            ModulePath::Multiple(deque) => deque.push_front(value),
        }
    }

    fn push_back(&mut self, value: InternedStr) {
        match self {
            ModulePath::Empty => *self = ModulePath::Single(value),
            ModulePath::Single(istr) => {
                *self = ModulePath::Multiple(VecDeque::from_iter([*istr, value]))
            }
            ModulePath::Multiple(deque) => deque.push_back(value),
        }
    }

    fn append(&mut self, mut deque: VecDeque<InternedStr>) {
        match self {
            ModulePath::Empty => *self = ModulePath::Multiple(deque),
            ModulePath::Single(istr) => {
                deque.push_front(*istr);
                *self = ModulePath::Multiple(deque)
            }
            ModulePath::Multiple(curr_deque) => {
                curr_deque.extend(deque);
            }
        }
    }
}

impl TrieKey for ModulePath {
    #[inline]
    fn encode(&self) -> Nibblet {
        match self {
            ModulePath::Empty => Nibblet::new(),
            ModulePath::Single(istr) => {
                let bytes = Spur::from(*istr).into_inner().get().to_be_bytes();
                Nibblet::from(bytes.as_slice())
            }
            ModulePath::Multiple(deque) => {
                let mut nibblet = Nibblet::new();
                for seg in deque.iter().copied() {
                    let bytes = Spur::from(seg).into_inner().get().to_be_bytes();
                    for byte in bytes {
                        nibblet.push(byte);
                    }
                }
                nibblet
            }
        }
    }
}

impl<T> From<T> for ModulePath
where
    T: Borrow<QualifiedIdent>,
{
    fn from(value: T) -> Self {
        let qualified_ident = value.borrow();
        if qualified_ident.idents.len() == 1 {
            Self::Single(qualified_ident.idents.get(0).unwrap().ident)
        } else {
            Self::Multiple(
                qualified_ident
                    .idents
                    .iter()
                    .map(|ident| ident.ident)
                    .collect(),
            )
        }
    }
}
