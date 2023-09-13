use lasso::Spur;
use radix_trie::TrieKey;
use std::borrow::Borrow;
use std::collections::VecDeque;
use std::ops::{Deref, DerefMut};
use std::path::Path;

use serde::{Deserialize, Serialize};

use crate::compiler::ast::{Ident, IdentType, QualifiedIdent, Segment};
use crate::compiler::tokens::tokenized_file::Span;
use crate::compiler::types::InternedStr;

use crate::gc::block::BLOCK_SIZE;

#[derive(PartialEq, Eq, Hash, Default, Debug, Clone, Serialize, Deserialize)]
pub struct ModulePath {
    module_path: VecDeque<InternedStr>,
}

impl ModulePath {
    pub fn from_iter<T: IntoIterator<Item = InternedStr>>(module_path: T) -> Self {
        Self {
            module_path: VecDeque::from_iter(module_path),
        }
    }
}

impl Deref for ModulePath {
    type Target = VecDeque<InternedStr>;

    fn deref(&self) -> &Self::Target {
        &self.module_path
    }
}

impl DerefMut for ModulePath {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.module_path
    }
}

impl TrieKey for ModulePath {
    fn encode_bytes(&self) -> Vec<u8> {
        self.module_path
            .iter()
            .map(|ident| Spur::from(*ident).into_inner().get().to_be_bytes())
            .flatten()
            .collect()
    }
}

impl<T> From<T> for ModulePath
where
    T: Borrow<QualifiedIdent>,
{
    fn from(value: T) -> Self {
        let qualified_ident = value.borrow();
        Self {
            module_path: qualified_ident
                .idents
                .iter()
                .map(|ident| ident.ident)
                .collect(),
        }
    }
}
