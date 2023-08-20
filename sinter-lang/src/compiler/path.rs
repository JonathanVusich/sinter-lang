use std::borrow::Borrow;
use std::collections::VecDeque;
use std::ops::Deref;
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

    pub fn as_crate_local_ident(&self) -> QualifiedIdent {
        QualifiedIdent::new(
            IdentType::Crate,
            self.module_path
                .iter()
                .map(|seg| Ident::new(*seg, Span::default()))
                .collect(),
        )
    }

    pub fn pop_back(&mut self) -> Option<InternedStr> {
        self.module_path.pop_back()
    }

    pub fn last(&mut self) -> Option<InternedStr> {
        self.module_path.back().copied()
    }

    pub fn pop_front(&mut self) -> Option<InternedStr> {
        self.module_path.pop_front()
    }

    pub fn front(&mut self) -> Option<InternedStr> {
        self.module_path.front().copied()
    }

    pub fn len(&self) -> usize {
        self.module_path.len()
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
