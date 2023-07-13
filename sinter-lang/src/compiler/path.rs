use std::borrow::Borrow;
use std::ops::Deref;
use std::path::Path;

use serde::{Deserialize, Serialize};

use crate::compiler::ast::{Ident, IdentType, QualifiedIdent, Segment};
use crate::compiler::types::InternedStr;

use crate::gc::block::BLOCK_SIZE;

#[derive(PartialEq, Eq, Hash, Default, Debug, Clone, Serialize, Deserialize)]
pub struct ModulePath {
    module_path: Vec<InternedStr>,
}

impl ModulePath {
    pub fn new(module_path: Vec<InternedStr>) -> Self {
        Self { module_path }
    }

    pub fn pop(&mut self) -> Option<InternedStr> {
        self.module_path.pop()
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
