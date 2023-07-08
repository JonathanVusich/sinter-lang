use std::borrow::Borrow;
use std::ops::Deref;
use std::path::Path;

use serde::{Deserialize, Serialize};

use crate::compiler::ast::{Ident, IdentType, QualifiedIdent, Segment};
use crate::compiler::types::InternedStr;

#[derive(PartialEq, Eq, Hash, Default, Debug, Clone, Serialize, Deserialize)]
pub struct ModulePath {
    krate_path: Vec<InternedStr>,
}

impl ModulePath {
    pub fn new(krate_path: Vec<InternedStr>) -> Self {
        Self { krate_path }
    }
}

impl<T> From<T> for ModulePath
where
    T: Borrow<QualifiedIdent>,
{
    fn from(value: T) -> Self {
        let qualified_ident = value.borrow();
        Self {
            krate_path: qualified_ident
                .idents
                .iter()
                .map(|ident| ident.ident)
                .collect(),
        }
    }
}
