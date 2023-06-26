use crate::compiler::ast::{Ident, QualifiedIdent, Segment};
use crate::compiler::types::types::InternedStr;
use serde::{Deserialize, Serialize};
use std::borrow::Borrow;
use std::ops::Deref;
use std::path::Path;

#[derive(PartialEq, Eq, Hash, Debug, Clone, Default, Serialize, Deserialize)]
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
        Self {
            krate_path: value.borrow().iter().map(|ident| ident.ident).collect(),
        }
    }
}

impl Deref for ModulePath {
    type Target = Vec<InternedStr>;

    fn deref(&self) -> &Self::Target {
        &self.krate_path
    }
}
