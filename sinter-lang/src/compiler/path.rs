use std::ops::Deref;
use crate::compiler::ast::{Ident, QualifiedIdent, Segment};
use crate::compiler::types::types::InternedStr;
use serde::{Deserialize, Serialize};
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

impl From<QualifiedIdent> for ModulePath {
    fn from(value: QualifiedIdent) -> Self {
        Self {
            krate_path: value.iter().map(|ident| ident.ident).collect()
        }
    }
}

impl Deref for ModulePath {
    type Target = Vec<InternedStr>;

    fn deref(&self) -> &Self::Target {
        &self.krate_path
    }
}

