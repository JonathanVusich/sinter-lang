use crate::compiler::ast::{Ident, QualifiedIdent, Segment};
use crate::compiler::types::types::InternedStr;
use serde::{Deserialize, Serialize};
use std::path::Path;

#[derive(PartialEq, Eq, Hash, Debug, Clone, Serialize, Deserialize)]
pub struct ModulePath {
    krate_path: Vec<InternedStr>,
}

impl ModulePath {
    pub fn new(krate_path: Vec<InternedStr>) -> Self {
        Self { krate_path }
    }
}
