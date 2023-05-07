use crate::compiler::ast::{Ident, Segment};
use crate::compiler::types::types::InternedStr;
use serde::{Deserialize, Serialize};

#[derive(Eq, PartialEq, Hash, Debug, Serialize, Deserialize)]
pub struct ModulePath {
    paths: Vec<InternedStr>,
}

impl ModulePath {
    pub fn new(paths: Vec<InternedStr>) -> Self {
        Self { paths }
    }
}
