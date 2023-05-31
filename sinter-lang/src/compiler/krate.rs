use crate::compiler::ast::{Ast, AstId};
use crate::compiler::path::ModulePath;
use crate::compiler::types::types::InternedStr;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(PartialEq, Debug, Default, Copy, Clone, Serialize, Deserialize)]
pub struct KrateId {
    id: usize,
}

impl KrateId {
    pub fn new(id: usize) -> Self {
        Self { id }
    }
}

impl From<KrateId> for usize {
    fn from(value: KrateId) -> Self {
        value.id
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct Krate {
    pub(crate) name: InternedStr,
    pub(crate) id: KrateId,
    pub(crate) modules: Vec<Ast>,
    pub(crate) module_lookup: HashMap<ModulePath, usize>,
}

impl Krate {
    pub fn new(name: InternedStr) -> Self {
        Self {
            name,
            id: Default::default(),
            modules: Default::default(),
            module_lookup: Default::default(),
        }
    }
}
