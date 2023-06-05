use crate::compiler::ast::Module;
use crate::compiler::hir::{HirItem, LocalDefId};
use crate::compiler::path::ModulePath;
use crate::compiler::types::types::InternedStr;
use serde::de::{DeserializeOwned, SeqAccess, Visitor};
use serde::ser::{SerializeSeq, SerializeStruct};
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::collections::HashMap;
use std::fmt::{format, Formatter};
use std::marker::PhantomData;
use std::ops::Deref;

#[derive(PartialEq, Debug, Default, Copy, Clone, Serialize, Deserialize)]
pub struct CrateId {
    id: u32,
}

impl CrateId {
    pub fn new(id: u32) -> Self {
        Self { id }
    }
}

impl From<u32> for CrateId {
    fn from(value: u32) -> Self {
        Self { id: value }
    }
}

#[derive(PartialEq, Debug, Default)]
pub struct ModuleMap {
    table: HashMap<ModulePath, Module>,
}

impl Deref for ModuleMap {
    type Target = HashMap<ModulePath, Module>;

    fn deref(&self) -> &Self::Target {
        &self.table
    }
}

impl ModuleMap {
    pub fn new(table: HashMap<ModulePath, Module>) -> Self {
        Self { table }
    }

    pub fn insert(&mut self, key: ModulePath, value: Module) -> Option<Module> {
        self.table.insert(key, value)
    }
}

impl Serialize for ModuleMap {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut seq = serializer.serialize_seq(Some(self.table.len()))?;
        for entry in &self.table {
            seq.serialize_element(&entry)?;
        }
        seq.end()
    }
}

#[derive(Default)]
struct ModuleMapVisitor;

impl<'de> Visitor<'de> for ModuleMapVisitor {
    type Value = ModuleMap;

    fn expecting(&self, formatter: &mut Formatter) -> std::fmt::Result {
        formatter.write_str("Module map")
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: SeqAccess<'de>,
    {
        let mut table = HashMap::with_capacity(seq.size_hint().unwrap_or(10));
        while let Some((key, value)) = seq.next_element()? {
            table.insert(key, value);
        }
        Ok(ModuleMap::new(table))
    }
}

impl<'de> Deserialize<'de> for ModuleMap {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_seq(ModuleMapVisitor::default())
    }
}

pub struct CrateNamespace {
    fns: HashMap<ModulePath, LocalDefId>,
    tys: LocalDefId,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct Crate {
    pub(crate) name: InternedStr,
    pub(crate) id: CrateId,
    pub(crate) module_lookup: ModuleMap,
    pub(crate) namespace: Namespace,
}

impl Crate {
    pub fn new(name: InternedStr) -> Self {
        Self {
            name,
            id: Default::default(),
            module_lookup: Default::default(),
        }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct ResolvedCrate {
    pub(crate) name: InternedStr,
    pub(crate) id: CrateId,
    pub(crate) items: Vec<HirItem>,
}
