use std::collections::{HashMap, HashSet};
use std::fmt::{format, Formatter};
use std::marker::PhantomData;
use std::ops::Deref;

use serde::de::{DeserializeOwned, SeqAccess, Visitor};
use serde::ser::{SerializeSeq, SerializeStruct};
use serde::{Deserialize, Deserializer, Serialize, Serializer};

use crate::compiler::ast::{AstPass, Field, Item, ItemKind, Module, QualifiedIdent};
use crate::compiler::ast_passes::{UsedCrate, UsedCrateCollector};
use crate::compiler::compiler::CompileError;
use crate::compiler::hir::{DefId, HirItem, LocalDefId};
use crate::compiler::parser::ParseError;
use crate::compiler::path::ModulePath;
use crate::compiler::resolver::ResolveError;
use crate::compiler::types::InternedStr;

#[derive(PartialEq, Eq, Debug, Default, Copy, Clone, PartialOrd, Ord, Serialize, Deserialize)]
pub struct CrateId {
    id: u32,
}

impl CrateId {
    pub fn new(id: u32) -> Self {
        Self { id }
    }
}

impl From<CrateId> for u32 {
    fn from(value: CrateId) -> Self {
        value.id
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct ModuleMap<T> {
    table: HashMap<ModulePath, T>,
}

impl<T> Default for ModuleMap<T> {
    fn default() -> Self {
        Self {
            table: HashMap::default(),
        }
    }
}

impl<V> FromIterator<(ModulePath, V)> for ModuleMap<V> {
    fn from_iter<T: IntoIterator<Item = (ModulePath, V)>>(iter: T) -> Self {
        let table = HashMap::from_iter(iter);
        Self { table }
    }
}

impl<T> Deref for ModuleMap<T> {
    type Target = HashMap<ModulePath, T>;

    fn deref(&self) -> &Self::Target {
        &self.table
    }
}

impl<T> ModuleMap<T> {
    pub fn new(table: HashMap<ModulePath, T>) -> Self {
        Self { table }
    }

    pub fn insert(&mut self, key: ModulePath, value: T) -> Option<T> {
        self.table.insert(key, value)
    }
}

impl<T> Serialize for ModuleMap<T>
where
    T: Serialize,
{
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

struct ModuleMapVisitor<T> {
    marker: PhantomData<T>,
}

impl<T> Default for ModuleMapVisitor<T> {
    fn default() -> Self {
        Self {
            marker: PhantomData::default(),
        }
    }
}

impl<'de, T> Visitor<'de> for ModuleMapVisitor<T>
where
    T: Deserialize<'de>,
{
    type Value = ModuleMap<T>;

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

impl<'de, T> Deserialize<'de> for ModuleMap<T>
where
    T: Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_seq(ModuleMapVisitor::default())
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct Crate {
    pub(crate) name: InternedStr,
    pub(crate) id: CrateId,
    pub(crate) local_def_id: u32,
    pub(crate) module_lookup: ModuleMap<Module>,
}

impl Crate {
    pub fn new(name: InternedStr, id: CrateId) -> Self {
        Self {
            name,
            id,
            local_def_id: 0,
            module_lookup: Default::default(),
        }
    }

    pub fn index(&self) -> usize {
        self.id.id as usize
    }

    pub fn add_module(
        &mut self,
        module_path: ModulePath,
        module: Module,
    ) -> Result<(), ParseError> {
        self.module_lookup.insert(module_path, module);
        Ok(())
    }

    pub fn get_used_crates(&self) -> HashSet<UsedCrate> {
        let mut used_crates = HashSet::default();
        for module in self.module_lookup.values() {
            used_crates.extend(UsedCrateCollector::visit(module));
        }
        used_crates
    }
}
