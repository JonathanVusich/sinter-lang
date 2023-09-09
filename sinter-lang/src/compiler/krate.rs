use radix_trie::Trie;
use std::collections::{HashMap, HashSet};
use std::fmt::{format, Formatter};
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};

use serde::de::{DeserializeOwned, SeqAccess, Visitor};
use serde::ser::{SerializeSeq, SerializeStruct};
use serde::{Deserialize, Deserializer, Serialize, Serializer};

use crate::compiler::ast::{
    AstPass, Field, Item, ItemKind, Module, PathExpr as AstPathExpr, QualifiedIdent,
};
use crate::compiler::ast_passes::{UsedCrate, UsedCrateCollector};
use crate::compiler::compiler::CompileError;
use crate::compiler::hir::{DefId, LocalDefId, ModuleId, PathExpr, Segment};
use crate::compiler::parser::ParseError;
use crate::compiler::path::ModulePath;
use crate::compiler::resolver::{EnumDef, ResolveError, ValueDef};
use crate::compiler::types::{InternedStr, StrMap};

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

impl<T> DerefMut for ModuleMap<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.table
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
    pub(crate) crate_id: CrateId,
    module_lookup: ModuleMap<ModuleId>,
    modules: Vec<Module>,
}

impl Crate {
    pub fn new(name: InternedStr, crate_id: CrateId) -> Self {
        Self {
            name,
            crate_id,
            module_lookup: Default::default(),
            modules: Default::default(),
        }
    }

    pub fn index(&self) -> usize {
        self.crate_id.id as usize
    }

    pub fn add_module(&mut self, module_path: ModulePath, mut module: Module) {
        let module_id = self.modules.len();
        let full_mod_id = ModuleId::new(self.crate_id.id, module_id as u32);
        module.id = full_mod_id;
        self.module_lookup.insert(module_path, full_mod_id);
        self.modules.push(module);
    }

    pub fn find_definition(
        &self,
        ident: &QualifiedIdent,
        trim_crate_name: bool,
    ) -> Option<CrateDef> {
        let mut module_path = ModulePath::from(ident);
        if trim_crate_name {
            module_path.pop_front();
        }
        match self.find_module(&module_path) {
            Some(mod_id) => Some(CrateDef::Module(module_path.front()?, mod_id)),
            None => {
                let value = module_path.pop_back()?;
                match self.find_module(&module_path) {
                    Some(mod_id) => {
                        let module = self.module(mod_id);
                        module
                            .ns
                            .find_value(value)
                            .map(|val| CrateDef::Value(value, val.clone()))
                            .or_else(|| {
                                module
                                    .ns
                                    .find_enum(value)
                                    .map(|enum_def| CrateDef::Enum(value, enum_def.clone()))
                            })
                    }
                    None => {
                        let enum_name = module_path.pop_back()?;
                        // Reuse previously popped value
                        let enum_discriminant = value;
                        match self.module_lookup.get(&module_path) {
                            Some(mod_id) => {
                                let module = self.module(*mod_id);
                                module
                                    .ns
                                    .find_enum(enum_name)
                                    .and_then(|enum_def| enum_def.members.get(&enum_discriminant))
                                    .map(|val| CrateDef::EnumMember(enum_discriminant, *val))
                            }
                            None => None,
                        }
                    }
                }
            }
        }
    }

    fn find_module(&self, module_path: &ModulePath) -> Option<ModuleId> {
        self.module_lookup.get(module_path).copied()
    }

    pub fn module(&self, mod_id: ModuleId) -> &Module {
        &self.modules[mod_id.module_id()]
    }

    pub fn module_mut(&mut self, module_id: usize) -> &mut Module {
        self.modules.get_mut(module_id).unwrap()
    }

    pub fn modules(&self) -> impl Iterator<Item = &Module> {
        self.modules.iter()
    }

    pub fn modules_mut(&mut self) -> impl Iterator<Item = &mut Module> {
        self.modules.iter_mut()
    }

    pub fn used_crates(&self) -> HashSet<UsedCrate> {
        let mut used_crates = HashSet::default();
        for module in self.modules() {
            used_crates.extend(UsedCrateCollector::visit(module));
        }
        used_crates
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub enum CrateDef {
    Module(InternedStr, ModuleId),
    Enum(InternedStr, EnumDef),
    EnumMember(InternedStr, DefId),
    Value(InternedStr, ValueDef),
}
