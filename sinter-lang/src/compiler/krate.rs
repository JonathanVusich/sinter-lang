use std::collections::{HashMap, HashSet};
use std::fmt::Formatter;
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut, Index};

use radix_trie::Trie;
use serde::de::{DeserializeOwned, SeqAccess, Visitor};
use serde::ser::{SerializeSeq, SerializeStruct};
use serde::{Deserialize, Deserializer, Serialize, Serializer};

use crate::compiler::ast::{AstPass, Module, QualifiedIdent};
use crate::compiler::ast_passes::{UsedCrate, UsedCrateCollector};
use crate::compiler::hir::ModuleId;
use crate::compiler::path::ModulePath;
use crate::compiler::resolver::ValueDef;
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

impl<'a> Index<CrateId> for Vec<&'a Crate> {
    type Output = Crate;

    fn index(&self, index: CrateId) -> &Self::Output {
        &self[index.id as usize]
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
#[serde(from = "DeserCrate")]
pub struct Crate {
    pub(crate) name: InternedStr,
    pub(crate) crate_id: CrateId,
    #[serde(skip)]
    module_trie: Trie<ModulePath, ModuleId>,
    modules: Vec<Module>,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
struct DeserCrate {
    pub(crate) name: InternedStr,
    pub(crate) crate_id: CrateId,
    modules: Vec<Module>,
}

impl From<DeserCrate> for Crate {
    fn from(value: DeserCrate) -> Self {
        let mut module_trie = Trie::default();
        for module in &value.modules {
            module_trie.insert(module.path.clone(), module.id);
        }
        Crate {
            name: value.name,
            crate_id: value.crate_id,
            module_trie,
            modules: value.modules,
        }
    }
}

impl Index<ModuleId> for Crate {
    type Output = Module;

    fn index(&self, index: ModuleId) -> &Self::Output {
        &self.modules[index]
    }
}

impl Crate {
    pub fn new(name: InternedStr, crate_id: CrateId) -> Self {
        Self {
            name,
            crate_id,
            module_trie: Trie::default(),
            // module_lookup: Default::default(),
            modules: Default::default(),
        }
    }

    pub fn add_module(&mut self, module_path: ModulePath, mut module: Module) {
        let module_id = self.modules.len();
        let full_mod_id = ModuleId::new(self.crate_id.id, module_id as u32);
        module.id = full_mod_id;
        self.module_trie.insert(module_path, full_mod_id);
        // self.module_lookup.insert(module_path, full_mod_id);
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
        self.module_trie
            .get(&module_path)
            .copied()
            // The module is an exact match, return it directly
            .map(|id| CrateDef::Module(module_path.back().unwrap(), id))
            .or_else(|| {
                // The module is not an exact match, we need to search the nearest match for the remaining path segments.
                self.module_trie
                    .get_ancestor_value(&module_path)
                    .copied()
                    .and_then(|id| {
                        // Search for a value in the given module namespace.
                        let value = module_path.back()?;
                        let module = &self.modules[id];
                        module
                            .ns
                            .find_value(value)
                            .map(|val_def| CrateDef::Value(value, val_def.clone()))
                            .or_else(|| {
                                // Pop off a POTENTIAL enum discriminant and search for the enum.
                                let discriminant = module_path.pop_back()?;
                                let enum_val = module_path.back()?;
                                let val_def = module
                                    .ns
                                    .find_value(enum_val)
                                    .and_then(|val_def| match val_def {
                                        ValueDef::Enum(enum_def) => Some(enum_def),
                                        _ => None,
                                    })
                                    .and_then(|enum_def| enum_def.members.get(&discriminant))
                                    .map(|member_def| ValueDef::EnumMember(member_def.clone()))
                                    .map(|val_def| CrateDef::Value(discriminant, val_def));
                                val_def
                            })
                    })
            })
    }

    pub fn module_trie(&self) -> &Trie<ModulePath, ModuleId> {
        &self.module_trie
    }

    pub fn module(&self, mod_id: ModuleId) -> &Module {
        &self.modules[mod_id]
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

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
pub enum CrateDef {
    Module(InternedStr, ModuleId),
    Value(InternedStr, ValueDef),
}
