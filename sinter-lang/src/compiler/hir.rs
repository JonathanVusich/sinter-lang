use crate::compiler::ast::{AstModule, Item, ItemKind, Ty};
use crate::compiler::compiler::CompilerCtxt;
use crate::compiler::path::ModulePath;
use crate::compiler::tokens::tokenized_file::Span;
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use std::num::NonZeroU32;

pub fn generate_hir(compiler_ctxt: CompilerCtxt, modules: AstModule) -> (CompilerCtxt, HirModule) {
    // Put hir lookups into compiler ctxt since it is the "global" state object for the compiler.
    // Generate
    todo!()
}

pub struct HirModule {
    path: ModulePath,
    uses: Vec<Use>,
    lets: Vec<Let>,
    classes: Vec<Class>,
    enums: Vec<Enum>,
    traits: Vec<Trait>,
    trait_impls: Vec<TraitImpl>,
    fns: Vec<Fn>,
}

/// The identifier for a node in the HIR.
#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash, Serialize, Deserialize)]
pub struct HirId {
    crate_id: NonZeroU32,
    local_id: NonZeroU32,
}

impl HirId {
    pub fn new(crate_id: NonZeroU32, local_id: NonZeroU32) -> Self {
        Self { crate_id, local_id }
    }
}

#[derive(PartialEq, Eq, Debug, Serialize, Deserialize)]
pub struct HirMap {
    items: FxHashMap<HirId, HirItem>,
}

impl HirMap {
    pub fn new() -> Self {
        Self {
            items: FxHashMap::default(),
        }
    }

    pub fn insert(&mut self, item: HirItem) {
        self.items.insert(item.id, item);
    }

    pub fn resolve(&self, id: HirId) -> &HirItem {
        self.items.get(&id).expect("Bug in the compiler!")
    }
}

#[derive(PartialEq, Eq, Debug, Serialize, Deserialize)]
pub struct HirItem {
    parent: Option<HirId>,
    id: HirId,
    item: HirItemKind,
    span: Span,
}

#[derive(PartialEq, Eq, Debug, Serialize, Deserialize)]
enum HirItemKind {
    Use(Use),
    Let(Let),
    Class(Class),
    Enum(Enum),
    Trait(Trait),
    TraitImpl(TraitImpl),
    Fn(Fn),
}

#[derive(PartialEq, Eq, Debug, Serialize, Deserialize)]
struct Use {}

#[derive(PartialEq, Eq, Debug, Serialize, Deserialize)]
struct Let {}

#[derive(PartialEq, Eq, Debug, Serialize, Deserialize)]
struct Class {}

#[derive(PartialEq, Eq, Debug, Serialize, Deserialize)]
struct Enum {}

#[derive(PartialEq, Eq, Debug, Serialize, Deserialize)]
struct Trait {}

#[derive(PartialEq, Eq, Debug, Serialize, Deserialize)]
struct TraitImpl {}

#[derive(PartialEq, Eq, Debug, Serialize, Deserialize)]
struct Fn {}
