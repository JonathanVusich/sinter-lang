use crate::compiler::ast::{
    ArrayExpr, AssignExpr, CallExpr, ClassStmt, ClosureExpr, EnumStmt, FieldExpr, FnStmt,
    GlobalLetStmt, IndexExpr, InfixExpr, MatchExpr, Parentheses, PathExpr, TraitImplStmt,
    TraitStmt, UnaryExpr,
};
use crate::compiler::tokens::tokenized_file::Span;
use crate::compiler::types::types::InternedStr;
use hashbrown::HashMap;
use serde::{Deserialize, Serialize};
use std::collections::HashSet;

#[derive(PartialEq, Debug, Clone, Copy, Serialize, Deserialize)]
pub struct DefId {
    crate_id: u32,
    local_id: u32,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash, Serialize, Deserialize)]
pub struct LocalDefId {
    local_id: u32,
}

impl LocalDefId {
    pub fn new(local_id: u32) -> Self {
        Self { local_id }
    }
    
    pub fn to_def_id(&self, crate_id: u32) -> DefId {
        DefId {
            crate_id,
            local_id: self.local_id,
        }
    }
}

impl From<u32> for LocalDefId {
    fn from(value: u32) -> Self {
        LocalDefId { local_id: value }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct HirItem {
    pub(crate) kind: HirItemKind,
    pub(crate) span: Span,
    pub(crate) id: LocalDefId,
}

impl HirItem {
    pub fn new(kind: HirItemKind, span: Span, id: LocalDefId) -> Self {
        Self {
            kind,
            span,
            id,
        }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub enum HirItemKind {
    GlobalLet(GlobalLetStmt),
    Class(ClassStmt),
    Enum(EnumStmt),
    Trait(TraitStmt),
    TraitImpl(TraitImplStmt),
    Fn(FnStmt),
}

pub struct HirCrate {}
