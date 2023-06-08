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
        Self { kind, span, id }
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

    Expr(Expr),
    Ty(Ty),
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct GlobalLetStmt {
    pub ident: InternedStr,
    pub ty: Option<Ty>,
    pub initializer: Expr,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct ClassStmt {}
#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct EnumStmt {}
#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct TraitStmt {}
#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct TraitImplStmt {}
#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct FnStmt {}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub enum Ty {
    Array { ty: Box<Ty> },
    Path { path: PathTy },
    Union { tys: Vec<Ty> },
    TraitBound { trait_bound: TraitBound },
    Closure { params: Vec<Ty>, ret_ty: Box<Ty> },
    Infer,
    QSelf,
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
    F32,
    F64,
    Str,
    None,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct PathTy {}
#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct TraitBound {}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub enum Expr {
    Array(ArrayExpr),
    Call(CallExpr),
    Constructor(CallExpr),
    Infix(InfixExpr),
    Unary(UnaryExpr),
    None,
    Boolean(bool),
    Integer(i64),
    Float(f64),
    String(InternedStr),
    Match(MatchExpr),
    Closure(ClosureExpr),
    Assign(AssignExpr),
    Field(FieldExpr),
    Index(IndexExpr),
    Path(PathExpr),
    Break,
    Continue,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct ArrayExpr {}
#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct CallExpr {}
#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct UnaryExpr {}
#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct InfixExpr {}
#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct MatchExpr {}
#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct ClosureExpr {}
#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct AssignExpr {}
#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct FieldExpr {}
#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct IndexExpr {}
#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct PathExpr {}

pub struct HirCrate {}
