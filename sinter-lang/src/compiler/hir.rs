use crate::compiler::ast::{Ident, InfixOp, Mutability, QualifiedIdent, UnaryOp};
use crate::compiler::parser::ClassType;
use crate::compiler::tokens::tokenized_file::Span;
use crate::compiler::types::types::InternedStr;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::marker::PhantomData;
use crate::compiler::krate::CrateId;

#[derive(PartialEq, Debug, Clone, Copy, Serialize, Deserialize)]
pub struct DefId {
    crate_id: u32,
    local_id: u32,
}

#[repr(transparent)]
#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct LocalDefId {
    local_id: u32,
}

impl LocalDefId {
    pub fn new(local_id: u32) -> Self {
        Self {
            local_id,
        }
    }
    
    pub fn to_def_id(&self, crate_id: CrateId) -> DefId {
        DefId {
            crate_id: crate_id.into(),
            local_id: self.local_id,
        }
    }
}

impl From<u32> for LocalDefId {
    fn from(value: u32) -> Self {
        LocalDefId {
            local_id: value,
        }
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
    Stmt(Stmt),

    Field(Field),
    Pattern(Pattern),
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct GlobalLetStmt {
    pub ident: InternedStr,
    pub ty: Option<LocalDefId>,
    pub initializer: LocalDefId,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct ClassStmt {
    pub(crate) name: Ident,
    pub(crate) class_type: ClassType,
    pub(crate) generic_params: GenericParams,
    pub(crate) fields: Fields,
    pub(crate) fn_stmts: FnStmts,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct EnumStmt {
    pub name: Ident,
    pub generic_params: GenericParams,
    pub members: EnumMembers,
    pub member_fns: FnStmts,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct EnumMember {
    pub name: InternedStr,
    pub fields: Fields,
    pub member_fns: FnStmts,
    pub span: Span,
    pub id: LocalDefId,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct EnumMembers {
    members: HashMap<InternedStr, LocalDefId>,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct TraitStmt {
    pub name: Ident,
    pub generic_params: GenericParams,
    pub member_fns: FnStmts,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct TraitImplStmt {
    pub trait_to_impl: DefId,
    pub target_ty: DefId,
    pub member_fns: FnStmts,
}
#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct FnStmt {
    pub(crate) sig: FnSig,
    pub(crate) body: Option<Block>,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct FnSig {
    pub(crate) name: Ident,
    pub(crate) generic_params: GenericParams,
    pub(crate) params: Params,
    pub(crate) return_type: Option<LocalDefId>,
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct Param {
    pub(crate) ident: Ident,
    pub(crate) ty: Ty,
    pub(crate) mutability: Mutability,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct Block {
    pub stmts: Stmts,
    pub span: Span,
    pub id: LocalDefId,
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct Expression {
    expr: LocalDefId,
    implicit_return: bool,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
//noinspection DuplicatedCode
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
pub enum Stmt {
    Let(LetStmt),
    For(ForStmt),
    If(IfStmt),
    Return(ReturnStmt),
    While(WhileStmt),
    Block(Block),
    Expression(Expression),
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct PathTy {
    pub ident: QualifiedIdent,
    pub generics: Generics,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct TraitBound {
    bounds: Vec<PathTy>,
}

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
pub enum ArrayExpr {
    Sized {
        initializer: LocalDefId,
        size: LocalDefId,
    },
    Unsized {
        initializers: Vec<LocalDefId>,
    },
}
#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct CallExpr {
    pub target: LocalDefId,
    pub args: Args,
}

impl CallExpr {
    pub fn new(target: LocalDefId, args: Args) -> Self {
        Self { target, args }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct UnaryExpr {
    pub operator: UnaryOp,
    pub expr: LocalDefId,
}

impl UnaryExpr {
    pub fn new(operator: UnaryOp, expr: LocalDefId) -> Self {
        Self { operator, expr }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct InfixExpr {
    pub operator: InfixOp,
    pub lhs: LocalDefId,
    pub rhs: LocalDefId,
}

impl InfixExpr {
    pub fn new(operator: InfixOp, lhs: LocalDefId, rhs: LocalDefId) -> Self {
        Self { operator, lhs, rhs }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct MatchExpr {
    pub source: LocalDefId,
    pub arms: Vec<MatchArm>,
}

impl MatchExpr {
    pub fn new(source: LocalDefId, arms: Vec<MatchArm>) -> Self {
        Self { source, arms }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct MatchArm {
    pub pattern: LocalDefId,
    pub body: LocalDefId,
}

impl MatchArm {
    pub fn new(pattern: LocalDefId, body: LocalDefId) -> Self {
        Self { pattern, body }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub enum Pattern {
    Wildcard,
    // _
    Or(OrPattern),
    // pat | pat
    Boolean(bool),
    // true/false
    Integer(i64),
    // 100
    String(InternedStr),
    // "true"
    Ty(TyPattern),
    // Logical logical => { }
    Destructure(DestructurePattern), // Logical(1, true, 100) => { }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct OrPattern {
    patterns: Vec<LocalDefId>,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct TyPattern {
    pub ty: LocalDefId,
    pub ident: Option<PatternLocal>,
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
#[repr(transparent)]
#[serde(transparent)]
pub struct PatternLocal {
    pub ident: InternedStr,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct DestructurePattern {
    pub ty: LocalDefId,
    pub exprs: Vec<LocalDefId>,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct ClosureExpr {
    pub params: Vec<ClosureParam>,
    pub stmt: LocalDefId,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct AssignExpr {
    pub lhs: LocalDefId,
    pub rhs: LocalDefId,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct FieldExpr {
    pub lhs: LocalDefId,
    pub ident: Ident,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct IndexExpr {
    pub expr: LocalDefId,
    pub key: LocalDefId,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct PathExpr {
    pub segments: Vec<Segment>,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct Segment {
    pub ident: Ident,
    pub generics: Option<Generics>,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct Field {
    pub(crate) ident: Ident,
    pub(crate) ty: Ty,
    pub(crate) span: Span,
    pub(crate) id: LocalDefId,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct LetStmt {
    pub ident: Ident,
    pub mutability: Mutability,
    pub ty: Option<LocalDefId>,
    pub initializer: Option<LocalDefId>,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct ReturnStmt {
    pub value: Option<LocalDefId>,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct WhileStmt {
    pub condition: LocalDefId,
    pub block_stmt: Block,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct ForStmt {}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct IfStmt {}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct FnStmts {
    fields: HashMap<InternedStr, LocalDefId>,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct Stmts {
    fields: Vec<LocalDefId>,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct Args {
    fields: HashMap<InternedStr, LocalDefId>,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct Fields {
    fields: HashMap<InternedStr, LocalDefId>,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
#[repr(transparent)]
#[serde(transparent)]
pub struct ClosureParam {
    ident: Ident,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct Params {
    fields: HashMap<InternedStr, LocalDefId>,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct GenericParams {
    fields: HashMap<InternedStr, LocalDefId>,
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct GenericParam {
    pub(crate) ident: Ident,
    pub(crate) trait_bound: Option<TraitBound>,
    pub(crate) span: Span,
    pub(crate) id: LocalDefId,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct Generics {
    generics: HashMap<InternedStr, LocalDefId>,
}

pub struct HirCrate {
    items: HashMap<DefId, HirItem>,
}
