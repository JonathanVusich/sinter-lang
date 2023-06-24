use crate::compiler::ast::{Ident, InfixOp, Mutability, QualifiedIdent, UnaryOp};
use crate::compiler::krate::CrateId;
use crate::compiler::parser::ClassType;
use crate::compiler::tokens::tokenized_file::Span;
use crate::compiler::types::types::InternedStr;
use crate::traits::traits::Trait;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::marker::PhantomData;
use std::ops::Deref;

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
        Self { local_id }
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
        LocalDefId { local_id: value }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct HirNode {
    pub(crate) kind: HirNodeKind,
    pub(crate) span: Span,
    pub(crate) id: LocalDefId,
}

impl HirNode {
    pub fn new(kind: HirNodeKind, span: Span, id: LocalDefId) -> Self {
        Self { kind, span, id }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub enum HirNodeKind {
    Item(HirItem),

    EnumMember(EnumMember),

    Expr(Expr),
    Stmt(Stmt),
    Ty(Ty),
    Fn(FnStmt),
    Block(Block),

    Param(Param),
    GenericParam(GenericParam),
    Field(Field),
    Pattern(Pattern),
    MatchArm(MatchArm),
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub enum HirItem {
    GlobalLet(GlobalLetStmt),
    Class(ClassStmt),
    Enum(EnumStmt),
    Trait(TraitStmt),
    TraitImpl(TraitImplStmt),
    Fn(FnStmt),
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

impl ClassStmt {
    pub fn new(
        name: Ident,
        class_type: ClassType,
        generic_params: GenericParams,
        fields: Fields,
        fn_stmts: FnStmts,
    ) -> Self {
        Self {
            name,
            class_type,
            generic_params,
            fields,
            fn_stmts,
        }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct EnumStmt {
    pub name: Ident,
    pub generic_params: GenericParams,
    pub members: EnumMembers,
    pub member_fns: FnStmts,
}

impl EnumStmt {
    pub fn new(
        name: Ident,
        generic_params: GenericParams,
        members: EnumMembers,
        member_fns: FnStmts,
    ) -> Self {
        Self {
            name,
            generic_params,
            members,
            member_fns,
        }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct EnumMember {
    pub name: InternedStr,
    pub fields: Fields,
    pub member_fns: FnStmts,
    pub span: Span,
    pub id: LocalDefId,
}

#[derive(PartialEq, Debug, Default, Serialize, Deserialize)]
pub struct EnumMembers {
    members: HashMap<InternedStr, LocalDefId>,
}

impl Deref for EnumMembers {
    type Target = HashMap<InternedStr, LocalDefId>;

    fn deref(&self) -> &Self::Target {
        &self.members
    }
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
    pub(crate) body: Option<LocalDefId>,
}

impl FnStmt {
    pub fn new(sig: FnSig, body: Option<LocalDefId>) -> Self {
        Self { sig, body }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct FnSig {
    pub(crate) name: Ident,
    pub(crate) generic_params: GenericParams,
    pub(crate) params: Params,
    pub(crate) return_type: Option<LocalDefId>,
}

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
pub struct Param {
    pub(crate) ident: Ident,
    pub(crate) ty: LocalDefId,
    pub(crate) mutability: Mutability,
}

impl Param {
    pub fn new(ident: Ident, ty: LocalDefId, mutability: Mutability) -> Self {
        Self {
            ident,
            ty,
            mutability,
        }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct Block {
    pub stmts: Stmts,
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct Expression {
    expr: LocalDefId,
    implicit_return: bool,
}

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
//noinspection DuplicatedCode
pub enum Ty {
    Array {
        ty: LocalDefId,
    },
    Path {
        path: PathTy,
    },
    Union {
        tys: Vec<LocalDefId>,
    },
    TraitBound {
        trait_bound: TraitBound,
    },
    Closure {
        params: Vec<LocalDefId>,
        ret_ty: LocalDefId,
    },
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

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
pub struct PathTy {
    pub definition: DefId,
    pub generics: Generics,
}

impl PathTy {
    pub fn new(definition: DefId, generics: Generics) -> Self {
        Self {
            definition,
            generics,
        }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
pub struct TraitBound {
    bounds: Vec<PathTy>,
}

impl TraitBound {
    pub fn new(bounds: Vec<PathTy>) -> Self {
        Self { bounds }
    }
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
    pub params: ClosureParams,
    pub stmt: LocalDefId,
}

impl ClosureExpr {
    pub fn new(params: ClosureParams, stmt: LocalDefId) -> Self {
        Self { params, stmt }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct AssignExpr {
    pub lhs: LocalDefId,
    pub rhs: LocalDefId,
}

impl AssignExpr {
    pub fn new(lhs: LocalDefId, rhs: LocalDefId) -> Self {
        Self { lhs, rhs }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct FieldExpr {
    pub lhs: LocalDefId,
    pub ident: Ident,
}

impl FieldExpr {
    pub fn new(lhs: LocalDefId, ident: Ident) -> Self {
        Self { lhs, ident }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct IndexExpr {
    pub expr: LocalDefId,
    pub key: LocalDefId,
}

impl IndexExpr {
    pub fn new(expr: LocalDefId, key: LocalDefId) -> Self {
        Self { expr, key }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct PathExpr {
    pub segments: Vec<Segment>,
}

impl PathExpr {
    pub fn new(segments: Vec<Segment>) -> Self {
        Self { segments }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct Segment {
    pub ident: Ident,
    pub generics: Option<Generics>,
}

impl Segment {
    pub fn new(ident: Ident, generics: Option<Generics>) -> Self {
        Self { ident, generics }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct Field {
    pub(crate) ident: Ident,
    pub(crate) ty: LocalDefId,
}

impl Field {
    pub fn new(ident: Ident, ty: LocalDefId) -> Self {
        Self { ident, ty }
    }
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
    pub block_stmt: LocalDefId,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct ForStmt {}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct IfStmt {}

#[derive(PartialEq, Debug, Default, Serialize, Deserialize)]
pub struct FnStmts {
    fields: HashMap<InternedStr, LocalDefId>,
}

impl Deref for FnStmts {
    type Target = HashMap<InternedStr, LocalDefId>;

    fn deref(&self) -> &Self::Target {
        &self.fields
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct Stmts {
    fields: Vec<LocalDefId>,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct Args {
    fields: HashMap<InternedStr, LocalDefId>,
}

impl Args {
    pub fn new(fields: Hashmap<InternedStr, LocalDefId>) -> Self {
        Self { fields }
    }
}

#[derive(PartialEq, Debug, Default, Serialize, Deserialize)]
pub struct Fields {
    fields: HashMap<InternedStr, LocalDefId>,
}

impl Deref for Fields {
    type Target = HashMap<InternedStr, LocalDefId>;

    fn deref(&self) -> &Self::Target {
        &self.fields
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
#[repr(transparent)]
#[serde(transparent)]
pub struct ClosureParam {
    ident: Ident,
}

impl ClosureParam {
    pub fn new(ident: Ident) -> Self {
        Self { ident }
    }
}

#[derive(PartialEq, Debug, Default, Serialize, Deserialize)]
pub struct Params {
    fields: HashMap<InternedStr, LocalDefId>,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct ClosureParams {
    fields: HashMap<InternedStr, ClosureParam>,
}

impl ClosureParams {
    pub fn new(fields: HashMap<InternedStr, ClosureParam>) -> Self {
        Self { fields }
    }
}

impl Deref for ClosureParams {
    type Target = HashMap<InternedStr, ClosureParam>;

    fn deref(&self) -> &Self::Target {
        &self.fields
    }
}

#[derive(PartialEq, Debug, Default, Serialize, Deserialize)]
pub struct GenericParams {
    fields: HashMap<InternedStr, LocalDefId>,
}

impl Deref for GenericParams {
    type Target = HashMap<InternedStr, LocalDefId>;

    fn deref(&self) -> &Self::Target {
        &self.fields
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct GenericParam {
    pub(crate) ident: Ident,
    pub(crate) trait_bound: Option<LocalDefId>,
}

impl GenericParam {
    pub fn new(ident: Ident, trait_bound: Option<LocalDefId>) -> Self {
        Self { ident, trait_bound }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
pub struct Generics {
    generics: Vec<LocalDefId>,
}

impl Generics {
    pub fn with_capacity(len: usize) -> Self {
        Self {
            generics: Vec::with_capacity(len),
        }
    }
}

impl Deref for Generics {
    type Target = Vec<LocalDefId>;

    fn deref(&self) -> &Self::Target {
        &self.generics
    }
}

pub struct HirCrate {
    pub(crate) name: InternedStr,
    pub(crate) id: CrateId,
    items: Vec<LocalDefId>,
    nodes: HashMap<LocalDefId, HirNode>,
}

impl HirCrate {
    pub fn new(
        name: InternedStr,
        id: CrateId,
        items: Vec<LocalDefId>,
        nodes: HashMap<LocalDefId, HirNode>,
    ) -> Self {
        Self {
            name,
            id,
            items,
            nodes,
        }
    }
}
