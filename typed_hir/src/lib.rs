#![allow(unused)]

use std::collections::HashMap;
use std::fmt::Debug;

use serde::Serialize;

use ast::{Ident, InfixOp, Mutability, UnaryOp};
use hir::{ClassDef, EnumDef, FnDef, MemberDef, TraitDef};
use id::{DefId, LocalDefId};
use interner::InternedStr;

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct ExprId(LocalDefId);

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct BlockId(LocalDefId);

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct StmtId(LocalDefId);

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct ArmId(LocalDefId);

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct PatternId(LocalDefId);

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct Expr<'a> {
    pub kind: ExprKind<'a>,
    pub ty: Ty<'a>,
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub enum ExprKind<'a> {
    Array(ArrayExpr),
    Call(CallExpr),
    Infix(InfixExpr),
    Unary(UnaryExpr),
    None,
    True,
    False,
    Int(i64),
    UInt(u64),
    Float(f64),
    String(InternedStr),
    Match(MatchExpr),
    Closure(ClosureExpr),
    Assign(AssignExpr),
    Field(FieldExpr),
    Index(IndexExpr),
    Path(PathExpr<'a>),
    Break,
    Continue,
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub enum Ty<'a> {
    Array(&'a Ty<'a>),
    Class(&'a ClassDef<'a>, Generics<'a>),
    Enum(&'a EnumDef<'a>, Generics<'a>),
    EnumMember(&'a MemberDef<'a>, Generics<'a>),
    TraitBound(TraitBound<'a>, Generics<'a>),
    GenericParam(&'a GenericParam<'a>),
    Fn(Fn<'a>),
    Infer(TyVar),
    Float(FloatTy),
    Int(IntTy),
    Uint(UintTy),
    Str,
    Boolean,
    None,
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub enum DestructureExpr<'a> {
    Pattern(DestructurePattern<'a>),
    Identifier(LocalVar),
    None,
    True,
    False,
    Int(i64),
    UInt(u64),
    Float(f64),
    String(InternedStr),
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub enum Stmt {
    Let(LetStmt),
    For(ForStmt),
    If(IfStmt),
    Return(ReturnStmt),
    While(WhileStmt),
    Block(Block),
    Expression(Expression),
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub enum StmtKind {}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct Pattern<'a> {
    kind: PatternKind<'a>,
    ty: &'a Ty<'a>,
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub enum PatternKind<'a> {
    Wildcard,
    Or(OrPattern<'a>),
    None,
    True,
    False,
    Int(i64),
    UInt(u64),
    Float(f64),
    String(InternedStr),
    Ty(TyPattern<'a>),
    Destructure(DestructurePattern<'a>),
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct Constant<'a> {
    pub local_var: LocalVar,
    pub ty: &'a Ty<'a>,
    pub initializer: ExprId,
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct TraitImplDef<'a> {
    pub trait_to_impl: &'a PathTy<'a>,
    pub target_ty: DefId,
    pub member_fns: FnStmts<'a>,
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct FnSig<'a> {
    pub name: Ident,
    pub generic_params: GenericParams<'a>,
    pub params: Params<'a>,
    pub return_type: Option<&'a Ty<'a>>,
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct Param<'a> {
    pub local_var: LocalVar,
    pub ty: &'a Ty<'a>,
    pub mutability: Mutability,
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct Block {
    pub stmts: Stmts,
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct Expression {
    pub expr: ExprId,
    pub implicit_return: bool,
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct TyVar {
    pub(crate) id: u32,
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub enum FloatTy {
    F32,
    F64,
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub enum IntTy {
    I8,
    I16,
    I32,
    I64,
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub enum UintTy {
    U8,
    U16,
    U32,
    U64,
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct PathTy<'a> {
    pub definition: DefId,
    pub generics: Generics<'a>,
}

pub type TraitBound<'a> = Box<[&'a TraitDef<'a>]>;
pub type Generics<'a> = &'a [&'a Ty<'a>];
pub type Args = Box<[ExprId]>;
pub type Stmts = Box<[StmtId]>;
pub type AnonParams<'a> = &'a [&'a Ty<'a>];
pub type Initializers = Box<[ExprId]>;
pub type Exprs = Box<[ExprId]>;
pub type DestructureExprs<'a> = &'a [&'a DestructureExpr<'a>];
pub type GenericParams<'a> = &'a [&'a GenericParam<'a>];
pub type Fields<'a> = &'a [&'a Field<'a>];
pub type ClosureParams = Box<[ClosureParam]>;
pub type Params<'a> = &'a [&'a Param<'a>];
pub type FnStmts<'a> = &'a [&'a FnDef<'a>];
pub type MemberDefs<'a> = &'a [&'a MemberDef<'a>];
pub type MatchArms = Box<[ArmId]>;
pub type Patterns<'a> = Box<[Pattern<'a>]>;

#[derive(PartialEq, Debug, Clone, Serialize)]
pub enum Literal {
    None,
    True,
    False,
    Integer(i64),
    Float(f64),
    String(InternedStr),
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub enum ArrayExpr {
    Sized { initializer: ExprId, size: ExprId },
    Unsized { initializers: Initializers },
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct CallExpr {
    pub target: ExprId,
    pub args: Args,
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct UnaryExpr {
    pub operator: UnaryOp,
    pub expr: ExprId,
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct InfixExpr {
    pub operator: InfixOp,
    pub lhs: ExprId,
    pub rhs: ExprId,
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct MatchExpr {
    pub source: ExprId,
    pub arms: MatchArms,
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct MatchArm<'a> {
    pub pattern: Pattern<'a>,
    pub body: StmtId,
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct OrPattern<'a> {
    patterns: Patterns<'a>,
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct TyPattern<'a> {
    pub ty: &'a PathExpr<'a>,
    pub ident: Option<LocalVar>,
}

#[derive(Clone, PartialEq, Debug, Serialize)]
pub struct LocalVar {
    pub ident: InternedStr,
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct DestructurePattern<'a> {
    pub ty: &'a PathExpr<'a>,
    pub exprs: DestructureExprs<'a>,
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct Fn<'a> {
    pub params: AnonParams<'a>,
    pub ret_ty: &'a Ty<'a>,
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct ClosureExpr {
    pub params: ClosureParams,
    pub stmt: StmtId,
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct AssignExpr {
    pub lhs: ExprId,
    pub rhs: ExprId,
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct FieldExpr {
    pub lhs: ExprId,
    pub ident: Ident,
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct IndexExpr {
    pub expr: ExprId,
    pub key: ExprId,
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub enum PathExpr<'a> {
    Class(&'a ClassDef<'a>, Generics<'a>),
    Enum(&'a EnumDef<'a>, Generics<'a>),
    Trait(&'a TraitDef<'a>, Generics<'a>),
    Fn(&'a FnDef<'a>, Generics<'a>),
    Var(LocalVar),
    Generic(GenericParam<'a>),
    Float(FloatTy),
    Int(IntTy),
    Uint(UintTy),
    Str,
    Boolean,
    None,
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub enum LocalDef {
    Var(LocalVar),
    Generic(LocalDefId),
}

#[derive(PartialEq, Eq, Debug, Clone, Serialize)]
pub enum DefTy {
    GlobalLet,
    Class,
    Enum,
    EnumMember,
    Trait,
    Fn,
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct Field<'a> {
    pub ident: Ident,
    pub ty: &'a Ty<'a>,
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct LetStmt {
    pub local_var: LocalVar,
    pub mutability: Mutability,
    // Don't need to record a Ty here because the expression will be typed.
    pub initializer: Option<ExprId>,
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct ReturnStmt {
    pub value: Option<ExprId>,
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct WhileStmt {
    pub condition: ExprId,
    pub block: BlockId,
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct ForStmt {
    pub ident: LocalVar,
    pub range: ExprId,
    pub body: BlockId,
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct IfStmt {
    pub condition: ExprId,
    pub if_true: BlockId,
    pub if_false: Option<BlockId>,
}

#[derive(PartialEq, Debug, Clone, Serialize)]
#[repr(transparent)]
#[serde(transparent)]
pub struct ClosureParam {
    ident: Ident,
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct GenericParam<'a> {
    pub ident: Ident,
    pub trait_bound: TraitBound<'a>, // Can be empty to indicate no trait bounds.
}

pub struct ThirBodies<'hir> {
    pub exprs: HashMap<LocalDefId, Thir<'hir>>,
}

pub struct Thir<'hir> {
    generic_tys: Vec<&'hir Ty<'hir>>,
    ret_ty: &'hir Ty<'hir>,

    // Contents of the block which will be useful for looking things up later.
    blocks: Vec<Block>,
    arms: Vec<MatchArm<'hir>>,
    stmts: Vec<Stmt>,
    exprs: Vec<Expr<'hir>>,
}
