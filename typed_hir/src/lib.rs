#![allow(unused)]

use std::fmt::Debug;
use std::ops::{Add, Deref, Index, IndexMut, Sub};

use serde::Serialize;

use ast::{Ident, Mutability, UnaryOp};
use hir::InfixOp;
use id::{CrateId, DefId, LocalDefId};
use interner::InternedStr;
use types::LDefMap;

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct ExprId {
    pub(crate) id: u32,
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct DestructureExprId(u32);

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct BlockId {
    pub(crate) id: u32,
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct StmtId {
    pub(crate) id: u32,
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct ArmId(u32);

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct PatternId(u32);

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
    Block(Block),
    Path(PathExpr<'a>),
    Break,
    Continue,
}

#[derive(PartialEq, Eq, Hash, Debug, Copy, Clone, Serialize)]
pub struct Ty<'a> {
    pub kind: &'a TyKind<'a>,
}

impl<'a> Deref for Ty<'a> {
    type Target = TyKind<'a>;

    fn deref(&self) -> &Self::Target {
        self.kind
    }
}

#[derive(PartialEq, Eq, Hash, Debug, Clone, Serialize)]
pub enum TyKind<'a> {
    Array(Ty<'a>),
    Class(&'a ClassDef<'a>, Generics<'a>),
    Enum(&'a EnumDef<'a>, Generics<'a>),
    Member(&'a MemberDef<'a>, Generics<'a>),
    Trait(&'a TraitDef<'a>, Generics<'a>),
    TraitBound(TraitBound<'a>),
    GenericParam(&'a GenericParam<'a>),
    Fn(&'a FnDef<'a>, Generics<'a>),
    Closure(&'a ClosureDef<'a>),
    Infer(TyVar),
    Float(FloatTy),
    Int(IntTy),
    Uint(UintTy),
    Str,
    Boolean,
    None,
}

#[derive(PartialEq, Eq, Hash, Debug, Clone, Serialize)]
pub struct ClassDef<'a> {
    pub name: Ident,
    pub generic_params: GenericParams<'a>,
    pub fields: Fields<'a>,
    pub fns: FnDefs<'a>,
}

#[derive(PartialEq, Eq, Hash, Debug, Clone, Serialize)]
pub struct EnumDef<'a> {
    pub name: Ident,
    pub generic_params: GenericParams<'a>,
    pub members: MemberDefs<'a>,
    pub fn_defs: FnDefs<'a>,
}

#[derive(PartialEq, Eq, Hash, Debug, Clone, Serialize)]
pub struct MemberDef<'a> {
    pub name: InternedStr,
    pub generic_params: GenericParams<'a>,
    pub fields: Fields<'a>,
    pub fn_defs: FnDefs<'a>,
}

#[derive(PartialEq, Eq, Hash, Debug, Clone, Serialize)]
pub struct TraitDef<'a> {
    pub name: Ident,
    pub generic_params: GenericParams<'a>,
    pub fn_defs: FnDefs<'a>,
}

#[derive(PartialEq, Eq, Hash, Debug, Clone, Serialize)]
pub struct Trait<'a> {
    pub trait_def: &'a TraitDef<'a>,
    pub generics: Generics<'a>,
}

#[derive(PartialEq, Eq, Hash, Debug, Copy, Clone, Serialize)]
pub struct Field {
    pub name: Ident, // Needed to break the cycle of recursive types
    pub ty: DefId,
}

#[derive(PartialEq, Eq, Hash, Debug, Clone, Serialize)]
pub struct FnDef<'a> {
    pub name: Ident,
    pub ret_ty: Option<DefId>, // Needed to break the cycle of recursive types
    pub generic_params: GenericParams<'a>,
    pub params: Params<'a>,
}

#[derive(PartialEq, Eq, Hash, Debug, Clone, Serialize)]
pub struct ClosureDef<'a> {
    pub params: ClosureDefParams<'a>,
    pub return_type: Ty<'a>,
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub enum DestructureExpr<'a> {
    Pattern(DestructurePattern),
    Identifier(LocalVar<'a>),
    None,
    True,
    False,
    Int(i64),
    UInt(u64),
    Float(f64),
    String(InternedStr),
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub enum Stmt<'a> {
    Let(LetStmt<'a>),
    For(ForStmt<'a>),
    If(IfStmt),
    Return(ReturnStmt),
    While(WhileStmt),
    Block(BlockStmt),
    Expression(Expression),
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct Pattern<'a> {
    kind: PatternKind<'a>,
    ty: Ty<'a>,
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
    Destructure(DestructurePattern),
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct Constant<'a> {
    pub local_var: LocalVar<'a>,
    pub initializer: ExprId,
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

#[derive(PartialEq, Eq, Hash, Debug, Copy, Clone, Serialize)]
pub struct TyVar {
    pub id: u32,
}

#[derive(PartialEq, Eq, Hash, Debug, Copy, Clone, Serialize)]
pub enum FloatTy {
    F32,
    F64,
}

#[derive(PartialEq, Eq, Hash, Debug, Copy, Clone, Serialize)]
pub enum IntTy {
    I8,
    I16,
    I32,
    I64,
}

#[derive(PartialEq, Eq, Hash, Debug, Copy, Clone, Serialize)]
pub enum UintTy {
    U8,
    U16,
    U32,
    U64,
}

pub type TraitBound<'a> = &'a [&'a Trait<'a>];
// TODO: I think this needs to be DefId
pub type Generics<'a> = &'a [Ty<'a>];
pub type GenericParams<'a> = &'a [&'a GenericParam<'a>];
pub type Params<'a> = &'a [Param];
pub type ClosureDefParams<'a> = &'a [Ty<'a>];
pub type Fields<'a> = &'a [Field];
pub type MemberDefs<'a> = &'a [MemberDef<'a>];
pub type FnDefs<'a> = &'a [FnDef<'a>];
pub type Args = Box<[ExprId]>;
pub type Stmts = Box<[StmtId]>;
pub type AnonParams<'a> = &'a [Ty<'a>];
pub type Initializers = Box<[ExprId]>;
pub type Exprs = Box<[ExprId]>;
pub type DestructureExprs = Box<[DestructureExprId]>;
pub type ClosureParams = Box<[ClosureParam]>;
pub type MatchArms = Box<[ArmId]>;
pub type Patterns<'a> = Box<[Pattern<'a>]>;

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
    pub ty: ExprId,
    pub ident: Option<LocalVar<'a>>,
}

#[derive(Copy, Clone, PartialEq, Debug, Serialize)]
pub struct LocalVar<'a> {
    pub ident: InternedStr,
    pub ty: Ty<'a>,
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct DestructurePattern {
    pub ty_expr: ExprId,
    pub exprs: DestructureExprs,
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
    Class(DefId),
    Enum(DefId),
    Trait(DefId),
    Fn(DefId),
    Var(LocalVar<'a>),
    Generic(GenericParam<'a>),
    Float(FloatTy),
    Int(IntTy),
    Uint(UintTy),
    Str,
    Boolean,
    None,
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub enum LocalDef<'a> {
    Var(LocalVar<'a>),
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
pub struct LetStmt<'a> {
    pub local_var: LocalVar<'a>,
    pub mutability: Mutability,
    // Don't need to record a Ty here because the expression will be typed.
    pub initializer: Option<ExprId>,
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct ReturnStmt {
    pub expr: Option<ExprId>,
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct WhileStmt {
    pub condition: ExprId,
    pub block: BlockId,
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct BlockStmt {
    pub block: ExprId,
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct ForStmt<'a> {
    pub ident: LocalVar<'a>,
    pub range: ExprId,
    pub body: BlockId,
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct IfStmt {
    pub condition: ExprId,
    pub if_true: ExprId,
    pub if_false: Option<ExprId>,
}

/// This type represents a generic parameter to a class, enum or function.
#[derive(PartialEq, Eq, Hash, Debug, Copy, Clone, Serialize)]
pub struct GenericParam<'a> {
    pub ident: Ident,
    pub trait_bound: Option<TraitBound<'a>>,
}

/// This type represents an instantiated generic.
/// It retains a reference to the underlying param for trait bound validation
/// and to propagate the inferred type to generic param references.
#[derive(PartialEq, Eq, Hash, Debug, Copy, Clone, Serialize)]
pub struct Generic<'a> {
    pub ty: Ty<'a>,
    pub param: &'a GenericParam<'a>,
}

#[derive(PartialEq, Eq, Hash, Debug, Copy, Clone, Serialize)]
pub struct Param {
    pub name: Ident,
    pub ty: DefId,
    pub id: DefId,
}

#[derive(PartialEq, Debug, Clone, Serialize)]
#[repr(transparent)]
#[serde(transparent)]
pub struct ClosureParam {
    ident: Ident,
}

#[derive(Debug, Serialize)]
pub struct Thir<'hir> {
    generic_params: GenericParams<'hir>,
    ret_ty: Ty<'hir>,

    // Contents of the block which will be useful for looking things up later.
    arms: Vec<MatchArm<'hir>>,
    stmts: Vec<Stmt<'hir>>,
    exprs: Vec<Expr<'hir>>,
    destructure_exprs: Vec<DestructureExpr<'hir>>,
}

impl<'hir> Thir<'hir> {
    pub fn new(generic_params: GenericParams<'hir>, ret_ty: Ty<'hir>) -> Self {
        Self {
            generic_params,
            ret_ty,
            arms: Default::default(),
            stmts: Default::default(),
            exprs: Default::default(),
            destructure_exprs: Default::default(),
        }
    }

    pub fn exprs(&mut self) -> impl Iterator<Item = &'_ mut Expr<'hir>> {
        self.exprs.iter_mut()
    }

    pub fn stmts(&mut self) -> impl Iterator<Item = &'_ mut Stmt<'hir>> {
        self.stmts.iter_mut()
    }

    pub fn insert_expr(&mut self, expr: Expr<'hir>) -> ExprId {
        let id = self.exprs.len() as u32;
        let expr_id = ExprId { id };
        self.exprs.push(expr);
        expr_id
    }

    pub fn insert_stmt(&mut self, stmt: Stmt<'hir>) -> StmtId {
        let id = self.stmts.len() as u32;
        let stmt_id = StmtId { id };
        self.stmts.push(stmt);
        stmt_id
    }
}

impl<'hir> Index<ExprId> for Thir<'hir> {
    type Output = Expr<'hir>;

    fn index(&self, expr_id: ExprId) -> &Self::Output {
        self.exprs
            .get(expr_id.id as usize)
            .expect("Invalid expr id!")
    }
}

impl<'hir> IndexMut<ExprId> for Thir<'hir> {
    fn index_mut(&mut self, expr_id: ExprId) -> &mut Self::Output {
        self.exprs
            .get_mut(expr_id.id as usize)
            .expect("Invalid expr id!")
    }
}

#[derive(Serialize)]
pub struct ThirMap<'hir> {
    pub crates: Vec<ThirCrate<'hir>>,
}

#[derive(Serialize)]
pub struct ThirCrate<'hir> {
    pub name: InternedStr,
    pub id: CrateId,
    pub bodies: LDefMap<Thir<'hir>>,
}
