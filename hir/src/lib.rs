#![allow(unused)]

use std::collections::BTreeMap;
use std::fmt::{Debug, Formatter};

use serde::Serialize;

use ast::{ClassType, Ident, InfixOp, MaybeFnDef, ModulePath, Mutability, UnaryOp, ValueDef};
use id::{CrateId, DefId, LocalDefId, ModuleId};
use interner::InternedStr;
use span::Span;
use types::{LDefMap, StrSet};

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub enum Node<'a> {
    Item(&'a Item<'a>),

    Member(&'a MemberDef<'a>),
    Fn(&'a FnDef<'a>),

    Ty(&'a Ty<'a>),

    Expr(&'a Expr<'a>),
    DestructureExpr(&'a DestructureExpr<'a>),
    Stmt(&'a Stmt<'a>),
    Block(&'a Block<'a>),

    Param(&'a Param<'a>),
    Field(&'a Field<'a>),
    LocalVar(LocalVar),
    Pattern(&'a Pattern<'a>),
    MatchArm(&'a MatchArm<'a>),
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct Item<'a> {
    pub kind: ItemKind<'a>,
    pub span: Span,
    pub id: LocalDefId,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub enum ItemKind<'a> {
    Constant(&'a Constant<'a>),
    Class(&'a ClassDef<'a>),
    Enum(&'a EnumDef<'a>),
    Fn(&'a FnDef<'a>),
    Trait(&'a TraitDef<'a>),
    TraitImpl(&'a TraitImplDef<'a>),
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct Expr<'a> {
    pub kind: ExprKind<'a>,
    pub span: Span,
    pub id: LocalDefId,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub enum ExprKind<'a> {
    Array(ArrayExpr<'a>),
    Call(CallExpr<'a>),
    Infix(InfixExpr<'a>),
    Unary(UnaryExpr<'a>),
    None,
    True,
    False,
    Int(i64),
    UInt(u64),
    Float(f64),
    String(InternedStr),
    Match(MatchExpr<'a>),
    Closure(ClosureExpr<'a>),
    Assign(AssignExpr<'a>),
    Field(FieldExpr<'a>),
    Index(IndexExpr<'a>),
    Path(PathExpr<'a>),
    Break,
    Continue,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct Ty<'a> {
    pub kind: TyKind<'a>,
    pub span: Span,
    pub id: LocalDefId,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub enum TyKind<'a> {
    Array(&'a Ty<'a>),
    Path(&'a PathTy<'a>),
    GenericParam(&'a GenericParam<'a>),
    TraitBound(TraitBound<'a>),
    Closure(Closure<'a>),
    Primitive(Primitive),
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct DestructureExpr<'a> {
    kind: DestructureExprKind<'a>,
    span: Span,
    id: LocalDefId,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub enum DestructureExprKind<'a> {
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

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct Stmt<'a> {
    kind: StmtKind<'a>,
    span: Span,
    id: LocalDefId,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub enum StmtKind<'a> {
    Let(&'a LetStmt<'a>),
    For(&'a ForStmt<'a>),
    If(&'a IfStmt<'a>),
    Return(&'a ReturnStmt<'a>),
    While(&'a WhileStmt<'a>),
    Block(&'a Block<'a>),
    Expression(&'a Expression<'a>),
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct Pattern<'a> {
    kind: PatternKind<'a>,
    span: Span,
    id: LocalDefId,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
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

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct Constant<'a> {
    pub local_var: LocalVar,
    pub ty: &'a Ty<'a>,
    pub initializer: &'a Expr<'a>,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct ClassDef<'a> {
    pub name: Ident,
    pub class_type: ClassType,
    pub generic_params: GenericParams<'a>,
    pub fields: Fields<'a>,
    pub fn_stmts: FnStmts<'a>,
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct EnumDef<'a> {
    pub name: Ident,
    pub generic_params: GenericParams<'a>,
    pub members: MemberDefs<'a>,
    pub member_fns: FnStmts<'a>,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct MemberDef<'a> {
    pub name: InternedStr,
    pub fields: Fields<'a>,
    pub member_fns: FnStmts<'a>,
    pub span: Span,
    pub id: LocalDefId,
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct TraitDef<'a> {
    pub name: Ident,
    pub generic_params: GenericParams<'a>,
    pub member_fns: FnStmts<'a>,
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct TraitImplDef<'a> {
    pub trait_to_impl: &'a PathTy<'a>,
    pub target_ty: DefId,
    pub member_fns: FnStmts<'a>,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct FnDef<'a> {
    pub sig: FnSig<'a>,
    pub body: Option<&'a Block<'a>>,
    pub span: Span,
    pub id: LocalDefId,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct FnSig<'a> {
    pub name: Ident,
    pub generic_params: GenericParams<'a>,
    pub params: Params<'a>,
    pub return_type: Option<&'a Ty<'a>>,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct Param<'a> {
    pub local_var: LocalVar,
    pub ty: &'a Ty<'a>,
    pub mutability: Mutability,
    pub span: Span,
    pub id: LocalDefId,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct Block<'a> {
    pub stmts: Stmts<'a>,
    pub span: Span,
    pub id: LocalDefId,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct Expression<'a> {
    pub expr: &'a Expr<'a>,
    pub implicit_return: bool,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub enum Primitive {
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
    Boolean,
    None,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct PathTy<'a> {
    pub definition: DefId,
    pub generics: Generics<'a>,
}

pub type TraitBound<'a> = &'a [&'a PathTy<'a>];
pub type Generics<'a> = &'a [&'a Ty<'a>];
pub type Args<'a> = &'a [&'a Expr<'a>];
pub type Stmts<'a> = &'a [&'a Stmt<'a>];
pub type AnonParams<'a> = &'a [&'a Ty<'a>];
pub type Initializers<'a> = &'a [&'a Expr<'a>];
pub type Exprs<'a> = &'a [&'a Expr<'a>];
pub type DestructureExprs<'a> = &'a [&'a DestructureExpr<'a>];
pub type GenericParams<'a> = &'a [&'a GenericParam<'a>];
pub type Fields<'a> = &'a [&'a Field<'a>];
pub type ClosureParams<'a> = &'a [ClosureParam];
pub type Params<'a> = &'a [&'a Param<'a>];
pub type FnStmts<'a> = &'a [&'a FnDef<'a>];
pub type MemberDefs<'a> = &'a [&'a MemberDef<'a>];
pub type MatchArms<'a> = &'a [&'a MatchArm<'a>];
pub type Segments<'a> = &'a [&'a Segment<'a>];
pub type Patterns<'a> = &'a [&'a Pattern<'a>];

#[derive(PartialEq, Debug, Clone, Copy, Serialize)]
pub enum Literal {
    None,
    True,
    False,
    Integer(i64),
    Float(f64),
    String(InternedStr),
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub enum ArrayExpr<'a> {
    Sized {
        initializer: &'a Expr<'a>,
        size: &'a Expr<'a>,
    },
    Unsized {
        initializers: Initializers<'a>,
    },
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct CallExpr<'a> {
    pub target: &'a Expr<'a>,
    pub args: Args<'a>,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct UnaryExpr<'a> {
    pub operator: UnaryOp,
    pub expr: &'a Expr<'a>,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct InfixExpr<'a> {
    pub operator: InfixOp,
    pub lhs: &'a Expr<'a>,
    pub rhs: &'a Expr<'a>,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct MatchExpr<'a> {
    pub source: &'a Expr<'a>,
    pub arms: MatchArms<'a>,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct MatchArm<'a> {
    pub pattern: &'a Pattern<'a>,
    pub body: &'a Stmt<'a>,
    pub span: Span,
    pub id: LocalDefId,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct OrPattern<'a> {
    patterns: Patterns<'a>,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct TyPattern<'a> {
    pub ty: &'a PathTy<'a>,
    pub ident: Option<LocalVar>,
}

#[derive(Copy, Clone, PartialEq, Debug, Serialize)]
pub struct LocalVar {
    pub ident: InternedStr,
    pub span: Span,
    pub id: LocalDefId,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct DestructurePattern<'a> {
    pub ty: &'a PathTy<'a>,
    pub exprs: DestructureExprs<'a>,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct Closure<'a> {
    pub params: AnonParams<'a>,
    pub ret_ty: &'a Ty<'a>,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct ClosureExpr<'a> {
    pub params: ClosureParams<'a>,
    pub stmt: &'a Stmt<'a>,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct AssignExpr<'a> {
    pub lhs: &'a Expr<'a>,
    pub rhs: &'a Expr<'a>,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct FieldExpr<'a> {
    pub lhs: &'a Expr<'a>,
    pub ident: Ident,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct IndexExpr<'a> {
    pub expr: &'a Expr<'a>,
    pub key: &'a Expr<'a>,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct PathExpr<'a> {
    pub segments: Segments<'a>,
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub enum Res {
    Crate(CrateId),
    ModuleSegment(CrateId, ModulePath),
    Module(ModuleId),
    ValueDef(ValueDef),
    Fn(MaybeFnDef),
    // These have to be late resolved after type information is deduced?
    Local(LocalDef),
    Primitive(Primitive),
}

#[derive(PartialEq, Debug, Clone, Copy, Serialize)]
pub enum LocalDef {
    Var(LocalVar),
    Generic(LocalDefId),
}

#[derive(PartialEq, Eq, Debug, Clone, Copy, Serialize)]
pub enum DefTy {
    GlobalLet,
    Class,
    Enum,
    EnumMember,
    Trait,
    Fn,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct Segment<'a> {
    pub res: &'a Res,
    pub generics: Option<Generics<'a>>,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct Field<'a> {
    pub ident: Ident,
    pub ty: &'a Ty<'a>,
    pub span: Span,
    pub id: LocalDefId,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct LetStmt<'a> {
    pub local_var: LocalVar,
    pub mutability: Mutability,
    pub ty: Option<&'a Ty<'a>>,
    pub initializer: Option<&'a Expr<'a>>,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct ReturnStmt<'a> {
    pub value: Option<&'a Expr<'a>>,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct WhileStmt<'a> {
    pub condition: &'a Expr<'a>,
    pub block: &'a Block<'a>,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct ForStmt<'a> {
    pub ident: LocalVar,
    pub range: &'a Expr<'a>,
    pub body: &'a Block<'a>,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct IfStmt<'a> {
    pub condition: &'a Expr<'a>,
    pub if_true: &'a Block<'a>,
    pub if_false: Option<&'a Block<'a>>,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
#[repr(transparent)]
#[serde(transparent)]
pub struct ClosureParam {
    ident: Ident,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct GenericParam<'a> {
    pub ident: Ident,
    pub trait_bound: Option<TraitBound<'a>>,
}

#[derive(PartialEq, Debug, Default, Clone, Serialize)]
pub struct HirMap<'a> {
    crates: Vec<HirCrate<'a>>,
    names_to_indices: StrSet,
}

impl<'a> HirMap<'a> {
    pub fn insert(&mut self, krate: HirCrate<'a>) {
        self.names_to_indices.insert(krate.name);
        self.crates.push(krate);
    }

    pub fn krate_by_name(&self, name: &InternedStr) -> &HirCrate {
        let index = self.names_to_indices.get_index_of(name).unwrap();
        &self.crates[index]
    }
    pub fn krate(&self, def_id: &DefId) -> &HirCrate {
        &self.crates[def_id.crate_id().as_usize()]
    }
    pub fn krates(&self) -> impl Iterator<Item = &HirCrate> {
        self.crates.iter()
    }

    pub fn into_krates(self) -> impl Iterator<Item = HirCrate<'a>> + Debug {
        self.crates.into_iter()
    }
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct HirCrate<'a> {
    pub name: InternedStr,
    pub id: CrateId,
    pub items: Vec<LocalDefId>,
    #[cfg(not(test))]
    pub nodes: LDefMap<Node<'a>>,
    #[cfg(test)]
    pub nodes: BTreeMap<LocalDefId, Node<'a>>,
}
