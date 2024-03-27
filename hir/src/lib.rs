#![allow(unused)]

use std::collections::BTreeMap;
use std::fmt::{Debug, Formatter};

use serde::Serialize;

use ast::{ClassType, Ident, InfixOp, MaybeFnDef, ModulePath, Mutability, UnaryOp, ValueDef};
use id::{CrateId, DefId, LocalDefId, ModuleId};
use interner::InternedStr;
use span::Span;
use types::{LDefMap, StrSet};

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

pub struct Item<'a> {
    pub kind: ItemKind<'a>,
    pub owner: Owner,
    // Represents the module that this item resides in.
    pub span: Span,
}

pub enum ItemKind<'a> {
    Constant(&'a Constant<'a>),
    Class(&'a ClassDef<'a>),
    Enum(&'a EnumDef<'a>),
    Fn(&'a FnDef<'a>),
    Trait(&'a TraitDef<'a>),
    TraitImpl(&'a TraitImplDef<'a>),
}

pub struct ExprV2<'a> {
    pub kind: ExprKindV2<'a>,
    pub owner: LocalDefId,
    pub id: LocalDefId,
}

pub enum ExprKindV2<'a> {}

pub enum Owner {
    Crate(CrateId),
    Node(LocalDefId),
}

/// HirNode stores a reference to the underlying node data along with other useful debugging info.
#[derive(PartialEq, Copy, Clone, Serialize)]
pub struct HirNode<'a> {
    pub kind: HirNodeKind<'a>,
    pub span: Span,
    pub id: LocalDefId,
}

impl<'a> Debug for HirNode<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.kind.fmt(f)
    }
}

impl<'a> HirNode<'a> {
    pub fn new(kind: HirNodeKind<'a>, span: Span, id: LocalDefId) -> Self {
        Self { kind, span, id }
    }
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub enum HirNodeKind<'a> {
    GlobalLet(&'a Constant<'a>),
    Class(&'a ClassDef<'a>),
    Enum(&'a EnumDef<'a>),
    Trait(&'a TraitDef<'a>),
    TraitImpl(&'a TraitImplDef<'a>),
    Fn(&'a FnDef<'a>),

    EnumMember(&'a MemberDef<'a>),

    Expr(&'a Expr<'a>),
    Ty(&'a Ty<'a>),
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
pub struct Constant<'a> {
    pub local_var: LocalVar,
    pub ty: &'a Ty<'a>,
    pub initializer: &'a Expr<'a>,
    pub owner: ModuleId,
}

impl<'a> Constant<'a> {
    pub fn new(
        local_var: LocalVar,
        ty: &'a Ty<'a>,
        initializer: &'a Expr<'a>,
        owner: ModuleId,
    ) -> Self {
        Self {
            local_var,
            ty,
            initializer,
            owner,
        }
    }
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct ClassDef<'a> {
    pub name: Ident,
    pub class_type: ClassType,
    pub generic_params: GenericParams<'a>,
    pub fields: Fields<'a>,
    pub fn_stmts: FnStmts<'a>,
    pub owner: CrateId,
}

impl<'a> ClassDef<'a> {
    pub fn new(
        name: Ident,
        class_type: ClassType,
        generic_params: GenericParams<'a>,
        fields: Fields<'a>,
        fn_stmts: FnStmts<'a>,
        owner: CrateId,
    ) -> Self {
        Self {
            name,
            class_type,
            generic_params,
            fields,
            fn_stmts,
            owner,
        }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct EnumDef<'a> {
    pub name: Ident,
    pub generic_params: GenericParams<'a>,
    pub members: MemberDefs<'a>,
    pub member_fns: FnStmts<'a>,
    pub owner: CrateId,
}

impl<'a> EnumDef<'a> {
    pub fn new(
        name: Ident,
        generic_params: GenericParams<'a>,
        members: MemberDefs<'a>,
        member_fns: FnStmts<'a>,
        owner: CrateId,
    ) -> Self {
        Self {
            name,
            generic_params,
            members,
            member_fns,
            owner,
        }
    }
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct MemberDef<'a> {
    pub name: InternedStr,
    pub fields: Fields<'a>,
    pub member_fns: FnStmts<'a>,
    pub owner: LocalDefId,
}

impl<'a> MemberDef<'a> {
    pub fn new(
        name: InternedStr,
        fields: Fields<'a>,
        member_fns: FnStmts<'a>,
        owner: LocalDefId,
    ) -> Self {
        Self {
            name,
            fields,
            member_fns,
            owner,
        }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct TraitDef<'a> {
    pub name: Ident,
    pub generic_params: GenericParams<'a>,
    pub member_fns: FnStmts<'a>,
    pub owner: CrateId,
}

impl<'a> TraitDef<'a> {
    pub fn new(
        name: Ident,
        generic_params: GenericParams<'a>,
        member_fns: FnStmts<'a>,
        owner: CrateId,
    ) -> Self {
        Self {
            name,
            generic_params,
            member_fns,
            owner,
        }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct TraitImplDef<'a> {
    pub trait_to_impl: &'a PathTy<'a>,
    pub target_ty: DefId,
    pub member_fns: FnStmts<'a>,
    pub owner: CrateId,
}

impl<'a> TraitImplDef<'a> {
    pub fn new(
        trait_to_impl: &'a PathTy<'a>,
        target_ty: DefId,
        member_fns: FnStmts<'a>,
        owner: CrateId,
    ) -> Self {
        Self {
            trait_to_impl,
            target_ty,
            member_fns,
            owner,
        }
    }
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct FnDef<'a> {
    pub sig: FnSig<'a>,
    pub body: Option<&'a Block<'a>>,
    pub owner: Owner,
}

impl<'a> FnDef<'a> {
    pub fn new(sig: FnSig<'a>, body: Option<&'a Block<'a>>, owner: Owner) -> Self {
        Self { sig, body, owner }
    }
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct FnSig<'a> {
    pub name: Ident,
    pub generic_params: GenericParams<'a>,
    pub params: Params<'a>,
    pub return_type: Option<&'a Ty<'a>>,
}

impl<'a> FnSig<'a> {
    pub fn new(
        name: Ident,
        generic_params: GenericParams<'a>,
        params: Params<'a>,
        return_type: Option<&'a Ty<'a>>,
    ) -> Self {
        Self {
            name,
            generic_params,
            params,
            return_type,
        }
    }
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct Param<'a> {
    pub local_var: LocalVar,
    pub ty: &'a Ty<'a>,
    pub mutability: Mutability,
}

impl<'a> Param<'a> {
    pub fn new(ident: LocalVar, ty: &'a Ty<'a>, mutability: Mutability) -> Self {
        Self {
            local_var: ident,
            ty,
            mutability,
        }
    }
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct Block<'a> {
    pub stmts: Stmts<'a>,
}

impl<'a> Block<'a> {
    pub fn new(stmts: Stmts<'a>) -> Self {
        Self { stmts }
    }
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct Expression<'a> {
    pub expr: &'a Expr<'a>,
    pub implicit_return: bool,
}

impl<'a> Expression<'a> {
    pub fn new(expr: &'a Expr<'a>, implicit_return: bool) -> Self {
        Self {
            expr,
            implicit_return,
        }
    }
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
//noinspection DuplicatedCode
pub enum Ty<'a> {
    Array(&'a Ty<'a>),
    Path(&'a PathTy<'a>),
    GenericParam(&'a GenericParam<'a>),
    TraitBound(TraitBound<'a>),
    Closure(Closure<'a>),
    Primitive(Primitive),
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub enum Stmt<'a> {
    Let(&'a LetStmt<'a>),
    For(&'a ForStmt<'a>),
    If(&'a IfStmt<'a>),
    Return(&'a ReturnStmt<'a>),
    While(&'a WhileStmt<'a>),
    Block(&'a Block<'a>),
    Expression(&'a Expression<'a>),
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct PathTy<'a> {
    pub definition: DefId,
    pub generics: Generics<'a>,
}

impl<'a> PathTy<'a> {
    pub fn new(definition: DefId, generics: Generics<'a>) -> Self {
        Self {
            definition,
            generics,
        }
    }
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

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub enum Expr<'a> {
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

impl<'a> CallExpr<'a> {
    pub fn new(target: &'a Expr<'a>, args: Args<'a>) -> Self {
        Self { target, args }
    }
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct UnaryExpr<'a> {
    pub operator: UnaryOp,
    pub expr: &'a Expr<'a>,
}

impl<'a> UnaryExpr<'a> {
    pub fn new(operator: UnaryOp, expr: &'a Expr<'a>) -> Self {
        Self { operator, expr }
    }
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct InfixExpr<'a> {
    pub operator: InfixOp,
    pub lhs: &'a Expr<'a>,
    pub rhs: &'a Expr<'a>,
}

impl<'a> InfixExpr<'a> {
    pub fn new(operator: InfixOp, lhs: &'a Expr<'a>, rhs: &'a Expr<'a>) -> Self {
        Self { operator, lhs, rhs }
    }
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct MatchExpr<'a> {
    pub source: &'a Expr<'a>,
    pub arms: MatchArms<'a>,
}

impl<'a> MatchExpr<'a> {
    pub fn new(source: &'a Expr<'a>, arms: MatchArms<'a>) -> Self {
        Self { source, arms }
    }
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct MatchArm<'a> {
    pub pattern: &'a Pattern<'a>,
    pub body: &'a Stmt<'a>,
}

impl<'a> MatchArm<'a> {
    pub fn new(pattern: &'a Pattern<'a>, body: &'a Stmt<'a>) -> Self {
        Self { pattern, body }
    }
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub enum Pattern<'a> {
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
pub struct OrPattern<'a> {
    patterns: Patterns<'a>,
}

impl<'a> OrPattern<'a> {
    pub fn new(patterns: Patterns<'a>) -> Self {
        Self { patterns }
    }
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct TyPattern<'a> {
    pub ty: &'a PathTy<'a>,
    pub ident: Option<LocalVar>,
}

impl<'a> TyPattern<'a> {
    pub fn new(ty: &'a PathTy<'a>, ident: Option<LocalVar>) -> Self {
        Self { ty, ident }
    }
}

#[derive(Copy, Clone, PartialEq, Debug, Serialize)]
pub struct LocalVar {
    pub ident: InternedStr,
    pub id: LocalDefId,
}

impl LocalVar {
    pub fn new(ident: InternedStr, id: LocalDefId) -> Self {
        Self { ident, id }
    }
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct DestructurePattern<'a> {
    pub ty: &'a PathTy<'a>,
    pub exprs: DestructureExprs<'a>,
}

impl<'a> DestructurePattern<'a> {
    pub fn new(ty: &'a PathTy<'a>, exprs: DestructureExprs<'a>) -> Self {
        Self { ty, exprs }
    }
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct Closure<'a> {
    pub params: AnonParams<'a>,
    pub ret_ty: &'a Ty<'a>,
}

impl<'a> Closure<'a> {
    pub fn new(params: AnonParams<'a>, ret_ty: &'a Ty<'a>) -> Self {
        Self { params, ret_ty }
    }
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct ClosureExpr<'a> {
    pub params: ClosureParams<'a>,
    pub stmt: &'a Stmt<'a>,
}

impl<'a> ClosureExpr<'a> {
    pub fn new(params: ClosureParams<'a>, stmt: &'a Stmt<'a>) -> Self {
        Self { params, stmt }
    }
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct AssignExpr<'a> {
    pub lhs: &'a Expr<'a>,
    pub rhs: &'a Expr<'a>,
}

impl<'a> AssignExpr<'a> {
    pub fn new(lhs: &'a Expr<'a>, rhs: &'a Expr<'a>) -> Self {
        Self { lhs, rhs }
    }
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct FieldExpr<'a> {
    pub lhs: &'a Expr<'a>,
    pub ident: Ident,
}

impl<'a> FieldExpr<'a> {
    pub fn new(lhs: &'a Expr<'a>, ident: Ident) -> Self {
        Self { lhs, ident }
    }
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct IndexExpr<'a> {
    pub expr: &'a Expr<'a>,
    pub key: &'a Expr<'a>,
}

impl<'a> IndexExpr<'a> {
    pub fn new(expr: &'a Expr<'a>, key: &'a Expr<'a>) -> Self {
        Self { expr, key }
    }
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct PathExpr<'a> {
    pub segments: Segments<'a>,
}

impl<'a> PathExpr<'a> {
    pub fn new(segments: Segments<'a>) -> Self {
        Self { segments }
    }

    pub fn is_single(&self) -> Option<&'a Segment> {
        if self.segments.len() == 1 {
            return self.segments.first().copied();
        }
        None
    }
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

impl<'a> Segment<'a> {
    pub fn new(res: &'a Res, generics: Option<Generics<'a>>) -> Self {
        Self { res, generics }
    }
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct Field<'a> {
    pub ident: Ident,
    pub ty: &'a Ty<'a>,
}

impl<'a> Field<'a> {
    pub fn new(ident: Ident, ty: &'a Ty<'a>) -> Self {
        Self { ident, ty }
    }
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct LetStmt<'a> {
    pub local_var: LocalVar,
    pub mutability: Mutability,
    pub ty: Option<&'a Ty<'a>>,
    pub initializer: Option<&'a Expr<'a>>,
}

impl<'a> LetStmt<'a> {
    pub fn new(
        local_var: LocalVar,
        mutability: Mutability,
        ty: Option<&'a Ty<'a>>,
        initializer: Option<&'a Expr<'a>>,
    ) -> Self {
        Self {
            local_var,
            mutability,
            ty,
            initializer,
        }
    }
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct ReturnStmt<'a> {
    pub value: Option<&'a Expr<'a>>,
}

impl<'a> ReturnStmt<'a> {
    pub fn new(value: Option<&'a Expr<'a>>) -> Self {
        Self { value }
    }
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct WhileStmt<'a> {
    pub condition: &'a Expr<'a>,
    pub block: &'a Block<'a>,
}

impl<'a> WhileStmt<'a> {
    pub fn new(condition: &'a Expr<'a>, block: &'a Block<'a>) -> Self {
        Self { condition, block }
    }
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct ForStmt<'a> {
    pub ident: LocalVar,
    pub range: &'a Expr<'a>,
    pub body: &'a Block<'a>,
}

impl<'a> ForStmt<'a> {
    pub fn new(ident: LocalVar, range: &'a Expr<'a>, body: &'a Block<'a>) -> Self {
        Self { ident, range, body }
    }
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct IfStmt<'a> {
    pub condition: &'a Expr<'a>,
    pub if_true: &'a Block<'a>,
    pub if_false: Option<&'a Block<'a>>,
}

impl<'a> IfStmt<'a> {
    pub fn new(
        condition: &'a Expr<'a>,
        if_true: &'a Block<'a>,
        if_false: Option<&'a Block<'a>>,
    ) -> Self {
        Self {
            condition,
            if_true,
            if_false,
        }
    }
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
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

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct GenericParam<'a> {
    pub ident: Ident,
    pub trait_bound: Option<TraitBound<'a>>,
}

impl<'a> GenericParam<'a> {
    pub fn new(ident: Ident, trait_bound: Option<TraitBound<'a>>) -> Self {
        Self { ident, trait_bound }
    }
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
    pub nodes: LDefMap<HirNode<'a>>,
    #[cfg(test)]
    pub nodes: BTreeMap<LocalDefId, HirNode<'a>>,
}

impl<'a> HirCrate<'a> {
    pub fn new(
        name: InternedStr,
        id: CrateId,
        items: Vec<LocalDefId>,
        nodes: LDefMap<HirNode<'a>>,
    ) -> Self {
        #[cfg(test)]
        let nodes = BTreeMap::from_iter(nodes);
        Self {
            name,
            id,
            items,
            nodes,
        }
    }
}
