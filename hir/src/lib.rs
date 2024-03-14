#![allow(unused)]

use std::collections::BTreeMap;
use std::fmt::{Debug, Formatter};
use std::ops::Deref;
use std::sync::Arc;

use serde::{Deserialize, Serialize};

use ast::{ClassType, Ident, InfixOp, MaybeFnDef, ModulePath, Mutability, UnaryOp, ValueDef};
use id::{CrateId, DefId, LocalDefId, ModuleId};
use interner::InternedStr;
use macros::{named_slice, named_strmap};
use span::Span;
use types::{LDefMap, StrMap, StrSet};

macro_rules! def_node_getter {
    ($fn_name:ident, $maybe_fn_name:ident, $patt:pat, $extractor:expr, $node_ty:ty) => {
        impl<'a> HirCrate<'a> {
            pub fn $fn_name(&self, node_id: &&'a HirNode<'a>) -> $node_ty {
                match self.nodes.get(node_id) {
                    Some(HirNode { kind: $patt, .. }) => $extractor,
                    _ => unreachable!(),
                }
            }

            pub fn $maybe_fn_name(&self, node_id: &&'a HirNode<'a>) -> Option<$node_ty> {
                match self.nodes.get(node_id) {
                    Some(HirNode { kind: $patt, .. }) => Some($extractor),
                    _ => None,
                }
            }
        }
    };
}

#[derive(PartialEq, Clone, Serialize)]
pub struct HirNode<'a> {
    pub(crate) kind: HirNodeKind<'a>,
    pub(crate) span: Span,
    pub(crate) id: LocalDefId,
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

#[derive(PartialEq, Debug, Clone, Serialize)]
pub enum HirNodeKind<'a> {
    GlobalLet(GlobalLetStmt<'a>),
    Class(ClassStmt<'a>),
    Enum(EnumStmt<'a>),
    Trait(TraitStmt<'a>),
    TraitImpl(TraitImplStmt<'a>),
    Fn(FnStmt<'a>),

    EnumMember(EnumMember<'a>),

    Expr(Expr<'a>),
    Ty(Ty<'a>),
    DestructureExpr(DestructureExpr<'a>),
    Stmt(Stmt<'a>),
    Block(Block<'a>),

    Param(Param<'a>),
    Field(Field<'a>),
    LocalVar(LocalVar),
    Pattern(Pattern<'a>),
    MatchArm(MatchArm<'a>),
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct GlobalLetStmt<'a> {
    pub local_var: &'a HirNode<'a>,
    pub ty: &'a HirNode<'a>,
    pub initializer: &'a HirNode<'a>,
}

impl<'a> GlobalLetStmt<'a> {
    pub fn new(
        local_var: &'a HirNode<'a>,
        ty: &'a HirNode<'a>,
        initializer: &'a HirNode<'a>,
    ) -> Self {
        Self {
            local_var,
            ty,
            initializer,
        }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct ClassStmt<'a> {
    pub name: Ident,
    pub class_type: ClassType,
    pub generic_params: GenericParams<'a>,
    pub fields: Fields<'a>,
    pub fn_stmts: FnStmts<'a>,
}

impl<'a> ClassStmt<'a> {
    pub fn new(
        name: Ident,
        class_type: ClassType,
        generic_params: GenericParams<'a>,
        fields: Fields<'a>,
        fn_stmts: FnStmts<'a>,
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

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct EnumStmt<'a> {
    pub name: Ident,
    pub generic_params: GenericParams<'a>,
    pub members: EnumMembers<'a>,
    pub member_fns: FnStmts<'a>,
}

impl<'a> EnumStmt<'a> {
    pub fn new(
        name: Ident,
        generic_params: GenericParams<'a>,
        members: EnumMembers<'a>,
        member_fns: FnStmts<'a>,
    ) -> Self {
        Self {
            name,
            generic_params,
            members,
            member_fns,
        }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct EnumMember<'a> {
    pub name: InternedStr,
    pub fields: Fields<'a>,
    pub member_fns: FnStmts<'a>,
}

impl<'a> EnumMember<'a> {
    pub fn new(name: InternedStr, fields: Fields<'a>, member_fns: FnStmts<'a>) -> Self {
        Self {
            name,
            fields,
            member_fns,
        }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct TraitStmt<'a> {
    pub name: Ident,
    pub generic_params: GenericParams<'a>,
    pub member_fns: FnStmts<'a>,
}

impl<'a> TraitStmt<'a> {
    pub fn new(name: Ident, generic_params: GenericParams<'a>, member_fns: FnStmts<'a>) -> Self {
        Self {
            name,
            generic_params,
            member_fns,
        }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct TraitImplStmt<'a> {
    pub trait_to_impl: PathTy<'a>,
    pub target_ty: DefId,
    pub member_fns: FnStmts<'a>,
}

impl<'a> TraitImplStmt<'a> {
    pub fn new(trait_to_impl: PathTy<'a>, target_ty: DefId, member_fns: FnStmts<'a>) -> Self {
        Self {
            trait_to_impl,
            target_ty,
            member_fns,
        }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct FnStmt<'a> {
    pub sig: FnSig<'a>,
    pub body: Option<&'a HirNode<'a>>,
}

impl<'a> FnStmt<'a> {
    pub fn new(sig: FnSig<'a>, body: Option<&'a HirNode<'a>>) -> Self {
        Self { sig, body }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct FnSig<'a> {
    pub name: Ident,
    pub generic_params: GenericParams<'a>,
    pub params: Params<'a>,
    pub return_type: Option<&'a HirNode<'a>>,
}

impl<'a> FnSig<'a> {
    pub fn new(
        name: Ident,
        generic_params: GenericParams<'a>,
        params: Params<'a>,
        return_type: Option<&'a HirNode<'a>>,
    ) -> Self {
        Self {
            name,
            generic_params,
            params,
            return_type,
        }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct Param<'a> {
    pub ident: Ident,
    pub ty: &'a HirNode<'a>,
    pub mutability: Mutability,
}

impl<'a> Param<'a> {
    pub fn new(ident: Ident, ty: &'a HirNode<'a>, mutability: Mutability) -> Self {
        Self {
            ident,
            ty,
            mutability,
        }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct Block<'a> {
    pub stmts: Stmts<'a>,
}

impl<'a> Block<'a> {
    pub fn new(stmts: Stmts<'a>) -> Self {
        Self { stmts }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize)]
pub struct Expression<'a> {
    pub expr: &'a HirNode<'a>,
    pub implicit_return: bool,
}

impl<'a> Expression<'a> {
    pub fn new(expr: &'a HirNode<'a>, implicit_return: bool) -> Self {
        Self {
            expr,
            implicit_return,
        }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize)]
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

#[derive(PartialEq, Debug, Clone, Serialize)]
//noinspection DuplicatedCode
pub enum Ty<'a> {
    Array(Array<'a>),
    Path(PathTy<'a>),
    GenericParam(GenericParam<'a>),
    TraitBound(TraitBound<'a>),
    Closure(Closure<'a>),
    Primitive(Primitive),
}

impl<'a> Ty<'a> {
    fn contains_ty(&self, other: &Ty) -> bool {
        todo!()
        // match self {
        //     Ty::Array { .. } => {}
        //     Ty::Path { .. } => {}
        //     Ty::TraitBound { .. } => {}
        //     Ty::Closure { .. } => {}
        //     Ty::Infer => {}
        //     Ty::QSelf => {}
        //     Ty::U8 => {}
        //     Ty::U16 => {}
        //     Ty::U32 => {}
        //     Ty::U64 => {}
        //     Ty::I8 => {}
        //     Ty::I16 => {}
        //     Ty::I32 => {}
        //     Ty::I64 => {}
        //     Ty::F32 => {}
        //     Ty::F64 => {}
        //     Ty::Str => {}
        //     Ty::Boolean => {}
        //     Ty::None => {}
        // }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub enum Stmt<'a> {
    Let(LetStmt<'a>),
    For(ForStmt<'a>),
    If(IfStmt<'a>),
    Return(ReturnStmt<'a>),
    While(WhileStmt<'a>),
    Block(&'a HirNode<'a>),
    Expression(Expression<'a>),
}

#[derive(PartialEq, Debug, Clone, Serialize)]
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

named_slice!('a, TraitBound, PathTy<'a>);
named_slice!('a, Generics, &'a HirNode<'a>);
named_slice!('a, Args, &'a HirNode<'a>);
named_slice!('a, Stmts, &'a HirNode<'a>);
named_slice!('a, AnonParams, &'a HirNode<'a>);
named_slice!('a, Initializers, &'a HirNode<'a>);
named_slice!('a, Exprs, &'a HirNode<'a>);
named_strmap!(ClosureParams, ClosureParam);
named_strmap!('a, GenericParams, &'a GenericParam<'a>);
named_strmap!('a, Params, &'a Param<'a>);
named_strmap!('a, Fields, &'a Field<'a>);
named_strmap!('a, FnStmts, &'a FnStmt<'a>);
named_strmap!('a, EnumMembers, &'a EnumMember<'a>);

#[derive(PartialEq, Debug, Clone, Serialize)]
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

#[derive(PartialEq, Debug, Clone, Serialize)]
pub enum DestructureExpr<'a> {
    Pattern(DestructurePattern<'a>),
    Identifier(InternedStr),
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

#[derive(PartialEq, Debug, Clone, Serialize)]
pub enum ArrayExpr<'a> {
    Sized {
        initializer: &'a HirNode<'a>,
        size: &'a HirNode<'a>,
    },
    Unsized {
        initializers: Initializers<'a>,
    },
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct CallExpr<'a> {
    pub target: &'a HirNode<'a>,
    pub args: Args<'a>,
}

impl<'a> CallExpr<'a> {
    pub fn new(target: &'a HirNode<'a>, args: Args<'a>) -> Self {
        Self { target, args }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct UnaryExpr<'a> {
    pub operator: UnaryOp,
    pub expr: &'a HirNode<'a>,
}

impl<'a> UnaryExpr<'a> {
    pub fn new(operator: UnaryOp, expr: &'a HirNode<'a>) -> Self {
        Self { operator, expr }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct InfixExpr<'a> {
    pub operator: InfixOp,
    pub lhs: &'a HirNode<'a>,
    pub rhs: &'a HirNode<'a>,
}

impl<'a> InfixExpr<'a> {
    pub fn new(operator: InfixOp, lhs: &'a HirNode<'a>, rhs: &'a HirNode<'a>) -> Self {
        Self { operator, lhs, rhs }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct MatchExpr<'a> {
    pub source: &'a HirNode<'a>,
    pub arms: Vec<MatchArm<'a>>,
}

impl<'a> MatchExpr<'a> {
    pub fn new(source: &'a HirNode<'a>, arms: Vec<MatchArm<'a>>) -> Self {
        Self { source, arms }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct MatchArm<'a> {
    pub pattern: Pattern<'a>,
    pub body: &'a HirNode<'a>,
}

impl<'a> MatchArm<'a> {
    pub fn new(pattern: Pattern<'a>, body: &'a HirNode<'a>) -> Self {
        Self { pattern, body }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize)]
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

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct OrPattern<'a> {
    patterns: Vec<Pattern<'a>>,
}

impl<'a> OrPattern<'a> {
    pub fn new(patterns: Vec<Pattern<'a>>) -> Self {
        Self { patterns }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct TyPattern<'a> {
    pub ty: PathTy<'a>,
    pub ident: Option<LocalVar>,
}

impl<'a> TyPattern<'a> {
    pub fn new(ty: PathTy<'a>, ident: Option<LocalVar>) -> Self {
        Self { ty, ident }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize)]
#[repr(transparent)]
#[serde(transparent)]
pub struct LocalVar {
    pub ident: InternedStr,
}

impl LocalVar {
    pub fn new(ident: InternedStr) -> Self {
        Self { ident }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct DestructurePattern<'a> {
    pub ty: PathTy<'a>,
    pub exprs: Exprs<'a>,
}

impl<'a> DestructurePattern<'a> {
    pub fn new(ty: PathTy<'a>, exprs: Vec<&'a HirNode<'a>>) -> Self {
        Self {
            ty,
            exprs: Exprs::from(exprs),
        }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct Array<'a> {
    pub ty: &'a HirNode<'a>,
}

impl<'a> Array<'a> {
    pub fn new(ty: &'a HirNode<'a>) -> Self {
        Self { ty }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct Closure<'a> {
    pub params: AnonParams<'a>,
    pub ret_ty: &'a HirNode<'a>,
}

impl<'a> Closure<'a> {
    pub fn new(params: AnonParams<'a>, ret_ty: &'a HirNode<'a>) -> Self {
        Self { params, ret_ty }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct ClosureExpr<'a> {
    pub params: ClosureParams,
    pub stmt: &'a HirNode<'a>,
}

impl<'a> ClosureExpr<'a> {
    pub fn new(params: ClosureParams, stmt: &'a HirNode<'a>) -> Self {
        Self { params, stmt }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct AssignExpr<'a> {
    pub lhs: &'a HirNode<'a>,
    pub rhs: &'a HirNode<'a>,
}

impl<'a> AssignExpr<'a> {
    pub fn new(lhs: &'a HirNode<'a>, rhs: &'a HirNode<'a>) -> Self {
        Self { lhs, rhs }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct FieldExpr<'a> {
    pub lhs: &'a HirNode<'a>,
    pub ident: Ident,
}

impl<'a> FieldExpr<'a> {
    pub fn new(lhs: &'a HirNode<'a>, ident: Ident) -> Self {
        Self { lhs, ident }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct IndexExpr<'a> {
    pub expr: &'a HirNode<'a>,
    pub key: &'a HirNode<'a>,
}

impl<'a> IndexExpr<'a> {
    pub fn new(expr: &'a HirNode<'a>, key: &'a HirNode<'a>) -> Self {
        Self { expr, key }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct PathExpr<'a> {
    pub segments: Arc<[Segment<'a>]>,
}

impl<'a> PathExpr<'a> {
    pub fn new(segments: Vec<Segment<'a>>) -> Self {
        Self {
            segments: segments.into(),
        }
    }

    pub fn is_single(&self) -> Option<&Segment> {
        if self.segments.len() == 1 {
            return self.segments.first();
        }
        None
    }
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub enum Res<'a> {
    Crate(CrateId),
    ModuleSegment(CrateId, ModulePath),
    Module(ModuleId),
    ValueDef(ValueDef),
    Fn(MaybeFnDef),
    // These have to be late resolved after type information is deduced?
    Local(LocalDef<'a>),
    Primitive(Primitive),
}

#[derive(PartialEq, Debug, Clone, Copy, Serialize)]
pub enum LocalDef<'a> {
    Var(&'a HirNode<'a>),
    Generic(&'a HirNode<'a>),
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

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct Segment<'a> {
    pub res: Res<'a>,
    pub generics: Option<Generics<'a>>,
}

impl<'a> Segment<'a> {
    pub fn new(res: Res<'a>, generics: Option<Generics<'a>>) -> Self {
        Self { res, generics }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct Field<'a> {
    pub ident: Ident,
    pub ty: &'a HirNode<'a>,
}

impl<'a> Field<'a> {
    pub fn new(ident: Ident, ty: &'a HirNode<'a>) -> Self {
        Self { ident, ty }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct LetStmt<'a> {
    pub local_var: &'a HirNode<'a>,
    pub mutability: Mutability,
    pub ty: Option<&'a HirNode<'a>>,
    pub initializer: Option<&'a HirNode<'a>>,
}

impl<'a> LetStmt<'a> {
    pub fn new(
        local_var: &'a HirNode<'a>,
        mutability: Mutability,
        ty: Option<&'a HirNode<'a>>,
        initializer: Option<&'a HirNode<'a>>,
    ) -> Self {
        Self {
            local_var,
            mutability,
            ty,
            initializer,
        }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct ReturnStmt<'a> {
    pub value: Option<&'a HirNode<'a>>,
}

impl<'a> ReturnStmt<'a> {
    pub fn new(value: Option<&'a HirNode<'a>>) -> Self {
        Self { value }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct WhileStmt<'a> {
    pub condition: &'a HirNode<'a>,
    pub block: &'a HirNode<'a>,
}

impl<'a> WhileStmt<'a> {
    pub fn new(condition: &'a HirNode<'a>, block: &'a HirNode<'a>) -> Self {
        Self { condition, block }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct ForStmt<'a> {
    pub ident: Ident,
    pub range: &'a HirNode<'a>,
    pub body: &'a HirNode<'a>,
}

impl<'a> ForStmt<'a> {
    pub fn new(ident: Ident, range: &'a HirNode<'a>, body: &'a HirNode<'a>) -> Self {
        Self { ident, range, body }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct IfStmt<'a> {
    pub condition: &'a HirNode<'a>,
    pub if_true: &'a HirNode<'a>,
    pub if_false: Option<&'a HirNode<'a>>,
}

impl<'a> IfStmt<'a> {
    pub fn new(
        condition: &'a HirNode<'a>,
        if_true: &'a HirNode<'a>,
        if_false: Option<&'a HirNode<'a>>,
    ) -> Self {
        Self {
            condition,
            if_true,
            if_false,
        }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize)]
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

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct GenericParam<'a> {
    pub ident: Ident,
    pub trait_bound: Option<&'a HirNode<'a>>,
}

impl<'a> GenericParam<'a> {
    pub fn new(ident: Ident, trait_bound: Option<&'a HirNode<'a>>) -> Self {
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
    pub items: Vec<&'a HirNode<'a>>,
    #[cfg(not(test))]
    pub nodes: LDefMap<&'a HirNode<'a>>,
    #[cfg(test)]
    pub nodes: BTreeMap<LocalDefId, &'a HirNode<'a>>,
}

impl<'a> HirCrate<'a> {
    pub fn new(
        name: InternedStr,
        id: CrateId,
        items: Vec<&'a HirNode<'a>>,
        nodes: LDefMap<&'a HirNode<'a>>,
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
