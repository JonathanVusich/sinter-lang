use std::borrow::Borrow;
use std::collections::{BTreeMap, HashMap};
use std::fmt::{Debug, Formatter};
use std::ops::{Deref, Index};
use std::sync::Arc;

use serde::{Deserialize, Serialize};

use crate::compiler::ast::{Ident, InfixOp, Module, Mutability, UnaryOp};
use crate::compiler::krate::{Crate, CrateId};
use crate::compiler::parser::ClassType;
use crate::compiler::path::ModulePath;
use crate::compiler::resolver::{FnDef, MaybeFnDef, ValueDef};
use crate::compiler::tokens::tokenized_file::Span;
use crate::compiler::types::{InternedStr, LDefMap, StrMap};
use crate::compiler::utils::{named_slice, named_strmap};
use crate::traits::traits::Trait;

macro_rules! def_node_getter {
    ($fn_name:ident, $maybe_fn_name:ident, $patt:pat, $extractor:expr, $node_ty:ty) => {
        impl HirCrate {
            pub fn $fn_name(&self, node_id: &LocalDefId) -> $node_ty {
                match self.nodes.get(node_id) {
                    Some(HirNode { kind: $patt, .. }) => $extractor,
                    _ => unreachable!(),
                }
            }
            
            pub fn $maybe_fn_name(&self, node_id: &LocalDefId) -> Option<$node_ty> {
                match self.nodes.get(node_id) {
                    Some(HirNode { kind: $patt, .. }) => Some($extractor),
                    _ => None,
                }
            }
        }
    };
}

#[derive(PartialEq, Eq, Debug, Default, Clone, Copy, Serialize, Deserialize)]
pub struct DefId {
    crate_id: u32,
    local_id: u32,
}

impl DefId {
    pub fn crate_id(&self) -> usize {
        self.crate_id as usize
    }

    pub fn local_id(&self) -> LocalDefId {
        LocalDefId::new(self.local_id)
    }
}

#[derive(PartialEq, Eq, Debug, Hash, Default, Copy, Clone, Serialize, Deserialize)]
pub struct ModuleId {
    crate_id: u32,
    module_id: u32,
}

impl Index<ModuleId> for Vec<Module> {
    type Output = Module;

    fn index(&self, index: ModuleId) -> &Self::Output {
        &self[index.module_id as usize]
    }
}

impl<'a> Index<ModuleId> for Vec<&'a Crate> {
    type Output = Module;

    fn index(&self, index: ModuleId) -> &Self::Output {
        &self[index.crate_id as usize][index]
    }
}

impl ModuleId {
    pub fn new(crate_id: u32, module_id: u32) -> Self {
        Self {
            crate_id,
            module_id,
        }
    }
}

#[repr(transparent)]
#[derive(
    Copy, Clone, PartialEq, Eq, Ord, Default, PartialOrd, Debug, Hash, Serialize, Deserialize,
)]
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

impl From<LocalDefId> for usize {
    fn from(value: LocalDefId) -> Self {
        value.local_id.try_into().unwrap()
    }
}

impl From<u32> for LocalDefId {
    fn from(value: u32) -> Self {
        LocalDefId { local_id: value }
    }
}

#[derive(PartialEq, Clone, Serialize, Deserialize)]
pub struct HirNode {
    pub(crate) kind: HirNodeKind,
    pub(crate) span: Span,
    pub(crate) id: LocalDefId,
}

impl Debug for HirNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.kind.fmt(f)
    }
}

impl HirNode {
    pub fn new(kind: HirNodeKind, span: Span, id: LocalDefId) -> Self {
        Self { kind, span, id }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
pub enum HirNodeKind {
    GlobalLet(GlobalLetStmt),
    Class(ClassStmt),
    Enum(EnumStmt),
    Trait(TraitStmt),
    TraitImpl(TraitImplStmt),
    Fn(FnStmt),

    EnumMember(EnumMember),

    Expr(Expr),
    Ty(Ty),
    DestructureExpr(DestructureExpr),
    Stmt(Stmt),
    Block(Block),

    Param(Param),
    GenericParam(GenericParam),
    Field(Field),
    Pattern(Pattern),
    MatchArm(MatchArm),
}

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
pub struct GlobalLetStmt {
    pub ident: InternedStr,
    pub ty: LocalDefId,
    pub initializer: LocalDefId,
}

impl GlobalLetStmt {
    pub fn new(ident: InternedStr, ty: LocalDefId, initializer: LocalDefId) -> Self {
        Self {
            ident,
            ty,
            initializer,
        }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
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

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
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

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
pub struct EnumMember {
    pub name: InternedStr,
    pub fields: Fields,
    pub member_fns: FnStmts,
}

impl EnumMember {
    pub fn new(name: InternedStr, fields: Fields, member_fns: FnStmts) -> Self {
        Self {
            name,
            fields,
            member_fns,
        }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
pub struct TraitStmt {
    pub name: Ident,
    pub generic_params: GenericParams,
    pub member_fns: FnStmts,
}

impl TraitStmt {
    pub fn new(name: Ident, generic_params: GenericParams, member_fns: FnStmts) -> Self {
        Self {
            name,
            generic_params,
            member_fns,
        }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
pub struct TraitImplStmt {
    pub trait_to_impl: PathTy,
    pub target_ty: DefId,
    pub member_fns: FnStmts,
}

impl TraitImplStmt {
    pub fn new(trait_to_impl: PathTy, target_ty: DefId, member_fns: FnStmts) -> Self {
        Self {
            trait_to_impl,
            target_ty,
            member_fns,
        }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
pub struct FnStmt {
    pub(crate) sig: FnSig,
    pub(crate) body: Option<LocalDefId>,
}

impl FnStmt {
    pub fn new(sig: FnSig, body: Option<LocalDefId>) -> Self {
        Self { sig, body }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
pub struct FnSig {
    pub(crate) name: Ident,
    pub(crate) generic_params: GenericParams,
    pub(crate) params: Params,
    pub(crate) return_type: Option<LocalDefId>,
}

impl FnSig {
    pub fn new(
        name: Ident,
        generic_params: GenericParams,
        params: Params,
        return_type: Option<LocalDefId>,
    ) -> Self {
        Self {
            name,
            generic_params,
            params,
            return_type,
        }
    }
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

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
pub struct Block {
    pub stmts: Stmts,
}

impl Block {
    pub fn new(stmts: Stmts) -> Self {
        Self { stmts }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct Expression {
    pub(crate) expr: LocalDefId,
    pub(crate) implicit_return: bool,
}

impl Expression {
    pub fn new(expr: LocalDefId, implicit_return: bool) -> Self {
        Self {
            expr,
            implicit_return,
        }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
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

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
//noinspection DuplicatedCode
pub enum Ty {
    Array {
        ty: LocalDefId,
    },
    Path {
        path: PathTy,
    },
    TraitBound {
        trait_bound: TraitBound,
    },
    Closure {
        params: AnonParams,
        ret_ty: LocalDefId,
    },
    Primitive(Primitive),
}

impl Ty {
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

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
pub enum Stmt {
    Let(LetStmt),
    For(ForStmt),
    If(IfStmt),
    Return(ReturnStmt),
    While(WhileStmt),
    Block(LocalDefId),
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

named_slice!(TraitBound, PathTy);
named_slice!(Generics, LocalDefId);
named_slice!(Args, LocalDefId);
named_slice!(Stmts, LocalDefId);
named_slice!(AnonParams, LocalDefId);
named_slice!(Initializers, LocalDefId);
named_slice!(Exprs, LocalDefId);
named_strmap!(ClosureParams, ClosureParam);
named_strmap!(GenericParams, LocalDefId);
named_strmap!(Params, LocalDefId);
named_strmap!(Fields, LocalDefId);
named_strmap!(FnStmts, LocalDefId);
named_strmap!(EnumMembers, LocalDefId);

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
pub enum Expr {
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
    Path(PathExpr),
    Break,
    Continue,
}

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
pub enum DestructureExpr {
    Pattern(DestructurePattern),
    Identifier(InternedStr),
    None,
    True,
    False,
    Int(i64),
    UInt(u64),
    Float(f64),
    String(InternedStr),
}

#[derive(PartialEq, Debug, Clone, Copy, Serialize, Deserialize)]
pub enum Literal {
    None,
    True,
    False,
    Integer(i64),
    Float(f64),
    String(InternedStr),
}

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
pub enum ArrayExpr {
    Sized {
        initializer: LocalDefId,
        size: LocalDefId,
    },
    Unsized {
        initializers: Initializers,
    },
}
#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
pub struct CallExpr {
    pub target: LocalDefId,
    pub args: Args,
}

impl CallExpr {
    pub fn new(target: LocalDefId, args: Args) -> Self {
        Self { target, args }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
pub struct UnaryExpr {
    pub operator: UnaryOp,
    pub expr: LocalDefId,
}

impl UnaryExpr {
    pub fn new(operator: UnaryOp, expr: LocalDefId) -> Self {
        Self { operator, expr }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
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

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
pub struct MatchExpr {
    pub source: LocalDefId,
    pub arms: Vec<MatchArm>,
}

impl MatchExpr {
    pub fn new(source: LocalDefId, arms: Vec<MatchArm>) -> Self {
        Self { source, arms }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub body: LocalDefId,
}

impl MatchArm {
    pub fn new(pattern: Pattern, body: LocalDefId) -> Self {
        Self { pattern, body }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
pub enum Pattern {
    Wildcard,
    Or(OrPattern),
    None,
    True,
    False,
    Int(i64),
    UInt(u64),
    Float(f64),
    String(InternedStr),
    Ty(TyPattern),
    Destructure(DestructurePattern),
}

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
pub struct OrPattern {
    patterns: Vec<Pattern>,
}

impl OrPattern {
    pub fn new(patterns: Vec<Pattern>) -> Self {
        Self { patterns }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
pub struct TyPattern {
    pub ty: PathTy,
    pub ident: Option<PatternLocal>,
}

impl TyPattern {
    pub fn new(ty: PathTy, ident: Option<PatternLocal>) -> Self {
        Self { ty, ident }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
#[repr(transparent)]
#[serde(transparent)]
pub struct PatternLocal {
    pub ident: InternedStr,
}

impl PatternLocal {
    pub fn new(ident: InternedStr) -> Self {
        Self { ident }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
pub struct DestructurePattern {
    pub ty: PathTy,
    pub exprs: Exprs,
}

impl DestructurePattern {
    pub fn new(ty: PathTy, exprs: Vec<LocalDefId>) -> Self {
        Self {
            ty,
            exprs: Exprs::from(exprs),
        }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
pub struct ClosureExpr {
    pub params: ClosureParams,
    pub stmt: LocalDefId,
}

impl ClosureExpr {
    pub fn new(params: ClosureParams, stmt: LocalDefId) -> Self {
        Self { params, stmt }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
pub struct AssignExpr {
    pub lhs: LocalDefId,
    pub rhs: LocalDefId,
}

impl AssignExpr {
    pub fn new(lhs: LocalDefId, rhs: LocalDefId) -> Self {
        Self { lhs, rhs }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
pub struct FieldExpr {
    pub lhs: LocalDefId,
    pub ident: Ident,
}

impl FieldExpr {
    pub fn new(lhs: LocalDefId, ident: Ident) -> Self {
        Self { lhs, ident }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
pub struct IndexExpr {
    pub expr: LocalDefId,
    pub key: LocalDefId,
}

impl IndexExpr {
    pub fn new(expr: LocalDefId, key: LocalDefId) -> Self {
        Self { expr, key }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
pub struct PathExpr {
    pub segments: Arc<[Segment]>,
}

impl PathExpr {
    pub fn new(segments: Vec<Segment>) -> Self {
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

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
pub enum Res {
    Crate(CrateId),
    ModuleSegment(CrateId, ModulePath),
    Module(ModuleId),
    ValueDef(ValueDef),
    Fn(MaybeFnDef), // These have to be late resolved after type information is deduced?
    Local(LocalDef),
    Primitive(Primitive),
}

#[derive(PartialEq, Eq, Debug, Clone, Copy, Serialize, Deserialize)]
pub enum LocalDef {
    Var(LocalDefId),
    Generic(LocalDefId),
}

#[derive(PartialEq, Eq, Debug, Clone, Copy, Serialize, Deserialize)]
pub enum DefTy {
    GlobalLet,
    Class,
    Enum,
    EnumMember,
    Trait,
    Fn,
}

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
pub struct Segment {
    pub res: Res,
    pub generics: Option<Generics>,
}

impl Segment {
    pub fn new(res: Res, generics: Option<Generics>) -> Self {
        Self { res, generics }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
pub struct Field {
    pub(crate) ident: Ident,
    pub(crate) ty: LocalDefId,
}

impl Field {
    pub fn new(ident: Ident, ty: LocalDefId) -> Self {
        Self { ident, ty }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
pub struct LetStmt {
    pub ident: Ident,
    pub mutability: Mutability,
    pub ty: Option<LocalDefId>,
    pub initializer: Option<LocalDefId>,
}

impl LetStmt {
    pub fn new(
        ident: Ident,
        mutability: Mutability,
        ty: Option<LocalDefId>,
        initializer: Option<LocalDefId>,
    ) -> Self {
        Self {
            ident,
            mutability,
            ty,
            initializer,
        }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
pub struct ReturnStmt {
    pub value: Option<LocalDefId>,
}

impl ReturnStmt {
    pub fn new(value: Option<LocalDefId>) -> Self {
        Self { value }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
pub struct WhileStmt {
    pub condition: LocalDefId,
    pub block: LocalDefId,
}

impl WhileStmt {
    pub fn new(condition: LocalDefId, block: LocalDefId) -> Self {
        Self { condition, block }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
pub struct ForStmt {
    pub ident: Ident,
    pub range: LocalDefId,
    pub body: LocalDefId,
}

impl ForStmt {
    pub fn new(ident: Ident, range: LocalDefId, body: LocalDefId) -> Self {
        Self { ident, range, body }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
pub struct IfStmt {
    condition: LocalDefId,
    if_true: LocalDefId,
    if_false: Option<LocalDefId>,
}

impl IfStmt {
    pub fn new(condition: LocalDefId, if_true: LocalDefId, if_false: Option<LocalDefId>) -> Self {
        Self {
            condition,
            if_true,
            if_false,
        }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
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

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
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
pub struct HirCrate {
    pub(crate) name: InternedStr,
    pub(crate) id: CrateId,
    pub(crate) items: Vec<LocalDefId>,
    #[cfg(not(test))]
    pub(crate) nodes: LDefMap<HirNode>,
    #[cfg(test)]
    pub(crate) nodes: BTreeMap<LocalDefId, HirNode>,
}

def_node_getter!(
    let_stmt,
    maybe_let_stmt,
    HirNodeKind::Stmt(Stmt::Let(let_stmt)),
    let_stmt,
    &LetStmt
);
def_node_getter!(fn_stmt, maybe_fn_stmt, HirNodeKind::Fn(fn_stmt), fn_stmt, &FnStmt);
def_node_getter!(ty_stmt, maybe_ty_stmt, HirNodeKind::Ty(ty), ty, &Ty);
def_node_getter!(
    global_let_stmt,
    maybe_global_let_stmt,
    HirNodeKind::GlobalLet(global_let_stmt),
    global_let_stmt,
    &GlobalLetStmt
);
def_node_getter!(
    class_stmt,
    maybe_class_stmt,
    HirNodeKind::Class(class_stmt),
    class_stmt,
    &ClassStmt
);
def_node_getter!(
    enum_member,
    maybe_enum_member,
    HirNodeKind::EnumMember(enum_member),
    enum_member,
    &EnumMember
);

def_node_getter!(
    trait_bound,
    maybe_trait_bound,
    HirNodeKind::Ty(Ty::TraitBound { trait_bound }),
    trait_bound,
    &TraitBound
);

def_node_getter!(param, maybe_param, HirNodeKind::Param(param), param, &Param);
def_node_getter!(
    generic_param,
    maybe_generic_param,
    HirNodeKind::GenericParam(generic_param),
    generic_param,
    &GenericParam
);
def_node_getter!(field, maybe_field, HirNodeKind::Field(field), field, &Field);

impl HirCrate {
    pub fn new(
        name: InternedStr,
        id: CrateId,
        items: Vec<LocalDefId>,
        nodes: LDefMap<HirNode>,
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

    pub fn node(&self, node: LocalDefId) -> &HirNodeKind {
        self.nodes.get(&node).map(|node| &node.kind).unwrap()
    }

    pub fn stmt(&self, stmt_id: LocalDefId) -> &Stmt {
        match self.nodes.get(&stmt_id) {
            Some(HirNode {
                kind: HirNodeKind::Stmt(stmt),
                ..
            }) => stmt,
            _ => unreachable!(),
        }
    }
}
