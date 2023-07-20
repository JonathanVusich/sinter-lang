use std::collections::{BTreeMap, HashMap, HashSet};
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};
use std::slice::SliceIndex;

use serde::{Deserialize, Serialize};

use crate::compiler::ast::{Ident, InfixOp, Mutability, QualifiedIdent, UnaryOp};
use crate::compiler::krate::CrateId;
use crate::compiler::parser::ClassType;
use crate::compiler::path::ModulePath;
use crate::compiler::tokens::tokenized_file::Span;
use crate::compiler::types::{InternedStr, StrMap};
use crate::traits::traits::Trait;

#[derive(PartialEq, Eq, Debug, Clone, Copy, Serialize, Deserialize)]
pub struct DefId {
    crate_id: u32,
    local_id: u32,
}

impl DefId {
    pub fn crate_id(&self) -> usize {
        self.crate_id as usize
    }

    pub fn local_id(&self) -> usize {
        self.local_id as usize
    }
}

#[derive(PartialEq, Eq, Debug, Copy, Clone, Serialize, Deserialize)]
pub struct ModuleId {
    crate_id: u32,
    module_id: u32,
}

impl ModuleId {
    pub fn new(crate_id: u32, module_id: u32) -> Self {
        Self {
            crate_id,
            module_id,
        }
    }

    pub fn crate_id(&self) -> usize {
        self.crate_id as usize
    }

    pub fn module_id(&self) -> usize {
        self.module_id as usize
    }
}

#[repr(transparent)]
#[derive(Copy, Clone, PartialEq, Eq, Ord, PartialOrd, Debug, Hash, Serialize, Deserialize)]
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

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
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
    Stmt(Stmt),
    Ty(Ty),
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
    pub ty: Option<LocalDefId>,
    pub initializer: LocalDefId,
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

#[derive(PartialEq, Debug, Clone, Default, Serialize, Deserialize)]
pub struct EnumMembers {
    members: StrMap<LocalDefId>,
}

impl Deref for EnumMembers {
    type Target = StrMap<LocalDefId>;

    fn deref(&self) -> &Self::Target {
        &self.members
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
    expr: LocalDefId,
    implicit_return: bool,
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
    Boolean,
    None,
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

#[derive(PartialEq, Debug, Default, Clone, Serialize, Deserialize)]
pub struct TraitBound {
    bounds: Vec<PathTy>,
}

impl TraitBound {
    pub fn new(bounds: Vec<PathTy>) -> Self {
        Self { bounds }
    }
}

impl Deref for TraitBound {
    type Target = Vec<PathTy>;

    fn deref(&self) -> &Self::Target {
        &self.bounds
    }
}

impl DerefMut for TraitBound {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.bounds
    }
}

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
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

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
pub enum ArrayExpr {
    Sized {
        initializer: LocalDefId,
        size: LocalDefId,
    },
    Unsized {
        initializers: Vec<LocalDefId>,
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
    pub ty: LocalDefId,
    pub ident: Option<PatternLocal>,
}

impl TyPattern {
    pub fn new(ty: LocalDefId, ident: Option<PatternLocal>) -> Self {
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
    pub ty: LocalDefId,
    pub exprs: Vec<LocalDefId>,
}

impl DestructurePattern {
    pub fn new(ty: LocalDefId, exprs: Vec<LocalDefId>) -> Self {
        Self { ty, exprs }
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
    pub segments: Vec<Segment>,
}

impl PathExpr {
    pub fn new(segments: Vec<Segment>) -> Self {
        Self { segments }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
pub struct Segment {
    pub ident: Ident,
    pub generics: Option<Generics>,
}

impl Segment {
    pub fn new(ident: Ident, generics: Option<Generics>) -> Self {
        Self { ident, generics }
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

#[derive(PartialEq, Debug, Clone, Default, Serialize, Deserialize)]
#[serde(transparent)]
pub struct FnStmts {
    fields: StrMap<LocalDefId>,
}

impl Deref for FnStmts {
    type Target = StrMap<LocalDefId>;

    fn deref(&self) -> &Self::Target {
        &self.fields
    }
}

impl DerefMut for FnStmts {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.fields
    }
}

#[derive(PartialEq, Debug, Default, Clone, Serialize, Deserialize)]
#[serde(transparent)]
pub struct Stmts {
    stmts: Vec<LocalDefId>,
}

impl Deref for Stmts {
    type Target = Vec<LocalDefId>;

    fn deref(&self) -> &Self::Target {
        &self.stmts
    }
}

impl DerefMut for Stmts {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.stmts
    }
}

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
#[serde(transparent)]
pub struct Args {
    args: Vec<LocalDefId>,
}

impl Args {
    pub fn new(args: Vec<LocalDefId>) -> Self {
        Self { args }
    }
}

#[derive(PartialEq, Debug, Clone, Default, Serialize, Deserialize)]
#[serde(transparent)]
pub struct Fields {
    fields: StrMap<LocalDefId>,
}

impl Deref for Fields {
    type Target = StrMap<LocalDefId>;

    fn deref(&self) -> &Self::Target {
        &self.fields
    }
}

impl DerefMut for Fields {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.fields
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

#[derive(PartialEq, Debug, Clone, Default, Serialize, Deserialize)]
#[serde(transparent)]
pub struct Params {
    params: StrMap<LocalDefId>,
}

impl Deref for Params {
    type Target = StrMap<LocalDefId>;

    fn deref(&self) -> &Self::Target {
        &self.params
    }
}

impl DerefMut for Params {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.params
    }
}

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
#[serde(transparent)]
pub struct ClosureParams {
    closure_params: StrMap<ClosureParam>,
}

impl ClosureParams {
    pub fn new(fields: StrMap<ClosureParam>) -> Self {
        Self {
            closure_params: fields,
        }
    }
}

impl Deref for ClosureParams {
    type Target = StrMap<ClosureParam>;

    fn deref(&self) -> &Self::Target {
        &self.closure_params
    }
}

#[derive(PartialEq, Debug, Clone, Default, Serialize, Deserialize)]
#[serde(transparent)]
pub struct GenericParams {
    generic_params: StrMap<LocalDefId>,
}

impl Deref for GenericParams {
    type Target = StrMap<LocalDefId>;

    fn deref(&self) -> &Self::Target {
        &self.generic_params
    }
}

impl DerefMut for GenericParams {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.generic_params
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
#[serde(transparent)]
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

impl DerefMut for Generics {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.generics
    }
}

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
pub struct HirCrate {
    pub(crate) name: InternedStr,
    pub(crate) id: CrateId,
    items: Vec<LocalDefId>,
    #[cfg(not(test))]
    nodes: HashMap<LocalDefId, HirNode>,
    #[cfg(test)]
    nodes: BTreeMap<LocalDefId, HirNode>,
}

impl HirCrate {
    pub fn new(
        name: InternedStr,
        id: CrateId,
        items: Vec<LocalDefId>,
        nodes: HashMap<LocalDefId, HirNode>,
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
