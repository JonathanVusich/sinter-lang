use std::collections::{HashMap, HashSet};
use std::ops::{Deref, DerefMut};
use crate::compiler::parser::{ClassType, ParseError};
use crate::compiler::types::types::{InternedStr, Type};
use crate::traits::traits::Trait;
use serde::{Deserialize, Serialize};
use std::path::{Path, Prefix};
use std::sync::atomic::{AtomicUsize, Ordering};
use crate::compiler::resolver::{PathDecl, PathKind, TraitTy, TyDecl, TyStmt, VarDecl, VarDeclKind};
use crate::compiler::tokens::tokenized_file::{Span, TokenSpan};

/// This is a unique identifier for a parsed module.
#[derive(Copy, Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct NodeId {
    id: u32,
}

impl From<u32> for NodeId {
    fn from(value: u32) -> Self {
        NodeId {
            id: value
        }
    }
}

/// This trait describes a visitor that can traverse the AST and collect information.
pub trait AstVisitor<T> {
    fn visit_node(&mut self, node: &Item);
    fn visit_expr(&mut self, expr: &Expr);
}

/// This enum represents the various types of nodes in the AST.
#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub enum ItemKind {
    Use(UseStmt),
    GlobalLet(GlobalLetStmt),
    Class(ClassStmt),
    Enum(EnumStmt),
    Trait(TraitStmt),
    TraitImpl(TraitImplStmt),
    Fn(FnStmt),
}

/// This struct represents the node plus diagnostic information.
#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct Item {
    kind: ItemKind,
    span: Span,
    id: NodeId,
}

impl Item {
    pub fn new(kind: ItemKind, span: Span, id: NodeId) -> Self {
        Self {
            kind,
            span,
            id,
        }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub enum ExprKind {
    Array(ArrayExpr),
    Call(CallExpr),
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
    Parentheses(Expr),
    Break,
    Continue,
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct Expr {
    kind: ExprKind,
    span: Span,
    id: NodeId,
}

impl Expr {
    pub fn new(kind: ExprKind, span: Span, id: NodeId) -> Self {
        Self {
            kind,
            span,
            id,
        }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct Ty {
    kind: TyKind,
    span: Span,
    id: NodeId,
}

impl Ty {
    pub fn new(kind: TyKind, span: Span, id: NodeId) -> Self {
        Self {
            kind,
            span,
            id,
        }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub enum TyKind {
    Array {
        ty: Ty,
    },
    Path {
        path: PathTy,
    },
    Union {
        tys: Vec<Ty>,
    },
    TraitBound {
        trait_bound: TraitBound,
    },
    Closure {
        params: Vec<Ty>,
        ret_ty: Ty,
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
pub struct Module {
    items: Vec<Item>,
    parse_errors: Vec<ParseError>,
}

impl Module {
    pub fn new(
        items: Vec<Item>,
    ) -> Self {
        Self {
            items,
            parse_errors: Vec::new(),
        }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct ResolvedModule {
    pub used_modules: HashMap<InternedStr, UseStmt>,
    pub module_tys: HashMap<InternedStr, TyStmt>,
    pub module_impls: HashMap<TraitTy, TraitImplStmt>,
    pub module_fns: HashMap<InternedStr, FnStmt>,
}

impl ResolvedModule {
    pub fn new(
        used_modules: HashMap<InternedStr, UseStmt>,
        module_tys: HashMap<InternedStr, TyStmt>,
        module_impls: HashMap<TraitTy, TraitImplStmt>,
        module_fns: HashMap<InternedStr, FnStmt>,
    ) -> Self {
        Self {
            used_modules,
            module_tys,
            module_impls,
            module_fns,
        }
    }
}


#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Ident {
    pub ident: InternedStr,
    pub span: Span,
    pub id: NodeId,
}

impl Ident {
    pub fn new(ident: InternedStr, span: Span, id: NodeId) -> Self {
        Self {
            ident,
            span,
            id,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct QualifiedIdent {
    idents: Vec<Ident>,
}

impl QualifiedIdent {
    pub fn new(idents: Vec<Ident>) -> Self {
        if idents.len() < 1 {
            panic!("QualifiedIdent must have at least one identifier!")
        }
        Self { idents }
    }

    pub fn first(&self) -> Ident {
        // It is safe to unwrap here since a QualifiedIdent should always have at least one element.
        self.idents.first().copied().unwrap()
    }

    pub fn last(&self) -> Ident {
        // It is safe to unwrap here since a QualifiedIdent should always have at least one element.
        self.idents.last().copied().unwrap()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct TraitBound {
    bounds: Vec<PathTy>,
}

impl TraitBound {
    pub fn new(bounds: Vec<PathTy>) -> Self {
        Self {
            bounds,
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug, Serialize, Deserialize)]
pub struct Generics {
    generics: Vec<Ty>,
}

impl Generics {
    pub fn new(generics: Vec<Ty>) -> Self {
        Self {
            generics,
        }
    }
}

impl IntoIterator for Generics {
    type Item = Ty;
    type IntoIter = <Vec<Ty> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.generics.into_iter()
    }
}

impl Deref for Generics {
    type Target = [Ty];

    fn deref(&self) -> &Self::Target {
        &self.generics.as_slice()
    }
}

impl DerefMut for Generics {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.generics
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct PathTy {
    pub ident: QualifiedIdent,
    pub generics: Generics,
}

impl PathTy {
    pub fn new(ident: QualifiedIdent, generics: Generics) -> Self {
        Self { ident, generics }
    }
}

impl PathDecl for PathTy {
    fn first(&self) -> InternedStr {
        self.ident.first().ident
    }

    fn module_path(&self) -> QualifiedIdent {
        let mut path = self.ident.clone();
        // Pop off the last segment, as it points to the actual type.
        path.idents.pop();
        path
    }

    fn into(&self) -> PathKind {
        PathKind::Ty(self.clone())
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct UseStmt {
    pub ident: QualifiedIdent,
}

impl UseStmt {
    pub fn new(ident: QualifiedIdent) -> Self {
        Self { ident }
    }
}

#[derive(Clone, PartialEq, Eq, Debug, Serialize, Deserialize)]
pub struct GenericParams {
    params: Vec<GenericParam>,
}

impl GenericParams {
    pub const fn new(params: Vec<GenericParam>) -> Self {
        Self { params }
    }
}

impl IntoIterator for GenericParams {
    type Item = GenericParam;
    type IntoIter = <Vec<GenericParam> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.params.into_iter()
    }
}

impl Deref for GenericParams {
    type Target = [GenericParam];

    fn deref(&self) -> &Self::Target {
        &self.params.as_slice()
    }
}

impl DerefMut for GenericParams {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.params
    }
}

#[derive(Clone, Eq, PartialEq, Debug, Hash, Serialize, Deserialize)]
pub struct GenericParam {
    pub(crate) ident: Ident,
    pub(crate) trait_bound: Option<TraitBound>,
    pub(crate) span: Span,
    pub(crate) id: NodeId,
}

impl GenericParam {
    pub fn new(
        ident: Ident,
        trait_bound: Option<TraitBound>,
        span: Span,
        id: NodeId,
    ) -> Self {
        Self { ident, trait_bound, span, id }
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Debug, Serialize, Deserialize)]
pub struct GenericCallSite {
    generics: Vec<Ty>,
}

impl GenericCallSite {
    pub fn new(generics: Vec<Ty>) -> Self {
        Self { generics }
    }
}

#[derive(PartialEq, Eq, Debug, Default, Clone, Serialize, Deserialize)]
pub struct Params {
    params: Vec<Param>,
}

impl Params {
    pub const fn new(params: Vec<Param>) -> Self {
        Self { params }
    }
}

impl IntoIterator for Params {
    type Item = Param;
    type IntoIter = <Vec<Param> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.params.into_iter()
    }
}

impl Deref for Params {
    type Target = [Param];

    fn deref(&self) -> &Self::Target {
        &self.params
    }
}

impl DerefMut for Params {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.params
    }
}

#[derive(Clone, PartialEq, Eq, Debug, Default, Serialize, Deserialize)]
pub struct Fields {
    params: Vec<Field>,
}

impl Fields {
    pub const fn new(params: Vec<Field>) -> Self {
        Self { params }
    }
}

impl IntoIterator for Fields {
    type Item = Field;
    type IntoIter = <Vec<Field> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.params.into_iter()
    }
}

impl Deref for Fields {
    type Target = [Field];

    fn deref(&self) -> &Self::Target {
        &self.params
    }
}

impl DerefMut for Fields {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.params
    }
}

pub const EMPTY_ARGS: Args = Args::new(Vec::new());

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct Args {
    args: Vec<Expr>,
}

impl Args {
    pub const fn new(args: Vec<Expr>) -> Self {
        Self { args }
    }

    pub fn empty() -> Self {
        EMPTY_ARGS
    }
}

impl IntoIterator for Args {
    type Item = Expr;
    type IntoIter = <Vec<Expr> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.args.into_iter()
    }
}

impl Deref for Args {
    type Target = [Expr];

    fn deref(&self) -> &Self::Target {
        &self.args
    }
}

impl DerefMut for Args {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.args
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Serialize, Deserialize)]
pub enum Mutability {
    Mutable,
    Immutable,
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct ClassStmt {
    pub(crate) name: Ident,
    pub(crate) class_type: ClassType,
    pub(crate) generic_params: GenericParams,
    pub(crate) fields: Fields,
    pub(crate) member_fns: Vec<FnStmt>,
}

impl ClassStmt {
    pub fn new(
        name: Ident,
        class_type: ClassType,
        generic_params: GenericParams,
        fields: Fields,
        member_functions: Vec<FnStmt>,
    ) -> Self {
        Self {
            name,
            class_type,
            generic_params,
            fields,
            member_fns: member_functions,
        }
    }
}

impl TyDecl for ClassStmt {
    fn ident(&self) -> Ident {
        self.name
    }

    fn into(&self) -> TyStmt {
        TyStmt::Class(self.clone())
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct EnumStmt {
    pub name: Ident,
    pub generic_params: GenericParams,
    pub members: Vec<EnumMember>,
    pub member_fns: Vec<FnStmt>,
}

impl EnumStmt {
    pub fn new(
        name: Ident,
        generic_params: GenericParams,
        members: Vec<EnumMember>,
        fn_stmts: Vec<FnStmt>,
    ) -> Self {
        Self {
            name,
            generic_params,
            members,
            member_fns: fn_stmts,
        }
    }

    pub fn generic_params(&self) -> &GenericParams {
        &self.generic_params
    }

    pub fn members(&self) -> &[EnumMember] {
        &self.members
    }
}

impl TyDecl for EnumStmt {
    fn ident(&self) -> Ident {
        self.name
    }

    fn into(&self) -> TyStmt {
        TyStmt::Enum(self.clone())
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct TraitStmt {
    pub name: Ident,
    pub generic_params: GenericParams,
    pub member_fns: Vec<FnStmt>,
}

impl TraitStmt {
    pub fn new(ident: Ident, generic_params: GenericParams, functions: Vec<FnStmt>) -> Self {
        Self {
            name: ident,
            generic_params,
            member_fns: functions,
        }
    }
}

impl TyDecl for TraitStmt {
    fn ident(&self) -> Ident {
        self.name
    }

    fn into(&self) -> TyStmt {
        TyStmt::Trait(self.clone())
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct TraitImplStmt {
    pub trait_to_impl: PathTy,
    pub target_ty: QualifiedIdent,
    pub member_fns: Vec<FnStmt>,
}

impl TraitImplStmt {
    pub fn new(trait_to_impl: PathTy, target_ty: QualifiedIdent, fns: Vec<FnStmt>) -> Self {
        Self {
            trait_to_impl,
            target_ty,
            member_fns: fns,
        }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct FnStmt {
    pub(crate) sig: FnSig,
    pub(crate) body: Option<Block>,
}

impl FnStmt {
    pub fn new(sig: FnSig, body: Option<Block>) -> Self {
        Self { sig, body }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct ForStmt {
    pub ident: Ident,
    pub range: Expr,
    pub body: Block,
}

impl ForStmt {
    pub fn new(ident: Ident, range: Expr, body: Block) -> Self {
        Self { ident, range, body }
    }
}

impl VarDecl for ForStmt {
    fn ident(&self) -> Ident {
        self.ident
    }

    fn is_global(&self) -> bool {
        false
    }

    fn into(&self) -> VarDeclKind {
        VarDeclKind::For(self.clone())
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct ReturnStmt {
    pub value: Option<Expr>,
}

impl ReturnStmt {
    pub fn new(value: Option<Expr>) -> Self {
        Self { value }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct IfStmt {
    pub condition: Expr,
    pub if_true: Block,
    pub if_false: Option<Block>,
}

impl IfStmt {
    pub fn new(condition: Expr, if_true: Block, if_false: Option<Block>) -> Self {
        Self {
            condition,
            if_true,
            if_false,
        }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct EnumMember {
    pub name: InternedStr,
    pub fields: Fields,
    pub member_fns: Vec<FnStmt>,
    pub span: Span,
    pub id: NodeId,
}

impl EnumMember {
    pub fn new(name: InternedStr,
               parameters: Fields,
               member_functions: Vec<FnStmt>,
               span: Span,
               id: NodeId) -> Self {
        Self {
            name,
            fields: parameters,
            member_fns: member_functions,
            id,
            span,
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug, Serialize, Deserialize)]
pub struct FnSig {
    pub(crate) name: Ident,
    pub(crate) generic_params: GenericParams,
    pub(crate) params: Params,
    pub(crate) return_type: Option<Ty>,
}

impl FnSig {
    pub fn new(
        name: Ident,
        generic_params: GenericParams,
        parameters: Params,
        return_type: Option<Ty>,
    ) -> Self {
        Self {
            name,
            generic_params,
            params: parameters,
            return_type,
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug, Serialize, Deserialize)]
pub struct Param {
    pub(crate) ident: Ident,
    pub(crate) ty: Ty,
    pub(crate) mutability: Mutability,
}

impl Param {
    pub fn new(ident: Ident, ty: Ty, mutability: Mutability) -> Self {
        Self {
            ident,
            ty,
            mutability,
        }
    }
}

impl VarDecl for Param {
    fn ident(&self) -> Ident {
        self.ident
    }

    fn is_global(&self) -> bool {
        false
    }

    fn into(&self) -> VarDeclKind {
        VarDeclKind::Param(self.clone())
    }
}

#[derive(Clone, PartialEq, Eq, Debug, Serialize, Deserialize)]
pub struct Field {
    pub(crate) ident: Ident,
    pub(crate) ty: Ty,
    pub(crate) mutability: Mutability,
    pub(crate) span: Span,
    pub(crate) id: NodeId,
}

impl Field {
    pub fn new(ident: Ident, ty: Ty, mutability: Mutability, span: Span, id: NodeId) -> Self {
        Self {
            ident,
            ty,
            mutability,
            span,
            id,
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct ArgumentDecl {
    ident: InternedStr,
    ty: Type,
}


#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct GlobalLetStmt {
    pub ident: Ident,
    pub ty: Option<Ty>,
    pub initializer: Expr,
    pub id: NodeId,
    pub span: Span,
}

impl GlobalLetStmt {
    pub fn new(
        ident: Ident,
        ty: Option<Ty>,
        initializer: Expr,
        id: NodeId,
        span: Span,
    ) -> Self {
        Self {
            ident,
            ty,
            initializer,
            id,
            span,
        }
    }
}

impl VarDecl for GlobalLetStmt {
    fn ident(&self) -> Ident {
        self.ident
    }

    fn is_global(&self) -> bool {
        true
    }

    fn into(&self) -> VarDeclKind {
        VarDeclKind::Global(self.clone())
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct LetStmt {
    pub ident: Ident,
    pub mutability: Mutability,
    pub ty: Option<Ty>,
    pub initializer: Option<Expr>,
}

impl LetStmt {
    pub fn new(
        ident: Ident,
        mutability: Mutability,
        ty: Option<Ty>,
        initializer: Option<Expr>,
    ) -> Self {
        Self {
            ident,
            mutability,
            ty,
            initializer,
        }
    }
}

impl VarDecl for LetStmt {
    fn ident(&self) -> Ident {
        self.ident
    }

    fn is_global(&self) -> bool {
        false
    }

    fn into(&self) -> VarDeclKind {
        VarDeclKind::NonGlobal(self.clone())
    }
}

#[derive(PartialEq, Debug)]
pub enum VarInitializer {
    Array(Vec<VarInitializer>),
    Statement(Stmt),
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct CallExpr {
    pub func: Expr,
    pub args: Args,
}

impl CallExpr {
    pub fn new(func: Expr, args: Args) -> Self {
        Self { func, args }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct InfixExpr {
    pub operator: InfixOp,
    pub lhs: Expr,
    pub rhs: Expr,
}

impl InfixExpr {
    pub fn new(lhs: Expr, rhs: Expr, operator: InfixOp) -> Self {
        Self { operator, lhs, rhs }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct UnaryExpr {
    pub operator: UnaryOp,
    pub expr: Expr,
}

impl UnaryExpr {
    pub fn new(operator: UnaryOp, expr: Expr) -> Self {
        Self { operator, expr }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct MatchExpr {
    pub source: Expr,
    pub arms: Vec<MatchArm>,
}

impl MatchExpr {
    pub fn new(source: Expr, arms: Vec<MatchArm>) -> Self {
        Self { source, arms }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub body: Stmt,
}

impl MatchArm {
    pub fn new(pattern: Pattern, body: Stmt) -> Self {
        Self { pattern, body }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct ClosureExpr {
    pub params: Vec<ClosureParam>,
    pub stmt: Stmt,
}

impl ClosureExpr {
    pub fn new(params: Vec<ClosureParam>, stmt: Stmt) -> Self {
        Self { params, stmt }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
#[repr(transparent)]
#[serde(transparent)]
pub struct ClosureParam {
    pub ident: Ident,
}

impl ClosureParam {
    pub fn new(ident: Ident) -> Self {
        Self {
            ident,
        }
    }
}

impl VarDecl for ClosureParam {
    fn ident(&self) -> Ident{
        self.ident
    }

    fn is_global(&self) -> bool {
        false
    }

    fn into(&self) -> VarDeclKind {
        VarDeclKind::ClosureParam(self.clone())
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct AssignExpr {
    pub lhs: Expr,
    pub rhs: Expr,
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct FieldExpr {
    pub lhs: Expr,
    pub ident: Ident,
}

impl FieldExpr {
    pub fn new(lhs: Expr, ident: Ident) -> Self {
        Self { lhs, ident }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct IndexExpr {
    pub expr: Expr,
    pub key: Expr,
}

impl IndexExpr {
    pub fn new(expr: Expr, key: Expr) -> Self {
        Self { expr, key }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub enum ArrayExpr {
    SizedInitializer(Expr, Expr),
    Initializer(Vec<Expr>),
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct Segment {
    pub ident: Ident,
    pub generics: Option<Generics>,
}

impl Segment {
    pub fn new(ident: Ident, generics: Option<Generics>) -> Self {
        Self {
            ident,
            generics,
        }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct PathExpr {
    pub segments: Vec<Segment>,
}

impl PathExpr {
    pub fn new(segments: Vec<Segment>) -> Self {
        Self { segments }
    }

    pub fn prefix(self, module: &QualifiedIdent) -> Self {
        // Verify that this is an appropriate module to prefix
        debug_assert_eq!(module.last(), self.first());
        let mut segments = Vec::with_capacity(module.idents.len() - 1 + self.segments.len());
        for ident in &module.idents {
            segments.push(Segment::new(*ident, None));
        }
        segments.pop();
        for segment in self.segments {
            segments.push(segment);
        }
        Self {
            segments,
        }
    }

    pub fn first(&self) -> Ident {
        self.segments.first().unwrap().ident
    }

    pub fn var_identifier(&self) -> Option<Ident> {
        if self.segments.len() == 1 {
            Some(self.first())
        } else {
            None
        }
    }


    pub fn to_module_path(&self) -> QualifiedIdent {
        let mut idents = self.segments
            .iter()
            .map(|f| f.ident)
            .collect::<Vec<Ident>>();
        // Remove the last ident because it is not part of the module path.
        idents.pop();
        QualifiedIdent::new(idents)
    }
}

impl PathDecl for PathExpr {
    fn first(&self) -> Ident {
        // This should be safe because a path should always have at least one segment.
        self.segments.first().unwrap().ident
    }

    fn module_path(&self) -> QualifiedIdent {
        self.to_module_path()
    }

    fn into(&self) -> PathKind {
        PathKind::Expr(self.clone())
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct RangeExpr {
    condition: Expr,
    increment: Expr,
}

impl RangeExpr {
    pub fn new(condition: Expr, increment: Expr) -> Self {
        Self {
            condition,
            increment,
        }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
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
    Destructure(DestructurePattern),  // Logical(1, true, 100) => { }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub enum Range {
    Default(i64, i64),
    // 1..3
    Full,
    // ..
    From(i64),
    // 1..
    FromInclusive(i64, i64),
    // 1..=3
    To(i64),
    // ..3
    ToInclusive(i64),        // ..=3
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct OrPattern {
    pub patterns: Vec<Pattern>,
}

impl OrPattern {
    pub fn new(patterns: Vec<Pattern>) -> Self {
        Self { patterns }
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
        Self {
            ident,
        }
    }
}

impl VarDecl for PatternLocal {
    fn ident(&self) -> InternedStr {
        self.ident
    }

    fn is_global(&self) -> bool {
        false
    }

    fn into(&self) -> VarDeclKind {
        VarDeclKind::PatternLocal(self.clone())
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct TyPattern {
    pub ty: Ty,
    pub ident: Option<PatternLocal>,
}

impl TyPattern {
    pub fn new(ty: Ty, ident: Option<PatternLocal>) -> Self {
        Self {
            ty,
            ident,
        }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct DestructurePattern {
    pub ty: Ty,
    pub exprs: Vec<Expr>,
}

impl DestructurePattern {
    pub fn new(ty: Ty, exprs: Vec<Expr>) -> Self {
        Self {
            ty,
            exprs,
        }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct WhileStmt {
    pub condition: Expr,
    pub block_stmt: Block,
}

impl WhileStmt {
    pub fn new(condition: Expr, block_stmt: Block) -> Self {
        Self {
            condition,
            block_stmt,
        }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub span: Span,
    pub id: NodeId,
}

impl Block {
    pub fn new(stmts: Vec<Stmt>, span: Span, id: NodeId) -> Self {
        Self {
            stmts,
            span,
            id,
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
pub enum AssignmentOp {
    Assign,
    PlusAssign,
    SubtractAssign,
    MultiplyAssign,
    DivideAssign,
    ModuloAssign,
}

#[derive(PartialEq, Eq, Debug, Serialize, Deserialize, Copy, Clone)]
pub enum UnaryOp {
    Bang,
    Minus,
    Plus,
    BitwiseComplement,
}

impl UnaryOp {
    pub fn binding_power(self) -> ((), u8) {
        ((), 23)
    }
}

#[derive(PartialEq, Eq, Debug, Serialize, Deserialize, Copy, Clone)]
pub enum PostfixOp {
    LeftBracket,
    LeftParentheses,
    Dot,
}

impl PostfixOp {
    pub fn binding_power(self) -> (u8, ()) {
        (24, ())
    }
}

#[derive(PartialEq, Eq, Debug, Serialize, Deserialize, Copy, Clone)]
pub enum InfixOp {
    Assign,
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Or,
    And,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    BitwiseOr,
    BitwiseAnd,
    BitwiseComplement,
    BitwiseXor,
    Equal,
    NotEqual,
    LeftShift,
    RightShift,
    TripleRightShift,
}

impl InfixOp {
    pub fn binding_power(self) -> (u8, u8) {
        match self {
            InfixOp::Assign => (2, 1),
            InfixOp::Or => (4, 3),
            InfixOp::And => (5, 6),
            InfixOp::BitwiseOr => (7, 8),
            InfixOp::BitwiseXor => (9, 10),
            InfixOp::BitwiseAnd => (11, 12),
            InfixOp::Equal | InfixOp::NotEqual => (13, 14),
            InfixOp::Less | InfixOp::Greater | InfixOp::LessEqual | InfixOp::GreaterEqual => {
                (15, 16)
            }
            InfixOp::LeftShift | InfixOp::RightShift | InfixOp::TripleRightShift => (17, 18),
            InfixOp::Add | InfixOp::Subtract => (19, 20),
            InfixOp::Multiply | InfixOp::Divide | InfixOp::Modulo => (21, 22),
            _ => panic!(),
        }
    }

    pub fn token_len(self) -> usize {
        match self {
            InfixOp::LeftShift => 2,
            InfixOp::RightShift => 2,
            InfixOp::TripleRightShift => 3,
            _ => 1,
        }
    }
}


#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub enum Stmt {
    Let(LetStmt),
    For(ForStmt),
    If(IfStmt),
    Return(ReturnStmt),
    While(WhileStmt),
    Block(Block),
    Expression { expr: Expr, implicit_return: bool },
}

mod tests {}
