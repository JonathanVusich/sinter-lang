use std::collections::{HashMap, HashSet};
use std::ops::{Deref, DerefMut};
use crate::compiler::parser::ClassType;
use crate::compiler::types::types::{InternedStr, InternedTy, Type};
use crate::gc::block::Block;
use crate::traits::traits::Trait;
use serde::{Deserialize, Serialize};
use std::path::{Path, Prefix};
use crate::compiler::interner::Key;
use crate::compiler::resolver::{PathDecl, PathKind, TyDecl, TyKind, VarDecl, VarDeclKind};
use crate::compiler::tokens::tokenized_file::Span;

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct Module {
    pub use_stmts: Vec<UseStmt>,
    pub global_let_stmts: Vec<GlobalLetStmt>,
    pub class_stmts: Vec<ClassStmt>,
    pub enum_stmts: Vec<EnumStmt>,
    pub trait_stmts: Vec<TraitStmt>,
    pub trait_impl_stmts: Vec<TraitImplStmt>,
    pub fn_stmts: Vec<FnStmt>,
}

impl Module {
    pub fn new(
        use_stmts: Vec<UseStmt>,
        const_let_stmts: Vec<GlobalLetStmt>,
        class_stmts: Vec<ClassStmt>,
        enum_stmts: Vec<EnumStmt>,
        trait_stmts: Vec<TraitStmt>,
        trait_impl_stmts: Vec<TraitImplStmt>,
        fn_stmts: Vec<FnStmt>,
    ) -> Self {
        Self {
            use_stmts,
            global_let_stmts: const_let_stmts,
            class_stmts,
            enum_stmts,
            trait_stmts,
            trait_impl_stmts,
            fn_stmts
        }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct ResolvedModule {
    pub used_modules: HashMap<InternedStr, QualifiedIdent>,
    pub module_tys: HashMap<InternedStr, TyKind>,
    pub module_impls: HashMap<InternedStr, TraitImplStmt>,
    pub module_fns: HashMap<InternedStr, FnStmt>,
}

impl ResolvedModule {
    pub fn new() -> Self {
        Self {
            used_modules: HashMap::default(),
            module_tys: HashMap::default(),
            module_impls: HashMap::default(),
            module_fns: HashMap::default(),
        }
    }
}

pub trait DeclaredType {
    fn name(&self) -> InternedStr;
    fn member_fns(&self) -> &[FnStmt];
    fn generic_params(&self) -> &[GenericParam];
}

pub trait AstNode {
    // fn node_id(&self) -> NodeId;
    fn span(&self) -> Span;
}


#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct QualifiedIdent {
    idents: Vec<InternedStr>,
}

impl QualifiedIdent {
    pub fn new(idents: Vec<InternedStr>) -> Self {
        if idents.len() < 1 {
            panic!("QualifiedIdent must have at least one identifier!")
        }
        Self { idents }
    }

    pub fn is_empty(&self) -> bool {
        self.idents.is_empty()
    }

    pub fn first(&self) -> InternedStr {
        // It is safe to unwrap here since a QualifiedIdent should always have at least one element.
        self.idents.first().copied().unwrap()
    }

    pub fn last(&self) -> InternedStr {
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
        Self { bounds }
    }
}

pub type Generics = Vec<InternedTy>;

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
        self.ident.first()
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

const EMPTY_GENERIC_DECL: GenericParams = GenericParams::new(Vec::new());

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
    pub (crate) ident: InternedStr,
    pub (crate) trait_bound: Option<TraitBound>,
}

impl GenericParam {
    pub fn new(ident: InternedStr, trait_bound: Option<TraitBound>) -> Self {
        Self { ident, trait_bound }
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Debug, Serialize, Deserialize)]
pub struct GenericCallSite {
    generics: Vec<Key>,
}

impl GenericCallSite {
    pub fn new(generics: Vec<Key>) -> Self {
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
    pub (crate) name: InternedStr,
    pub (crate) class_type: ClassType,
    pub (crate) generic_params: GenericParams,
    pub (crate) fields: Fields ,
    pub (crate) member_fns: Vec<FnStmt>,
}

impl ClassStmt {
    pub fn new(
        name: InternedStr,
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
    fn ident(&self) -> InternedStr {
        self.name
    }

    fn into(&self) -> TyKind {
        TyKind::Class(self.clone())
    }
}

impl DeclaredType for ClassStmt {
    fn name(&self) -> InternedStr {
        self.name
    }

    fn member_fns(&self) -> &[FnStmt] {
        self.member_fns.as_slice()    
    }

    fn generic_params(&self) -> &[GenericParam] {
        &self.generic_params   
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct EnumStmt {
    pub name: InternedStr,
    pub generic_params: GenericParams,
    pub members: Vec<EnumMemberStmt>,
    pub member_fns: Vec<FnStmt>,
}

impl EnumStmt {
    pub fn new(
        name: InternedStr,
        generic_params: GenericParams,
        members: Vec<EnumMemberStmt>,
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

    pub fn members(&self) -> &[EnumMemberStmt] {
        &self.members
    }
}

impl TyDecl for EnumStmt {
    fn ident(&self) -> InternedStr {
        self.name
    }

    fn into(&self) -> TyKind {
        TyKind::Enum(self.clone())
    }
}

impl DeclaredType for EnumStmt {
    fn name(&self) -> InternedStr {
        self.name
    }

    fn member_fns(&self) -> &[FnStmt] {
        self.member_fns.as_slice()
    }

    fn generic_params(&self) -> &[GenericParam] {
        &self.generic_params
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct TraitStmt {
    pub name: InternedStr,
    pub generic_params: GenericParams,
    pub member_fns: Vec<FnStmt>,
}

impl TraitStmt {
    pub fn new(ident: InternedStr, generic_params: GenericParams, functions: Vec<FnStmt>) -> Self {
        Self {
            name: ident,
            generic_params,
            member_fns: functions,
        }
    }
}

impl TyDecl for TraitStmt {
    fn ident(&self) -> InternedStr {
        self.name
    }

    fn into(&self) -> TyKind {
        TyKind::Trait(self.clone())
    }
}

impl DeclaredType for TraitStmt {
    fn name(&self) -> InternedStr {
        self.name
    }

    fn member_fns(&self) -> &[FnStmt] {
        self.member_fns.as_slice()
    }

    fn generic_params(&self) -> &[GenericParam] {
        &self.generic_params 
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct TraitImplStmt {
    pub trait_to_impl: PathTy,
    pub target_ty: Key,
    pub member_fns: Vec<FnStmt>,
}

impl TraitImplStmt {
    pub fn new(trait_to_impl: PathTy, target_ty: Key, fns: Vec<FnStmt>) -> Self {
        Self {
            trait_to_impl,
            target_ty,
            member_fns: fns,
        }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct FnStmt {
    pub (crate) sig: FnSig,
    pub (crate) body: Option<BlockStmt>,
}

impl FnStmt {
    pub fn new(sig: FnSig, body: Option<BlockStmt>) -> Self {
        Self { sig, body }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct ForStmt {
    pub ident: InternedStr,
    pub range: Expr,
    pub body: BlockStmt,
}

impl ForStmt {
    pub fn new(ident: InternedStr, range: Expr, body: BlockStmt) -> Self {
        Self { ident, range, body }
    }
}

impl VarDecl for ForStmt {

    fn ident(&self) -> InternedStr {
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
    pub if_true: BlockStmt,
    pub if_false: Option<BlockStmt>,
}

impl IfStmt {
    pub fn new(condition: Expr, if_true: BlockStmt, if_false: Option<BlockStmt>) -> Self {
        Self {
            condition,
            if_true,
            if_false,
        }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct EnumMemberStmt {
    pub name: InternedStr,
    pub fields: Fields,
    pub member_fns: Vec<FnStmt>,
}

impl EnumMemberStmt {
    pub fn new(name: InternedStr, parameters: Fields, member_functions: Vec<FnStmt>) -> Self {
        Self {
            name,
            fields: parameters,
            member_fns: member_functions,
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug, Serialize, Deserialize)]
pub struct FnSig {
    pub (crate) name: InternedStr,
    pub (crate) generic_params: GenericParams,
    pub (crate) params: Params,
    pub (crate) return_type: Option<Key>,
}

impl FnSig {
    pub fn new(
        name: InternedStr,
        generic_params: GenericParams,
        parameters: Params,
        return_type: Option<Key>,
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
    pub (crate) ident: InternedStr,
    pub (crate) ty: InternedTy,
    pub (crate) mutability: Mutability,
}

impl Param {
    pub fn new(name: InternedStr, ty: Key, mutability: Mutability) -> Self {
        Self {
            ident: name,
            ty,
            mutability,
        }
    }
}

impl VarDecl for Param {
    fn ident(&self) -> InternedStr {
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
    pub (crate) ident: InternedStr,
    pub (crate) ty: Key,
    pub (crate) mutability: Mutability,
}

impl Field {
    pub fn new(name: InternedStr, ty: Key, mutability: Mutability) -> Self {
        Self {
            ident: name,
            ty,
            mutability,
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
    pub ident: InternedStr,
    pub ty: Option<InternedTy>,
    pub initializer: Expr,
}

impl GlobalLetStmt {
    pub fn new(
        ident: InternedStr,
        ty: Option<Key>,
        initializer: Expr,
    ) -> Self {
        Self {
            ident,
            ty,
            initializer,
        }
    }
}

impl VarDecl for GlobalLetStmt {

    fn ident(&self) -> InternedStr {
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
    pub ident: InternedStr,
    pub mutability: Mutability,
    pub ty: Option<Key>,
    pub initializer: Option<Expr>,
}

impl LetStmt {
    pub fn new(
        ident: InternedStr,
        mutability: Mutability,
        ty: Option<Key>,
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

    fn ident(&self) -> InternedStr {
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
pub enum Expr {
    Array(Box<ArrayExpr>),
    Call(Box<Call>),
    Infix(Box<InfixExpr>),
    Unary(Box<UnaryExpr>),
    None,
    Boolean(bool),
    Integer(i64),
    Float(f64),
    String(InternedStr),
    Match(Box<MatchExpr>),
    Closure(Box<ClosureExpr>),
    Assign(Box<AssignExpr>),
    Field(Box<FieldExpr>),
    Index(Box<IndexExpr>),
    Path(PathExpr),
    Break,
    Continue,
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct Call {
    pub func: Expr,
    pub args: Args,
}

impl Call {
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
    pub ident: InternedStr,
}

impl ClosureParam {
    pub fn new(ident: InternedStr) -> Self {
        Self {
            ident,
        }
    }
}

impl VarDecl for ClosureParam {
    fn ident(&self) -> InternedStr {
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
    pub ident: InternedStr,
}

impl FieldExpr {
    pub fn new(lhs: Expr, ident: InternedStr) -> Self {
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
    pub ident: InternedStr,
    pub generics: Option<Generics>,
}

impl Segment {
    pub fn new(ident: InternedStr, generics: Option<Generics>) -> Self {
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

    pub fn first(&self) -> InternedStr {
        self.segments.first().unwrap().ident
    }

    pub fn to_module_path(&self) -> QualifiedIdent {
        let mut idents = self.segments
            .iter()
            .map(|f| f.ident)
            .collect::<Vec<InternedStr>>();
        // Remove the last ident because it is not part of the module path.
        idents.pop();
        QualifiedIdent::new(idents)
    }
}

impl PathDecl for PathExpr {
    fn first(&self) -> InternedStr {
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
    Wildcard,                     // _
    Or(OrPattern),                // pat | pat
    Boolean(bool),                // true/false
    Integer(i64),                 // 100
    String(InternedStr),          // "true"
    Ty(TyPattern), // Logical logical => { }
    Destructure(DestructurePattern),  // Logical(1, true, 100) => { }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub enum Range {
    Default(i64, i64),       // 1..3
    Full,                    // ..
    From(i64),               // 1..
    FromInclusive(i64, i64), // 1..=3
    To(i64),                 // ..3
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
    pub ty: InternedTy,
    pub ident: Option<PatternLocal>,
}

impl TyPattern {
    pub fn new(ty: InternedTy, ident: Option<PatternLocal>) -> Self {
        Self {
            ty,
            ident,
        }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct DestructurePattern {
    pub ty: InternedTy,
    pub exprs: Vec<Expr>,
}

impl DestructurePattern {
    pub fn new(ty: InternedTy, exprs: Vec<Expr>) -> Self {
        Self {
            ty,
            exprs,
        }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct WhileStmt {
    pub condition: Expr,
    pub block_stmt: BlockStmt,
}

impl WhileStmt {
    pub fn new(condition: Expr, block_stmt: BlockStmt) -> Self {
        Self {
            condition,
            block_stmt,
        }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct BlockStmt {
    pub stmts: Vec<Stmt>,
}

impl BlockStmt {
    pub fn new(stmts: Vec<Stmt>) -> Self {
        Self { stmts }
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
pub enum OuterStmt {
    Use(UseStmt),
    GlobalLet(GlobalLetStmt),
    Class(ClassStmt),
    Enum(EnumStmt),
    Trait(TraitStmt),
    TraitImpl(TraitImplStmt),
    Fn(FnStmt),
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub enum Stmt {
    Let(LetStmt),
    For(ForStmt),
    If(IfStmt),
    Return(ReturnStmt),
    While(WhileStmt),
    Block(BlockStmt),
    Expression { expr: Expr, implicit_return: bool },
}

mod tests {}
