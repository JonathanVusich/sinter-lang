use std::ops::{Deref, DerefMut};
use crate::compiler::parser::ClassType;
use crate::compiler::types::types::{InternedStr, Type};
use crate::gc::block::Block;
use crate::traits::traits::Trait;
use serde::{Deserialize, Serialize};
use std::path::{Path, Prefix};
use crate::compiler::interner::Key;
use crate::compiler::tokens::tokenized_file::Span;

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct Module {
    stmts: Vec<OuterStmt>,
}

impl Module {
    pub fn new(stmts: Vec<OuterStmt>) -> Self {
        Self { stmts }
    }

    pub fn stmts(&self) -> &[OuterStmt] {
        &self.stmts
    }
}

pub trait DeclaredType {
    fn name(&self) -> InternedStr;
    fn member_fns(&self) -> &[FnStmt];
    fn generic_params(&self) -> &[GenericParam];
}

pub trait AstNode {
    fn span(&self) -> Span;
}


#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct QualifiedIdent {
    idents: Vec<InternedStr>,
}

impl QualifiedIdent {
    pub fn new(idents: Vec<InternedStr>) -> Self {
        Self { idents }
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

pub type Generics = Vec<Key>;

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct PathTy {
    ident: QualifiedIdent,
    generics: Generics,
}

impl PathTy {
    pub fn new(ident: QualifiedIdent, generics: Generics) -> Self {
        Self { ident, generics }
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct UseStmt {
    ident: QualifiedIdent,
}

impl UseStmt {
    pub fn new(ident: QualifiedIdent) -> Self {
        Self { ident }
    }
}

const EMPTY_GENERIC_DECL: GenericParams = GenericParams::new(Vec::new());

#[derive(PartialEq, Eq, Debug, Serialize, Deserialize)]
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

#[derive(PartialEq, Eq, Debug, Default, Clone, Serialize, Deserialize)]
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

#[derive(PartialEq, Debug, Serialize, Deserialize)]
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

#[derive(Copy, Clone, PartialEq, Eq, Debug, Serialize, Deserialize)]
pub enum Mutability {
    Mutable,
    Immutable,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
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

#[derive(PartialEq, Debug, Serialize, Deserialize)]
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

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct TraitStmt {
    pub ident: InternedStr,
    pub generic_params: GenericParams,
    pub member_fns: Vec<FnStmt>,
}

impl TraitStmt {
    pub fn new(ident: InternedStr, generic_params: GenericParams, functions: Vec<FnStmt>) -> Self {
        Self {
            ident,
            generic_params,
            member_fns: functions,
        }
    }
}

impl DeclaredType for TraitStmt {
    fn name(&self) -> InternedStr {
        self.ident
    }

    fn member_fns(&self) -> &[FnStmt] {
        self.member_fns.as_slice()
    }

    fn generic_params(&self) -> &[GenericParam] {
        &self.generic_params 
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct TraitImplStmt {
    trait_to_impl: PathTy,
    target_ty: Key,
    fns: Vec<FnStmt>,
}

impl TraitImplStmt {
    pub fn new(trait_to_impl: PathTy, target_ty: Key, fns: Vec<FnStmt>) -> Self {
        Self {
            trait_to_impl,
            target_ty,
            fns,
        }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct FnStmt {
    pub (crate) sig: FnSig,
    pub (crate) body: Option<BlockStmt>,
}

impl FnStmt {
    pub fn new(sig: FnSig, body: Option<BlockStmt>) -> Self {
        Self { sig, body }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct ForStmt {
    range: Expr,
    body: BlockStmt,
}

impl ForStmt {
    pub fn new(range: Expr, body: BlockStmt) -> Self {
        Self { range, body }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct ReturnStmt {
    value: Option<Expr>,
}

impl ReturnStmt {
    pub fn new(value: Option<Expr>) -> Self {
        Self { value }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct IfStmt {
    condition: Expr,
    if_true: BlockStmt,
    if_false: Option<BlockStmt>,
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

#[derive(PartialEq, Debug, Serialize, Deserialize)]
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

#[derive(PartialEq, Eq, Debug, Serialize, Deserialize)]
pub struct FnSig {
    pub (crate) ident: InternedStr,
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
            ident: name,
            generic_params,
            params: parameters,
            return_type,
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug, Serialize, Deserialize)]
pub struct Param {
    pub (crate) ident: InternedStr,
    pub (crate) ty: Key,
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

#[derive(PartialEq, Debug, Serialize, Deserialize)]
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
        LetStmt {
            ident,
            mutability,
            ty,
            initializer,
        }
    }
}

#[derive(PartialEq, Debug)]
pub enum VarInitializer {
    Array(Vec<VarInitializer>),
    Statement(Stmt),
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
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

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct Call {
    func: Expr,
    args: Args,
}

impl Call {
    pub fn new(func: Expr, args: Args) -> Self {
        Self { func, args }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct InfixExpr {
    operator: InfixOp,
    lhs: Expr,
    rhs: Expr,
}

impl InfixExpr {
    pub fn new(lhs: Expr, rhs: Expr, operator: InfixOp) -> Self {
        Self { operator, lhs, rhs }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct UnaryExpr {
    operator: UnaryOp,
    expr: Expr,
}

impl UnaryExpr {
    pub fn new(operator: UnaryOp, expr: Expr) -> Self {
        Self { operator, expr }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct MatchExpr {
    source: Expr,
    arms: Vec<MatchArm>,
}

impl MatchExpr {
    pub fn new(source: Expr, arms: Vec<MatchArm>) -> Self {
        Self { source, arms }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct MatchArm {
    pattern: Pattern,
    body: Stmt,
}

impl MatchArm {
    pub fn new(pattern: Pattern, body: Stmt) -> Self {
        Self { pattern, body }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct ClosureExpr {
    params: Vec<InternedStr>,
    stmt: Stmt,
}

impl ClosureExpr {
    pub fn new(params: Vec<InternedStr>, stmt: Stmt) -> Self {
        Self { params, stmt }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct AssignExpr {
    lhs: Expr,
    rhs: Expr,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct FieldExpr {
    lhs: Expr,
    ident: InternedStr,
}

impl FieldExpr {
    pub fn new(lhs: Expr, ident: InternedStr) -> Self {
        Self { lhs, ident }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct IndexExpr {
    expr: Expr,
    key: Expr,
}

impl IndexExpr {
    pub fn new(expr: Expr, key: Expr) -> Self {
        Self { expr, key }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub enum ArrayExpr {
    SizedInitializer(Expr, Expr),
    Initializer(Vec<Expr>),
}

#[derive(Clone, PartialEq, Eq, Hash, Debug, Serialize, Deserialize)]
pub enum PathSegment {
    Identifier(InternedStr),
    Generic(Vec<Key>),
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct PathExpr {
    segments: Vec<PathSegment>,
}

impl PathExpr {
    pub fn new(segments: Vec<PathSegment>) -> Self {
        Self { segments }
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

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub enum Pattern {
    Wildcard,
    Or(OrPattern),
    Boolean(bool),
    Integer(i64),
    Float(f64),
    String(InternedStr),
    Ty(Key, Option<InternedStr>),
    Destructure(Key, Vec<Expr>),
    None,
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

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct OrPattern {
    patterns: Vec<Pattern>,
}

impl OrPattern {
    pub fn new(patterns: Vec<Pattern>) -> Self {
        Self { patterns }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct WhileStmt {
    condition: Expr,
    block_stmt: BlockStmt,
}

impl WhileStmt {
    pub fn new(condition: Expr, block_stmt: BlockStmt) -> Self {
        Self {
            condition,
            block_stmt,
        }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
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


#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub enum OuterStmt {
    Use(UseStmt),
    Let(LetStmt),
    Class(ClassStmt),
    Enum(EnumStmt),
    Trait(TraitStmt),
    TraitImpl(TraitImplStmt),
    Fn(FnStmt),
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
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
