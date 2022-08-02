use crate::compiler::types::types::{InternedStr, Type};
use crate::gc::block::Block;
use crate::traits::traits::Trait;
use serde::{Deserialize, Serialize};
use std::path::Prefix;

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct Module {
    stmts: Vec<Stmt>,
}

impl Module {
    pub fn new(stmts: Vec<Stmt>) -> Self {
        Self { stmts }
    }

    pub fn stmts(&self) -> &[Stmt] {
        &self.stmts
    }
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

#[derive(Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct UseStmt {
    ident: QualifiedIdent,
}

impl UseStmt {
    pub fn new(ident: QualifiedIdent) -> Self {
        Self { ident }
    }
}

#[derive(PartialEq, Eq, Debug, Default, Serialize, Deserialize)]
pub struct Generics {
    generics: Vec<GenericTy>,
}

impl Generics {
    pub fn new(generics: Vec<GenericTy>) -> Self {
        Self { generics }
    }
}

#[derive(PartialEq, Eq, Debug, Default, Clone, Serialize, Deserialize)]
pub struct Params {
    params: Vec<Param>,
}

impl Params {
    pub fn new(params: Vec<Param>) -> Self {
        Self { params }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct Args {
    args: Vec<Box<Expr>>,
}

impl Args {
    pub fn new(args: Vec<Box<Expr>>) -> Self {
        Self { args }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Serialize, Deserialize)]
pub enum Mutability {
    Mutable,
    Immutable,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct ClassStmt {
    name: InternedStr,
    generic_types: Generics,
    members: Params,
    member_functions: Vec<FnStmt>,
}

impl ClassStmt {
    pub fn new(
        name: InternedStr,
        generic_types: Generics,
        members: Params,
        member_functions: Vec<FnStmt>,
    ) -> Self {
        Self {
            name,
            generic_types,
            members,
            member_functions,
        }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct EnumStmt {
    name: InternedStr,
    generics: Generics,
    members: Vec<EnumMemberStmt>,
}

impl EnumStmt {
    pub fn new(name: InternedStr, generic_types: Generics, members: Vec<EnumMemberStmt>) -> Self {
        Self {
            name,
            generics: generic_types,
            members,
        }
    }

    pub fn generics(&self) -> &Generics {
        &self.generics
    }

    pub fn members(&self) -> &[EnumMemberStmt] {
        &self.members
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct TraitStmt {
    ident: InternedStr,
    functions: Vec<FnStmt>,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct TraitImplStmt {
    ty: Type,
    functions: Vec<FnStmt>,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct FnStmt {
    sig: FnSig,
    body: Option<BlockStmt>,
}

impl FnStmt {
    pub fn new(sig: FnSig, body: Option<BlockStmt>) -> Self {
        Self { sig, body }
    }
}

#[derive(PartialEq, Eq, Debug, Serialize, Deserialize)]
pub struct GenericTy {
    ident: InternedStr,
    trait_bound: Option<Type>,
}

impl GenericTy {
    pub fn new(ident: InternedStr, trait_bound: Option<Type>) -> Self {
        Self { ident, trait_bound }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct EnumMemberStmt {
    name: InternedStr,
    parameters: Params,
    member_functions: Vec<FnStmt>,
}

impl EnumMemberStmt {
    pub fn new(name: InternedStr, parameters: Params, member_functions: Vec<FnStmt>) -> Self {
        Self {
            name,
            parameters,
            member_functions,
        }
    }
}

#[derive(PartialEq, Eq, Debug, Serialize, Deserialize)]
pub struct FnSig {
    name: InternedStr,
    generic_types: Generics,
    parameters: Params,
    return_type: Option<Type>,
}

impl FnSig {
    pub fn new(
        name: InternedStr,
        generic_types: Generics,
        parameters: Params,
        return_type: Option<Type>,
    ) -> Self {
        Self {
            name,
            generic_types,
            parameters,
            return_type,
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug, Serialize, Deserialize)]
pub struct Param {
    name: InternedStr,
    ty: Type,
    mutability: Mutability,
}

impl Param {
    pub fn new(name: InternedStr, ty: Type, mutability: Mutability) -> Self {
        Self {
            name,
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
pub struct LocalStmt {
    ident: InternedStr,
    ty: Option<Type>,
    initializer: Option<Expr>,
}

#[derive(PartialEq, Debug)]
pub enum VarInitializer {
    Array(Vec<VarInitializer>),
    Statement(Stmt),
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub enum Expr {
    Array(Vec<Expr>),
    Call(Box<FnCall>),
    Binary(Box<BinaryExpr>),
    Unary(Box<UnaryExpr>),
    Literal(Literal),
    If(Box<IfExpr>),
    While(Box<WhileExpr>),
    For(Box<ForExpr>),
    Match(Box<MatchExpr>),
    Closure(Box<ClosureExpr>),
    Assign(Box<AssignExpr>),
    AssignOp(Box<AssignOpExpr>),
    Field(Box<FieldExpr>),
    Index(Box<IndexExpr>),
    Break,
    Continue,
    Return(Option<Box<Expr>>),
    Try(Box<Expr>),
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct FnCall {
    func: Box<Expr>,
    generic_tys: Generics,
    args: Args,
}

impl FnCall {
    pub fn new(func: Box<Expr>, generic_tys: Generics, args: Args) -> Self {
        Self {
            func,
            generic_tys,
            args,
        }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct BinaryExpr {
    operator: BinaryOp,
    lhs: Expr,
    rhs: Expr,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct UnaryExpr {
    operator: UnaryOp,
    expr: Expr,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct IfExpr {
    condition: Expr,
    if_true: BlockStmt,
    if_false: Option<BlockStmt>,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct WhileExpr {
    condition: Expr,
    block: BlockStmt,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct ForExpr {
    pattern: Pattern,
    block: BlockStmt,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct MatchExpr {
    expr: Expr,
    arms: Vec<MatchArm>,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct MatchArm {
    pattern: Pattern,
    guard: Option<Expr>,
    body: Expr,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct ClosureExpr {
    params: Params,
    expr: Expr,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct AssignExpr {
    lhs: Expr,
    rhs: Expr,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct AssignOpExpr {
    op: BinaryOp,
    lhs: Expr,
    rhs: Expr,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct FieldExpr {
    lhs: Expr,
    ident: InternedStr,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct IndexExpr {
    lhs: Expr,
    rhs: Expr,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub enum Pattern {
    Wildcard,
    Range(RangePattern),
    Or(OrPattern),
    Literal(Literal),
    Ty(TypePattern),
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct RangePattern {
    start: Expr,
    end: Expr,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct OrPattern {
    patterns: Vec<Pattern>,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct TypePattern {
    tys: Vec<Type>,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct BlockStmt {
    stmts: Vec<Stmt>,
}

impl BlockStmt {
    pub fn new(stmts: Vec<Stmt>) -> Self {
        Self { stmts }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub enum Literal {
    IntegerLiteral(i64),
    FloatingPointerLiteral(f64),
    BooleanLiteral(bool),
    StringLiteral(InternedStr),
    None,
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

#[derive(PartialEq, Eq, Debug, Serialize, Deserialize)]
pub enum BinaryOp {
    Or,
    And,
    Equal,
    NotEqual,
    Less,
    Greater,
    LessThan,
    GreaterThan,
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo,
}

#[derive(PartialEq, Eq, Debug, Serialize, Deserialize)]
pub enum UnaryOp {
    Bang,
    Negate,
    Plus,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub enum Stmt {
    Use(Box<UseStmt>),
    Local(Box<LocalStmt>),
    Class(Box<ClassStmt>),
    Enum(Box<EnumStmt>),
    Trait(Box<TraitStmt>),
    TraitImpl(Box<TraitImplStmt>),
    Fn(Box<FnStmt>),
    Expression(Box<Expr>),
}

mod tests {}
