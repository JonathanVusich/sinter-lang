use crate::compiler::types::types::{Ident, Type};
use crate::gc::block::Block;
use crate::traits::traits::Trait;
use std::path::Prefix;

pub struct Module {
    stmts: Vec<Stmt>,
}

impl Module {
    pub fn new(stmts: Vec<Stmt>) -> Self {
        Self { stmts }
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct QualifiedIdent {
    idents: Vec<Ident>,
}

impl QualifiedIdent {
    pub fn new(idents: Vec<Ident>) -> Self {
        Self { idents }
    }
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct UseStmt {
    ident: QualifiedIdent,
}

impl UseStmt {
    pub fn new(ident: QualifiedIdent) -> Self {
        Self { ident }
    }
}

pub struct ClassStmt {
    name: Ident,
    generic_types: Vec<GenericTy>,
    members: Vec<MemberDecl>,
    member_functions: Vec<FnStmt>,
}

impl ClassStmt {
    pub fn new(
        name: Ident,
        generic_types: Vec<GenericTy>,
        members: Vec<MemberDecl>,
        member_functions: Vec<MemberFunctionDecl>,
    ) -> Self {
        Self {
            name,
            generic_types,
            members,
            member_functions,
        }
    }
}

pub struct EnumStmt {
    name: Ident,
    generic_tys: Vec<GenericTy>,
    members: Vec<EnumMemberStmt>,
}

impl EnumStmt {
    pub fn new(name: Ident, generic_types: Vec<GenericTy>, members: Vec<EnumMemberStmt>) -> Self {
        Self {
            name,
            generic_tys: generic_types,
            members,
        }
    }

    pub fn generic_tys(&self) -> &[GenericTy] {
        &self.generic_tys
    }

    pub fn members(&self) -> &[EnumMemberStmt] {
        &self.members
    }
}

pub struct TraitStmt {
    ident: Ident,
    functions: Vec<FnStmt>,
}

#[derive(PartialEq, Debug)]
pub struct TraitImplStmt {
    ty: Type,
    functions: Vec<FnStmt>,
}

#[derive(PartialEq, Debug)]
pub struct FnStmt {
    sig: FnSig,
    body: Option<BlockStmt>,
}

#[derive(PartialEq, Eq, Debug)]
pub struct GenericTy {
    ident: Ident,
    trait_bound: Option<Type>,
}

impl GenericTy {
    pub fn new(ident: Ident, trait_bound: Option<Type>) -> Self {
        Self { ident, trait_bound }
    }
}

pub struct MemberDecl {
    ident: Ident,
    ty: Type,
}

#[derive(PartialEq, Debug)]
pub struct EnumMemberStmt {
    name: Ident,
    parameters: Vec<Param>,
    member_functions: Vec<FnStmt>,
}

impl EnumMemberStmt {
    pub fn new(
        name: Ident,
        parameters: Vec<Param>,
        member_functions: Vec<MemberFunctionDecl>,
    ) -> Self {
        Self {
            name,
            parameters,
            member_functions,
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct FnSig {
    name: Ident,
    generic_types: Vec<GenericTy>,
    parameters: Vec<Param>,
    return_type: Type,
}

impl FnSig {
    pub fn new(generic_types: Vec<GenericTy>, parameters: Vec<Param>, return_type: Type) -> Self {
        Self {
            generic_types,
            parameters,
            return_type,
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Param {
    name: Ident,
    ty: Type,
}

impl Param {
    pub fn new(name: Ident, ty: Type) -> Self {
        Self { name, ty }
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct ArgumentDecl {
    ident: Ident,
    ty: Type,
}

#[derive(PartialEq, Debug)]
pub struct LocalStmt {
    ident: Ident,
    ty: Option<Type>,
    initializer: Option<Expr>,
}

#[derive(PartialEq, Debug)]
pub enum VarInitializer {
    Array(Vec<VarInitializer>),
    Statement(Stmt),
}

#[derive(PartialEq, Debug)]
pub enum Expr {
    Array(Vec<Expr>),
    Call(Box<FunctionCall>),
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
    Return,
    Try(Box<Expr>),
}

#[derive(PartialEq, Debug)]
pub struct FunctionCall {
    func: Box<Expr>,
    generic_tys: Vec<GenericTy>,
    parameters: Vec<Expr>,
}

#[derive(PartialEq, Debug)]
pub struct BinaryExpr {
    operator: BinaryOp,
    lhs: Expr,
    rhs: Expr,
}

#[derive(PartialEq, Debug)]
pub struct UnaryExpr {
    operator: UnaryOp,
    expr: Expr,
}

#[derive(PartialEq, Debug)]
pub struct IfExpr {
    condition: Expr,
    if_true: BlockStmt,
    if_false: Option<BlockStmt>,
}

#[derive(PartialEq, Debug)]
pub struct WhileExpr {
    condition: Expr,
    block: BlockStmt,
}

#[derive(PartialEq, Debug)]
pub struct ForExpr {
    pattern: Pattern,
    block: BlockStmt,
}

#[derive(PartialEq, Debug)]
pub struct MatchExpr {
    expr: Expr,
    arms: Vec<MatchArm>,
}

#[derive(PartialEq, Debug)]
pub struct MatchArm {
    pattern: Pattern,
    guard: Option<Expr>,
    body: Expr,
}

#[derive(PartialEq, Debug)]
pub struct ClosureExpr {
    params: Vec<Param>,
    expr: Expr,
}

#[derive(PartialEq, Debug)]
pub struct AssignExpr {
    lhs: Expr,
    rhs: Expr,
}

#[derive(PartialEq, Debug)]
pub struct AssignOpExpr {
    op: BinaryOp,
    lhs: Expr,
    rhs: Expr,
}

#[derive(PartialEq, Debug)]
pub struct FieldExpr {
    lhs: Expr,
    ident: Ident,
}

#[derive(PartialEq, Debug)]
pub struct IndexExpr {
    lhs: Expr,
    rhs: Expr,
}

#[derive(PartialEq, Debug)]
pub enum Pattern {
    Wildcard,
    Range(RangePattern),
    Or(OrPattern),
    Literal(Literal),
    Ty(TypePattern),
}

#[derive(PartialEq, Debug)]
pub struct RangePattern {
    start: Expr,
    end: Expr,
}

#[derive(PartialEq, Debug)]
pub struct OrPattern {
    patterns: Vec<Pattern>,
}

#[derive(PartialEq, Debug)]
pub struct TypePattern {
    tys: Vec<Type>,
}

#[derive(PartialEq, Debug)]
pub struct BlockStmt {
    stmts: Vec<Stmt>,
}

#[derive(PartialEq, Debug)]
pub enum Literal {
    IntegerLiteral(i64),
    FloatingPointerLiteral(f64),
    BooleanLiteral(bool),
    StringLiteral(Ident),
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

#[derive(PartialEq, Eq, Debug)]
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

#[derive(PartialEq, Eq, Debug)]
pub enum UnaryOp {
    Bang,
    Negate,
    Plus,
}

#[derive(PartialEq, Debug)]
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

#[derive(PartialEq, Debug)]
pub struct IfExpression {
    condition: Box<Expr>,
    statement_true: Expr,
    statement_false: Option<Expr>,
}

#[derive(PartialEq, Debug)]
pub struct MatchStatement {
    expr: Box<Expr>,
    statements: Vec<MatchArm>,
}

#[derive(PartialEq, Debug)]
pub struct WhileExpression {
    condition: Box<Expr>,
    statement: Stmt,
}

#[derive(PartialEq, Debug)]
pub struct ForExpression {
    ident: Ident,
    loop_expr: Box<Expr>,
    statement: Stmt,
}

mod tests {
    use crate::compiler::ast::{Expr, MemberFunctionDecl, Module, TypeStatement};

    #[test]
    pub fn check_size() {
        assert_eq!(72, std::mem::size_of::<Module>());
        assert_eq!(16, std::mem::size_of::<TypeStatement>());

        assert_eq!(280, std::mem::size_of::<Expr>());
    }
}
