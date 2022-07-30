use crate::compiler::types::types::{Ident, Type};
use crate::gc::block::Block;
use crate::traits::traits::Trait;
use std::path::Prefix;

pub struct Module {
    use_stmts: Vec<UseStmt>,
    stmts: Vec<Stmt>,
}

impl Module {
    pub fn new(use_stmts: Vec<UseStmt>, stmts: Vec<Stmt>) -> Self {
        Self { use_stmts, stmts }
    }

    pub fn use_stmts(&self) -> &[UseStmt] {
        &self.use_stmts
    }

    pub fn stmts(&self) -> &[Stmt] {
        &self.stmts
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

#[derive(PartialEq, Eq, Debug, Default)]
pub struct Generics {
    generics: Vec<GenericTy>,
}

impl Generics {
    pub fn new(generics: Vec<GenericTy>) -> Self {
        Self {
            generics,
        }
    }
}

#[derive(PartialEq, Eq, Debug, Default, Clone)]
pub struct Params {
    params: Vec<Param>
}

impl Params {
    pub fn new(params: Vec<Param>) -> Self {
        Self {
            params,
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Mutability {
    Mutable,
    Immutable,
}

#[derive(PartialEq, Debug)]
pub struct ClassStmt {
    name: Ident,
    generic_types: Generics,
    members: Params,
    member_functions: Vec<FnStmt>,
}

impl ClassStmt {
    pub fn new(
        name: Ident,
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

#[derive(PartialEq, Debug)]
pub struct EnumStmt {
    name: Ident,
    generic_tys: Generics,
    members: Vec<EnumMemberStmt>,
}

impl EnumStmt {
    pub fn new(name: Ident, generic_types: Generics, members: Vec<EnumMemberStmt>) -> Self {
        Self {
            name,
            generic_tys: generic_types,
            members,
        }
    }

    pub fn generics(&self) -> &Generics {
        &self.generic_tys
    }

    pub fn members(&self) -> &[EnumMemberStmt] {
        &self.members
    }
}

#[derive(PartialEq, Debug)]
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
    name: Ident,
    sig: FnSig,
    body: Option<BlockStmt>,
}

impl FnStmt {

    pub fn new(name: Ident, sig: FnSig, body: Option<BlockStmt>) -> Self {
        Self {
            name,
            sig,
            body,
        }
    }
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

#[derive(PartialEq, Debug)]
pub struct EnumMemberStmt {
    name: Ident,
    parameters: Params,
    member_functions: Vec<FnStmt>,
}

impl EnumMemberStmt {
    pub fn new(
        name: Ident,
        parameters: Params,
        member_functions: Vec<FnStmt>,
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
    generic_types: Generics,
    parameters: Params,
    return_type: Option<Type>,
}

impl FnSig {
    pub fn new(name: Ident, generic_types: Generics, parameters: Params, return_type: Option<Type>) -> Self {
        Self {
            name,
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
    mutability: Mutability,
}

impl Param {
    pub fn new(name: Ident, ty: Type, mutability: Mutability) -> Self {
        Self { name, ty, mutability, }
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
    Return,
    Try(Box<Expr>),
}

#[derive(PartialEq, Debug)]
pub struct FnCall {
    func: Box<Expr>,
    generic_tys: Generics,
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
    params: Params,
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

impl BlockStmt {
    pub fn new(stmts: Vec<Stmt>) -> Self {
        Self {
            stmts,
        }
    }
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

mod tests {

}
