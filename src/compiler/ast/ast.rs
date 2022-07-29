use crate::compiler::types::types::{Ident, Type};
use crate::gc::block::Block;
use crate::traits::traits::Trait;
use std::path::Prefix;

pub struct Module {
    use_stmts: Vec<UseStatement>,
    tys: Vec<TypeStatement>,
    fns: Vec<FunctionStatement>,
}

impl Module {
    pub fn new(
        use_stmts: Vec<UseStatement>,
        tys: Vec<TypeStatement>,
        fns: Vec<FunctionStatement>,
    ) -> Self {
        Self {
            use_stmts,
            tys,
            fns,
        }
    }

    pub fn use_statements(&self) -> &[UseStatement] {
        &self.use_stmts
    }

    pub fn tys(&self) -> &[TypeStatement] {
        &self.tys
    }

    pub fn fns(&self) -> &[FunctionStatement] {
        &self.fns
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
pub struct UseStatement {
    ident: QualifiedIdent,
}

impl UseStatement {
    pub fn new(ident: QualifiedIdent) -> Self {
        Self { ident }
    }
}

pub enum TypeStatement {
    Enum {
        enum_stmt: Box<EnumStatement>,
    },
    InlineClass {
        class_stmt: Box<InlineClassStatement>,
    },
    Class {
        class_stmt: Box<ClassStatement>,
    },
    Trait {
        trait_stmt: Box<TraitStatement>,
    },
}

pub struct InlineClassStatement {
    name: Ident,
    generic_types: Vec<GenericTy>,
    members: Vec<MemberDecl>,
    member_functions: Vec<MemberFunctionDecl>,
}

pub struct ClassStatement {
    name: Ident,
    generic_types: Vec<GenericTy>,
    members: Vec<MemberDecl>,
    member_functions: Vec<MemberFunctionDecl>,
}

impl ClassStatement {
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

pub struct EnumStatement {
    name: Ident,
    generic_tys: Vec<GenericTy>,
    members: Vec<EnumMemberDecl>,
}

impl EnumStatement {
    pub fn new(name: Ident, generic_types: Vec<GenericTy>, members: Vec<EnumMemberDecl>) -> Self {
        Self {
            name,
            generic_tys: generic_types,
            members,
        }
    }

    pub fn generic_tys(&self) -> &[GenericTy] {
        &self.generic_tys
    }

    pub fn members(&self) -> &[EnumMemberDecl] {
        &self.members
    }
}

pub struct TraitStatement {
    ident: Ident,
    functions: Vec<MemberFunctionDecl>,
}

pub struct FunctionStatement {
    ident: Ident,
    generic_types: Vec<GenericTy>,
    parameters: Vec<ParameterDecl>,
    return_type: Type,
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
    type_ref: Type,
}

#[derive(PartialEq, Debug)]
pub struct EnumMemberDecl {
    name: Ident,
    parameters: Vec<ParameterDecl>,
    member_functions: Vec<MemberFunctionDecl>,
}

impl EnumMemberDecl {
    pub fn new(
        name: Ident,
        parameters: Vec<ParameterDecl>,
        member_functions: Vec<MemberFunctionDecl>,
    ) -> Self {
        Self {
            name,
            parameters,
            member_functions,
        }
    }
}

#[derive(PartialEq, Debug)]
pub struct MemberFunctionDecl {
    ident: Ident,
    signature: FunctionSignature,
    body: BlockStatement,
}

impl MemberFunctionDecl {
    pub fn new(ident: Ident, signature: FunctionSignature, body: BlockStatement) -> Self {
        Self {
            ident,
            signature,
            body,
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct FunctionSignature {
    generic_types: Vec<GenericTy>,
    parameters: Vec<ParameterDecl>,
    return_type: Type,
}

impl FunctionSignature {
    pub fn new(
        generic_types: Vec<GenericTy>,
        parameters: Vec<ParameterDecl>,
        return_type: Type,
    ) -> Self {
        Self {
            generic_types,
            parameters,
            return_type,
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct ParameterDecl {
    name: Ident,
    ty: Type,
}

impl ParameterDecl {
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
pub struct LocalVarDecl {
    ident: Ident,
    ty: Type,
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

pub struct FunctionCall {
    func: Box<Expr>,
    generic_tys: Vec<GenericTy>,
    parameters: Vec<Expr>,
}

pub struct BinaryExpr {
    operator: BinaryOp,
    lhs: Expr,
    rhs: Expr,
}

pub struct UnaryExpr {
    operator: UnaryOp,
    expr: Expr,
}

pub struct IfExpr {
    condition: Expr,
    if_true: BlockStmt,
    if_false: Option<BlockStmt>,
}

pub struct WhileExpr {
    condition: Expr,
    block: BlockStmt,
}

pub struct ForExpr {
    pattern: Pattern,
    block: BlockStmt,
}

pub enum Pattern {
    Wildcard,
    Range(RangePattern),
    Or(OrPattern),
    Literal(Literal),
    Ty(TypePattern),
}

pub struct RangePattern {
    start: Expr,
    end: Expr,
}

pub struct OrPattern {
    patterns: Vec<Pattern>,
}

pub struct TypePattern {
    tys: Vec<Type>,
}

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
    Local(Box<LocalVarDecl>),
    Class(Box<ClassStatement>),
    Enum(Box<EnumStatement>),
    Trait(Box<TraitStatement>),
    Fn(Box<FunctionStatement>),
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
pub struct MatchArm {
    ty: Type,
    ident: Ident,
    statement: Stmt,
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
