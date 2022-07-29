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
    generic_types: Vec<GenericTypeDecl>,
    members: Vec<MemberDecl>,
    member_functions: Vec<MemberFunctionDecl>,
}

pub struct ClassStatement {
    name: Ident,
    generic_types: Vec<GenericTypeDecl>,
    members: Vec<MemberDecl>,
    member_functions: Vec<MemberFunctionDecl>,
}

impl ClassStatement {
    pub fn new(
        name: Ident,
        generic_types: Vec<GenericTypeDecl>,
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
    generic_tys: Vec<GenericTypeDecl>,
    members: Vec<EnumMemberDecl>,
}

impl EnumStatement {
    pub fn new(
        name: Ident,
        generic_types: Vec<GenericTypeDecl>,
        members: Vec<EnumMemberDecl>,
    ) -> Self {
        Self {
            name,
            generic_tys: generic_types,
            members,
        }
    }

    pub fn generic_tys(&self) -> &[GenericTypeDecl] {
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
    generic_types: Vec<GenericTypeDecl>,
    parameters: Vec<ParameterDecl>,
    return_type: Type,
}

#[derive(PartialEq, Eq, Debug)]
pub struct GenericTypeDecl {
    ident: Ident,
    trait_bound: Option<Type>,
}

impl GenericTypeDecl {
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
    generic_types: Vec<GenericTypeDecl>,
    parameters: Vec<ParameterDecl>,
    return_type: Type,
}

impl FunctionSignature {
    pub fn new(
        generic_types: Vec<GenericTypeDecl>,
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
    initializer: Option<Expression>,
}

#[derive(PartialEq, Debug)]
pub enum VarInitializer {
    Array(Vec<VarInitializer>),
    Statement(Statement),
}

#[derive(PartialEq, Debug)]
pub enum Expression {
    Array(Vec<VarInitializer>),
    Call(Box<Expression>, Vec<Expression>),
    Binary(BinaryOperator, Box<Expression>, Box<Expression>),
    Unary(UnaryOperator, Box<Expression>),
    Literal(Literal),
    If(IfExpression),
    While(WhileExpression),
    For(ForExpression),
    Match(MatchExpression),

}

impl Expression {
    pub fn new(lhs: InfixExpression, rhs: Option<(AssignmentOperator, InfixExpression)>) -> Self {
        Self { lhs, rhs }
    }
}

#[derive(PartialEq, Debug)]
pub struct InfixExpression {
    lhs: BasicExpression,
    rhs: Option<(BinaryOperator, BasicExpression)>,
}

impl InfixExpression {
    pub fn new(lhs: BasicExpression, rhs: Option<(BinaryOperator, BasicExpression)>) -> Self {
        Self { lhs, rhs }
    }
}

#[derive(PartialEq, Debug)]
pub struct BasicExpression {
    prefix: Option<UnaryOperator>,
    primary: PrimaryExpression,
    selector: Vec<Selector>,
}

impl BasicExpression {
    pub fn new(
        prefix: Option<UnaryOperator>,
        primary: PrimaryExpression,
        selector: Vec<Selector>,
    ) -> Self {
        Self {
            prefix,
            primary,
            selector,
        }
    }
}

#[derive(PartialEq, Debug)]
pub enum PrimaryExpression {
    Literal {
        literal: Literal,
    },
    InternalConstructor {
        arguments: Vec<ArgumentDecl>,
    },
    ArrayInitializer {
        initializers: Vec<VarInitializer>,
    },
    ClassInitializer {
        ident: Ident,
        arguments: Vec<ArgumentDecl>,
    },
}

#[derive(PartialEq, Debug)]
pub enum Selector {
    FunctionCall {
        ident: Ident,
        arguments: Vec<ArgumentDecl>,
    },
    Expression {
        expr: Box<Expression>,
    },
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
pub enum AssignmentOperator {
    Assign,
    PlusAssign,
    SubtractAssign,
    MultiplyAssign,
    DivideAssign,
    ModuloAssign,
}

#[derive(PartialEq, Eq, Debug)]
pub enum BinaryOperator {
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
pub enum UnaryOperator {
    Bang,
    Negate,
    Plus,
}

#[derive(PartialEq, Debug)]
pub enum Statement {
    Local(Box<LocalVarDecl>),
    InternalConstructor(Vec<ArgumentDecl>),
    Array(Vec<VarInitializer>),
    Class(Ident, Vec<ArgumentDecl>),
    Binary(BinaryOperator, Box<Statement>, Box<Statement>),
    Unary(UnaryOperator, Box<Statement>),
    Call(Box<Statement>, Vec<ArgumentDecl>>),
    Expression(Box<Expression>), // Expand this
    If(Box<IfExpression>),
    Match(Box<MatchStatement>),
    While(Box<WhileExpression>),
    For(Box<ForExpression>),
    Break,
    Continue,
    Return(Box<Expression>),
}

#[derive(PartialEq, Debug)]
pub struct IfExpression {
    condition: Box<Expression>,
    statement_true: Expression,
    statement_false: Option<Expression>,
}

#[derive(PartialEq, Debug)]
pub struct MatchStatement {
    expr: Box<Expression>,
    statements: Vec<MatchArm>,
}

#[derive(PartialEq, Debug)]
pub struct MatchArm {
    ty: Type,
    ident: Ident,
    statement: Statement,
}

#[derive(PartialEq, Debug)]
pub struct WhileExpression {
    condition: Box<Expression>,
    statement: Statement,
}

#[derive(PartialEq, Debug)]
pub struct ForExpression {
    ident: Ident,
    loop_expr: Box<Expression>,
    statement: Statement,
}

mod tests {
    use crate::compiler::ast::{Expression, MemberFunctionDecl, Module, TypeStatement};

    #[test]
    pub fn check_size() {
        assert_eq!(72, std::mem::size_of::<Module>());
        assert_eq!(16, std::mem::size_of::<TypeStatement>());

        assert_eq!(280, std::mem::size_of::<Expression>());
    }
}
