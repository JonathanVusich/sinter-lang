use string_interner::symbol::SymbolUsize;
use crate::compiler::types::types::{Identifier, Type};
use crate::traits::traits::Trait;

pub struct Module {
    use_stmts: Vec<UseStatement>,
    tys: Vec<TypeStatement>,
    fns: Vec<FunctionStatement>,
}

impl Module {

    pub fn new(use_stmts: Vec<UseStatement>, tys: Vec<TypeStatement>, fns: Vec<FunctionStatement>) -> Self {
        Self {
            use_stmts,
            tys,
            fns,
        }
    }
}

pub struct UseStatement {
    ident: Identifier,
}

impl UseStatement {

    pub fn new(ident: Identifier) -> Self {
        Self {
            ident,
        }
    }
}

pub enum TypeStatement {
    Enum(Box<EnumStatement>),
    InlineClass(Box<InlineClassStatement>),
    Class(Box<ClassStatement>),
    Trait(Box<TraitStatement>),
}

pub struct InlineClassStatement {
    name: Identifier,
    generic_types: Vec<GenericTypeDecl>,
    members: Vec<MemberDecl>,
    member_functions: Vec<MemberFunctionDecl>,
}

pub struct ClassStatement {
    name: Identifier,
    generic_types: Vec<GenericTypeDecl>,
    members: Vec<MemberDecl>,
    member_functions: Vec<MemberFunctionDecl>,
}

pub struct EnumStatement {
    ident: Identifier,
    members: Vec<EnumMemberDecl>,
}

pub struct TraitStatement {
    ident: Identifier,
    functions: Vec<MemberFunctionDecl>,
}

pub struct FunctionStatement {
    ident: Identifier,
    generic_types: Vec<GenericTypeDecl>,
    parameters: Vec<ParameterDecl>,
    return_type: Type,
}

pub struct GenericTypeDecl {
    ident: Identifier,
    type_ref: Type,
}

pub struct MemberDecl {
    ident: Identifier,
    type_ref: Type,
}

pub struct EnumMemberDecl {
    ident: Identifier,
    parameters: Vec<ParameterDecl>,
    member_functions: Vec<MemberFunctionDecl>,
}

pub struct MemberFunctionDecl {
    ident: Identifier,
    generic_types: Vec<GenericTypeDecl>,
    parameters: Vec<ParameterDecl>,
    return_type: Type,
    body: BlockStatement,
}

pub struct ParameterDecl {
    ident: Identifier,
    ty: Type,
}

pub struct ArgumentDecl {
    ident: Identifier,
    ty: Type,
}

pub enum BlockStatement {
    LocalVarDeclaration(LocalVarDecl),
    Statement(Statement),
}

pub struct LocalVarDecl {
    ident: Identifier,
    ty: Type,
    initializer: Option<VarInitializer>,
}

pub enum VarInitializer {
    Array(Vec<VarInitializer>),
    Statement(Statement)
}

pub struct Expression {
    lhs: InfixExpression,
    rhs: Option<(AssignmentOperator, InfixExpression)>,
}

pub struct InfixExpression {
    lhs: BasicExpression,
    rhs: Option<(InfixOperator, BasicExpression)>,
}

pub struct BasicExpression {
    prefix: Option<PrefixOperator>,
    primary: PrimaryExpression,
    selector: Vec<Selector>,
}

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
        ident: Identifier,
        arguments: Vec<ArgumentDecl>,
    }
}

pub enum Selector {
    FunctionCall {
        ident: Identifier,
        arguments: Vec<ArgumentDecl>,
    },
    Expression {
        expr: Box<Expression>,
    }
}

pub enum Literal {
    IntegerLiteral(i64),
    FloatingPointerLiteral(f64),
    BooleanLiteral(bool),
    StringLiteral(Identifier),
    None,
}

pub enum AssignmentOperator {
    Assign,
    PlusAssign,
    SubtractAssign,
    MultiplyAssign,
    DivideAssign,
    ModuloAssign,
}

pub enum InfixOperator {
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

pub enum PrefixOperator {
    Bang,
    Negate,
    Plus,
}

pub enum Statement {
    Block(Box<BlockStatement>),
    Expression(Box<Expression>),
    If(Box<IfStatement>),
    Match(Box<MatchStatement>),
    While(Box<WhileStatement>),
    For(Box<ForStatement>),
    Break,
    Continue,
    Return(Box<Expression>),
}

pub struct IfStatement {
    condition: Box<Expression>,
    statement_true: Statement,
    statement_false: Option<Statement>,
}

pub struct MatchStatement {
    expr: Box<Expression>,
    statements: Vec<MatchArm>,
}

pub struct MatchArm {
    ty: Type,
    ident: Identifier,
    statement: Statement,
}

pub struct WhileStatement {
    condition: Box<Expression>,
    statement: Statement,
}

pub struct ForStatement {
    ident: Identifier,
    loop_expr: Box<Expression>,
    statement: Statement,
}

mod tests {
    use crate::compiler::ast::{Expression, MemberFunctionDecl, Module, TypeStatement};

    #[test]
    pub fn check_size() {
        assert_eq!(48, std::mem::size_of::<Module>());
        assert_eq!(16, std::mem::size_of::<TypeStatement>());

        assert_eq!(0, std::mem::size_of::<Expression>());
    }
}
