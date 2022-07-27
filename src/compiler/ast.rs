use crate::compiler::types::types::{Ident, Type};
use crate::gc::block::Block;
use crate::traits::traits::Trait;

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
    Enum(Box<EnumStatement>),
    InlineClass(Box<InlineClassStatement>),
    Class(Box<ClassStatement>),
    Trait(Box<TraitStatement>),
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
    generic_types: Vec<GenericTypeDecl>,
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
            generic_types,
            members,
        }
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

pub struct FunctionSignature {
    generic_types: Vec<GenericTypeDecl>,
    parameters: Vec<ParameterDecl>,
    return_type: Type,
}

impl FunctionSignature {
    
    pub fn new(generic_types: Vec<GenericTypeDecl>, parameters: Vec<ParameterDecl>, return_type: Type) -> Self {
        Self {
            generic_types,
            parameters,
            return_type,
        }
    }
}

pub struct ParameterDecl {
    name: Ident,
    ty: Type,
}

impl ParameterDecl {
    pub fn new(name: Ident, ty: Type) -> Self {
        Self { name, ty }
    }
}

pub struct ArgumentDecl {
    ident: Ident,
    ty: Type,
}

pub enum BlockStatement {
    LocalVarDeclaration(LocalVarDecl),
    Statement(Statement),
}

pub struct LocalVarDecl {
    ident: Ident,
    ty: Type,
    initializer: Option<VarInitializer>,
}

pub enum VarInitializer {
    Array(Vec<VarInitializer>),
    Statement(Statement),
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
        ident: Ident,
        arguments: Vec<ArgumentDecl>,
    },
}

pub enum Selector {
    FunctionCall {
        ident: Ident,
        arguments: Vec<ArgumentDecl>,
    },
    Expression {
        expr: Box<Expression>,
    },
}

pub enum Literal {
    IntegerLiteral(i64),
    FloatingPointerLiteral(f64),
    BooleanLiteral(bool),
    StringLiteral(Ident),
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
    ident: Ident,
    statement: Statement,
}

pub struct WhileStatement {
    condition: Box<Expression>,
    statement: Statement,
}

pub struct ForStatement {
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
