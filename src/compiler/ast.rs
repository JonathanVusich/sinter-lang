use crate::types::types::Type;

pub struct AbstractSyntaxTree {
    expression: Expression,
}

pub enum Expression {
    Literal(Box<LiteralExpression>),
    Assignment(Box<AssignmentExpression>),
    VariableDeclaration(Box<VariableDeclaration>),
    StructDeclaration(Box<StructDeclaration>),
    EnumDeclaration(Box<EnumDeclaration>),
    FunctionDeclaration(Box<FunctionDeclaration>),
}

pub enum UnaryOperator {
    Negate,
    Not,
}

pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,

    LessThan,
    LessThanEqual,
    Equal,
    NotEqual,
    GreaterThanEqual,
    GreaterThan,
}

pub struct TypeReference {

}
