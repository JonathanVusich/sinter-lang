use crate::types::types::Type;

pub struct AbstractSyntaxTree {
    expression: Expression,
}

pub enum Expression {
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
