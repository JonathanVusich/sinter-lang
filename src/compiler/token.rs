#[derive(PartialEq, Clone, Copy, Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub pos: usize,
}

#[derive(PartialEq, Clone, Copy, Debug)]
pub enum TokenType {
    Unrecognized(&'static str),

    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    LeftParentheses,
    RightParentheses,

    Comma,
    Dot,
    Minus,
    Plus,

    Semicolon,
    Slash,
    Star,

    Bang,
    BangEqual,
    Equal,
    EqualEqual,

    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    Identifier(&'static str),
    String(&'static str),
    SignedInteger(i64),
    Float(f64),

    // Keywords
    Class,
    Fn,

    Break,
    Continue,

    And,
    Or,

    Else,
    If,
    In,
    False,
    True,

    For,
    This,

    Var,
    Val,

    While,
    Return,
    SelfLowercase,
    SelfCapitalized,

    Native,

    Enum,
    Impl,
    Match,

    Pub,
    Static,

    Type,
    Use,

    Inline,
}

impl Token {

    pub fn new(token_type: TokenType, pos: usize) -> Self {
        Self {
            token_type,
            pos,
        }
    }
}