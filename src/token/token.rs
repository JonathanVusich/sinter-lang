pub struct Token {
    token_type: TokenType,
    line: usize,
    pos: usize,
}

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
    UnsignedInteger(u64),
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

    Inline,
}

impl Token {

    pub fn new(token_type: TokenType, line: usize, pos: usize) -> Self {
        Self {
            token_type,
            line,
            pos,
        }
    }
}