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

    And,
    Or,

    Else,
    If,
    False,
    True,

    For,
    This,

    Var,
    Val,

    While,
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