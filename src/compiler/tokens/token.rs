#[derive(PartialEq, Clone, Copy, Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub start: usize,
    pub end: usize,
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

    Colon,
    Semicolon,
    Slash,
    Star,
    Pipe,

    Bang,
    BangEqual,
    Equal,
    EqualEqual,

    RightArrow,

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
    Trait,
    Use,
    None,

    Inline,
}

impl Token {

    pub fn new(token_type: TokenType, start: usize, end: usize) -> Self {
        Self {
            token_type,
            start,
            end,
        }
    }

    pub fn end_pos(&self) -> usize {
        self.start + self.end
    }
}