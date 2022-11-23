use std::fmt::{Display, Formatter};
use serde::{Serialize, Deserialize};
use crate::compiler::types::types::InternedStr;

#[derive(PartialEq, Clone, Copy, Debug, Serialize, Deserialize)]
pub struct Token {
    pub token_type: TokenType,
    pub start: usize,
    pub end: usize,
}

#[derive(PartialEq, Clone, Copy, Debug, Serialize, Deserialize)]
pub enum TokenType {
    Unrecognized(InternedStr),

    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    LeftParentheses,
    RightParentheses,

    Comma,
    Dot,
    Colon,
    Semicolon,

    Minus,
    Plus,

    Slash,
    Star,
    Percent,

    Bang,
    BangEqual,
    Equal,
    EqualEqual,

    RightArrow,

    Greater,
    GreaterEqual,
    RightShift,
    TripleRightShift,

    Less,
    LessEqual,
    LeftShift,

    Identifier(InternedStr),
    String(InternedStr),
    SignedInteger(i64),
    Float(f64),

    // Keywords
    Class,
    Fn,

    Break,
    Continue,

    And,
    Or,

    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseComplement,

    Else,
    If,
    False,
    True,

    For,

    Let,
    Mut,

    While,
    Return,
    SelfLowercase,
    SelfCapitalized,

    In,

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
    Sync
}

impl TokenType {

    pub fn is_operator(self) -> bool {
        matches!(self, TokenType::Equal |
            TokenType::Or |
            TokenType::And |
            TokenType::BitwiseOr |
            TokenType::BitwiseXor |
            TokenType::BitwiseAnd |
            TokenType::BitwiseComplement |
            TokenType::EqualEqual |
            TokenType::BangEqual |
            TokenType::Less |
            TokenType::Greater |
            TokenType::LessEqual |
            TokenType::GreaterEqual |
            TokenType::LeftShift |
            TokenType::RightShift |
            TokenType::TripleRightShift |
            TokenType::Plus |
            TokenType::Minus |
            TokenType::Star |
            TokenType::Slash |
            TokenType::Percent |
            TokenType::Bang)
    }
}

impl Display for TokenType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::Unrecognized(_) => write!(f, "unrecognized"),
            TokenType::LeftBrace => write!(f, "{{"),
            TokenType::RightBrace => write!(f, "}}"),
            TokenType::LeftBracket => write!(f, "["),
            TokenType::RightBracket => write!(f, "]"),
            TokenType::LeftParentheses => write!(f, "("),
            TokenType::RightParentheses => write!(f, ")"),
            TokenType::Comma => write!(f, ","),
            TokenType::Dot => write!(f, "."),
            TokenType::Minus => write!(f, "-"),
            TokenType::Plus => write!(f, "+"),
            TokenType::Colon => write!(f, ":"),
            TokenType::Semicolon => write!(f, ";"),
            TokenType::Slash => write!(f, "\\"),
            TokenType::Percent => write!(f, "%"),
            TokenType::Star => write!(f, "*"),
            TokenType::BitwiseOr => write!(f, "|"),
            TokenType::Bang => write!(f, "!"),
            TokenType::BangEqual => write!(f, "!="),
            TokenType::Equal => write!(f, "="),
            TokenType::EqualEqual => write!(f, "=="),
            TokenType::RightArrow => write!(f, "=>"),
            TokenType::Greater => write!(f, ">"),
            TokenType::GreaterEqual => write!(f, ">="),
            TokenType::Less => write!(f, "<"),
            TokenType::LessEqual => write!(f, "<="),
            TokenType::Identifier(identifier) => write!(f, "identifier"),
            TokenType::String(_) => write!(f, "string"),
            TokenType::SignedInteger(_) => write!(f, "signed integer"),
            TokenType::Float(_) => write!(f, "float"),
            TokenType::Class => write!(f, "class"),
            TokenType::Fn => write!(f, "fn"),
            TokenType::Break => write!(f, "break"),
            TokenType::Continue => write!(f, "continue"),
            TokenType::And => write!(f, "&&"),
            TokenType::Or => write!(f, "||"),
            TokenType::Else => write!(f, "else"),
            TokenType::If => write!(f, "if"),
            TokenType::False => write!(f, "false"),
            TokenType::True => write!(f, "true"),
            TokenType::For => write!(f, "for"),
            TokenType::While => write!(f, "while"),
            TokenType::Return => write!(f, "return"),
            TokenType::SelfLowercase => write!(f, "self"),
            TokenType::SelfCapitalized => write!(f, "Self"),
            TokenType::Native => write!(f, "native"),
            TokenType::Enum => write!(f, "enum"),
            TokenType::Impl => write!(f, "impl"),
            TokenType::Match => write!(f, "match"),
            TokenType::Pub => write!(f, "pub"),
            TokenType::Static => write!(f, "static"),
            TokenType::Type => write!(f, "type"),
            TokenType::Trait => write!(f, "trait"),
            TokenType::Use => write!(f, "use"),
            TokenType::None => write!(f, "None"),
            TokenType::Inline => write!(f, "inline"),
            TokenType::Sync => write!(f, "sync"),
            TokenType::Let => write!(f, "let"),
            TokenType::Mut => write!(f, "mut"),
            TokenType::In => write!(f, "in"),
            TokenType::RightShift => write!(f, ">>"),
            TokenType::TripleRightShift => write!(f, ">>>"),
            TokenType::LeftShift => write!(f, "<<"),
            TokenType::BitwiseAnd => write!(f, "&"),
            TokenType::BitwiseXor => write!(f, "^"),
            TokenType::BitwiseComplement => write!(f, "~"),
        }
    }
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
