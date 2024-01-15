use std::borrow::Cow;
use std::fmt::{Display, Formatter};

use crate::compiler::compiler::CompilerCtxt;
use crate::compiler::StringInterner;
use serde::{Deserialize, Serialize};

use crate::compiler::tokens::tokenized_file::Span;
use crate::compiler::types::InternedStr;

#[derive(PartialEq, Clone, Copy, Debug, Serialize, Deserialize)]
pub struct Token {
    pub token_type: TokenType,
    pub span: Span,
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

    Less,
    LessEqual,

    Identifier(InternedStr),
    String(InternedStr),
    Int(i64),
    UInt(u64),
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

    Underscore,

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

    Ref,
}

pub enum PrintOption {
    Type,
    Value,
}

impl TokenType {
    pub(crate) fn pretty_print<'a>(
        &'a self,
        ctxt: &'a CompilerCtxt,
        print_option: PrintOption,
    ) -> Cow<str> {
        match self {
            TokenType::Unrecognized(str) => {
                let interned_str = ctxt.resolve_str(*str);
                Cow::Borrowed(interned_str)
            }
            TokenType::Identifier(str) => match print_option {
                PrintOption::Type => Cow::Borrowed("'identifier'"),
                PrintOption::Value => {
                    let interned_str = ctxt.resolve_str(*str);
                    Cow::Borrowed(interned_str)
                }
            },
            TokenType::String(str) => match print_option {
                PrintOption::Type => Cow::Borrowed("'string'"),
                PrintOption::Value => {
                    let interned_str = ctxt.resolve_str(*str);
                    Cow::Borrowed(interned_str)
                }
            },
            TokenType::Int(int) => match print_option {
                PrintOption::Type => Cow::Borrowed("'int'"),
                PrintOption::Value => Cow::Owned(int.to_string()),
            },
            TokenType::UInt(uint) => match print_option {
                PrintOption::Type => Cow::Borrowed("'uint'"),
                PrintOption::Value => Cow::Owned(uint.to_string()),
            },
            TokenType::Float(float) => match print_option {
                PrintOption::Type => Cow::Borrowed("'float'"),
                PrintOption::Value => Cow::Owned(float.to_string()),
            },
            TokenType::LeftBrace => Cow::Borrowed("{"),
            TokenType::RightBrace => Cow::Borrowed("}"),
            TokenType::LeftBracket => Cow::Borrowed("["),
            TokenType::RightBracket => Cow::Borrowed("]"),
            TokenType::LeftParentheses => Cow::Borrowed("("),
            TokenType::RightParentheses => Cow::Borrowed(")"),
            TokenType::Comma => Cow::Borrowed(","),
            TokenType::Dot => Cow::Borrowed("."),
            TokenType::Colon => Cow::Borrowed(":"),
            TokenType::Semicolon => Cow::Borrowed(";"),
            TokenType::Minus => Cow::Borrowed("-"),
            TokenType::Plus => Cow::Borrowed("+"),
            TokenType::Slash => Cow::Borrowed("/"),
            TokenType::Star => Cow::Borrowed("*"),
            TokenType::Percent => Cow::Borrowed("%"),
            TokenType::Bang => Cow::Borrowed("!"),
            TokenType::BangEqual => Cow::Borrowed("!="),
            TokenType::Equal => Cow::Borrowed("="),
            TokenType::EqualEqual => Cow::Borrowed("=="),
            TokenType::RightArrow => Cow::Borrowed("=>"),
            TokenType::Greater => Cow::Borrowed(">"),
            TokenType::GreaterEqual => Cow::Borrowed(">="),
            TokenType::Less => Cow::Borrowed("<"),
            TokenType::LessEqual => Cow::Borrowed("<="),
            TokenType::Class => Cow::Borrowed("class"),
            TokenType::Fn => Cow::Borrowed("fn"),
            TokenType::Break => Cow::Borrowed("break"),
            TokenType::Continue => Cow::Borrowed("continue"),
            TokenType::And => Cow::Borrowed("&&"),
            TokenType::Or => Cow::Borrowed("||"),
            TokenType::BitwiseAnd => Cow::Borrowed("&"),
            TokenType::BitwiseOr => Cow::Borrowed("|"),
            TokenType::BitwiseXor => Cow::Borrowed("^"),
            TokenType::BitwiseComplement => Cow::Borrowed("~"),
            TokenType::Underscore => Cow::Borrowed("_"),
            TokenType::Else => Cow::Borrowed("else"),
            TokenType::If => Cow::Borrowed("if"),
            TokenType::False => Cow::Borrowed("false"),
            TokenType::True => Cow::Borrowed("true"),
            TokenType::For => Cow::Borrowed("for"),
            TokenType::Let => Cow::Borrowed("let"),
            TokenType::Mut => Cow::Borrowed("mut"),
            TokenType::While => Cow::Borrowed("while"),
            TokenType::Return => Cow::Borrowed("return"),
            TokenType::SelfLowercase => Cow::Borrowed("self"),
            TokenType::SelfCapitalized => Cow::Borrowed("Self"),
            TokenType::In => Cow::Borrowed("in"),
            TokenType::Native => Cow::Borrowed("native"),
            TokenType::Enum => Cow::Borrowed("enum"),
            TokenType::Impl => Cow::Borrowed("impl"),
            TokenType::Match => Cow::Borrowed("match"),
            TokenType::Pub => Cow::Borrowed("pub"),
            TokenType::Static => Cow::Borrowed("static"),
            TokenType::Type => Cow::Borrowed("type"),
            TokenType::Trait => Cow::Borrowed("trait"),
            TokenType::Use => Cow::Borrowed("use"),
            TokenType::None => Cow::Borrowed("None"),
            TokenType::Ref => Cow::Borrowed("ref"),
        }
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
            TokenType::Int(i) => write!(f, "signed integer"),
            TokenType::UInt(i) => write!(f, "unsigned integer"),
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
            TokenType::Ref => write!(f, "inline"),
            TokenType::Let => write!(f, "let"),
            TokenType::Mut => write!(f, "mut"),
            TokenType::In => write!(f, "in"),
            TokenType::BitwiseAnd => write!(f, "&"),
            TokenType::BitwiseXor => write!(f, "^"),
            TokenType::BitwiseComplement => write!(f, "~"),
            TokenType::Underscore => write!(f, "_"),
        }
    }
}

impl Token {
    pub fn new(token_type: TokenType, start: usize, end: usize) -> Self {
        Self {
            token_type,
            span: Span::new(start as u32, end as u32),
        }
    }
}
