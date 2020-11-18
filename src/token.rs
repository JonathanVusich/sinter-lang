#[derive(Eq, PartialEq, Debug, Clone)]
pub (crate) struct Token {
    pub (crate) token_type: TokenType,
    pub (crate) token: String,
    pub (crate) line: i32
}

impl Token {

    pub (crate) fn new(token_type: TokenType, token: String, line: i32) -> Token {
        return Token {
            token_type,
            token,
            line
        }
    }

    pub (crate) fn from_str(token_type: TokenType, token: &str, line: i32) -> Token {
        return Token {
            token_type,
            token: token.to_string(),
            line
        }
    }
}

#[derive(Eq, PartialEq, Debug, Copy, Clone)]
pub (crate) enum TokenType {
    // Single-character tokens.
    TokenLeftParen,
    TokenRightParen,
    TokenLeftBrace,
    TokenRightBrace,
    TokenComma,
    TokenDot,
    TokenMinus,
    TokenPlus,
    TokenSemicolon,
    TokenSlash,
    TokenStar,

    // One or two character tokens.
    TokenBang,
    TokenBangEqual,
    TokenEqual,
    TokenEqualEqual,
    TokenGreater,
    TokenGreaterEqual,
    TokenLess,
    TokenLessEqual,

    // Literals.
    TokenIdentifier,
    TokenString,
    TokenLong,
    TokenDouble,

    // Keywords.
    TokenAnd,
    TokenClass,
    TokenElse,
    TokenFalse,
    TokenFor,
    TokenFn,
    TokenIf,
    TokenOr,
    TokenReturn,
    TokenThis,
    TokenTrue,
    TokenVar,
    TokenVal,
    TokenWhile,

    TokenError,
    TokenEof
}

