pub(crate) struct Token<'a> {
    token_type: TokenType,
    token: &'a str,
    line: i32
}

impl Token {

    pub (crate) fn new(token_type: TokenType, token: &str, line: i32) -> Token {
        return Token {
            token_type,
            token,
            line
        }
    }
}


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

    // Generics tokens
    TokenGenericStart,
    TokenGenericEnd,

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
    TokenFun,
    TokenIf,
    TokenOr,
    TokenReturn,
    TokenSuper,
    TokenThis,
    TokenTrue,
    TokenVar,
    TokenVal,
    TokenWhile,

    TokenError,
    TokenEof
}

