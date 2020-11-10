use crate::token::{Token, TokenType};
use crate::token::TokenType::{TokenEof, TokenError};

pub(crate) struct Scanner<'a> {
    code: &'a str,
    current: usize,
    start: usize,
    line: i32
}

impl Scanner {

    pub (crate) fn new(code: &str) -> Scanner {
        return Scanner {
            code,
            current: 0,
            start: 0,
            line: 1
        }
    }

    pub (crate) fn scan_token(&mut self) -> Token {
        self.start = self.current;
        if self.is_at_end() {
            return self.make_token(TokenEof);
        }

        return self.make_token(TokenError);
    }

    fn make_token(&self, token_type: TokenType) -> Token {
        return Token::new(
            token_type,
            &self.code[self.start..self.current],
            self.line
        )
    }

    fn is_at_end(&self) -> bool {
        return self.current < self.code.len();
    }
}