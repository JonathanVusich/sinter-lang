extern crate unicode_segmentation;

use unicode_segmentation::UnicodeSegmentation;

use crate::token::{Token, TokenType};
use crate::token::TokenType::{TokenEof, TokenError, TokenLeftParen, TokenRightParen, TokenLeftBrace, TokenRightBrace, TokenSemicolon, TokenComma, TokenDot, TokenMinus, TokenPlus, TokenSlash, TokenStar, TokenBangEqual, TokenBang, TokenEqualEqual, TokenEqual, TokenLessEqual, TokenLess, TokenGreater, TokenGreaterEqual, TokenString, TokenDouble, TokenLong};
use crate::number::NumberType::{Long, Double};


pub(crate) struct Scanner<'a> {
    code: Vec<&'a str>,
    start: usize,
    current: usize,
    line: i32
}

impl Scanner {

    pub (crate) fn new(string: &str) -> Scanner {
        return Scanner {
            code: UnicodeSegmentation::graphemes(string, true).collect::<Vec<&str>>(),
            start: 0,
            current: 0,
            line: 1
        }
    }

    pub (crate) fn scan_token(&mut self) -> Token {
        self.start = self.current;
        if self.is_at_end() {
            return self.make_token(TokenEof);
        }

        let char = self.advance();

        if self.is_digit(char) {
            return self.number();
        }

        match char {
            "(" => return self.make_token(TokenLeftParen),
            ")" => return self.make_token(TokenRightParen),
            "{" => return self.make_token(TokenLeftBrace),
            "}" => return self.make_token(TokenRightBrace),
            ";" => return self.make_token(TokenSemicolon),
            "," => return self.make_token(TokenComma),
            "." => return self.make_token(TokenDot),
            "-" => return self.make_token(TokenMinus),
            "+" => return self.make_token(TokenPlus),
            "/" => return self.make_token(TokenSlash),
            "*" => return self.make_token(TokenStar),

            "!" => {
                return if self.next_match("=") {
                    self.make_token(TokenBangEqual)
                } else {
                    self.make_token(TokenBang)
                }
            },

            "=" => {
                return if self.next_match("=") {
                    self.make_token(TokenEqualEqual)
                } else {
                    self.make_token(TokenEqual)
                }
            },

            "<" => {
                return if self.next_match("=") {
                    self.make_token(TokenLessEqual)
                } else {
                    self.make_token(TokenLess)
                }
            },

            ">" => {
                return if self.next_match("=") {
                    self.make_token(TokenGreaterEqual)
                } else {
                    self.make_token(TokenGreater)
                }
            }

            "\"" => return self.string(),

            _ => {}
        }



        return self.make_token(TokenError);
    }

    fn advance(&mut self) -> &str {
        self.current += 1;
        return code[self.current - 1];
    }

    fn string(&mut self) -> Token {
        while self.peek() != "\"" && !self.is_at_end() {
            if self.peek() == "\n" {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            return self.error_token("Unterminated string.");
        }

        self.advance();
        return self.make_token(TokenString);
    }

    fn number(&mut self) -> Token {
        let mut number_type = Long;
        while Scanner::is_digit(self.peek()) {
            self.advance();
        }
        if self.peek() == "." {
            let next_is_digit: bool = match self.peek_next() {
                Some(val) => Scanner::is_digit(val),
                _ => false
            };
            if next_is_digit {
                number_type = Double;
                self.advance();

                while Scanner::is_digit(self.peek()) {
                    self.advance();
                }

                return self.make_token(TokenDouble);
            }
        }

        return self.make_token(TokenLong);
    }

    fn is_digit(char: &str) -> bool {
        return match char {
            "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" => true,
            _ => false
        }
    }

    fn make_token(&self, token_type: TokenType) -> Token {
        return Token::new(
            token_type,
            self.code[self.start..self.current].join(""),
            self.line
        )
    }

    fn next_match(&mut self, expected: &str) -> bool {
        if self.is_at_end() {
            return false;
        }
        if self.peek() != expected {
            return false;
        }
        self.current += 1;
        return true;
    }

    fn skip_whitespace(&mut self) {
        loop {
            let char = self.peek();
            match char {
                " " | "\r" | "\t" => self.advance(),
                "\n" => {
                    self.line += 1;
                    self.advance();
                }
                "/" => {
                    match self.peek_next() {
                        Some("/") => {
                            while self.peek() != "\n" && !self.is_at_end() {
                                self.advance();
                            }
                        }
                        _ => {}
                    }
                }
                _ => return
            }
        }
    }

    fn peek(&mut self) -> &str {
        return self.code[self.current];
    }

    fn peek_next(&mut self) -> Option<&str> {
        return if self.is_at_end() {
            Option::None
        } else {
            Option::Some(self.code[self.current + 1])
        }
    }

    fn is_at_end(&self) -> bool {
        return self.current < self.code.len();
    }
}