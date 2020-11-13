extern crate unicode_segmentation;

use unicode_segmentation::UnicodeSegmentation;

use crate::token::{Token, TokenType};
use crate::token::TokenType::{TokenEof, TokenError, TokenLeftParen, TokenRightParen, TokenLeftBrace, TokenRightBrace, TokenSemicolon, TokenComma, TokenDot, TokenMinus, TokenPlus, TokenSlash, TokenStar, TokenBangEqual, TokenBang, TokenEqualEqual, TokenEqual, TokenLessEqual, TokenLess, TokenGreater, TokenGreaterEqual, TokenString, TokenDouble, TokenLong, TokenIdentifier, TokenAnd, TokenClass, TokenElse, TokenIf, TokenOr, TokenReturn, TokenVal, TokenVar, TokenWhile, TokenFalse, TokenFor, TokenFn, TokenThis, TokenTrue};
use crate::number::NumberType::{Long, Double};

static CLASS_KEYWORD: &str = "class";
static AND_KEYWORD: &str = "and";
static ELSE_KEYWORD: &str = "else";
static FALSE_KEYWORD: &str = "false";
static FOR_KEYWORD: &str = "for";
static FUNCTION_KEYWORD: &str = "fn";
static IF_KEYWORD: &str = "if";
static OR_KEYWORD: &str = "or";
static RETURN_KEYWORD: &str = "return";
static THIS_KEYWORD: &str = "this";
static TRUE_KEYWORD: &str = "true";
static VAR_KEYWORD: &str = "var";
static VAL_KEYWORD: &str = "val";
static WHILE_KEYWORD: &str = "while";

#[derive(Debug)]
pub(crate) struct Scanner<'a> {
    code: Vec<&'a str>,
    start: usize,
    current: usize,
    line: i32
}

impl Scanner<'_> {

    pub (crate) fn new(string: &str) -> Scanner {
        return Scanner {
            code: UnicodeSegmentation::graphemes(string, true).collect::<Vec<&str>>(),
            start: 0,
            current: 0,
            line: 1
        }
    }

    pub (crate) fn scan_token(&mut self) -> Token {
        self.skip_whitespace();

        self.start = self.current;

        if self.is_at_end() {
            return self.make_token(TokenEof);
        }

        let char = self.advance();

        if Scanner::is_alpha(char) {
            return self.identifier();
        }
        if Scanner::is_digit(char) {
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
        return self.code[self.current - 1];
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

    fn identifier(&mut self) -> Token {
        while Scanner::is_alpha(self.peek()) || Scanner::is_digit(self.peek()) {
            self.advance();
        }

        return self.make_token(self.identifier_type());
    }

    fn identifier_type(&self) -> TokenType {
        return match self.code[self.start] {
            "a" => self.check_keyword(1, 2, "nd", TokenAnd),
            "c" => self.check_keyword(1, 4, "lass", TokenClass),
            "e" => self.check_keyword(1, 3, "lse", TokenElse),
            "f" => {
                if self.current - self.start > 1 {
                    match self.code[self.start + 1] {
                        "a" => self.check_keyword(2, 3, "lse", TokenFalse),
                        "o" => self.check_keyword(2, 1, "r", TokenFor),
                        "n" => TokenFn,
                        _ => TokenIdentifier
                    }
                } else {
                    TokenIdentifier
                }
            }
            "i" => self.check_keyword(1, 1, "f", TokenIf),
            "o" => self.check_keyword(1, 1, "r", TokenOr),
            "r" => self.check_keyword(1, 5, "eturn", TokenReturn),
            "t" => {
                if self.current - self.start > 1 {
                    match self.code[self.start + 1] {
                        "h" => self.check_keyword(2, 2, "is", TokenThis),
                        "r" => self.check_keyword(2, 2, "ue", TokenTrue),
                        _ => TokenIdentifier
                    }
                } else {
                    TokenIdentifier
                }
            }
            "v" => {
                if self.current - self.start > 2 {
                    match self.code[self.start + 2] {
                        "l" => TokenVal,
                        "r" => TokenVar,
                        _ => TokenIdentifier
                    }
                } else {
                    TokenIdentifier
                }
            },
            "w" => self.check_keyword(1, 4, "hile", TokenWhile),
            _ => TokenIdentifier
        }
    }

    fn check_keyword(&self, start: usize, length: usize, remaining: &str, token_type: TokenType) -> TokenType {
        if self.current - self.start == start + length && self.code[self.start + start..self.start + start + length].join("") == remaining {
            return token_type;
        }
        return TokenIdentifier;
    }

    fn is_digit(char: &str) -> bool {
        return match char {
            "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" => true,
            _ => false
        }
    }

    fn is_alpha(char: &str) -> bool {
        return match char {
            "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" |
            "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" |
            "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z" |
            "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" |
            "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" |
            "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" | "_" => {
                true
            },
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

    fn error_token(&self, error_message: &str) -> Token {
        return Token::new(TokenError, String::from(error_message), self.line);
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
            if self.is_at_end() {
                return;
            }
            let char = self.peek();
            match char {
                " " | "\r" | "\t" => {
                    self.advance();
                },
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
                        _ => break
                    }
                }
                _ => break
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
        return self.current >= self.code.len() - 1;
    }
}


// Tests
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_scanner() {
        let graphemes = UnicodeSegmentation::graphemes("val x = 12.45;", true).collect::<Vec<&str>>();

        let scanner = Scanner::new("val x = 12.45;");

        let all_tokens = scan_all_tokens(scanner);

        assert_eq!(vec![
            Token::new(TokenVal, "val".to_owned(), 1),
            Token::new(TokenIdentifier, "x".to_owned(), 1),
            Token::new(TokenEqual, "=".to_owned(), 1),
            Token::new(TokenDouble, "12.45".to_owned(), 1),
            Token::new(TokenEof, "".to_owned(), 1)
        ], all_tokens);
    }

    fn scan_all_tokens(mut scanner: Scanner) -> Vec<Token> {
        let mut tokens: Vec<Token> = vec![];
        let mut current_token = scanner.scan_token();
        while current_token.token_type != TokenEof {
            tokens.push(current_token);
            current_token = scanner.scan_token();
        }
        tokens.push(current_token);
        return tokens;
    }
}