use std::error::Error;
use std::fs;
use std::fs::File;
use std::io;
use std::io::{BufRead, BufReader, Read};
use std::path::Path;
use std::thread::current;

use anyhow::Result;
use unicode_segmentation::UnicodeSegmentation;

use crate::token::token::{Token, TokenType};

pub fn read_tokens(path: &Path) -> Result<Vec<Token>> {
    let source_file = fs::read_to_string(path)?;

    let tokenizer = Tokenizer::new(&source_file);
    Ok(tokenizer.read_tokens())
}

struct Tokenizer<'this> {
    source_chars: Vec<&'this str>,
    start: usize,
    current: usize,
    line_num: usize,
    line_pos: usize,
}


impl<'this> Tokenizer<'this> {

    pub fn new(source: &'this str) -> Self {
        let source_chars = source.graphemes(true).collect::<Vec<&'this str>>();
        Self {
            source_chars,
            start: 0,
            current: 0,
            line_num: 0,
            line_pos: 0,
        }
    }

    pub fn read_tokens(mut self) -> Vec<Token> {
        let mut tokens: Vec<Token> = Vec::new();

        while !self.is_at_end() {
            self.skip_whitespace();

            let token = self.scan_token();
            tokens.push(token);
        }

        tokens
    }

    fn scan_token(&mut self) -> Token {
        let char = self.advance();

        match char {
            "(" => self.create_token(TokenType::LeftParentheses),
            ")" => self.create_token(TokenType::RightParentheses),
            "{" => self.create_token(TokenType::LeftBrace),
            "}" => self.create_token(TokenType::RightBrace),
            "[" => self.create_token(TokenType::LeftBracket),
            "]" => self.create_token(TokenType::RightBracket),
            ";" => self.create_token(TokenType::Semicolon),
            "," => self.create_token(TokenType::Comma),
            "." => self.create_token(TokenType::Dot),
            "-" => self.create_token(TokenType::Minus),
            "+" => self.create_token(TokenType::Plus),
            "/" => self.create_token(TokenType::Slash),
            "*" => self.create_token(TokenType::Star),
            "!" => {
                if self.matches("=") {
                    self.create_token(TokenType::BangEqual)
                } else {
                    self.create_token(TokenType::Bang)
                }
            },
            "=" => {
                if self.matches("=") {
                    self.create_token(TokenType::EqualEqual)
                } else {
                    self.create_token(TokenType::Equal)
                }
            },
            "<" => {
                if self.matches("=") {
                    self.create_token(TokenType::LessEqual)
                } else {
                    self.create_token(TokenType::Less)
                }
            },
            ">" => {
                if self.matches("=") {
                    self.create_token(TokenType::GreaterEqual)
                } else {
                    self.create_token(TokenType::Greater)
                }
            },
            "\"" => self.parse_string(),
            "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" => self.parse_num(),
            _ => self.parse_identifier(char)
        }
    }

    fn parse_identifier(&mut self, character: &str) -> Token {
        let token_type = match character {
            "a" => self.check_keyword(1, "nd", TokenType::And),
            "b" => self.check_keyword( 1, "reak", TokenType::Break),
            "c" => {
                match self.peek() {
                    "l" => self.check_keyword(2, "ass", TokenType::Class),
                    "o" => self.check_keyword(2, "ntinue", TokenType::Continue),
                    _ => None
                }
            },
            "e" => {
                match self.peek() {
                    "l" => self.check_keyword(2, "se", TokenType::Else),
                    "n" => self.check_keyword(2, "um", TokenType::Enum),
                    _ => None
                }
            },
            "f" => {
                match self.peek() {
                    "a" => self.check_keyword(2, "lse", TokenType::False),
                    "o" => self.check_keyword(2, "r", TokenType::For),
                    "n" => Some(TokenType::Fn),
                    _ => None
                }
            },
            "i" => {
                match self.peek() {
                    "f" => Some(TokenType::If),
                    "m" => self.check_keyword(2, "pl", TokenType::Impl),
                    "n" => {
                        match self.peek_next() {
                            Some(x) if x == "l" => self.check_keyword(3, "ine", TokenType::Inline),
                            _ => Some(TokenType::In)
                        }
                    },
                    _ => None
                }
            },
            "o" => self.check_keyword(1, "r", TokenType::Or),
            "n" => self.check_keyword(1, "ative", TokenType::Native),
            "m" => self.check_keyword(1, "atch", TokenType::Match),
            "p" => self.check_keyword(1, "ub", TokenType::Pub),
            "r" => self.check_keyword(1, "eturn", TokenType::Return),
            "s" => {
                match self.peek() {
                    "e" => self.check_keyword(2, "lf", TokenType::SelfLowercase),
                    "t" => self.check_keyword(2, "atic", TokenType::Static),
                    _ => None
                }
            },
            "S" => self.check_keyword(1, "elf", TokenType::SelfCapitalized),
            "t" => self.check_keyword(1, "ype", TokenType::Type),
            "u" => self.check_keyword(1, "se", TokenType::Use),
            "w" => self.check_keyword(1, "hile", TokenType::While),
            _ => None
        }.unwrap_or_else(|| {
            while !self.is_at_end() && self.is_valid_identifier() {
                self.advance();
            }

            let identifier = self.source_chars[self.start..self.current].join("");
            let static_ident = Box::leak(identifier.into_boxed_str());
            TokenType::Identifier(static_ident)
        });

        self.create_token(token_type)
    }

    fn check_keyword(&mut self, start: usize, remainder: &'static str, token_type: TokenType) -> Option<TokenType> {
        let end = self.start + start + remainder.len();
        if end > self.source_chars.len() {
            return None;
        }
        let next_chars = self.source_chars[self.start + start..end].join("");
        if next_chars == remainder && (end == self.source_chars.len() || end < self.source_chars.len() && self.source_chars[end] == " ") {
            self.current = end;
            return Some(token_type)
        }
        None
    }

    fn parse_num(&mut self) -> Token {
        while is_digit(self.peek()) {
            self.advance();
        }

        if self.peek() == "." && is_digit(self.peek()) {
            self.advance();

            while is_digit(self.peek()) {
                self.advance();
            }

            let token_type: TokenType = self.source_chars[self.start..self.current].join("").parse::<f64>()
                .map(TokenType::Float)
                .unwrap_or(TokenType::Unrecognized("Invalid float."));

            return self.create_token(token_type);
        }

        let token_type: TokenType = self.source_chars[self.start..self.current].join("").parse::<f64>()
            .map(TokenType::Float)
            .unwrap_or(TokenType::Unrecognized("Invalid integer."));

        self.create_token(token_type)
    }

    fn parse_string(&mut self) -> Token {
        while self.peek() != "\"" && !self.is_at_end() {
            if self.peek() == "\n" {
                self.line_num += 1;
                self.line_pos = 0;
            }
            self.advance();
        }

        if self.is_at_end() {
            return self.create_unrecognized_token("Unterminated string.");
        }

        self.advance();

        let string = self.source_chars[self.start..self.current].join("");
        let static_string: &'static str = Box::leak(string.into_boxed_str());

        self.create_token(TokenType::String(static_string))
    }

    fn skip_whitespace(&mut self) {
        loop {
            let char = self.peek();
            match char {
                " " | "\r" | "\t" => {
                    self.advance();
                    self.line_pos += 1;
                }
                "\n" => {
                    self.line_num += 1;
                    self.line_pos = 0;
                    self.advance();
                }
                "/" => {
                    if let Some("/") = self.peek_next() {
                        while self.peek() != "\n" && !self.is_at_end() {
                            self.advance();
                        }
                    }
                }
                _ => {
                    break
                }
            }
        }
        self.start = self.current;
    }

    fn is_at_end(&self) -> bool {
        self.current == self.source_chars.len()
    }

    fn advance(&mut self) -> &'this str {
        self.current += 1;
        self.source_chars[self.current - 1]
    }

    fn peek(&mut self) -> &'this str {
        self.source_chars[self.current]
    }

    fn peek_next(&mut self) -> Option<&'this str> {
        if self.is_at_end() {
            None
        } else {
            Some(self.source_chars[self.current + 1])
        }
    }

    fn matches(&mut self, expected: &str) -> bool {
        if self.is_at_end() {
            return false;
        }
        if expected != self.source_chars[self.current] {
            return false;
        }
        self.current += 1;
        true
    }

    fn is_valid_identifier(&self) -> bool {
        !matches!(self.source_chars[self.current], "\r" | "\t" | "\n" | " " | "~" | "`" | "!" | "@"
            | "#" | "$" | "%" | "^" | "&" | "*" | "(" | ")" | "-" | "+"
            | "=" | "[" | "]" | "{" | "}" | "\\" | "|" | ";" | ":" | "'"
            | "\"" | "<" | ">" | "," | "." | "?" | "/"
        )
    }

    fn create_unrecognized_token(&self, error_message: &'static str) -> Token {
        Token::new(TokenType::Unrecognized(error_message), self.line_num, self.line_pos)
    }

    fn create_token(&mut self, token_type: TokenType) -> Token {
        let token = Token::new(token_type, self.line_num, self.line_pos);
        self.line_pos += self.current - self.start;
        self.start = self.current;
        token
    }
}

fn is_digit(word: &str) -> bool {
    matches!(word, "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9")
}

mod tests {
    use crate::token::token::{Token, TokenType};
    use crate::token::tokenizer::Tokenizer;

    #[test]
    pub fn token_generation() {
        assert_eq!(vec![
            Token::new(TokenType::Pub, 0, 0),
            Token::new(TokenType::Class, 0, 4),
            Token::new(TokenType::Identifier("Random"), 0, 10),
            Token::new(TokenType::LeftBrace, 0, 17),
            Token::new(TokenType::RightBrace, 0, 18),
        ], Tokenizer::new("pub class Random {}").read_tokens());

        assert_eq!(vec![
            Token::new(TokenType::Impl, 0, 0),
            Token::new(TokenType::Enum, 0, 5),
            Token::new(TokenType::Identifier("Reader"), 1, 1),
            Token::new(TokenType::LeftBracket, 2, 1),
            Token::new(TokenType::RightBracket, 2, 3)
        ], Tokenizer::new("impl enum \n Reader \n [ ]").read_tokens());

        assert_eq!(vec![
            Token::new(TokenType::Native, 0, 0),
            Token::new(TokenType::Identifier("nativer"), 0, 7),
            Token::new(TokenType::Identifier("enative"), 0, 15),
        ], Tokenizer::new("native nativer enative").read_tokens());
    }
}