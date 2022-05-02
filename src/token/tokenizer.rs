use std::error::Error;
use std::fs;
use std::fs::File;
use std::io;
use std::io::{BufRead, BufReader, Read};
use std::path::Path;

use anyhow::Result;
use unicode_segmentation::UnicodeSegmentation;

use crate::token::token::{Token, TokenType};

pub fn read_tokens(path: &Path) -> Result<Vec<Token>> {
    let source_file = fs::read_to_string(path)?;

    let tokenizer = Tokenizer::new();
    Ok(tokenizer.read_tokens(&source_file))
}

struct Tokenizer {
    start: usize,
    current: usize,
    line_num: usize,
    line_pos: usize,
}


impl Tokenizer {

    pub fn new() -> Self {
        Self {
            start: 0,
            current: 0,
            line_num: 0,
            line_pos: 0,
        }
    }

    pub fn read_tokens(mut self, source_file: &str) -> Vec<Token> {
        let mut tokens: Vec<Token> = Vec::new();

        let source_chars = source_file.graphemes(true)
            .collect::<Vec<&str>>();

        while !self.is_at_end(&source_chars) {
            self.skip_whitespace(&source_chars);
            let token = self.scan_token(&source_chars);
            tokens.push(token);
        }

        return tokens;
    }

    fn scan_token(&mut self, source_chars: &[&str]) -> Token {
        let char = self.advance(source_chars);

        let token = match char {
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
                if self.matches("=", source_chars) {
                    self.create_token(TokenType::BangEqual)
                } else {
                    self.create_token(TokenType::Bang)
                }
            },
            "=" => {
                if self.matches("=", source_chars) {
                    self.create_token(TokenType::EqualEqual)
                } else {
                    self.create_token(TokenType::Equal)
                }
            },
            "<" => {
                if self.matches("=", source_chars) {
                    self.create_token(TokenType::LessEqual)
                } else {
                    self.create_token(TokenType::Less)
                }
            },
            ">" => {
                if self.matches("=", source_chars) {
                    self.create_token(TokenType::GreaterEqual)
                } else {
                    self.create_token(TokenType::Greater)
                }
            },
            "\"" => {
                while self.peek(source_chars) != "\"" && !self.is_at_end(source_chars) {
                    if self.peek(source_chars) == "\n" {
                        self.line_num += 1;
                    }
                    self.advance(source_chars);
                }

                if self.is_at_end(source_chars) {
                    return self.create_unrecognized_token("Unterminated string.");
                }

                self.advance(source_chars);

                let string = source_chars[self.start..self.current].join("");
                let static_string: &'static str = Box::leak(string.into_boxed_str());

                self.create_token(TokenType::String(static_string))
            },

            "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" => {
                todo!()
            }

            

            _ => self.create_unrecognized_token("Unrecognized token!")
        };
        
        token
    }

    fn skip_whitespace(&mut self, source_chars: &[&str]) {
        loop {
            let char = self.peek(source_chars);
            match char {
                " " | "\r" | "\t" => {
                    self.advance(source_chars);
                    break;
                }
                "\n" => {
                    self.line_num += 1;
                    self.advance(source_chars);
                    break;
                }
                "/" => {
                    if let Some("/") = self.peek_next(source_chars) {
                        while self.peek(source_chars) != "\n" && !self.is_at_end(source_chars) {
                            self.advance(source_chars);
                        }
                    } else {
                        break;
                    }
                }
                _ => {
                    break
                }
            }
        }
    }

    fn is_at_end(&self, source_chars: &[&str]) -> bool {
        self.current == source_chars.len()
    }

    fn advance<'a>(&mut self, source_chars: &[&'a str]) -> &'a str {
        self.current += 1;
        source_chars[self.current - 1]
    }

    fn peek<'a>(&mut self, source_chars: &[&'a str]) -> &'a str {
        source_chars[self.current]
    }

    fn peek_next<'a>(&mut self, source_chars: &[&'a str]) -> Option<&'a str> {
        if self.is_at_end(source_chars) {
            return None
        } else {
            Some(source_chars[self.current + 1])
        }
    }

    fn matches<'a>(&mut self, expected: &str, source_chars: &[&'a str]) -> bool {
        if self.is_at_end(source_chars) {
            return false;
        }
        if expected != source_chars[self.current] {
            return false;
        }
        self.current += 1;
        true
    }

    fn create_unrecognized_token(&self, error_message: &'static str) -> Token {
        Token::new(TokenType::Unrecognized(error_message), self.line_num, self.line_pos)
    }

    fn create_token(&self, token_type: TokenType) -> Token {
        Token::new(token_type, self.line_num, self.line_pos)
    }
}