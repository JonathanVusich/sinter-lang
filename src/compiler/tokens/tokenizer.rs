use std::concat;
use std::error::Error;
use std::fs;
use std::fs::File;
use std::io;
use std::path::Path;

use crate::compiler::tokens::token::{Token, TokenType};
use crate::compiler::tokens::tokenized_file::TokenizedInput;
use anyhow::Result;
use unicode_segmentation::UnicodeSegmentation;

pub fn read_tokens(path: &Path) -> Result<TokenizedInput> {
    let source_file = fs::read_to_string(path)?;

    let tokenizer = Tokenizer::new(&source_file);
    Ok(tokenizer.into())
}

#[derive(Debug)]
struct Tokenizer<'this> {
    source_chars: Vec<&'this str>,
    tokenized_file: TokenizedInput,
    start: usize,
    current: usize,
}

impl<'this> Tokenizer<'this> {
    pub fn new(source: &'this str) -> Self {
        let source_chars = source.graphemes(true).collect::<Vec<&'this str>>();
        Self {
            source_chars,
            tokenized_file: TokenizedInput::new(),
            start: 0,
            current: 0,
        }
    }

    pub fn into(mut self) -> TokenizedInput {
        while !self.is_at_end() {
            self.skip_whitespace();
            self.scan_token();
        }

        self.tokenized_file
    }

    fn scan_token(&mut self) {
        if self.is_at_end() {
            return;
        }
        let char = self.advance();

        match char {
            "(" => self.create_token(TokenType::LeftParentheses),
            ")" => self.create_token(TokenType::RightParentheses),
            "{" => self.create_token(TokenType::LeftBrace),
            "}" => self.create_token(TokenType::RightBrace),
            "[" => self.create_token(TokenType::LeftBracket),
            "]" => self.create_token(TokenType::RightBracket),
            ":" => self.create_token(TokenType::Colon),
            ";" => self.create_token(TokenType::Semicolon),
            "," => self.create_token(TokenType::Comma),
            "." => self.create_token(TokenType::Dot),
            "-" => {
                if self.matcher(is_digit) {
                    self.parse_num()
                } else {
                    self.create_token(TokenType::Minus)
                }
            }
            "+" => self.create_token(TokenType::Plus),
            "/" => self.create_token(TokenType::Slash),
            "|" => self.create_token(TokenType::Pipe),
            "*" => self.create_token(TokenType::Star),
            "!" => {
                if self.matches("=") {
                    self.create_token(TokenType::BangEqual)
                } else {
                    self.create_token(TokenType::Bang)
                }
            }
            "=" => {
                if self.matches("=") {
                    self.create_token(TokenType::EqualEqual)
                } else if self.matches(">") {
                    self.create_token(TokenType::RightArrow)
                } else {
                    self.create_token(TokenType::Equal)
                }
            }
            "<" => {
                if self.matches("=") {
                    self.create_token(TokenType::LessEqual)
                } else {
                    self.create_token(TokenType::Less)
                }
            }
            ">" => {
                if self.matches("=") {
                    self.create_token(TokenType::GreaterEqual)
                } else {
                    self.create_token(TokenType::Greater)
                }
            }
            "\"" => self.parse_string(),
            "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" => self.parse_num(),
            _ => self.parse_identifier(char),
        }
    }

    fn parse_identifier(&mut self, character: &str) {
        let token_type = match character {
            "a" => self.check_keyword(1, "nd", TokenType::And),
            "b" => self.check_keyword(1, "reak", TokenType::Break),
            "c" => match self.peek() {
                "l" => self.check_keyword(2, "ass", TokenType::Class),
                "o" => self.check_keyword(2, "ntinue", TokenType::Continue),
                _ => None,
            },
            "e" => match self.peek() {
                "l" => self.check_keyword(2, "se", TokenType::Else),
                "n" => self.check_keyword(2, "um", TokenType::Enum),
                _ => None,
            },
            "f" => match self.peek() {
                "a" => self.check_keyword(2, "lse", TokenType::False),
                "o" => self.check_keyword(2, "r", TokenType::For),
                "n" => {
                    self.advance();
                    Some(TokenType::Fn)
                }
                _ => None,
            },
            "i" => match self.peek() {
                "f" => Some(TokenType::If),
                "m" => self.check_keyword(2, "pl", TokenType::Impl),
                "n" => match self.peek_next() {
                    Some(x) if x == "l" => self.check_keyword(3, "ine", TokenType::Inline),
                    _ => Some(TokenType::In),
                },
                _ => None,
            },
            "o" => self.check_keyword(1, "r", TokenType::Or),
            "n" => self.check_keyword(1, "ative", TokenType::Native),
            "N" => self.check_keyword(1, "one", TokenType::None),
            "m" => self.check_keyword(1, "atch", TokenType::Match),
            "p" => self.check_keyword(1, "ub", TokenType::Pub),
            "r" => self.check_keyword(1, "eturn", TokenType::Return),
            "s" => match self.peek() {
                "e" => self.check_keyword(2, "lf", TokenType::SelfLowercase),
                "t" => self.check_keyword(2, "atic", TokenType::Static),
                _ => None,
            },
            "S" => self.check_keyword(1, "elf", TokenType::SelfCapitalized),
            "t" => match self.peek() {
                "y" => self.check_keyword(2, "pe", TokenType::Type),
                "r" => self.check_keyword(2, "ait", TokenType::Trait),
                _ => None,
            },
            "u" => self.check_keyword(1, "se", TokenType::Use),
            "w" => self.check_keyword(1, "hile", TokenType::While),
            "v" => match self.peek() {
                "a" => match self.peek_next() {
                    Some(letter) => match letter {
                        "r" => {
                            self.advance();
                            self.advance();
                            Some(TokenType::Var)
                        }
                        "l" => {
                            self.advance();
                            self.advance();
                            Some(TokenType::Val)
                        }
                        _ => None,
                    },
                    None => None,
                },
                _ => None,
            },
            _ => None,
        }
        .unwrap_or_else(|| {
            while !self.is_at_end() && self.is_valid_identifier() {
                self.advance();
            }

            let identifier = self.source_chars[self.start..self.current].join("");
            let static_ident = Box::leak(identifier.into_boxed_str());
            TokenType::Identifier(static_ident)
        });

        self.create_token(token_type);
    }

    fn check_keyword(
        &mut self,
        start: usize,
        remainder: &'static str,
        token_type: TokenType,
    ) -> Option<TokenType> {
        let end = self.start + start + remainder.len();
        if end > self.source_chars.len() {
            return None;
        }

        let next_chars = self.source_chars[self.start + start..end].join("");
        if next_chars == remainder
            && (end == self.source_chars.len()
                || end < self.source_chars.len() && is_delimiter(self.source_chars[end]))
        {
            self.current = end;
            return Some(token_type);
        }
        None
    }

    fn parse_num(&mut self) {
        while !self.is_at_end() && is_digit(self.peek()) {
            self.advance();
        }

        if !self.is_at_end() && self.peek() == "." && is_digit(self.peek_next().unwrap_or("")) {
            self.advance();

            while !self.is_at_end() && is_digit(self.peek()) {
                self.advance();
            }

            let token_type: TokenType = self.source_chars[self.start..self.current]
                .join("")
                .parse::<f64>()
                .map(TokenType::Float)
                .unwrap_or(TokenType::Unrecognized("Invalid float."));

            self.create_token(token_type);
            return;
        }

        let token_type: TokenType = self.source_chars[self.start..self.current]
            .join("")
            .parse::<i64>()
            .map(TokenType::SignedInteger)
            .unwrap_or(TokenType::Unrecognized("Invalid integer."));

        self.create_token(token_type);
    }

    fn parse_string(&mut self) {
        while !self.is_at_end() && self.peek() != "\"" {
            if self.peek() == "\n" {
                self.tokenized_file.add_line_break(self.current);
            }
            self.advance();
        }

        if self.is_at_end() {
            self.create_unrecognized_token("Unterminated string.");
            return;
        }

        self.advance();

        let string = self.source_chars[self.start + 1..self.current - 1].join("");
        let static_string: &'static str = Box::leak(string.into_boxed_str());

        self.create_token(TokenType::String(static_string));
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
                }
                "\n" => {
                    self.tokenized_file.add_line_break(self.current);
                    self.advance();
                }
                "/" => {
                    if let Some("/") = self.peek_next() {
                        while !self.is_at_end() && self.peek() != "\n" {
                            self.advance();
                        }
                    }
                }
                _ => break,
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
        if self.current >= self.source_chars.len() - 1 {
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

    fn matcher<T: FnOnce(&str) -> bool>(&mut self, func: T) -> bool {
        if self.is_at_end() {
            return false;
        }
        let result = func(self.source_chars[self.current]);
        if result {
            self.current += 1;
        }
        result
    }

    fn is_valid_identifier(&self) -> bool {
        !matches!(
            self.source_chars[self.current],
            "\r" | "\t"
                | "\n"
                | " "
                | "~"
                | "`"
                | "!"
                | "@"
                | "#"
                | "$"
                | "%"
                | "^"
                | "&"
                | "*"
                | "("
                | ")"
                | "-"
                | "+"
                | "="
                | "["
                | "]"
                | "{"
                | "}"
                | "\\"
                | "|"
                | ";"
                | ":"
                | "'"
                | "\""
                | "<"
                | ">"
                | ","
                | "."
                | "?"
                | "/"
        )
    }

    fn create_unrecognized_token(&mut self, error_message: &'static str) {
        self.create_token(TokenType::Unrecognized(error_message));
    }

    fn create_token(&mut self, token_type: TokenType) {
        let token = Token::new(token_type, self.start, self.current - 1);
        self.start = self.current;
        self.tokenized_file.add_token(token);
    }
}

fn is_digit(word: &str) -> bool {
    matches!(
        word,
        "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
    )
}

fn is_delimiter(char: &str) -> bool {
    match char {
        " " | "\r" | "\t" | "\n" | ";" => true,
        _ => false,
    }
}

mod tests {

    use crate::compiler::tokens::token::{Token, TokenType};
    use crate::compiler::tokens::tokenizer::Tokenizer;

    #[allow(unused_macros)]
    macro_rules! make_token {
        ($token_ident:expr, $start:tt, $end:tt) => {
            Token::new($token_ident, $start, $end)
        };
    }

    #[allow(unused_macros)]
    macro_rules! tokenize {
        ($($token_ident:expr, $start:literal, $end:literal),*) => {
            vec![
                $(
                    make_token!($token_ident, $start, $end),
                )*
            ]
        }
    }

    #[allow(unused_macros)]
    macro_rules! test {
        ($tokens:expr, $matching_text:literal) => {
            assert_eq!($tokens, Tokenizer::new($matching_text).into().tokens());
        };
        ($tokens:expr, $matching_text:expr) => {
            assert_eq!($tokens, Tokenizer::new($matching_text).into().tokens());
        };
    }

    #[test]
    pub fn token_generation() {
        test!(
            tokenize!(
                TokenType::Pub,
                0, 2,
                TokenType::Class,
                4, 8,
                TokenType::Identifier("Random"),
                10, 15,
                TokenType::LeftBrace,
                17, 17,
                TokenType::RightBrace,
                18, 18
            ),
            "pub class Random {}"
        );

        test!(
            tokenize!(
                TokenType::Impl,
                0, 3,
                TokenType::Enum,
                5, 8,
                TokenType::Identifier("Reader"),
                12, 17,
                TokenType::LeftBracket,
                21, 21,
                TokenType::RightBracket,
                23, 23
            ),
            "impl enum \n Reader \n [ ]"
        );

        test!(
            tokenize!(
                TokenType::Native,
                0, 5,
                TokenType::Identifier("nativer"),
                7, 13,
                TokenType::Identifier("enative"),
                15, 21
            ),
            "native nativer enative"
        );

        test!(
            tokenize!(
                TokenType::Trait,
                0, 4,
                TokenType::Identifier("Serializable"),
                6, 17,
                TokenType::LeftBrace,
                19, 19,
                TokenType::RightBrace,
                21, 21
            ),
            "trait Serializable { }"
        );

        test!(
            tokenize!(
                TokenType::Trait,
                0, 4,
                TokenType::Identifier("Iterator"),
                6, 13,
                TokenType::Less,
                14, 14,
                TokenType::Identifier("T"),
                15, 15,
                TokenType::Greater,
                16, 16,
                TokenType::LeftBrace,
                18, 18,
                TokenType::Fn,
                20, 21,
                TokenType::Identifier("next"),
                23, 26,
                TokenType::LeftParentheses,
                27, 27,
                TokenType::RightParentheses,
                28, 28,
                TokenType::RightArrow,
                30, 31,
                TokenType::Identifier("T"),
                33, 33,
                TokenType::Pipe,
                35, 35,
                TokenType::None,
                37, 40,
                TokenType::Semicolon,
                41, 41,
                TokenType::RightBrace,
                43, 43
            ),
            "trait Iterator<T> { \
                 fn next() => T | None; \
             }"
        );

        test!(
            tokenize!(
                TokenType::Class,
                0, 4,
                TokenType::Identifier("Point"),
                6, 10,
                TokenType::Less,
                11, 11,
                TokenType::Identifier("T"),
                12, 12,
                TokenType::Comma,
                13, 13,
                TokenType::Identifier("U"),
                15, 15,
                TokenType::Greater,
                16, 16,
                TokenType::LeftParentheses,
                17, 17,
                TokenType::Identifier("x"),
                18, 18,
                TokenType::Colon,
                19, 19,
                TokenType::Identifier("T"),
                21, 21,
                TokenType::Comma,
                22, 22,
                TokenType::Identifier("y"),
                24, 24,
                TokenType::Colon,
                25, 25,
                TokenType::Identifier("U"),
                27, 27,
                TokenType::RightParentheses,
                28, 28,
                TokenType::Semicolon,
                29, 29
            ),
            "class Point<T, U>(x: T, y: U);"
        );

        test!(
            tokenize!(
                TokenType::Fn,
                0, 1,
                TokenType::Identifier("main"),
                3, 6,
                TokenType::LeftParentheses,
                7, 7,
                TokenType::Identifier("arguments"),
                8, 16,
                TokenType::Colon,
                17, 17,
                TokenType::LeftBracket,
                19, 19,
                TokenType::Identifier("str"),
                20, 22,
                TokenType::RightBracket,
                23, 23,
                TokenType::RightParentheses,
                24, 24,
                TokenType::LeftBrace,
                26, 26,
                TokenType::Identifier("print"),
                28, 32,
                TokenType::LeftParentheses,
                33, 33,
                TokenType::Identifier("arguments"),
                34, 42,
                TokenType::Dot,
                43, 43,
                TokenType::Identifier("to_string"),
                44, 52,
                TokenType::LeftParentheses,
                53, 53,
                TokenType::RightParentheses,
                54, 54,
                TokenType::RightParentheses,
                55, 55,
                TokenType::Semicolon,
                56, 56,
                TokenType::RightBrace,
                58, 58
            ),
            "fn main(arguments: [str]) { \
                 print(arguments.to_string()); \
             }"
        );

        test!(
            tokenize!(
                TokenType::Val,
                0, 2,
                TokenType::Identifier("greeting"),
                4, 11,
                TokenType::Equal,
                13, 13,
                TokenType::String("Hello world!"),
                15, 28,
                TokenType::Semicolon,
                29, 29,
                TokenType::Val,
                57, 59,
                TokenType::Identifier("bytearray"),
                61, 69,
                TokenType::Colon,
                70, 70,
                TokenType::LeftBracket,
                72, 72,
                TokenType::Identifier("u8"),
                73, 74,
                TokenType::RightBracket,
                75, 75,
                TokenType::Equal,
                77, 77,
                TokenType::LeftBracket,
                79, 79,
                TokenType::SignedInteger(72),
                80, 81,
                TokenType::Comma,
                82, 82,
                TokenType::SignedInteger(101),
                84, 86,
                TokenType::Comma,
                87, 87,
                TokenType::SignedInteger(108),
                89, 91,
                TokenType::Comma,
                92, 92,
                TokenType::SignedInteger(108),
                94, 96,
                TokenType::Comma,
                97, 97,
                TokenType::SignedInteger(111),
                99, 101,
                TokenType::Comma,
                102, 102,
                TokenType::SignedInteger(32),
                104, 105,
                TokenType::Comma,
                106, 106,
                TokenType::SignedInteger(119),
                108, 110,
                TokenType::Comma,
                111, 111,
                TokenType::SignedInteger(111),
                113, 115,
                TokenType::Comma,
                116, 116,
                TokenType::SignedInteger(114),
                118, 120,
                TokenType::Comma,
                121, 121,
                TokenType::SignedInteger(108),
                123, 125,
                TokenType::Comma,
                126, 126,
                TokenType::SignedInteger(100),
                128, 130,
                TokenType::Comma,
                131, 131,
                TokenType::SignedInteger(33),
                133, 134,
                TokenType::RightBracket,
                135, 135,
                TokenType::Semicolon,
                136, 136,
                TokenType::Val,
                138, 140,
                TokenType::Identifier("greeting_from_array"),
                142, 160,
                TokenType::Equal,
                162, 162,
                TokenType::Identifier("str"),
                164, 166,
                TokenType::LeftParentheses,
                167, 167,
                TokenType::Identifier("bytearray"),
                168, 176,
                TokenType::RightParentheses,
                177, 177,
                TokenType::Semicolon,
                178, 178
            ),
            concat!(
                r#"val greeting = "Hello world!"; // 'str' type is inferred"#,
                "\n",
                r#"val bytearray: [u8] = [72, 101, 108, 108, 111, 32, 119, 111, 114, 108, 100, 33];"#,
                "\n",
                r#"val greeting_from_array = str(bytearray); // "Hello world!""#
            )
        );

        test!(tokenize!(TokenType::String(""), 0, 1), r#""""#);

        test!(tokenize!(TokenType::String("a"), 0, 2), r#""a""#);

        test!(
            tokenize!(
                TokenType::Trait,
                0, 4,
                TokenType::Identifier("Node"),
                6, 9,
                TokenType::LeftBrace,
                11, 11,
                TokenType::Fn,
                16, 17,
                TokenType::Identifier("bounds"),
                19, 24,
                TokenType::LeftParentheses,
                25, 25,
                TokenType::RightParentheses,
                26, 26,
                TokenType::RightArrow,
                28, 29,
                TokenType::Identifier("Bounds"),
                31, 36,
                TokenType::Semicolon,
                37, 37,
                TokenType::Fn,
                42, 43,
                TokenType::Identifier("draw"),
                45, 48,
                TokenType::LeftParentheses,
                49, 49,
                TokenType::Identifier("Graphics"),
                50, 57,
                TokenType::Identifier("g"),
                59, 59,
                TokenType::RightParentheses,
                60, 60,
                TokenType::Semicolon,
                61, 61,
                TokenType::Fn,
                66, 67,
                TokenType::Identifier("children"),
                69, 76,
                TokenType::LeftParentheses,
                77, 77,
                TokenType::RightParentheses,
                78, 78,
                TokenType::RightArrow,
                80, 81,
                TokenType::Identifier("MutableList"),
                83, 93,
                TokenType::Less,
                94, 94,
                TokenType::Identifier("Node"),
                95, 98,
                TokenType::Greater,
                99, 99,
                TokenType::Semicolon,
                100, 100,
                TokenType::RightBrace,
                102, 102,
                TokenType::Fn,
                105, 106,
                TokenType::Identifier("draw_frame"),
                108, 117,
                TokenType::LeftParentheses,
                118, 118,
                TokenType::Identifier("nodes"),
                119, 123,
                TokenType::Colon,
                124, 124,
                TokenType::Identifier("List"),
                126, 129,
                TokenType::Less,
                130, 130,
                TokenType::Identifier("Node"),
                131, 134,
                TokenType::Greater,
                135, 135,
                TokenType::RightParentheses,
                136, 136,
                TokenType::LeftBrace,
                138, 138,
                TokenType::RightBrace,
                140, 140
            ),
            concat!(
                "trait Node {\n",
                "   fn bounds() => Bounds;\n",
                "   fn draw(Graphics g);\n",
                "   fn children() => MutableList<Node>;\n",
                "}\n",
                "\n",
                "fn draw_frame(nodes: List<Node>) { }"
            )
        );

        test!(
            tokenize!(
                TokenType::Class,
                0, 4,
                TokenType::Identifier("SortedMap"),
                6, 14,
                TokenType::Less,
                15, 15,
                TokenType::Identifier("T"),
                16, 16,
                TokenType::Colon,
                17, 17,
                TokenType::Identifier("Sortable"),
                19, 26,
                TokenType::Plus,
                28, 28,
                TokenType::Identifier("Hashable"),
                30, 37,
                TokenType::Greater,
                38, 38,
                TokenType::Semicolon,
                39, 39
            ),
            "class SortedMap<T: Sortable + Hashable>;"
        );

        test!(
            tokenize!(
                TokenType::Enum,
                0, 3,
                TokenType::Identifier("Message"),
                5, 11,
                TokenType::LeftBrace,
                13, 13,
                TokenType::Identifier("Text"),
                19, 22,
                TokenType::LeftParentheses,
                23, 23,
                TokenType::Identifier("message"),
                24, 30,
                TokenType::Colon,
                31, 31,
                TokenType::Identifier("str"),
                33, 35,
                TokenType::RightParentheses,
                36, 36,
                TokenType::Comma,
                37, 37,
                TokenType::Identifier("Photo"),
                43, 47,
                TokenType::LeftParentheses,
                48, 48,
                TokenType::Identifier("caption"),
                49, 55,
                TokenType::Colon,
                56, 56,
                TokenType::Identifier("str"),
                58, 60,
                TokenType::Comma,
                61, 61,
                TokenType::Identifier("photo"),
                63, 67,
                TokenType::Colon,
                68, 68,
                TokenType::Identifier("SerializedPhoto"),
                70, 84,
                TokenType::RightParentheses,
                85, 85,
                TokenType::LeftBrace,
                87, 87,
                TokenType::Fn,
                97, 98,
                TokenType::Identifier("size"),
                100, 103,
                TokenType::LeftParentheses,
                104, 104,
                TokenType::RightParentheses,
                105, 105,
                TokenType::LeftBrace,
                107, 107,
                TokenType::Return,
                121, 126,
                TokenType::Identifier("photo"),
                128, 132,
                TokenType::Dot,
                133, 133,
                TokenType::Identifier("size"),
                134, 137,
                TokenType::LeftParentheses,
                138, 138,
                TokenType::RightParentheses,
                139, 139,
                TokenType::Semicolon,
                140, 140,
                TokenType::RightBrace,
                150, 150,
                TokenType::RightBrace,
                156, 156,
                TokenType::Comma,
                157, 157,
                TokenType::RightBrace,
                159, 159
            ),
            concat!(
                "enum Message {\n",
                "    Text(message: str),\n",
                "    Photo(caption: str, photo: SerializedPhoto) {\n",
                "        fn size() {\n",
                "            return photo.size();\n",
                "        }\n",
                "    },\n",
                "}"
            )
        );
    }

    #[test]
    pub fn float_parsing() {
        test!(tokenize!(TokenType::Float(123.45), 0, 5), "123.45");

        test!(tokenize!(TokenType::Float(123.01), 1, 6), " 123.01");
    }

    #[test]
    pub fn int_parsing() {
        test!(tokenize!(TokenType::SignedInteger(-123), 0, 3), "-123");

        test!(tokenize!(TokenType::SignedInteger(123), 0, 2), "123");

        test!(
            tokenize!(
                TokenType::SignedInteger(123), 0, 2,
                TokenType::Dot, 3, 3
            ), "123."
        );
    }
}
