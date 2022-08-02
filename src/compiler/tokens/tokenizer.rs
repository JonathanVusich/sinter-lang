use std::borrow::Cow;
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
use crate::compiler::StringInterner;
use crate::compiler::types::types::InternedStr;

pub fn tokenize_file(string_interner: StringInterner, path: &Path) -> Result<TokenizedInput> {
    let source_file = fs::read_to_string(path)?;
    let tokenizer = Tokenizer::new(string_interner.clone(), &source_file);
    Ok(tokenizer.into())
}

pub fn tokenize<T: AsRef<str>>(string_interner: StringInterner, input: T) -> Result<TokenizedInput> {
    let source_file = input.as_ref();
    let tokenizer = Tokenizer::new(string_interner, source_file);
    Ok(tokenizer.into())
}

#[derive(Debug)]
struct Tokenizer<'this> {
    string_interner: StringInterner,
    source_chars: Vec<&'this str>,
    tokenized_file: TokenizedInput,
    start: usize,
    current: usize,
}

impl<'this> Tokenizer<'this> {
    pub fn new(string_interner: StringInterner, source: &'this str) -> Self {
        let source_chars = source.graphemes(true).collect::<Vec<&'this str>>();
        Self {
            string_interner,
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
            "&" => {
                if self.matches("&") {
                    self.create_token(TokenType::And)
                } else {
                    self.create_unrecognized_token("&")
                }
            }
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
            "|" => {
                if self.matches("|") {
                    self.create_token(TokenType::Or)
                } else {
                    self.create_token(TokenType::Pipe)
                }
            }
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
                "n" => self.check_keyword(4, "line", TokenType::Inline),
                _ => None,
            },
            "l" => self.check_keyword(1, "et", TokenType::Let),
            "m" => match self.peek() {
                "a" => self.check_keyword(2, "tch", TokenType::Match),
                "u" => self.check_keyword(2, "t", TokenType::Mut),
                _ => None,
            },
            "n" => self.check_keyword(1, "ative", TokenType::Native),
            "N" => self.check_keyword(1, "one", TokenType::None),
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
            _ => None,
        }
        .unwrap_or_else(|| {
            while !self.is_at_end() && self.is_valid_identifier() {
                self.advance();
            }

            let identifier = self.source_chars[self.start..self.current].join("");
            let interned_str = self.string_interner.get_or_intern(identifier);
            TokenType::Identifier(interned_str)
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
                .unwrap_or(TokenType::Unrecognized(self.string_interner.get_or_intern("Invalid float.")));

            self.create_token(token_type);
            return;
        }

        let token_type: TokenType = self.source_chars[self.start..self.current]
            .join("")
            .parse::<i64>()
            .map(TokenType::SignedInteger)
            .unwrap_or(TokenType::Unrecognized(self.string_interner.get_or_intern("Invalid integer.")));

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
        let interned_str = self.string_interner.get_or_intern(string);

        self.create_token(TokenType::String(interned_str));
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
        let interned_error = self.string_interner.get_or_intern(error_message);
        self.create_token(TokenType::Unrecognized(interned_error));
    }

    fn create_token(&mut self, token_type: TokenType) {
        let token = Token::new(token_type, self.start, self.current);
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
    matches!(char, " " | "\r" | "\t" | "\n" | ";")
}

mod tests {
    use std::fs::File;
    use std::io::{BufReader, BufWriter};
    use std::path::Path;
    use crate::compiler::tokens::token::{Token, TokenType};
    use crate::compiler::tokens::tokenized_file::TokenizedInput;
    use crate::compiler::tokens::tokenizer::{tokenize, Tokenizer};
    use anyhow::Result;
    use cfg_if::cfg_if;
    use crate::compiler::StringInterner;
    cfg_if! {
        if #[cfg(test)] {
            use crate::util::utils::{load, save};
            use crate::util::utils::resolve_test_path;
        }
    }

    #[cfg(test)]
    macro_rules! make_token {
        ($token_ident:expr, $start:tt, $current:tt) => {
            Token::new($token_ident, $start, $current)
        };
    }

    #[cfg(test)]
    macro_rules! tokenize {
        ($($token_ident:expr, $start:literal, $current:literal),*) => {
            vec![
                $(
                    make_token!($token_ident, $start, $current),
                )*
            ]
        }
    }

    #[cfg(test)]
    macro_rules! test {
        ($string_interner:expr, $tokens:expr, $matching_text:literal) => {
            assert_eq!($tokens, Tokenizer::new($string_interner, $matching_text).into().tokens());
        };
        ($string_interner:expr, $tokens:expr, $matching_text:expr) => {
            assert_eq!($tokens, Tokenizer::new($string_interner, $matching_text).into().tokens());
        };
    }

    #[cfg(test)]
    fn compare_tokens(test: &str, code: &str) {
        let string_interner = StringInterner::default();
        let tokenized_file = Tokenizer::new(string_interner.clone(), code).into();
        if let Ok(loaded) = load::<Vec<Token>>("tokenizer", test) {
            assert_eq!(loaded, tokenized_file.tokens());
        } else {
            save("tokenizer", test, tokenized_file.tokens()).expect("Error saving module!");
        }
    }

    #[test]
    pub fn tokenize_strings() {
        let static_str = "pub class Random {}";
        let string = String::from(static_str);

        let string_interner = StringInterner::default();

        let expected = tokenize!(
            TokenType::Pub,
            0,
            3,
            TokenType::Class,
            4,
            9,
            TokenType::Identifier(string_interner.get_or_intern("Random")),
            10,
            16,
            TokenType::LeftBrace,
            17,
            18,
            TokenType::RightBrace,
            18,
            19
        );

        assert_eq!(expected, tokenize(string_interner.clone(), string).unwrap().tokens());
        assert_eq!(expected, tokenize(string_interner.clone(), static_str).unwrap().tokens());
    }

    #[test]
    pub fn token_generation() {
        let string_interner = StringInterner::default();

        test!(
            string_interner.clone(),
            tokenize!(
                TokenType::Pub,
                0,
                3,
                TokenType::Class,
                4,
                9,
                TokenType::Identifier(string_interner.get_or_intern("Random")),
                10,
                16,
                TokenType::LeftBrace,
                17,
                18,
                TokenType::RightBrace,
                18,
                19
            ),
            "pub class Random {}"
        );

        test!(
            string_interner.clone(),
            tokenize!(
                TokenType::Impl,
                0,
                4,
                TokenType::Enum,
                5,
                9,
                TokenType::Identifier(string_interner.get_or_intern("Reader")),
                12,
                18,
                TokenType::LeftBracket,
                21,
                22,
                TokenType::RightBracket,
                23,
                24
            ),
            "impl enum \n Reader \n [ ]"
        );

        test!(
            string_interner.clone(),
            tokenize!(
                TokenType::Native,
                0,
                6,
                TokenType::Identifier(string_interner.get_or_intern("nativer")),
                7,
                14,
                TokenType::Identifier(string_interner.get_or_intern("nativer")),
                15,
                22
            ),
            "native nativer enative"
        );

        test!(
            string_interner.clone(),
            tokenize!(
                TokenType::Trait,
                0,
                5,
                TokenType::Identifier(string_interner.get_or_intern("Serializable")),
                6,
                18,
                TokenType::LeftBrace,
                19,
                20,
                TokenType::RightBrace,
                21,
                22
            ),
            "trait Serializable { }"
        );

        test!(
            string_interner.clone(),
            tokenize!(
                TokenType::Trait,
                0,
                5,
                TokenType::Identifier(string_interner.get_or_intern("Iterator")),
                6,
                14,
                TokenType::Less,
                14,
                15,
                TokenType::Identifier(string_interner.get_or_intern("T")),
                15,
                16,
                TokenType::Greater,
                16,
                17,
                TokenType::LeftBrace,
                18,
                19,
                TokenType::Fn,
                20,
                22,
                TokenType::Identifier(string_interner.get_or_intern("next")),
                23,
                27,
                TokenType::LeftParentheses,
                27,
                28,
                TokenType::RightParentheses,
                28,
                29,
                TokenType::RightArrow,
                30,
                32,
                TokenType::Identifier(string_interner.get_or_intern("T")),
                33,
                34,
                TokenType::Pipe,
                35,
                36,
                TokenType::None,
                37,
                41,
                TokenType::Semicolon,
                41,
                42,
                TokenType::RightBrace,
                43,
                44
            ),
            "trait Iterator<T> { \
                 fn next() => T | None; \
             }"
        );

        test!(
            string_interner.clone(),
            tokenize!(
                TokenType::Class,
                0,
                5,
                TokenType::Identifier(string_interner.get_or_intern("Point")),
                6,
                11,
                TokenType::Less,
                11,
                12,
                TokenType::Identifier(string_interner.get_or_intern("T")),
                12,
                13,
                TokenType::Comma,
                13,
                14,
                TokenType::Identifier(string_interner.get_or_intern("U")),
                15,
                16,
                TokenType::Greater,
                16,
                17,
                TokenType::LeftParentheses,
                17,
                18,
                TokenType::Identifier(string_interner.get_or_intern("x")),
                18,
                19,
                TokenType::Colon,
                19,
                20,
                TokenType::Identifier(string_interner.get_or_intern("T")),
                21,
                22,
                TokenType::Comma,
                22,
                23,
                TokenType::Identifier(string_interner.get_or_intern("y")),
                24,
                25,
                TokenType::Colon,
                25,
                26,
                TokenType::Identifier(string_interner.get_or_intern("U")),
                27,
                28,
                TokenType::RightParentheses,
                28,
                29,
                TokenType::Semicolon,
                29,
                30
            ),
            "class Point<T, U>(x: T, y: U);"
        );

        test!(
            string_interner.clone(),
            tokenize!(
                TokenType::Fn,
                0,
                2,
                TokenType::Identifier(string_interner.get_or_intern("main")),
                3,
                7,
                TokenType::LeftParentheses,
                7,
                8,
                TokenType::Identifier(string_interner.get_or_intern("arguments")),
                8,
                17,
                TokenType::Colon,
                17,
                18,
                TokenType::LeftBracket,
                19,
                20,
                TokenType::Identifier(string_interner.get_or_intern("str")),
                20,
                23,
                TokenType::RightBracket,
                23,
                24,
                TokenType::RightParentheses,
                24,
                25,
                TokenType::LeftBrace,
                26,
                27,
                TokenType::Identifier(string_interner.get_or_intern("print")),
                28,
                33,
                TokenType::LeftParentheses,
                33,
                34,
                TokenType::Identifier(string_interner.get_or_intern("arguments")),
                34,
                43,
                TokenType::Dot,
                43,
                44,
                TokenType::Identifier(string_interner.get_or_intern("to_string")),
                44,
                53,
                TokenType::LeftParentheses,
                53,
                54,
                TokenType::RightParentheses,
                54,
                55,
                TokenType::RightParentheses,
                55,
                56,
                TokenType::Semicolon,
                56,
                57,
                TokenType::RightBrace,
                58,
                59
            ),
            "fn main(arguments: [str]) { \
                 print(arguments.to_string()); \
             }"
        );

        test!(
            string_interner.clone(),
            tokenize!(
                TokenType::Let,
                0,
                3,
                TokenType::Identifier(string_interner.get_or_intern("greeting")),
                4,
                12,
                TokenType::Equal,
                13,
                14,
                TokenType::String(string_interner.get_or_intern("Hello world!")),
                15,
                29,
                TokenType::Semicolon,
                29,
                30,
                TokenType::Let,
                57,
                60,
                TokenType::Identifier(string_interner.get_or_intern("bytearray")),
                61,
                70,
                TokenType::Colon,
                70,
                71,
                TokenType::LeftBracket,
                72,
                73,
                TokenType::Identifier(string_interner.get_or_intern("u8")),
                73,
                75,
                TokenType::RightBracket,
                75,
                76,
                TokenType::Equal,
                77,
                78,
                TokenType::LeftBracket,
                79,
                80,
                TokenType::SignedInteger(72),
                80,
                82,
                TokenType::Comma,
                82,
                83,
                TokenType::SignedInteger(101),
                84,
                87,
                TokenType::Comma,
                87,
                88,
                TokenType::SignedInteger(108),
                89,
                92,
                TokenType::Comma,
                92,
                93,
                TokenType::SignedInteger(108),
                94,
                97,
                TokenType::Comma,
                97,
                98,
                TokenType::SignedInteger(111),
                99,
                102,
                TokenType::Comma,
                102,
                103,
                TokenType::SignedInteger(32),
                104,
                106,
                TokenType::Comma,
                106,
                107,
                TokenType::SignedInteger(119),
                108,
                111,
                TokenType::Comma,
                111,
                112,
                TokenType::SignedInteger(111),
                113,
                116,
                TokenType::Comma,
                116,
                117,
                TokenType::SignedInteger(114),
                118,
                121,
                TokenType::Comma,
                121,
                122,
                TokenType::SignedInteger(108),
                123,
                126,
                TokenType::Comma,
                126,
                127,
                TokenType::SignedInteger(100),
                128,
                131,
                TokenType::Comma,
                131,
                132,
                TokenType::SignedInteger(33),
                133,
                135,
                TokenType::RightBracket,
                135,
                136,
                TokenType::Semicolon,
                136,
                137,
                TokenType::Let,
                138,
                141,
                TokenType::Identifier(string_interner.get_or_intern("greeting_from_array")),
                142,
                161,
                TokenType::Equal,
                162,
                163,
                TokenType::Identifier(string_interner.get_or_intern("str")),
                164,
                167,
                TokenType::LeftParentheses,
                167,
                168,
                TokenType::Identifier(string_interner.get_or_intern("bytearray")),
                168,
                177,
                TokenType::RightParentheses,
                177,
                178,
                TokenType::Semicolon,
                178,
                179
            ),
            concat!(
                r#"let greeting = "Hello world!"; // 'str' type is inferred"#,
                "\n",
                r#"let bytearray: [u8] = [72, 101, 108, 108, 111, 32, 119, 111, 114, 108, 100, 33];"#,
                "\n",
                r#"let greeting_from_array = str(bytearray); // "Hello world!""#
            )
        );

        test!(string_interner.clone(), tokenize!(TokenType::String(string_interner.get_or_intern("")), 0, 2), r#""""#);

        test!(string_interner.clone(), tokenize!(TokenType::String(string_interner.get_or_intern("a")), 0, 3), r#""a""#);

        test!(
            string_interner.clone(),
            tokenize!(
                TokenType::Trait,
                0,
                5,
                TokenType::Identifier(string_interner.get_or_intern("Node")),
                6,
                10,
                TokenType::LeftBrace,
                11,
                12,
                TokenType::Fn,
                16,
                18,
                TokenType::Identifier(string_interner.get_or_intern("bounds")),
                19,
                25,
                TokenType::LeftParentheses,
                25,
                26,
                TokenType::RightParentheses,
                26,
                27,
                TokenType::RightArrow,
                28,
                30,
                TokenType::Identifier(string_interner.get_or_intern("Bounds")),
                31,
                37,
                TokenType::Semicolon,
                37,
                38,
                TokenType::Fn,
                42,
                44,
                TokenType::Identifier(string_interner.get_or_intern("draw")),
                45,
                49,
                TokenType::LeftParentheses,
                49,
                50,
                TokenType::Identifier(string_interner.get_or_intern("Graphics")),
                50,
                58,
                TokenType::Identifier(string_interner.get_or_intern("g")),
                59,
                60,
                TokenType::RightParentheses,
                60,
                61,
                TokenType::Semicolon,
                61,
                62,
                TokenType::Fn,
                66,
                68,
                TokenType::Identifier(string_interner.get_or_intern("children")),
                69,
                77,
                TokenType::LeftParentheses,
                77,
                78,
                TokenType::RightParentheses,
                78,
                79,
                TokenType::RightArrow,
                80,
                82,
                TokenType::Identifier(string_interner.get_or_intern("MutableList")),
                83,
                94,
                TokenType::Less,
                94,
                95,
                TokenType::Identifier(string_interner.get_or_intern("Node")),
                95,
                99,
                TokenType::Greater,
                99,
                100,
                TokenType::Semicolon,
                100,
                101,
                TokenType::RightBrace,
                102,
                103,
                TokenType::Fn,
                105,
                107,
                TokenType::Identifier(string_interner.get_or_intern("draw_frame")),
                108,
                118,
                TokenType::LeftParentheses,
                118,
                119,
                TokenType::Identifier(string_interner.get_or_intern("nodes")),
                119,
                124,
                TokenType::Colon,
                124,
                125,
                TokenType::Identifier(string_interner.get_or_intern("List")),
                126,
                130,
                TokenType::Less,
                130,
                131,
                TokenType::Identifier(string_interner.get_or_intern("Node")),
                131,
                135,
                TokenType::Greater,
                135,
                136,
                TokenType::RightParentheses,
                136,
                137,
                TokenType::LeftBrace,
                138,
                139,
                TokenType::RightBrace,
                140,
                141
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
            string_interner.clone(),
            tokenize!(
                TokenType::Class,
                0,
                5,
                TokenType::Identifier(string_interner.get_or_intern("SortedMap")),
                6,
                15,
                TokenType::Less,
                15,
                16,
                TokenType::Identifier(string_interner.get_or_intern("T")),
                16,
                17,
                TokenType::Colon,
                17,
                18,
                TokenType::Identifier(string_interner.get_or_intern("Sortable")),
                19,
                27,
                TokenType::Plus,
                28,
                29,
                TokenType::Identifier(string_interner.get_or_intern("Hashable")),
                30,
                38,
                TokenType::Greater,
                38,
                39,
                TokenType::Semicolon,
                39,
                40
            ),
            "class SortedMap<T: Sortable + Hashable>;"
        );

        test!(
            string_interner.clone(),
            tokenize!(
                TokenType::Enum,
                0,
                4,
                TokenType::Identifier(string_interner.get_or_intern("Message")),
                5,
                12,
                TokenType::LeftBrace,
                13,
                14,
                TokenType::Identifier(string_interner.get_or_intern("Text")),
                19,
                23,
                TokenType::LeftParentheses,
                23,
                24,
                TokenType::Identifier(string_interner.get_or_intern("message")),
                24,
                31,
                TokenType::Colon,
                31,
                32,
                TokenType::Identifier(string_interner.get_or_intern("str")),
                33,
                36,
                TokenType::RightParentheses,
                36,
                37,
                TokenType::Comma,
                37,
                38,
                TokenType::Identifier(string_interner.get_or_intern("Photo")),
                43,
                48,
                TokenType::LeftParentheses,
                48,
                49,
                TokenType::Identifier(string_interner.get_or_intern("caption")),
                49,
                56,
                TokenType::Colon,
                56,
                57,
                TokenType::Identifier(string_interner.get_or_intern("str")),
                58,
                61,
                TokenType::Comma,
                61,
                62,
                TokenType::Identifier(string_interner.get_or_intern("photo")),
                63,
                68,
                TokenType::Colon,
                68,
                69,
                TokenType::Identifier(string_interner.get_or_intern("SerializedPhoto")),
                70,
                85,
                TokenType::RightParentheses,
                85,
                86,
                TokenType::LeftBrace,
                87,
                88,
                TokenType::Fn,
                97,
                99,
                TokenType::Identifier(string_interner.get_or_intern("size")),
                100,
                104,
                TokenType::LeftParentheses,
                104,
                105,
                TokenType::RightParentheses,
                105,
                106,
                TokenType::LeftBrace,
                107,
                108,
                TokenType::Return,
                121,
                127,
                TokenType::Identifier(string_interner.get_or_intern("photo")),
                128,
                133,
                TokenType::Dot,
                133,
                134,
                TokenType::Identifier(string_interner.get_or_intern("size")),
                134,
                138,
                TokenType::LeftParentheses,
                138,
                139,
                TokenType::RightParentheses,
                139,
                140,
                TokenType::Semicolon,
                140,
                141,
                TokenType::RightBrace,
                150,
                151,
                TokenType::RightBrace,
                156,
                157,
                TokenType::Comma,
                157,
                158,
                TokenType::RightBrace,
                159,
                160
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
    pub fn complex_enum_parsing() {
        let code = concat!(
            "enum Vector<X: Number + Display, Y: Number + Display> {\n",
            "    Normalized(x: X, y: Y),\n",
            "    Absolute(x: X, y: Y) {\n",
            "        pub fn to_normalized(self) -> Vector {\n",
            "            return Normalized(self.x, self.y);\n",
            "        }\n",
            "    }\n",
            "}"
        );
        compare_tokens("complex_enum", code);
    }

    #[test]
    pub fn simple_statement_parsing() {
        let string_interner = StringInterner::default();
        test!(
            string_interner.clone(),
            tokenize!(
                TokenType::Use,
                0,
                3,
                TokenType::Identifier(string_interner.get_or_intern("std")),
                4,
                7,
                TokenType::Colon,
                7,
                8,
                TokenType::Colon,
                8,
                9,
                TokenType::Identifier(string_interner.get_or_intern("vector")),
                9,
                15,
                TokenType::Colon,
                15,
                16,
                TokenType::Colon,
                16,
                17,
                TokenType::Identifier(string_interner.get_or_intern("Vector")),
                17,
                23
            ),
            "use std::vector::Vector"
        );
    }

    #[test]
    pub fn float_parsing() {
        let string_interner = StringInterner::default();

        test!(string_interner.clone(), tokenize!(TokenType::Float(123.45), 0, 6), "123.45");

        test!(string_interner.clone(), tokenize!(TokenType::Float(123.01), 1, 7), " 123.01");
    }

    #[test]
    pub fn int_parsing() {
        let string_interner = StringInterner::default();

        test!(string_interner.clone(), tokenize!(TokenType::SignedInteger(-123), 0, 4), "-123");

        test!(string_interner.clone(), tokenize!(TokenType::SignedInteger(123), 0, 3), "123");

        test!(
            string_interner.clone(),
            tokenize!(TokenType::SignedInteger(123), 0, 3, TokenType::Dot, 3, 4),
            "123."
        );
    }
}
