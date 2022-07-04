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

pub fn tokenize_file(path: &Path) -> Result<TokenizedInput> {
    let source_file = fs::read_to_string(path)?;
    let tokenizer = Tokenizer::new(&source_file);
    Ok(tokenizer.into())
}

pub fn tokenize<T: AsRef<str>>(input: T) -> Result<TokenizedInput> {
    let source_file = input.as_ref();
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
    match char {
        " " | "\r" | "\t" | "\n" | ";" => true,
        _ => false,
    }
}

mod tests {
    use libc::stat;
    use crate::compiler::tokens::token::{Token, TokenType};
    use crate::compiler::tokens::tokenized_file::TokenizedInput;
    use crate::compiler::tokens::tokenizer::{tokenize, Tokenizer};

    #[allow(unused_macros)]
    macro_rules! make_token {
        ($token_ident:expr, $start:tt, $current:tt) => {
            Token::new($token_ident, $start, $current)
        }
    }

    #[allow(unused_macros)]
    macro_rules! tokenize {
        ($($token_ident:expr, $start:literal, $current:literal),*) => {
            vec![
                $(
                    make_token!($token_ident, $start, $current),
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
    pub fn tokenize_strings() {
        let static_str = "pub class Random {}";
        let string = String::from(static_str);

        let expected = tokenize!(
            TokenType::Pub, 0, 3,
            TokenType::Class, 4, 9,
            TokenType::Identifier("Random"), 10, 16,
            TokenType::LeftBrace, 17, 18,
            TokenType::RightBrace, 18, 19
        );

        assert_eq!(expected, tokenize(string).unwrap().tokens());
        assert_eq!(expected, tokenize(static_str).unwrap().tokens());
    }

    #[test]
    pub fn token_generation() {
        test!(
            tokenize!(
                TokenType::Pub, 0, 3,
                TokenType::Class, 4, 9,
                TokenType::Identifier("Random"), 10, 16,
                TokenType::LeftBrace, 17, 18,
                TokenType::RightBrace, 18, 19
            ),
            "pub class Random {}"
        );

        test!(
            tokenize!(
                TokenType::Impl, 0, 4,
                TokenType::Enum, 5, 9,
                TokenType::Identifier("Reader"), 12, 18,
                TokenType::LeftBracket, 21, 22,
                TokenType::RightBracket, 23, 24
            ),
            "impl enum \n Reader \n [ ]"
        );

        test!(
            tokenize!(
                TokenType::Native, 0, 6,
                TokenType::Identifier("nativer"), 7, 14,
                TokenType::Identifier("enative"), 15, 22
            ),
            "native nativer enative"
        );

        test!(
            tokenize!(
                TokenType::Trait, 0, 5,
                TokenType::Identifier("Serializable"), 6, 18,
                TokenType::LeftBrace, 19, 20,
                TokenType::RightBrace, 21, 22
            ),
            "trait Serializable { }"
        );

        test!(
            tokenize!(
                TokenType::Trait, 0, 5,
                TokenType::Identifier("Iterator"), 6, 14,
                TokenType::Less, 14, 15,
                TokenType::Identifier("T"), 15, 16,
                TokenType::Greater, 16, 17,
                TokenType::LeftBrace, 18, 19,
                TokenType::Fn, 20, 22,
                TokenType::Identifier("next"), 23, 27,
                TokenType::LeftParentheses, 27, 28,
                TokenType::RightParentheses, 28, 29,
                TokenType::RightArrow, 30, 32,
                TokenType::Identifier("T"), 33, 34,
                TokenType::Pipe, 35, 36,
                TokenType::None, 37, 41,
                TokenType::Semicolon, 41, 42,
                TokenType::RightBrace, 43, 44
            ),
            "trait Iterator<T> { \
                 fn next() => T | None; \
             }"
        );

        test!(
            tokenize!(
                TokenType::Class, 0, 5,
                TokenType::Identifier("Point"), 6, 11,
                TokenType::Less, 11, 12,
                TokenType::Identifier("T"), 12, 13,
                TokenType::Comma, 13, 14,
                TokenType::Identifier("U"), 15, 16,
                TokenType::Greater, 16, 17,
                TokenType::LeftParentheses, 17, 18,
                TokenType::Identifier("x"), 18, 19,
                TokenType::Colon, 19, 20,
                TokenType::Identifier("T"), 21, 22,
                TokenType::Comma, 22, 23,
                TokenType::Identifier("y"), 24, 25,
                TokenType::Colon, 25, 26,
                TokenType::Identifier("U"), 27, 28,
                TokenType::RightParentheses, 28, 29,
                TokenType::Semicolon, 29, 30
            ),
            "class Point<T, U>(x: T, y: U);"
        );

        test!(
            tokenize!(
                TokenType::Fn, 0, 2,
                TokenType::Identifier("main"), 3, 7,
                TokenType::LeftParentheses, 7, 8,
                TokenType::Identifier("arguments"), 8, 17,
                TokenType::Colon, 17, 18,
                TokenType::LeftBracket, 19, 20,
                TokenType::Identifier("str"), 20, 23,
                TokenType::RightBracket, 23, 24,
                TokenType::RightParentheses, 24, 25,
                TokenType::LeftBrace, 26, 27,
                TokenType::Identifier("print"), 28, 33,
                TokenType::LeftParentheses, 33, 34,
                TokenType::Identifier("arguments"), 34, 43,
                TokenType::Dot, 43, 44,
                TokenType::Identifier("to_string"), 44, 53,
                TokenType::LeftParentheses, 53, 54,
                TokenType::RightParentheses, 54, 55,
                TokenType::RightParentheses, 55, 56,
                TokenType::Semicolon, 56, 57,
                TokenType::RightBrace, 58, 59
            ),
            "fn main(arguments: [str]) { \
                 print(arguments.to_string()); \
             }"
        );

        test!(
            tokenize!(
                TokenType::Val, 0, 3,
                TokenType::Identifier("greeting"), 4, 12,
                TokenType::Equal, 13, 14,
                TokenType::String("Hello world!"), 15, 29,
                TokenType::Semicolon, 29, 30,
                TokenType::Val, 57, 60,
                TokenType::Identifier("bytearray"), 61, 70,
                TokenType::Colon, 70, 71,
                TokenType::LeftBracket, 72, 73,
                TokenType::Identifier("u8"), 73, 75,
                TokenType::RightBracket, 75, 76,
                TokenType::Equal, 77, 78,
                TokenType::LeftBracket, 79, 80,
                TokenType::SignedInteger(72), 80, 82,
                TokenType::Comma, 82, 83,
                TokenType::SignedInteger(101), 84, 87,
                TokenType::Comma, 87, 88,
                TokenType::SignedInteger(108), 89, 92,
                TokenType::Comma, 92, 93,
                TokenType::SignedInteger(108), 94, 97,
                TokenType::Comma, 97, 98,
                TokenType::SignedInteger(111), 99, 102,
                TokenType::Comma, 102, 103,
                TokenType::SignedInteger(32), 104, 106,
                TokenType::Comma, 106, 107,
                TokenType::SignedInteger(119), 108, 111,
                TokenType::Comma, 111, 112,
                TokenType::SignedInteger(111), 113, 116,
                TokenType::Comma, 116, 117,
                TokenType::SignedInteger(114), 118, 121,
                TokenType::Comma, 121, 122,
                TokenType::SignedInteger(108), 123, 126,
                TokenType::Comma, 126, 127,
                TokenType::SignedInteger(100), 128, 131,
                TokenType::Comma, 131, 132,
                TokenType::SignedInteger(33), 133, 135,
                TokenType::RightBracket, 135, 136,
                TokenType::Semicolon, 136, 137,
                TokenType::Val, 138, 141,
                TokenType::Identifier("greeting_from_array"), 142, 161,
                TokenType::Equal, 162, 163,
                TokenType::Identifier("str"), 164, 167,
                TokenType::LeftParentheses, 167, 168,
                TokenType::Identifier("bytearray"), 168, 177,
                TokenType::RightParentheses, 177, 178,
                TokenType::Semicolon, 178, 179
            ),
            concat!(
                r#"val greeting = "Hello world!"; // 'str' type is inferred"#,
                "\n",
                r#"val bytearray: [u8] = [72, 101, 108, 108, 111, 32, 119, 111, 114, 108, 100, 33];"#,
                "\n",
                r#"val greeting_from_array = str(bytearray); // "Hello world!""#
            )
        );

        test!(
            tokenize!(
                TokenType::String(""), 0, 2
            ),
            r#""""#
        );

        test!(
            tokenize!(
                TokenType::String("a"), 0, 3
            ),
            r#""a""#
        );

        test!(
            tokenize!(
                TokenType::Trait, 0, 5,
                TokenType::Identifier("Node"), 6, 10,
                TokenType::LeftBrace, 11, 12,
                TokenType::Fn, 16, 18,
                TokenType::Identifier("bounds"), 19, 25,
                TokenType::LeftParentheses, 25, 26,
                TokenType::RightParentheses, 26, 27,
                TokenType::RightArrow, 28, 30,
                TokenType::Identifier("Bounds"), 31, 37,
                TokenType::Semicolon, 37, 38,
                TokenType::Fn, 42, 44,
                TokenType::Identifier("draw"), 45, 49,
                TokenType::LeftParentheses, 49, 50,
                TokenType::Identifier("Graphics"), 50, 58,
                TokenType::Identifier("g"), 59, 60,
                TokenType::RightParentheses, 60, 61,
                TokenType::Semicolon, 61, 62,
                TokenType::Fn, 66, 68,
                TokenType::Identifier("children"), 69, 77,
                TokenType::LeftParentheses, 77, 78,
                TokenType::RightParentheses, 78, 79,
                TokenType::RightArrow, 80, 82,
                TokenType::Identifier("MutableList"), 83, 94,
                TokenType::Less, 94, 95,
                TokenType::Identifier("Node"), 95, 99,
                TokenType::Greater, 99, 100,
                TokenType::Semicolon, 100, 101,
                TokenType::RightBrace, 102, 103,
                TokenType::Fn, 105, 107,
                TokenType::Identifier("draw_frame"), 108, 118,
                TokenType::LeftParentheses, 118, 119,
                TokenType::Identifier("nodes"), 119, 124,
                TokenType::Colon, 124, 125,
                TokenType::Identifier("List"), 126, 130,
                TokenType::Less, 130, 131,
                TokenType::Identifier("Node"), 131, 135,
                TokenType::Greater, 135, 136,
                TokenType::RightParentheses, 136, 137,
                TokenType::LeftBrace, 138, 139,
                TokenType::RightBrace, 140, 141
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
                TokenType::Class, 0, 5,
                TokenType::Identifier("SortedMap"), 6, 15,
                TokenType::Less, 15, 16,
                TokenType::Identifier("T"), 16, 17,
                TokenType::Colon, 17, 18,
                TokenType::Identifier("Sortable"), 19, 27,
                TokenType::Plus, 28, 29,
                TokenType::Identifier("Hashable"), 30, 38,
                TokenType::Greater, 38, 39,
                TokenType::Semicolon, 39, 40
            ),
            "class SortedMap<T: Sortable + Hashable>;"
        );

        test!(
            tokenize!(
                TokenType::Enum, 0, 4,
                TokenType::Identifier("Message"), 5, 12,
                TokenType::LeftBrace, 13, 14,
                TokenType::Identifier("Text"), 19, 23,
                TokenType::LeftParentheses, 23, 24,
                TokenType::Identifier("message"), 24, 31,
                TokenType::Colon, 31, 32,
                TokenType::Identifier("str"), 33, 36,
                TokenType::RightParentheses, 36, 37,
                TokenType::Comma, 37, 38,
                TokenType::Identifier("Photo"), 43, 48,
                TokenType::LeftParentheses, 48, 49,
                TokenType::Identifier("caption"), 49, 56,
                TokenType::Colon, 56, 57,
                TokenType::Identifier("str"), 58, 61,
                TokenType::Comma, 61, 62,
                TokenType::Identifier("photo"), 63, 68,
                TokenType::Colon, 68, 69,
                TokenType::Identifier("SerializedPhoto"), 70, 85,
                TokenType::RightParentheses, 85, 86,
                TokenType::LeftBrace, 87, 88,
                TokenType::Fn, 97, 99,
                TokenType::Identifier("size"), 100, 104,
                TokenType::LeftParentheses, 104, 105,
                TokenType::RightParentheses, 105, 106,
                TokenType::LeftBrace, 107, 108,
                TokenType::Return, 121, 127,
                TokenType::Identifier("photo"), 128, 133,
                TokenType::Dot, 133, 134,
                TokenType::Identifier("size"), 134, 138,
                TokenType::LeftParentheses, 138, 139,
                TokenType::RightParentheses, 139, 140,
                TokenType::Semicolon, 140, 141,
                TokenType::RightBrace, 150, 151,
                TokenType::RightBrace, 156, 157,
                TokenType::Comma, 157, 158,
                TokenType::RightBrace, 159, 160
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
        test!(
            tokenize!(
                TokenType::Float(123.45), 0, 6
            ),
            "123.45"
        );

        test!(
            tokenize!(
                TokenType::Float(123.01), 1, 7
            ),
            " 123.01"
        );
    }

    #[test]
    pub fn int_parsing() {
        test!(
            tokenize!(
                TokenType::SignedInteger(-123), 0, 4
            ),
            "-123"
        );

        test!(
            tokenize!(
                TokenType::SignedInteger(123), 0, 3
            ),
            "123"
        );

        test!(
            tokenize!(
                TokenType::SignedInteger(123), 0, 3,
                TokenType::Dot, 3, 4
            ),
            "123."
        );
    }
}
