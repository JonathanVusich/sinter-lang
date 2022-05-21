use std::error::Error;
use std::fs;
use std::fs::File;
use std::io;
use std::path::Path;
use std::concat;

use anyhow::Result;
use unicode_segmentation::UnicodeSegmentation;
use crate::compiler::tokens::token::{Token, TokenType};
use crate::compiler::tokens::tokenized_file::TokenizedInput;


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
            },
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
            },
            "=" => {
                if self.matches("=") {
                    self.create_token(TokenType::EqualEqual)
                } else if self.matches(">") {
                    self.create_token(TokenType::RightArrow)
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

    fn parse_identifier(&mut self, character: &str) {
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
                    "n" => {
                        self.advance();
                        Some(TokenType::Fn)
                    },
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
            "N" => self.check_keyword(1, "one", TokenType::None),
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
            "t" => {
                match self.peek() {
                    "y" => self.check_keyword(2, "pe", TokenType::Type),
                    "r" => self.check_keyword(2, "ait", TokenType::Trait),
                    _ => None
                }
            },
            "u" => self.check_keyword(1, "se", TokenType::Use),
            "w" => self.check_keyword(1, "hile", TokenType::While),
            "v" => {
                match self.peek() {
                    "a" => {
                        match self.peek_next() {
                            Some(letter) => {
                                match letter {
                                    "r" => {
                                        self.advance();
                                        self.advance();
                                        Some(TokenType::Var)
                                    },
                                    "l" => {
                                        self.advance();
                                        self.advance();
                                        Some(TokenType::Val)
                                    },
                                    _ => None,
                                }
                            },
                            None => None,
                        }
                    },
                    _ => None,
                }
            }
            _ => None
        }.unwrap_or_else(|| {
            while !self.is_at_end() && self.is_valid_identifier() {
                self.advance();
            }

            let identifier = self.source_chars[self.start..self.current].join("");
            let static_ident = Box::leak(identifier.into_boxed_str());
            TokenType::Identifier(static_ident)
        });

        self.create_token(token_type);
    }

    fn check_keyword(&mut self, start: usize, remainder: &'static str, token_type: TokenType) -> Option<TokenType> {
        let end = self.start + start + remainder.len();
        if end > self.source_chars.len() {
            return None;
        }

        let next_chars = self.source_chars[self.start + start..end].join("");
        if next_chars == remainder && (end == self.source_chars.len() || end < self.source_chars.len() && is_delimiter(self.source_chars[end])) {
            self.current = end;
            return Some(token_type)
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

            let token_type: TokenType = self.source_chars[self.start..self.current].join("").parse::<f64>()
                .map(TokenType::Float)
                .unwrap_or(TokenType::Unrecognized("Invalid float."));

            self.create_token(token_type);
            return;
        }

        let token_type: TokenType = self.source_chars[self.start..self.current].join("").parse::<i64>()
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
        !matches!(self.source_chars[self.current], "\r" | "\t" | "\n" | " " | "~" | "`" | "!" | "@"
            | "#" | "$" | "%" | "^" | "&" | "*" | "(" | ")" | "-" | "+"
            | "=" | "[" | "]" | "{" | "}" | "\\" | "|" | ";" | ":" | "'"
            | "\"" | "<" | ">" | "," | "." | "?" | "/"
        )
    }

    fn create_unrecognized_token(&mut self, error_message: &'static str) {
        self.create_token(TokenType::Unrecognized(error_message));
    }

    fn create_token(&mut self, token_type: TokenType) {
        let token = Token::new(token_type, self.start);
        self.start = self.current;
        self.tokenized_file.add_token(token);
    }
}

fn is_digit(word: &str) -> bool {
    matches!(word, "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9")
}

fn is_delimiter(char: &str) -> bool {
    match char {
        " " | "\r" | "\t" | "\n" | ";" => {
            true
        }
        _ => false
    }
}

mod tests {

    use crate::compiler::tokens::token::{Token, TokenType};
    use crate::compiler::tokens::tokenizer::Tokenizer;

    macro_rules! make_token {
        ($token_ident:expr, $pos:tt) => {
            Token::new($token_ident, $pos)
        }
    }

    macro_rules! tokenize {
        ($($token_ident:expr, $pos:literal),*) => {
            vec![
                $(
                    make_token!($token_ident, $pos),
                )*
            ]
        }
    }

    macro_rules! test {
        ($tokens:expr, $matching_text:literal) => {
            assert_eq!($tokens, Tokenizer::new($matching_text).into().tokens());
        };
        ($tokens:expr, $matching_text:expr) => {
            assert_eq!($tokens, Tokenizer::new($matching_text).into().tokens());
        }
    }

    #[test]
    pub fn token_generation() {
        test!(
            tokenize!(
                TokenType::Pub, 0,
                TokenType::Class, 4,
                TokenType::Identifier("Random"), 10,
                TokenType::LeftBrace, 17,
                TokenType::RightBrace, 18
            ),
            "pub class Random {}"
        );

        test!(
            tokenize!(
                TokenType::Impl, 0,
                TokenType::Enum, 5,
                TokenType::Identifier("Reader"), 12,
                TokenType::LeftBracket, 21,
                TokenType::RightBracket, 23
            ),
            "impl enum \n Reader \n [ ]"
        );

        test!(
            tokenize!(
                TokenType::Native, 0,
                TokenType::Identifier("nativer"), 7,
                TokenType::Identifier("enative"), 15
            ),
            "native nativer enative"
        );

        test!(
            tokenize!(
                TokenType::Trait, 0,
                TokenType::Identifier("Serializable"), 6,
                TokenType::LeftBrace, 19,
                TokenType::RightBrace, 21
            ),
            "trait Serializable { }"
        );

        test!(
            tokenize!(
                TokenType::Trait, 0,
                TokenType::Identifier("Iterator"), 6,
                TokenType::Less, 14,
                TokenType::Identifier("T"), 15,
                TokenType::Greater, 16,
                TokenType::LeftBrace, 18,
                TokenType::Fn, 20,
                TokenType::Identifier("next"), 23,
                TokenType::LeftParentheses, 27,
                TokenType::RightParentheses, 28,
                TokenType::RightArrow, 30,
                TokenType::Identifier("T"), 33,
                TokenType::Pipe, 35,
                TokenType::None, 37,
                TokenType::Semicolon, 41,
                TokenType::RightBrace, 43
            ),
            "trait Iterator<T> { \
                 fn next() => T | None; \
             }"
        );


        test!(
            tokenize!(
                TokenType::Class, 0,
                TokenType::Identifier("Point"), 6,
                TokenType::Less, 11,
                TokenType::Identifier("T"), 12,
                TokenType::Comma, 13,
                TokenType::Identifier("U"), 15,
                TokenType::Greater, 16,
                TokenType::LeftParentheses, 17,
                TokenType::Identifier("x"), 18,
                TokenType::Colon, 19,
                TokenType::Identifier("T"), 21,
                TokenType::Comma, 22,
                TokenType::Identifier("y"), 24,
                TokenType::Colon, 25,
                TokenType::Identifier("U"), 27,
                TokenType::RightParentheses, 28,
                TokenType::Semicolon, 29
            ),
            "class Point<T, U>(x: T, y: U);"
        );

        test!(
            tokenize!(
                TokenType::Fn, 0,
                TokenType::Identifier("main"), 3,
                TokenType::LeftParentheses, 7,
                TokenType::Identifier("arguments"), 8,
                TokenType::Colon, 17,
                TokenType::LeftBracket, 19,
                TokenType::Identifier("str"), 20,
                TokenType::RightBracket, 23,
                TokenType::RightParentheses, 24,
                TokenType::LeftBrace, 26,
                TokenType::Identifier("print"), 28,
                TokenType::LeftParentheses, 33,
                TokenType::Identifier("arguments"), 34,
                TokenType::Dot, 43,
                TokenType::Identifier("to_string"), 44,
                TokenType::LeftParentheses, 53,
                TokenType::RightParentheses, 54,
                TokenType::RightParentheses, 55,
                TokenType::Semicolon, 56,
                TokenType::RightBrace, 58
            ),
            "fn main(arguments: [str]) { \
                 print(arguments.to_string()); \
             }"
        );

        test!(
            tokenize!(
                TokenType::Val, 0,
                TokenType::Identifier("greeting"), 4,
                TokenType::Equal, 13,
                TokenType::String("Hello world!"), 15,
                TokenType::Semicolon, 29,
                TokenType::Val, 57,
                TokenType::Identifier("bytearray"), 61,
                TokenType::Colon, 70,
                TokenType::LeftBracket, 72,
                TokenType::Identifier("u8"), 73,
                TokenType::RightBracket, 75,
                TokenType::Equal, 77,
                TokenType::LeftBracket, 79,
                TokenType::SignedInteger(72), 80,
                TokenType::Comma, 82,
                TokenType::SignedInteger(101), 84,
                TokenType::Comma, 87,
                TokenType::SignedInteger(108), 89,
                TokenType::Comma, 92,
                TokenType::SignedInteger(108), 94,
                TokenType::Comma, 97,
                TokenType::SignedInteger(111), 99,
                TokenType::Comma, 102,
                TokenType::SignedInteger(32), 104,
                TokenType::Comma, 106,
                TokenType::SignedInteger(119), 108,
                TokenType::Comma, 111,
                TokenType::SignedInteger(111), 113,
                TokenType::Comma, 116,
                TokenType::SignedInteger(114), 118,
                TokenType::Comma, 121,
                TokenType::SignedInteger(108), 123,
                TokenType::Comma, 126,
                TokenType::SignedInteger(100), 128,
                TokenType::Comma, 131,
                TokenType::SignedInteger(33), 133,
                TokenType::RightBracket, 135,
                TokenType::Semicolon, 136,
                TokenType::Val, 138,
                TokenType::Identifier("greeting_from_array"), 142,
                TokenType::Equal, 162,
                TokenType::Identifier("str"), 164,
                TokenType::LeftParentheses, 167,
                TokenType::Identifier("bytearray"), 168,
                TokenType::RightParentheses, 177,
                TokenType::Semicolon, 178
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
                TokenType::String(""), 0
            ),
            r#""""#
        );

        test!(
            tokenize!(
                TokenType::String("a"), 0
            ),
            r#""a""#
        );

        test!(
            tokenize!(
                TokenType::Trait, 0,
                TokenType::Identifier("Node"), 6,
                TokenType::LeftBrace, 11,
                TokenType::Fn, 16,
                TokenType::Identifier("bounds"), 19,
                TokenType::LeftParentheses, 25,
                TokenType::RightParentheses, 26,
                TokenType::RightArrow, 28,
                TokenType::Identifier("Bounds"), 31,
                TokenType::Semicolon, 37,
                TokenType::Fn, 42,
                TokenType::Identifier("draw"), 45,
                TokenType::LeftParentheses, 49,
                TokenType::Identifier("Graphics"), 50,
                TokenType::Identifier("g"), 59,
                TokenType::RightParentheses, 60,
                TokenType::Semicolon, 61,
                TokenType::Fn, 66,
                TokenType::Identifier("children"), 69,
                TokenType::LeftParentheses, 77,
                TokenType::RightParentheses, 78,
                TokenType::RightArrow, 80,
                TokenType::Identifier("MutableList"), 83,
                TokenType::Less, 94,
                TokenType::Identifier("Node"), 95,
                TokenType::Greater, 99,
                TokenType::Semicolon, 100,
                TokenType::RightBrace, 102,
                TokenType::Fn, 105,
                TokenType::Identifier("draw_frame"), 108,
                TokenType::LeftParentheses, 118,
                TokenType::Identifier("nodes"), 119,
                TokenType::Colon, 124,
                TokenType::Identifier("List"), 126,
                TokenType::Less, 130,
                TokenType::Identifier("Node"), 131,
                TokenType::Greater, 135,
                TokenType::RightParentheses, 136,
                TokenType::LeftBrace, 138,
                TokenType::RightBrace, 140
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
                TokenType::Class, 0,
                TokenType::Identifier("SortedMap"), 6,
                TokenType::Less, 15,
                TokenType::Identifier("T"), 16,
                TokenType::Colon, 17,
                TokenType::Identifier("Sortable"), 19,
                TokenType::Plus, 28,
                TokenType::Identifier("Hashable"), 30,
                TokenType::Greater, 38,
                TokenType::Semicolon, 39
            ),
            "class SortedMap<T: Sortable + Hashable>;"
        );

        test!(
            tokenize!(
                TokenType::Enum, 0,
                TokenType::Identifier("Message"), 5,
                TokenType::LeftBrace, 13,
                TokenType::Identifier("Text"), 19,
                TokenType::LeftParentheses, 23,
                TokenType::Identifier("message"), 24,
                TokenType::Colon, 31,
                TokenType::Identifier("str"), 33,
                TokenType::RightParentheses, 36,
                TokenType::Comma, 37,
                TokenType::Identifier("Photo"), 43,
                TokenType::LeftParentheses, 48,
                TokenType::Identifier("caption"), 49,
                TokenType::Colon, 56,
                TokenType::Identifier("str"), 58,
                TokenType::Comma, 61,
                TokenType::Identifier("photo"), 63,
                TokenType::Colon, 68,
                TokenType::Identifier("SerializedPhoto"), 70,
                TokenType::RightParentheses, 85,
                TokenType::LeftBrace, 87,
                TokenType::Fn, 97,
                TokenType::Identifier("size"), 100,
                TokenType::LeftParentheses, 104,
                TokenType::RightParentheses, 105,
                TokenType::LeftBrace, 107,
                TokenType::Return, 121,
                TokenType::Identifier("photo"), 128,
                TokenType::Dot, 133,
                TokenType::Identifier("size"), 134,
                TokenType::LeftParentheses, 138,
                TokenType::RightParentheses, 139,
                TokenType::Semicolon, 140,
                TokenType::RightBrace, 150,
                TokenType::RightBrace, 156,
                TokenType::Comma, 157,
                TokenType::RightBrace, 159
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
                TokenType::Float(123.45), 0
            ),
            "123.45"
        );

        test!(
            tokenize!(
                TokenType::Float(123.01), 1
            ),
            " 123.01"
        );
    }

    #[test]
    pub fn int_parsing() {
        test!(
            tokenize!(
                TokenType::SignedInteger(-123), 0
            ),
            "-123"
        );

        test!(
            tokenize!(
                TokenType::SignedInteger(123), 0
            ),
            "123"
        );

        test!(
            tokenize!(
                TokenType::SignedInteger(123), 0,
                TokenType::Dot, 3
            ),
            "123."
        );
    }
}