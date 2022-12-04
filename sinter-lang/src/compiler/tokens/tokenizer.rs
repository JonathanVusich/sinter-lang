use std::borrow::Cow;
use std::concat;
use std::error::Error;
use std::fs;
use std::fs::File;
use std::io;
use std::path::Path;

use phf::phf_map;

use crate::compiler::tokens::token::{Token, TokenType};
use crate::compiler::tokens::tokenized_file::TokenizedInput;
use anyhow::Result;
use unicode_segmentation::UnicodeSegmentation;
use crate::compiler::StringInterner;
use crate::compiler::types::types::InternedStr;

pub fn tokenize_file(string_interner: StringInterner, path: &Path) -> Result<TokenizedInput> {
    let source_file = fs::read_to_string(path)?;
    let tokenizer = Tokenizer::new(string_interner, &source_file);
    Ok(tokenizer.into())
}

pub fn tokenize<T: AsRef<str>>(string_interner: StringInterner, input: T) -> Result<TokenizedInput> {
    let source_file = input.as_ref();
    let tokenizer = Tokenizer::new(string_interner, source_file);
    Ok(tokenizer.into())
}

static KEYWORDS: phf::Map<&'static str, TokenType> = phf_map! {
    "class" => TokenType::Class,
    "fn" => TokenType::Fn,
    "break" => TokenType::Break,
    "continue" => TokenType::Continue,
    "else" => TokenType::Else,
    "&&" => TokenType::And,
    "||" => TokenType::Or,
    "if" => TokenType::If,
    "false" => TokenType::False,
    "true" => TokenType::True,
    "for" => TokenType::For,
    "let" => TokenType::Let,
    "mut" => TokenType::Mut,
    "while" => TokenType::While,
    "return" => TokenType::Return,
    "self" => TokenType::SelfLowercase,
    "Self" => TokenType::SelfCapitalized,
    "native" => TokenType::Native,
    "enum" => TokenType::Enum,
    "impl" => TokenType::Impl,
    "match" => TokenType::Match,
    "pub" => TokenType::Pub,
    "static" => TokenType::Static,
    "trait" => TokenType::Trait,
    "use" => TokenType::Use,
    "None" => TokenType::None,
    "inline" => TokenType::Inline,
    "sync" => TokenType::Sync,
    "in" => TokenType::In,
};

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
                    self.create_token(TokenType::BitwiseAnd)
                }
            }
            "{" => self.create_token(TokenType::LeftBrace),
            "}" => self.create_token(TokenType::RightBrace),
            "[" => self.create_token(TokenType::LeftBracket),
            "]" => self.create_token(TokenType::RightBracket),
            "(" => self.create_token(TokenType::LeftParentheses),
            ")" => self.create_token(TokenType::RightParentheses),
            "," => self.create_token(TokenType::Comma),
            "." => self.create_token(TokenType::Dot),
            ":" => self.create_token(TokenType::Colon),
            ";" => self.create_token(TokenType::Semicolon),
            "-" => {
                if self.matcher(is_digit) {
                    self.parse_num()
                } else {
                    self.create_token(TokenType::Minus)
                }
            }
            "+" => self.create_token(TokenType::Plus),
            "/" => self.create_token(TokenType::Slash),
            "%" => self.create_token(TokenType::Percent),
            "*" => self.create_token(TokenType::Star),
            "|" => {
                if self.matches("|") {
                    self.create_token(TokenType::Or)
                } else {
                    self.create_token(TokenType::BitwiseOr)
                }
            }
            "~" => self.create_token(TokenType::BitwiseComplement),
            "^" => self.create_token(TokenType::BitwiseXor),
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
                } else if self.matches("<") {
                    self.create_token(TokenType::LeftShift)
                } else {
                    self.create_token(TokenType::Less)
                }
            }
            ">" => {
                if self.matches("=") {
                    self.create_token(TokenType::GreaterEqual)
                } else if self.matches(">") {
                    if self.peek_next().contains(&">") {
                        self.create_token(TokenType::TripleRightShift)
                    } else {
                        self.create_token(TokenType::RightShift)
                    }
                } else if self.matches("=") {
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
        while !self.is_at_end() && self.is_valid_identifier() {
            self.advance();
        }
        let identifier = self.source_chars[self.start..self.current].join("");
        let token_type = KEYWORDS.get(&identifier).copied()
            .unwrap_or_else(|| {
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
        println!("Check keyword!");
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
                .unwrap_or_else(|_| TokenType::Unrecognized(self.string_interner.get_or_intern("Invalid float.")));

            self.create_token(token_type);
            return;
        }

        let token_type: TokenType = self.source_chars[self.start..self.current]
            .join("")
            .parse::<i64>()
            .map(TokenType::SignedInteger)
            .unwrap_or_else(|_| TokenType::Unrecognized(self.string_interner.get_or_intern("Invalid integer.")));

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
        self.tokenized_file.tokens.push(token);
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
    use serde::de::Unexpected::Str;
    use crate::compiler::StringInterner;

    use snap::snapshot;

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
    fn tokenize_str(code: &str) -> (StringInterner, TokenizedInput) {
        let mut string_interner = StringInterner::default();
        (string_interner, tokenize(string_interner.clone(), code).unwrap())
    }

    #[test]
    #[snapshot]
    pub fn simple_class() -> (StringInterner, TokenizedInput) {
        tokenize_str("pub class Random {}")
    }

    #[test]
    #[snapshot]
    pub fn simple_enum() -> (StringInterner, TokenizedInput) {
        tokenize_str("impl enum \n Reader \n [ ]")
    }

    #[test]
    #[snapshot]
    pub fn invalid_native_keyword() -> (StringInterner, TokenizedInput) {
        tokenize_str("native nativer enative")
    }

    #[test]
    #[snapshot]
    pub fn simple_expression() -> (StringInterner, TokenizedInput) {
        tokenize_str("x = 123 >> 2 | 89 * 21")
    }

    #[test]
    #[snapshot]
    pub fn simple_trait() -> (StringInterner, TokenizedInput) {
        tokenize_str("trait Serializable { }")
    }

    #[test]
    #[snapshot]
    pub fn simple_iterator_trait() -> (StringInterner, TokenizedInput) {
        let code = concat!(
        "trait Iterator<T> {\n",
        "     fn next() => T | None;\n",
        "}"
        );

        tokenize_str(code)
    }

    #[test]
    #[snapshot]
    pub fn generic_point_class() -> (StringInterner, TokenizedInput) {
        tokenize_str("class Point<T, U>(x: T, y: U);")
    }

    #[test]
    #[snapshot]
    pub fn simple_main_stmt() -> (StringInterner, TokenizedInput) {
        tokenize_str(
            concat!(
            "fn main(arguments: [str]) {\n",
            "    println(arguments.to_string());\n",
            "}"
            )
        )
    }

    #[test]
    #[snapshot]
    pub fn var_declarations() -> (StringInterner, TokenizedInput) {
        tokenize_str(
            concat!(
            "let mut x = None;\n",
            "let y = 0;"
            ))
    }

    #[test]
    #[snapshot]
    pub fn uppercase_self() -> (StringInterner, TokenizedInput) {
        tokenize_str("Self::lower_hir")
    }

    #[test]
    #[snapshot]
    pub fn parameter_parsing() -> (StringInterner, TokenizedInput) {
        tokenize_str("fn mutate(mut self) => None;")
    }

    #[test]
    #[snapshot]
    pub fn bytearray_to_str() -> (StringInterner, TokenizedInput) {
        tokenize_str(concat!(
        r#"let greeting = "Hello world!"; // 'str' type is inferred"#,
        "\n",
        r#"let bytearray: [u8] = [72, 101, 108, 108, 111, 32, 119, 111, 114, 108, 100, 33];"#,
        "\n",
        r#"let greeting_from_array = str(bytearray); // "Hello world!""#
        ))
    }

    #[test]
    #[snapshot]
    pub fn empty_string() -> (StringInterner, TokenizedInput) {
        tokenize_str("")
    }

    #[test]
    #[snapshot]
    pub fn small_a_string() -> (StringInterner, TokenizedInput) {
        tokenize_str("a")
    }

    #[test]
    #[snapshot]
    pub fn scene_graph_node() -> (StringInterner, TokenizedInput) {
        tokenize_str(concat!(
        "trait Node {\n",
        "   fn bounds() => Bounds;\n",
        "   fn draw(Graphics g);\n",
        "   fn children() => MutableList<Node>;\n",
        "}\n",
        "\n",
        "fn draw_frame(nodes: List<Node>) { }"
        ))
    }

    #[test]
    #[snapshot]
    pub fn empty_class_with_traits() -> (StringInterner, TokenizedInput) {
        compare_tokens(function_name!(), "class SortedMap<T: Sortable + Hashable>;");
    }

    #[test]
    #[named]
    pub fn enum_with_member_funcs() {
        let code = concat!(
        "enum Message {\n",
        "    Text(message: str),\n",
        "    Photo(caption: str, photo: SerializedPhoto) {\n",
        "        fn size() {\n",
        "            return photo.size();\n",
        "        }\n",
        "    },\n",
        "}"
        );
        compare_tokens(function_name!(), code);
    }

    #[test]
    #[named]
    pub fn complex_enum() {
        let code = concat!(
        "enum Vector<X: Number + Display, Y: Number + Display> {\n",
        "    Normalized(x: X, y: Y),\n",
        "    Absolute(x: X, y: Y) {\n",
        "        fn to_normalized(self) => Vector {\n",
        "            return Normalized(self.x, self.y);\n",
        "        }\n",
        "    }\n",
        "}"
        );
        compare_tokens(function_name!(), code);
    }

    #[test]
    #[named]
    pub fn for_loop() {
        let code = concat!(
        "for x in 0..100 { }"
        );

        compare_tokens(function_name!(), code);
    }

    #[test]
    #[named]
    pub fn let_stmt_none() {
        compare_tokens(function_name!(), "let x: None;")
    }

    #[test]
    #[named]
    pub fn simple_statement() {
        compare_tokens(function_name!(), "use std::vector::Vector");
    }

    #[test]
    pub fn float_parsing() {
        let string_interner = StringInterner::default();

        test!(string_interner.clone(), tokenize!(TokenType::Float(123.45), 0, 6), "123.45");

        test!(string_interner, tokenize!(TokenType::Float(123.01), 1, 7), " 123.01");
    }

    #[test]
    pub fn int_parsing() {
        let string_interner = StringInterner::default();

        test!(string_interner.clone(), tokenize!(TokenType::SignedInteger(-123), 0, 4), "-123");

        test!(string_interner.clone(), tokenize!(TokenType::SignedInteger(123), 0, 3), "123");

        test!(
            string_interner,
            tokenize!(TokenType::SignedInteger(123), 0, 3, TokenType::Dot, 3, 4),
            "123."
        );
    }
}
