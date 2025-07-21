use std::error::Error;
use std::fs;
use std::path::Path;

use diagnostics::Diagnostic;
use diagnostics::Diagnostics;
use diagnostics::FatalError;
use interner::InternedStr;
use phf::phf_map;
use source::Source;
use unicode_segmentation::UnicodeSegmentation;

use crate::token::Token;
use crate::token::TokenType;
use crate::tokenized_file::TokenizedOutput;
use crate::tokenized_file::TokenizedSource;

pub fn tokenize_file(diagnostics: &Diagnostics, path: &Path) -> Option<TokenizedSource> {
    let token_source = Source::Path(path.to_path_buf());

    let source_file = fs::read_to_string(path)
        .map_err(|err| {
            diagnostics.push(Diagnostic::Fatal(FatalError::FileOpen));
            err
        })
        .ok()?;
    let tokenizer = Tokenizer::new(&source_file);
    let tokenized_output = tokenizer.tokenize();
    Some(TokenizedSource {
        tokens: tokenized_output.tokens,
        line_map: tokenized_output.line_map,
        token_source,
    })
}

pub fn tokenize(input: String) -> TokenizedSource {
    let tokenizer = Tokenizer::new(&input);
    let tokenized_output = tokenizer.tokenize();
    TokenizedSource {
        tokens: tokenized_output.tokens,
        line_map: tokenized_output.line_map,
        token_source: Source::Inline(input),
    }
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
    "ref" => TokenType::Ref,
    "in" => TokenType::In,
};

#[derive(Debug)]
pub(crate) struct Tokenizer<'this> {
    chars: Vec<&'this str>,
    tokenized_file: TokenizedOutput,
    start: usize,
    current: usize,
    start_byte_pos: usize,
    current_byte_pos: usize,
}

impl<'this> Tokenizer<'this> {
    pub fn new(source: &'this str) -> Self {
        let chars = source.graphemes(true).collect::<Vec<&'this str>>();

        Self {
            chars,
            tokenized_file: TokenizedOutput::new(),
            start: 0,
            current: 0,
            start_byte_pos: 0,
            current_byte_pos: 0,
        }
    }

    pub fn tokenize(mut self) -> TokenizedOutput {
        while self.current < self.chars.len() {
            self.skip_whitespace();
            self.scan_token();
        }
        self.tokenized_file
    }

    fn scan_token(&mut self) {
        self.start = self.current;
        self.start_byte_pos = self.current_byte_pos;
        if let Some(char) = self.peek() {
            self.next();
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
                "-" => match self.peek() {
                    Some(digit) => {
                        if is_digit(digit) {
                            self.parse_num(char)
                        } else {
                            self.create_token(TokenType::Minus)
                        }
                    }
                    _ => self.create_token(TokenType::Minus),
                },
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
                "_" => self.create_token(TokenType::Underscore),
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
                "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" => self.parse_num(char),
                _ => self.parse_identifier(char),
            }
        }
    }

    fn parse_identifier(&mut self, character: &str) {
        let mut chars = vec![character];
        while let Some(char) = self.peek().filter(|char| is_ident(char)) {
            chars.push(char);
            self.next();
        }

        let identifier = chars.join("");
        let token_type = KEYWORDS.get(&identifier).copied().unwrap_or_else(|| {
            let interned_str = InternedStr::new(&identifier);
            TokenType::Identifier(interned_str)
        });

        self.create_token(token_type);
    }

    fn parse_num(&mut self, char: &str) {
        let negative = char == "-";
        let mut tokens = vec![char];

        while let Some(char) = self.peek().filter(|char| is_digit(char)) {
            tokens.push(char);
            self.next();
        }

        if let Some(char) = self.peek().filter(|char| *char == ".") {
            if let Some(next) = self.peek_next().filter(|next| is_digit(next)) {
                tokens.push(char);
                self.next();

                while let Some(char) = self.peek().filter(|char| is_digit(char)) {
                    tokens.push(char);
                    self.next();
                }

                let str = tokens.join("");

                let token_type: TokenType = str
                    .parse::<f64>()
                    .map(TokenType::Float)
                    .unwrap_or_else(|_| TokenType::Unrecognized(InternedStr::new(&str)));

                self.create_token(token_type);
                return;
            }
        }

        let str = tokens.join("");

        let token_type = str
            .parse::<i64>()
            .map(TokenType::Int)
            .unwrap_or_else(|_| TokenType::Unrecognized(InternedStr::new(&str)));
        self.create_token(token_type);
    }

    fn parse_string(&mut self) {
        let mut tokens = vec![];
        while let Some(char) = self.peek().filter(|char| *char != "\"") {
            self.next();
            if is_line_break(char) {
                self.tokenized_file
                    .add_line_break(self.current_byte_pos as u32);
            }
            tokens.push(char);
        }

        if self.next() == "\"" {
            let string = tokens.join("");
            let interned_str = InternedStr::from(string);

            self.create_token(TokenType::String(interned_str))
        } else {
            self.create_unrecognized_token("Unterminated string.")
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(char) = self.peek() {
            match char {
                " " | "\t" => {
                    // TODO: Properly handle tab spacing for span reprinting
                    self.next();
                }
                "\r" | "\n" | "\r\n" => {
                    self.tokenized_file
                        .add_line_break(self.current_byte_pos as u32);
                    self.next();
                }
                "/" => {
                    if let Some("/") = self.peek_next() {
                        while let Some(char) = self.peek().filter(|char| !is_line_break(char)) {
                            self.next();
                        }
                    } else {
                        break;
                    }
                }
                _ => break,
            }
        }
    }

    fn next(&mut self) -> &str {
        let current_char = self.chars.get(self.current).unwrap();
        self.current += 1;
        self.current_byte_pos += current_char.len();
        current_char
    }

    fn peek(&mut self) -> Option<&'this str> {
        self.chars.get(self.current).copied()
    }

    fn peek_next(&mut self) -> Option<&'this str> {
        self.chars.get(self.current + 1).copied()
    }

    fn matches(&mut self, expected: &str) -> bool {
        if let Some(char) = self.peek().filter(|char| *char == expected) {
            self.next();
            true
        } else {
            false
        }
    }

    fn create_unrecognized_token(&mut self, error_message: &'static str) {
        let interned_error = InternedStr::from(error_message);
        self.create_token(TokenType::Unrecognized(interned_error));
    }

    fn create_token(&mut self, token_type: TokenType) {
        let token = Token::new(token_type, self.start, self.current);
        self.start = self.current;
        self.start_byte_pos = self.current_byte_pos;
        self.tokenized_file.tokens.push(token);
    }
}

fn is_digit(word: &str) -> bool {
    matches!(
        word,
        "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
    )
}

fn is_line_break(char: &str) -> bool {
    matches!(char, "\r" | "\r\n" | "\n")
}

fn is_ident(char: &str) -> bool {
    !matches!(
        char,
        "\r" | "\t"
            | "\r\n"
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

fn is_delimiter(char: &str) -> bool {
    matches!(char, " " | "\r" | "\t" | "\n" | "\r\n" | ";")
}

mod tests {
    use snap::snapshot;

    use crate::tokenized_file::TokenizedOutput;
    use crate::tokenizer::Tokenizer;

    #[cfg(test)]
    fn tokenize_str<T: AsRef<str>>(code: T) -> TokenizedOutput {
        let tokenizer = Tokenizer::new(code.as_ref());
        let tokens = tokenizer.tokenize();
        tokens
    }

    #[test]
    #[snapshot]
    pub fn simple_class() -> TokenizedOutput {
        tokenize_str("pub class Random {}")
    }

    #[test]
    #[snapshot]
    pub fn simple_enum() -> TokenizedOutput {
        tokenize_str("impl enum \n Reader \n [ ]")
    }

    #[test]
    #[snapshot]
    pub fn invalid_native_keyword() -> TokenizedOutput {
        tokenize_str("native nativer enative")
    }

    #[test]
    #[snapshot]
    pub fn simple_statement() -> TokenizedOutput {
        tokenize_str("use std::vector::Vector")
    }

    #[test]
    #[snapshot]
    pub fn parse_float_base_case() -> TokenizedOutput {
        tokenize_str("123.45")
    }

    #[test]
    #[snapshot]
    pub fn parse_float_with_preceding_whitespace() -> TokenizedOutput {
        tokenize_str(" 123.45")
    }

    #[test]
    #[snapshot]
    pub fn parse_positive_int() -> TokenizedOutput {
        tokenize_str("123")
    }

    #[test]
    #[snapshot]
    pub fn parse_negative_int() -> TokenizedOutput {
        tokenize_str("-123")
    }

    #[test]
    #[snapshot]
    pub fn parse_int_with_dot_after() -> TokenizedOutput {
        tokenize_str("123.")
    }

    #[test]
    #[snapshot]
    pub fn simple_expression() -> TokenizedOutput {
        tokenize_str("x = 123 >> 2 | 89 * 21 & 2")
    }

    #[test]
    #[snapshot]
    pub fn complex_function_composition() -> TokenizedOutput {
        tokenize_str("1 + 2 + f(g(h())) * 3 * 4")
    }

    #[test]
    #[snapshot]
    pub fn double_infix() -> TokenizedOutput {
        tokenize_str("--1 * 2")
    }

    #[test]
    #[snapshot]
    pub fn double_infix_call() -> TokenizedOutput {
        tokenize_str("--f(g)")
    }

    #[test]
    #[snapshot]
    pub fn simple_trait() -> TokenizedOutput {
        tokenize_str("trait Serializable { }")
    }

    #[test]
    #[snapshot]
    pub fn simple_iterator_trait() -> TokenizedOutput {
        let code = concat!(
            "trait Iterator<T> {\n",
            "     fn next() => T | None;\n",
            "}"
        );

        tokenize_str(code)
    }

    #[test]
    #[snapshot]
    pub fn generic_point_class() -> TokenizedOutput {
        tokenize_str("class Point<T, U>(x: T, y: U);")
    }

    #[test]
    #[snapshot]
    pub fn simple_main_stmt() -> TokenizedOutput {
        tokenize_str(concat!(
            "fn main(arguments: [str]) {\n",
            "    println(arguments.to_string());\n",
            "}"
        ))
    }

    #[test]
    #[snapshot]
    pub fn var_declarations() -> TokenizedOutput {
        tokenize_str(concat!("let mut x = None;\n", "let y = 0;"))
    }

    #[test]
    #[snapshot]
    pub fn uppercase_self() -> TokenizedOutput {
        tokenize_str("Self::lower_hir")
    }

    #[test]
    #[snapshot]
    pub fn parameter_parsing() -> TokenizedOutput {
        tokenize_str("fn mutate(mut self) => None;")
    }

    #[test]
    #[snapshot]
    pub fn bytearray_to_str() -> TokenizedOutput {
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
    pub fn empty_string() -> TokenizedOutput {
        tokenize_str("")
    }

    #[test]
    #[snapshot]
    pub fn small_a_string() -> TokenizedOutput {
        tokenize_str("a")
    }

    #[test]
    #[snapshot]
    pub fn scene_graph_node() -> TokenizedOutput {
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
    pub fn empty_class_with_traits() -> TokenizedOutput {
        tokenize_str("class SortedMap<T: Sortable + Hashable>;")
    }

    #[test]
    #[snapshot]
    pub fn enum_with_member_funcs() -> TokenizedOutput {
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
        tokenize_str(code)
    }

    #[test]
    #[snapshot]
    pub fn complex_enum() -> TokenizedOutput {
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
        tokenize_str(code)
    }

    #[test]
    #[snapshot]
    pub fn for_loop() -> TokenizedOutput {
        tokenize_str("for x in 0..100 { }")
    }

    #[test]
    #[snapshot]
    pub fn let_stmt_none() -> TokenizedOutput {
        tokenize_str("let x: None;")
    }

    #[test]
    #[snapshot]
    pub fn multiple_let_stmts() -> TokenizedOutput {
        let code = concat!(
            "let a: i64 = 1; // Immediate assignment\n",
            "let b = 2; // `i64` type is inferred\n"
        );
        tokenize_str(code)
    }

    #[test]
    #[snapshot]
    pub fn mutable_assignment() -> TokenizedOutput {
        let code = concat!(
            "fn mut_var() {\n",
            "    let mut x = 5; // `i64` type is inferred\n",
            "    x = x + 1;\n",
            "}"
        );
        tokenize_str(code)
    }

    #[test]
    #[snapshot]
    pub fn print_fn() -> TokenizedOutput {
        let code = concat!("fn print(text: str) {\n", "    println(text);\n", "}");
        tokenize_str(code)
    }
}
