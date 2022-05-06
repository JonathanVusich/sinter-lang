use std::collections::BTreeMap;
use crate::compiler::token::Token;
use crate::compiler::token::TokenType::Pub;

#[derive(Debug)]
pub struct TokenizedFile {
    tokens: Vec<Token>,
    line_map: Vec<usize>,
    line_counter: usize,
}

impl TokenizedFile {

    pub fn new() -> Self {
        Self {
            tokens: Vec::new(),
            line_map: Vec::new(),
            line_counter: 0,
        }
    }

    pub fn tokens(&self) -> &[Token] {
        &self.tokens
    }

    pub fn line_for_pos(&self, pos: usize) -> usize {
        self.line_map.partition_point(|&line_pos| line_pos <= pos)
    }

    pub fn add_line_break(&mut self, pos: usize) {
        self.line_map.push(pos);
        self.line_counter += 1;
    }

    pub fn add_token(&mut self, token: Token) {
        self.tokens.push(token);
    }
}

mod tests {
    use crate::compiler::tokenized_file::TokenizedFile;

    #[test]
    pub fn line_map() {
        let mut tokenized_file = TokenizedFile::new();

        tokenized_file.add_line_break(2);
        tokenized_file.add_line_break(5);

        assert_eq!(0, tokenized_file.line_for_pos(0));
        assert_eq!(0, tokenized_file.line_for_pos(1));
        assert_eq!(1, tokenized_file.line_for_pos(2));
        assert_eq!(1, tokenized_file.line_for_pos(3));
        assert_eq!(1, tokenized_file.line_for_pos(4));
        assert_eq!(2, tokenized_file.line_for_pos(5));
    }
}