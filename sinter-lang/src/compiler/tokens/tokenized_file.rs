use crate::compiler::tokens::token::Token;
use serde::{Deserialize, Serialize};

#[derive(Debug, Default, Serialize, Deserialize, PartialEq)]
pub struct TokenizedInput {
    pub(crate) tokens: Vec<Token>,
    pub(crate) line_map: Vec<usize>,
    line_counter: usize,
}

#[derive(Eq, PartialEq, Debug)]
pub struct TokenSpan {
    pub line: usize,
    pub pos: usize,
}

#[derive(Eq, PartialEq, Debug, Hash, Serialize, Deserialize, Copy, Clone)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {

    pub fn new(start: usize, end: usize) -> Self {
        Self {
            start,
            end,
        }
    }
}

impl TokenizedInput {
    pub fn new() -> Self {
        Self {
            tokens: Vec::new(),
            line_map: Vec::new(),
            line_counter: 0,
        }
    }

    pub fn token_position(&self, pos: usize) -> TokenSpan {
        let line = self.line_map.partition_point(|&line_pos| line_pos <= pos);

        let line_pos = if line > 0 {
            pos - self.line_map[line - 1]
        } else {
            pos
        };
        TokenSpan::new(line + 1, line_pos)
    }

    pub fn add_line_break(&mut self, pos: usize) {
        self.line_map.push(pos);
        self.line_counter += 1;
    }
}

impl TokenSpan {
    pub fn new(line: usize, pos: usize) -> Self {
        Self { line, pos }
    }
}

mod tests {
    use crate::compiler::tokens::tokenized_file::{TokenSpan, TokenizedInput};

    #[test]
    pub fn line_map() {
        let mut tokenized_input = TokenizedInput::new();

        tokenized_input.add_line_break(2);
        tokenized_input.add_line_break(5);

        assert_eq!(TokenSpan::new(1, 0), tokenized_input.token_position(0));
        assert_eq!(TokenSpan::new(1, 1), tokenized_input.token_position(1));
        assert_eq!(TokenSpan::new(2, 0), tokenized_input.token_position(2));
        assert_eq!(TokenSpan::new(2, 1), tokenized_input.token_position(3));
        assert_eq!(TokenSpan::new(2, 2), tokenized_input.token_position(4));
        assert_eq!(TokenSpan::new(3, 0), tokenized_input.token_position(5));
    }
}
