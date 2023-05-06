use crate::compiler::tokens::token::Token;
use serde::{Deserialize, Serialize};
use std::path::Path;

#[derive(Debug, Default, Serialize, Deserialize, PartialEq)]
pub struct TokenizedInput {
    pub(crate) tokens: Vec<Token>,
    pub(crate) line_map: Vec<u32>,
    line_counter: u32,
}

#[derive(PartialEq, Default, Debug, Serialize, Deserialize)]
pub struct NormalizedSpan {
    pub start_pos: u32,
    pub start_line: u32,
    pub end_pos: u32,
    pub end_line: u32,
}

impl NormalizedSpan {
    pub fn new(start_pos: u32, start_line: u32, end_pos: u32, end_line: u32) -> Self {
        Self {
            start_pos,
            start_line,
            end_pos,
            end_line,
        }
    }
}

#[derive(Eq, PartialEq, Debug, Hash, Serialize, Deserialize, Copy, Clone)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

impl Span {
    pub fn new(start: u32, end: u32) -> Self {
        Self { start, end }
    }

    pub fn to(&self, other: Self) -> Self {
        Self {
            start: self.start,
            end: other.end,
        }
    }
}

impl Default for Span {
    fn default() -> Self {
        Self { start: 0, end: 0 }
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

    pub fn token_position(&self, span: Span) -> NormalizedSpan {
        let start_line = self
            .line_map
            .partition_point(|&line_pos| line_pos <= span.start);
        let end_line = self
            .line_map
            .partition_point(|&line_pos| line_pos <= span.end);

        let start_pos = if start_line > 0 {
            span.start - self.line_map[start_line - 1]
        } else {
            span.start
        };

        let end_pos = if end_line > 0 {
            span.end - self.line_map[end_line - 1]
        } else {
            span.end
        };

        NormalizedSpan::new(
            start_pos,
            start_line as u32 + 1,
            end_pos,
            end_line as u32 + 1,
        )
    }

    pub fn add_line_break(&mut self, pos: u32) {
        self.line_map.push(pos);
        self.line_counter += 1;
    }
}

mod tests {
    use crate::compiler::tokens::tokenized_file::{NormalizedSpan, Span, TokenizedInput};

    #[test]
    pub fn line_map() {
        let mut tokenized_input = TokenizedInput::new();

        tokenized_input.add_line_break(2);
        tokenized_input.add_line_break(5);

        assert_eq!(
            NormalizedSpan::new(0, 1, 1, 1),
            tokenized_input.token_position(Span::new(0, 1))
        );
        assert_eq!(
            NormalizedSpan::new(1, 1, 0, 2),
            tokenized_input.token_position(Span::new(1, 2))
        );
    }
}
