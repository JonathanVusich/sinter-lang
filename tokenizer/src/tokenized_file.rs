use std::io::Seek;

use serde::{Deserialize, Serialize};

use source::{LineMap, Source};
use span::{NormalizedSpan, Span};

use crate::token::Token;

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct TokenizedSource {
    pub tokens: Vec<Token>,
    pub line_map: LineMap,
    pub token_source: Source,
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct TokenizedOutput {
    pub(crate) tokens: Vec<Token>,
    pub(crate) line_map: LineMap,
    line_counter: u32,
}

impl TokenizedOutput {
    pub fn new() -> Self {
        Self {
            tokens: Vec::new(),
            line_map: LineMap::default(),
            line_counter: 0,
        }
    }

    pub fn token_position(&self, span: Span) -> NormalizedSpan {
        self.line_map.with_lines(span)
    }

    pub fn add_line_break(&mut self, pos: u32) {
        self.line_map.push_line(pos);
        self.line_counter += 1;
    }
}

mod tests {
    use span::{NormalizedSpan, Span};

    use crate::tokenized_file::TokenizedOutput;

    #[test]
    pub fn line_map() {
        let mut tokenized_input = TokenizedOutput::new();

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
