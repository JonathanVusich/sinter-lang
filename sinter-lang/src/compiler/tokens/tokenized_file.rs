use std::fs;
use std::fs::File;
use std::io::{Read, Seek, SeekFrom};
use std::path::PathBuf;
use std::str::from_utf8;

use itertools::Itertools;
use serde::{Deserialize, Serialize};

use crate::compiler::tokens::token::Token;

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct TokenizedSource {
    pub(crate) tokens: Vec<Token>,
    pub(crate) line_map: LineMap,
    pub(crate) token_source: Source,
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct TokenizedOutput {
    pub(crate) tokens: Vec<Token>,
    pub(crate) line_map: LineMap,
    line_counter: u32,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub enum Source {
    Inline(String),
    Path(PathBuf),
}

impl Source {
    pub(crate) fn span(&self, span: &Span) -> Option<String> {
        match self {
            Source::Inline(string) => string
                .as_bytes()
                .get(span.start as usize..span.end as usize)
                .and_then(|slice| from_utf8(slice).ok())
                .map(|string| string.to_string()),
            Source::Path(path_buf) => {
                let mut file = File::open(path_buf).ok()?;

                let mut buffer = vec![0u8; span.len()];
                file.seek(SeekFrom::Start(span.start as u64)).ok()?;
                file.read_exact(&mut buffer).ok()?;
                String::from_utf8(buffer).ok()
            }
        }
    }

    pub(crate) fn line(&self, line_no: usize) -> Option<String> {
        match self {
            Source::Inline(string) => string
                .lines()
                .skip(line_no)
                .next()
                .map(|str| str.to_string()),
            Source::Path(path_buf) => fs::read_to_string(path_buf)
                .ok()
                .and_then(|str| str.lines().skip(line_no).next().map(|str| str.to_string())),
        }
    }
}

impl Default for Source {
    fn default() -> Self {
        Source::Inline("".to_string())
    }
}

#[derive(PartialEq, Default, Debug, Serialize, Deserialize)]
pub struct LineMap {
    lines: Vec<u32>,
}

impl LineMap {
    pub fn push_line(&mut self, line: u32) {
        self.lines.push(line);
    }

    pub fn with_lines(&self, span: Span) -> NormalizedSpan {
        let start_line = self
            .lines
            .partition_point(|&line_pos| line_pos <= span.start);
        let end_line = self.lines.partition_point(|&line_pos| line_pos <= span.end);

        let start_pos = if start_line > 0 {
            span.start - self.lines[start_line - 1]
        } else {
            span.start
        };

        let end_pos = if end_line > 0 {
            span.end - self.lines[end_line - 1]
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

#[derive(Eq, PartialEq, Default, Debug, Hash, Serialize, Deserialize, Copy, Clone)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

impl Span {
    pub fn new(start: u32, end: u32) -> Self {
        assert!(end > start);
        Self { start, end }
    }

    pub fn to(&self, other: Self) -> Self {
        Self {
            start: self.start,
            end: other.end,
        }
    }

    pub fn len(&self) -> usize {
        (self.end - self.start) as usize
    }
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
    use crate::compiler::tokens::tokenized_file::{NormalizedSpan, Span, TokenizedOutput};

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
