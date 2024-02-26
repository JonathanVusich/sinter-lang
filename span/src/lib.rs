use serde::{Deserialize, Serialize};

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
