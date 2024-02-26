use std::collections::HashMap;
use std::fs;
use std::fs::File;
use std::io::{Read, Seek, SeekFrom};
use std::path::PathBuf;
use std::str::from_utf8;

use id::ModuleId;
use serde::{Deserialize, Serialize};
use span::{NormalizedSpan, Span};

#[derive(PartialEq, Debug, Default, Serialize, Deserialize)]
pub struct SourceMap {
    map: HashMap<ModuleId, SourceCode>,
}

impl SourceMap {
    pub fn intern(&mut self, id: ModuleId, source: SourceCode) {
        assert!(self.map.insert(id, source).is_none());
    }

    pub fn source(&self, id: &ModuleId) -> &SourceCode {
        self.map.get(id).unwrap()
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub enum Source {
    Inline(String),
    Path(PathBuf),
}

impl Source {
    pub fn span(&self, span: &Span) -> Option<String> {
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

    pub fn line(&self, line_no: usize) -> Option<String> {
        match self {
            Source::Inline(string) => string.lines().nth(line_no).map(|str| str.to_string()),
            Source::Path(path_buf) => fs::read_to_string(path_buf)
                .ok()
                .and_then(|str| str.lines().nth(line_no).map(|str| str.to_string())),
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

#[derive(Debug, Serialize, Deserialize)]
pub struct SourceCode {
    #[serde(skip)]
    pub source: Source,
    pub line_map: LineMap,
}

// We need to exclude the source file on purpose because it is non-deterministic when representing a path.
impl PartialEq for SourceCode {
    fn eq(&self, other: &Self) -> bool {
        self.line_map == other.line_map
    }
}

impl SourceCode {
    pub fn new(source: Source, line_map: LineMap) -> Self {
        Self { source, line_map }
    }
}
