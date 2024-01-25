use crate::bytes::serializers::ByteWriter;
use crate::compiler::hir::ModuleId;
use crate::compiler::tokens::tokenized_file::{Source, Span};
use serde::{Deserialize, Serialize};
use std::error::Error;
use std::fmt::format;
use std::io;
use std::io::Write;
use std::path::PathBuf;

#[derive(PartialEq, Debug, Default, Serialize, Deserialize)]
pub struct Diagnostics {
    diagnostics: Vec<Diagnostic>,
}

impl Diagnostics {
    pub fn push(&mut self, error: Diagnostic) {
        self.diagnostics.push(error);
    }

    pub fn filter(&self, kind: DiagnosticKind) -> impl Iterator<Item = Diagnostic> + '_ {
        self.diagnostics
            .iter()
            .filter(move |diagnostic| diagnostic.kind() == kind)
            .cloned()
    }

    pub fn flush(&self, kind: DiagnosticKind, buffer: &mut impl Write) -> bool {
        let mut flushed = false;
        for diagnostic in &self.diagnostics {
            if diagnostic.kind() == kind {
                diagnostic.write(buffer);
                flushed = true;
            }
        }
        flushed
    }
}

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
pub enum FatalError {
    FileOpen,
    InvalidOsStr,
}

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
pub enum Diagnostic {
    Fatal(FatalError),
    Error(String),
    BlankError,
}

#[derive(PartialEq, Debug, Clone, Serialize, Deserialize)]
pub enum DiagnosticKind {
    Error,
    Warning,
    Lint,
}

impl Diagnostic {
    fn write(&self, buffer: &mut impl Write) {
        let output = match self {
            Diagnostic::Fatal(fatal) => match fatal {
                FatalError::FileOpen => "Could not open file!",
                FatalError::InvalidOsStr => "Invalid string!",
            },
            Diagnostic::Error(error) => error,
            Diagnostic::BlankError => "Blank error, need to add more diagnostics",
        };
        buffer.write_all(output.as_bytes()).ok().unwrap()
    }

    fn kind(&self) -> DiagnosticKind {
        match self {
            Diagnostic::Fatal(_) => DiagnosticKind::Error,
            Diagnostic::Error { .. } => DiagnosticKind::Error,
            Diagnostic::BlankError => DiagnosticKind::Error,
        }
    }
}
