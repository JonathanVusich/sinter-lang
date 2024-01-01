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
    has_error: bool,
}

impl Diagnostics {
    pub fn emit(&mut self, error: Diagnostic) {
        if let Diagnostic::Error { .. } = error {
            self.has_error = true;
        }
        self.diagnostics.push(error);
    }

    pub fn errors(&self) -> Vec<&Diagnostic> {
        self.diagnostics
            .iter()
            .filter(|diagnostic| diagnostic.is_error())
            .collect()
    }

    pub fn has_errors(&self) -> bool {
        self.has_error
    }

    pub fn write_all(&self, buffer: &mut impl Write) -> io::Result<()> {
        for diagnostic in &self.diagnostics {
            let formatted_args = match diagnostic {
                Diagnostic::Fatal(_) => {
                    format_args!("")
                }
                Diagnostic::Error { .. } => {
                    format_args!("")
                }
                Diagnostic::BlankError => {
                    format_args!("")
                }
            };
            buffer.write_fmt(formatted_args)?;
        }
        Ok(())
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub enum InternalError {
    Panic,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub enum FatalError {
    InvalidOsStr,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub enum Diagnostic {
    Internal(InternalError),
    Fatal(FatalError),
    Error {
        line: String,
        highlight_span: Span, // Local to the line with the error.
        error_msg: String,
    },
    BlankError,
}

impl Diagnostic {
    fn is_error(&self) -> bool {
        true
    }
}
