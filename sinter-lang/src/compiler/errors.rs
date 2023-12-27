use crate::compiler::hir::ModuleId;
use crate::compiler::tokens::tokenized_file::{Span, TokenSource};
use serde::{Deserialize, Serialize};
use std::fmt::format;
use std::io;
use std::io::Write;
use std::path::PathBuf;

#[derive(PartialEq, Debug, Default, Serialize, Deserialize)]
pub struct Diagnostics {
    diagnostics: Vec<Diagnostic>,
}

impl Diagnostics {
    pub fn emit(&mut self, error: Diagnostic) {
        self.diagnostics.push(error);
    }

    pub fn write_all(&self, buffer: &mut impl Write) -> io::Result<()> {
        for diagnostic in &self.diagnostics {
            let formatted_args = match diagnostic {
                Diagnostic::IOError(_) => {}
                Diagnostic::Error() => {}
            };
            buffer.write_fmt(formatted_args)?;
        }
        Ok(())
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub enum Diagnostic {
    IOError(PathBuf),
    Error {
        token_source: TokenSource,
        highlight_span: Span, // Local to the line with the error.
        error_msg: String,
    },
}
