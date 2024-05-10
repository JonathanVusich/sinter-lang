use std::cell::RefCell;
use std::io::Write;

use serde::{Deserialize, Serialize};

#[derive(PartialEq, Debug, Default, Clone, Serialize, Deserialize)]
pub struct Diagnostics {
    diagnostics: RefCell<Vec<Diagnostic>>,
}

impl From<Vec<Diagnostic>> for Diagnostics {
    fn from(diagnostics: Vec<Diagnostic>) -> Self {
        Self {
            diagnostics: RefCell::new(diagnostics),
        }
    }
}

impl Diagnostics {
    pub fn push(&self, error: Diagnostic) {
        self.diagnostics.borrow_mut().push(error);
    }

    pub fn is_empty(&self) -> bool {
        self.diagnostics.borrow().is_empty()
    }

    pub fn filter(&self, kind: DiagnosticKind) -> Vec<Diagnostic> {
        self.diagnostics
            .borrow()
            .iter()
            .filter(move |diagnostic| diagnostic.kind() == kind)
            .cloned()
            .collect()
    }

    pub fn flush(&self, kind: DiagnosticKind, buffer: &mut impl Write) -> bool {
        let mut flushed = false;
        let borrowed_diagnostics = self.diagnostics.borrow();
        for diagnostic in borrowed_diagnostics.iter() {
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
