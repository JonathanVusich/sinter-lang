use crate::class::version::Version;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};

#[derive(Debug, Copy, Clone)]
pub struct VMError {
    error_kind: VMErrorKind,
}

#[derive(Debug, Copy, Clone)]
pub enum VMErrorKind {
    UnsupportedClassVersion,
    MalformedModuleFile,
    MissingMainClass,
    MissingMainMethod,
}

impl VMError {
    pub fn new(error_kind: VMErrorKind) -> Self {
        Self { error_kind }
    }
}

impl Display for VMError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "VMError({:?})", self.error_kind)
    }
}

impl Error for VMError {}
