use std::sync::Arc;
use lasso::ThreadedRodeo;

pub mod ast;
pub mod parser;
pub mod tokens;
pub mod types;

pub type StringInterner = Arc<ThreadedRodeo>;
