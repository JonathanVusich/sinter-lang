use std::sync::Arc;
use lasso::ThreadedRodeo;

pub mod parser;
pub mod tokens;
pub mod types;
pub mod ast;

pub type StringInterner = Arc<ThreadedRodeo>;
