use lasso::{Spur, ThreadedRodeo};
use rustc_hash::FxHasher;
use std::hash::{BuildHasher, BuildHasherDefault};
use std::sync::Arc;

pub mod parser;
pub mod ast;
pub mod tokens;
pub mod types;
pub mod type_checker;
pub mod compiler;
pub mod compiled_module;

type SeedableHasher = BuildHasherDefault<FxHasher>;
pub type StringInterner = Arc<ThreadedRodeo>;
