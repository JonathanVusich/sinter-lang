use lasso::{Spur, ThreadedRodeo};
use rustc_hash::FxHasher;
use std::hash::{BuildHasher, BuildHasherDefault};
use std::sync::Arc;

pub mod ast;
pub mod parser;
pub mod tokens;
pub mod types;

type SeedableHasher = BuildHasherDefault<FxHasher>;
pub type StringInterner = Arc<ThreadedRodeo>;
