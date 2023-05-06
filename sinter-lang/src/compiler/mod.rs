use crate::compiler::interner::Interner;
use lasso::{LargeSpur, Rodeo, Spur, ThreadedRodeo};
use rustc_hash::FxHasher;
use std::collections::hash_map::RandomState;
use std::hash::{BuildHasher, BuildHasherDefault};
use std::rc::Rc;
use std::sync::Arc;

pub mod ast;
pub mod codegen;
pub mod compiled_module;
pub mod compiler;
pub mod interner;
pub mod parser;
pub mod tokens;
pub mod ty_checker;
pub mod types;
pub mod hir;
pub mod path;

pub type SeedableHasher = BuildHasherDefault<FxHasher>;
pub type StringInterner = Rodeo;
