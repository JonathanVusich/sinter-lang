use crate::compiler::interner::Interner;
use lasso::{LargeSpur, Rodeo, Spur, ThreadedRodeo};
use rustc_hash::FxHasher;
use std::collections::hash_map::RandomState;
use std::hash::{BuildHasher, BuildHasherDefault};
use std::rc::Rc;
use std::sync::Arc;

pub mod ast;
pub mod ast_passes;
pub mod codegen;
pub mod compiled_module;
pub mod compiler;
pub mod hir;
pub mod interner;
pub mod krate;
pub mod mir;
pub mod parser;
pub mod path;
pub mod resolver;
pub mod tokens;
pub mod ty_infer;
pub mod types;
mod utils;
pub mod validator;

pub type SeedableHasher = BuildHasherDefault<FxHasher>;
pub type StringInterner = Rodeo;
