use std::collections::hash_map::RandomState;
use lasso::{LargeSpur, Rodeo, Spur, ThreadedRodeo};
use rustc_hash::FxHasher;
use std::hash::{BuildHasher, BuildHasherDefault};
use std::rc::Rc;
use std::sync::Arc;
use crate::compiler::interner::Interner;
use crate::compiler::types::types::Type;

pub mod parser;
pub mod ast;
pub mod tokens;
pub mod types;
pub mod type_checker;
pub mod compiler;
pub mod compiled_module;
mod interner;
pub mod resolver;
pub mod ty_checker;
pub mod codegen;

pub type SeedableHasher = BuildHasherDefault<FxHasher>;
pub type StringInterner = Rodeo;
pub type TyInterner = Interner<Type>;
