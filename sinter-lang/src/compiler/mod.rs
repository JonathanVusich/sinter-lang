use std::collections::hash_map::RandomState;
use lasso::{Rodeo, Spur, ThreadedRodeo};
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

type SeedableHasher = BuildHasherDefault<FxHasher>;
pub type StringInterner = Interner<String>;
pub type TyInterner = Interner<Type>;
