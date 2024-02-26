use std::hash::{BuildHasher, BuildHasherDefault};

use rustc_hash::FxHasher;

pub mod compiler;
pub mod mir;
pub mod resolver;
mod type_inference;

pub type SeedableHasher = BuildHasherDefault<FxHasher>;
