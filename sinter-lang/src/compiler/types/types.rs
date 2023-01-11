use crate::compiler::ast::{PathTy, QualifiedIdent, TraitBound};
use lasso::Spur;
use serde::{Deserialize, Serialize};
use crate::compiler::interner::Key;

pub type InternedStr = Spur;

#[derive(Clone, Eq, PartialEq, Debug, Hash, Serialize, Deserialize)]
pub enum Type {
    Array(Key),
    Path(PathTy),
    Union(Vec<Key>),
    TraitBound(TraitBound),
    Closure(Vec<Key>, Key),
    Infer,
    QSelf,
    Basic(BasicType),
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash, Serialize, Deserialize)]
pub enum BasicType {
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
    F32,
    F64,
    Str,
    None,
}
