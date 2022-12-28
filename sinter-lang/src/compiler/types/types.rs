use crate::compiler::ast::ast::{PathTy, QualifiedIdent, TraitBound};
use lasso::Spur;
use serde::{Deserialize, Serialize};

pub type InternedStr = Spur;

#[derive(Clone, Eq, PartialEq, Debug, Hash, Serialize, Deserialize)]
pub enum Type {
    Array(Box<Type>),
    Path(PathTy),
    Union(Vec<Type>),
    TraitBound(TraitBound),
    Closure(Vec<Type>, Box<Type>),
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
