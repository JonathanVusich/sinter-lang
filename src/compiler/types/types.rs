use crate::compiler::ast::ast::QualifiedIdent;
use lasso::Spur;
use serde::{Deserialize, Serialize};

pub type InternedStr = Spur;

#[derive(Clone, Eq, PartialEq, Debug, Hash, Serialize, Deserialize)]
pub enum Type {
    Array(Box<Type>),
    Enum(QualifiedIdent),
    Trait(QualifiedIdent),
    TraitBounds(Vec<QualifiedIdent>),
    Union(Vec<Type>),
    Class(QualifiedIdent),
    Generic(QualifiedIdent),
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
    None,
}
