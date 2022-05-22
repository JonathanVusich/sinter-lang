use string_interner::DefaultBackend;
use string_interner::symbol::{DefaultSymbol};

pub type Identifier = DefaultSymbol;

#[derive(Eq, PartialEq, Debug, Hash)]
pub enum Type {
    Array(Box<Type>),
    Enum(Identifier),
    Trait(Identifier),
    TraitBounds(Vec<Identifier>),
    Union(Vec<Type>),
    Class(Identifier),
    Basic(BasicType),
}

#[derive(Eq, PartialEq, Debug, Hash)]
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
    None
}