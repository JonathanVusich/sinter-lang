use string_interner::symbol::DefaultSymbol;
use string_interner::DefaultBackend;

pub type Ident = DefaultSymbol;

#[derive(Eq, PartialEq, Debug, Hash)]
pub enum Type {
    Array(Box<Type>),
    Enum(Ident),
    Trait(Ident),
    TraitBounds(Vec<Ident>),
    Union(Vec<Type>),
    Class(Ident),
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
    None,
}
