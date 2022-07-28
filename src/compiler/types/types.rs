use lasso::Spur;

pub type Ident = Spur;

#[derive(Clone, Eq, PartialEq, Debug, Hash)]
pub enum Type {
    Array(Box<Type>),
    Enum(Ident),
    Trait(Ident),
    TraitBounds(Vec<Ident>),
    Union(Vec<Type>),
    Class(Ident),
    Generic(Ident),
    Basic(BasicType),
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
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
