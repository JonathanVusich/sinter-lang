use string_interner::symbol::SymbolUsize;

type Identifier = SymbolUsize;

#[derive(Copy, Clone, Debug)]
pub struct Type<'ctx> {
    type_description: &'ctx TypeDescription,
}

impl<'ctx> Type<'ctx> {

    pub fn new(type_description: &'ctx TypeDescription) -> Self {
        Self {
            type_description,
        }
    }
}

impl<'ctx> Eq for Type<'ctx> {

}

impl<'ctx> PartialEq<Self> for Type<'ctx> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.type_description, other.type_description)
    }
}

#[derive(Eq, PartialEq, Debug)]
pub enum TypeDescription {
    Array(Box<TypeDescription>),
    Enum(Identifier),
    Trait(Identifier),
    TraitBounds(Vec<Identifier>),
    Union(Vec<TypeDescription>),
    Class(Identifier),
    Basic(BasicType),
}

#[derive(Eq, PartialEq, Debug)]
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