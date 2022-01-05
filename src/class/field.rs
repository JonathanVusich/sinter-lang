use crate::class::references::{InlineReference, Reference};
use crate::pointers::heap_pointer::HeapPointer;
use crate::pointers::pointer::Pointer;

#[derive(Debug, Eq, PartialEq)]
pub enum Field {
    REFERENCE(ReferenceField),
    INLINE(InlineField)
}

#[derive(Debug, Eq, PartialEq)]
pub struct ReferenceField {
    name: &'static str,
    reference: Reference,
}

impl ReferenceField {
    
    pub fn new(name: &'static str, reference: Reference) -> Self {
        Self {
            name, 
            reference,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct InlineField {
    name: &'static str,
    reference: InlineReference
}

impl InlineField {

    pub fn new(name: &'static str, reference: InlineReference) -> Self {
        Self {
            name,
            reference,
        }
    }
}