use crate::class::references::{InlineReference, Reference};
use crate::pointers::heap_pointer::HeapPointer;
use crate::pointers::pointer::Pointer;
use crate::values::value::Value;

#[derive(Debug, Eq, PartialEq)]
pub enum Field {
    REFERENCE(ReferenceField),
    INLINE(InlineField)
}

#[derive(Debug, Eq, PartialEq)]
pub struct ReferenceField {
    name: String,
    reference: Reference,
}

impl ReferenceField {
    
    pub fn new(name: String, reference: Reference) -> Self {
        Self {
            name, 
            reference,
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct InlineField {
    name: String,
    reference: InlineReference
}

impl InlineField {

    pub fn new(name: String, reference: InlineReference) -> Self {
        Self {
            name,
            reference,
        }
    }
}