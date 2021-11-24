use crate::class::inline_field::InlineField;
use crate::class::reference_field::ReferenceField;
use crate::pointers::heap_pointer::HeapPointer;
use crate::pointers::pointer::Pointer;
use crate::values::value::Value;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Field {
    REFERENCE(ReferenceField),
    INLINE(InlineField)
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum FieldVisibility {
    PUBLIC,
    PRIVATE,
    PROTECTED,
    INTERNAL
}