use crate::class::constant_pool::ConstantPoolEntry;
use crate::class::references::{InlineReference, Reference};
use crate::pointers::heap_pointer::HeapPointer;
use crate::pointers::pointer::Pointer;
use crate::strings::internal_string::InternalString;
use crate::types::types::Type;

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct Field {
    name: InternalString,
    descriptor: Type,
    offset: u64,
    size: u64,
}
