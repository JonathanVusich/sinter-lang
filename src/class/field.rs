use crate::class::constant_pool::ConstantPoolEntry;
use crate::class::references::{InlineReference, Reference};
use crate::pointers::heap_pointer::HeapPointer;
use crate::pointers::pointer::Pointer;

#[derive(Copy, Clone)]
pub struct Field {
    name: ConstantPoolEntry,
    descriptor: ConstantPoolEntry,
    offset: u64,
    size: u64,
}