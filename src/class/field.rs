use std::io::ErrorKind;
use crate::bytes::byte_reader::ByteReader;
use crate::bytes::from_bytes::FromBytes;
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

impl Field {

    pub fn name(&self) -> InternalString {
        self.name
    }

    pub fn descriptor(&self) -> Type {
        self.descriptor
    }

    pub fn offset(&self) -> u64 {
        self.offset
    }

    pub fn size(&self) -> u64 {
        self.size
    }
}
