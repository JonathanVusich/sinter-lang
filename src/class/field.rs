use std::io::ErrorKind;
use crate::bytes::byte_reader::ByteReader;
use crate::bytes::from_bytes::FromBytes;
use crate::class::constant_pool::ConstantPoolEntry;
use crate::class::references::{InlineReference, Reference};
use crate::pointers::heap_pointer::HeapPointer;
use crate::pointers::pointer::Pointer;
use crate::strings::internal_string::InternalString;
use crate::types::types::{CompiledType, Type};

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct Field {
    pub name: InternalString,
    pub type_descriptor: Type,
    pub offset: u64,
    pub size: u64,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct CompiledField {
    pub name: ConstantPoolEntry,
    pub type_descriptor: CompiledType,
    pub offset: u64,
    pub size: u64,
}

impl Field {

    pub fn new(name: InternalString, 
               type_descriptor: Type, 
               offset: u64,
               size: u64,) -> Self {
        Self {
            name,
            type_descriptor,
            offset,
            size,
        }
    }
}

impl CompiledField {

    pub fn new(name: ConstantPoolEntry, type_descriptor: CompiledType, offset: u64) -> Self {
        Self {
            name,
            type_descriptor,
            offset,
        }
    }
}

impl FromBytes for CompiledField {

    fn load(byte_reader: &mut impl ByteReader) -> Option<Self> {
        let name = ConstantPoolEntry::load(byte_reader)?;
        let type_descriptor = CompiledType::load(byte_reader)?;
        let offset = u64::load(byte_reader)?;
        let size = u64::load(byte_reader)?;

        Some(Self {
            name,
            type_descriptor,
            offset,
            size,
        })
    }
}
