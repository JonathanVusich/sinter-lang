use std::io::{Error, ErrorKind};
use crate::bytes::serializers::{ByteReader, ByteWriter};
use crate::bytes::serializable::Serializable;
use crate::class::constant_pool::ConstantPoolEntry;
use crate::class::references::{InlineReference, Reference};
use crate::pointers::heap_pointer::HeapPointer;
use crate::pointers::pointer::Pointer;
use crate::pool::internal_string::InternalString;
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

    pub fn new(name: ConstantPoolEntry,
               type_descriptor: CompiledType,
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

impl Serializable for CompiledField {

    fn read(byte_reader: &mut impl ByteReader) -> Result<Self, Error> {
        let name = ConstantPoolEntry::read(byte_reader)?;
        let type_descriptor = CompiledType::read(byte_reader)?;
        let offset = u64::read(byte_reader)?;
        let size = u64::read(byte_reader)?;

        Ok(Self {
            name,
            type_descriptor,
            offset,
            size,
        })
    }

    fn write(&self, byte_writer: &mut impl ByteWriter) -> Result<(), Error> {
        self.name.write(byte_writer)?;
        self.type_descriptor.write(byte_writer)?;
        self.offset.write(byte_writer)?;
        self.size.write(byte_writer)
    }
}
