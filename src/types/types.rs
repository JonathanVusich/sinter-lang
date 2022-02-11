use std::io::{ErrorKind, Read};
use crate::bytes::byte_reader::ByteReader;
use crate::bytes::from_bytes::FromBytes;
use crate::class::class::Class;
use crate::class::constant_pool::ConstantPoolEntry;
use crate::strings::internal_string::InternalString;

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum Type {
    Void,
    InlineClass(InternalString),
    Class(InternalString),
    Trait(InternalString),
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct CompiledType {
    base_type: CompiledBaseType,
    actual_type: ConstantPoolEntry,
}

#[repr(u8)]
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum CompiledBaseType {
    Void,
    InlineClass,
    Class,
    Trait,
}

impl FromBytes for CompiledType {
    fn load(byte_reader: &mut impl ByteReader) -> Result<Self, ErrorKind> {
        let base_type = CompiledBaseType::load(byte_reader)?;
        let actual_type = ConstantPoolEntry::load(byte_reader)?;

        Ok(Self {
            base_type,
            actual_type,
        })
    }
}

impl FromBytes for CompiledBaseType {
    fn load(byte_reader: &mut impl ByteReader) -> Result<Self, ErrorKind> {
        Ok(CompiledBaseType::from(u8::load(byte_reader)?))
    }
}

impl From<u8> for CompiledBaseType {
    fn from(byte: u8) -> Self {
        let opcode: CompiledBaseType;
        unsafe {
            opcode = std::mem::transmute::<u8, CompiledBaseType>(byte);
        }
        opcode
    }
}