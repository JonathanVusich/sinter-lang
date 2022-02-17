use std::io::{ErrorKind, Read};
use crate::bytes::serializers::ByteReader;
use crate::bytes::serializable::Serializable;
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
    pub base_type: CompiledBaseType,
    pub actual_type: ConstantPoolEntry,
}

#[repr(u8)]
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum CompiledBaseType {
    Void,
    InlineClass,
    Class,
    Trait,
}

impl Serializable for CompiledType {
    fn read(byte_reader: &mut impl ByteReader) -> Option<Self> {
        let base_type = CompiledBaseType::read(byte_reader)?;
        let actual_type = ConstantPoolEntry::read(byte_reader)?;

        Some(Self {
            base_type,
            actual_type,
        })
    }
}

impl Serializable for CompiledBaseType {
    fn read(byte_reader: &mut impl ByteReader) -> Option<Self> {
        Some(CompiledBaseType::from(u8::load(byte_reader)?))
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