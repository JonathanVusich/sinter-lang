use crate::bytes::serializable::Serializable;
use crate::bytes::serializers::{ByteReader, ByteWriter};
use crate::class::class::Class;
use crate::class::constant_pool::ConstantPoolEntry;
use crate::pool::internal_string::InternalString;
use std::io::{Error, ErrorKind, Read};

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
    fn read(byte_reader: &mut impl ByteReader) -> Result<Self, Error> {
        let base_type = CompiledBaseType::read(byte_reader)?;
        let actual_type = ConstantPoolEntry::read(byte_reader)?;

        Ok(Self {
            base_type,
            actual_type,
        })
    }

    fn write(&self, byte_writer: &mut impl ByteWriter) -> Result<(), Error> {
        self.base_type.write(byte_writer)?;
        self.actual_type.write(byte_writer)
    }
}

impl Serializable for CompiledBaseType {
    fn read(byte_reader: &mut impl ByteReader) -> Result<Self, Error> {
        Ok(CompiledBaseType::from(u8::read(byte_reader)?))
    }

    fn write(&self, byte_writer: &mut impl ByteWriter) -> Result<(), Error> {
        u8::from(*self).write(byte_writer)
    }
}

impl From<u8> for CompiledBaseType {
    fn from(byte: u8) -> Self {
        unsafe { std::mem::transmute::<u8, CompiledBaseType>(byte) }
    }
}

impl From<CompiledBaseType> for u8 {
    fn from(base_type: CompiledBaseType) -> Self {
        unsafe { std::mem::transmute::<CompiledBaseType, u8>(base_type) }
    }
}
