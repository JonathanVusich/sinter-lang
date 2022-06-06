use crate::bytes::serializable::Serializable;
use crate::bytes::serializers::{ByteReader, ByteWriter};
use crate::class::constant_pool::ConstantPoolEntry;
use crate::types::types::CompiledType;
use std::io::{BufRead, BufReader, Error, ErrorKind};

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct CompiledMethod {
    pub name: ConstantPoolEntry,
    pub descriptor: CompiledMethodDescriptor,
    pub param_size: u16,
    pub max_stack_size: u16,
    pub max_locals: u16,
    pub is_main_method: bool,
    pub code: Box<[u8]>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct CompiledMethodDescriptor {
    pub return_type: CompiledType,
    pub parameters: Box<[CompiledType]>,
}

impl Serializable for CompiledMethod {
    fn read(byte_reader: &mut impl ByteReader) -> Result<Self, Error> {
        let name = ConstantPoolEntry::read(byte_reader)?;
        let descriptor = CompiledMethodDescriptor::read(byte_reader)?;
        let param_size = u16::read(byte_reader)?;
        let max_stack_size = u16::read(byte_reader)?;
        let max_locals = u16::read(byte_reader)?;
        let is_main_method = bool::read(byte_reader)?;

        let code = Box::<[u8]>::read(byte_reader)?;

        Ok(Self {
            name,
            descriptor,
            param_size,
            max_stack_size,
            max_locals,
            is_main_method,
            code,
        })
    }

    fn write(&self, byte_writer: &mut impl ByteWriter) -> Result<(), Error> {
        self.name.write(byte_writer)?;
        self.descriptor.write(byte_writer)?;
        self.max_stack_size.write(byte_writer)?;
        self.max_locals.write(byte_writer)?;
        self.code.write(byte_writer)
    }
}

impl Serializable for CompiledMethodDescriptor {
    fn read(byte_reader: &mut impl ByteReader) -> Result<Self, Error> {
        let return_type = CompiledType::read(byte_reader)?;
        let parameters = Box::<[CompiledType]>::read(byte_reader)?;

        Ok(Self {
            return_type,
            parameters,
        })
    }

    fn write(&self, byte_writer: &mut impl ByteWriter) -> Result<(), Error> {
        self.return_type.write(byte_writer)?;
        self.parameters.write(byte_writer)
    }
}
