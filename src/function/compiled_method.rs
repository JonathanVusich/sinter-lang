use std::io::{BufRead, BufReader, ErrorKind};
use crate::bytes::byte_reader::ByteReader;
use crate::bytes::from_bytes::FromBytes;
use crate::class::constant_pool::ConstantPoolEntry;
use crate::types::types::{CompiledType, Type};

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct CompiledMethod {
    name: ConstantPoolEntry,
    descriptor: CompiledMethodDescriptor,
    max_stack_size: u16,
    max_locals: u16,
    code: Box<[u8]>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct CompiledMethodDescriptor {
    return_type: CompiledType,
    parameters: Box<[Type]>,
}

impl FromBytes for CompiledMethod {
    fn load(byte_reader: &mut impl ByteReader) -> Result<Self, ErrorKind> {
        let name = ConstantPoolEntry::load(byte_reader)?;
        let descriptor = CompiledMethodDescriptor::load(byte_reader)?;
        let max_stack_size = u16::load(byte_reader)?;
        let max_locals = u16::load(byte_reader)?;

        let code = Box::<[u8]>::load(byte_reader)?;

        Ok(Self {
            name,
            descriptor,
            max_stack_size,
            max_locals,
            code,
        })
    }
}

impl FromBytes for CompiledMethodDescriptor {
    fn load(byte_reader: &mut impl ByteReader) -> Self {
        let return_type = CompiledType::load(byte_reader);
        let parameters = Box::<[Type]>::load(byte_reader);

        Self {
            return_type,
            parameters,
        }
    }
}