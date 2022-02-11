use std::io::{ErrorKind, Read};
use crate::bytes::byte_reader::ByteReader;
use crate::bytes::from_bytes::FromBytes;
use crate::class::constant_pool::ConstantPoolEntry;
use crate::function::compiled_method::CompiledMethod;

pub struct CompiledTrait {
    package: ConstantPoolEntry,
    name: ConstantPoolEntry,
    methods: Box<[CompiledMethod]>,
}

impl FromBytes for CompiledTrait {
    fn load(byte_reader: &mut impl ByteReader) -> Result<Self, ErrorKind> {
        let package = ConstantPoolEntry::load(byte_reader)?;
        let name = ConstantPoolEntry::load(byte_reader)?;

        let methods = Box::<[CompiledMethod]>::load(byte_reader)?;

        Ok(Self {
            package,
            name,
            methods,
        })
    }
}

