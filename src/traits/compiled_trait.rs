use std::io::{ErrorKind, Read};
use crate::bytes::serializers::ByteReader;
use crate::bytes::serializable::Serializable;
use crate::class::constant_pool::ConstantPoolEntry;
use crate::function::compiled_method::CompiledMethod;

pub struct CompiledTrait {
    package: ConstantPoolEntry,
    name: ConstantPoolEntry,
    methods: Box<[CompiledMethod]>,
}

impl Serializable for CompiledTrait {
    fn read(byte_reader: &mut impl ByteReader) -> Option<Self> {
        let package = ConstantPoolEntry::read(byte_reader)?;
        let name = ConstantPoolEntry::read(byte_reader)?;

        let methods = Box::<[CompiledMethod]>::read(byte_reader)?;

        Some(Self {
            package,
            name,
            methods,
        })
    }
}

