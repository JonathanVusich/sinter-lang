use std::io::{Error, ErrorKind, Read};
use crate::bytes::serializers::{ByteReader, ByteWriter};
use crate::bytes::serializable::Serializable;
use crate::class::constant_pool::ConstantPoolEntry;
use crate::function::compiled_method::CompiledMethod;

pub struct CompiledTrait {
    package: ConstantPoolEntry,
    name: ConstantPoolEntry,
    methods: Box<[CompiledMethod]>,
}

impl Serializable for CompiledTrait {

    fn read(byte_reader: &mut impl ByteReader) -> Result<Self, Error> {
        let package = ConstantPoolEntry::read(byte_reader)?;
        let name = ConstantPoolEntry::read(byte_reader)?;

        let methods = Box::<[CompiledMethod]>::read(byte_reader)?;

        Ok(Self {
            package,
            name,
            methods,
        })
    }

    fn write(&self, byte_writer: &mut impl ByteWriter) -> Result<(), Error> {
        self.package.write(byte_writer)?;
        self.name.write(byte_writer)?;
        self.methods.write(byte_writer)
    }
}

