use std::io::{Error, ErrorKind};
use crate::bytes::serializers::{ByteReader, ByteWriter};
use crate::bytes::serializable::Serializable;
use crate::class::constant_pool::{ConstantPool, ConstantPoolEntry};
use crate::class::field::{CompiledField, Field};
use crate::class::version::Version;
use crate::function::compiled_method::CompiledMethod;
use crate::function::method::Method;
use crate::types::types::CompiledType;

use super::class::Class;

#[derive(Clone)]
pub struct CompiledClass {
    pub version: Version,
    pub constant_pool: ConstantPool,

    pub package: ConstantPoolEntry,
    pub name: ConstantPoolEntry,

    pub size: u64,

    pub fields: Box<[CompiledField]>,
    pub methods: Box<[CompiledMethod]>,
}

impl Serializable for CompiledClass {

    fn read(byte_reader: &mut impl ByteReader) -> Result<Self, Error> {
        let version = Version::read(byte_reader)?;
        let constant_pool = ConstantPool::read(byte_reader)?;

        let package = ConstantPoolEntry::read(byte_reader)?;
        let name = ConstantPoolEntry::read(byte_reader)?;
        let size = u64::read(byte_reader)?;

        let fields = Box::<[CompiledField]>::read(byte_reader)?;
        let methods = Box::<[CompiledMethod]>::read(byte_reader)?;

        Ok(Self {
            version,
            constant_pool,
            package,
            name,
            size,
            fields,
            methods,
        })
    }

    fn write(&self, byte_writer: &mut impl ByteWriter) -> Result<(), Error> {
        self.version.write(byte_writer)?;
        self.constant_pool.write(byte_writer)?;
        self.package.write(byte_writer)?;
        self.name.write(byte_writer)?;
        self.size.write(byte_writer)?;
        self.fields.write(byte_writer)?;
        self.methods.write(byte_writer)
    }
}
