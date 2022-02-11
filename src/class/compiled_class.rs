use std::io::ErrorKind;
use crate::bytes::byte_reader::ByteReader;
use crate::bytes::from_bytes::FromBytes;
use crate::class::constant_pool::{ConstantPool, ConstantPoolEntry};
use crate::class::field::Field;
use crate::class::version::Version;
use crate::function::method::Method;
use crate::types::types::CompiledType;

#[derive(Clone)]
pub struct CompiledClass {
    version: Version,
    constant_pool: ConstantPool,

    package: ConstantPoolEntry,
    name: ConstantPoolEntry,

    fields: Box<[Field]>,
    reference_fields: Box<[Field]>,
    methods: Box<[Method]>,
}

impl CompiledClass {

    pub fn qualified_name(&self) -> String {
        let package_name = self.constant_pool.load_str(self.package);
        let class_name = self.constant_pool.load_str(self.name);

        package_name.to_string() + class_name
    }

    pub fn version(&self) -> Version {
        self.version
    }

    pub fn constant_pool(&self) -> &ConstantPool {
        &self.constant_pool
    }

    pub fn fields(&self) -> &[Field] {
        &*self.fields
    }

    pub fn reference_fields(&self) -> &[Field] {
        &*self.reference_fields
    }

    pub fn methods(&self) -> &[Method] {
        &*self.methods
    }
}

impl FromBytes for CompiledClass {

    fn load(byte_reader: &mut impl ByteReader) -> Result<Self, ErrorKind> {
        let version = Version::load(byte_reader)?;
        let constant_pool = ConstantPool::load(byte_reader)?;

        let package = ConstantPoolEntry::load(byte_reader)?;
        let name = ConstantPoolEntry::load(byte_reader)?;

        let fields = Box::<[Field]>::load(byte_reader)?;
        let
    }
}
