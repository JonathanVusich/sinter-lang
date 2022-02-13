use std::io::ErrorKind;
use crate::bytes::byte_reader::ByteReader;
use crate::bytes::from_bytes::FromBytes;
use crate::class::constant_pool::{ConstantPool, ConstantPoolEntry};
use crate::class::field::{CompiledField, Field};
use crate::class::version::Version;
use crate::function::compiled_method::CompiledMethod;
use crate::function::method::Method;
use crate::types::types::CompiledType;

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

impl CompiledClass {

    pub fn qualified_name(&self) -> String {
        let package_name = self.constant_pool.load_str(self.package);
        let class_name = self.constant_pool.load_str(self.name);

        package_name.to_string() + class_name
    }
}

impl FromBytes for CompiledClass {

    fn load(byte_reader: &mut impl ByteReader) -> Option<Self> {
        let version = Version::load(byte_reader)?;
        let constant_pool = ConstantPool::load(byte_reader)?;

        let package = ConstantPoolEntry::load(byte_reader)?;
        let name = ConstantPoolEntry::load(byte_reader)?;
        let size = u64::load(byte_reader)?;

        let fields = Box::<[CompiledField]>::load(byte_reader)?;
        let methods = Box::<[CompiledMethod]>::load(byte_reader)?;

        Some(Self {
            version,
            constant_pool,
            package,
            name,
            size,
            fields,
            methods,
        })
    }
}
