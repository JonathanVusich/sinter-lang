use std::io::ErrorKind;
use crate::bytes::serializers::ByteReader;
use crate::bytes::serializable::Serializable;
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
    
}

impl Serializable for CompiledClass {

    fn read(byte_reader: &mut impl ByteReader) -> Option<Self> {
        let version = Version::read(byte_reader)?;
        let constant_pool = ConstantPool::read(byte_reader)?;

        let package = ConstantPoolEntry::read(byte_reader)?;
        let name = ConstantPoolEntry::read(byte_reader)?;
        let size = u64::load(byte_reader)?;

        let fields = Box::<[CompiledField]>::read(byte_reader)?;
        let methods = Box::<[CompiledMethod]>::read(byte_reader)?;

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
