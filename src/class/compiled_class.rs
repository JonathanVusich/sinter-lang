use crate::class::constant_pool::{ConstantPool, ConstantPoolEntry};
use crate::class::field::Field;
use crate::class::version::Version;
use crate::function::method::Method;

#[derive(Clone)]
pub struct CompiledClass {
    version: Version,
    constant_pool: ConstantPool,

    package: ConstantPoolEntry,
    name: ConstantPoolEntry,

    fields: Vec<Field>,
    reference_fields: Vec<Field>,
    methods: Vec<Method>,
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
        self.fields.as_slice()
    }

    pub fn reference_fields(&self) -> &[Field] {
        self.reference_fields.as_slice()
    }

    pub fn methods(&self) -> &[Method] {
        self.methods.as_slice()
    }
}
