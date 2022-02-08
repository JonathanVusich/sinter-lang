use crate::class::accessibility::Accessibility;
use crate::class::constant_pool::ConstantPool;
use crate::class::version::Version;
use crate::function::method::Method;
use crate::interface::interface::Interface;

pub struct CompiledClass {
    version: Version,
    constant_pool: ConstantPool,
    accessibility: Accessibility,
    interfaces: Vec<Interface>,
    methods: Vec<Method>,
}