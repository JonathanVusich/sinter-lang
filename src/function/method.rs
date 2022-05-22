use std::io::ErrorKind;
use crate::bytes::serializers::ByteReader;
use crate::bytes::serializable::Serializable;
use crate::class::compiled_class::CompiledClass;
use crate::class::constant_pool::{ConstantPool, ConstantPoolEntry};
use crate::compiler::types::types::Type;
use crate::function::compiled_method::CompiledMethodDescriptor;
use crate::pool::internal_string::InternalString;
use crate::pool::string_pool::StringPool;

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Method {
    pub name: InternalString,
    pub param_size: u16,
    pub max_stack_size: u16,
    pub max_locals: u16,
    pub is_main_method: bool,
    pub code: Box<[u8]>,
}

impl Method {

    pub fn new(name: InternalString,
               param_size: u16,
               max_stack_size: u16,
               max_locals: u16,
               is_main_method: bool,
               code: Box<[u8]>) -> Self {
        Self {
            name,
            param_size,
            max_stack_size,
            max_locals,
            is_main_method,
            code,
        }
    }
}