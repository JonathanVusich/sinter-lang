use std::io::ErrorKind;
use crate::bytes::serializers::ByteReader;
use crate::bytes::serializable::Serializable;
use crate::class::compiled_class::CompiledClass;
use crate::class::constant_pool::{ConstantPool, ConstantPoolEntry};
use crate::function::compiled_method::CompiledMethodDescriptor;
use crate::strings::internal_string::InternalString;
use crate::strings::string_pool::StringPool;
use crate::types::types::Type;

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Method {
    name: InternalString,
    descriptor: MethodDescriptor,
    max_stack_size: u16,
    max_locals: u16,
    code: Box<[u8]>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct MethodDescriptor {
    return_type: Type,
    parameters: Box<[Type]>,
}

impl Method {

    pub fn new(name: InternalString, 
               descriptor: MethodDescriptor,
               max_stack_size: u16,
               max_locals: u16,
               code: Box<[u8]>) -> Self {
        Self {
            name,
            descriptor,
            max_stack_size,
            max_locals,
            code,
        }
    }
    
    pub fn is_main_method(&self, pool: &StringPool) -> bool {
        let method_name = pool.lookup(self.name);
        method_name == "main" && self.descriptor.return_type == Type::Void
    }
}

impl MethodDescriptor {
    
    pub fn new(return_type: Type, parameters: Box<[Type]>) -> Self {
        Self {
            return_type,
            parameters,
        }
    }
}