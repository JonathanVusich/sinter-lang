use crate::bytes::byte_reader::ByteReader;
use crate::bytes::from_bytes::FromBytes;
use crate::class::compiled_class::CompiledClass;
use crate::class::constant_pool::{ConstantPool, ConstantPoolEntry};
use crate::strings::internal_string::InternalString;

#[derive(Clone)]
pub struct Method {
    name: InternalString,
    descriptor: MethodDescriptor,
    max_stack_size: u16,
    max_locals: u16,
    code: Vec<u8>,
}

pub struct MethodDescriptor {
    return_type: ConstantPoolEntry,
    parameters: Vec<ConstantPoolEntry>,
}

impl Method {

    pub fn is_main_method(&self, pool: &ConstantPool) -> bool {
        let method_name = pool.load_str(self.name);
        let method_descriptor = pool.load::<MethodDescriptor>(self.descriptor);

        method_name == "main" && pool.load::<Type>(method_descriptor.return_type) == Type::Void
    }
}

impl FromBytes for MethodDescriptor {

    fn load(byte_reader: &mut ByteReader) -> Self {
        assert!(byte_reader.remaining() >= 4);

        let return_type = ConstantPoolEntry::load(byte_reader);

        return if byte_reader.remaining() > 4 {
            let num_entries = u16::load(byte_reader);
            let mut parameters = Vec::<ConstantPoolEntry>::with_capacity(num_entries as usize);

            while !byte_reader.is_empty() {
                let parameter = ConstantPoolEntry::load(byte_reader);
                parameters.push(parameter)
            }

            MethodDescriptor {
                return_type,
                parameters
            }
        } else {
            MethodDescriptor {
                return_type,
                parameters: Vec::new()
            }
        }
    }
}