use crate::bytes::byte_reader::ByteReader;
use crate::bytes::from_bytes::FromBytes;
use crate::class::compiled_class::CompiledClass;
use crate::class::constant_pool::{ConstantPool, ConstantPoolEntry};

#[derive(Clone)]
pub struct Method {
    name: ConstantPoolEntry,
    descriptor: ConstantPoolEntry,
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
        let method_name = std::str::from_utf8(pool.load(self.name)).unwrap();
        let method_descriptor = MethodDescriptor::from(pool.load(self.descriptor));

        return method_name == "main" && method_descriptor.return_type
    }
}

impl MethodDescriptor {

    pub fn from(bytes: &[u8]) -> MethodDescriptor {
        assert!(bytes.len() >= 4);

        let mut byte_reader = ByteReader::new(bytes);

        let return_type = ConstantPoolEntry::load(&mut byte_reader);

        return if bytes.len() > 4 {
            let num_entries = u16::load(&mut byte_reader);
            let mut parameters = Vec::<ConstantPoolEntry>::with_capacity(num_entries as usize);

            while !byte_reader.is_empty() {
                let parameter = ConstantPoolEntry::load(&mut byte_reader);
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