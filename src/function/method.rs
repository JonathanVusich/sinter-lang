use crate::class::constant_pool::ConstantPoolEntry;

#[derive(Clone)]
pub struct Method {
    name: ConstantPoolEntry,
    descriptor: ConstantPoolEntry,
    max_stack_size: u16,
    max_locals: u16,
    code: Vec<u8>,
}