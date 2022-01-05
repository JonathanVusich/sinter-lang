use crate::function::function::Function;

pub struct CallFrame<'a> {
    pub function: Function,
    pub instructions: &'a [u8],
    pub ip: usize,
    pub address: usize,
    pub size: usize,
}