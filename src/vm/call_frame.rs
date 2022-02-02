use crate::function::function::Function;

pub struct CallFrame {
    pub ip: usize,
    pub address: usize,
    pub size: usize,
}