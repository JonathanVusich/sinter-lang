use crate::function::function::Function;

pub struct CallFrame {
    pub function: Function,
    pub ip: usize,
    pub address: usize,
    pub size: usize,
}