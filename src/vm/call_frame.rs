use crate::class::class::Class;
use crate::function::method::Method;

pub struct CallFrame {
    method: &'static Method,

    ip: usize,
    address: usize,
    size: usize,
}

impl CallFrame {

    pub fn new(method: &'static Method, address: usize) -> Self {
        Self {
            method,
            ip: 0,
            address,
            size: method.max_stack_size as usize,
        }
    }

    pub fn ip(&self) -> usize {
        self.ip
    }

    pub fn increment_ip(&mut self, delta: usize) {
        self.ip += delta;
    }

    pub fn decrement_ip(&mut self, delta: usize) {
        self.ip -= delta;
    }

    pub fn address(&self) -> usize {
        self.address
    }

    pub fn size(&self) -> usize {
        self.size
    }
}

