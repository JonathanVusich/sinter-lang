use crate::vm::slot::Slot;

pub struct CallFrame {
    return_address: usize,
    slots: Vec<Slot>
}

impl CallFrame {

    pub fn new() -> Self {
        Self {
            return_address: 0,
            slots: Vec::new()
        }
    }
}