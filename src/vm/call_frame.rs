pub struct CallFrame {
    return_address: usize,
    size: usize,
}

impl CallFrame {

    pub fn new(return_address: usize, size: usize) -> Self {
        Self {
            return_address,
            size
        }
    }
}