#[repr(transparent)]
pub struct ConstantPool {
    pool: Vec<u8>
}

impl ConstantPool {

    pub fn new() -> Self {
        Self {
            pool: Vec::new()
        }
    }

    pub fn load<const SIZE: usize>(&self, offset: usize) -> [u8; SIZE] {
        self.pool[offset..offset + SIZE].try_into().unwrap()
    }
}