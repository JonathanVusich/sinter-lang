#[repr(transparent)]
#[derive(Clone)]
pub struct ConstantPool {
    pool: Vec<u8>
}

#[derive(Copy, Clone)]
pub struct ConstantPoolEntry {
    offset: u16,
    size: u16,
}

impl ConstantPool {

    pub fn new() -> Self {
        Self {
            pool: Vec::new()
        }
    }

    pub fn load(&self, entry: ConstantPoolEntry) -> &[u8] {
        &self.pool[entry.start()..entry.end()]
    }
}

impl ConstantPoolEntry {

    pub fn new(offset: u16, size: u16) -> Self {
        Self {
            offset,
            size
        }
    }

    pub fn start(&self) -> usize {
        self.offset as usize
    }

    pub fn end(&self) -> usize {
        (self.offset + self.size) as usize
    }
}