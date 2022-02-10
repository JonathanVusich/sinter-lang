use crate::bytes::byte_reader::ByteReader;
use crate::bytes::from_bytes::FromBytes;

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

impl FromBytes for ConstantPoolEntry {

    fn load(byte_reader: &mut ByteReader) -> Self {
        let offset = u16::from_ne_bytes(*byte_reader.read_bytes::<2>());
        let size = u16::from_ne_bytes(*byte_reader.read_bytes::<2>());
        Self {
            offset,
            size
        }
    }
}

impl ConstantPoolEntry {

    pub fn start(&self) -> usize {
        self.offset as usize
    }

    pub fn end(&self) -> usize {
        (self.offset + self.size) as usize
    }
}