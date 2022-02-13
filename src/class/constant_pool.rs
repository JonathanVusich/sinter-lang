use std::io::ErrorKind;
use crate::bytes::byte_reader::ByteReader;
use crate::bytes::from_bytes::FromBytes;

#[repr(transparent)]
#[derive(Clone)]
pub struct ConstantPool {
    pool: Box<[u8]>,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct ConstantPoolEntry {
    offset: u16,
    size: u16,
}

impl ConstantPool {

    pub fn load_bytes(&self, entry: ConstantPoolEntry) -> &[u8] {
        &self.pool[entry.start()..entry.end()]
    }

    pub fn load_str(&self, entry: ConstantPoolEntry) -> &str {
        std::str::from_utf8(self.load_bytes(entry)).expect("Invalid UTF-8 strings!")
    }
}

impl FromBytes for ConstantPool {

    fn load(byte_reader: &mut impl ByteReader) -> Option<Self> {
        let pool = Box::<[u8]>::load(byte_reader)?;
        Some(Self {
            pool
        })
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

impl FromBytes for ConstantPoolEntry {
    
    fn load(byte_reader: &mut impl ByteReader) -> Option<Self> {
        let offset = u16::from_ne_bytes(byte_reader.read_bytes::<2>()?);
        let size = u16::from_ne_bytes(byte_reader.read_bytes::<2>()?);
        Some(
            Self {
                offset,
                size,
            })
    }
}
