use crate::bytes::serializable::Serializable;
use crate::bytes::serializers::{ByteReader, ByteWriter};
use std::io::{Error, ErrorKind};

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

impl Serializable for ConstantPool {
    fn read(byte_reader: &mut impl ByteReader) -> Result<Self, Error> {
        let pool = Box::<[u8]>::read(byte_reader)?;
        Ok(Self { pool })
    }

    fn write(&self, byte_writer: &mut impl ByteWriter) -> Result<(), Error> {
        self.pool.write(byte_writer)
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

impl Serializable for ConstantPoolEntry {
    fn read(byte_reader: &mut impl ByteReader) -> Result<Self, Error> {
        let offset = u16::read(byte_reader)?;
        let size = u16::read(byte_reader)?;
        Ok(Self { offset, size })
    }

    fn write(&self, byte_writer: &mut impl ByteWriter) -> Result<(), Error> {
        self.offset.write(byte_writer)?;
        self.size.write(byte_writer)?;
        Ok(())
    }
}
