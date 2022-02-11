use std::io::{ErrorKind, Read};
use crate::bytes::byte_reader::ByteReader;
use crate::class::constant_pool::ConstantPoolEntry;

pub trait FromBytes: Sized {

    fn load(byte_reader: &mut impl ByteReader) -> Result<Self, ErrorKind>;
}

impl FromBytes for u8 {

    fn load(byte_reader: &mut impl ByteReader) -> Result<Self, ErrorKind> {
        Ok(u8::from_ne_bytes(byte_reader.read_bytes::<1>()?))
    }
}

impl FromBytes for u16 {

    fn load(byte_reader: &mut impl ByteReader) -> Result<Self, ErrorKind> {
        Ok(u16::from_ne_bytes(byte_reader.read_bytes::<2>()?))
    }
}

impl FromBytes for Box<[u8]> {

    fn load(byte_reader: &mut impl ByteReader) -> Result<Box<[u8]>, ErrorKind> {
        let size = u16::load(byte_reader);
        Ok(Box::new(*byte_reader.read(size as usize)?))
    }
}
