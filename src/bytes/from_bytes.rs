use crate::bytes::byte_reader::ByteReader;
use crate::class::constant_pool::ConstantPoolEntry;

pub trait FromBytes {

    fn load(byte_reader: &mut ByteReader) -> Self;
}

impl FromBytes for u16 {

    fn load(byte_reader: &mut ByteReader) -> Self {
        u16::from_ne_bytes(*byte_reader.read_bytes::<2>())
    }
}