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

impl<T: FromBytes> FromBytes for Box<[T]> {

    fn load(byte_reader: &mut impl ByteReader) -> Result<Box<[T]>, ErrorKind> {
        let size = u16::load(byte_reader)?;

        let mut results = Vec::<T>::with_capacity(size as usize);
        for i in 0..size {
            results.push(T::load(byte_reader)?)
        }
        Ok(results.into_boxed_slice())
    }
}
