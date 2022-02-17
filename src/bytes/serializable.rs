use std::io::{Error, ErrorKind, Read};
use std::mem;
use crate::bytes::serializers::{ByteReader, ByteWriter};
use crate::class::constant_pool::ConstantPoolEntry;

pub trait Serializable: Sized {

    fn read(byte_reader: &mut impl ByteReader) -> Result<Self, Error>;
    fn write(&self, byte_writer: &mut impl ByteWriter) -> Result<(), Error>;
}

macro_rules! serializable_primitive {
    ($($typ:ty),+) => {
        $(
            impl Serializable for $typ {

                fn read(byte_reader: &mut impl ByteReader) -> Result<Self, Error> {
                    let bytes = byte_reader.read_array::<{ std::mem::size_of::<$typ>() }>()?;
                    Ok(<$typ>::from_ne_bytes(bytes))
                }

                fn write(&self, byte_writer: &mut impl ByteWriter) -> Result<(), Error> {
                    byte_writer.write(&self.to_ne_bytes())
                }
            }
        )*
    }
}

serializable_primitive!(u8, u16, u32, u64, u128, i8, i16, i32, i64, i128, f32, f64);

impl<T: Serializable> Serializable for Box<[T]> {

    fn read(byte_reader: &mut impl ByteReader) -> Result<Box<[T]>, Error> {
        let size = u64::read(byte_reader)?;

        let mut slice = Vec::with_capacity(size as usize);
        for i in 0..size {
            slice.push(T::read(byte_reader)?);
        }
        Ok(slice.into_boxed_slice())
    }

    fn write(&self, byte_writer: &mut impl ByteWriter) -> Result<(), Error> {
        (self.len() as u64).write(byte_writer)?;
        for item in self.iter() {
            item.write(byte_writer)?;
        }
        Ok(())
    }
}
