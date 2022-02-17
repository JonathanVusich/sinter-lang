use std::io;
use std::io::{BufRead, BufReader, BufWriter, Error, ErrorKind, Read, Write};
use crate::bytes::serializable::Serializable;
use crate::errors::vm_error::{VMError, VMErrorKind};

pub trait ByteReader {

    fn read_array<const LEN: usize>(&mut self) -> Result<[u8; LEN], Error>;
    fn read_vec(&mut self, bytes: usize) -> Result<Box<[u8]>, Error>;
}

pub trait ByteWriter {

    fn write(&mut self, bytes: &[u8]) -> Result<(), Error>;
}

impl<T: Read> ByteReader for BufReader<T> {

    fn read_array<const LEN: usize>(&mut self) -> Result<[u8; LEN], Error> {
        let mut buffer: [u8; LEN] = [0u8; LEN];
        self.read_exact(&mut buffer)?;
        Ok(buffer)
    }

    fn read_vec(&mut self, bytes: usize) -> Result<Box<[u8]>, Error> {
        let mut buffer = Vec::<u8>::with_capacity(bytes);
        self.read_exact(&mut buffer)?;
        Ok(buffer.into_boxed_slice())
    }
}

impl<T: Write> ByteWriter for BufWriter<T> {

    fn write(&mut self, bytes: &[u8]) -> Result<(), Error> {
        self.write(bytes)?;
        Ok(())
    }
}

mod tests {
    use crate::bytes::serializers::ByteReader;
}