use std::io;
use std::io::{BufRead, BufReader, ErrorKind, Read};
use crate::bytes::from_bytes::FromBytes;

pub trait ByteReader {

    fn read_bytes<const LEN: usize>(&mut self) -> Result<[u8; LEN], ErrorKind>;
    fn read(&mut self, bytes: usize) -> Result<&[u8], ErrorKind>;
}

impl<T: Read> ByteReader for BufReader<T> {

    fn read_bytes<const LEN: usize>(&mut self) -> Result<[u8; LEN], ErrorKind> {
        let mut buffer: [u8; LEN] = [0u8; LEN];
        self.read_exact(&mut buffer);
        Ok(buffer)
    }

    fn read(&mut self, bytes: usize) -> Result<&[u8], ErrorKind> {
        let mut buffer = Vec::<u8>::with_capacity(bytes);
        self.read_exact(&mut buffer);
        Ok(&buffer)
    }
}

mod tests {
    use crate::bytes::byte_reader::ByteReader;
}