use std::io;
use std::io::{BufRead, BufReader, ErrorKind, Read};
use crate::bytes::from_bytes::FromBytes;
use crate::errors::vm_error::{VMError, VMErrorKind};

pub trait ByteReader {

    fn read_bytes<const LEN: usize>(&mut self) -> Option<[u8; LEN]>;
    fn read(&mut self, bytes: usize) -> Option<&[u8]>;
}

impl<T: Read> ByteReader for BufReader<T> {

    fn read_bytes<const LEN: usize>(&mut self) -> Option<[u8; LEN]> {
        let mut buffer: [u8; LEN] = [0u8; LEN];
        self.read_exact(&mut buffer);
        Some(buffer)
    }

    fn read(&mut self, bytes: usize) -> Option<&[u8]> {
        let mut buffer = Vec::<u8>::with_capacity(bytes);
        self.read_exact(&mut buffer);
        Some(&buffer)
    }
}

mod tests {
    use crate::bytes::byte_reader::ByteReader;
}