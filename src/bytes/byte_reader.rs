use crate::bytes::from_bytes::FromBytes;

pub struct ByteReader<'a> {
    pos: usize,
    buffer: &'a[u8],
}

impl<'a> ByteReader<'a> {

    pub fn new(buffer: &'a [u8]) -> ByteReader {
        Self {
            pos: 0,
            buffer,
        }
    }

    pub fn is_empty(&self) -> bool {
        return self.pos >= self.buffer.len() - 1
    }

    pub fn read_bytes<const LEN: usize>(&mut self, ) -> &[u8; LEN] {
        let end = self.pos + LEN;
        let bytes: &[u8; LEN] = self.buffer[self.pos..end].try_into().expect("Buffer is empty!");
        self.pos += LEN;
        bytes
    }
}