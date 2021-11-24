use crate::pointers::pointer::Pointer;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct InlineField {
    offset: usize,
    length: usize
}

impl InlineField {

    pub fn new(offset: usize, length: usize) -> Self {
        Self {
            offset,
            length,
        }
    }

    pub fn load(&self, instance: Pointer<u8>) -> Pointer<u8> {
        instance.add(self.offset)
    }
}