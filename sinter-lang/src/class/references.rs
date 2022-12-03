use crate::pointers::heap_pointer::HeapPointer;
use crate::pointers::pointer::Pointer;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Reference {
    offset: usize,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct InlineReference {
    offset: usize,
    size: usize,
}

impl Reference {
    pub fn new(offset: usize) -> Self {
        Self { offset }
    }

    pub fn load(&self, instance: Pointer<u8>) -> HeapPointer {
        let ptr: Pointer<usize> = instance.add(self.offset).cast();
        HeapPointer::from(ptr.read().to_ne_bytes())
    }
}

impl InlineReference {
    pub fn new(offset: usize, size: usize) -> Self {
        Self { offset, size }
    }

    pub fn load(&self, instance: Pointer<u8>) -> Pointer<u8> {
        instance.add(self.offset)
    }
}
