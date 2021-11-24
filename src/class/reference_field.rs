use crate::pointers::heap_pointer::HeapPointer;
use crate::pointers::pointer::Pointer;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct ReferenceField {
    offset: usize,
}

impl ReferenceField {

    pub fn new(offset: usize) -> Self {
        Self {
            offset
        }
    }

    pub fn load(&self, instance: Pointer<u8>) -> HeapPointer {
        let ptr: Pointer<u64> = instance.add(self.offset).cast();
        HeapPointer::from_address(ptr.read())
    }
}