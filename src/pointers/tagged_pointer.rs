pub struct TaggedPointer<T> {
    ptr: *mut T
}

impl<T> TaggedPointer<T> {
    pub fn new(ptr: *mut T) -> Self {
        TaggedPointer { ptr }
    }

    pub fn with_bit(ptr: *mut T, bit: u8) -> Self {
        let mut pointer = TaggedPointer::new(ptr);
        pointer.set_bit(bit);
        pointer
    }

    pub fn set_bit(&mut self, bit: u8) {
        self.ptr = (self.ptr as usize | 1 << bit) as _
    }
}