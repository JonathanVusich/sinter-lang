use std::ops::Deref;
use std::pin::Pin;
use std::ptr::NonNull;

use crate::class::class::Class;
use crate::class::reference_field::ReferenceField;
use crate::gc::block::{Block, BLOCK_BYTEMASK};
use crate::gc::byte_map::ByteMap;
use crate::pointers::pointer::Pointer;
use crate::pointers::tagged_pointer::TaggedPointer;

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct HeapPointer {
    ptr: *mut u8,
}

impl HeapPointer {

    pub fn new(class: &Class, ptr: *mut u8) -> Self {
        let pointer = TaggedPointer::new_with_mark_word(class, class.mark_word());

        let address: u64 = pointer.into();
        let heap_pointer = HeapPointer { ptr };
        unsafe {
            heap_pointer.ptr.cast::<u64>().write(address);
        }
        heap_pointer
    }

    pub fn from_address(address: u64) -> Self {
        let ptr: *mut u8 = address as _;
        HeapPointer { ptr }
    }

    pub fn start_address(&self) -> Pointer<u8> {
        unsafe {
            Pointer::from_raw(self.ptr.offset(8))
        }
    }

    pub fn is_young(&self) -> bool {
        !self.is_old()
    }

    pub fn is_old(&self) -> bool {
        self.tagged_class_pointer().is_bit_set(3)
    }

    pub fn is_marked(&self) -> bool {
        self.tagged_class_pointer().is_bit_set(4)
    }

    pub fn mark(&self) {
        let mut tagged_class_ptr = self.tagged_class_pointer();
        tagged_class_ptr.set_bit(4);

        self.block().mark_instance(&tagged_class_ptr, self.ptr);
    }

    pub fn spans_lines(&self) -> bool {
        self.tagged_class_pointer().is_bit_set(5)
    }

    pub fn is_new(&self) -> bool {
        self.tagged_class_pointer().is_bit_set(6)
    }

    pub fn is_forwarded(&self) -> bool {
        let tagged_pointer = self.tagged_class_pointer();
        tagged_pointer.is_bit_set(7) && tagged_pointer.is_bit_set(8)
    }

    pub fn is_being_forwarded(&self) -> bool {
        self.tagged_class_pointer().is_bit_set(7)
    }

    pub fn mark_forwarded(&self) {
        let mut class_pointer = self.tagged_class_pointer();
        class_pointer.set_bit(7);
        class_pointer.set_bit(8);
        self.write_class_pointer(class_pointer);
    }

    pub fn mark_being_forwarded(&self) {
        let mut class_pointer = self.tagged_class_pointer();
        class_pointer.set_bit(7);
        self.write_class_pointer(class_pointer);
    }

    pub fn class_pointer(&self) -> Pointer<Class> {
        let mut ptr = self.class_address() as isize;
        // Clear out tags
        ptr <<= 16;
        ptr >>= 16;

        let pointer: *mut Class = ptr as _;

        Pointer::from_raw(pointer)
    }

    pub fn block(&self) -> Pointer<Block> {
        let address = self.ptr as usize & BLOCK_BYTEMASK;

        let block: *mut Block = address as _;
        Pointer::from_raw(block)
    }

    /// # Safety
    ///
    /// This function is completely safe since the internal pointer is guaranteed to be valid.
    pub fn to_raw(&self) -> NonNull<u8> {
        unsafe { NonNull::new_unchecked(self.ptr) }
    }

    fn tagged_class_pointer(&self) -> TaggedPointer<Class> {
        let class_address = self.class_address();
        TaggedPointer::from_address(class_address)
    }

    fn class_address(&self) -> u64 {
        unsafe {
            self.ptr.cast::<u64>().read()
        }
    }

    fn write_class_pointer(&self, class_pointer: TaggedPointer<Class>) {
        unsafe {
            self.ptr.cast::<u64>().write(class_pointer.into())
        }
    }
}

impl From<HeapPointer> for u64 {
    fn from(ptr: HeapPointer) -> Self {
        ptr.ptr as u64
    }
}

mod tests {
    use crate::class::class::Class;
    use crate::gc::block::{Block, LINE_SIZE};
    use crate::pointers::heap_pointer::HeapPointer;

    #[test]
    pub fn constructor() {
        let mut val: u64 = 128;
        let raw_ptr: *mut u64 = &mut val;
        let class = Class::new(2);
        let heap_pointer = HeapPointer::new(&class, raw_ptr.cast::<u8>());
    }

    #[test]
    pub fn class_pointer() {
        let mut val: u64 = 128;
        let raw_ptr: *mut u64 = &mut val;

        let class = Class::new(2);

        let heap_pointer = HeapPointer::new(&class, raw_ptr.cast::<u8>());

        let read_class = heap_pointer.tagged_class_pointer();

        assert_eq!(class, *read_class);
    }

    #[test]
    pub fn spans_lines() {
        let mut val: u64 = 128;
        let raw_ptr: *mut u64 = &mut val;

        let class = Class::new(2);

        let heap_pointer = HeapPointer::new(&class, raw_ptr.cast::<u8>());

        assert!(!heap_pointer.spans_lines());

        let large_class = Class::new(LINE_SIZE);

        let heap_pointer = HeapPointer::new(&large_class, raw_ptr.cast::<u8>());

        assert!(heap_pointer.spans_lines());
    }
}

