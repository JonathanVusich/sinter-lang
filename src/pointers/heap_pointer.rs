use std::ops::Deref;
use std::pin::Pin;
use std::ptr::NonNull;

use crate::class::class::Class;
use crate::class::references::Reference;
use crate::gc::block::{Block, BLOCK_BYTEMASK};
use crate::gc::byte_map::ByteMap;
use crate::pointers::mark_word::MarkWord;
use crate::pointers::pointer::Pointer;
use crate::pointers::tagged_pointer::TaggedPointer;
use crate::util::constants::WORD;

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
#[repr(transparent)]
pub struct HeapPointer {
    ptr: Pointer<u8>,
}

impl HeapPointer {

    pub fn new(class: &Class, ptr: *mut u8) -> Self {
        #[cfg(target_pointer_width = "64")] {
            let byte: u8 = class.mark_word().into();
            let shifted_mark_word = (byte as usize) << 56;
            let tagged_ptr = ptr as usize | shifted_mark_word;

            let pointer = TaggedPointer::new_with_mark_word(class, class.mark_word());

            let address: u64 = pointer.into();
            let heap_pointer = HeapPointer { ptr: Pointer::from_raw(ptr) };
            heap_pointer.ptr.cast::<u64>().write(address);
            heap_pointer
        }
        #[cfg(target_pointer_width = "32")] {
            let pointer = Pointer::new(class);

            let heap_pointer = HeapPointer { ptr: Pointer::from_raw(ptr) };
            heap_pointer.ptr.cast::<u32>().write(pointer as u32);
            heap_pointer
        }
    }

    pub fn class_pointer(&self) -> Pointer<Class> {
        #[cfg(target_pointer_width = "64")] {
            let mut val = self.ptr.cast::<isize>().read();
            // Clear out tags
            val <<= 16;
            val >>= 16;

            let ptr: *mut Class = val as _;
            Pointer::from_raw(ptr)
        }
        #[cfg(target_pointer_width = "32")] {
            let val = self.ptr.offset(4).cast::<usize>().read();
            let ptr: *mut Class = val as _;
            Pointer::from_raw(ptr)
        }
    }

    pub fn start_address(&self) -> Pointer<u8> {
        self.ptr.offset(8)
    }

    pub fn is_young(&self) -> bool {
        !self.is_old()
    }

    pub fn is_old(&self) -> bool {
        self.mark_word().is_bit_set(3)
    }

    pub fn is_marked(&self) -> bool {
        self.mark_word().is_bit_set(4)
    }

    pub fn mark(&self) {
        let mut mark_word = self.mark_word();
        mark_word.set_bit(4);
        self.set_mark_word(mark_word);

        self.block().mark_instance(&*self.class_pointer(), self.start_address().to_raw());
    }

    pub fn spans_lines(&self) -> bool {
        self.mark_word().is_bit_set(5)
    }

    pub fn is_new(&self) -> bool {
        self.mark_word().is_bit_set(6)
    }

    pub fn is_forwarded(&self) -> bool {
        let mark_word = self.mark_word();
        mark_word.is_bit_set(7) && mark_word.is_bit_set(8)
    }

    pub fn is_being_forwarded(&self) -> bool {
        self.mark_word().is_bit_set(7)
    }

    pub fn mark_forwarded(&self) {
        let mut mark_word = self.mark_word();
        mark_word.set_bit(7);
        mark_word.set_bit(8);
        self.set_mark_word(mark_word);
    }

    pub fn mark_being_forwarded(&self) {
        let mut mark_word = self.mark_word();
        mark_word.set_bit(7);
        self.set_mark_word(mark_word);
    }

    pub fn block(&self) -> Pointer<Block> {
        let val: usize = self.ptr.into();
        let address: usize = val & BLOCK_BYTEMASK;

        let block: *mut Block = address as _;
        Pointer::from_raw(block)
    }

    /// # Safety
    ///
    /// This function is completely safe since the internal pointer is guaranteed to be valid.
    pub fn to_raw(&self) -> NonNull<u8> {
        self.ptr.into()
    }

    fn mark_word(&self) -> MarkWord {
       self.ptr.read().into()
    }

    fn set_mark_word(&self, mark_word: MarkWord) {
        self.ptr.write(mark_word.into())
    }
}

impl From<[u8; WORD]> for HeapPointer {

    fn from(bytes: [u8; WORD]) -> Self {
        HeapPointer { ptr: bytes.into() }
    }
}

impl From<HeapPointer> for [u8; WORD] {

    fn from(pointer: HeapPointer) -> Self {
        pointer.ptr.into()
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

        let read_class = heap_pointer.class_pointer();

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

