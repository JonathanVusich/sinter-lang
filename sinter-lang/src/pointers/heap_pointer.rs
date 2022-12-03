use std::ops::Deref;
use std::pin::Pin;
use std::ptr::NonNull;

use crate::class::class::Class;
use crate::class::references::Reference;
use crate::gc::block::{Block, BLOCK_BYTEMASK};
use crate::gc::byte_map::ByteMap;
use crate::pointers::mark_word::MarkWord;
use crate::pointers::pointer::Pointer;
use crate::util::constants::WORD;

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
#[repr(transparent)]
pub struct HeapPointer {
    ptr: Pointer<u8>,
}

impl HeapPointer {
    pub fn new(class: &Class, ptr: *mut u8) -> Self {
        let byte: u8 = class.mark_word.into();
        let shifted_mark_word = (byte as u64) << 56;
        let tagged_ptr = shifted_mark_word | class as *const Class as u64;

        let heap_pointer = HeapPointer {
            ptr: Pointer::from_raw(ptr),
        };
        heap_pointer.ptr.cast::<u64>().write(tagged_ptr);
        heap_pointer
    }

    pub fn class_pointer(&self) -> Pointer<Class> {
        let mut val = self.ptr.cast::<i64>().read();
        // Clear out tags
        val <<= 16;
        val >>= 16;

        let ptr: *mut Class = val as _;
        Pointer::from_raw(ptr)
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

        self.block()
            .mark_instance(&*self.class_pointer(), self.start_address().to_raw());
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
        let val: u64 = self.ptr.into();
        let address: u64 = val & BLOCK_BYTEMASK;

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
        let tagged_ptr = self.ptr.cast::<u64>().read();
        let byte = (tagged_ptr >> 56) as u8;
        byte.into()
    }

    fn set_mark_word(&self, mark_word: MarkWord) {
        let mut tagged_ptr = self.ptr.cast::<i64>().read();
        tagged_ptr <<= 16;
        tagged_ptr >>= 16;

        let shifted_mark_word = (u8::from(mark_word) as u64) << 56;
        let tagged_ptr = tagged_ptr as u64 | shifted_mark_word;

        self.ptr.cast::<u64>().write(tagged_ptr);
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
    use crate::class::class_builder::ClassBuilder;
    use crate::gc::block::{Block, LINE_SIZE};
    use crate::pointers::heap_pointer::HeapPointer;
    use crate::pool::internal_string::InternalString;

    #[test]
    pub fn constructor() {
        let mut val: u64 = 128;
        let raw_ptr: *mut u64 = &mut val;
        let class = ClassBuilder::new()
            .set_size(2)
            .build(|val| InternalString(0));
        let heap_pointer = HeapPointer::new(&class, raw_ptr.cast::<u8>());
    }

    #[test]
    pub fn class_pointer() {
        let mut val: u64 = 128;
        let raw_ptr: *mut u64 = &mut val;

        let class = ClassBuilder::new()
            .set_size(2)
            .build(|val| InternalString(0));

        let heap_pointer = HeapPointer::new(&class, raw_ptr.cast::<u8>());

        let read_class = heap_pointer.class_pointer();

        assert_eq!(class, *read_class);
    }

    #[test]
    pub fn spans_lines() {
        let mut val: u64 = 128;
        let raw_ptr: *mut u64 = &mut val;

        let class = ClassBuilder::new()
            .set_size(2)
            .build(|val| InternalString(0));

        let heap_pointer = HeapPointer::new(&class, raw_ptr.cast::<u8>());

        assert!(!class.mark_word.is_bit_set(5));

        assert!(!heap_pointer.mark_word().is_bit_set(5));
        assert!(!heap_pointer.spans_lines());

        let large_class = ClassBuilder::new()
            .set_size(LINE_SIZE)
            .build(|val| InternalString(0));

        assert!(large_class.mark_word.is_bit_set(5));

        let heap_pointer = HeapPointer::new(&large_class, raw_ptr.cast::<u8>());

        assert!(heap_pointer.spans_lines());
    }
}
