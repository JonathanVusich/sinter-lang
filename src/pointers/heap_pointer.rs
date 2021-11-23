use std::ops::Deref;
use std::ptr::NonNull;
use crate::object::class::Class;
use crate::pointers::tagged_pointer::TaggedPointer;
use crate::pointers::pointer::Pointer;

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

    pub fn start_address(&self) -> *mut u64 {
        unsafe {
            self.ptr.cast::<u64>().offset(1)
        }
    }

    pub fn increment_ref_count(&mut self) {
        let mut class_pointer = self.tagged_class_pointer();
        let mut mark_word = class_pointer.get_mark_word();
        let mut ref_count = mark_word >> 5;

        if ref_count < 7 {
            ref_count += 1;
        }

        ref_count <<= 5;

        mark_word &= !0xe0;

        let result = ref_count | mark_word;

        class_pointer.set_mark_word(result);
        self.write_class_pointer(class_pointer);
    }

    pub fn decrement_ref_count(&mut self) {
        let mut class_pointer = self.tagged_class_pointer();
        let mut mark_word = class_pointer.get_mark_word();
        let mut ref_count = mark_word >> 5;

        if ref_count > 0 {
            ref_count -= 1;
        }

        ref_count <<= 5;

        mark_word &= !0xe0;

        let result = ref_count | mark_word;

        class_pointer.set_mark_word(result);
        self.write_class_pointer(class_pointer);
    }

    pub fn ref_count(&self) -> u8 {
        let class_pointer = self.tagged_class_pointer();
        class_pointer.get_mark_word() >> 5
    }

    pub fn spans_lines(&self) -> bool {
        self.tagged_class_pointer().is_bit_set(4)
    }

    pub fn is_new(&self) -> bool {
        self.tagged_class_pointer().is_bit_set(6)
    }

    pub fn is_forwarded(&self) -> bool {
        let tagged_pointer = self.tagged_class_pointer();
        tagged_pointer.is_bit_set(7) || tagged_pointer.is_bit_set(8)
    }

    pub fn is_being_forwarded(&self) -> bool {
        self.tagged_class_pointer().is_bit_set(7)
    }

    pub fn mark_forwarded(&mut self) {
        let mut class_pointer = self.tagged_class_pointer();
        class_pointer.set_bit(7);
        class_pointer.set_bit(8);
        self.write_class_pointer(class_pointer);
    }

    pub fn mark_being_forwarded(&mut self) {
        let mut class_pointer = self.tagged_class_pointer();
        class_pointer.set_bit(7);
        self.write_class_pointer(class_pointer);
    }

    pub fn class_pointer(&self) -> Pointer<Class> {
        Pointer::from_address(self.class_address())
    }

    pub unsafe fn to_raw(&self) -> NonNull<u8> {
        NonNull::new_unchecked(self.ptr)
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

    fn write_class_pointer(&mut self, class_pointer: TaggedPointer<Class>) {
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
    use crate::object::class::Class;
    use crate::pointers::heap_pointer::HeapPointer;
    use crate::gc::block::LINE_SIZE;

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

    #[test]
    pub fn ref_count() {
        let mut val: u64 = 128;
        let raw_ptr: *mut u64 = &mut val;

        let class = Class::new(2);

        let mut heap_pointer = HeapPointer::new(&class, raw_ptr.cast::<u8>());

        // Clear out mark word
        let mut tagged_ptr = heap_pointer.tagged_class_pointer();
        tagged_ptr.set_mark_word(1);
        heap_pointer.write_class_pointer(tagged_ptr);

        for x in 1..8 {
            heap_pointer.increment_ref_count();
            assert_eq!(((x as u8) << 5) + 1, heap_pointer.tagged_class_pointer().get_mark_word());
        }

        // Check overflow
        assert_eq!(0b11100001, heap_pointer.tagged_class_pointer().get_mark_word());

        heap_pointer.increment_ref_count();

        assert_eq!(0b11100001, heap_pointer.tagged_class_pointer().get_mark_word());

        for x in (0..7).rev() {
            heap_pointer.decrement_ref_count();
            assert_eq!(((x as u8) << 5) + 1, heap_pointer.tagged_class_pointer().get_mark_word());
        }

        assert_eq!(0b00000001, heap_pointer.tagged_class_pointer().get_mark_word());

        heap_pointer.decrement_ref_count();

        assert_eq!(0b00000001, heap_pointer.tagged_class_pointer().get_mark_word());
    }
}

