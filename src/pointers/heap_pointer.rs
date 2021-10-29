use crate::object::class::Class;
use crate::pointers::tagged_pointer::TaggedPointer;

pub struct HeapPointer {
    ptr: *mut u8,
}

impl HeapPointer {

    pub fn new(class: &Class, ptr: *mut u8) -> Self {
        let pointer = TaggedPointer::new(class);
        let address: u64 = pointer.into();
        let heap_pointer = HeapPointer { ptr };
        unsafe {
            heap_pointer.ptr.cast::<u64>().write(address);
        }
        heap_pointer
    }

    pub fn class_pointer(&self) -> TaggedPointer<Class> {
        let address = self.class_address();
        TaggedPointer::from_address(address)
    }

    pub fn start_address(&self) -> *mut u64 {
        unsafe {
            self.ptr.cast::<u64>().offset(1)
        }
    }

    pub fn increment_ref_count(&mut self) {
        let mut class_pointer = self.class_pointer();
        let mut mark_word = class_pointer.get_mark_word();
        let mut ref_count = mark_word >> 4;

        // println!("{}", ref_count);

        if ref_count < 15 {
            ref_count += 1;
        }

        ref_count <<= 4;

        // println!("{}", ref_count);

        mark_word &= 0xF0;

        // println!("{}", mark_word);

        let result = mark_word | ref_count;

        println!("{}", result);
        class_pointer.set_mark_word(result);
    }

    pub fn mark_spans_lines(&mut self) {
        let mut class_pointer = self.class_pointer();
        class_pointer.set_bit(5);
        self.write_class_pointer(class_pointer);
    }

    pub fn spans_lines(&self) -> bool {
        self.class_pointer().is_bit_set(5)
    }

    pub fn mark_as_new(&mut self) {
        let mut class_pointer = self.class_pointer();
        class_pointer.set_bit(6);
        self.write_class_pointer(class_pointer);
    }

    pub fn is_new(&self) -> bool {
        self.class_pointer().is_bit_set(6)
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

mod tests {
    use crate::object::class::Class;
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

        let mut heap_pointer = HeapPointer::new(&class, raw_ptr.cast::<u8>());

        assert!(!heap_pointer.spans_lines());

        heap_pointer.mark_spans_lines();

        assert!(heap_pointer.spans_lines());
    }

    #[test]
    pub fn increment_ref_count() {
        let mut val: u64 = 128;
        let raw_ptr: *mut u64 = &mut val;

        let class = Class::new(2);

        let mut heap_pointer = HeapPointer::new(&class, raw_ptr.cast::<u8>());

        heap_pointer.increment_ref_count();

        assert_eq!(0b00010000, heap_pointer.class_pointer().get_mark_word());

        heap_pointer.increment_ref_count();

        assert_eq!(0b00100000, heap_pointer.class_pointer().get_mark_word());
    }
}

