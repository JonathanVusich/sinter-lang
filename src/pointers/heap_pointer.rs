use crate::object::class::Class;
use crate::pointers::tagged_pointer::TaggedPointer;

pub struct HeapPointer {
    ptr: *mut u64,
}

impl HeapPointer {

    pub fn new(class: &Class, ptr: *mut u64) -> Self {
        let pointer = TaggedPointer::new(class);
        let address: u64 = pointer.into();
        let heap_pointer = HeapPointer { ptr };
        unsafe {
            heap_pointer.ptr.write(address);
        }
        heap_pointer
    }

    pub fn class_pointer(&self) -> TaggedPointer<Class> {
        let address = self.class_address();
        TaggedPointer::from_address(address)
    }

    pub fn start_address(&self) -> *mut u64 {
        unsafe {
            self.ptr.offset(1)
        }
    }

    fn class_address(&self) -> u64 {
        unsafe {
            self.ptr.read()
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
        let heap_pointer = HeapPointer::new(&class, raw_ptr);
    }

    #[test]
    pub fn class_pointer() {
        let mut val: u64 = 128;
        let raw_ptr: *mut u64 = &mut val;

        let class = Class::new(2);

        let heap_pointer = HeapPointer::new(&class, raw_ptr);

        let read_class = heap_pointer.class_pointer();

        assert_eq!(class, *read_class);
    }
}

