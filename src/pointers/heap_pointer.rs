use crate::object::class::Class;
use crate::pointers::tagged_pointer::TaggedPointer;

pub struct HeapPointer {
    ptr: *mut u64,
}

impl HeapPointer {

    pub fn new(ptr: *mut u64) -> Self {
        HeapPointer { ptr }
    }

    pub fn write_class_pointer(&mut self, class: *const Class) {
        unsafe {
            let pointer = TaggedPointer::new(class);
            let ptr = self.ptr as *mut TaggedPointer<Class>;
            ptr.write(pointer);
        }
    }

    pub fn class_pointer(&mut self) -> *const Class {
        unsafe {
            let val = self.ptr.cast::<TaggedPointer<Class>>().read();
            return &*val;
        }
    }
}

mod tests {
    use crate::pointers::heap_pointer::HeapPointer;
    use crate::object::class::Class;

    #[test]
    pub fn constructor() {
        let val: u64 = 128;
        let raw_ptr: *mut u64 = val as *mut u64;
        let mut heap_pointer = HeapPointer::new(raw_ptr);    }

    #[test]
    pub fn class_pointer() {
        let val: u64 = 128;
        let raw_ptr: *mut u64 = val as *mut u64;
        let mut heap_pointer = HeapPointer::new(raw_ptr);

        let class = Class {};
        let class_pointer = &class as *const Class;

        heap_pointer.write_class_pointer(class_pointer);
        //
        // let read_class = heap_pointer.class_pointer();
        //
        // assert_eq!(class_pointer, read_class);
    }
}

