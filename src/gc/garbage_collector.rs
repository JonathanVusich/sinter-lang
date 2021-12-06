use std::rc::Rc;
use std::sync::{Arc, Mutex};
use std::thread::Thread;

use crate::class::class_loader::ClassLoader;
use crate::gc::thread_allocator::ThreadAllocator;
use crate::pointers::heap_pointer::HeapPointer;
use crate::thread_stack::ThreadStack;

pub struct GarbageCollector {
    class_loader: Rc<ClassLoader>
}

impl GarbageCollector {

    pub fn new(class_loader: Rc<ClassLoader>) -> Self {
        GarbageCollector {
            class_loader
        }
    }

    pub fn collect(&self, thread_stack: &ThreadStack) {
        for class in self.class_loader.classes() {
            for root in class.static_roots() {
                trace(root)
            }
        }

        for root in thread_stack.gc_roots() {
            trace(root)
        }
    }
}

fn trace(root: HeapPointer) {
    if !root.is_marked() {
        root.mark();

        let class_ptr = root.class_pointer();
        let address = root.start_address();

        for reference in class_ptr.references() {
            let new_root = reference.load(address);

            trace(new_root)
        }
    }
}