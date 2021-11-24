use std::sync::{Arc, Mutex};
use std::thread::Thread;

use crate::class::class_loader::ClassLoader;
use crate::gc::thread_allocator::ThreadAllocator;
use crate::pointers::heap_pointer::HeapPointer;
use crate::thread_stack::ThreadStack;

type ThreadInfo = (Arc<ThreadStack>, Arc<ThreadAllocator>);

pub struct GarbageCollector {
    thread_info: Mutex<Vec<ThreadInfo>>,
    class_loader: Arc<ClassLoader>
}

impl GarbageCollector {

    pub fn new(class_loader: Arc<ClassLoader>) -> Self {
        GarbageCollector {
            thread_info: Mutex::new(vec![]),
            class_loader
        }
    }

    pub fn collect(&self) {
        let stacks = self.thread_info.lock().unwrap();
        for (thread_stack, thread_allocator) in stacks.as_slice() {

        }

        for class in self.class_loader.classes() {
            for root in class.static_roots() {
                trace(root)
            }
        }
    }
}

fn trace(root: HeapPointer) {
    root.clear_ref_count();
    root.increment_ref_count();

    let class_ptr = root.class_pointer();
    let address = root.start_address();

    for reference in class_ptr.references() {
        let new_root = reference.load(address);

        trace(new_root)
    }
}