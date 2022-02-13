use std::rc::Rc;
use std::sync::{Arc, Mutex};
use std::thread::Thread;

use crate::class::class_loader::ClassLoader;
use crate::gc::thread_allocator::ThreadAllocator;
use crate::pointers::heap_pointer::HeapPointer;

pub struct GarbageCollector {
    class_loader: Rc<ClassLoader>,
    thread_stacks: Vec<Arc<Thread>>
}

impl GarbageCollector {

    pub fn new(class_loader: Rc<ClassLoader>) -> Self {
        GarbageCollector {
            class_loader,
            thread_stacks: vec![]
        }
    }

    pub fn collect(&self) {

    }
}

fn trace(root: HeapPointer) {
    if !root.is_marked() {
        root.mark();

        let class_ptr = root.class_pointer();
        let address = root.start_address();

        for reference in class_ptr.references.iter() {
            let new_root = address
                .add(reference.offset as usize)
                .cast::<HeapPointer>()
                .read();

            trace(new_root)
        }
    }
}