use std::sync::{Arc, Mutex};
use std::thread::Thread;
use crate::class::class_loader::ClassLoader;
use crate::gc::thread_allocator::ThreadAllocator;
use crate::thread_stack::ThreadStack;

pub struct GarbageCollector {
    thread_stacks: Mutex<Vec<Arc<ThreadStack>>>,
    class_loader: Arc<ClassLoader>
}

impl GarbageCollector {

    pub fn new(class_loader: Arc<ClassLoader>) -> Self {
        GarbageCollector {
            thread_stacks: Mutex::new(vec![]),
            class_loader
        }
    }

    pub fn collect(&self) {
        let stacks = self.thread_stacks.lock().unwrap();
        for thread_stack in stacks.as_slice() {

        }

        for class in self.class_loader.classes() {
            for root in class.static_roots() {

            }
        }
    }
}