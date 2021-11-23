use std::sync::{Arc, Mutex};
use std::thread::Thread;
use crate::stack::ThreadStack;

pub struct GarbageCollector {
    thread_stacks: Mutex<Vec<ThreadStack>>
}

impl GarbageCollector {

    pub fn new() -> Self {
        GarbageCollector {
            thread_stacks: Mutex::new(vec![])
        }
    }

    pub fn collect(&self) {
        let stacks = self.thread_stacks.lock().unwrap();
        for thread_stack in stacks.as_slice() {

        }
    }
}