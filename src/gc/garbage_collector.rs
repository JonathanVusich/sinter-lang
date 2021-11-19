use std::sync::Arc;
use crate::gc::global_allocator::GlobalAllocator;

pub struct GarbageCollector {
    global_allocator: Arc<GlobalAllocator>
}

impl GarbageCollector {

    pub fn new(global_allocator: Arc<GlobalAllocator>) -> Self {
        GarbageCollector {
            global_allocator
        }
    }

    pub fn collect(&self) {
        
    }
}