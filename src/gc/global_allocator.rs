use std::sync::Arc;
use std::sync::atomic::AtomicPtr;
use crate::gc::block::Block;
use crate::gc::garbage_collector::GarbageCollector;

use crate::heap::region::Region;
use crate::pointers::pointer::Pointer;

pub struct GlobalAllocator {
    garbage_collector: Arc<GarbageCollector>,
    large_object_space: Region,
    heap: Region
}

impl GlobalAllocator {

    pub fn new(garbage_collector: Arc<GarbageCollector>, los_size: usize, heap_size: usize) -> Self {
        let large_object_space = Region::new(los_size).unwrap();
        let heap = Region::new(heap_size).unwrap();
        Self {
            garbage_collector,
            large_object_space,
            heap
        }
    }

    pub fn allocate_block(&self) -> Pointer<Block> {
        match self.heap.allocate_block() {
            Some(block) => block,
            None => {
                self.garbage_collector.collect();
                return self.allocate_block();
            }
        }
    }
}

mod tests {
    use std::thread;
    use std::time::Duration;

    use crate::gc::global_allocator::GlobalAllocator;

    #[test]
    pub fn allocate_huge_heap() {

        // thread::sleep(Duration::from_secs(1000));
    }
}