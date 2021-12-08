use std::sync::atomic::AtomicPtr;

use crate::heap::region::Region;

pub struct GlobalAllocator {
    large_object_space: Region,
    heap: Region
}

impl GlobalAllocator {

    pub fn new(los_size: usize, heap_size: usize) -> Self {

        let mut large_object_space = Region::new(los_size).unwrap();
        let mut heap = Region::new(heap_size).unwrap();
        Self {
            large_object_space,
            heap
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