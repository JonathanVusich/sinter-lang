use std::sync::atomic::AtomicPtr;

use region::{Allocation, Protection, Region};

pub struct GlobalAllocator {
    large_object_space: Allocation,
    heap: Allocation,
    cursor: AtomicPtr<u8>
}

impl GlobalAllocator {

    pub fn new(los_size: usize, heap_size: usize) -> Self {

        let mut large_object_space = region::alloc(los_size, Protection::READ_WRITE).unwrap();
        let mut allocation = region::alloc(heap_size, Protection::READ_WRITE).unwrap();
        let alloc_ptr = allocation.as_mut_ptr::<u8>();
        Self {
            heap: allocation,
            cursor: AtomicPtr::new(alloc_ptr)
        }
    }
}

mod tests {
    use std::thread;
    use std::time::Duration;

    use crate::gc::global_allocator::GlobalAllocator;

    #[test]
    pub fn allocate_huge_heap() {
        let global_alloc = GlobalAllocator::new(16 * 1024 * 1024 * 1024);
        println!("{}", global_alloc.heap.len());
        println!("{}", region::page::size());
        // thread::sleep(Duration::from_secs(1000));
    }
}