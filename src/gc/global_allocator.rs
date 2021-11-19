use crate::gc::block::{Block, BLOCK_SIZE};
use std::sync::atomic::{AtomicU64, AtomicUsize, Ordering};
use std::alloc::{AllocError, Layout, Global, Allocator, alloc_zeroed, System};
use std::sync::atomic::Ordering::{SeqCst, Relaxed, Acquire};
use std::mem::MaybeUninit;
use std::sync::Mutex;
use crate::pointers::heap_pointer::HeapPointer;
use crate::object::class::Class;

pub struct GlobalAllocator {
    reclaimed_blocks: Mutex<Vec<Box<Block>>>
}


impl GlobalAllocator {

    pub fn new() -> Self {
        GlobalAllocator {
            reclaimed_blocks: Mutex::new(vec![])
        }
    }

    pub fn allocate_block(&self) -> Box<Block> {
        let mut reclaimed_blocks = self.reclaimed_blocks.lock().unwrap();
        let possible_block = reclaimed_blocks.pop();
        std::mem::drop(reclaimed_blocks);

        match possible_block {
            Some(block) => block,
            None => Block::boxed()
        }
    }

    pub fn deallocate_block(&self, block: Box<Block>) {
        let mut blocks = self.reclaimed_blocks.lock().unwrap();
        blocks.push(block);
    }

    pub fn allocate_large_object(&self, class: &Class) -> HeapPointer {
        let layout = Layout::array::<u8>(class.object_size()).unwrap();
        let ptr = Global.allocate(layout).unwrap();

        HeapPointer::new(class, ptr.cast().as_ptr())
    }

    pub fn deallocate_large_object(&self, object: HeapPointer) {
        let class = object.class_pointer();
        let layout = Layout::array::<u8>(class.object_size()).unwrap();
        unsafe { Global.deallocate(object.to_raw(), layout); }
    }
}

mod tests {
    use std::alloc::alloc;
    use crate::gc::global_allocator::GlobalAllocator;
    use crate::gc::block::{BLOCK_SIZE, Block};
    use std::sync::atomic::Ordering::{Relaxed, SeqCst};
    use std::thread;
    use std::sync::Arc;
    use std::thread::JoinHandle;
    use std::sync::atomic::AtomicUsize;
    use std::time::Duration;
    use crate::object::class::Class;

    #[test]
    pub fn allocate_and_reuse() {
        let global_alloc = GlobalAllocator::new();

        let block = global_alloc.allocate_block();
        let second_block = global_alloc.allocate_block();

        global_alloc.deallocate_block(second_block);

        assert_eq!(global_alloc.reclaimed_blocks.lock().unwrap().len(), 1);

        global_alloc.deallocate_block(block);

        assert_eq!(global_alloc.reclaimed_blocks.lock().unwrap().len(), 2);
    }

    #[test]
    #[ignore]
    pub fn allocate_from_multiple_threads() {
        let max_size = 50_000;

        let global_alloc = Arc::new(GlobalAllocator::new());

        let mut handles = vec![];

        for _ in 0..16 {
            let allocator = global_alloc.clone();
            let handle = thread::spawn(move || {
                let mut blocks: Vec<Box<Block>> = vec![];
                for i in 0..1000 {
                    blocks.push(allocator.allocate_block());
                }
            });
            handles.push(handle);
        }

        for handle in handles {
            handle.join().unwrap();
        }
    }

    #[test]
    pub fn allocate_and_deallocate_large_object() {
        let max_size = 1;

        let class = Class::new(BLOCK_SIZE * max_size);

        let global_allocator = GlobalAllocator::new();
        let large_object = global_allocator.allocate_large_object(&class);

        global_allocator.deallocate_large_object(large_object);
    }
}
