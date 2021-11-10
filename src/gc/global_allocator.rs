use crate::gc::block::{Block, BLOCK_SIZE};
use std::sync::atomic::{AtomicU64, AtomicUsize, Ordering};
use std::alloc::{AllocError, Layout, Global, Allocator, alloc_zeroed, System};
use std::sync::atomic::Ordering::{SeqCst, Relaxed, Acquire};
use std::mem::MaybeUninit;
use crate::pointers::heap_pointer::HeapPointer;
use crate::object::class::Class;

pub struct GlobalAllocator {
    max_size: usize,
    current_allocation: AtomicUsize,
    reclaimed_blocks: Vec<Box<Block>>
}


impl GlobalAllocator {

    pub fn new(size: usize) -> Self {
        GlobalAllocator {
            max_size: size * BLOCK_SIZE,
            current_allocation: AtomicUsize::new(0),
            reclaimed_blocks: vec![]
        }
    }

    pub fn allocate_block(&self) -> Result<Box<Block>, AllocError> {
        let current_alloc = self.current_allocation.fetch_add(BLOCK_SIZE, SeqCst);
        if current_alloc + BLOCK_SIZE <= self.max_size {
            let block = Block::try_allocate()?;
            Ok(block)
        } else {
            self.current_allocation.fetch_sub(BLOCK_SIZE, SeqCst);
            Err(AllocError {})
        }
    }

    pub fn deallocate_block(&mut self, block: Box<Block>) {
        self.current_allocation.fetch_sub(BLOCK_SIZE, SeqCst);
        self.reclaimed_blocks.push(block);
    }

    pub fn allocate_large_object<A: Allocator>(&self, class: &Class) -> Result<HeapPointer, AllocError> {
        let current_alloc = self.current_allocation.fetch_add(class.object_size(), SeqCst);
        if current_alloc + class.object_size() <= self.max_size {
            let layout = Layout::array::<u8>(class.object_size()).unwrap();
            let ptr = Global.allocate_zeroed(layout)?;
            Ok(HeapPointer::new(class, ptr.cast().as_ptr()))
        } else {
            self.current_allocation.fetch_sub(class.object_size(), SeqCst);
            Err(AllocError {})
        }
    }

    pub fn get_max_size(&self) -> usize {
        self.max_size
    }
}

mod tests {
    use crate::gc::global_allocator::GlobalAllocator;
    use crate::gc::block::{BLOCK_SIZE, Block};
    use std::sync::atomic::Ordering::{Relaxed, SeqCst};
    use std::thread;
    use std::sync::Arc;
    use std::thread::JoinHandle;
    use std::sync::atomic::AtomicUsize;
    use std::time::Duration;

    #[test]
    pub fn constructor() {
        let global_alloc = GlobalAllocator::new(1);
        assert_eq!(global_alloc.max_size, BLOCK_SIZE);
        assert_eq!(0, global_alloc.current_allocation.load(Relaxed))
    }

    #[test]
    pub fn allocate() {
        let global_alloc = GlobalAllocator::new(2);

        let block = global_alloc.allocate_block().unwrap();
        let second_block = global_alloc.allocate_block().unwrap();

        let third_attempt = global_alloc.allocate_block();
        assert!(third_attempt.is_err());
    }

    #[test]
    pub fn large_allocation() {
        let max_size = 20_000;
        let global_alloc = GlobalAllocator::new(max_size);

        // let mut blocks: Vec<Box<Block>> = vec![];

        loop {
            let block = global_alloc.allocate_block();
            if block.is_err() {
                assert_eq!(global_alloc.max_size, max_size * BLOCK_SIZE);
                assert_eq!(global_alloc.current_allocation.load(SeqCst), global_alloc.max_size);
                break;
            }
        }
    }

    #[test]
    pub fn allocate_from_multiple_threads() {
        let max_size = 50_000;

        let global_alloc = Arc::new(GlobalAllocator::new(max_size));

        let mut handles = vec![];

        for _ in 0..16 {
            let allocator = global_alloc.clone();
            let handle = thread::spawn(move || {
                let mut blocks: Vec<Box<Block>> = vec![];
                'allocating: loop {
                    let block = allocator.allocate_block();
                    match block {
                        Ok(b) => {
                            blocks.push(b);
                        },
                        Err(_) => {
                            break 'allocating;
                        }
                    }
                }
                std::mem::drop(blocks)
            });
            handles.push(handle);
        }

        for handle in handles {
            handle.join().unwrap();
        }
    }
}
