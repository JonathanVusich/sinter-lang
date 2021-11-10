use crate::gc::block::{Block, BLOCK_SIZE};
use std::sync::atomic::{AtomicU64, AtomicUsize, Ordering};
use std::alloc::{AllocError, Layout, Global, Allocator, alloc_zeroed, System};
use std::sync::atomic::Ordering::{SeqCst, Relaxed, Acquire};
use std::mem::MaybeUninit;
use std::sync::Mutex;
use crate::pointers::heap_pointer::HeapPointer;
use crate::object::class::Class;

pub struct GlobalAllocator {
    max_size: usize,
    current_allocation: AtomicUsize,
    reclaimed_blocks: Mutex<Vec<Box<Block>>>
}


impl GlobalAllocator {

    pub fn new(size: usize) -> Self {
        GlobalAllocator {
            max_size: size * BLOCK_SIZE,
            current_allocation: AtomicUsize::new(0),
            reclaimed_blocks: Mutex::new(vec![])
        }
    }

    pub fn allocate_block(&self) -> Result<Box<Block>, AllocError> {
        let mut reclaimed_blocks = self.reclaimed_blocks.lock().unwrap();
        let possible_block = reclaimed_blocks.pop();
        std::mem::drop(reclaimed_blocks);

        possible_block.map_or_else(move || {
            let current_alloc = self.current_allocation.fetch_add(BLOCK_SIZE, SeqCst);
            if current_alloc + BLOCK_SIZE <= self.max_size {
                let block = Block::try_allocate()?;
                Ok(block)
            } else {
                self.current_allocation.fetch_sub(BLOCK_SIZE, SeqCst);
                Err(AllocError {})
            }
        }, Ok)
    }

    pub fn deallocate_block(&self, block: Box<Block>) {
        self.current_allocation.fetch_sub(BLOCK_SIZE, SeqCst);
        let mut blocks = self.reclaimed_blocks.lock().unwrap();
        blocks.push(block);
    }

    pub fn allocate_large_object(&self, class: &Class) -> Result<HeapPointer, AllocError> {
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

    pub fn deallocate_large_object(&self, object: HeapPointer) {
        let class = object.class_pointer();
        self.current_allocation.fetch_sub(class.object_size(), SeqCst);
        let layout = Layout::array::<u8>(class.object_size()).unwrap();
        unsafe { Global.deallocate(object.to_raw(), layout); }
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
    use crate::object::class::Class;

    #[test]
    pub fn constructor() {
        let global_alloc = GlobalAllocator::new(1);
        assert_eq!(global_alloc.max_size, BLOCK_SIZE);
        assert_eq!(0, global_alloc.current_allocation.load(Relaxed))
    }

    #[test]
    pub fn allocate_and_reuse() {
        let global_alloc = GlobalAllocator::new(2);

        let block = global_alloc.allocate_block().unwrap();
        let second_block = global_alloc.allocate_block().unwrap();

        let third_attempt = global_alloc.allocate_block();
        assert!(third_attempt.is_err());

        global_alloc.deallocate_block(second_block);

        assert_eq!(global_alloc.reclaimed_blocks.lock().unwrap().len(), 1);
        assert_eq!(global_alloc.current_allocation.load(SeqCst), BLOCK_SIZE);

        global_alloc.deallocate_block(block);

        assert_eq!(global_alloc.reclaimed_blocks.lock().unwrap().len(), 2);
        assert_eq!(global_alloc.current_allocation.load(SeqCst), 0);
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

        let global_allocator = GlobalAllocator::new(max_size);
        let large_object = global_allocator.allocate_large_object(&class);

        assert!(large_object.is_ok());
        assert_eq!(global_allocator.current_allocation.load(SeqCst), global_allocator.max_size);

        let actual_object = large_object.unwrap();

        global_allocator.deallocate_large_object(actual_object);

        assert_eq!(global_allocator.current_allocation.load(SeqCst), 0);
    }

    #[test]
    pub fn allocate_too_large_object() {
        let max_size = 1;
        let class = Class::new(BLOCK_SIZE * max_size + 1);
        let global_allocator = GlobalAllocator::new(max_size);
        let large_object = global_allocator.allocate_large_object(&class);

        assert!(large_object.is_err());
        assert_eq!(global_allocator.current_allocation.load(SeqCst), 0);
    }
}
