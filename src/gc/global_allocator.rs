use crate::gc::block::Block;
use std::sync::atomic::{AtomicU64, AtomicUsize, Ordering};
use std::alloc::AllocError;
use std::sync::atomic::Ordering::{SeqCst, Relaxed};
use std::mem::MaybeUninit;

struct GlobalAllocator {
    max_size: usize,
    current_allocation: AtomicUsize
}

impl GlobalAllocator {

    pub fn new(size: usize) -> Self {
        GlobalAllocator {
            max_size: size,
            current_allocation: AtomicUsize::new(0)
        }
    }

    pub fn allocate_block(&mut self) -> Result<Box<Block>, AllocError> {
        let block_size = std::mem::size_of::<Block>();
        let current_alloc = self.current_allocation.load(SeqCst);
        if self.max_size - current_alloc > block_size {
            let result = self.current_allocation
                .compare_exchange(current_alloc, current_alloc + block_size,
                                  SeqCst, Relaxed);

            match result {
                Ok(x) => {
                    let block = Block::try_allocate()?;
                    Ok(block)
                }
                Err(x) => {
                    Err(AllocError {})
                }
            }
        } else {
            Err(AllocError {})
        }
    }
}

mod tests {
    use crate::gc::global_allocator::GlobalAllocator;
    use crate::gc::block::BLOCK_SIZE;
    use std::sync::atomic::Ordering::Relaxed;

    #[test]
    pub fn constructor() {
        let global_alloc = GlobalAllocator::new(BLOCK_SIZE);
        assert_eq!(global_alloc.max_size, BLOCK_SIZE);
        assert_eq!(0, global_alloc.current_allocation.load(Relaxed))
    }

    #[test]
    pub fn allocate() {
        let mut global_alloc = GlobalAllocator::new(BLOCK_SIZE * 2);

        let block = global_alloc.allocate_block().unwrap();
        let second_block = global_alloc.allocate_block().unwrap();

        let third_attempt = global_alloc.allocate_block();
        assert!(third_attempt.is_err());
    }
}
