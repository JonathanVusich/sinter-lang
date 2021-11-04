use crate::gc::block::Block;
use std::sync::atomic::{AtomicU64, AtomicUsize, Ordering};
use std::alloc::AllocError;
use std::sync::atomic::Ordering::SeqCst;
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
        let current_alloc = self.current_allocation.load(Ordering::SeqCst);
        if self.max_size - current_alloc > block_size {
            let result = self.current_allocation
                .compare_exchange(current_alloc, current_alloc + block_size,
                                  Ordering::SeqCst, Ordering::Relaxed);

            match result {
                Ok(x) => {
                    let block: Box<MaybeUninit<Block>> = Box::try_new_uninit()?;
                    let init_block = unsafe { block.assume_init() };
                    Ok(init_block)
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