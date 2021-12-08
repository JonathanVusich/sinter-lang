use std::alloc::{Allocator, AllocError, Layout};
use std::path::Component::Prefix;
use std::ptr::{NonNull, slice_from_raw_parts};
use std::sync::atomic::AtomicPtr;
use std::sync::atomic::Ordering::SeqCst;

use region::{alloc, Allocation, Protection};

use crate::gc::block::Block;

pub struct Region {
    allocation: Allocation,
    cursor: AtomicPtr<u8>
}

impl Region {

    pub fn new(size: usize) -> Self {
        let mut allocation = region::alloc(size, Protection::READ_WRITE).unwrap();
        let cursor = allocation.as_mut_ptr::<u8>();

        Self {
            allocation,
            cursor: AtomicPtr::new(cursor)
        }
    }

    // pub fn allocate() -> Block {
    //
    // }
}

unsafe impl Allocator for Region {

    fn allocate(&self, layout: Layout) -> Result<NonNull<[u8]>, AllocError> {
        loop {
            let current_cursor = self.cursor.load(SeqCst);
            let new_cursor = unsafe { current_cursor.offset(layout.size() as isize) };
            match self.cursor.compare_exchange(current_cursor, new_cursor, SeqCst, SeqCst) {
                Ok(_) => {
                    unsafe {
                        return Ok(NonNull::slice_from_raw_parts(NonNull::new_unchecked(current_cursor), layout.size()))
                    }
                }
                Err(_) => {}
            }
        }
    }

    unsafe fn deallocate(&self, ptr: NonNull<u8>, layout: Layout) {
        todo!()
    }
}

