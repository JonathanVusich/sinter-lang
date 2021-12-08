use std::alloc::{Allocator, AllocError, Layout};
use std::mem::MaybeUninit;
use std::path::Component::Prefix;
use std::ptr::{NonNull, slice_from_raw_parts};
use std::sync::atomic::{AtomicPtr, AtomicU8, AtomicUsize};
use std::sync::atomic::Ordering::SeqCst;
use std::sync::Once;

use region::{alloc, Allocation, Protection};
use winapi::um::memoryapi::{
    VirtualAlloc, VirtualFree, VirtualLock, VirtualProtect, VirtualQuery, VirtualUnlock,
};
use winapi::um::sysinfoapi::{GetNativeSystemInfo, SYSTEM_INFO};
use winapi::um::winnt::{MEM_COMMIT, MEM_RELEASE, MEM_RESERVE, MEMORY_BASIC_INFORMATION};

use crate::gc::block::{Block, BLOCK_SIZE};
use crate::heap::page;
use crate::heap::region::Error::{AllocationFailed, SizeTooLarge, SizeTooSmall};
use crate::os;
use crate::pointers::pointer::Pointer;

pub struct Region {
    start: *const u8,
    num_blocks: usize,
    block_cursor: AtomicUsize
}

#[derive(Debug)]
pub enum Error {
    SizeTooSmall,
    SizeTooLarge,
    AllocationFailed
}

impl Region {

    pub fn new(size: usize) -> Result<Self, Error> {
        let num_blocks = size / BLOCK_SIZE;
        if num_blocks == 0 {
            return Err(SizeTooSmall)
        }
        let min_size = num_blocks * BLOCK_SIZE;
        let size = match min_size.checked_add(page::size()) {
            Some(offset) => ((offset - 1) & !(page::size() - 1)),
            None => {
                return Err(SizeTooLarge)
            },
        };

        println!("{}", min_size);
        println!("{}", size);


        let region_ptr = unsafe { os::alloc(std::ptr::null::<>(), size) };
        let ptr = region_ptr.cast::<u8>();

        if ptr.is_null() {
            return Err(AllocationFailed)
        }

        Ok(Self {
            start: ptr,
            num_blocks,
            block_cursor: AtomicUsize::new(0)
        })
    }

    pub fn allocate_block(&self) -> Option<Pointer<Block>> {
        let block_start = self.block_cursor.fetch_add(1, SeqCst);
        if block_start < self.num_blocks {
            let block_ptr: *mut Block = unsafe { self.start.add(block_start) } as _;
            Some(Pointer::from_raw(block_ptr))
        } else {
            None
        }
    }
}

mod tests {
    use crate::gc::block::BLOCK_SIZE;
    use crate::heap::region::Region;

    #[test]
    pub fn huge_region() {
        let size = BLOCK_SIZE * 1024 * 1024;
        let region = Region::new(size).unwrap();
        assert_eq!(region.block_cursor.into_inner(), 0);
        assert_eq!(region.num_blocks, 1024 * 1024);
    }

    #[test]
    pub fn small_region() {
        let size = BLOCK_SIZE * 3;
        let region = Region::new(size + 23).unwrap();
        assert_eq!(region.block_cursor.into_inner(), 0);
        assert_eq!(region.num_blocks, 3);
    }
}
