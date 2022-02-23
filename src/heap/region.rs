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
        let block_num = self.block_cursor.fetch_add(1, SeqCst);
        if block_num < self.num_blocks {
            let start_ptr = self.start.cast::<Block>() as *mut Block;
            let block_ptr: *mut Block = unsafe { start_ptr.add(block_num) };
            unsafe { Block::initialize(block_ptr) };
            Some(Pointer::from_raw(block_ptr))
        } else {
            None
        }
    }
}

mod tests {
    use crate::class::class::Class;
    use crate::class::class_builder::ClassBuilder;
    use crate::gc::block::BLOCK_SIZE;
    use crate::heap::region::Region;
    use crate::pool::internal_string::InternalString;

    #[test]
    pub fn huge_region() {
        let size = BLOCK_SIZE * 256;
        let region = Region::new(size).unwrap();
        assert_eq!(region.block_cursor.into_inner(), 0);
        assert_eq!(region.num_blocks, 256);
    }

    #[test]
    pub fn small_region() {
        let size = BLOCK_SIZE * 3;
        let region = Region::new(size + 23).unwrap();
        assert_eq!(region.block_cursor.into_inner(), 0);
        assert_eq!(region.num_blocks, 3);
    }

    #[test]
    pub fn allocation() {
        let size = BLOCK_SIZE * 2;
        let region = Region::new(size).unwrap();

        let mut block_1 = region.allocate_block().unwrap();
        let block_2 = region.allocate_block().unwrap();

        assert_eq!(region.start, block_1.to_raw().cast());
        assert_eq!(block_1.add(1).to_raw(), block_2.to_raw());

        let empty_block = region.allocate_block();
        assert!(empty_block.is_none());

        let size_class = ClassBuilder::new().set_size(16).build(|val| InternalString(0));

        let heap_ptr = block_1.allocate(&size_class).unwrap();

        heap_ptr.start_address().cast::<u64>().write(123);

        let heap_val = unsafe { region.start.cast::<u64>().add(1).read() };
        assert_eq!(123, heap_val);
    }
}
