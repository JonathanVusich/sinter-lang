use std::alloc::{Allocator, AllocError, Layout};
use std::mem::MaybeUninit;
use std::path::Component::Prefix;
use std::ptr::{NonNull, slice_from_raw_parts};
use std::sync::atomic::AtomicPtr;
use std::sync::atomic::Ordering::SeqCst;
use std::sync::Once;

use region::{alloc, Allocation, Protection};
use winapi::um::memoryapi::{
    VirtualAlloc, VirtualFree, VirtualLock, VirtualProtect, VirtualQuery, VirtualUnlock,
};
use winapi::um::sysinfoapi::{GetNativeSystemInfo, SYSTEM_INFO};
use winapi::um::winnt::{MEM_COMMIT, MEM_RELEASE, MEM_RESERVE, MEMORY_BASIC_INFORMATION};

use crate::gc::block::Block;
use crate::heap::page;
use crate::heap::region::Error::{AllocationFailed, InvalidSize};
use crate::os;

pub struct Region {
    start: *const u8,
    end: *const u8,
    cursor: AtomicPtr<u8>
}

#[derive(Debug)]
pub enum Error {
    InvalidSize,
    AllocationFailed
}

impl Region {

    pub fn new(size: usize) -> Result<Self, Error> {
        if size == 0 {
            return Err(InvalidSize)
        }
        let size = page::ceil(size as *const ()) as usize;

        let region_ptr = unsafe { os::alloc(std::ptr::null::<>(), size) };
        let ptr = region_ptr.cast::<u8>();
        let end_ptr = unsafe { ptr.add(size) };

        if ptr.is_null() {
            return Err(AllocationFailed)
        }

        Ok(Self {
            start: ptr,
            end: end_ptr,
            cursor: AtomicPtr::new(ptr as *mut u8)
        })
    }
}

mod tests {
    use crate::heap::region::Region;

    #[test]
    pub fn huge_region() {
        let size = 16 * 1024 * 1024 * 1024;
        let region = Region::new(size).unwrap();
        assert_eq!(region.cursor.into_inner() as *const u8, region.start);
        assert_eq!(unsafe { region.start.add(size) }, region.end);
    }
}
