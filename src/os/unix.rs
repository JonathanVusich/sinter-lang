use std::sync::Once;
use libc::{_SC_PAGESIZE, sysconf, posix_memalign, MAP_ANONYMOUS, MAP_SHARED, MAP_PRIVATE, PROT_READ, PROT_WRITE, c_void, off_t, MAP_ANON, mmap};
use crate::gc::block::BLOCK_SIZE;

pub unsafe fn alloc(size: usize) -> *mut u8 {
    let mut allocation: *mut c_void = std::ptr::null_mut();
    posix_memalign(&mut allocation, BLOCK_SIZE, size);
    allocation as *mut u8
}

pub fn page_size() -> usize {
    static mut PAGE_SIZE: usize = 0;
    static INIT: Once = Once::new();

    unsafe {
        INIT.call_once(|| PAGE_SIZE = sysconf(_SC_PAGESIZE) as usize);
        PAGE_SIZE
    }
}