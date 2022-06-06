use crate::gc::block::BLOCK_SIZE;
use libc::{
    c_void, mmap, off_t, posix_memalign, sysconf, MAP_ANON, MAP_ANONYMOUS, MAP_PRIVATE, MAP_SHARED,
    PROT_READ, PROT_WRITE, _SC_PAGESIZE,
};
use std::sync::Once;

pub unsafe fn alloc(size: usize) -> *mut u8 {
    let mut allocation: *mut c_void = std::ptr::null_mut();
    posix_memalign(&mut allocation, BLOCK_SIZE, size);

    let ptr = allocation as *mut u8;

    if (ptr as usize) % BLOCK_SIZE != 0 {
        panic!("The allocated address is not aligned to the block size!")
    }

    ptr
}

pub fn page_size() -> usize {
    static mut PAGE_SIZE: usize = 0;
    static INIT: Once = Once::new();

    unsafe {
        INIT.call_once(|| PAGE_SIZE = sysconf(_SC_PAGESIZE) as usize);
        PAGE_SIZE
    }
}
