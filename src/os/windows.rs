use std::io;
use std::io::Error;
use std::mem::MaybeUninit;
use std::sync::Once;

use winapi::um::memoryapi::{
    VirtualAlloc, VirtualFree, VirtualLock, VirtualProtect, VirtualQuery, VirtualUnlock,
};
use winapi::um::sysinfoapi::{GetNativeSystemInfo, SYSTEM_INFO};
use winapi::um::winnt::{MEM_COMMIT, MEM_RELEASE, MEM_RESERVE, PAGE_READWRITE};
use crate::gc::block::BLOCK_SIZE;

pub unsafe fn alloc(size: usize) -> *mut u8 {
    let allocation = VirtualAlloc(
        std::ptr::null() as winapi::um::winnt::PVOID,
        size,
        MEM_COMMIT | MEM_RESERVE,
        PAGE_READWRITE,
    );

    let ptr = allocation as *mut u8;
    if (ptr as usize) % BLOCK_SIZE != 0 {
        panic!("The allocated address is not aligned to the block size!")
    }

    ptr
}

pub fn page_size() -> usize {
    static INIT: Once = Once::new();
    static mut PAGE_SIZE: usize = 0;

    unsafe {
        INIT.call_once(|| PAGE_SIZE = os_page_size());
        PAGE_SIZE
    }
}

fn os_page_size() -> usize {
    system_info().dwPageSize as usize
}

fn system_info() -> &'static SYSTEM_INFO {
    static INIT: Once = Once::new();
    static mut INFO: MaybeUninit<SYSTEM_INFO> = MaybeUninit::uninit();

    unsafe {
        INIT.call_once(|| GetNativeSystemInfo(INFO.as_mut_ptr()));
        &*INFO.as_ptr()
    }
}