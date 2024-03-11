#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(unused_variables)]
#![feature(allocator_api)]
#![feature(assert_matches)]
#![feature(error_generic_member_access)]
#![feature(new_uninit)]
#![feature(maybe_uninit_slice)]
#![feature(ptr_sub_ptr)]

extern crate core;

pub mod bytes;
pub mod class;
pub mod errors;
pub mod function;
pub mod gc;
pub mod heap;
pub mod opcode;
pub mod os;
pub mod pointers;
pub mod pool;
pub mod traits;
pub mod types;
pub mod util;
pub mod vm;
