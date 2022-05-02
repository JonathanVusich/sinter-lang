#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(unused_variables)]
#![feature(test)]
#![feature(allocator_api)]
#![feature(new_uninit)]
#![feature(type_alias_impl_trait)]
#![feature(nonnull_slice_from_raw_parts)]
#![feature(buf_read_has_data_left)]
#![feature(fn_traits)]

pub mod opcode;
pub mod gc;
pub mod pointers;
pub mod class;
pub mod util;
pub mod heap;
pub mod os;
pub mod function;
pub mod vm;
pub mod bytes;
pub mod types;
pub mod pool;
pub mod traits;
pub mod errors;
pub mod token;