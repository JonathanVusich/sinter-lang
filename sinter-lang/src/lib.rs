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
#![feature(entry_insert)]
#![feature(hash_set_entry)]
#![feature(option_result_contains)]
#![feature(let_chains)]

extern crate core;

pub mod bytes;
pub mod class;
pub mod compiler;
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
