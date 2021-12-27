#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(unused_variables)]
#![feature(test)]
#![feature(allocator_api)]
#![feature(new_uninit)]
#![feature(generic_const_exprs)]
#![feature(type_alias_impl_trait)]
#![feature(nonnull_slice_from_raw_parts)]

pub mod opcode;
pub mod gc;
pub mod pointers;
pub mod class;
pub mod util;
pub mod values;
pub mod heap;
pub mod os;
pub mod function;
pub mod vm;