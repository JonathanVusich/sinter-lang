#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(unused_variables)]
#![feature(test)]
#![feature(allocator_api)]
#![feature(new_uninit)]
#![feature(generic_const_exprs)]
#![feature(type_alias_impl_trait)]

pub mod stack;
pub mod opcode;
pub mod gc;
pub mod pointers;
pub mod object;
pub mod util;
pub mod values;