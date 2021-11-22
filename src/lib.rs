#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(unused_variables)]
#![feature(test)]
#![feature(allocator_api)]
#![feature(new_uninit)]
// #![feature(adt_const_params)]
#![feature(generic_const_exprs)]
// #![feature(fn_traits)]

pub mod stack;
pub mod opcode;
pub mod gc;
pub mod pointers;
pub mod object;
pub mod util;
pub mod values;