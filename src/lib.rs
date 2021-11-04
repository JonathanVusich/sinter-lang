#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(unused_variables)]
#![feature(test)]
#![feature(allocator_api)]
#![feature(new_uninit)]

pub mod stack;
pub mod opcode;
pub mod gc_arena;
pub mod memory;
pub mod gc;
pub mod pointers;
pub mod object;