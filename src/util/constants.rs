use std::mem::size_of;

pub const WORD: usize = size_of::<usize>();
pub const DOUBLE_WORD: usize = size_of::<usize>() * 2;