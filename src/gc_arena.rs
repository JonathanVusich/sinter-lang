use std::alloc::{self, Layout};
use std::collections::HashMap;
use std::ptr::NonNull;

use crate::memory::MemoryBlock;

pub (crate) struct GCArena {
    regions: Vec<MemoryBlock>,
    large_objects: Vec<u8>
}

impl GCArena {
    pub fn new() -> Self {
        GCArena {
            regions: vec![],
            large_objects: vec![]
        }
    }
}