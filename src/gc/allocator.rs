use crate::gc::block::Block;
use crate::gc::global_allocator::GlobalAllocator;
use std::thread::current;
use std::sync::Arc;
use std::alloc::{AllocError, Global};
use crate::pointers::heap_pointer::HeapPointer;
use crate::object::class::Class;
use crate::object::object_size::ObjectClassification;

struct Allocator {
    global_allocator: Arc<GlobalAllocator>,
    current_block: Option<Box<Block>>,
    overflow_block: Option<Box<Block>>,
    blocks_in_use: Vec<Box<Block>>
}

impl Allocator {
    pub fn new(global_allocator: Arc<GlobalAllocator>) -> Self {
        Allocator {
            global_allocator,
            current_block: None,
            overflow_block: None,
            blocks_in_use: vec![]
        }
    }

    pub fn allocate(&mut self, class: &Class) -> HeapPointer {
        unreachable!()
    }
}