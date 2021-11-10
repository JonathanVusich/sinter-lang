use crate::gc::block::Block;
use crate::gc::global_allocator::GlobalAllocator;
use std::thread::current;
use std::sync::Arc;
use std::alloc::{AllocError, Global};
use crate::pointers::heap_pointer::HeapPointer;
use crate::object::class::Class;
use crate::object::object_classification::ObjectClassification;

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
        match class.object_classification() {
            ObjectClassification::Small | ObjectClassification::Medium => {
                let block = self.get_current_block();
                let possible_alloc = block.allocate(class);

                possible_alloc.unwrap_or_else(move || {
                    self.get_overflow_block().allocate(class).unwrap_or_else(move || {
                        self.overflow_block.insert(self.allocate_block())
                            .allocate(class)
                            .unwrap()
                    })
                })
            }
            ObjectClassification::Large => {
                self.global_allocator.allocate_large_object(class).unwrap()
            }
        }

    }

    fn get_overflow_block(&mut self) -> &mut Box<Block> {
        self.overflow_block.get_or_insert(self.allocate_block())
    }

    fn get_current_block(&mut self) -> &mut Box<Block> {
        self.current_block.get_or_insert(self.allocate_block())
    }

    fn allocate_block(&self) -> Box<Block> {
        self.global_allocator.allocate_block().unwrap()
    }
}