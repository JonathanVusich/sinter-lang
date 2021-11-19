use std::alloc::{AllocError, Global};
use std::sync::Arc;
use std::thread::current;

use crate::gc::block::Block;
use crate::gc::garbage_collector::GarbageCollector;
use crate::gc::global_allocator::GlobalAllocator;
use crate::object::class::Class;
use crate::object::size_class::SizeClass;
use crate::pointers::heap_pointer::HeapPointer;

struct Allocator {
    global_allocator: Arc<GlobalAllocator>,
    garbage_collector: Arc<GarbageCollector>,
    current_block: Option<Box<Block>>,
    overflow_block: Option<Box<Block>>,
    blocks_in_use: Vec<Box<Block>>
}

impl Allocator {
    pub fn new(global_allocator: Arc<GlobalAllocator>, garbage_collector: Arc<GarbageCollector>) -> Self {
        Allocator {
            global_allocator,
            garbage_collector,
            current_block: None,
            overflow_block: None,
            blocks_in_use: vec![]
        }
    }

    pub fn allocate(&mut self, class: &Class) -> HeapPointer {
        let ptr = match class.size_class() {
            SizeClass::Small | SizeClass::Medium => {
                let block = self.get_current_block();
                let possible_alloc = block.allocate(class);

                match possible_alloc {
                    Some(ptr) => ptr,
                    None => {
                        let overflow_block = self.get_overflow_block();
                        let overflow_alloc = overflow_block.allocate(class);
                        let pointer: HeapPointer = match overflow_alloc {
                            Some(overflow_ptr) => {
                                overflow_ptr
                            },
                            None => {
                                self.blocks_in_use.push(self.overflow_block.take().unwrap());
                                let new_overflow = self.get_overflow_block();
                                new_overflow.allocate(class).unwrap()
                            }
                        };
                        return pointer;
                    }
                }
            }
            SizeClass::Large => self.global_allocator.allocate_large_object(class),
        };
        ptr
    }

    fn get_overflow_block(&mut self) -> &mut Box<Block> {
        self.overflow_block.get_or_insert(self.global_allocator.allocate_block())
    }

    fn get_current_block(&mut self) -> &mut Box<Block> {
        self.current_block.get_or_insert(self.global_allocator.allocate_block())
    }
}