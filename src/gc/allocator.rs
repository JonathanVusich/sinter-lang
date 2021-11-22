use std::alloc::{Allocator, AllocError, Global, Layout};
use std::sync::Arc;

use crate::gc::block::Block;
use crate::gc::garbage_collector::GarbageCollector;
use crate::object::class::Class;
use crate::object::size_class::SizeClass;
use crate::pointers::heap_pointer::HeapPointer;

struct ThreadAllocator {
    garbage_collector: Arc<GarbageCollector>,
    current_block: Option<Box<Block>>,
    overflow_block: Option<Box<Block>>,
    blocks_in_use: Vec<Box<Block>>
}

impl ThreadAllocator {
    pub fn new(garbage_collector: Arc<GarbageCollector>) -> Self {
        ThreadAllocator {
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
            SizeClass::Large => self.allocate_large_object(class),
        };
        ptr
    }

    fn get_overflow_block(&mut self) -> &mut Box<Block> {
        self.overflow_block.get_or_insert(self.allocate_block())
    }

    fn get_current_block(&mut self) -> &mut Box<Block> {
        self.current_block.get_or_insert(self.allocate_block())
    }

    fn allocate_block(&self) -> Box<Block> {
        return match Block::boxed() {
            Ok(block) => block,
            Err(error) => {
                self.garbage_collector.collect();
                Block::boxed().unwrap() // Don't attempt to continue if this fails.
            }
        }
    }

    fn allocate_large_object(&self, class: &Class) -> HeapPointer {
        let layout = Layout::array::<u8>(class.object_size()).unwrap();

        let ptr = match Global.allocate(layout) {
            Ok(ptr) => ptr,
            Err(error) => {
                self.garbage_collector.collect();
                Global.allocate(layout).unwrap() // Don't attempt to continue if this fails.
            }
        };

        HeapPointer::new(class, ptr.cast().as_ptr())
    }

    fn deallocate_large_object(&self, object: HeapPointer) {
        let class = object.class_pointer();
        let layout = Layout::array::<u8>(class.object_size()).unwrap();
        unsafe { Global.deallocate(object.to_raw(), layout); }
    }
}