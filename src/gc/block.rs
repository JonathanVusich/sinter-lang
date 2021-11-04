use crate::gc::byte_map::ByteMap;
use crate::object::class::Class;
use crate::pointers::heap_pointer::HeapPointer;
use crate::gc::block_state::BlockState;
use std::alloc::AllocError;

/// The number of bytes in a block.
pub const BLOCK_SIZE: usize = 32768;

/// The number of bytes in single line.
pub const LINE_SIZE: usize = 1 << 7;

/// The number of lines in a block.
/// We have to subtract three lines to make room for the line map and for the cursors.
pub const LINES_PER_BLOCK: usize = (BLOCK_SIZE / LINE_SIZE) - 3;

/// The number of bytes in a block.
pub const BYTES_PER_BLOCK: usize = LINE_SIZE * LINES_PER_BLOCK;

#[repr(align(32768))]
pub struct Block {
    line_map: ByteMap,
    lines: [u8; BYTES_PER_BLOCK],
    start_address: *const u8,
    max_address: *const u8,
    cursor: *mut u8,
    block_state: BlockState
}

impl Block {
    pub fn new() -> Box<Block> {
        let mut block: Box<Block> = Box::default();
        block.cursor = block.lines.as_mut_ptr();
        unsafe {
            block.start_address = block.cursor;
            block.max_address = block.cursor.add(BYTES_PER_BLOCK);
        }
        block
    }

    pub fn allocate(&mut self, class: &Class) -> Option<HeapPointer> {
        let object_start = self.cursor;
        let end_cursor = end_cursor(self.cursor, class.object_size());

        if object_fits(self.max_address, end_cursor) {
            self.cursor = end_cursor;
            let heap_pointer = HeapPointer::new(class, object_start);

            let start_line = self.line_for_address(object_start);
            if class.is_small_object() {
                self.line_map.increment(start_line as usize)
            } else {
                let end_line = self.line_for_address(end_of_object(object_start, class.object_size()));
                for i in start_line..=end_line {
                    self.line_map.increment(i as usize);
                }
            }

            Some(heap_pointer)
        } else {
            None
        }
    }

    #[inline(always)]
    fn line_for_address(&self, address: *mut u8) -> isize {
        unsafe {
            address.offset_from(self.start_address) / 128
        }
    }

    #[inline(always)]
    fn end_cursor(&self, object_size: usize) -> *mut u8 {
        unsafe {
            self.cursor.add(object_size)
        }
    }
}

#[inline(always)]
fn object_fits(end_address: *const u8, end_cursor: *mut u8) -> bool {
    unsafe {
        end_address.offset_from(end_cursor) >= 0
    }
}

#[inline(always)]
fn end_cursor(cursor: *mut u8, object_size: usize) -> *mut u8 {
    unsafe {
        cursor.add(object_size)
    }
}

#[inline(always)]
fn end_of_object(start_cursor: *mut u8, object_size: usize) -> *mut u8 {
    unsafe {
        start_cursor.add(object_size - 1)
    }
}


impl Default for Block {
    fn default() -> Self {
        Block {
            line_map: ByteMap::new(),
            lines: [0; BYTES_PER_BLOCK],
            start_address: std::ptr::null(),
            max_address: std::ptr::null_mut(),
            cursor: std::ptr::null_mut(),
            block_state: BlockState::Free
        }
    }
}



mod tests {

    extern crate test;

    use crate::gc::block::{Block, BLOCK_SIZE, BYTES_PER_BLOCK};
    use crate::object::class::Class;

    #[test]
    pub fn size() {
        assert_eq!(BLOCK_SIZE, std::mem::size_of::<Block>());
    }

    #[test]
    pub fn constructor() {
        let block = Block::new();
        assert_eq!(BLOCK_SIZE, std::mem::size_of::<Block>());
    }

    #[test]
    pub fn simple_allocation() {
        let mut block = Block::new();

        let mut start_cursor = block.cursor;

        let class = Class::new(16);

        let pointer = block.allocate(&class).unwrap();

        // Compute offset between pointers
        unsafe {
            let offset = block.cursor.offset_from(start_cursor);
            assert_eq!(16, offset);
        }

        let larger_class = Class::new(32);

        start_cursor = block.cursor;
        let second_pointer = block.allocate(&larger_class).unwrap();

        // Compute offset between pointers
        unsafe {
            let offset = block.cursor.offset_from(start_cursor);
            assert_eq!(32, offset);
        }
    }

    #[test]
    pub fn allocation_mutation() {
        let mut block = Block::new();

        let class = Class::new(16);

        let pointer = block.allocate(&class).unwrap();

        let raw_ptr = pointer.start_address();

        unsafe {
            raw_ptr.write(123);
            raw_ptr.add(1).write(234);

            assert_eq!(123, raw_ptr.read());
            assert_eq!(234, raw_ptr.offset(1).read())
        }
    }

    #[test]
    pub fn spans_lines() {
        let mut block = Block::new();

        let class_64 = Class::new(64);
        let class_128 = Class::new(128);

        let pointer = block.allocate(&class_64).unwrap();

        assert!(!pointer.spans_lines());

        let large_pointer = block.allocate(&class_128).unwrap();

        assert!(large_pointer.spans_lines());

        let another_small_pointer = block.allocate(&class_64).unwrap();

        assert!(!another_small_pointer.spans_lines());

        let another_large_pointer = block.allocate(&class_128).unwrap();

        assert!(another_large_pointer.spans_lines());
    }

    #[test]
    pub fn allocate_single_object() {
        let mut block = Block::new();

        let class = Class::new(BYTES_PER_BLOCK);

        let heap_pointer = block.allocate(&class);
        assert!(heap_pointer.is_some());

        let small_class = Class::new(1);

        let no_pointer = block.allocate(&small_class);

        assert!(no_pointer.is_none());
    }
}