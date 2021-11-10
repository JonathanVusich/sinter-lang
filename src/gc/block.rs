use crate::gc::byte_map::ByteMap;
use crate::object::class::Class;
use crate::pointers::heap_pointer::HeapPointer;
use crate::gc::block_state::BlockState;
use std::alloc::AllocError;
use std::ptr::addr_of;
use std::ptr::addr_of_mut;
use crate::object::object_size::ObjectClassification;

/// The number of bytes in a block.
pub const BLOCK_SIZE: usize = 1024 * 1024;

/// The number of bytes in single line.
pub const LINE_SIZE: usize = 256;

/// The number of lines in a block.
/// We have to subtract three lines to make room for the line map and for the cursors.
pub const LINES_PER_BLOCK: usize = (BLOCK_SIZE / LINE_SIZE) - 17;

/// The number of bytes in a block.
pub const BYTES_PER_BLOCK: usize = LINE_SIZE * LINES_PER_BLOCK;

pub struct Block {
    line_map: ByteMap,
    lines: [u8; BYTES_PER_BLOCK],
    start_address: *const u8,
    max_address: *const u8,
    cursor: *mut u8,
    block_state: BlockState
}

unsafe impl Send for Block {}
unsafe impl Sync for Block {}

impl Block {

    pub fn try_allocate() -> Result<Box<Block>, AllocError> {
        let mut block = Box::<Block>::try_new_uninit()?;
        let block_ptr = block.as_mut_ptr();

        unsafe {
            let lines_ptr: *mut [u8; BYTES_PER_BLOCK] = addr_of_mut!((*block_ptr).lines);
            let start_address_ptr: *mut *const u8 = addr_of_mut!((*block_ptr).start_address);
            let end_address_ptr: *mut *const u8 = addr_of_mut!((*block_ptr).max_address);
            let cursor_ptr: *mut *mut u8 = addr_of_mut!((*block_ptr).cursor);
            let block_state_ptr: *mut BlockState = addr_of_mut!((*block_ptr).block_state);

            start_address_ptr.write((*lines_ptr).as_ptr());
            end_address_ptr.write((*lines_ptr).as_ptr().add(BYTES_PER_BLOCK));
            cursor_ptr.write((*lines_ptr).as_mut_ptr());
            block_state_ptr.write(BlockState::Free);
            let safe_block = block.assume_init();
            Ok(safe_block)
        }
    }

    pub fn allocate(&mut self, class: &Class) -> Option<HeapPointer> {
        let object_start = self.cursor;
        let end_cursor = end_cursor(self.cursor, class.object_size());

        if object_fits(self.max_address, end_cursor) {
            self.cursor = end_cursor;
            let heap_pointer = HeapPointer::new(class, object_start);

            let start_line = self.line_for_address(object_start);
            if class.object_classification() == ObjectClassification::Small {
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
            address.offset_from(self.start_address) / LINE_SIZE as isize
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

mod tests {

    extern crate test;

    use crate::gc::block::{Block, BLOCK_SIZE, BYTES_PER_BLOCK, LINES_PER_BLOCK, LINE_SIZE};
    use crate::object::class::Class;
    use crate::gc::block_state::BlockState;

    #[test]
    pub fn size() {
        assert!(BLOCK_SIZE < LINE_SIZE + std::mem::size_of::<Block>());
        assert!(BLOCK_SIZE > std::mem::size_of::<Block>());
    }

    #[test]
    pub fn constructor() {
        let mut block = Block::try_allocate().unwrap();

        for i in 0..LINES_PER_BLOCK {
            block.line_map.increment(i);
            assert_eq!(block.line_map.get_ref_count(i), 1);
            block.line_map.decrement(i);
            assert_eq!(block.line_map.get_ref_count(i), 0);
        }

        for val in block.lines.as_mut_slice() {
            assert_eq!(0, *val);
        }

        assert_eq!(block.start_address, block.lines.as_ptr());
        assert_eq!(block.max_address, block.lines.as_ptr_range().end);
        assert_eq!(block.cursor, block.start_address as *mut u8);
        assert_eq!(block.block_state, BlockState::Free);
    }

    #[test]
    pub fn simple_allocation() {
        let mut block = Block::try_allocate().unwrap();

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
        let mut block = Block::try_allocate().unwrap();

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
        let mut block = Block::try_allocate().unwrap();

        let class_half_line = Class::new(LINE_SIZE / 2);
        let class_full_line = Class::new(LINE_SIZE);

        let pointer = block.allocate(&class_half_line).unwrap();

        assert!(!pointer.spans_lines());

        let large_pointer = block.allocate(&class_full_line).unwrap();

        assert!(large_pointer.spans_lines());

        let another_small_pointer = block.allocate(&class_half_line).unwrap();

        assert!(!another_small_pointer.spans_lines());

        let another_large_pointer = block.allocate(&class_full_line).unwrap();

        assert!(another_large_pointer.spans_lines());
    }

    #[test]
    pub fn allocate_single_object() {
        let mut block = Block::try_allocate().unwrap();

        let class = Class::new(BYTES_PER_BLOCK);

        let heap_pointer = block.allocate(&class);
        assert!(heap_pointer.is_some());

        let small_class = Class::new(1);

        let no_pointer = block.allocate(&small_class);

        assert!(no_pointer.is_none());
    }
}