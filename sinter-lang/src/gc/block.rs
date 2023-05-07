use std::ptr::addr_of;
use std::ptr::addr_of_mut;

use crate::class::class::Class;
use crate::class::size_class::SizeClass;
use crate::gc::block_state::BlockState;
use crate::gc::byte_map::ByteMap;
use crate::pointers::heap_pointer::HeapPointer;
use crate::pointers::pointer::Pointer;

/// The number of bytes in a block.
pub const BLOCK_SIZE: usize = 32 * 1024;

/// The number of bytes in single line.
pub const LINE_SIZE: usize = 256;

/// The number of lines in a block.
/// We have to subtract three lines to make room for the line map and for the cursors.
pub const LINES_PER_BLOCK: usize = (BLOCK_SIZE / LINE_SIZE) - 1;

/// The number of bytes in a block.
pub const BYTES_PER_BLOCK: usize = LINE_SIZE * LINES_PER_BLOCK;

/// The mask to apply to go from a heap pointer to the block pointer.
/// This is HIGHLY UNSAFE! There must be strict tests to ensure that this
/// invariant is maintained.
pub const BLOCK_BYTEMASK: u64 = !(BLOCK_SIZE - 1) as u64;

#[repr(C, align(32768))]
pub struct Block {
    lines: [u8; BYTES_PER_BLOCK],
    line_map: ByteMap,
    start_address: *const u8,
    max_address: *const u8,
    cursor: *mut u8,
    block_state: BlockState,
}

unsafe impl Send for Block {}
unsafe impl Sync for Block {}

impl Block {
    pub unsafe fn initialize(block: *mut Block) {
        let lines_ptr: *mut [u8; BYTES_PER_BLOCK] = addr_of_mut!((*block).lines);
        let start_address_ptr: *mut *const u8 = addr_of_mut!((*block).start_address);
        let end_address_ptr: *mut *const u8 = addr_of_mut!((*block).max_address);
        let cursor_ptr: *mut *mut u8 = addr_of_mut!((*block).cursor);
        let block_state_ptr: *mut BlockState = addr_of_mut!((*block).block_state);

        start_address_ptr.write((*lines_ptr).as_ptr());
        end_address_ptr.write((*lines_ptr).as_ptr().add(BYTES_PER_BLOCK));
        cursor_ptr.write((*lines_ptr).as_mut_ptr());
        block_state_ptr.write(BlockState::Free);
    }

    pub fn allocate(&mut self, class: &Class) -> Option<HeapPointer> {
        let object_start = self.cursor;
        let end_cursor = end_cursor(self.cursor, class.size);

        if object_fits(self.max_address, end_cursor) {
            self.cursor = end_cursor;
            let heap_pointer = HeapPointer::new(class, object_start);

            self.mark_instance(class, object_start);
            Some(heap_pointer)
        } else {
            None
        }
    }

    pub fn mark_instance(&mut self, class: &Class, instance: *mut u8) {
        let start_line = self.line_for_address(instance);
        if class.size_class == SizeClass::Small {
            self.line_map.mark(start_line as usize)
        } else {
            let end_line = self.line_for_address(end_of_object(instance, class.size));
            for i in start_line..=end_line {
                self.line_map.mark(i as usize);
            }
        }
    }

    #[inline(always)]
    fn line_for_address(&self, address: *mut u8) -> isize {
        unsafe { address.offset_from(self.start_address) / LINE_SIZE as isize }
    }

    #[inline(always)]
    fn end_cursor(&self, object_size: usize) -> *mut u8 {
        unsafe { self.cursor.add(object_size) }
    }
}

#[inline(always)]
fn object_fits(end_address: *const u8, end_cursor: *mut u8) -> bool {
    unsafe { end_address.offset_from(end_cursor) >= 0 }
}

#[inline(always)]
fn end_cursor(cursor: *mut u8, object_size: usize) -> *mut u8 {
    unsafe { cursor.add(object_size) }
}

#[inline(always)]
fn end_of_object(start_cursor: *mut u8, object_size: usize) -> *mut u8 {
    unsafe { start_cursor.add(object_size - 1) }
}

mod tests {

    use std::mem::MaybeUninit;
    use std::ptr::addr_of;

    use crate::class::class::Class;
    use crate::class::class_builder::ClassBuilder;
    use crate::gc::block::{
        Block, BLOCK_BYTEMASK, BLOCK_SIZE, BYTES_PER_BLOCK, LINES_PER_BLOCK, LINE_SIZE,
    };
    use crate::gc::block_state::BlockState;
    use crate::heap::region::Region;
    use crate::pointers::pointer::Pointer;
    use crate::pool::internal_string::InternalString;

    #[test]
    pub fn size() {
        assert_eq!(BLOCK_SIZE, std::mem::size_of::<Block>());
    }

    #[test]
    pub fn uninit_size() {
        assert_eq!(BLOCK_SIZE, std::mem::size_of::<MaybeUninit<Block>>());
    }

    #[test]
    pub fn constructor() {
        let region = Region::new(BLOCK_SIZE).unwrap();
        let mut block = region.allocate_block().unwrap();

        for i in 0..LINES_PER_BLOCK {
            // block.line_map.mark(i);
            // assert_eq!(block.line_map.get_ref_count(i), 1);
            // block.line_map.clear(i);
            // assert_eq!(block.line_map.get_ref_count(i), 0);
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
        let region = Region::new(BLOCK_SIZE).unwrap();
        let mut block = region.allocate_block().unwrap();

        let mut start_cursor = block.cursor;

        let class = ClassBuilder::new()
            .set_size(16)
            .build(|val| InternalString(0));

        let pointer = block.allocate(&class).unwrap();

        // Compute offset between pointers
        unsafe {
            let offset = block.cursor.offset_from(start_cursor);
            assert_eq!(16, offset);
        }

        let larger_class = ClassBuilder::new()
            .set_size(32)
            .build(|val| InternalString(0));

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
        let region = Region::new(BLOCK_SIZE).unwrap();
        let mut block = region.allocate_block().unwrap();

        let class = ClassBuilder::new()
            .set_size(16)
            .build(|val| InternalString(0));

        let pointer = block.allocate(&class).unwrap();

        let raw_ptr = pointer.start_address();

        raw_ptr.write(123);
        raw_ptr.add(1).write(234);

        assert_eq!(123, raw_ptr.read());
        assert_eq!(234, raw_ptr.add(1).read())
    }

    #[test]
    pub fn spans_lines() {
        let region = Region::new(BLOCK_SIZE).unwrap();
        let mut block = region.allocate_block().unwrap();

        let class_half_line = ClassBuilder::new()
            .set_size(LINE_SIZE / 2)
            .build(|val| InternalString(0));
        let class_full_line = ClassBuilder::new()
            .set_size(LINE_SIZE)
            .build(|val| InternalString(0));

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
        let region = Region::new(BLOCK_SIZE).unwrap();
        let mut block = region.allocate_block().unwrap();

        let class = ClassBuilder::new()
            .set_size(BYTES_PER_BLOCK)
            .build(|val| InternalString(0));

        let heap_pointer = block.allocate(&class);
        assert!(heap_pointer.is_some());

        let small_class = ClassBuilder::new()
            .set_size(BYTES_PER_BLOCK)
            .build(|val| InternalString(0));

        let no_pointer = block.allocate(&small_class);

        assert!(no_pointer.is_none());
    }

    #[test]
    pub fn block() {
        let region = Region::new(BLOCK_SIZE).unwrap();
        let mut block = region.allocate_block().unwrap();

        let class = ClassBuilder::new()
            .set_size(16)
            .build(|val| InternalString(0));

        let raw_block_ptr: *mut Block = &mut *block;
        let block_pointer = Pointer::from_raw(raw_block_ptr);

        loop {
            let heap_pointer = block.allocate(&class);
            match heap_pointer {
                Some(ptr) => {
                    let ptr_block = ptr.block();
                    assert!(ptr_block == block_pointer);
                }
                None => return,
            }
        }
    }
}
