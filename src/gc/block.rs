use crate::gc::byte_map::ByteMap;
use crate::object::class::Class;
use crate::pointers::heap_pointer::HeapPointer;

/// The number of bytes in a block.
pub const BLOCK_SIZE: usize = 32768;

/// The number of bytes in single line.
pub const LINE_SIZE: usize = 128;

/// The number of lines in a block.
/// We have to subtract three lines to make room for the line map and for the cursors.
pub const LINES_PER_BLOCK: usize = (BLOCK_SIZE / LINE_SIZE) - 3;

/// The number of 8 byte chunks in a block.
pub const CHUNKS_PER_BLOCK: usize = (LINE_SIZE * LINES_PER_BLOCK) / 8;

#[repr(align(32768))]
pub struct Block {
    line_map: ByteMap,
    lines: [u64; CHUNKS_PER_BLOCK], // Aligned pointer for faster accesses
    cursor: *mut u64,
    max_address: *mut u64
}

impl Block {
    pub fn new() -> Box<Block> {
        let mut block: Box<Block> = Box::default();
        block.cursor = block.lines.as_mut_ptr();
        unsafe {
            block.max_address = block.cursor.add(CHUNKS_PER_BLOCK);
        }
        block
    }

    pub fn allocate(&mut self, class: &Class) -> Option<HeapPointer> {
        let start_cursor = self.cursor;
        let end_cursor = self.end_cursor(class.object_size());

        if (end_cursor as usize) < (self.max_address as usize) {
            self.cursor = end_cursor;
            let heap_pointer = HeapPointer::new(class, start_cursor);
            Some(heap_pointer)
        } else {
            None
        }
    }

    fn end_cursor(&self, object_size: usize) -> *mut u64 {
        unsafe {
            self.cursor.add(object_size)
        }
    }
}

impl Default for Block {
    fn default() -> Self {
        Block {
            line_map: ByteMap::new(),
            lines: [0; CHUNKS_PER_BLOCK],
            cursor: std::ptr::null_mut(),
            max_address: std::ptr::null_mut()
        }
    }
}



mod tests {
    use crate::gc::block::{Block, BLOCK_SIZE};
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

        let class = Class::new(2);

        let pointer = block.allocate(&class).unwrap();

        // Compute offset between pointers
        unsafe {
            let offset = block.cursor.offset_from(start_cursor);
            assert_eq!(2, offset);
        }

        let larger_class = Class::new(4);

        start_cursor = block.cursor;
        let second_pointer = block.allocate(&larger_class).unwrap();

        // Compute offset between pointers
        unsafe {
            let offset = block.cursor.offset_from(start_cursor);
            assert_eq!(4, offset);
        }
    }

    #[test]
    pub fn allocation_mutation() {
        let mut block = Block::new();

        let class = Class::new(2);

        let pointer = block.allocate(&class).unwrap();

        let raw_ptr = pointer.start_address();

        unsafe {
            raw_ptr.write(123);
            raw_ptr.add(1).write(234);

            assert_eq!(123, raw_ptr.read());
            assert_eq!(234, raw_ptr.offset(1).read())
        }
    }
}