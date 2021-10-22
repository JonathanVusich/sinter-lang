use crate::gc::byte_map::ByteMap;

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
    pub fn boxed() -> Box<Block> {
        let mut block: Box<Block> = Box::default();
        block.cursor = block.lines.as_mut_ptr();
        unsafe {
            block.max_address = block.cursor.add(CHUNKS_PER_BLOCK);
        }
        block
    }

    pub fn allocate(&mut self, object_size: usize) -> Option<*mut u64> {
        unsafe {
            let start_cursor = self.cursor;
            let end_cursor = self.cursor.add(object_size);

            if (end_cursor as usize) < (self.max_address as usize) {
                self.cursor = end_cursor;
                Some(start_cursor)
            } else {
                None
            }
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

    #[test]
    pub fn size() {
        assert_eq!(BLOCK_SIZE, std::mem::size_of::<Block>());
    }

    #[test]
    pub fn constructor() {
        let block = Block::boxed();
        assert_eq!(BLOCK_SIZE, std::mem::size_of::<Block>());
    }

    #[test]
    pub fn simple_allocation() {
        let mut block = Block::boxed();

        let mut start_cursor = block.cursor;

        let pointer = block.allocate(2).unwrap();

        assert_eq!(start_cursor as usize, pointer as usize);

        // Compute offset between pointers
        unsafe {
            let offset = block.cursor.offset_from(start_cursor);
            assert_eq!(2, offset);
        }

        start_cursor = block.cursor;
        let second_pointer = block.allocate(4).unwrap();

        assert_eq!(start_cursor as usize, second_pointer as usize);

        // Compute offset between pointers
        unsafe {
            let offset = block.cursor.offset_from(start_cursor);
            assert_eq!(4, offset);
        }
    }

    #[test]
    pub fn allocation_mutation() {
        let mut block = Block::boxed();

        let pointer = block.allocate(2).unwrap();

        unsafe {
            pointer.write(123);
            pointer.add(1).write(234);

            assert_eq!(123, pointer.read());
            assert_eq!(234, pointer.offset(1).read())
        }
    }
}