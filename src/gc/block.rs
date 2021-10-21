use crate::gc::byte_map::ByteMap;

/// The number of bytes in a block.
pub const BLOCK_SIZE: usize = 32768;

/// The number of bytes in single line.
pub const LINE_SIZE: usize = 128;

/// The number of lines in a block.
pub const LINES_PER_BLOCK: usize = (BLOCK_SIZE / LINE_SIZE) - 3; // Subtract two for the line map

/// The number of 8 byte chunks in a block.
pub const CHUNKS_PER_BLOCK: usize = (LINE_SIZE * LINES_PER_BLOCK) / 8;

#[repr(align(32768))]
pub struct Block {
    line_map: ByteMap,
    lines: [u64; CHUNKS_PER_BLOCK], // Aligned pointer for faster accesses
    cursor: *mut u64,
    max_address: *mut u64
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

impl Block {
    pub fn boxed() -> Box<Block> {
        let mut block: Box<Block> = Box::default();
        block.cursor = block.lines.as_mut_ptr();
        unsafe {
            block.max_address = block.cursor.add(CHUNKS_PER_BLOCK);
        }
        block
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
        // print!("{:?}", block.cursor as usize);
    }
}