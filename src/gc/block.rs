use std::alloc::Layout;

/// The number of bytes in a block.
pub const BLOCK_SIZE: usize = 8 * 1024;

/// The number of bytes in single line.
pub const LINE_SIZE: usize = 128;

/// The number of lines in a block.
pub const LINES_PER_BLOCK: usize = BLOCK_SIZE / LINE_SIZE;



unsafe fn heap_layout_for_block() -> Layout {
    Layout::from_size_align_unchecked(BLOCK_SIZE, BLOCK_SIZE)
}

