/// The number of bytes in a block.
pub const BLOCK_SIZE: usize = 8 * 1024;

/// The number of bytes in single line.
pub const LINE_SIZE: usize = 128;

/// The number of lines in a block.
pub const LINES_PER_BLOCK: usize = BLOCK_SIZE / LINE_SIZE;

/// The maximum number of holes a block can have. Consecutive empty lines count
/// as one hole, so the max is half the number of lines (used -> empty -> used,
/// etc).
pub const MAX_HOLES: usize = LINES_PER_BLOCK / 2;

