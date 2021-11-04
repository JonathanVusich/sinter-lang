use crate::gc::block::Block;

struct Allocator {
    current_block: Box<Block>,
    blocks_in_use: Vec<Box<Block>>

}