use crate::gc::block::LINES_PER_BLOCK;

pub struct ByteMap {
    map: [u8; LINES_PER_BLOCK],
}

impl ByteMap {
    pub fn new() -> Self {
        ByteMap {
            map: [0; LINES_PER_BLOCK],
        }
    }

    pub fn mark(&mut self, line: usize) {
        self.map[line] = 1;
    }

    pub fn clear(&mut self, line: usize) {
        self.map[line] = 0;
    }

    pub fn is_marked(&self, line: usize) -> bool {
        self.map[line] == 1
    }
}

mod tests {
    use crate::gc::block::LINES_PER_BLOCK;
    use crate::gc::byte_map::ByteMap;

    #[test]
    pub fn size() {
        assert_eq!(std::mem::size_of::<ByteMap>(), LINES_PER_BLOCK);
    }
}
