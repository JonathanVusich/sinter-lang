use crate::gc::block::LINES_PER_BLOCK;

pub (crate) struct ByteMap {
    map: [u8; LINES_PER_BLOCK]
}

impl ByteMap {
    pub fn new() -> Self {
        ByteMap { map: [0; LINES_PER_BLOCK] }
    }

    pub fn set_ref_count(&mut self, line: usize, ref_count: u8) {
        self.map[line] = ref_count;
    }

    pub fn get_ref_count(&self, line: usize) -> u8 {
        self.map[line]
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

