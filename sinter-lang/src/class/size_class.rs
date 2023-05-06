use crate::gc::block::{BLOCK_SIZE, LINE_SIZE};

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum SizeClass {
    Small,
    Medium,
    Large,
}

impl SizeClass {
    pub fn from(object_size: usize) -> Self {
        if object_size <= LINE_SIZE {
            Self::Small
        } else if object_size < (BLOCK_SIZE / 4) {
            Self::Medium
        } else {
            Self::Large
        }
    }
}
