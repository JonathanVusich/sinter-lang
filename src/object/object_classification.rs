use crate::gc::block::{LINE_SIZE, BLOCK_SIZE};

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum ObjectClassification {
    Small,
    Medium,
    Large
}

impl ObjectClassification {

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