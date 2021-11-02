use crate::gc::block::LINE_SIZE;

#[derive(Eq, PartialEq, Debug)]
pub struct Class {
    object_size: usize,
    small_object: bool,
    mark_word: u8,
}

impl Class {

    pub fn new(object_size: usize) -> Class {
        let small_object = object_size < LINE_SIZE;
        let mark_word: u8 = if small_object {
            0b00000100
        } else {
            0b00010100
        };

        Class {
            object_size,
            small_object,
            mark_word
        }
    }

    pub fn mark_word(&self) -> u8 {
        self.mark_word
    }

    pub fn object_size(&self) -> usize {
        self.object_size
    }

    pub fn is_small_object(&self) -> bool {
        self.small_object
    }
}

