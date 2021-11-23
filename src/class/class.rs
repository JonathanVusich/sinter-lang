use crate::gc::block::{LINE_SIZE, BLOCK_SIZE};
use crate::class::size_class::SizeClass;
use crate::pointers::heap_pointer::HeapPointer;

#[derive(Eq, PartialEq, Debug)]
pub struct Class {
    object_size: usize,
    object_classification: SizeClass,
    mark_word: u8,
    static_roots: Vec<HeapPointer>
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
            object_classification: SizeClass::from(object_size),
            mark_word,
            static_roots: vec![]
        }
    }

    pub fn static_roots(&self) -> &Vec<HeapPointer> {
        &self.static_roots
    }

    pub fn mark_word(&self) -> u8 {
        self.mark_word
    }

    pub fn object_size(&self) -> usize {
        self.object_size
    }

    pub fn size_class(&self) -> SizeClass {
        self.object_classification
    }
}

