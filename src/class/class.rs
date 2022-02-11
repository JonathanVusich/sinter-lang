use std::ptr::NonNull;
use std::slice::Iter;
use crate::class::field::Field;
use crate::class::references::Reference;

use crate::class::size_class::SizeClass;
use crate::class::version::{CURRENT_VERSION, Version};
use crate::function::method::Method;
use crate::gc::block::{BLOCK_SIZE, LINE_SIZE};
use crate::pointers::heap_pointer::HeapPointer;
use crate::pointers::mark_word::MarkWord;
use crate::pointers::pointer::Pointer;

#[derive(Eq, PartialEq, Debug)]
pub struct Class {
    version: Version,
    object_size: usize,
    object_classification: SizeClass,
    mark_word: MarkWord,

    fields: Vec<Field>,
    references: Vec<Field>,
    methods: Vec<Method>
}

impl Class {

    pub fn new(object_size: usize) -> Class {
        let small_object = object_size < LINE_SIZE;
        let mark_word = if small_object {
            0b00000100.into()
        } else {
            0b00001100.into()
        };

        Class {
            version: CURRENT_VERSION,
            object_size,
            object_classification: SizeClass::from(object_size),
            mark_word,
            fields: vec![],
            references: vec![],
            methods: vec![]
        }
    }

    pub fn references(&self) -> impl Iterator<Item = &Field> + '_ {
        self.references.as_slice().iter()
    }

    pub fn mark_word(&self) -> MarkWord {
        self.mark_word
    }

    pub fn object_size(&self) -> usize {
        self.object_size
    }

    pub fn size_class(&self) -> SizeClass {
        self.object_classification
    }
}

