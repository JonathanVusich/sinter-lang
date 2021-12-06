use std::ptr::NonNull;
use std::slice::Iter;
use crate::class::field::Field;
use crate::class::reference_field::ReferenceField;

use crate::class::size_class::SizeClass;
use crate::gc::block::{BLOCK_SIZE, LINE_SIZE};
use crate::pointers::heap_pointer::HeapPointer;
use crate::pointers::pointer::Pointer;

#[derive(Eq, PartialEq, Debug)]
pub struct Class {
    object_size: usize,
    object_classification: SizeClass,
    mark_word: u8,
    static_roots: Vec<HeapPointer>,
    fields: Vec<Field>,
    references: Vec<ReferenceField>
}

impl Class {

    pub fn new(object_size: usize) -> Class {
        let small_object = object_size < LINE_SIZE;
        let mark_word: u8 = if small_object {
            0b00000100
        } else {
            0b00001100
        };

        Class {
            object_size,
            object_classification: SizeClass::from(object_size),
            mark_word,
            static_roots: vec![],
            fields: vec![],
            references: vec![]
        }
    }

    pub fn static_roots(&self) -> impl Iterator<Item = HeapPointer> + '_ {
        self.static_roots.as_slice().iter().copied()
    }

    pub fn references(&self) -> impl Iterator<Item = &ReferenceField> + '_ {
        self.references.as_slice().iter()
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

