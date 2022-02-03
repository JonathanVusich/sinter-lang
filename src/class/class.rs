use std::ptr::NonNull;
use std::slice::Iter;
use crate::class::field::Field;
use crate::class::references::Reference;

use crate::class::size_class::SizeClass;
use crate::function::function::Function;
use crate::gc::block::{BLOCK_SIZE, LINE_SIZE};
use crate::pointers::heap_pointer::HeapPointer;
use crate::pointers::mark_word::MarkWord;
use crate::pointers::pointer::Pointer;

#[derive(Eq, PartialEq, Debug)]
pub struct Class {
    object_size: usize,
    object_classification: SizeClass,
    mark_word: MarkWord,
    static_roots: Vec<HeapPointer>,
    fields: Vec<Field>,
    references: Vec<Reference>,
    functions: Vec<Function>
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
            object_size,
            object_classification: SizeClass::from(object_size),
            mark_word,
            static_roots: vec![],
            fields: vec![],
            references: vec![],
            functions: vec![]
        }
    }

    pub fn static_roots(&self) -> impl Iterator<Item = HeapPointer> + '_ {
        self.static_roots.as_slice().iter().copied()
    }

    pub fn references(&self) -> impl Iterator<Item = &Reference> + '_ {
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

