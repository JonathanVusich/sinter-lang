use std::ptr::NonNull;
use std::slice::Iter;
use crate::class::compiled_class::CompiledClass;
use crate::class::constant_pool::ConstantPoolEntry;
use crate::class::field::Field;
use crate::class::references::Reference;

use crate::class::size_class::SizeClass;
use crate::class::version::{CURRENT_VERSION, Version};
use crate::function::method::Method;
use crate::gc::block::{BLOCK_SIZE, LINE_SIZE};
use crate::pointers::heap_pointer::HeapPointer;
use crate::pointers::mark_word::MarkWord;
use crate::pointers::pointer::Pointer;
use crate::strings::string_pool::StringPool;
use crate::types::types::{CompiledBaseType, CompiledType, Type};

#[derive(Eq, PartialEq, Debug)]
pub struct Class {
    version: Version,
    size: usize,
    object_classification: SizeClass,
    mark_word: MarkWord,

    fields: Vec<Field>,
    references: Vec<Field>,
    methods: Vec<Method>,
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
            size: object_size,
            object_classification: SizeClass::from(object_size),
            mark_word,
            fields: vec![],
            references: vec![],
            methods: vec![],
        }
    }

    pub fn from(compiled_class: CompiledClass, string_pool: &mut StringPool) -> Self {
        let version = compiled_class.version();

        let fields = compiled_class.fields().iter().map(|compiled_field| {
            let name = compiled_class.constant_pool().load_str(compiled_field.name);
            let interned_name = string_pool.intern(name);

            let type_descriptor = compiled_field.type_descriptor();

            let actual_type = compiled_class.constant_pool().load_str(type_descriptor.actual_type);
            let interned_type = string_pool.intern(actual_type);

            let base_type = match type_descriptor.base_type {
                CompiledBaseType::Void => Type::Void,
                CompiledBaseType::InlineClass => Type::InlineClass(interned_type),
                CompiledBaseType::Class => Type::Class(interned_type),
                CompiledBaseType::Trait => Type::Trait(interned_type),
            };

            Field::new(interned_name, base_type, compiled_field.offset)
        }).collect_vec();

        let references = fields.iter()
            .filter(|field| field.size == 8)
            .cloned()
            .collect_vec();

        let methods = compiled_class.methods.iter().map(|method| {
            let name = compiled_class.constant_pool.load_str(method.name);
            let interned_name = string_pool.intern(name);

            let descriptor = method.descriptor;
        }).collect_vec();

        let size_class = SizeClass::from(compiled_class.size as usize);
        let mark_word = if size_class == SizeClass::Small {
            0b00000100.into()
        } else {
            0b00001100.into()
        };

        Self {
            version: compiled_class.version(),
            size: compiled_class.size as usize,
            object_classification: size_class,
            mark_word,
            fields,
            references,
            methods,
        }
    }

    pub fn references(&self) -> impl Iterator<Item=&Field> + '_ {
        self.references.as_slice().iter()
    }

    pub fn mark_word(&self) -> MarkWord {
        self.mark_word
    }

    pub fn object_size(&self) -> usize {
        self.size
    }

    pub fn size_class(&self) -> SizeClass {
        self.object_classification
    }
}

pub fn compute_class_size(fields: Box<[Field]>)

