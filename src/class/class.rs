use std::ptr::NonNull;
use std::slice::Iter;
use crate::class::compiled_class::CompiledClass;
use crate::class::constant_pool::{ConstantPool, ConstantPoolEntry};
use crate::class::field::Field;
use crate::class::references::Reference;

use crate::class::size_class::SizeClass;
use crate::class::version::{CURRENT_VERSION, Version};
use crate::function::method::{Method, MethodDescriptor};
use crate::gc::block::{BLOCK_SIZE, LINE_SIZE};
use crate::pointers::heap_pointer::HeapPointer;
use crate::pointers::mark_word::MarkWord;
use crate::pointers::pointer::Pointer;
use crate::strings::internal_string::InternalString;
use crate::strings::string_pool::StringPool;
use crate::types::types::{CompiledBaseType, CompiledType, Type};
use crate::types::types::Type::*;

#[derive(Eq, PartialEq, Debug)]
pub struct Class {
    pub version: Version,

    pub package: InternalString,
    pub name: InternalString,

    pub size: usize,
    pub size_class: SizeClass,
    pub mark_word: MarkWord,

    pub fields: &'static [Field],
    pub references: &'static [Field],
    pub methods: &'static [Method],
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

            package: InternalString(0),
            name: InternalString(0),

            size: object_size,
            size_class: SizeClass::from(object_size),
            mark_word,
            fields: Box::leak(Vec::new().into_boxed_slice()),
            references: Box::leak(Vec::new().into_boxed_slice()),
            methods: Box::leak(Vec::new().into_boxed_slice()),
        }
    }

    pub fn from(compiled_class: CompiledClass, string_pool: &mut StringPool) -> Self {
        let version = compiled_class.version;

        let fields = Box::leak(compiled_class.fields.iter().map(|compiled_field| {
            let name = load_str(compiled_field.name, &compiled_class.constant_pool, string_pool);
            let actual_type = load_type(&compiled_field.type_descriptor, &compiled_class.constant_pool, string_pool);

            Field::new(name, actual_type, compiled_field.offset, compiled_field.size)
        }).collect::<Vec<Field>>()
          .into_boxed_slice());

        let references = Box::leak(fields.iter()
            .filter(|field| {
                return match field.type_descriptor {
                    Trait(b) | Class(b) => true,
                    _ => false
                }
            })
            .cloned()
            .collect::<Vec<Field>>()
            .into_boxed_slice());

        let methods = Box::leak(compiled_class.methods.iter().map(|method| {
            let name = load_str(method.name, &compiled_class.constant_pool, string_pool);

            let return_type = load_type(&&method.descriptor.return_type, &compiled_class.constant_pool, string_pool);
            let parameters = method.descriptor.parameters.iter().map(|compiled_type| {
                load_type(compiled_type, &compiled_class.constant_pool, string_pool)
            }).collect::<Vec<Type>>()
              .into_boxed_slice();

            let descriptor = MethodDescriptor::new(return_type, parameters);

            Method::new(name, descriptor, method.max_stack_size, method.max_locals, method.code.clone())
        }).collect::<Vec<Method>>()
          .into_boxed_slice());

        let size_class = SizeClass::from(compiled_class.size as usize);
        let mark_word = if size_class == SizeClass::Small {
            0b00000100.into()
        } else {
            0b00001100.into()
        };

        let package = load_str(compiled_class.package, &compiled_class.constant_pool, string_pool);
        let name = load_str(compiled_class.package, &compiled_class.constant_pool, string_pool);

        Self {
            version: compiled_class.version,
            package,
            name,

            size: compiled_class.size as usize,
            size_class,
            mark_word,
            fields,
            references,
            methods,
        }
    }

    pub fn get_main_method(&self, string_pool: &StringPool) -> Option<&'static Method> {
        self.methods.iter().find(|method| method.is_main_method(string_pool))
    }
}

fn load_str(entry: ConstantPoolEntry, constant_pool: &ConstantPool, string_pool: &mut StringPool) -> InternalString {
    let string = constant_pool.load_str(entry);
    string_pool.intern(string)
}

fn load_type(compiled_type: &CompiledType, constant_pool: &ConstantPool, string_pool: &mut StringPool) -> Type {
    let actual_type = constant_pool.load_str(compiled_type.actual_type);
    let interned_type = string_pool.intern(actual_type);

    return match compiled_type.base_type {
        CompiledBaseType::Void => Void,
        CompiledBaseType::InlineClass => InlineClass(interned_type),
        CompiledBaseType::Class => Class(interned_type),
        CompiledBaseType::Trait => Trait(interned_type),
    }
}

