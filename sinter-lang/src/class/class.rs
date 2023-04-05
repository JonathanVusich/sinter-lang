use crate::class::compiled_class::CompiledClass;
use crate::class::constant_pool::{ConstantPool, ConstantPoolEntry};
use crate::class::field::Field;
use crate::class::references::Reference;
use std::ptr::NonNull;
use std::slice::Iter;

use crate::class::size_class::SizeClass;
use crate::class::version::{Version, CURRENT_VERSION};
use crate::function::method::Method;
use crate::gc::block::{BLOCK_SIZE, LINE_SIZE};
use crate::pointers::heap_pointer::HeapPointer;
use crate::pointers::mark_word::MarkWord;
use crate::pointers::pointer::Pointer;
use crate::pool::internal_string::InternalString;
use crate::pool::string_pool::StringPool;
use crate::types::types::CompiledBaseType::Void;
use crate::types::types::{CompiledBaseType, CompiledType};

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
    pub fn from(compiled_class: CompiledClass, string_pool: &mut StringPool) -> Self {
        let version = compiled_class.version;

        let fields = Box::leak(
            compiled_class
                .fields
                .iter()
                .map(|compiled_field| {
                    let name = load_str(
                        compiled_field.name,
                        &compiled_class.constant_pool,
                        string_pool,
                    );

                    Field::new(name, false, compiled_field.offset, compiled_field.size)
                })
                .collect::<Vec<Field>>()
                .into_boxed_slice(),
        );

        let references = Box::leak(
            fields
                .iter()
                .filter(|field| field.is_reference)
                .cloned()
                .collect::<Vec<Field>>()
                .into_boxed_slice(),
        );

        let methods = Box::leak(
            compiled_class
                .methods
                .iter()
                .map(|method| {
                    let name = load_str(method.name, &compiled_class.constant_pool, string_pool);
                    Method::new(
                        name,
                        method.param_size,
                        method.max_stack_size,
                        method.max_locals,
                        method.is_main_method,
                        method.code.clone(),
                    )
                })
                .collect::<Vec<Method>>()
                .into_boxed_slice(),
        );

        let size_class = SizeClass::from(compiled_class.size as usize);
        let mark_word = if size_class == SizeClass::Small {
            0b00000100.into()
        } else {
            0b00001100.into()
        };

        let package = load_str(
            compiled_class.package,
            &compiled_class.constant_pool,
            string_pool,
        );
        let name = load_str(
            compiled_class.package,
            &compiled_class.constant_pool,
            string_pool,
        );

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

    pub fn get_main_method(&self) -> Option<&'static Method> {
        self.methods.iter().find(|method| method.is_main_method)
    }
}

fn load_str(
    entry: ConstantPoolEntry,
    constant_pool: &ConstantPool,
    string_pool: &mut StringPool,
) -> InternalString {
    let string = constant_pool.load_str(entry);
    string_pool.intern(string)
}
