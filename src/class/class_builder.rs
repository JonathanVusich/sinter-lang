use crate::class::class::Class;
use crate::class::field::Field;
use crate::class::size_class::SizeClass;
use crate::class::version::Version;
use crate::function::method::Method;
use crate::gc::block::LINE_SIZE;
use crate::pool::internal_string::InternalString;
use crate::pool::string_pool::StringPool;
use crate::types::types::Type;

pub struct ClassBuilder {
    version: Option<Version>,
    package: Option<String>,
    name: Option<String>,
    size: Option<usize>,

    fields: Vec<Field>,
    methods: Vec<Method>,
}

impl ClassBuilder {

    pub fn new() -> Self {
        Self {
            version: None,
            package: None,
            name: None,
            size: None,

            fields: vec![],
            methods: vec![],
        }
    }

    pub fn set_version(&mut self, version: Version) {
        self.version = Some(version);
    }

    pub fn set_package(&mut self, package: String) {
        self.package = Some(package);
    }

    pub fn set_name(&mut self, name: String) {
        self.name = Some(name);
    }

    pub fn set_size(mut self, size: usize) -> Self {
        self.size = Some(size);
        self
    }

    pub fn add_field(&mut self, field: Field) {
        self.fields.push(field)
    }

    pub fn add_method(&mut self, method: Method) {
        self.methods.push(method)
    }
    
    pub fn build<F: FnMut(String) -> InternalString>(self, mut string_interner: F) -> Class {

        let object_size = self.size.unwrap_or_default();
        let small_object = object_size < LINE_SIZE;
        let mark_word = if small_object {
            0b00000100.into()
        } else {
            0b00001100.into()
        };

        let fields = Box::leak(self.fields.into_boxed_slice());

        let references = Box::leak(fields.iter()
            .filter(|field| matches!(field.type_descriptor, Type::Trait(b) | Type::Class(b)))
            .cloned()
            .collect::<Vec<Field>>()
            .into_boxed_slice());

        let methods = Box::leak(self.methods.into_boxed_slice());

        Class {
            version: self.version.unwrap_or_default(),
            package: self.package.map_or_else(|| InternalString(0), |val| string_interner(val)),
            name: self.name.map_or_else(|| InternalString(0), |val| string_interner(val)),
            size: self.size.unwrap_or_default(),
            size_class: SizeClass::from(self.size.unwrap_or_default()),
            mark_word,
            fields,
            references,
            methods,
        }
    } 
}