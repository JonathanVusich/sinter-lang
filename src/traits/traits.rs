use crate::function::method::Method;
use crate::strings::internal_string::InternalString;

pub struct Trait {
    package: InternalString,
    name: InternalString,
    methods: Vec<Method>
}