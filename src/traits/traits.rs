use crate::function::method::Method;
use crate::pool::internal_string::InternalString;

pub struct Trait {
    package: InternalString,
    name: InternalString,
    methods: Vec<Method>
}