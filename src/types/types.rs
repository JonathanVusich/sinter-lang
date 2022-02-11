use crate::class::class::Class;
use crate::strings::internal_string::InternalString;

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum Type {
    InlineClass(InternalString),
    Class(InternalString),
    Trait(InternalString)
}