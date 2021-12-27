use crate::class::class::Class;

pub enum Slot {
    Reference,
    Sized(usize)
}