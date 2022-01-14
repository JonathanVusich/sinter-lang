use std::convert::TryInto;
use std::fmt::{Display, Formatter};

use crate::pointers::heap_pointer::HeapPointer;

pub const STACK_SIZE: usize = 4 * 1024;

#[derive(Debug)]
pub struct Stack {
    internal: [u8; STACK_SIZE],
    index: usize
}

impl Stack {

    pub(crate) fn new() -> Self {
        Stack {
            internal: [0; STACK_SIZE],
            index: 0
        }
    }

    pub fn set(&mut self, index: usize) {
        self.index = index;
    }

    pub fn push(&mut self, value: &[u8]) {
        let start = self.index;
        let end = self.index + value.len();
        self.internal[start..end].copy_from_slice(value);
        self.index += value.len();
    }

    pub fn pop(&mut self, size: u16) -> &[u8] {
        let end = self.index;
        self.index -= size as usize;
        let start = self.index;

        let array: &[u8] = &self.internal[start..end];
        array
    }
}

impl Display for Stack {

    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Stack[index: {}, stack: {:?}]", self.index, self.internal)
    }
}


#[cfg(test)]
mod tests {
    use crate::class::class::Class;

    use super::*;

    #[test]
    fn read_and_write() {
        let mut stack = Stack::new();
        stack.push(&12i32.to_ne_bytes());
        assert_eq!(&12i32.to_ne_bytes(), stack.pop(4));
    }

    #[test]
    fn stack_map() {
        let mut stack = Stack::new();
        stack.push(&1i32.to_ne_bytes());

        let mut heap_value = 0i128;
        let ptr: *mut i128 = &mut heap_value;
        let heap_ptr = HeapPointer::new(&Class::new(8), ptr.cast());

        heap_ptr.start_address().write(123);

        let bytes: [u8; 8] = (heap_ptr.to_raw().as_ptr() as usize).to_ne_bytes();

        stack.push(&bytes);
        stack.push(&bytes);
        stack.push(&bytes);
        stack.push(&3i64.to_ne_bytes());

        let num = i64::from_ne_bytes(stack.pop(8).try_into().unwrap());

        let mut pointer: HeapPointer = HeapPointer::from_address(u64::from_ne_bytes(stack.pop(8).try_into().unwrap()));
        assert_eq!(123, pointer.start_address().read());

        pointer = HeapPointer::from_address(u64::from_ne_bytes(stack.pop(8).try_into().unwrap()));
        assert_eq!(heap_ptr, pointer);
        assert_eq!(123, pointer.start_address().read());

        pointer = HeapPointer::from_address(u64::from_ne_bytes(stack.pop(8).try_into().unwrap()));
        assert_eq!(heap_ptr, pointer);
        assert_eq!(123, pointer.start_address().read());

        let small_num: i32 = i32::from_ne_bytes(stack.pop(4).try_into().unwrap());

        assert_eq!(small_num, 1);
    }

    #[test]
    #[should_panic]
    fn read_uninit_value() {
        std::panic::set_hook(Box::new(|info| {

        }));

        let mut stack = Stack::new();
        let val = stack.pop(1);
    }
}