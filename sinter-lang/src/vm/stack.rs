use std::convert::TryInto;
use std::fmt::{Display, Formatter};

use crate::pointers::heap_pointer::HeapPointer;
use crate::util::constants::WORD;

pub const STACK_SIZE: usize = 4 * 1024;

#[derive(Debug)]
pub struct Stack {
    internal: [u8; STACK_SIZE],
    index: usize,
}

impl Stack {
    pub(crate) fn new() -> Self {
        Stack {
            internal: [0; STACK_SIZE],
            index: 0,
        }
    }

    pub fn set_index(&mut self, index: usize) {
        self.index = index;
    }

    pub fn get_index(&self) -> usize {
        self.index
    }

    pub fn push<const NUM: usize>(&mut self, value: [u8; NUM]) {
        let start = self.index;
        let end = self.index + NUM;
        self.internal[start..end].copy_from_slice(&value);
        self.index += NUM;
    }

    pub fn pop<const NUM: usize>(&mut self) -> [u8; NUM] {
        let val = self.peek::<NUM>();
        self.index -= NUM;
        val
    }

    pub fn peek<const NUM: usize>(&mut self) -> [u8; NUM] {
        let end = self.index;
        let start = self.index - NUM;

        self.internal[start..end].try_into().unwrap()
    }

    pub fn write<const NUM: usize>(&mut self, offset: usize, bytes: [u8; NUM]) {
        self.internal[offset..offset + NUM].copy_from_slice(&bytes)
    }

    pub fn read<const NUM: usize>(&mut self, offset: usize) -> [u8; NUM] {
        self.internal[offset..offset + NUM].try_into().unwrap()
    }
}

impl Display for Stack {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Stack[index: {}, stack: {:?}]",
            self.index, self.internal
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        class::{class::Class, class_builder::ClassBuilder},
        pool::internal_string::InternalString,
    };

    use super::*;

    #[test]
    fn read_and_write() {
        let mut stack = Stack::new();

        let bytes = 12u32.to_ne_bytes();

        stack.push(bytes);
        assert_eq!(bytes, stack.pop());
    }

    #[test]
    fn stack_map() {
        let mut stack = Stack::new();

        let bytes = 1u32.to_ne_bytes();

        stack.push(bytes);

        let mut heap_value = 0i128;
        let ptr: *mut i128 = &mut heap_value;
        let class = ClassBuilder::new()
            .set_size(8)
            .build(|val| InternalString(0));
        let heap_ptr = HeapPointer::new(&class, ptr.cast());

        heap_ptr.start_address().write(123);

        let bytes: [u8; WORD] = (heap_ptr.to_raw().as_ptr() as usize).to_ne_bytes();

        stack.push(bytes);
        stack.push(bytes);
        stack.push(bytes);
        stack.push(3isize.to_ne_bytes());

        let num = isize::from_ne_bytes(stack.pop());

        let pointer1: HeapPointer = stack.pop().into();
        assert_eq!(123, pointer1.start_address().read());

        let pointer2: HeapPointer = stack.pop().into();
        assert_eq!(123, pointer2.start_address().read());

        let pointer3: HeapPointer = stack.pop().into();
        assert_eq!(123, pointer3.start_address().read());

        let num = u32::from_ne_bytes(stack.pop());

        assert_eq!(num, 1);
    }

    #[test]
    #[should_panic]
    fn read_uninit_value() {
        std::panic::set_hook(Box::new(|info| {}));

        let mut stack = Stack::new();
        let val: HeapPointer = stack.pop().into();
    }
}
