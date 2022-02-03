use std::convert::TryInto;
use std::fmt::{Display, Formatter};

use crate::pointers::heap_pointer::HeapPointer;
use crate::util::constants::{BITS_64, BITS_32, WORD};

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

    pub fn push_32(&mut self, value: &[u8; BITS_32]) {
        let start = self.index;
        let end = self.index + BITS_32;
        self.internal[start..end].copy_from_slice(value);
        self.index += BITS_32;
    }

    pub fn push_64(&mut self, value: &[u8; BITS_64]) {
        let start = self.index;
        let end = self.index + BITS_64;
        self.internal[start..end].copy_from_slice(value);
        self.index += BITS_64;
    }

    pub fn push_word(&mut self, value: &[u8; WORD]) {
        let start = self.index;
        let end = self.index + WORD;
        self.internal[start..end].copy_from_slice(value);
        self.index += WORD;
    }

    pub fn pop_32(&mut self) -> &[u8; BITS_32] {
        let end = self.index;
        self.index -= BITS_32;
        let start = self.index;

        self.internal[start..end].try_into().unwrap()
    }

    pub fn pop_64(&mut self) -> &[u8; BITS_64] {
        let end = self.index;
        self.index -= BITS_64;
        let start = self.index;

        self.internal[start..end].try_into().unwrap()
    }

    pub fn pop_word(&mut self) -> &[u8; WORD] {
        let end = self.index;
        self.index -= WORD;
        let start = self.index;

        self.internal[start..end].try_into().unwrap()
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

        let bytes: [u8; BITS_32] = 12u32.to_ne_bytes();

        stack.push_32(&bytes);
        assert_eq!(&bytes, stack.pop_32());
    }

    #[test]
    fn stack_map() {
        let mut stack = Stack::new();

        let bytes: [u8; BITS_32] = 1u32.to_ne_bytes();

        stack.push_32(&bytes);

        let mut heap_value = 0i128;
        let ptr: *mut i128 = &mut heap_value;
        let heap_ptr = HeapPointer::new(&Class::new(8), ptr.cast());

        heap_ptr.start_address().write(123);

        let bytes: [u8; WORD] = (heap_ptr.to_raw().as_ptr() as usize).to_ne_bytes();

        stack.push_word(&bytes);
        stack.push_word(&bytes);
        stack.push_word(&bytes);
        stack.push_word(&3isize.to_ne_bytes());

        let num = isize::from_ne_bytes(*stack.pop_word());

        let pointer1 = HeapPointer::from_bytes(stack.pop_word());
        assert_eq!(123, pointer1.start_address().read());

        let pointer2 = HeapPointer::from_bytes(stack.pop_word());
        assert_eq!(123, pointer2.start_address().read());

        let pointer3 = HeapPointer::from_bytes(stack.pop_word());
        assert_eq!(123, pointer3.start_address().read());

        let num = u32::from_ne_bytes(*stack.pop_32());

        assert_eq!(num, 1);
    }

    #[test]
    #[should_panic]
    fn read_uninit_value() {
        std::panic::set_hook(Box::new(|info| {

        }));

        let mut stack = Stack::new();
        let val = stack.pop_32();
    }
}