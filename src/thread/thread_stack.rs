use std::convert::TryInto;

use crate::pointers::heap_pointer::HeapPointer;
use crate::values::value::Value;

pub const STACK_SIZE: usize = 4 * 1024;

#[derive(Debug)]
pub struct ThreadStack {
    internal: [u8; STACK_SIZE],
    index: usize
}

impl ThreadStack {

    pub (crate) fn new() -> Self {
        ThreadStack {
            internal: [0; STACK_SIZE],
            index: 0
        }
    }

    pub fn push<const L: usize, T: Value<L>>(&mut self, value: T) {
        let bytes = value.to_bytes();
        let start = self.index;
        let end = self.index + T::len();
        self.internal[start..end].copy_from_slice(&bytes);
        self.index += T::len();
    }

    pub fn pop<const L: usize, T: Value<L>>(&mut self) -> T {
        let end = self.index;
        self.index -= T::len();
        let start = self.index;

        let array: [u8; L] = self.internal[start..end].try_into().unwrap();
        T::from_bytes(array)
    }

    fn read_ptr(&self, index: usize) -> HeapPointer {
        let end = index + 8;
        let array: [u8; 8] = self.internal[index..end].try_into().unwrap();
        HeapPointer::from_bytes(array)
    }
}


#[cfg(test)]
mod tests {
    use crate::class::class::Class;

    use super::*;

    #[test]
    fn read_and_write() {
        let mut stack = ThreadStack::new();
        stack.push(12i32);
        assert_eq!(12i32, stack.pop::<4, i32>());
    }

    #[test]
    fn stack_map() {
        let mut stack = ThreadStack::new();
        stack.push(1i32);

        let mut heap_value = 0i128;
        let ptr: *mut i128 = &mut heap_value;
        let heap_ptr = HeapPointer::new(&Class::new(8), ptr.cast());

        heap_ptr.start_address().write(123);

        stack.push(heap_ptr);
        stack.push(heap_ptr);
        stack.push(heap_ptr);

        stack.push(3i64);

        let num: i64 = stack.pop();

        let mut pointer: HeapPointer = stack.pop();
        assert_eq!(heap_ptr, pointer);
        assert_eq!(123, pointer.start_address().read());
        pointer = stack.pop();
        assert_eq!(123, pointer.start_address().read());
        pointer = stack.pop();
        assert_eq!(123, pointer.start_address().read());

        let small_num: i32 = stack.pop();

        assert_eq!(num, 3);
        assert_eq!(small_num, 1);
    }

    #[test]
    #[should_panic]
    fn read_uninit_value() {
        std::panic::set_hook(Box::new(|info| {

        }));

        let mut stack = ThreadStack::new();
        let val: i32 = stack.pop();
    }
}