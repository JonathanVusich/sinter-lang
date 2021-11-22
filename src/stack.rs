use std::convert::TryInto;
use bit_set::BitSet;
use crate::pointers::heap_pointer::HeapPointer;
use crate::values::value::Value;

pub const STACK_SIZE: usize = 2_000_000;

#[derive(Debug)]
pub (crate) struct Stack {
    internal: [u8; STACK_SIZE],
    index: usize,
    stack_map: BitSet
}

impl Stack {

    pub (crate) fn new() -> Self {
        Stack {
            internal: [0; STACK_SIZE],
            index: 0,
            stack_map: BitSet::with_capacity(STACK_SIZE)
        }
    }

    pub fn push_value<const L: usize, T: Value<L>>(&mut self, value: T) {
        let bytes = value.to_bytes();
        for i in 0..T::len() {
            self.internal[self.index + i] = bytes[i]
        }
        if T::is_reference_type() {
            self.stack_map.insert(self.index);
        }
        self.index += T::len();

    }

    pub fn read_value<const L: usize, T: Value<L>>(&mut self) -> T {
        let end = self.index;
        self.index -= T::len();
        let start = self.index;

        let array: [u8; L] = self.internal[start..end].try_into().unwrap();
        T::from_bytes(array)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn read_and_write() {
        let mut stack = Stack::new();
        stack.push_value(12i32);
        assert_eq!(12i32, stack.read_value());
    }

    #[test]
    fn stack_map() {
        let mut stack = Stack::new();
        stack.push_value(1i32);
        stack.push_value(HeapPointer::from_address(0));
        stack.push_value(HeapPointer::from_address(0));
        stack.push_value(HeapPointer::from_address(0));

        stack.push_value(3i64);

        let mut expected_bitset = BitSet::new();
        expected_bitset.insert(4);
        expected_bitset.insert(12);
        expected_bitset.insert(20);

        assert_eq!(expected_bitset, stack.stack_map);

        let num: i64 = stack.read_value();

        let mut pointer: HeapPointer = stack.read_value();
        pointer = stack.read_value();
        pointer = stack.read_value();

        let small_num: i32 = stack.read_value();

        assert_eq!(num, 3);
        assert_eq!(small_num, 1);
    }

    #[test]
    #[should_panic]
    fn read_uninit_value() {
        std::panic::set_hook(Box::new(|info| {

        }));

        let mut stack = Stack::new();
        let val: i32 = stack.read_value();
    }
}