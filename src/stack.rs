use std::convert::TryInto;

use bit_set::{BitSet, Iter};

use crate::pointers::heap_pointer::HeapPointer;
use crate::values::value::Value;

pub const STACK_SIZE: usize = 1024 * 1024;

#[derive(Debug)]
pub (crate) struct ThreadStack {
    internal: [u8; STACK_SIZE],
    index: usize,
    stack_map: BitSet
}

impl ThreadStack {

    pub (crate) fn new() -> Self {
        ThreadStack {
            internal: [0; STACK_SIZE],
            index: 0,
            stack_map: BitSet::with_capacity(STACK_SIZE)
        }
    }

    pub fn push_value<const L: usize, T: Value<L>>(&mut self, value: T) {
        let bytes = value.to_bytes();
        let start = self.index;
        let end = self.index + T::len();
        self.internal[start..end].copy_from_slice(&bytes);
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

    pub fn gc_roots(&self) -> impl Iterator<Item=HeapPointer> + '_ {
        RootIterator {
            thread_stack: self,
            iterator: self.stack_map.iter()
        }
    }

    fn read_ptr(&self, index: usize) -> HeapPointer {
        let end = index + 8;
        let array: [u8; 8] = self.internal[index..end].try_into().unwrap();
        HeapPointer::from_bytes(array)
    }
}

struct RootIterator<'a> {
    thread_stack: &'a ThreadStack,
    iterator: Iter<'a, u32>
}

impl<'a> Iterator for RootIterator<'a> {
    type Item = HeapPointer;

    fn next(&mut self) -> Option<Self::Item> {
        match self.iterator.next() {
            Some(val) => {
                Some(self.thread_stack.read_ptr(val))
            }
            None => None
        }
    }
}


#[cfg(test)]
mod tests {
    use crate::object::class::Class;

    use super::*;

    #[test]
    fn read_and_write() {
        let mut stack = ThreadStack::new();
        stack.push_value(12i32);
        assert_eq!(12i32, stack.read_value());
    }

    #[test]
    fn stack_map() {
        let mut stack = ThreadStack::new();
        stack.push_value(1i32);

        let mut heap_value = 0i128;
        let ptr: *mut i128 = &mut heap_value;
        let heap_ptr = HeapPointer::new(&Class::new(8), ptr.cast());

        unsafe { heap_ptr.start_address().write(123) };

        stack.push_value(heap_ptr);
        stack.push_value(heap_ptr);
        stack.push_value(heap_ptr);

        stack.push_value(3i64);

        let mut expected_bitset = BitSet::new();
        expected_bitset.insert(4);
        expected_bitset.insert(12);
        expected_bitset.insert(20);

        assert_eq!(expected_bitset, stack.stack_map);

        let num: i64 = stack.read_value();

        let mut pointer: HeapPointer = stack.read_value();
        assert_eq!(heap_ptr, pointer);
        unsafe { assert_eq!(123, pointer.start_address().read()) };
        pointer = stack.read_value();
        unsafe { assert_eq!(123, pointer.start_address().read()) };
        pointer = stack.read_value();
        unsafe { assert_eq!(123, pointer.start_address().read()) };

        let small_num: i32 = stack.read_value();

        assert_eq!(num, 3);
        assert_eq!(small_num, 1);
    }

    #[test]
    fn gc_roots() {
        let mut stack = ThreadStack::new();

        stack.push_value(0i32);
        stack.push_value(0i64);
        stack.push_value(0.123f64);

        let mut heap_value = 0i128;
        let ptr: *mut i128 = &mut heap_value;
        let heap_ptr = HeapPointer::new(&Class::new(8), ptr.cast());

        unsafe { heap_ptr.start_address().write(12345u64) };

        stack.push_value(heap_ptr);

        stack.push_value(0i32);

        stack.push_value(heap_ptr);

        stack.push_value(0.12345678f64);

        stack.push_value(heap_ptr);

        let gc_roots = stack.gc_roots().collect::<Vec<HeapPointer>>();
        assert_eq!(3, gc_roots.len());

        for ptr in gc_roots {
            assert_eq!(12345, unsafe { ptr.start_address().read() });
        }
    }

    #[test]
    #[should_panic]
    fn read_uninit_value() {
        std::panic::set_hook(Box::new(|info| {

        }));

        let mut stack = ThreadStack::new();
        let val: i32 = stack.read_value();
    }
}