use std::borrow::Borrow;
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};
use crate::pointers::mark_word::MarkWord;

const BIT_1_MASK: usize = compute_mask(1);
const BIT_2_MASK: usize = compute_mask(2);
const BIT_3_MASK: usize = compute_mask(3);
const BIT_4_MASK: usize = compute_mask(4);
const BIT_5_MASK: usize = compute_mask(5);
const BIT_6_MASK: usize = compute_mask(6);
const BIT_7_MASK: usize = compute_mask(7);
const BIT_8_MASK: usize = compute_mask(8);

#[derive(Copy, Clone)]
pub struct TaggedPointer<T> {
    ptr: usize,
    data: PhantomData<T>,
}

impl<T> TaggedPointer<T> {
    pub fn new(ptr: *const T) -> Self {
        TaggedPointer {
            ptr: ptr as usize,
            data: PhantomData::default(),
        }
    }

    pub fn new_with_mark_word(ptr: *const T, mark_word: MarkWord) -> Self {
        let byte: u8 = mark_word.into();
        let shifted_mark_word = (byte as usize) << 56;
        let tagged_ptr = ptr as usize | shifted_mark_word;

        TaggedPointer {
            ptr: tagged_ptr,
            data: PhantomData::default(),
        }
    }

    pub fn get_mark_word(&self) -> MarkWord {
        let shifted_val = self.ptr >> 56;
        (shifted_val as u8).into()
    }

    pub fn set_mark_word(&mut self, mark_word: MarkWord) {
        let mut ptr = self.ptr;
        ptr <<= 8;
        ptr >>= 8;
        let byte: u8 = mark_word.into();
        let shifted_mark_word = (byte as usize) << 56;
        self.ptr = (ptr | shifted_mark_word) as _
    }
}

const fn compute_mask(bit: u32) -> usize {
    assert!(bit > 0 && bit < 9, "Unsupported bit index!");
    let mut mask: usize = 1;
    mask = mask.rotate_right(bit);
    mask
}

#[inline(always)]
const fn get_mask(bit: u32) -> usize {
    match bit {
        1 => BIT_1_MASK,
        2 => BIT_2_MASK,
        3 => BIT_3_MASK,
        4 => BIT_4_MASK,
        5 => BIT_5_MASK,
        6 => BIT_6_MASK,
        7 => BIT_7_MASK,
        8 => BIT_8_MASK,
        _ => panic!("Unsupported bit index!")
    }
}

impl<T> From<TaggedPointer<T>> for usize {
    fn from(pointer: TaggedPointer<T>) -> Self {
        pointer.ptr as usize
    }
}

impl<T> Borrow<T> for TaggedPointer<T> {
    fn borrow(&self) -> &T {
        self.deref()
    }
}

impl<T> Deref for TaggedPointer<T> {
    type Target = T;

    fn deref(&self) -> &T {
        let mut val = self.ptr as i64;
        // Clear out tags
        val <<= 16;
        val >>= 16;

        let ptr: *mut T = val as _;

        unsafe {
            let reference: &T = &*ptr;
            reference
        }
    }
}


impl<T> DerefMut for TaggedPointer<T> {
    fn deref_mut(&mut self) -> &mut T {
        let mut val = self.ptr as i64;

        // Clear out tags
        val <<= 16;
        val >>= 16;

        let ptr: *mut T = val as _;

        unsafe {
            let reference: &mut T = &mut *ptr;
            reference
        }
    }
}

mod tests {
    extern crate test;

    use std::ops::Shl;
    use test::Bencher;
    use test::black_box;

    use crate::class::class::Class;

    use super::*;

    #[test]
    pub fn size() {
        assert_eq!(std::mem::size_of::<TaggedPointer<i32>>(), std::mem::size_of::<usize>());
        assert_eq!(std::mem::size_of::<TaggedPointer<Class>>(), std::mem::size_of::<usize>());
    }

    #[test]
    pub fn dereferencing() {
        let val: i32 = 1;
        let mut tagged_pointer = TaggedPointer::new(&val);

        assert_eq!(*tagged_pointer, val);

        *tagged_pointer <<= 1;

        assert_eq!(2, val);
        assert_eq!(*tagged_pointer, val);

        *tagged_pointer <<= 2;

        assert_eq!(8, val);
        assert_eq!(*tagged_pointer, val);

        let new_val = tagged_pointer.shl(1);

        assert_eq!(16, new_val);

        assert_eq!(8, val);
        assert_eq!(*tagged_pointer, val);
    }

    #[test]
    #[should_panic]
    pub fn bit_too_low() {
        std::panic::set_hook(Box::new(|info| {}));

        let val = 1;
        let mut tagged_pointer = TaggedPointer::new(&val);
    }

    #[test]
    pub fn set_mark_word() {
        let val = 1;

        let mut tagged_pointer = TaggedPointer::new(&val);

        assert_eq!(tagged_pointer.get_mark_word(), 0.into());

        let new_mark_word: MarkWord = 0b11110000.into();

        tagged_pointer.set_mark_word(new_mark_word);

        assert_eq!(new_mark_word, tagged_pointer.get_mark_word());

        assert_eq!(1, *tagged_pointer);

        let different_mark_word: MarkWord = 0b10001000.into();

        tagged_pointer.set_mark_word(different_mark_word);

        assert_eq!(different_mark_word, tagged_pointer.get_mark_word());

        assert_eq!(1, *tagged_pointer);
    }

    #[test]
    pub fn new_with_mark_word() {
        let val = 1;

        let mark_word = 0b01010101.into();

        let tagged_pointer = TaggedPointer::new_with_mark_word(&val, mark_word);

        assert_eq!(tagged_pointer.get_mark_word(), mark_word);
    }
}