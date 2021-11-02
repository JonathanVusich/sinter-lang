use std::ops::{Deref, DerefMut};
use std::borrow::Borrow;
use std::marker::PhantomData;
use crate::object::class::Class;

const BIT_1_MASK: u64 = compute_mask(1);
const BIT_2_MASK: u64 = compute_mask(2);
const BIT_3_MASK: u64 = compute_mask(3);
const BIT_4_MASK: u64 = compute_mask(4);
const BIT_5_MASK: u64 = compute_mask(5);
const BIT_6_MASK: u64 = compute_mask(6);
const BIT_7_MASK: u64 = compute_mask(7);
const BIT_8_MASK: u64 = compute_mask(8);

pub struct TaggedPointer<T> {
    ptr: u64,
    data: PhantomData<T>
}

impl<T> TaggedPointer<T> {
    pub fn new(ptr: *const T) -> Self {
        TaggedPointer {
            ptr: ptr as u64,
            data: PhantomData::default()
        }
    }

    pub fn new_with_mark_word(ptr: *const T, mark_word: u8) -> Self {
        let shifted_mark_word = (mark_word as u64) << 56;
        let tagged_ptr = ptr as u64 | shifted_mark_word;

        TaggedPointer {
            ptr: tagged_ptr,
            data: PhantomData::default()
        }
    }

    pub fn from_address(address: u64) -> Self {
        TaggedPointer {
            ptr: address,
            data: PhantomData::default()
        }
    }

    pub fn get_mark_word(&self) -> u8 {
        let shifted_val = self.ptr >> 56;
        shifted_val as u8
    }

    pub fn set_mark_word(&mut self, mark_word: u8) {
        let mut ptr = self.ptr;
        ptr <<= 8;
        ptr >>= 8;
        let shifted_mark_word = (mark_word as u64) << 56;
        self.ptr = (ptr | shifted_mark_word) as _
    }

    pub fn set_bit(&mut self, bit: u32) {
        let ptr = self.ptr;
        let mask = get_mask(bit);
        self.ptr = (self.ptr as u64 | mask) as _
    }

    pub fn clear_bit(&mut self, bit: u32) {
        let ptr = self.ptr;
        let mask = get_mask(bit);
        self.ptr = (self.ptr as u64 & !mask) as _
    }

    pub fn is_bit_set(&self, bit: u32) -> bool {
        let ptr = self.ptr;
        let mask = get_mask(bit);
        (ptr & mask) != 0
    }
}

#[inline(always)]
const fn compute_mask(bit: u32) -> u64 {
    assert!(bit > 0 && bit < 9, "Unsupported bit index!");
    let mut mask: u64 = 1;
    mask = mask.rotate_right(bit);
    mask
}

#[inline(always)]
const fn get_mask(bit: u32) -> u64 {
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

impl<T> From<TaggedPointer<T>> for u64 {
    fn from(pointer: TaggedPointer<T>) -> Self {
        pointer.ptr as u64
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
        #[cfg(target_pointer_width = "32")] {
            let ptr: *mut T = self.ptr as *mut T;
            unsafe {
                &*ptr
            }
        }

        #[cfg(target_pointer_width = "64")] {
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
}

impl<T> DerefMut for TaggedPointer<T> {

    fn deref_mut(&mut self) -> &mut T {
        #[cfg(target_pointer_width = "32")] {
            let ptr: *mut T = self.ptr as *mut T;
            unsafe {
                &mut *ptr
            }
        }

        #[cfg(target_pointer_width = "64")] {
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
}

mod tests {
    use std::ops::Shl;

    use super::*;
    use crate::object::class::Class;

    #[test]
    pub fn size() {
        assert_eq!(std::mem::size_of::<TaggedPointer<i32>>(), std::mem::size_of::<u64>());
        assert_eq!(std::mem::size_of::<TaggedPointer<Class>>(), std::mem::size_of::<u64>());
    }

    #[test]
    pub fn dereferencing() {
        let val: i32 = 1;
        let mut tagged_pointer = TaggedPointer::new(&val);

        assert_eq!(*tagged_pointer, val);

        *tagged_pointer <<= 1;

        tagged_pointer.set_bit(1);

        assert_eq!(2, val);
        assert_eq!(*tagged_pointer, val);

        assert!(tagged_pointer.is_bit_set(1));

        *tagged_pointer <<= 2;

        tagged_pointer.set_bit(2);

        assert_eq!(8, val);
        assert_eq!(*tagged_pointer, val);

        assert!(tagged_pointer.is_bit_set(1));
        assert!(tagged_pointer.is_bit_set(2));

        let new_val = tagged_pointer.shl(1);

        assert_eq!(16, new_val);

        assert_eq!(8, val);
        assert_eq!(*tagged_pointer, val);

        assert!(tagged_pointer.is_bit_set(1));
        assert!(tagged_pointer.is_bit_set(2));
    }

    #[test]
    pub fn setting_bits() {
        let val: i32 = 1;
        let mut tagged_pointer = TaggedPointer::new(&val);

        tagged_pointer.set_bit(1);

        assert_eq!(*tagged_pointer, 1);
        assert!(tagged_pointer.is_bit_set(1));

        tagged_pointer.clear_bit(1);

        assert_eq!(*tagged_pointer, 1);
        assert!(!tagged_pointer.is_bit_set(1));

        tagged_pointer.set_bit(2);

        assert_eq!(*tagged_pointer, 1);
        assert!(tagged_pointer.is_bit_set(2));

        tagged_pointer.clear_bit(1);

        assert_eq!(*tagged_pointer, 1);
        assert!(tagged_pointer.is_bit_set(2));

        tagged_pointer.clear_bit(2);

        assert_eq!(*tagged_pointer, 1);
        assert!(!tagged_pointer.is_bit_set(2));
    }

    #[test]
    #[should_panic]
    pub fn bit_too_low() {
        std::panic::set_hook(Box::new(|info| {

        }));

        let val = 1;
        let mut tagged_pointer = TaggedPointer::new(&val);

        tagged_pointer.set_bit(0);
    }

    #[test]
    #[should_panic]
    pub fn bit_too_high() {
        std::panic::set_hook(Box::new(|info| {

        }));

        let val = 1;
        let mut tagged_pointer = TaggedPointer::new(&val);

        tagged_pointer.set_bit(9);
    }

    #[test]
    pub fn mark_word() {
        let val = 1;
        let mut tagged_pointer = TaggedPointer::new(&val);

        tagged_pointer.set_bit(1);

        assert_eq!(0b10000000, tagged_pointer.get_mark_word());

        tagged_pointer.set_bit(2);

        assert_eq!(0b11000000, tagged_pointer.get_mark_word());

        tagged_pointer.set_bit(3);

        assert_eq!(0b11100000, tagged_pointer.get_mark_word());

        tagged_pointer.set_bit(4);

        assert_eq!(0b11110000, tagged_pointer.get_mark_word());

        tagged_pointer.set_bit(5);

        assert_eq!(0b11111000, tagged_pointer.get_mark_word());

        tagged_pointer.set_bit(6);

        assert_eq!(0b11111100, tagged_pointer.get_mark_word());

        tagged_pointer.set_bit(7);

        assert_eq!(0b11111110, tagged_pointer.get_mark_word());

        tagged_pointer.set_bit(8);

        assert_eq!(0b11111111, tagged_pointer.get_mark_word());
    }

    #[test]
    pub fn set_mark_word() {
        let val = 1;

        let mut tagged_pointer = TaggedPointer::new(&val);

        assert_eq!(0b00000000, tagged_pointer.get_mark_word());

        let new_mark_word: u8 = 0b11110000;

        tagged_pointer.set_mark_word(new_mark_word);

        assert_eq!(new_mark_word, tagged_pointer.get_mark_word());

        assert_eq!(1, *tagged_pointer);

        let different_mark_word: u8 = 0b10001000;

        tagged_pointer.set_mark_word(different_mark_word);

        assert_eq!(different_mark_word, tagged_pointer.get_mark_word());

        assert_eq!(1, *tagged_pointer);
    }

    #[test]
    pub fn new_with_mark_word() {
        let val = 1;

        let mark_word = 0b01010101;

        let tagged_pointer = TaggedPointer::new_with_mark_word(&val, mark_word);

        assert_eq!(tagged_pointer.get_mark_word(), mark_word);
    }
}