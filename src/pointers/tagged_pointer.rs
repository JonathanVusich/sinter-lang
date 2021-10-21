use std::ops::{Deref, DerefMut};

const BIT_1_MASK: usize = compute_mask(1);
const BIT_2_MASK: usize = compute_mask(2);
const BIT_3_MASK: usize = compute_mask(3);
const BIT_4_MASK: usize = compute_mask(4);
const BIT_5_MASK: usize = compute_mask(5);
const BIT_6_MASK: usize = compute_mask(6);
const BIT_7_MASK: usize = compute_mask(7);
const BIT_8_MASK: usize = compute_mask(8);

pub struct TaggedPointer<T> {
    ptr: *mut T
}

impl<T> TaggedPointer<T> {
    pub fn new(ptr: *mut T) -> Self {
        TaggedPointer { ptr }
    }

    pub fn set_bit(&mut self, bit: u32) {
        let val = self.ptr as usize;
        let mask = get_mask(bit);
        self.ptr = (self.ptr as usize | mask) as _
    }

    pub fn clear_bit(&mut self, bit: u32) {
        let val = self.ptr as usize;
        let mask = get_mask(bit);
        self.ptr = (self.ptr as usize & !mask) as _
    }

    pub fn is_bit_set(&self, bit: u32) -> bool {
        let val = self.ptr as usize;
        let mask = get_mask(bit);
        (val & mask) != 0
    }
}

#[inline(always)]
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

fn print_usize(val: usize) {
    let bytes = val.to_be_bytes();
    assert_eq!(bytes.len(), 8);
    for byte in bytes {
        print!("{:08b}", byte);
    }
    println!();
}

impl<T> Deref for TaggedPointer<T> {
    type Target = T;

    fn deref(&self) -> &T {
        let mut val = self.ptr as isize;

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
        let mut val = self.ptr as isize;

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
    use std::ops::Shl;

    use super::*;

    #[test]
    pub fn size() {
        assert_eq!(std::mem::size_of::<TaggedPointer<i32>>(), std::mem::size_of::<usize>());
    }

    #[test]
    pub fn dereferencing() {
        let mut val: i32 = 1;
        let mut tagged_pointer = TaggedPointer::new(&mut val);

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
        let mut val: i32 = 1;
        let mut tagged_pointer = TaggedPointer::new(&mut val);

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

        let mut val = 1;
        let mut tagged_pointer = TaggedPointer::new(&mut val);

        tagged_pointer.set_bit(0);
    }

    #[test]
    #[should_panic]
    pub fn bit_too_high() {
        std::panic::set_hook(Box::new(|info| {

        }));

        let mut val = 1;
        let mut tagged_pointer = TaggedPointer::new(&mut val);

        tagged_pointer.set_bit(9);
    }
}