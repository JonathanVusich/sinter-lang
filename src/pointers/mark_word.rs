const BIT_1_MASK: u8 = compute_mask(1);
const BIT_2_MASK: u8 = compute_mask(2);
const BIT_3_MASK: u8 = compute_mask(3);
const BIT_4_MASK: u8 = compute_mask(4);
const BIT_5_MASK: u8 = compute_mask(5);
const BIT_6_MASK: u8 = compute_mask(6);
const BIT_7_MASK: u8 = compute_mask(7);
const BIT_8_MASK: u8 = compute_mask(8);

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct MarkWord {
    word: u8
}

impl MarkWord {

    pub fn set_bit(&mut self, bit: u8) {
        let mask = get_mask(bit);
        self.word |= mask
    }

    pub fn clear_bit(&mut self, bit: u8) {
        let mask = get_mask(bit);
        self.word &= !mask
    }

    pub fn is_bit_set(&self, bit: u8) -> bool {
        let mask = get_mask(bit);
        self.word & mask != 0
    }
}

#[inline(always)]
const fn get_mask(bit: u8) -> u8 {
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

const fn compute_mask(bit: u8) -> u8 {
    assert!(bit > 0 && bit < 9, "Unsupported bit index!");
    let mut mask: u8 = 1;
    mask = mask.rotate_right(bit as u32);
    mask as u8
}

impl From<u8> for MarkWord {

    fn from(word: u8) -> Self {
        Self { word }
    }
}

impl From<MarkWord> for u8 {

    fn from(word: MarkWord) -> Self {
        word.word
    }
}

mod tests {
    use crate::pointers::mark_word::MarkWord;

    #[test]
    pub fn flipping_bits() {
        let mut mark_word: MarkWord = 0.into();

        mark_word.set_bit(1);

        println!("{:?}", mark_word);

        assert!(mark_word.is_bit_set(1));

        mark_word.clear_bit(1);

        assert!(!mark_word.is_bit_set(1));

        mark_word.set_bit(2);

        assert!(mark_word.is_bit_set(2));

        mark_word.clear_bit(1);

        assert!(mark_word.is_bit_set(2));

        mark_word.clear_bit(2);

        assert!(!mark_word.is_bit_set(2));
    }

    #[test]
    #[should_panic]
    pub fn bit_too_low() {
        std::panic::set_hook(Box::new(|info| {

        }));

        let mut mark_word = MarkWord::from(0);
        mark_word.set_bit(0);
    }

    #[test]
    #[should_panic]
    pub fn bit_too_high() {
        std::panic::set_hook(Box::new(|info| {

        }));

        let mut mark_word = MarkWord::from(0);
        mark_word.set_bit(9);
    }
}