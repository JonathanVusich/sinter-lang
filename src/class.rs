use std::convert::TryInto;

use rand::Rng;

static MAX_CLASS_INDEX: u32 = 16_777_215;

static HASHCODE_INDEX: usize = 0;
static GENERATION_INDEX: usize = 4;
static CLASS_INDEX: usize = 5;
static FIELD_RANGE_START: usize = 8;

pub (crate) fn identity_hashcode(class_instance: &mut [u8]) -> u32 {
    let mut rng = rand::thread_rng();
    let hashcode = u32::from_be_bytes(class_instance[..GENERATION_INDEX].try_into().unwrap());
    if hashcode == 0 {
        let computed_hashcode: u32 = rng.gen();
        let bytes = computed_hashcode.to_be_bytes();
        for i in HASHCODE_INDEX..GENERATION_INDEX {
            class_instance[i as usize] = bytes[i as usize];
        }
        computed_hashcode
    } else {
        hashcode
    }
}

pub (crate) fn get_generation(class_instance: &[u8]) -> u8 {
    return class_instance[GENERATION_INDEX];
}

pub (crate) fn bump_generation(class_instance: &mut [u8]) {
    let generation = get_generation(&class_instance);
    class_instance[GENERATION_INDEX] = generation + 1;
}

pub (crate) fn get_class_index(class_instance: &[u8]) -> u32 {
    let class_word = &class_instance[CLASS_INDEX..FIELD_RANGE_START];
    let padded_word: [u8; 4] = [0, class_word[0], class_word[1], class_word[2]];
    return u32::from_be_bytes(padded_word);
}

pub (crate) fn set_class_index(class_instance: &mut [u8], class_index: u32) {
    assert!(class_index <= MAX_CLASS_INDEX);
    let bytes: [u8; 4] = class_index.to_be_bytes();
    for i in 0..3 {
        class_instance[CLASS_INDEX + i] = bytes[i + 1];
    }
}

mod tests {
    use crate::class::{bump_generation, get_class_index, set_class_index, get_generation, identity_hashcode, HASHCODE_INDEX, GENERATION_INDEX, MAX_CLASS_INDEX};

    #[test]
    fn hashcode_test() {
        let original_class_instance: Vec<u8> = vec![0, 0, 0, 0];
        let mut class_instance = original_class_instance.clone();
        let first_hashcode = identity_hashcode(&mut class_instance);

        assert_ne!(0, first_hashcode);
        assert_ne!(original_class_instance, class_instance);
        assert_eq!(first_hashcode.to_be_bytes(), class_instance[HASHCODE_INDEX..GENERATION_INDEX]);

        let second_hashcode = identity_hashcode(&mut class_instance);
        assert_eq!(first_hashcode, second_hashcode);
    }

    #[test]
    fn generations() {
        let class_instance: Vec<u8> = vec![0, 0, 0, 0, 0];
        assert_eq!(0, get_generation(&class_instance));

        let mut mutable_class_instance: Vec<u8> = vec![0, 0, 0, 0, 0];
        bump_generation(&mut mutable_class_instance);

        assert_eq!(1, get_generation(&mutable_class_instance));
    }

    #[test]
    fn class_index() {
        let class_instance: Vec<u8> = vec![0, 0, 0, 0, 0, 0, 0, 0];
        assert_eq!(0, get_class_index(&class_instance));

        let mut mutable_class_instance: Vec<u8> = vec![0, 0, 0, 0, 0, 0, 0, 128];
        assert_eq!(128, get_class_index(&mutable_class_instance));


        set_class_index(&mut mutable_class_instance, 16384);
        assert_eq!(16384, get_class_index(&mutable_class_instance));
        assert_eq!(vec![0, 0, 0, 0, 0, 0, 64, 0], mutable_class_instance);

        set_class_index(&mut mutable_class_instance, 65535);
        assert_eq!(65535, get_class_index(&mutable_class_instance));
        assert_eq!(vec![0, 0, 0, 0, 0, 0, 255, 255], mutable_class_instance);

        set_class_index(&mut mutable_class_instance, MAX_CLASS_INDEX);
        assert_eq!(MAX_CLASS_INDEX, get_class_index(&mutable_class_instance));
        assert_eq!(vec![0, 0, 0, 0, 0, 255, 255, 255], mutable_class_instance);
    }
}