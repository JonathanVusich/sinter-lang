use std::convert::TryInto;
use rand::Rng;

static GENERATION_INDEX: u8 = 4;
static FIELD_RANGE_START: u8 = 5;

pub (crate) fn identity_hashcode(class_instance: &mut Vec<u8>) -> u32 {
    let mut rng = rand::thread_rng();
    let hashcode = u32::from_ne_bytes(class_instance[..GENERATION_INDEX].try_into().unwrap());
    if hashcode == 0 {
        let computed_hashcode: u32 = rng.gen();
        let bytes = computed_hashcode.to_ne_bytes();
        for i in 0..GENERATION_INDEX {
            class_instance[i] = bytes[i];
        }
        computed_hashcode
    } else {
        hashcode
    }
}

pub (crate) fn get_generation(class_instance: &Vec<u8>) -> u8 {
    return class_instance[4];
}

pub (crate) fn bump_generation(class_instance: &mut Vec<u8>) {
    let generation = get_generation(&class_instance);
    class_instance[4] = generation + 1;
}

mod tests {

    use crate::class::identity_hashcode;
    use crate::class::get_generation;
    use crate::class::bump_generation;

    #[test]
    fn hashcode_test() {
        let original_class_instance: Vec<u8> = vec![0, 0, 0, 0];
        let mut class_instance = original_class_instance.clone();
        let first_hashcode = identity_hashcode(&mut class_instance);

        assert_ne!(0, first_hashcode);
        assert_ne!(original_class_instance, class_instance);

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
}