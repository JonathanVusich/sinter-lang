use crate::pointers::heap_pointer::HeapPointer;

pub trait Value<const SIZE: usize> {

    fn from_bytes(bytes: [u8; SIZE]) -> Self;
    fn is_reference_type() -> bool;
    fn len() -> usize;

    fn to_bytes(self) -> [u8; SIZE];
}

macro_rules! value_impl {
    ($type:ty) => {

        impl Value<{ std::mem::size_of::<$type>() }> for $type {

            fn from_bytes(bytes: [u8; std::mem::size_of::<$type>()]) -> Self {
                <$type>::from_ne_bytes(bytes)
            }

            fn is_reference_type() -> bool {
                false
            }

            fn len() -> usize {
                std::mem::size_of::<$type>()
            }

            fn to_bytes(self) -> [u8; std::mem::size_of::<$type>()] {
                self.to_ne_bytes()
            }
        }
    };
}

// Implement value for numeric types.
value_impl!(i8);
value_impl!(i16);
value_impl!(i32);
value_impl!(i64);

value_impl!(f32);
value_impl!(f64);

impl Value<8> for HeapPointer {
    fn from_bytes(bytes: [u8; 8]) -> Self {
        HeapPointer::from_address(u64::from_ne_bytes(bytes))
    }

    fn is_reference_type() -> bool {
        true
    }


    fn len() -> usize {
        8
    }

    fn to_bytes(self) -> [u8; 8] {
        let address: u64 = self.into();
        address.to_ne_bytes()
    }
}

mod tests {
    use crate::pointers::heap_pointer::HeapPointer;
    use crate::values::value::Value;

    #[test]
    pub fn conversion() {
        // let value = HeapPointer::from_address(1);
        assert_eq!(HeapPointer::len(), 8);

    }
}