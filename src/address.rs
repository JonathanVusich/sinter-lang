const MARK0_MASK: u64 = 1 << 61;
const MARK1_MASK: u64 = 1 << 62;
const REMAP_MASK: u64 = 1 << 63;

#[derive(PartialEq, Eq, Debug)]
pub (crate) struct Address(u64);

impl Address {

    pub (crate) fn new(address: u64) -> Self {
        let address = Address(address);
        debug_assert!(!address.is_mark0());
        debug_assert!(!address.is_mark1());
        debug_assert!(!address.is_remapped());
        return address;
    }

    pub (crate) fn mark0(&mut self) {
        self.0 = self.0 | MARK0_MASK;
    }

    pub (crate) fn mark1(&mut self) {
        self.0 = self.0 | MARK1_MASK;
    }

    pub (crate) fn remap(&mut self) {
        self.0 = self.0 | REMAP_MASK;
    }

    pub (crate) fn is_mark0(&self) -> bool {
        return self.0 & MARK0_MASK != 0;
    }

    pub (crate) fn is_mark1(&self) -> bool {
        return self.0 & MARK1_MASK != 0;
    }

    pub (crate) fn is_remapped(&self) -> bool {
        return self.0 & REMAP_MASK != 0;
    }

    pub (crate) fn clear_mark0(&mut self) {
        self.0 = self.0 ^ MARK0_MASK;
    }

    pub (crate) fn clear_mark1(&mut self) {
        self.0 = self.0 ^ MARK1_MASK;
    }

    pub (crate) fn clear_remap(&mut self) {
        self.0 = self.0 ^ REMAP_MASK;
    }
}

impl PartialEq<u64> for Address {
    fn eq(&self, other: &u64) -> bool {
        return self.0 == *other;
    }
}

impl PartialEq<Address> for u64 {
    fn eq(&self, other: &Address) -> bool {
        return *self == other.0;
    }
}

mod tests {
    use crate::address::Address;

    #[test]
    pub (crate) fn address_instantiation() {
        let mut address = Address::new(100);

        assert_eq!(address, 100);
        assert_eq!(100, address);
        assert_eq!(false, address.is_mark0());
        assert_eq!(false, address.is_mark1());
        assert_eq!(false, address.is_remapped());

        address.mark0();

        assert_eq!(true, address.is_mark0());
        assert_eq!(false, address.is_mark1());
        assert_eq!(false, address.is_remapped());

        address.clear_mark0();

        assert_eq!(false, address.is_mark0());
        assert_eq!(false, address.is_mark1());
        assert_eq!(false, address.is_remapped());

        address.mark1();

        assert_eq!(false, address.is_mark0());
        assert_eq!(true, address.is_mark1());
        assert_eq!(false, address.is_remapped());

        address.clear_mark1();

        assert_eq!(false, address.is_mark0());
        assert_eq!(false, address.is_mark1());
        assert_eq!(false, address.is_remapped());

        address.remap();

        assert_eq!(false, address.is_mark0());
        assert_eq!(false, address.is_mark1());
        assert_eq!(true, address.is_remapped());

        address.clear_remap();

        assert_eq!(false, address.is_mark0());
        assert_eq!(false, address.is_mark1());
        assert_eq!(false, address.is_remapped());
    }

    #[test]
    #[should_panic]
    pub (crate) fn constructor_with_remap_value() {
        std::panic::set_hook(Box::new(|_| {}));
        let address = Address::new(u64::MAX);
    }

    #[test]
    #[should_panic]
    pub (crate) fn constructor_with_mark1_value() {
        std::panic::set_hook(Box::new(|_| {}));
        let mut max_val = u64::MAX;
        max_val = max_val >> 1;
        let address = Address::new(max_val);
    }

    #[test]
    #[should_panic]
    pub (crate) fn constructor_with_mark0_value() {
        std::panic::set_hook(Box::new(|_| {}));
        let mut max_val = u64::MAX;
        max_val = max_val >> 2;
        let address = Address::new(max_val);
    }

    #[test]
    pub (crate) fn valid_constructor() {
        let mut max_val = u64::MAX;
        max_val = max_val >> 3;
        let address = Address::new(max_val);
    }
}