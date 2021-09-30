const MARK_MASK: u64 = 1 << 49;
const REMAP_MASK: u64 = 1 << 50;

pub (crate) struct Address {
    address: u64,
    size: u64
}

impl Address {

    pub (crate) fn new(address: u64, size: u64) -> Self {
        Address {
            address,
            size
        }
    }

    pub (crate) fn mark(&mut self) {
        self.address = self.address | MARK_MASK;
    }

    pub (crate) fn clear_mark(&mut self) {
        self.address = self.address ^ MARK_MASK;
    }

    pub (crate) fn is_marked(&self) -> bool {
        return self.address & MARK_MASK != 0;
    }

    pub (crate) fn remap(&mut self) {
        self.address = self.address | REMAP_MASK;
    }

    pub (crate) fn clear_remap(&mut self) {
        self.address = self.address ^ REMAP_MASK;
    }

    pub (crate) fn is_remapped(&self) -> bool {
        return self.address & REMAP_MASK != 0;
    }
}

mod tests {
    use crate::address::Address;

    #[test]
    pub (crate) fn address_instantiation() {
        let mut address = Address::new(0, 4);

        assert_eq!(4, address.size);
        assert_eq!(false, address.is_marked());
        assert_eq!(false, address.is_remapped());

        address.mark();

        assert_eq!(true, address.is_marked());
        assert_eq!(false, address.is_remapped());

        address.clear_mark();

        assert_eq!(false, address.is_marked());
        assert_eq!(false, address.is_remapped());

        address.remap();

        assert_eq!(false, address.is_marked());
        assert_eq!(true, address.is_remapped());

        address.clear_remap();

        assert_eq!(false, address.is_marked());
        assert_eq!(false, address.is_remapped());
    }
}