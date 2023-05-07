use snap::snapshot;

pub fn main() {
    #[snapshot]
    fn no_type_specified() {}
}
