#[macro_use]
extern crate snap;

#[test]
#[snapshot]
pub fn inner_func() -> usize {
    return 123;
}
