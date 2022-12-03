#[macro_use]
extern crate snap;

#[test]
#[snapshot]
pub fn path() -> i32 {
    return 123;
}