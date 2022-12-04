#[macro_use]
extern crate snap;

use serde::{Serialize, Deserialize};

#[derive(Serialize, Deserialize, PartialEq, Debug)]
pub enum Platform {
    Windows,
    Linux,
    MacOS
}

#[test]
pub fn test_fn() {

}

#[test]
#[snapshot]
pub fn inner_func() -> usize {
    123
}

#[test]
#[snapshot]
pub fn serialize_complex_struct() -> Platform {
    Platform::MacOS
}

#[test]
#[snapshot]
pub fn serialize_error() -> Platform {
    Platform::MacOS
}
