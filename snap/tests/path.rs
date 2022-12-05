use std::path::PathBuf;
use serde::{Serialize, Deserialize};
use snap::{snapshot, snapshot_path};

#[derive(Serialize, Deserialize, PartialEq, Eq, Debug)]
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
