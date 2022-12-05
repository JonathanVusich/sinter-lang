use std::fs::File;
use snap::{snapshot};

#[test]
#[snapshot]
pub fn assert_file_is_created() -> usize {
    let vec = vec![0];
    let path = snapshot_path!();

    assert!(File::open(path).is_err());

    0
}


#[test]
pub fn test_fn() {
}