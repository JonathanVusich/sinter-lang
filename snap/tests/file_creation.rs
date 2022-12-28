use snap::{snapshot, snapshot_folder};
use std::fs::File;
use std::io::BufReader;
use std::panic;
use std::path::{Path, PathBuf};

#[test]
pub fn assert_file_is_created() -> std::io::Result<()> {
    let mut path: PathBuf = snapshot_folder!();
    path.push("write_usize");
    path.set_extension("snap");

    fn actual_test(path: &Path) {
        File::open(&path).unwrap_err();

        #[snapshot]
        pub fn write_usize() -> usize {
            17
        }

        write_usize().unwrap();

        let open_file = File::open(&path).unwrap();

        let value: usize = serde_json::from_reader(BufReader::new(open_file)).unwrap();

        assert_eq!(17, value);
    }

    let _ignored = panic::catch_unwind(|| {
        actual_test(&path);
    });

    std::fs::remove_file(&path)
}
