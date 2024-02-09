use std::fs::File;
use std::path::PathBuf;

pub fn resolve_code_example_path(path: &str) -> PathBuf {
    let mut pathbuf = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    pathbuf.push("short_examples");
    pathbuf.push(path);

    pathbuf
}

pub fn read_code_example(path: &str) -> String {
    let pathbuf = resolve_code_example_path(path);

    std::io::read_to_string(File::open(pathbuf).expect("File not found!"))
        .expect("Unable to read file.")
}

pub fn resolve_test_krate_path(path: &str) -> PathBuf {
    let mut pathbuf = PathBuf::from(env!("CARGO_MANIFEST_DIR"));

    pathbuf.push("crates");
    pathbuf.push(path);

    pathbuf
}
