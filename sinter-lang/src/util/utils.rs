use anyhow::Result;
use serde::de::DeserializeOwned;
use serde::{Deserialize, Serialize};
use std::fs::{File, OpenOptions};
use std::io::{BufReader, BufWriter};
use std::path::{Path, PathBuf};

#[cfg(test)]
pub fn resolve_code_example_path(path: &str) -> PathBuf {
    let mut pathbuf = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    pathbuf.push("sources");
    pathbuf.push("short_examples");
    pathbuf.push(path);

    pathbuf
}

#[cfg(test)]
pub fn read_code_example(path: &str) -> String {
    let pathbuf = resolve_code_example_path(path);

    std::io::read_to_string(File::open(pathbuf).expect("File not found!")).expect("File not found!")
}

#[cfg(test)]
pub fn resolve_test_krate_path(path: &str) -> PathBuf {
    let mut pathbuf = PathBuf::from(env!("CARGO_MANIFEST_DIR"));

    pathbuf.push("sources");
    pathbuf.push("crates");
    pathbuf.push(path);

    pathbuf
}
