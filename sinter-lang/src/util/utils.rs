use anyhow::Result;
use serde::de::DeserializeOwned;
use serde::{Deserialize, Serialize};
use std::fs::{File, OpenOptions};
use std::io::{BufReader, BufWriter};
use std::path::{Path, PathBuf};

#[cfg(test)]
pub fn read_file<const N: usize>(paths: [&str; N]) -> String {
    let mut pathbuf = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    pathbuf.push("sources");
    for path in paths {
        pathbuf.push(path);
    }

    std::io::read_to_string(File::open(pathbuf).expect("File not found!")).expect("File not found!")
}
