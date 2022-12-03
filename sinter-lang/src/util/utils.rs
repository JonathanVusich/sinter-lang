use std::fs::{File, OpenOptions};
use std::io::{BufReader, BufWriter};
use std::path::{Path, PathBuf};
use anyhow::Result;
use serde::{Deserialize, Serialize};
use serde::de::DeserializeOwned;

#[cfg(test)]
pub fn resolve_test_path<const N: usize>(paths: [&str; N]) -> Box<Path> {
    let mut pathbuf = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    pathbuf.push("resources");
    pathbuf.push("test");
    for path in paths {
        pathbuf.push(path);
    }
    pathbuf.set_extension("json");

    pathbuf.into_boxed_path()
}

#[cfg(test)]
pub fn save<T: Serialize>(path: &Path, data: T) -> Result<()> {
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    let file = OpenOptions::new()
        .write(true)
        .create(true)
        .open(path)?;
    let writer = BufWriter::new(file);
    Ok(serde_json::to_writer_pretty(writer, &data)?)
}

#[cfg(test)]
pub fn load<T: DeserializeOwned>(path: &Path) -> Result<T> {
    let file = File::open(path)?;
    let reader = BufReader::new(file);
    Ok(serde_json::from_reader(reader)?)
}
