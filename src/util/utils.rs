use std::fs::File;
use std::io::{BufReader, BufWriter};
use std::path::{Path, PathBuf};
use anyhow::Result;
use serde::{Deserialize, Serialize};
use serde::de::DeserializeOwned;

#[cfg(test)]
pub fn resolve_test_path(module: &str, test: &str) -> Box<Path> {
    let mut pathbuf = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    pathbuf.push(module);
    pathbuf.push(test);
    pathbuf.into_boxed_path()
}

#[cfg(test)]
pub fn save<T: Serialize>(module: &str, test: &str, data: T) -> Result<()> {
    let file = File::open(resolve_test_path(module, test))?;
    let writer = BufWriter::new(file);
    Ok(serde_json::to_writer(writer, &data)?)
}

#[cfg(test)]
pub fn load<T: DeserializeOwned>(module: &str, test: &str) -> Result<T> {
    let file = File::open(resolve_test_path(module, test))?;
    let reader = BufReader::new(file);
    Ok(serde_json::from_reader(reader)?)
}
