use std::path::{Path, PathBuf};

#[cfg(test)]
pub fn resolve_test_path(module: &str, test: &str) -> Box<Path> {
    let mut pathbuf = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    pathbuf.push(module);
    pathbuf.push(test);
    pathbuf.into_boxed_path()
}
