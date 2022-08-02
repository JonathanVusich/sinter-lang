use std::path::{Path, PathBuf};

#[cfg(test)]
pub fn resolve_test_path(local_path: &Path) -> Box<Path> {
    let mut pathbuf = PathBuf::new();
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    pathbuf.push(Path::new(manifest_dir));
    pathbuf.push(local_path);
    pathbuf.into_boxed_path()
}
