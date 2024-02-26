#[cfg(test)]
mod tests {
    use std::fmt::Debug;
    use std::path::PathBuf;

    use serde::de::DeserializeOwned;
    use serde::Serialize;

    use diagnostics::Diagnostics;

    #[derive(Copy, Clone)]
    enum SnapshotType {
        Parsed,
        Validated,
        Resolved,
        Typed,
    }

    impl SnapshotType {
        fn path(&self) -> &str {
            match self {
                SnapshotType::Parsed => "parsed",
                SnapshotType::Validated => "validated",
                SnapshotType::Resolved => "resolved",
                SnapshotType::Typed => "typed",
            }
        }
    }

    fn resolve_file_path(path: &str) -> PathBuf {
        let mut pathbuf = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        pathbuf.push("sources");
        pathbuf.push("file");
        pathbuf.push(path);
        pathbuf.set_extension("si");

        pathbuf
    }

    fn resolve_snapshot_path(path: &str, snapshot_type: SnapshotType) -> PathBuf {
        let mut pathbuf = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        pathbuf.push("snapshots");
        pathbuf.push(path);
        pathbuf.push(snapshot_type.path());
        pathbuf.set_extension("snap");
        pathbuf
    }

    fn load_single_file_crate(file: &str) -> String {
        let file_path = resolve_file_path(file);
        eprintln!("{:?}", &file_path);
        std::fs::read_to_string(file_path).unwrap()
    }

    fn compare_or_snapshot<V: Serialize>(val: &V, path: &str, snapshot_type: SnapshotType) {
        let serialized_val = ron::ser::to_string_pretty(
            val,
            ron::ser::PrettyConfig::new()
                .indentor("  ".to_string())
                .compact_arrays(true),
        )
        .unwrap();

        if let Some(saved_val) = load_snapshot(path, snapshot_type) {
            assert_eq!(saved_val, serialized_val);
        } else {
            save_snapshot(serialized_val, path, snapshot_type);
        }
    }

    fn save_snapshot(val: String, path: &str, snapshot_type: SnapshotType) {
        let snapshot_path = resolve_snapshot_path(path, snapshot_type);
        if let Some(parent) = &snapshot_path.parent() {
            std::fs::create_dir_all(parent).unwrap();
        }
        std::fs::write(snapshot_path, val).unwrap();
    }

    fn load_snapshot(path: &str, snapshot_type: SnapshotType) -> Option<String> {
        let snapshot_path = resolve_snapshot_path(path, snapshot_type);
        std::fs::read_to_string(snapshot_path).ok()
    }

    fn validate_result<T: Serialize>(
        result: Result<T, Diagnostics>,
        path: &str,
        snapshot_type: SnapshotType,
    ) -> Option<T> {
        match result {
            Ok(val) => {
                compare_or_snapshot(&val, path, snapshot_type);
                Some(val)
            }
            Err(diagnostics) => {
                compare_or_snapshot(&diagnostics, path, snapshot_type);
                None
            }
        }
    }

    fn validate_diagnostics<T>(
        result: Result<T, Diagnostics>,
        path: &str,
        snapshot_type: SnapshotType,
    ) -> Option<T> {
        match result {
            Ok(val) => Some(val),
            Err(diagnostics) => {
                compare_or_snapshot(&diagnostics, path, snapshot_type);
                None
            }
        }
    }

    macro_rules! single_crate {
        ($name:ident) => {
            #[test]
            pub fn $name() {
                let name = ::std::stringify!($name);
                let mut compiler = ::sinter_lang::compiler::compiler::Compiler::default();
                let application = ::sinter_lang::compiler::compiler::Application::Inline {
                    code: load_single_file_crate(name),
                };
                validate_result(
                    compiler.parse_crates(application),
                    name,
                    SnapshotType::Parsed,
                )
                .and_then(|parsed_krates| {
                    validate_diagnostics(
                        compiler.validate_crates(parsed_krates),
                        name,
                        SnapshotType::Validated,
                    )
                })
                .and_then(|parsed_krates| {
                    validate_result(
                        compiler.resolve_crates(parsed_krates),
                        name,
                        SnapshotType::Resolved,
                    )
                })
                .and_then(|hir_map| {
                    validate_result(compiler.infer_types(hir_map), name, SnapshotType::Typed)
                });
            }
        };
    }

    single_crate!(basic_enum);
}
