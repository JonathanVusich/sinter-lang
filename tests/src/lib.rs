#![allow(unused)]

#[cfg(test)]
mod tests {
    use std::fmt::Debug;
    use std::path::PathBuf;

    use serde::Serialize;

    use diagnostics::Diagnostics;

    #[derive(Copy, Clone)]
    enum TestType {
        Crate,
        SingleFile,
    }

    impl TestType {
        fn path(&self) -> &str {
            match self {
                TestType::Crate => "crate",
                TestType::SingleFile => "single_file",
            }
        }
    }

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

    fn resolve_snapshot_path(
        path: &str,
        test_type: TestType,
        snapshot_type: SnapshotType,
    ) -> PathBuf {
        let mut pathbuf = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        pathbuf.push("snapshots");
        pathbuf.push(test_type.path());
        pathbuf.push(path);
        pathbuf.push(snapshot_type.path());
        pathbuf.set_extension("snap");
        pathbuf
    }

    fn load_single_file_crate(file: &str) -> String {
        let file_path = resolve_file_path(file);
        std::fs::read_to_string(file_path).unwrap()
    }

    fn compare_or_snapshot<V: Serialize>(
        val: &V,
        path: &str,
        test_type: TestType,
        snapshot_type: SnapshotType,
    ) {
        let serialized_val = ron::ser::to_string_pretty(
            val,
            ron::ser::PrettyConfig::new()
                .indentor("  ".to_string())
                .compact_arrays(true),
        )
        .unwrap();

        if let Some(saved_val) = load_snapshot(path, test_type, snapshot_type) {
            pretty_assertions::assert_eq!(saved_val, serialized_val);
        } else {
            save_snapshot(serialized_val, path, test_type, snapshot_type);
        }
    }

    fn save_snapshot(val: String, path: &str, test_type: TestType, snapshot_type: SnapshotType) {
        let snapshot_path = resolve_snapshot_path(path, test_type, snapshot_type);
        if let Some(parent) = &snapshot_path.parent() {
            std::fs::create_dir_all(parent).unwrap();
        }
        std::fs::write(snapshot_path, val).unwrap();
    }

    fn load_snapshot(
        path: &str,
        test_type: TestType,
        snapshot_type: SnapshotType,
    ) -> Option<String> {
        let snapshot_path = resolve_snapshot_path(path, test_type, snapshot_type);
        std::fs::read_to_string(snapshot_path).ok()
    }

    fn validate_result<T: Serialize>(
        result: Result<T, Diagnostics>,
        path: &str,
        test_type: TestType,
        snapshot_type: SnapshotType,
    ) -> Option<T> {
        match result {
            Ok(val) => {
                compare_or_snapshot(&val, path, test_type, snapshot_type);
                Some(val)
            }
            Err(diagnostics) => {
                compare_or_snapshot(&diagnostics, path, test_type, snapshot_type);
                None
            }
        }
    }

    fn validate_diagnostics<T>(
        result: Result<T, Diagnostics>,
        path: &str,
        test_type: TestType,
        snapshot_type: SnapshotType,
    ) -> Option<T> {
        match result {
            Ok(val) => Some(val),
            Err(diagnostics) => {
                compare_or_snapshot(&diagnostics, path, test_type, snapshot_type);
                None
            }
        }
    }

    macro_rules! single_file {
        ($name:ident) => {
            #[test]
            pub fn $name() {
                let name = ::std::stringify!($name);
                let mut compiler = ::compiler::Compiler::default();
                let application = ::compiler::Application::Inline {
                    code: load_single_file_crate(name),
                };

                validate_result(
                    compiler.parse_crates(application),
                    name,
                    TestType::SingleFile,
                    SnapshotType::Parsed,
                )
                .and_then(|parsed_krates| {
                    validate_diagnostics(
                        compiler.validate_crates(parsed_krates),
                        name,
                        TestType::SingleFile,
                        SnapshotType::Validated,
                    )
                })
                .and_then(|parsed_krates| {
                    validate_result(
                        compiler.resolve_crates(parsed_krates),
                        name,
                        TestType::SingleFile,
                        SnapshotType::Resolved,
                    )
                })
                .and_then(|hir_map| {
                    validate_result(
                        compiler.infer_types(hir_map),
                        name,
                        TestType::SingleFile,
                        SnapshotType::Typed,
                    )
                });
            }
        };
    }

    single_file!(basic_enum);
    single_file!(classes_and_vars);
    single_file!(enum_match);
    single_file!(enum_message);
    single_file!(generic_lists);
    single_file!(hello_world);
    single_file!(impl_trait);
    single_file!(infer_generics);
    single_file!(int_match);
    single_file!(main_fn);
    single_file!(mutable_assignment);
    single_file!(print_fn);
    single_file!(rectangle_class);
    single_file!(returning_error_union);
    single_file!(sum_fn);
    single_file!(trait_vs_generic);
    single_file!(use_stmts);
    single_file!(var_declarations);
    single_file!(vector_enum);
    single_file!(f64_to_i64_error);
}
