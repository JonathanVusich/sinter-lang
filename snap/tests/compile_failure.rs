#[test]
pub fn no_specified_type() {
    let t = trybuild::TestCases::new();
    t.compile_fail("tests/no_specified_type.rs")
}

#[test]
pub fn not_json_serializable() {
    let t = trybuild::TestCases::new();
    t.compile_fail("tests/not_json_serializable.rs")
}
