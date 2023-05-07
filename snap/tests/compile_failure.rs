#[test]
pub fn no_specified_type() {
    let t = trybuild::TestCases::new();
    t.compile_fail("tests/ui/*.rs")
}
