use snap::snapshot;

pub fn main() {

    #[derive(PartialEq, Debug)]
    struct NotSerializable(usize, usize);

    #[snapshot]
    fn not_json_serializable() -> NotSerializable {
        NotSerializable { 0: 0, 1: 0 }
    }
}