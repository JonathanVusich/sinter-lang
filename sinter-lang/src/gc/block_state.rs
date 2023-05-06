#[derive(PartialEq, Debug)]
pub(crate) enum BlockState {
    Full,
    Free,
    Recycled,
}
