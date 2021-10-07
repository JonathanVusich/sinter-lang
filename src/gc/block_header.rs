pub (crate) struct BlockHeader {
    block: *mut Block,
    bucket: *mut Bucket,
    holes: usize,
    next: DerefPointer<Block>,
    fragmented: bool
}
