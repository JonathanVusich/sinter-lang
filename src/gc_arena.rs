



pub (crate) struct GCArena {
    large_objects: Vec<u8>
}

impl GCArena {
    pub fn new() -> Self {
        GCArena {
            large_objects: vec![]
        }
    }
}