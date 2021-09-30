pub (crate) struct GCArena {
    new_generation: Vec<u8>,
    permanent_generation: Vec<u8>
}

impl GCArena {
    pub fn new() -> Self {
        GCArena {
            new_generation: vec![],
            permanent_generation: vec![]
        }
    }
}