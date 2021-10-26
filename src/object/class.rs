#[derive(Eq, PartialEq, Debug)]
pub struct Class {
    object_size: usize
}

impl Class {

    pub fn new(object_size: usize) -> Class {
        Class {
            object_size
        }
    }


    pub fn object_size(&self) -> usize {
        self.object_size
    }
}

