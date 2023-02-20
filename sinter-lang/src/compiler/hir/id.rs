use serde::{Serialize, Deserialize};

macro_rules! create_id {
    ($name:ident) => {
        #[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Serialize, Deserialize)]
        #[serde(transparent)]
        pub struct $name {
            id: u32,
        }

        impl $name {
            pub fn new(id: u32) -> Self {
                Self {
                    id,
                }
            }
        }
    }
}

create_id!(ModuleId);
create_id!(ConstLetId);