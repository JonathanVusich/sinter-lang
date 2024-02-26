use serde::{Deserialize, Serialize};

#[repr(transparent)]
#[derive(
    Copy, Clone, PartialEq, Eq, Ord, Default, PartialOrd, Debug, Hash, Serialize, Deserialize,
)]
#[serde(transparent)]
pub struct LocalDefId {
    local_id: u32,
}

impl LocalDefId {
    pub fn new(local_id: u32) -> Self {
        Self { local_id }
    }

    pub fn to_def_id(&self, crate_id: CrateId) -> DefId {
        DefId {
            crate_id: crate_id.into(),
            local_id: self.local_id,
        }
    }
}

impl From<LocalDefId> for usize {
    fn from(value: LocalDefId) -> Self {
        value.local_id.try_into().unwrap()
    }
}

impl From<u32> for LocalDefId {
    fn from(value: u32) -> Self {
        LocalDefId { local_id: value }
    }
}

#[derive(PartialEq, Eq, Hash, Debug, Default, Clone, Copy, Serialize, Deserialize)]
pub struct DefId {
    crate_id: u32,
    local_id: u32,
}

impl DefId {
    pub fn crate_id(&self) -> CrateId {
        CrateId::new(self.crate_id)
    }
    pub fn local_id(&self) -> LocalDefId {
        LocalDefId::new(self.local_id)
    }
}

#[derive(PartialEq, Eq, Debug, Default, Copy, Clone, PartialOrd, Ord, Serialize, Deserialize)]
pub struct CrateId {
    id: u32,
}

impl CrateId {
    pub fn new(id: u32) -> Self {
        Self { id }
    }

    pub fn as_usize(self) -> usize {
        self.id as usize
    }
}

impl From<CrateId> for u32 {
    fn from(value: CrateId) -> Self {
        value.id
    }
}

#[derive(PartialEq, Eq, Debug, Hash, Default, Copy, Clone, Serialize, Deserialize)]
pub struct ModuleId {
    crate_id: u32,
    module_id: u32,
}

impl ModuleId {
    pub fn new(crate_id: u32, module_id: u32) -> Self {
        Self {
            crate_id,
            module_id,
        }
    }

    pub fn crate_id(self) -> usize {
        self.crate_id as usize
    }

    pub fn module_id(self) -> usize {
        self.module_id as usize
    }
}

#[derive(PartialEq, Debug, Default, Serialize, Deserialize)]
pub struct IdGenerator {
    crate_id: u32,
    local_def_id: u32,
}

impl IdGenerator {
    pub fn crate_id(&mut self) -> CrateId {
        let id = self.crate_id;
        self.local_def_id = 0;
        self.crate_id += 1;
        CrateId::new(id)
    }

    pub fn local_def_id(&mut self) -> LocalDefId {
        let id = self.local_def_id;
        self.local_def_id += 1;
        id.into()
    }
}
