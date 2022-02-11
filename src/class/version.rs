use std::cmp::Ordering;

pub const CURRENT_VERSION: Version = Version::new(0, 1);

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct Version {
    major: u16,
    minor: u16,
}

impl Version {
    
    pub fn new(major: u16, minor: u16) -> Self {
        Self {
            major, 
            minor
        }
    }
}

impl PartialOrd<Self> for Version {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Version {
    fn cmp(&self, other: &Self) -> Ordering {
        return self.major.cmp(&other.major).cmp(&self.minor.cmp(&other.minor))
    }
}