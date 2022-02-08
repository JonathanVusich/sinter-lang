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