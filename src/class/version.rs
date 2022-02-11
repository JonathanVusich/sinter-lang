use std::cmp::Ordering;
use std::io::ErrorKind;
use crate::bytes::byte_reader::ByteReader;
use crate::bytes::from_bytes::FromBytes;

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

impl FromBytes for Version {

    fn load(byte_reader: &mut impl ByteReader) -> Result<Self, ErrorKind> {
        let major = u16::load(byte_reader)?;
        let minor = u16::load(byte_reader)?;

        Ok(Self {
            major,
            minor,
        })
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