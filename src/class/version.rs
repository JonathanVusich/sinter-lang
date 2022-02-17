use std::cmp::Ordering;
use std::io::{Error, ErrorKind};
use crate::bytes::serializers::{ByteReader, ByteWriter};
use crate::bytes::serializable::Serializable;

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

impl Serializable for Version {

    fn read(byte_reader: &mut impl ByteReader) -> Result<Self, Error> {
        let major = u16::read(byte_reader)?;
        let minor = u16::read(byte_reader)?;

        Ok(Self {
            major,
            minor,
        })
    }

    fn write(&self, byte_writer: &mut impl ByteWriter) -> Result<(), Error> {
        self.major.write(byte_writer)?;
        self.minor.write(byte_writer)
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