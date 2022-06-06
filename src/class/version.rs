use crate::bytes::serializable::Serializable;
use crate::bytes::serializers::{ByteReader, ByteWriter};
use std::cmp::Ordering;
use std::io::{Error, ErrorKind};

pub const CURRENT_VERSION: Version = Version { major: 0, minor: 1 };

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub struct Version {
    major: u16,
    minor: u16,
}

impl Version {
    pub fn new(major: u16, minor: u16) -> Self {
        Self { major, minor }
    }
}

impl Default for Version {
    fn default() -> Self {
        CURRENT_VERSION
    }
}

impl Serializable for Version {
    fn read(byte_reader: &mut impl ByteReader) -> Result<Self, Error> {
        let major = u16::read(byte_reader)?;
        let minor = u16::read(byte_reader)?;

        Ok(Self { major, minor })
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
        return self
            .major
            .cmp(&other.major)
            .cmp(&self.minor.cmp(&other.minor));
    }
}
