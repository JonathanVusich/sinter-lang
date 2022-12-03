pub trait ToBytes<const LEN: usize>: Sized + Copy {
    fn write(&self) -> [u8; LEN];
}
