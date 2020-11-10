#[derive(Debug)]
pub (crate) struct Stack {
    internal: Vec<u8>
}

impl Stack {

    pub (crate) fn new() -> Stack {
        return Stack {
            internal: vec![]
        }
    }

    pub (crate) fn push_i64(&mut self, val: i64) {
        let ibytes: [u8; 8] = val.to_ne_bytes();
        self.internal.extend_from_slice(&ibytes);
    }

    pub (crate) fn push_f64(&mut self, val: f64) {
        let fbytes = val.to_ne_bytes();
        self.internal.extend_from_slice(&fbytes);
    }

    pub (crate) fn read_i64(&mut self) -> i64 {
        let mut bytes: [u8; 8] = [0; 8];
        let index = self.internal.len() - 8;
        bytes.copy_from_slice(&self.internal[index..]);
        self.internal.truncate(index);
        let long = i64::from_ne_bytes(bytes);
        return long;
    }

    pub (crate) fn read_f64(&mut self) -> f64 {
        let mut bytes: [u8; 8] = [0; 8];
        let index = self.internal.len() - 8;
        bytes.copy_from_slice(&self.internal[index..]);
        self.internal.truncate(index);
        let double = f64::from_ne_bytes(bytes);
        return double;
    }
}