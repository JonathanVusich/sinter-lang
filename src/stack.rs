use std::convert::TryInto;

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

    pub (crate) fn push_i64(&mut self, val: &i64) {
        self.internal.extend_from_slice(&val.to_ne_bytes());
    }

    pub (crate) fn push_f64(&mut self, val: &f64) {
        self.internal.extend_from_slice(&val.to_ne_bytes());
    }

    pub (crate) fn read_i64(&mut self) -> i64 {
        let index = self.internal.len() - 8;
        let bytes: [u8; 8] = self.internal[index..].try_into().unwrap();
        let long = i64::from_ne_bytes(bytes);
        self.internal.truncate(index);
        return long;
    }

    pub (crate) fn read_f64(&mut self) -> f64 {
        let index = self.internal.len() - 8;
        let bytes: [u8; 8] = self.internal[index..].try_into().unwrap();
        let double = f64::from_ne_bytes(bytes);
        self.internal.truncate(index);
        return double;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_i64() {
        let mut stack = Stack::new();
        stack.push_i64(&12);

        assert_eq!(stack.read_i64(), 12);
    }

    #[test]
    fn test_f64() {
        let mut stack = Stack::new();
        stack.push_f64(&12.23);

        assert_eq!(stack.read_f64(), 12.23);
    }
}