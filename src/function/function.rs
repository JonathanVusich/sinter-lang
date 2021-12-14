use bit_set::BitSet;

use crate::opcode::OpCode;

pub struct Function  {
    code: Vec<OpCode>,
    stack_map: BitSet,
    lines: Vec<usize>,
}

impl Function {

    pub fn new() -> Self {
        Self {
            code: Vec::new(),
            stack_map: BitSet::new(),
            lines: Vec::new(),
        }
    }

    pub fn line_for_index(&self, index: usize) -> Option<usize> {
        let mut i = 0;
        let mut count = 0;
        loop {
            if i < self.lines.len() {
                count += self.lines[i];
                if count > index {
                    return Some(i + 1);
                }
                i += 1;
            } else {
                return None;
            }
        }
    }
}

mod tests {
    use crate::function::function::Function;

    #[test]
    pub fn lines() {
        let mut function = Function::new();
        function.lines.push(12);
        function.lines.push(4);

        assert_eq!(1, function.line_for_index(0).unwrap());
        assert_eq!(1, function.line_for_index(11).unwrap());
        assert_eq!(2, function.line_for_index(12).unwrap());
        assert!(function.line_for_index(100).is_none());
    }
}

