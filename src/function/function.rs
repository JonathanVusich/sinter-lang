use bit_set::BitSet;
use crate::class::class::Class;

use crate::opcode::OpCode;

pub struct Function {
    name: &'static str,
    parameters: Vec<Parameter>,
    call_frame_size: usize,
    code: Vec<OpCode>,
}

#[derive(Eq, PartialEq, Debug)]
pub struct Parameter {
    parameter_type: &'static Class,
    name: &'static str
}

impl Function {

    pub fn new(name: &'static str) -> Self {
        Self {
            name,
            parameters: Vec::new(),
            call_frame_size: 0,
            code: Vec::new()
        }
    }

    pub fn call(&self) {

    }
}

mod tests {
    use crate::function::function::Function;

    #[test]
    pub fn constructor() {
        let function = Function::new("TestFunction");
        assert_eq!("TestFunction", function.name);
    }
}

