use bit_set::BitSet;
use crate::class::class::Class;

use crate::opcode::OpCode;
use crate::vm::call_frame::CallFrame;

#[derive(Eq, PartialEq, Debug)]
pub struct Function {
    name: &'static str,
    parameters: Vec<Parameter>,
    call_frame_size: usize,
    code: Vec<u8>,
    constants: Vec<u8>,
}

#[derive(Eq, PartialEq, Debug)]
pub struct Parameter {
    pub parameter_type: &'static Class,
    pub name: &'static str,
}

impl Function {

    pub fn new(name: &'static str) -> Self {
        Self {
            name,
            parameters: vec![],
            call_frame_size: 0,
            code: vec![],
            constants: vec![],
        }
    }

    pub fn name(&self) -> &str {
        self.name
    }

    pub fn parameters(&self) -> &[Parameter] {
        self.parameters.as_slice()
    }

    pub fn call_frame_size(&self) -> usize {
        self.call_frame_size
    }

    pub fn code(&self) -> &[u8] {
        self.code.as_slice()
    }

    pub fn constants(&self) -> &[u8] {
        self.constants.as_slice()
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

