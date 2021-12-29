use bit_set::BitSet;
use crate::class::class::Class;

use crate::opcode::OpCode;

pub struct Function {
    name: &'static str,
    parameters: Vec<Parameter>,
    code: Vec<OpCode>,
}

#[derive(Eq, PartialEq, Debug)]
pub struct Parameter {
    class: &'static Class,
    name: &'static str
}

impl Function {

    pub fn new(name: &'static str) -> Self {
        Self {
            name,
            parameters: Vec::new(),
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

