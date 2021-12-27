use bit_set::BitSet;
use crate::class::class::Class;

use crate::opcode::OpCode;

pub struct Function<'a> {
    name: &'a str,
    parameters: Vec<Parameter<'a>>,
    code: Vec<OpCode>,
}

pub struct Parameter<'a> {
    class: &'a Class,
    name: &'a str
}

impl<'a> Function<'a> {

    pub fn new(name: &'a str) -> Self {
        Self {
            name,
            parameters: Vec::new(),
            code: Vec::new()
        }
    }

    pub fn call() {
        
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

