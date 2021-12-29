use crate::function::function::Parameter;
use crate::opcode::OpCode;
use crate::pointers::pointer::Pointer;

#[derive(Eq, PartialEq, Debug)]
pub struct Method {
    name: String,
    instance: Pointer<u8>,
    parameters: Vec<Parameter>,
    bytecode: Vec<OpCode>,
}