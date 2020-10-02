use crate::chunk::Chunk;

pub (crate) struct VirtualMachine {
    chunks: Vec<Chunk>
}

impl VirtualMachine {

    pub (crate) fn new() -> VirtualMachine {
        return VirtualMachine { chunks: vec![] }
    }

    pub (crate) fn interpret(&self, chunk: &Chunk) -> InterpretResult {
        
    }
}

pub (crate) enum InterpretResult {
    OK,
    CompileError,
    RuntimeError
}