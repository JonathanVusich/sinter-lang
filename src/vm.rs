use crate::chunk::Chunk;
use crate::opcode::*;
use crate::dis;
use crate::dis::disassemble_instruction;

const DEBUG: bool = false;

pub (crate) struct VirtualMachine {
    chunk: Chunk,
    debug: bool,
}

impl VirtualMachine {

    pub (crate) fn new(chunk: Chunk) -> VirtualMachine {
        return VirtualMachine {
            chunk,
            debug: true
        }
    }

    pub (crate) fn run(self) -> InterpretResult {
        let mut i: usize = 0;
        let bytecode = self.chunk.get_code();
        loop {
            if self.debug {
                disassemble_instruction(&self.chunk, i);
            }
            let instruction = bytecode[i];
            i += 1;
            match instruction {
                OP_RETURN => {
                    return InterpretResult::OK;
                }
                OP_U8_CONSTANT => {
                    let val = self.chunk.get_u8_constant(bytecode[i]);
                    i += 1;
                }
                OP_U16_CONSTANT => {
                    let val = self.chunk.get_u16_constant(bytecode[i]);
                    i += 1;
                }
                OP_U32_CONSTANT => {
                    let val = self.chunk.get_u32_constant(bytecode[i]);
                    i += 1;
                }
                OP_U64_CONSTANT => {
                    let val = self.chunk.get_u64_constant(bytecode[i]);
                    i += 1;
                }
                OP_U128_CONSTANT => {
                    let val = self.chunk.get_u128_constant(bytecode[i]);
                    i += 1;
                }
                OP_I8_CONSTANT => {
                    let val = self.chunk.get_i8_constant(bytecode[i]);
                    i += 1;
                }
                OP_I16_CONSTANT => {
                    let val = self.chunk.get_i16_constant(bytecode[i]);
                    i += 1;
                }
                OP_I32_CONSTANT => {
                    let val = self.chunk.get_i32_constant(bytecode[i]);
                    i += 1;
                }
                OP_I64_CONSTANT => {
                    let val = self.chunk.get_i64_constant(bytecode[i]);
                    i += 1;
                }
                OP_I128_CONSTANT => {
                    let val = self.chunk.get_i128_constant(bytecode[i]);
                    i += 1;
                }
                OP_F32_CONSTANT => {
                    let val = self.chunk.get_f32_constant(bytecode[i]);
                    i += 1;
                }
                OP_F64_CONSTANT => {
                    let val = self.chunk.get_f64_constant(bytecode[i]);
                    i += 1;
                }
                _ => {
                    return InterpretResult::RuntimeError;
                }
            }
        }
    }
}

pub (crate) enum InterpretResult {
    OK,
    CompileError,
    RuntimeError
}