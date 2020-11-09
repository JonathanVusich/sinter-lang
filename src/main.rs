mod chunk;
mod opcode;
mod vm;
mod macros;
mod dis;

use crate::chunk::Chunk;
use crate::vm::VirtualMachine;
use crate::opcode::*;

fn main() {

    let mut chunk = Chunk::new();

    // Test int
    chunk.add_opcode(OP_U8_CONSTANT, 1);
    let index = chunk.add_u8_constant(255);
    chunk.add_opcode(index, 1);

    // Test long
    chunk.add_opcode(OP_U64_CONSTANT, 1);
    let index = chunk.add_u64_constant(1254124);
    chunk.add_opcode(index, 1);

    // Test float
    chunk.add_opcode(OP_F32_CONSTANT, 2);
    let index = chunk.add_f32_constant(1.2345);
    chunk.add_opcode(index, 2);

    // Test double
    chunk.add_opcode(OP_F64_CONSTANT, 2);
    let index = chunk.add_f64_constant(12323412.123123452134);
    chunk.add_opcode(index, 2);

    chunk.add_opcode(OP_RETURN, 1);

    // disassemble_chunk(&chunk, "test chunk");
    let vm = VirtualMachine::new(chunk);
    let result = vm.run();
}

fn disassemble_chunk(chunk: &Chunk, name: &str) {
    println!("== {0} ==", name);
    let mut index: usize = 0;
    let code = chunk.get_code();
    while index < code.len() - 1 {
        let byte = code[index];

    }
}



