mod chunk;
mod opcode;
mod vm;

use crate::chunk::Chunk;
use crate::opcode::OpCode::{OpReturn, OpIntConstant, OpLongConstant, OpFloatConstant, OpDoubleConstant};
use crate::vm::VirtualMachine;

fn main() {
    let vm = VirtualMachine::new();

    let mut chunk = Chunk::new();
    chunk.add_opcode(OpReturn, 1);

    // Test int
    let index = chunk.add_int_constant(12);
    chunk.add_opcode(OpIntConstant(index), 1);

    // Test long
    let index = chunk.add_long_constant(1254124);
    chunk.add_opcode(OpLongConstant(index), 1);

    // Test float
    let index = chunk.add_float_constant(1.2345);
    chunk.add_opcode(OpFloatConstant(index), 2);

    // Test double
    let index = chunk.add_double_constant(12323412.123123452134);
    chunk.add_opcode(OpDoubleConstant(index), 2);

    disassemble_chunk(&chunk, "test chunk");
    vm.interpret(&chunk);
}

fn disassemble_chunk(chunk: &Chunk, name: &str) {
    println!("== {0} ==", name);
    for (index, opcode) in chunk.get_opcodes().enumerate() {
        match opcode {
            OpReturn => println!("index {:04} line {} {:?}", index, chunk.get_line_no(index), opcode),
            OpIntConstant(i) => println!("index {:04} line {} {:?} {}", index, chunk.get_line_no(index), opcode, chunk.get_int_constant(*i)),
            OpLongConstant(i) => println!("index {:04} line {} {:?} {}", index, chunk.get_line_no(index), opcode, chunk.get_long_constant(*i)),
            OpFloatConstant(i) => println!("index {:04} line {} {:?} {}", index, chunk.get_line_no(index), opcode, chunk.get_float_constant(*i)),
            OpDoubleConstant(i) => println!("index {:04} line {} {:?} {}", index, chunk.get_line_no(index), opcode, chunk.get_double_constant(*i))
        }
    }
}



