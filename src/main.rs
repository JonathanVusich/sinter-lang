mod chunk;
mod opcode;
mod vm;
mod macros;

use crate::chunk::Chunk;
use crate::vm::VirtualMachine;
use crate::opcode::{OP_RETURN,
                    OP_U8_CONSTANT,
                    OP_U16_CONSTANT,
                    OP_U32_CONSTANT,
                    OP_U64_CONSTANT,
                    OP_U128_CONSTANT,
                    OP_I8_CONSTANT,
                    OP_I16_CONSTANT,
                    OP_I32_CONSTANT,
                    OP_I64_CONSTANT,
                    OP_I128_CONSTANT,
                    OP_F32_CONSTANT,
                    OP_F64_CONSTANT,};

fn main() {
    let vm = VirtualMachine::new();

    let mut chunk = Chunk::new();
    chunk.add_opcode(OP_RETURN, 1);

    // Test int
    chunk.add_opcode(OP_U8_CONSTANT, 1);
    let index = chunk.add_u8_constant(12);
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

    disassemble_chunk(&chunk, "test chunk");
    // vm.interpret(&chunk);
}

fn disassemble_chunk(chunk: &Chunk, name: &str) {
    println!("== {0} ==", name);
    let mut index: usize = 0;
    let code = chunk.get_code();
    while index < code.len() - 1 {
        let byte = code[index];
        match byte {
            OP_RETURN => {
                println!("index {:04} line {} {:?}", index, chunk.get_line_no(index), "OP_RETURN");
                index += 1;
            },
            OP_I8_CONSTANT => {
                let constant = chunk.get_i8_constant(code[index + 1]);
                println!("index {:04} line {} {:?} {}", index, chunk.get_line_no(index), "OP_I8_CONSTANT", constant);
                index += 2;
            },
            OP_I16_CONSTANT => {
                let constant = chunk.get_i16_constant(code[index + 1]);
                println!("index {:04} line {} {:?} {}", index, chunk.get_line_no(index), "OP_I16_CONSTANT", constant);
                index += 2;
            },
            OP_I32_CONSTANT => {
                let constant = chunk.get_i32_constant(code[index + 1]);
                println!("index {:04} line {} {:?} {}", index, chunk.get_line_no(index), "OP_I32_CONSTANT", constant);
                index += 2;
            },
            OP_I64_CONSTANT => {
                let constant = chunk.get_i64_constant(code[index + 1]);
                println!("index {:04} line {} {:?} {}", index, chunk.get_line_no(index), "OP_I64_CONSTANT", constant);
                index += 2;
            },
            OP_I128_CONSTANT => {
                let constant = chunk.get_i128_constant(code[index + 1]);
                println!("index {:04} line {} {:?} {}", index, chunk.get_line_no(index), "OP_I128_CONSTANT", constant);
                index += 2;
            },
            OP_U8_CONSTANT => {
                let constant = chunk.get_u8_constant(code[index + 1]);
                println!("index {:04} line {} {:?} {}", index, chunk.get_line_no(index), "OP_U8_CONSTANT", constant);
                index += 2;
            },
            OP_U16_CONSTANT => {
                let constant = chunk.get_u16_constant(code[index + 1]);
                println!("index {:04} line {} {:?} {}", index, chunk.get_line_no(index), "OP_U16_CONSTANT", constant);
                index += 2;
            },
            OP_U32_CONSTANT => {
                let constant = chunk.get_u32_constant(code[index + 1]);
                println!("index {:04} line {} {:?} {}", index, chunk.get_line_no(index), "OP_U32_CONSTANT", constant);
                index += 2;
            },
            OP_U64_CONSTANT => {
                let constant = chunk.get_u64_constant(code[index + 1]);
                println!("index {:04} line {} {:?} {}", index, chunk.get_line_no(index), "OP_U64_CONSTANT", constant);
                index += 2;
            },
            OP_U128_CONSTANT => {
                let constant = chunk.get_u128_constant(code[index + 1]);
                println!("index {:04} line {} {:?} {}", index, chunk.get_line_no(index), "OP_U128_CONSTANT", constant);
                index += 2;
            },
            OP_F32_CONSTANT => {
                let constant = chunk.get_f32_constant(code[index + 1]);
                println!("index {:04} line {} {:?} {}", index, chunk.get_line_no(index), "OP_F32_CONSTANT", constant);
                index += 2;
            },
            OP_F64_CONSTANT => {
                let constant = chunk.get_f64_constant(code[index + 1]);
                println!("index {:04} line {} {:?} {}", index, chunk.get_line_no(index), "OP_F64_CONSTANT", constant);
                index += 2;
            },
            _ => {
                println!("{}", "Unrecognized bytecode!");
                index += 1;
            }
        }
    }
}



