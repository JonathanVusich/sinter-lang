use crate::chunk::Chunk;
use crate::opcode::*;

pub (crate) fn disassemble_instruction(chunk: &Chunk, index: usize) -> usize {
    let code = chunk.get_code();
    let byte = code[index];
    return match byte {
        OP_RETURN => {
            println!("index {:04} line {} {:?}", index, chunk.get_line_no(index), "OP_RETURN");
            index + 1
        },
        OP_I64_CONSTANT => {
            let constant = chunk.get_i64_constant(code[index + 1]);
            println!("index {:04} line {} {:?} {}", index, chunk.get_line_no(index), "OP_I64_CONSTANT", constant);
            index + 2
        },
        OP_F64_CONSTANT => {
            let constant = chunk.get_f64_constant(code[index + 1]);
            println!("index {:04} line {} {:?} {}", index, chunk.get_line_no(index), "OP_F64_CONSTANT", constant);
            index + 2
        },
        OP_I2F => {
            println!("index {:04} line {} {:?}", index, chunk.get_line_no(index), "OP_I2F");
            index + 1
        }
        OP_F2I => {
            println!("index {:04} line {} {:?}", index, chunk.get_line_no(index), "OP_F2I");
            index + 1
        }
        OP_IADD => {
            println!("index {:04} line {} {:?}", index, chunk.get_line_no(index), "OP_IADD");
            index + 1
        }
        OP_ISUB => {
            println!("index {:04} line {} {:?}", index, chunk.get_line_no(index), "OP_ISUB");
            index + 1
        }
        OP_IMULT => {
            println!("index {:04} line {} {:?}", index, chunk.get_line_no(index), "OP_IMULT");
            index + 1
        }
        OP_IDIV => {
            println!("index {:04} line {} {:?}", index, chunk.get_line_no(index), "OP_IDIV");
            index + 1
        }
        OP_FADD => {
            println!("index {:04} line {} {:?}", index, chunk.get_line_no(index), "OP_FADD");
            index + 1
        }
        OP_FSUB => {
            println!("index {:04} line {} {:?}", index, chunk.get_line_no(index), "OP_FSUB");
            index + 1
        }
        OP_FMULT => {
            println!("index {:04} line {} {:?}", index, chunk.get_line_no(index), "OP_FMULT");
            index + 1
        }
        OP_FDIV => {
            println!("index {:04} line {} {:?}", index, chunk.get_line_no(index), "OP_FDIV");
            index + 1
        }
        _ => {
            println!("{}", "Unrecognized bytecode!");
            index + 1
        }
    }
}