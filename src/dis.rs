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
        OP_I8_CONSTANT => {
            let constant = chunk.get_i8_constant(code[index + 1]);
            println!("index {:04} line {} {:?} {}", index, chunk.get_line_no(index), "OP_I8_CONSTANT", constant);
            index + 2
        },
        OP_I16_CONSTANT => {
            let constant = chunk.get_i16_constant(code[index + 1]);
            println!("index {:04} line {} {:?} {}", index, chunk.get_line_no(index), "OP_I16_CONSTANT", constant);
            index + 2
        },
        OP_I32_CONSTANT => {
            let constant = chunk.get_i32_constant(code[index + 1]);
            println!("index {:04} line {} {:?} {}", index, chunk.get_line_no(index), "OP_I32_CONSTANT", constant);
            index + 2
        },
        OP_I64_CONSTANT => {
            let constant = chunk.get_i64_constant(code[index + 1]);
            println!("index {:04} line {} {:?} {}", index, chunk.get_line_no(index), "OP_I64_CONSTANT", constant);
            index + 2
        },
        OP_I128_CONSTANT => {
            let constant = chunk.get_i128_constant(code[index + 1]);
            println!("index {:04} line {} {:?} {}", index, chunk.get_line_no(index), "OP_I128_CONSTANT", constant);
            index + 2
        },
        OP_U8_CONSTANT => {
            let constant = chunk.get_u8_constant(code[index + 1]);
            println!("index {:04} line {} {:?} {}", index, chunk.get_line_no(index), "OP_U8_CONSTANT", constant);
            index + 2
        },
        OP_U16_CONSTANT => {
            let constant = chunk.get_u16_constant(code[index + 1]);
            println!("index {:04} line {} {:?} {}", index, chunk.get_line_no(index), "OP_U16_CONSTANT", constant);
            index + 2
        },
        OP_U32_CONSTANT => {
            let constant = chunk.get_u32_constant(code[index + 1]);
            println!("index {:04} line {} {:?} {}", index, chunk.get_line_no(index), "OP_U32_CONSTANT", constant);
            index + 2
        },
        OP_U64_CONSTANT => {
            let constant = chunk.get_u64_constant(code[index + 1]);
            println!("index {:04} line {} {:?} {}", index, chunk.get_line_no(index), "OP_U64_CONSTANT", constant);
            index + 2
        },
        OP_U128_CONSTANT => {
            let constant = chunk.get_u128_constant(code[index + 1]);
            println!("index {:04} line {} {:?} {}", index, chunk.get_line_no(index), "OP_U128_CONSTANT", constant);
            index + 2
        },
        OP_F32_CONSTANT => {
            let constant = chunk.get_f32_constant(code[index + 1]);
            println!("index {:04} line {} {:?} {}", index, chunk.get_line_no(index), "OP_F32_CONSTANT", constant);
            index + 2
        },
        OP_F64_CONSTANT => {
            let constant = chunk.get_f64_constant(code[index + 1]);
            println!("index {:04} line {} {:?} {}", index, chunk.get_line_no(index), "OP_F64_CONSTANT", constant);
            index + 2
        },
        _ => {
            println!("{}", "Unrecognized bytecode!");
            index + 1
        }
    }
}