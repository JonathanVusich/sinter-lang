use crate::opcode::OpCode;
use std::slice::Iter;


pub (crate) struct Chunk {
    code: Vec<OpCode>,
    int_constants: Vec<i32>,
    long_constants: Vec<i64>,
    float_constants: Vec<f32>,
    double_constants: Vec<f64>,
    lines: Vec<i32>
}

impl Chunk {
    pub (crate) fn new() -> Chunk {
        return Chunk {
            code: vec![],
            int_constants: vec![],
            long_constants: vec![],
            float_constants: vec![],
            double_constants: vec![],
            lines: vec![]
        }
    }

    pub (crate) fn get_opcodes(&self) -> Iter<'_, OpCode> {
        return self.code.iter();
    }

    pub (crate) fn add_opcode(&mut self, opcode: OpCode, line_no: i32) {
        self.code.push(opcode);
        self.lines.push(line_no);
    }

    pub (crate) fn get_line_no(&self, index: usize) -> i32 {
        return self.lines[index];
    }

    pub (crate) fn add_int_constant(&mut self, constant: i32) -> u16 {
        self.int_constants.push(constant);
        return (self.int_constants.len() - 1) as u16;
    }

    pub (crate) fn get_int_constant(&self, index: u16) -> i32 {
        return self.int_constants[(index as usize)]
    }

    pub (crate) fn add_long_constant(&mut self, constant: i64) -> u16 {
        self.long_constants.push(constant);
        return (self.long_constants.len() - 1) as u16;
    }

    pub (crate) fn get_long_constant(&self, index: u16) -> i64 {
        return self.long_constants[(index as usize)];
    }

    pub (crate) fn add_float_constant(&mut self, constant: f32) -> u16 {
        self.float_constants.push(constant);
        return (self.float_constants.len() - 1) as u16;
    }

    pub (crate) fn get_float_constant(&self, index: u16) -> f32 {
        return self.float_constants[(index as usize)];
    }

    pub (crate) fn add_double_constant(&mut self, constant: f64) -> u16 {
        self.double_constants.push(constant);
        return (self.double_constants.len() - 1) as u16;
    }

    pub (crate) fn get_double_constant(&self, index: u16) -> f64 {
        return self.double_constants[(index as usize)];
    }
}

