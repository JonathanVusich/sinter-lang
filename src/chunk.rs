pub (crate) struct Chunk {
    code: Vec<u8>,
    count: i32,

    i64_constants: Vec<i64>,
    f64_constants: Vec<f64>,

    lines: Vec<i32>
}

impl Chunk {
    pub (crate) fn new() -> Chunk {
        return Chunk {
            code: vec![],
            count: 0,

            i64_constants: Vec::with_capacity(0),
            f64_constants: Vec::with_capacity(0),

            lines: vec![]
        }
    }

    pub (crate) fn get_code(&self) -> &Vec<u8>{
        return &self.code;
    }

    pub (crate) fn get_count(self) -> i32 {
        return self.count;
    }

    pub (crate) fn add_opcode(&mut self, opcode: u8, line_no: i32) {
        self.code.push(opcode);
        self.count += 1;
        self.lines.push(line_no);
    }

    pub (crate) fn get_line_no(&self, index: usize) -> i32 {
        return self.lines[index];
    }

    pub (crate) fn add_i64_constant(&mut self, constant: i64) -> u8 {
        self.i64_constants.push(constant);
        return (self.i64_constants.len() - 1) as u8;
    }

    pub (crate) fn get_i64_constant(&self, index: u8) -> i64 {
        return self.i64_constants[(index as usize)]
    }

    pub (crate) fn add_f64_constant(&mut self, constant: f64) -> u8 {
        self.f64_constants.push(constant);
        return (self.f64_constants.len() - 1) as u8;
    }

    pub (crate) fn get_f64_constant(&self, index: u8) -> f64 {
        return self.f64_constants[(index as usize)]
    }
}

