pub (crate) struct Chunk {
    code: Vec<u8>,

    u8_constants: Vec<u8>,
    u16_constants: Vec<u16>,
    u32_constants: Vec<u32>,
    u64_constants: Vec<u64>,
    u128_constants: Vec<u128>,

    i8_constants: Vec<i8>,
    i16_constants: Vec<i16>,
    i32_constants: Vec<i32>,
    i64_constants: Vec<i64>,
    i128_constants: Vec<i128>,

    f32_constants: Vec<f32>,
    f64_constants: Vec<f64>,

    lines: Vec<i32>
}

impl Chunk {
    pub (crate) fn new() -> Chunk {
        return Chunk {
            code: vec![],
            u8_constants: Vec::with_capacity(0),
            u16_constants: Vec::with_capacity(0),
            u32_constants: Vec::with_capacity(0),
            u64_constants: Vec::with_capacity(0),
            u128_constants: Vec::with_capacity(0),

            i8_constants: Vec::with_capacity(0),
            i16_constants: Vec::with_capacity(0),
            i32_constants: Vec::with_capacity(0),
            i64_constants: Vec::with_capacity(0),
            i128_constants: Vec::with_capacity(0),

            f32_constants: Vec::with_capacity(0),
            f64_constants: Vec::with_capacity(0),

            lines: vec![]
        }
    }

    pub (crate) fn get_code(&self) -> &Vec<u8>{
        return &self.code;
    }

    pub (crate) fn add_opcode(&mut self, opcode: u8, line_no: i32) {
        self.code.push(opcode);
        self.lines.push(line_no);
    }

    pub (crate) fn get_line_no(&self, index: usize) -> i32 {
        return self.lines[index];
    }

    pub (crate) fn add_u8_constant(&mut self, constant: u8) -> u8 {
        self.u8_constants.push(constant);
        return (self.u8_constants.len() - 1) as u8;
    }

    pub (crate) fn get_u8_constant(&self, index: u8) -> u8 {
        return self.u8_constants[(index as usize)]
    }

    pub (crate) fn add_u16_constant(&mut self, constant: u16) -> u8 {
        self.u16_constants.push(constant);
        return (self.u16_constants.len() - 1) as u8;
    }

    pub (crate) fn get_u16_constant(&self, index: u8) -> u16 {
        return self.u16_constants[(index as usize)]
    }

    pub (crate) fn add_u32_constant(&mut self, constant: u32) -> u8 {
        self.u32_constants.push(constant);
        return (self.u32_constants.len() - 1) as u8;
    }

    pub (crate) fn get_u32_constant(&self, index: u8) -> u32 {
        return self.u32_constants[(index as usize)]
    }

    pub (crate) fn add_u64_constant(&mut self, constant: u64) -> u8 {
        self.u64_constants.push(constant);
        return (self.u64_constants.len() - 1) as u8;
    }

    pub (crate) fn get_u64_constant(&self, index: u8) -> u64 {
        return self.u64_constants[(index as usize)]
    }

    pub (crate) fn add_u128_constant(&mut self, constant: u128) -> u8 {
        self.u128_constants.push(constant);
        return (self.u128_constants.len() - 1) as u8;
    }

    pub (crate) fn get_u128_constant(&self, index: u8) -> u128 {
        return self.u128_constants[(index as usize)]
    }

    pub (crate) fn add_i8_constant(&mut self, constant: i8) -> u8 {
        self.i8_constants.push(constant);
        return (self.i8_constants.len() - 1) as u8;
    }

    pub (crate) fn get_i8_constant(&self, index: u8) -> i8 {
        return self.i8_constants[(index as usize)]
    }

    pub (crate) fn add_i16_constant(&mut self, constant: i16) -> u8 {
        self.i16_constants.push(constant);
        return (self.i16_constants.len() - 1) as u8;
    }

    pub (crate) fn get_i16_constant(&self, index: u8) -> i16 {
        return self.i16_constants[(index as usize)]
    }

    pub (crate) fn add_i32_constant(&mut self, constant: i32) -> u8 {
        self.i32_constants.push(constant);
        return (self.i32_constants.len() - 1) as u8;
    }

    pub (crate) fn get_i32_constant(&self, index: u8) -> i32 {
        return self.i32_constants[(index as usize)]
    }

    pub (crate) fn add_i64_constant(&mut self, constant: i64) -> u8 {
        self.i64_constants.push(constant);
        return (self.i64_constants.len() - 1) as u8;
    }

    pub (crate) fn get_i64_constant(&self, index: u8) -> i64 {
        return self.i64_constants[(index as usize)]
    }

    pub (crate) fn add_i128_constant(&mut self, constant: i128) -> u8 {
        self.i128_constants.push(constant);
        return (self.i128_constants.len() - 1) as u8;
    }

    pub (crate) fn get_i128_constant(&self, index: u8) -> i128 {
        return self.i128_constants[(index as usize)]
    }

    pub (crate) fn add_f32_constant(&mut self, constant: f32) -> u8 {
        self.f32_constants.push(constant);
        return (self.f32_constants.len() - 1) as u8;
    }

    pub (crate) fn get_f32_constant(&self, index: u8) -> f32 {
        return self.f32_constants[(index as usize)]
    }

    pub (crate) fn add_f64_constant(&mut self, constant: f64) -> u8 {
        self.f64_constants.push(constant);
        return (self.f64_constants.len() - 1) as u8;
    }

    pub (crate) fn get_f64_constant(&self, index: u8) -> f64 {
        return self.f64_constants[(index as usize)]
    }
}

