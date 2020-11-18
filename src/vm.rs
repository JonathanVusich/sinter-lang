use crate::chunk::Chunk;
use crate::opcode::*;
use crate::dis::disassemble_instruction;
use crate::stack::Stack;

pub (crate) struct VirtualMachine<'_a> {
    chunk: &'_a Chunk,
    stack: Stack,
    ip: usize,
    debug: bool,
}

impl VirtualMachine<'_> {

    pub (crate) fn new(chunk: &Chunk) -> VirtualMachine {
        return VirtualMachine {
            chunk,
            stack: Stack::new(),
            ip: 0,
            debug: true
        }
    }

    pub (crate) fn run(&mut self) -> InterpretResult {
        let bytecode = self.chunk.get_code();
        loop {
            if self.debug {
                disassemble_instruction(&self.chunk, self.ip);
            }
            let instruction = bytecode[self.ip];
            self.ip += 1;
            match instruction {
                OP_RETURN => {
                    println!("{:?}", self.stack);
                    return InterpretResult::OK;
                }
                OP_I64_CONSTANT => {
                    let val = self.chunk.get_i64_constant(bytecode[self.ip]);
                    self.stack.push_i64(&val);
                    self.ip += 1;
                }
                OP_F64_CONSTANT => {
                    let val = self.chunk.get_f64_constant(bytecode[self.ip]);
                    self.stack.push_f64(&val);
                    self.ip += 1;
                }
                OP_I2F => {
                    let i64 = self.stack.read_i64();
                    let f64 = i64 as f64;
                    self.stack.push_f64(&f64);
                }
                OP_F2I => {
                    let f64 = self.stack.read_f64();
                    let i64 = f64 as i64;
                    self.stack.push_i64(&i64);
                }
                OP_IADD => {
                    let two = self.stack.read_i64();
                    let one = self.stack.read_i64();
                    let result = one + two;
                    self.stack.push_i64(&result);
                }
                OP_ISUB => {
                    let two = self.stack.read_i64();
                    let one = self.stack.read_i64();
                    let result = one + two;
                    self.stack.push_i64(&result);
                }
                OP_IMULT => {
                    let two = self.stack.read_i64();
                    let one = self.stack.read_i64();
                    let result = one * two;
                    self.stack.push_i64(&result);
                }
                OP_IDIV => {
                    let two = self.stack.read_i64();
                    let one = self.stack.read_i64();
                    let result = one / two;
                    self.stack.push_i64(&result);
                }
                OP_FADD => {
                    let two = self.stack.read_f64();
                    let one = self.stack.read_f64();
                    let result = one + two;
                    self.stack.push_f64(&result);
                }
                OP_FSUB => {
                    let two = self.stack.read_f64();
                    let one = self.stack.read_f64();
                    let result = one - two;
                    self.stack.push_f64(&result);
                }
                OP_FMULT => {
                    let two = self.stack.read_f64();
                    let one = self.stack.read_f64();
                    let result = one * two;
                    self.stack.push_f64(&result);
                }
                OP_FDIV => {
                    let two = self.stack.read_f64();
                    let one = self.stack.read_f64();
                    let result = one / two;
                    self.stack.push_f64(&result);
                }
                OP_INEG => {
                    let val = self.stack.read_i64();
                    self.stack.push_i64(&-val);
                }
                OP_FNEG => {
                    let val = self.stack.read_f64();
                    self.stack.push_f64(&-val);
                }
                _ => {
                    return InterpretResult::RuntimeError;
                }
            }
        }
    }

    fn runtime_error(&mut self) {
        let line_no = self.chunk.get_line_no(self.ip - 1);
        eprintln!("[line {}] in script.", line_no);
    }
}

pub (crate) enum InterpretResult {
    OK,
    CompileError,
    RuntimeError
}