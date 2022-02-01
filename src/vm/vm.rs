use std::mem::size_of;
use std::slice::SliceIndex;
use crate::function::function::Function;
use crate::opcode::OpCode;
use crate::util::constants::WORD;
use crate::vm::call_frame::CallFrame;
use crate::vm::stack::Stack;

pub struct VM {
    thread_stack: Stack,
    call_frames: Vec<CallFrame>,

    current_frame: usize
}

impl VM {

    pub fn new() -> Self {
        Self {
            thread_stack: Stack::new(),
            call_frames: vec![],
            current_frame: 0,
        }
    }

    pub fn run(&mut self, code: Vec<u8>, args: Box<[&str]>) -> usize {

        let call_frame = CallFrame {
            code,
            ip: 0,
            address: 0,
            size: 8 // This is the size of the pointer to the args array
        };

        self.call_frames.push(call_frame);

        loop {
            let opcode = self.read_opcode();
            match opcode {
                OpCode::Return => {
                    let word: &[u8; WORD] = self.thread_stack.pop_word();
                    self.current_frame -= 1;
                    if self.current_frame == 0 {
                        return 0;
                    }
                    self.call_frames.pop();
                    break;
                }
                _ => {}
            }
        }
        return 0;
    }



    fn read_short(&mut self) -> u16 {
        let call_frame = self.call_frames.get_mut(self.current_frame).unwrap();
        let first_byte = *call_frame.function.code().get(call_frame.ip).unwrap();
        let second_byte = *call_frame.function.code().get(call_frame.ip + 1).unwrap();
        let short: u16 = ((first_byte as u16) << 8) | (second_byte as u16);
        call_frame.ip += 2;
        short
    }

    fn read_byte(&mut self) -> u8 {
        let mut call_frame = self.call_frames.get_mut(self.current_frame).unwrap();
        let byte = *call_frame.function.code().get(call_frame.ip).unwrap();
        call_frame.ip += 1;
        byte
    }

    fn read_opcode(&mut self) -> OpCode {
        let instruction = self.read_byte();
        OpCode::from(instruction)
    }
}