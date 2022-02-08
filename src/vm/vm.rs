use std::mem::size_of;
use std::slice::SliceIndex;
use crate::class::compiled_class::CompiledClass;
use crate::function::function::Function;
use crate::opcode::OpCode;
use crate::vm::call_frame::CallFrame;
use crate::vm::stack::Stack;

pub struct VM {

    classes: Vec<CompiledClass>,
    args: Box<[&'static str]>,
    thread_stack: Stack,
    call_frames: Vec<CallFrame>,
    current_frame: usize

}

impl VM {

    pub fn new(classes: Vec<CompiledClass>, args: Box<[&'static str]>) -> Self {
        Self {
            classes,
            args,
            thread_stack: Stack::new(),
            call_frames: vec![],
            current_frame: 0,
        }
    }

    pub fn run(&mut self) -> usize {
        let call_frame = CallFrame {
            ip: 0,
            address: 0,
            size: 8 // This is the size of the pointer to the args array
        };

        self.call_frames.push(call_frame);

        loop {
            let opcode = self.read_opcode();
            match opcode {
                OpCode::ReturnVoid => {
                    self.current_frame -= 1;
                    if self.current_frame == 0 {
                        return 0;
                    }
                    self.call_frames.pop();
                    break;
                }
                OpCode::Return32 => {
                    let val = self.thread_stack.pop_32();
                    self.current_frame -= 1;
                    if self.current_frame == 0 {
                        return 0;
                    }
                    self.call_frames.pop();

                    self.thread_stack.push_32(val);
                }
                OpCode::Return64 => {
                    let val = self.thread_stack.pop_64();
                    self.current_frame -= 1;
                    if self.current_frame == 0 {
                        return 0;
                    }
                    self.call_frames.pop();

                    self.thread_stack.push_64(val);
                }
                OpCode::ReturnWord => {
                    let val = self.thread_stack.pop_word();
                    self.current_frame -= 1;
                    if self.current_frame == 0 {
                        return 0;
                    }
                    self.call_frames.pop();

                    self.thread_stack.push_word(val);
                }

                OpCode::Pop32 => {
                    self.thread_stack.pop_32();
                }
                OpCode::Pop64 => {
                    self.thread_stack.pop_64();
                }
                OpCode::PopWord => {
                    self.thread_stack.pop_word();
                }

                OpCode::GetConstant32 => {

                }
                OpCode::GetConstant64 => {}
                OpCode::GetConstantWord => {}
                OpCode::SetLocal32 => {}
                OpCode::SetLocal64 => {}
                OpCode::SetConstantWord => {}
                OpCode::GetLocal32 => {}
                OpCode::GetLocal64 => {}
                OpCode::GetLocalWord => {}
                OpCode::Jump => {}
                OpCode::JumpBack => {}
                OpCode::Call => {}
            }
        }
        return 0;
    }



    fn read_short(&mut self) -> u16 {
        let call_frame = self.call_frames.get_mut(self.current_frame).unwrap();
        let first_byte = *self.code.get(call_frame.ip).unwrap();
        let second_byte = *self.code.get(call_frame.ip + 1).unwrap();
        let short: u16 = ((first_byte as u16) << 8) | (second_byte as u16);
        call_frame.ip += 2;
        short
    }

    fn read_byte(&mut self) -> u8 {
        let mut call_frame = self.call_frames.get_mut(self.current_frame).unwrap();
        let byte = *self.code.get(call_frame.ip).unwrap();
        call_frame.ip += 1;
        byte
    }

    fn read_opcode(&mut self) -> OpCode {
        let instruction = self.read_byte();
        OpCode::from(instruction)
    }
}