use std::slice::SliceIndex;
use crate::function::function::Function;
use crate::opcode::OpCode;
use crate::vm::call_frame::CallFrame;
use crate::vm::stack::Stack;

pub struct VM<'a> {
    thread_stack: Stack,
    call_frames: Vec<CallFrame<'a>>,
    current_frame: usize,
    main_function: Function
}

impl<'a> VM<'a> {

    pub fn new() -> Self {
        Self {
            thread_stack: Stack::new(),
            call_frames: vec![],
            current_frame: 0,
            main_function: Function::new("Default")
        }
    }

    pub fn run(mut self, function: Function, args: Box<[&str]>) -> usize {
        debug_assert_eq!(function.call_frame_size(), 8);
        debug_assert_eq!(function.parameters().len(), 1);

        self.main_function = function;

        let call_frame = CallFrame {
            function: self.main_function,
            instructions: self.main_function.code(),
            ip: 0,
            address: 0,
            size: 8
        };

        self.call_frames.push(call_frame);

        loop {
            let opcode = self.read_opcode();
            match opcode {
                OpCode::Return => {
                    let size = self.read_short();
                    let value = self.thread_stack.pop(size);
                    self.current_frame -= 1;
                    if self.current_frame == 0 {
                        return 0;
                    }
                    let current_frame = self.call_frames.get_mut()
                }

                OpCode::GetConstant => {}
                OpCode::Pop => {}
                OpCode::Push => {}
                OpCode::SetLocal => {}
                OpCode::GetLocal => {}
                OpCode::GetGlobal => {}
                OpCode::SetGlobal => {}
                OpCode::Jump => {}
                OpCode::Loop => {}
                OpCode::Call => {}
            }
        }
        return 0;
    }



    fn read_short(&mut self) -> u16 {
        let call_frame = self.call_frames.get_mut(self.current_frame).unwrap();
        let first_byte = *call_frame.instructions.get(call_frame.ip).unwrap();
        let second_byte = *call_frame.instructions.get(call_frame.ip + 1).unwrap();
        let short: u16 = ((first_byte as u16) << 8) | (second_byte as u16);
        call_frame.ip += 2;
        short
    }

    fn read_byte(&mut self) -> u8 {
        let mut call_frame = self.call_frames.get_mut(self.current_frame).unwrap();
        let byte = *call_frame.instructions.get(call_frame.ip).unwrap();
        call_frame.ip += 1;
        byte
    }

    fn read_opcode(&mut self) -> OpCode {
        let instruction = self.read_byte();
        OpCode::from(instruction)
    }
}