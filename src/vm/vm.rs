use crate::function::function::Function;
use crate::vm::call_frame::CallFrame;
use crate::vm::stack::Stack;

pub struct VM {
    thread_stack: Stack,
    stack_frames: Vec<CallFrame>,
    current_frame: usize,
}

impl VM {

    pub fn new() -> Self {
        Self {
            thread_stack: Stack::new(),
            stack_frames: vec![],
            current_frame: 0
        }
    }

    pub fn run(function: Function) {
        let slots = function.slots();
        let call_frame = CallFrame::new();
    }
}