use crate::function::function::Function;
use crate::thread::stack_frame::StackFrame;
use crate::thread::thread_stack::ThreadStack;

pub struct Thread {
    thread_stack: ThreadStack,
    stack_frames: Vec<StackFrame>,
    current_frame: usize,
}

impl Thread {

    pub fn new() -> Self {
        Self {
            thread_stack: ThreadStack::new(),
            stack_frames: vec![],
            current_frame: 0
        }
    }


}