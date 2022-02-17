use std::collections::HashMap;
use std::error::Error;
use std::io::ErrorKind;
use std::iter::Map;
use std::mem::size_of;
use std::slice::SliceIndex;
use crate::bytes::serializers::ByteReader;
use crate::bytes::serializable::Serializable;
use crate::class::class::Class;
use crate::class::compiled_class::CompiledClass;
use crate::class::version::Version;
use crate::errors::vm_error::{VMError, VMErrorKind};
use crate::opcode::OpCode;
use crate::strings::string_pool::StringPool;
use crate::vm::call_frame::CallFrame;
use crate::vm::stack::Stack;

pub const CURRENT_VERSION: Version = Version::new(0, 1);

pub struct VM {
    classes: HashMap<String, Class>,
    thread_stack: Stack,
    call_frames: Vec<CallFrame>,
    current_frame: usize,
    string_pool: StringPool,
}

impl VM {

    pub fn new(mut module_readers: Vec<impl ByteReader>) -> Result<Self, VMError> {
        let mut vm = Self {
            classes: HashMap::new(),
            thread_stack: Stack::new(),
            call_frames: vec![],
            current_frame: 0,
            string_pool: StringPool::with_capacity(8192),
        };

        for mut reader in module_readers.into_iter() {

            let classes = Box::<[CompiledClass]>::read(&mut reader)
                .or_else(move |err| Err(VMError::new(VMErrorKind::MalformedClassFile)))?;

            if classes.iter().any(|class| class.version > CURRENT_VERSION) {
                return Err(VMError::new(VMErrorKind::UnsupportedClassVersion));
            }

            for class in classes.into_iter() {
                let runtime_class = Class::from(*class, &mut vm.string_pool);

                let package_name = vm.string_pool.lookup(runtime_class.package).to_owned();
                let name = vm.string_pool.lookup(runtime_class.name);

                let qualified_name = package_name + name;

                vm.classes.insert(qualified_name, runtime_class);
            }
        }

        Ok(vm)
    }

    pub fn run(&mut self, main_class_name: String) -> usize {
        let main_class: &Class = self.classes.get(&*main_class_name).expect("The specified main class is not loaded into the VM!");
        let main_method = main_class.get_main_method().expect("Main class did not provide a main() method!");
        
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

                OpCode::Return8 => {}
                OpCode::Return16 => {}
                OpCode::Return32 => {}
                OpCode::Return64 => {}
                OpCode::Pop8 => {}
                OpCode::Pop16 => {}
                OpCode::Pop32 => {}
                OpCode::Pop64 => {}
                OpCode::GetConstant8 => {}
                OpCode::GetConstant16 => {}
                OpCode::GetConstant32 => {}
                OpCode::GetConstant64 => {}
                OpCode::SetLocal8 => {}
                OpCode::SetLocal16 => {}
                OpCode::SetLocal32 => {}
                OpCode::SetLocal64 => {}
                OpCode::GetLocal8 => {}
                OpCode::GetLocal16 => {}
                OpCode::GetLocal32 => {}
                OpCode::GetLocal64 => {}
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