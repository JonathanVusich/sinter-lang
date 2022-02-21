use std::cell::Cell;
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
use crate::class::version::{CURRENT_VERSION, Version};
use crate::errors::vm_error::{VMError, VMErrorKind};
use crate::function::method::Method;
use crate::opcode::OpCode;
use crate::strings::string_pool::StringPool;
use crate::vm::call_frame::CallFrame;
use crate::vm::stack::Stack;

pub const MAX_CALL_FRAMES: usize = 1024;

pub struct VM {
    classes: HashMap<String, &'static Class>,
    thread_stack: Stack,
    call_frames: [CallFrame; MAX_CALL_FRAMES],
    current_frame: usize,
    current_class: &'static Class,
    current_method: &'static Method,
    current_code: &'static [u8],
    string_pool: StringPool,
}

impl VM {

    pub fn new(main_class_name: &str, module_readers: Vec<impl ByteReader>) -> Result<Self, VMError> {
        let mut class_map: HashMap<String, &'static Class> = HashMap::new();
        let mut string_pool = StringPool::with_capacity(8192);

        for mut reader in module_readers.into_iter() {

            let classes = Box::<[CompiledClass]>::read(&mut reader)
                .map_err(move |err| VMError::new(VMErrorKind::MalformedClassFile))?;

            if classes.iter().any(|class| class.version > CURRENT_VERSION) {
                return Err(VMError::new(VMErrorKind::UnsupportedClassVersion));
            }

            for class in classes.into_vec().drain(..) {
                let runtime_class = Box::leak(Box::new(Class::from(class, &mut string_pool)));
                let package_name = string_pool.lookup(runtime_class.package).to_owned();
                let name = string_pool.lookup(runtime_class.name);

                let qualified_name = package_name + name;

                class_map.insert(qualified_name, runtime_class);
            }
        }

        let current_class = *class_map.get(&*main_class_name)
            .ok_or_else(move || VMError::new(VMErrorKind::MissingMainClass))?;

        let current_method = current_class.get_main_method(&string_pool)
            .ok_or_else(move || VMError::new(VMErrorKind::MissingMainMethod))?;

        let current_code = &current_method.code;

        let vm = Self {
            classes: class_map,
            thread_stack: Stack::new(),
            call_frames: [Default::default(); MAX_CALL_FRAMES],
            current_frame: 0,
            current_class,
            current_method,
            current_code,
            string_pool,
        };

        Ok(vm)
    }

    pub fn run(&mut self, args: &[&'static str]) -> usize {
        let call_frame = CallFrame {
            ip: 0,
            address: 0,
            size: 8 // This is the size of the pointer to the args array
        };

        self.call_frames[self.current_frame] = call_frame;

        loop {
            let opcode = self.read_opcode();
            match opcode {
                OpCode::ReturnVoid => {
                    self.current_frame -= 1;
                    if self.current_frame == 0 {
                        return 0;
                    }
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
        0
    }

    #[inline(always)]
    fn read_short(&mut self) -> u16 {
        let call_frame = self.call_frames.get_mut(self.current_frame).unwrap();
        let first_byte = *self.current_code.get(call_frame.ip).unwrap();
        let second_byte = *self.current_code.get(call_frame.ip + 1).unwrap();
        let short: u16 = ((first_byte as u16) << 8) | (second_byte as u16);
        call_frame.ip += 2;
        short
    }

    #[inline(always)]
    fn read_byte(&mut self) -> u8 {
        let mut call_frame = self.call_frames.get_mut(self.current_frame).unwrap();
        let byte = *self.current_code.get(call_frame.ip).unwrap();
        call_frame.ip += 1;
        byte
    }

    #[inline(always)]
    fn read_opcode(&mut self) -> OpCode {
        let instruction = self.read_byte();
        OpCode::from(instruction)
    }
}