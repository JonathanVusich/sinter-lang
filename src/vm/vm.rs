use std::cell::Cell;
use std::collections::HashMap;
use std::env::var;
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
use crate::pool::string_pool::StringPool;
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
                .map_err(move |err| VMError::new(VMErrorKind::MalformedModuleFile))?;

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

        // Need to perform linking and constant folding here.

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
                    if self.do_return::<0>() {
                        return 0;
                    }
                }

                OpCode::Return8 => {
                    if self.do_return::<8>() {
                        return 0;
                    }
                }
                OpCode::Return16 => {
                    if self.do_return::<16>() {
                        return 0;
                    }
                }
                OpCode::Return32 => {
                    if self.do_return::<32>() {
                        return 0;
                    }
                }
                OpCode::Return64 => {
                    if self.do_return::<64>() {
                        return 0;
                    }
                }
                OpCode::Pop8 => {
                    self.pop::<8>();
                }
                OpCode::Pop16 => {
                    self.pop::<16>();
                }
                OpCode::Pop32 => {
                    self.pop::<32>();
                }
                OpCode::Pop64 => {
                    self.pop::<64>();
                }
                OpCode::GetConstant8 => {
                    self.get_constant::<1>();
                }
                OpCode::GetConstant16 => {
                    self.get_constant::<2>();
                }
                OpCode::GetConstant32 => {
                    self.get_constant::<4>();
                }
                OpCode::GetConstant64 => {
                    self.get_constant::<8>()
                }
                OpCode::SetLocal8 => {

                }
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
    }

    #[inline(always)]
    fn do_return<const LEN: usize>(&mut self) -> bool {
        if LEN == 0 {
            self.current_frame -= 1;
            return self.current_frame == 0;
        } else {
            let result = self.thread_stack.pop::<LEN>();
            self.current_frame -= 1;
            self.thread_stack.push(result);
        }
        return self.current_frame == 0;
    } 

    #[inline(always)]
    fn pop<const LEN: usize>(&mut self) {
        self.thread_stack.pop::<LEN>();
    }

    fn get_constant<const LEN: usize>(&mut self) {
        let bytes = self.read_bytes::<LEN>();
        self.thread_stack.push(bytes);
    }

    fn set_local<const SIZE: usize>(&mut self) {
        let address = self.get_call_frame().address;
        let offset = u32::from_be_bytes(self.read_bytes::<4>()) as usize;
        let bytes = self.thread_stack.pop::<SIZE>();
        self.thread_stack.write(address + offset, bytes);
    }

    fn read_byte(&mut self) -> u8 {
        let call_frame = self.get_call_frame();
        let addr = call_frame.ip;
        call_frame.ip += 1;

        let byte = self.current_code[addr];
        byte
    }

    fn read_bytes<const SIZE: usize>(&mut self) -> [u8; SIZE] {
        let call_frame = self.get_call_frame();
        let start = call_frame.ip;
        call_frame.ip += SIZE;
        let end = call_frame.ip;
        let bytes: [u8; SIZE] = self.current_code[start..end].try_into().unwrap();
        bytes
    }

    #[inline(always)]
    fn get_call_frame(&mut self) -> &mut CallFrame {
        self.call_frames.get_mut(self.current_frame).unwrap()
    }

    #[inline(always)]
    fn read_opcode(&mut self) -> OpCode {
        let instruction = self.read_bytes::<1>()[0];
        OpCode::from(instruction)
    }
}