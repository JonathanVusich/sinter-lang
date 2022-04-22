use std::cell::Cell;
use std::collections::HashMap;
use std::env::var;
use std::error::Error;
use std::io::ErrorKind;
use std::iter::Map;
use std::mem::size_of;
use std::slice::SliceIndex;
use region::page::size;
use crate::bytes::serializers::ByteReader;
use crate::bytes::serializable::Serializable;
use crate::class::class::Class;
use crate::class::compiled_class::CompiledClass;
use crate::class::version::{CURRENT_VERSION, Version};
use crate::errors::vm_error::{VMError, VMErrorKind};
use crate::function::method::Method;
use crate::opcode::OpCode;
use crate::pointers::heap_pointer::HeapPointer;
use crate::pointers::pointer::Pointer;
use crate::pool::internal_string::InternalString;
use crate::pool::string_pool::StringPool;
use crate::util::constants::WORD;
use crate::vm::call_frame::CallFrame;
use crate::vm::stack::Stack;

pub const MAX_CALL_FRAMES: usize = 1024;

pub struct VM {
    classes: HashMap<InternalString, &'static Class>,
    args: Vec<&'static str>,
    thread_stack: Stack,
    call_frames: Vec<CallFrame>,
    current_frame: usize,
    string_pool: StringPool,
    current_code: [u8; 0]
}

impl VM {

    pub fn new() -> Self {
        Self {
            classes: HashMap::new(),
            args: Vec::new(),
            thread_stack: Stack::new(),
            call_frames: Vec::new(),
            current_frame: 0,
            string_pool: StringPool::with_capacity(8192),
            current_code: [],
        }
    }

    pub fn load_classes(mut self, module_readers: Vec<impl ByteReader>) -> Result<Self, VMError> {
        for mut reader in module_readers.into_iter() {

            let classes = Box::<[CompiledClass]>::read(&mut reader)
                .map_err(move |err| VMError::new(VMErrorKind::MalformedModuleFile))?;

            if classes.iter().any(|class| class.version > CURRENT_VERSION) {
                return Err(VMError::new(VMErrorKind::UnsupportedClassVersion));
            }

            for class in classes.into_vec().drain(..) {
                let runtime_class = Box::leak(Box::new(Class::from(class, &mut self.string_pool)));
                let package_name = self.string_pool.lookup(runtime_class.package).to_owned();
                let name = self.string_pool.lookup(runtime_class.name);

                let qualified_name = self.string_pool.intern(package_name + name);



                self.classes.insert(qualified_name, runtime_class);
            }
        }
        Ok(self)
    }

    pub fn set_args(mut self, args: &[&'static str]) -> Self {
        self.args.clear();
        self.args.extend(args);
        self
    }

    pub fn run(mut self, main_class: &str, main_method: &str) -> Result<(), VMError> {
        let main_class_name = self.string_pool.intern(main_class);
        let main_method_name = self.string_pool.intern(main_method);

        let main_class = self.classes.get(&main_class_name)
            .ok_or(VMError::new(VMErrorKind::MissingMainClass))?;
        let main_method = main_class.methods.iter()
            .find(|method| method.name == main_method_name)
            .ok_or(VMError::new(VMErrorKind::MissingMainMethod))?;

        let call_frame = CallFrame::new(main_method, 0);

        self.call_frames.push(call_frame);

        loop {
            let opcode = self.read_opcode();
            match opcode {
                OpCode::ReturnVoid => {
                    if self.return_op::<0>() {
                        return Ok(());
                    }
                }

                OpCode::Return8 => {
                    if self.return_op::<1>() {
                        return Ok(());
                    }
                }
                OpCode::Return16 => {
                    if self.return_op::<2>() {
                        return Ok(());
                    }
                }
                OpCode::Return32 => {
                    if self.return_op::<4>() {
                        return Ok(());
                    }
                }
                OpCode::Return64 => {
                    if self.return_op::<8>() {
                        return Ok(());
                    }
                }
                OpCode::Pop8 => self.pop::<1>(),
                OpCode::Pop16 => self.pop::<2>(),
                OpCode::Pop32 => self.pop::<4>(),
                OpCode::Pop64 => self.pop::<8>(),
                OpCode::GetConstant8 => self.get_constant::<1>(),
                OpCode::GetConstant16 => self.get_constant::<2>(),
                OpCode::GetConstant32 => self.get_constant::<4>(),
                OpCode::GetConstant64 => self.get_constant::<8>(),
                OpCode::SetLocal8 => self.set_local::<1>(),
                OpCode::SetLocal16 => self.set_local::<2>(),
                OpCode::SetLocal32 => self.set_local::<4>(),
                OpCode::SetLocal64 => self.set_local::<8>(),
                OpCode::GetLocal8 => self.get_local::<1>(),
                OpCode::GetLocal16 => self.get_local::<2>(),
                OpCode::GetLocal32 => self.get_local::<4>(),
                OpCode::GetLocal64 => self.get_local::<8>(),
                OpCode::Jump => self.jump(),
                OpCode::JumpBack => self.jump_back(),
                OpCode::Call => self.call()
            }
        }
    }

    #[inline(always)]
    fn return_op<const LEN: usize>(&mut self) -> bool {
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
        let address = self.get_call_frame().address();
        let offset = self.read_offset() as usize;
        let bytes = self.thread_stack.pop::<SIZE>();
        self.thread_stack.write(address + offset, bytes);
    }

    fn get_local<const SIZE: usize>(&mut self) {
        let address = self.get_call_frame().address();
        let offset = self.read_offset() as usize;
        let bytes = self.thread_stack.read::<SIZE>(address + offset);
        self.thread_stack.push(bytes);
    }

    fn jump(&mut self) {
        let offset = self.read_offset() as usize;
        self.get_call_frame().increment_ip(offset);
    }

    fn jump_back(&mut self) {
        let offset = self.read_offset() as usize;
        self.get_call_frame().decrement_ip(offset);
    }

    fn call(&mut self) {
        let method_ptr: Pointer<&'static Method> = self.thread_stack.pop::<8>().into();
        self.current_frame += 1;
        let address = self.thread_stack.get_index() - (method_ptr.descriptor.parameters.len() * WORD);

        let call_frame = CallFrame::new(&method_ptr, address);
        self.call_frames.push(call_frame);
    }


    fn read_byte(&mut self) -> u8 {
        let call_frame = self.get_call_frame();
        let addr = call_frame.ip();
        call_frame.increment_ip(1);

        let byte = self.current_code[addr];
        byte
    }

    fn read_bytes<const SIZE: usize>(&mut self) -> [u8; SIZE] {
        let call_frame = self.get_call_frame();
        let start = call_frame.ip();
        call_frame.increment_ip(SIZE);
        let end = call_frame.ip();
        let bytes: [u8; SIZE] = self.current_code[start..end].try_into().unwrap();
        bytes
    }

    fn read_offset(&mut self) -> u32 {
        u32::from_be_bytes(self.read_bytes::<4>())
    }

    #[inline(always)]
    fn get_call_frame(&mut self) -> &mut CallFrame {
        self.call_frames.get_mut(self.current_frame).unwrap()
    }

    #[inline(always)]
    fn read_opcode(&mut self) -> OpCode {
        let instruction = self.read_byte();
        OpCode::from(instruction)
    }
}