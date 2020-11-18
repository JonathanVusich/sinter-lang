mod chunk;
mod opcode;
mod vm;
mod macros;
mod dis;
mod stack;
mod scanner;
mod compiler;
mod token;
mod utils;

use std::io;

use crate::vm::VirtualMachine;
use crate::compiler::Compiler;

fn main() {
    loop {
        let mut input = String::new();
        print!("> ");
        match io::stdin().read_line(&mut input) {
            Ok(_n) => {}
            Err(error) => eprintln!("{}", error)
        }
        println!();
        let mut compiler = Compiler::new(&input);
        let chunk = compiler.compile();
        match chunk {
            Some(chunk) => {
                let mut vm = VirtualMachine::new(chunk);
                vm.run();
            }
            None => {}
        }

    }
}



