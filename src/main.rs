mod chunk;
mod opcode;
mod vm;
mod macros;
mod dis;
mod stack;
mod scanner;
mod compiler;
mod token;
mod number;

use std::io;

use crate::vm::VirtualMachine;
use crate::compiler::compile;

fn main() {
    loop {
        let mut input = String::new();
        print!("> ");
        match io::stdin().read_line(&mut input) {
            Ok(n) => {}
            Err(error) => eprintln!("{}", error)
        }
        println!();
        let chunk = compile(&input);
        let mut vm = VirtualMachine::new(chunk);
        vm.run();
    }
}



