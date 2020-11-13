use crate::scanner::Scanner;
use crate::chunk::Chunk;

pub (crate) fn compile(code: &str) -> Chunk {
    let mut scanner = Scanner::new(code);
    loop {

    }
    return Chunk::new();
}