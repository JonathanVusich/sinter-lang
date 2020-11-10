use crate::scanner::Scanner;
use crate::chunk::Chunk;

pub (crate) fn compile(code: &str) -> Option<Chunk> {
    let mut scanner = Scanner::new(code);

    return Chunk::new();
}