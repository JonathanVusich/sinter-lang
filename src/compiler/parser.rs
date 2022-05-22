use crate::class::compiled_class::CompiledClass;
use crate::compiler::ast::{ClassDecl, Module};
use anyhow::Result;
use string_interner::StringInterner;
use crate::compiler::tokens::tokenized_file::TokenizedInput;
use crate::compiler::types::types::Type;

pub fn parse(input: TokenizedInput) -> Result<Module> {
    todo!()
}

struct Parser {
    ident_arena: StringInterner,
}