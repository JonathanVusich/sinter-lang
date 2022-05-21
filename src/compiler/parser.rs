use crate::class::compiled_class::CompiledClass;
use crate::compiler::ast::Module;
use anyhow::Result;
use crate::compiler::tokens::tokenized_file::TokenizedInput;

pub fn parse<'ctx>(input: TokenizedInput) -> Result<Module<'ctx>> {
    todo!()
}

struct Parser {

}