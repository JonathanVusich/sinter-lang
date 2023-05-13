use crate::compiler::ast::Ast;
use crate::compiler::compiler::{CompileError, CompiledApplication, CompilerCtxt};
use crate::compiler::ty_checker::TyCheckedModule;

pub fn emit_code(ctxt: CompilerCtxt, module: Ast) -> Result<CompiledApplication, CompileError> {
    todo!()
}
