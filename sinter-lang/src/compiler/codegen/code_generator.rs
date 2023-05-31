use crate::compiler::ast::Ast;
use crate::compiler::compiler::{ByteCode, CompileError, CompilerCtxt};
use crate::compiler::ty_checker::TyCheckedModule;

pub fn emit_code(ctxt: CompilerCtxt, module: Ast) -> Result<ByteCode, CompileError> {
    todo!()
}
