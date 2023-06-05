use crate::compiler::ast::Module;
use crate::compiler::compiler::{ByteCode, CompileError, CompilerCtxt};
use crate::compiler::ty_checker::TyCheckedModule;

pub fn emit_code(ctxt: CompilerCtxt, module: Module) -> Result<ByteCode, CompileError> {
    todo!()
}
