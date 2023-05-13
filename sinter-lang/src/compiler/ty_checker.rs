use crate::compiler::ast::Ast;
use crate::compiler::compiler::{CompileError, CompilerCtxt};

pub struct TyCheckedModule {}

pub fn ty_check(ctxt: CompilerCtxt, module: Ast) -> Result<(CompilerCtxt, Ast), CompileError> {
    todo!()
}
