use crate::compiler::ast::Module;
use crate::compiler::compiler::{CompileError, CompilerCtxt};

pub struct TyCheckedModule {}

pub fn ty_check(
    ctxt: CompilerCtxt,
    module: Module,
) -> Result<(CompilerCtxt, Module), CompileError> {
    todo!()
}
