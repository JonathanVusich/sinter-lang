use crate::compiler::ast::Module;
use crate::compiler::compiler::CompilerCtxt;
use anyhow::Result;

pub struct TyCheckedModule {}

pub fn ty_check(ctxt: CompilerCtxt, module: Module) -> Result<(CompilerCtxt, Module)> {
    todo!()
}
