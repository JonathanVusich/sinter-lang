use crate::compiler::compiler::CompilerCtxt;
use anyhow::Result;
use crate::compiler::ast::Module;

pub struct TyCheckedModule {

}

pub fn ty_check(ctxt: CompilerCtxt, module: Module) -> Result<(CompilerCtxt, TyCheckedModule)> {
    todo!()
}