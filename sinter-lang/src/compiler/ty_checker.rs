use crate::compiler::ast::AstModule;
use crate::compiler::compiler::CompilerCtxt;
use anyhow::Result;

pub struct TyCheckedModule {}

pub fn ty_check(ctxt: CompilerCtxt, module: AstModule) -> Result<(CompilerCtxt, AstModule)> {
    todo!()
}
