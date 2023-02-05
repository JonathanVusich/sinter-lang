use crate::compiler::compiler::CompilerCtxt;
use crate::compiler::resolver::ResolvedModule;
use anyhow::Result;

pub struct TyCheckedModule {

}

pub fn ty_check(ctxt: CompilerCtxt, module: ResolvedModule) -> Result<(CompilerCtxt, TyCheckedModule)> {
    todo!()
}