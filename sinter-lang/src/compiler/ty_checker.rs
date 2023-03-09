use crate::compiler::compiler::CompilerCtxt;
use anyhow::Result;
use crate::compiler::ast::{Module, ResolvedModule};

pub struct TyCheckedModule {

}

pub fn ty_check(ctxt: CompilerCtxt, module: ResolvedModule) -> Result<(CompilerCtxt, TyCheckedModule)> {
    todo!()
}