use crate::compiler::compiler::{CompiledApplication, CompilerCtxt};
use crate::compiler::ty_checker::TyCheckedModule;
use anyhow::Result;

pub fn emit_code(ctxt: CompilerCtxt, module: TyCheckedModule) -> Result<CompiledApplication> {
    todo!()
}