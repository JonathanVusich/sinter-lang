use crate::compiler::ast::AstModule;
use crate::compiler::compiler::{CompiledApplication, CompilerCtxt};
use crate::compiler::ty_checker::TyCheckedModule;
use anyhow::Result;

pub fn emit_code(ctxt: CompilerCtxt, module: AstModule) -> Result<CompiledApplication> {
    todo!()
}
