use crate::compiler::ast::Module;
use crate::compiler::compiler::CompilerCtxt;
use anyhow::Result;

pub struct ResolvedModule {

}

pub fn resolve(ctxt: CompilerCtxt, module: Module) -> Result<(CompilerCtxt, ResolvedModule)> {
    todo!()
}