use std::fs::File;
use std::path::Path;
use anyhow::Result;
use serde::{Serialize, Deserialize};
use crate::compiler::ast::Stmt;
use crate::compiler::interner::Interner;
use crate::compiler::parser::parse;
use crate::compiler::{StringInterner, TyInterner};
use crate::compiler::tokens::tokenizer::tokenize_file;
use crate::compiler::type_checker::type_check;
use crate::compiler::types::types::Type;

struct Compiler {

}

struct Application {
    entry_point: Box<Path>,
}

struct CompiledApplication {

}

#[derive(PartialEq, Eq, Default, Debug, Serialize, Deserialize)]
#[serde(bound(deserialize = "'ctxt: 'de"))]
pub (crate) struct CompilerCtxt<'ctxt> {
    string_interner: StringInterner,
    ty_interner: TyInterner<'ctxt>,
}

unsafe impl Send for CompilerCtxt<'_> {}
unsafe impl Sync for CompilerCtxt<'_> {}

impl<'ctxt> CompilerCtxt<'ctxt> {

    pub (crate) fn string_interner(&self) -> StringInterner {
        self.string_interner.clone()
    }

    pub (crate) fn ty_interner(&self) -> TyInterner {
        self.ty_interner.clone()
    }
}

fn compile(application: Application) -> Result<CompiledApplication> {
    let ctxt = CompilerCtxt::default();
    let tokens = tokenize_file(&ctxt, &application.entry_point)?;
    let module = parse(&ctxt, tokens)?;
    
    let type_report = type_check(&module);

    for stmt in module.stmts() {
        match stmt {
            Stmt::Let(let_stmt) => {

            }
            _ => {

            }
        }
    }


    // let file = File::open(path)?;
    todo!()
}