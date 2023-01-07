use std::fs::File;
use std::path::Path;
use anyhow::Result;
use crate::compiler::ast::Stmt;
use crate::compiler::parser::parse;
use crate::compiler::StringInterner;
use crate::compiler::tokens::tokenizer::tokenize_file;
use crate::compiler::type_checker::type_check;

struct Compiler {

}

struct Application {
    entry_point: Box<Path>,
}

struct CompiledApplication {

}

fn compile(application: Application) -> Result<CompiledApplication> {
    let mut string_interner = StringInterner::default();
    let tokens = tokenize_file(string_interner.clone(), &application.entry_point)?;
    let module = parse(string_interner.clone(), tokens)?;
    
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