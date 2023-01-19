use std::fs::File;
use std::num::NonZeroUsize;
use std::path::Path;
use anyhow::Result;
use lasso::{Key as K, LargeSpur, Rodeo};
use serde::{Serialize, Deserialize, Deserializer};
use serde::de::DeserializeOwned;
use crate::compiler::ast::Stmt;
use crate::compiler::interner::{Interner, Key};
use crate::compiler::parser::parse;
use crate::compiler::{StringInterner, TyInterner};
use crate::compiler::ast::Expr::String;
use crate::compiler::tokens::tokenizer::tokenize_file;
use crate::compiler::type_checker::type_check;
use crate::compiler::types::types::{InternedStr, InternedTy, Type};

struct Compiler {

}

struct Application {
    entry_point: Box<Path>,
}

struct CompiledApplication {

}

#[derive(PartialEq, Eq, Debug, Serialize, Deserialize)]
pub struct CompilerCtxt {
    string_interner: StringInterner,
    ty_interner: TyInterner,
}

impl CompilerCtxt {
    pub (crate) fn intern_str(&mut self, str: &str) -> InternedStr {
        self.string_interner.get_or_intern(str)
    }

    pub (crate) fn resolve_str(&self, str: InternedStr) -> &str {
        self.string_interner.resolve(&str)
    }

    pub (crate) fn intern_ty(&mut self, ty: Type) -> InternedTy {
        self.ty_interner.intern(ty)
    }

    pub (crate) fn resolve_ty(&self, key: &InternedTy) -> &Type {
        self.ty_interner.resolve(key).unwrap()
    }
}

impl Default for CompilerCtxt {
    fn default() -> Self {
        Self {
            string_interner: StringInterner::default(),
            ty_interner: TyInterner::default(),
        }
    }
}

impl From<CompilerCtxt> for TyInterner {
    fn from(value: CompilerCtxt) -> Self {
        value.ty_interner
    }
}

impl From<CompilerCtxt> for StringInterner {
    fn from(value: CompilerCtxt) -> Self {
        value.string_interner
    }
}

impl From<CompilerCtxt> for (StringInterner, TyInterner) {
    fn from(value: CompilerCtxt) -> Self {
        (value.string_interner, value.ty_interner)
    }
}

fn compile(application: Application) -> Result<CompiledApplication> {
    let (compiler_ctxt, tokens) = tokenize_file(&application.entry_point)?;
    let (compiler_ctxt, module) = parse(compiler_ctxt, tokens)?;
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