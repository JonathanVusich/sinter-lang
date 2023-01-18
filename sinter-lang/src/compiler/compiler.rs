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
pub (crate) struct CompilerCtxt {
    string_interner: StringInterner,
    ty_interner: TyInterner,
}

impl CompilerCtxt {
    pub (crate) fn intern_str(&mut self, str: &str) -> InternedStr {
        let spur = self.string_interner.get_or_intern(str);
        Key::new(spur.into_inner())
    }

    pub (crate) fn resolve_str(&self, key: Key) -> &'static str {
        let spur = LargeSpur::try_from_usize(*key.into()).unwrap();
        self.string_interner.resolve(&spur)
    }

    pub (crate) fn intern_ty(&mut self, ty: Type) -> InternedTy {
        self.ty_interner.intern(ty)
    }

    pub (crate) fn resolve_ty(&self, key: &Key) -> &Type {
        self.ty_interner.resolve(key).unwrap()
    }
}

impl Default for CompilerCtxt {
    fn default() -> Self {
        Self {
            string_interner: Rodeo::<LargeSpur>::new(),
            ty_interner: TyInterner::default(),
        }
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