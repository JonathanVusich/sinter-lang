use crate::compiler::ast::{ItemKind, Module, NodeId, QualifiedIdent, Stmt, UseStmt};
use crate::compiler::codegen::code_generator::emit_code;
use crate::compiler::interner::{Interner, Key};
use crate::compiler::parser::parse;
use crate::compiler::tokens::tokenized_file::Span;
use crate::compiler::tokens::tokenizer::tokenize_file;
use crate::compiler::ty_checker::ty_check;
use crate::compiler::types::types::InternedStr;
use crate::compiler::StringInterner;
use anyhow::Result;
use lasso::{Key as K, LargeSpur, Rodeo};
use serde::de::DeserializeOwned;
use serde::{Deserialize, Deserializer, Serialize};
use std::collections::{HashMap, VecDeque};
use std::fs::File;
use std::mem::discriminant;
use std::num::NonZeroUsize;
use std::path::Path;

struct Compiler {
    modules: HashMap<QualifiedIdent, Module>,
}

struct Application {
    entry_point: Box<Path>,
}

pub struct CompiledApplication {}

#[derive(PartialEq, Eq, Debug, Serialize, Deserialize)]
pub struct CompilerCtxt {
    string_interner: StringInterner,
}

impl CompilerCtxt {
    pub(crate) fn intern_str(&mut self, str: &str) -> InternedStr {
        self.string_interner.get_or_intern(str).into()
    }

    pub(crate) fn resolve_str(&self, str: InternedStr) -> &str {
        self.string_interner.resolve(&str.into())
    }
}

impl Default for CompilerCtxt {
    fn default() -> Self {
        Self {
            string_interner: StringInterner::default(),
        }
    }
}

impl From<CompilerCtxt> for StringInterner {
    fn from(value: CompilerCtxt) -> Self {
        value.string_interner
    }
}

fn compile(application: Application) -> Result<CompiledApplication> {
    let compiler_ctxt = CompilerCtxt::default();

    let (compiler_ctxt, module) = import_module(compiler_ctxt, &application.entry_point)?;
    let (compiler_ctxt, tychecked_module) = ty_check(compiler_ctxt, module)?;

    emit_code(compiler_ctxt, tychecked_module)
}

fn import_module(
    compiler_ctxt: CompilerCtxt,
    module_path: &Path,
) -> Result<(CompilerCtxt, Module)> {
    let (compiler_ctxt, tokens) = tokenize_file(module_path.as_ref())?;
    let (compiler_ctxt, module) = parse(compiler_ctxt, tokens);

    // TODO: Generate HIR
    // TODO: Generate externally visible map
    // TODO: Import transitive modules
    // TODO: Resolve variables
    // TODO: Type check module
    // TODO: Generate bytecode
    todo!()
}
