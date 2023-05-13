use crate::compiler::ast::{
    Ast, AstMap, AstPass, Item, ItemKind, NodeId, QualifiedIdent, Stmt, UseStmt,
};
use crate::compiler::ast_passes::{UsedModuleCollector, VisibilityCollector};
use crate::compiler::codegen::code_generator::emit_code;
use crate::compiler::interner::{Interner, Key};
use crate::compiler::parser::{parse, ParseError};
use crate::compiler::tokens::tokenized_file::Span;
use crate::compiler::tokens::tokenizer::tokenize_file;
use crate::compiler::ty_checker::ty_check;
use crate::compiler::types::types::InternedStr;
use crate::compiler::StringInterner;
use lasso::{Key as K, LargeSpur, Rodeo};
use serde::de::DeserializeOwned;
use serde::{Deserialize, Deserializer, Serialize};
use std::collections::{HashMap, VecDeque};
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::fs::File;
use std::mem::discriminant;
use std::num::NonZeroUsize;
use std::path::Path;

struct Compiler {
    modules: HashMap<QualifiedIdent, Ast>,
}

struct Application {
    entry_point: Box<Path>,
    module_paths: Vec<Box<Path>>,
}

pub struct CompiledApplication {}

#[derive(Debug)]
pub enum CompileError {
    Generic(Box<dyn Error>),
    ParseErrors(Vec<ParseError>),
}

impl Display for CompileError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

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

fn compile(application: Application) -> Result<CompiledApplication, CompileError> {
    let mut compiler_ctxt = CompilerCtxt::default();

    let main_ast = parse_ast(&mut compiler_ctxt, &application.entry_point)?;

    let used_modules = collect_used_modules(&mut compiler_ctxt, &main_ast);

    // TODO: Import transitive modules
    // TODO: Generate externally visible map
    // TODO: Resolve variables
    // TODO: Type check module
    // TODO: Generate bytecode
    let (compiler_ctxt, tychecked_module) = ty_check(compiler_ctxt, main_ast)?;

    emit_code(compiler_ctxt, tychecked_module)
}

fn parse_ast(compiler_ctxt: &mut CompilerCtxt, module_path: &Path) -> Result<Ast, CompileError> {
    let tokens = tokenize_file(compiler_ctxt, module_path)?;
    parse(compiler_ctxt, tokens)
}

fn collect_used_modules(compiler_ctxt: &mut CompilerCtxt, ast: &Ast) -> Vec<Ast> {
    let asts = Vec::new();
    asts
}
