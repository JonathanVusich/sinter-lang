use std::collections::{HashMap, VecDeque};
use std::fs::File;
use std::mem::discriminant;
use std::num::NonZeroUsize;
use std::path::Path;
use anyhow::Result;
use lasso::{Key as K, LargeSpur, Rodeo};
use serde::{Serialize, Deserialize, Deserializer};
use serde::de::DeserializeOwned;
use crate::compiler::ast::{AstNode, Module, NodeId, NodeKind, QualifiedIdent, ResolvedModule, Stmt, UseStmt};
use crate::compiler::interner::{Interner, Key};
use crate::compiler::parser::parse;
use crate::compiler::{StringInterner, TyInterner};
use crate::compiler::ast::Expr::String;
use crate::compiler::ast::OuterStmt::Use;
use crate::compiler::codegen::code_generator::emit_code;
use crate::compiler::resolver::resolve_module;
use crate::compiler::tokens::tokenized_file::Span;
use crate::compiler::tokens::tokenizer::tokenize_file;
use crate::compiler::ty_checker::ty_check;
use crate::compiler::types::types::{InternedStr, InternedTy, Type};

struct Compiler {
    modules: HashMap<QualifiedIdent, Module>,
}

struct Application {
    entry_point: Box<Path>,
}

pub struct CompiledApplication {
}

#[derive(PartialEq, Eq, Debug, Serialize, Deserialize)]
pub struct CompilerCtxt {
    string_interner: StringInterner,
    ty_interner: TyInterner,

    ast_id: usize,
}

impl CompilerCtxt {

    pub (crate) fn create_node(&mut self, node: NodeKind, span: Span) -> AstNode {
        let node_id = NodeId::new(self.ast_id);
        self.ast_id += 1;
        AstNode::new(node, node_id, span)
    }

    pub (crate) fn intern_str(&mut self, str: &str) -> InternedStr {
        self.string_interner.get_or_intern(str)
    }

    pub (crate) fn resolve_str(&self, str: InternedStr) -> &str {
        self.string_interner.resolve(&str)
    }

    pub (crate) fn intern_ty(&mut self, ty: Type) -> InternedTy {
        self.ty_interner.intern(ty)
    }

    pub (crate) fn resolve_ty(&self, key: InternedTy) -> &Type {
        self.ty_interner.resolve(key).unwrap()
    }
}

impl Default for CompilerCtxt {
    fn default() -> Self {
        Self {
            string_interner: StringInterner::default(),
            ty_interner: TyInterner::default(),
            ast_id: 0,
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
    let compiler_ctxt = CompilerCtxt::default();

    let (compiler_ctxt, module) = import_module(compiler_ctxt, &application.entry_point)?;
    let (compiler_ctxt, tychecked_module) = ty_check(compiler_ctxt, module)?;

    emit_code(compiler_ctxt, tychecked_module)
}


fn import_module(compiler_ctxt: CompilerCtxt, module_path: &Path) -> Result<(CompilerCtxt, ResolvedModule)> {
    let (compiler_ctxt, tokens) = tokenize_file(module_path.as_ref())?;
    let (compiler_ctxt, module) = parse(compiler_ctxt, tokens)?;

    // Ensure that all names and variable uses are correct
    resolve_module(compiler_ctxt, module, module_path)
}