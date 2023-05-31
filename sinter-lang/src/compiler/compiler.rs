use crate::compiler::ast::{
    Ast, AstId, AstMap, AstPass, Item, ItemKind, NodeId, QualifiedIdent, Stmt, UseStmt,
};
use crate::compiler::ast_passes::{UsedCrateCollector, VisibilityCollector};
use crate::compiler::codegen::code_generator::emit_code;
use crate::compiler::interner::{Interner, Key};
use crate::compiler::krate::{Krate, KrateId};
use crate::compiler::parser::{parse, ParseError};
use crate::compiler::path::ModulePath;
use crate::compiler::resolver::VarResolver;
use crate::compiler::tokens::tokenized_file::Span;
use crate::compiler::tokens::tokenizer::{tokenize, tokenize_file};
use crate::compiler::ty_checker::ty_check;
use crate::compiler::types::types::InternedStr;
use crate::compiler::StringInterner;
use lasso::{Key as K, LargeSpur, Rodeo};
use serde::de::DeserializeOwned;
use serde::{Deserialize, Deserializer, Serialize};
use std::collections::{HashMap, VecDeque};
use std::error::Error;
use std::ffi::{OsStr, OsString};
use std::fmt::{Display, Formatter};
use std::fs::File;
use std::mem::discriminant;
use std::num::NonZeroUsize;
use std::path::{Path, PathBuf};
use walkdir::{DirEntry, WalkDir};

struct Compiler {
    modules: HashMap<QualifiedIdent, Ast>,
}

struct Application {
    main_crate: Box<Path>,
    crate_path: Box<Path>,
}

pub struct ByteCode {}

#[derive(Debug)]
pub enum CompileError {
    Generic(Box<dyn Error>),
    InvalidCrateName(OsString),
    InvalidModuleName(OsString),
    ParseErrors(Vec<ParseError>),
}

impl Display for CompileError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

#[derive(PartialEq, Debug, Default, Serialize, Deserialize)]
pub struct CompilerCtxt {
    string_interner: StringInterner,
    krate_id: usize,
    krates: Vec<Krate>,
    krate_lookup: HashMap<InternedStr, usize>,
}

impl CompilerCtxt {
    pub(crate) fn parse_main_krate(&mut self, path: &Path) -> Result<(), CompileError> {
        let krate = self.parse_crate(path)?;

        self.krate_lookup.insert(krate.name, krate.id.into());
        self.krates.push(krate);
        Ok(())
    }

    pub(crate) fn parse_crate(&mut self, path: &Path) -> Result<Krate, CompileError> {
        let krate_name = path
            .file_name()
            .ok_or_else(|| CompileError::InvalidCrateName(path.as_os_str().to_os_string()))
            .and_then(|name| {
                name.to_str()
                    .ok_or_else(|| CompileError::InvalidCrateName(name.to_os_string()))
            })?;
        let krate_name = self.intern_str(krate_name);

        let code_files = WalkDir::new(path)
            .follow_links(true)
            .into_iter()
            .filter_map(Result::ok)
            .filter(|entry| {
                let is_code_file = entry
                    .path()
                    .extension()
                    .and_then(OsStr::to_str)
                    .filter(|ext| *ext == "si")
                    .is_some();
                is_code_file
            })
            .collect::<Vec<DirEntry>>();

        let mut krate = Krate::new(krate_name);
        let krate_id = self.krate_id;
        self.krate_id += 1;
        let mut ast_id = 0;
        for file in code_files {
            // TODO: Refactor this
            let mut segments = Vec::new();
            for segment in file.path().to_path_buf().iter() {
                let segment = segment
                    .to_str()
                    .ok_or_else(|| CompileError::InvalidModuleName(segment.to_os_string()))?;
                let interned_segment = self.intern_str(segment);
                segments.push(interned_segment);
            }

            let module_path = ModulePath::new(segments);
            let mut ast = self.parse_ast(file.path())?;
            let ast_id = AstId::new(krate_id, ast_id);
            let id = krate.modules.len();
            let ast_id = AstId::new(krate.id.into(), id);
            ast.id = ast_id;
            krate.modules.push(ast);
            krate.module_lookup.insert(module_path, id);
        }

        Ok(krate)
    }

    pub(crate) fn parse_ast(&mut self, path: &Path) -> Result<Ast, CompileError> {
        let tokens = tokenize_file(self, path)?;
        parse(self, tokens)
    }

    pub(crate) fn parse_used_crates(&mut self, crate_path: &Path) -> Result<(), CompileError> {
        // Do a DFS search of all code files and import all unique, referenced crates
        let mut buffer: VecDeque<AstId> = self.krates
            .iter()
            .flat_map(|krate| &krate.modules)
            .map(|ast| ast.id)
            .collect();

        while let Some(ast_id) = buffer.pop_front() {
            buffer.extend(self.collect_crates(ast_id, crate_path)?);
        }
        Ok(())
    }

    fn collect_crates(
        &mut self,
        ast_id: AstId,
        module_path: &Path,
    ) -> Result<Vec<AstId>, CompileError> {
        let ast = &mut self
            .krates[ast_id.krate]
            .modules[ast_id.ast];

        let used_crates = UsedCrateCollector::visit(ast);
        let mut used_asts = Vec::new();

        for used_crate in &used_crates {
            if !self.krate_lookup.contains_key(&used_crate.crate_name) {
                let crate_name = self.resolve_str(used_crate.crate_name);
                let mut crate_path = module_path.to_path_buf();
                crate_path.push(crate_name);

                self.parse_crate(crate_path.as_path())?;
            }
        }
        Ok(used_asts)
    }

    fn resolve_path(&mut self, module_path: &Path, idents: &Vec<InternedStr>) -> PathBuf {
        let mut path = PathBuf::from(module_path);
        for ident in idents {
            path.push(self.resolve_str(*ident));
        }
        path
    }

    pub(crate) fn resolve_variables(&self) -> Result<(), CompileError> {
        todo!()
    }

    pub(crate) fn ty_check(&self) -> Result<(), CompileError> {
        todo!()
    }

    pub(crate) fn emit_code(&self) -> Result<ByteCode, CompileError> {
        todo!()
    }

    pub(crate) fn intern_str(&mut self, str: &str) -> InternedStr {
        self.string_interner.get_or_intern(str).into()
    }

    pub(crate) fn resolve_str(&self, str: InternedStr) -> &str {
        self.string_interner.resolve(&str.into())
    }
}

impl From<CompilerCtxt> for StringInterner {
    fn from(value: CompilerCtxt) -> Self {
        value.string_interner
    }
}

fn compile(application: Application) -> Result<ByteCode, CompileError> {
    let mut compiler_ctxt = CompilerCtxt::default();

    compiler_ctxt.parse_main_krate(&application.main_crate)?;
    compiler_ctxt.parse_used_crates(&application.crate_path)?;

    compiler_ctxt.resolve_variables()?;
    compiler_ctxt.ty_check()?;
    compiler_ctxt.emit_code()

    // TODO: Generate externally visible map
    // TODO: Resolve types, variables
    // TODO: Type check module
    // TODO: Generate bytecode
}
