use std::collections::{HashMap, HashSet, VecDeque};
use std::error::Error;
use std::ffi::{OsStr, OsString};
use std::fmt::{Display, Formatter};
use std::fs::File;
use std::mem::discriminant;
use std::num::NonZeroUsize;
use std::path::{Path, PathBuf};

use lasso::{Key as K, LargeSpur, Rodeo};
use serde::de::DeserializeOwned;
use serde::{Deserialize, Deserializer, Serialize};
use walkdir::{DirEntry, WalkDir};

use crate::compiler::ast::{AstPass, Item, ItemKind, Module, QualifiedIdent, Stmt, UseStmt};
use crate::compiler::ast_passes::{NameCollector, UsedCrateCollector};
use crate::compiler::codegen::code_generator::emit_code;
use crate::compiler::hir::{HirCrate, LocalDefId};
use crate::compiler::interner::{Interner, Key};
use crate::compiler::krate::{Crate, CrateId};
use crate::compiler::parser::{parse, ParseError};
use crate::compiler::path::ModulePath;
use crate::compiler::resolver::{resolve, ResolveError};
use crate::compiler::tokens::tokenized_file::Span;
use crate::compiler::tokens::tokenizer::{tokenize, tokenize_file};
use crate::compiler::ty_checker::ty_check;
use crate::compiler::types::{InternedStr, StrMap};
use crate::compiler::validator::{validate, ValidationError};
use crate::compiler::StringInterner;

#[derive(Default)]
pub struct Compiler {
    compiler_ctxt: CompilerCtxt,
}

#[cfg(test)]
impl From<Compiler> for CompilerCtxt {
    fn from(compiler: Compiler) -> Self {
        compiler.compiler_ctxt
    }
}

pub struct Application<'a> {
    main_crate: &'a Path,
    crate_path: &'a Path,
}

impl<'a> Application<'a> {
    pub fn new(main_crate: &'a Path, crate_path: &'a Path) -> Self {
        Application {
            main_crate,
            crate_path,
        }
    }
}

pub struct ByteCode {}

#[derive(Debug)]
pub enum CompileError {
    /// Generic error type
    Generic(Box<dyn Error>),
    /// Crate name contains non UTF-8 characters
    InvalidCrateName(OsString),
    /// Module name contains non UTF-8 characters
    InvalidModuleName(OsString),

    ValidationErrors(Vec<ValidationError>),

    /// Errors found during initial resolution
    ResolveErrors(Vec<ResolveError>),

    /// Errors found while parsing
    ParseErrors(Vec<ParseError>),

    /// Duplicate definition found while parsing
    DuplicateDefinition,
}

impl Display for CompileError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl<T> From<T> for CompileError
where
    T: Error + Sized + 'static,
{
    fn from(error: T) -> Self {
        CompileError::Generic(Box::new(error))
    }
}

#[derive(PartialEq, Debug, Default, Serialize, Deserialize)]
pub struct CompilerCtxt {
    string_interner: StringInterner,
    crate_id: usize,
    local_def_id: usize,
}

impl CompilerCtxt {
    pub(crate) fn intern_str(&mut self, str: &str) -> InternedStr {
        self.string_interner.get_or_intern(str).into()
    }

    pub(crate) fn resolve_str(&self, str: InternedStr) -> &str {
        self.string_interner.resolve(&str.into())
    }

    pub(crate) fn crate_id(&mut self) -> CrateId {
        let id = self.crate_id;
        self.local_def_id = 0;
        self.crate_id += 1;
        CrateId::new(id as u32)
    }

    pub(crate) fn local_def_id(&mut self) -> LocalDefId {
        let id = self.local_def_id;
        self.local_def_id += 1;
        (id as u32).into()
    }

    pub(crate) fn crate_local_def_id(&self) -> u32 {
        self.local_def_id as u32
    }

    pub(crate) fn module_path(&mut self, path: &Path) -> Result<ModulePath, CompileError> {
        let mut segments = Vec::new();
        for segment in path.iter() {
            let segment = segment
                .to_str()
                .ok_or_else(|| CompileError::InvalidModuleName(segment.to_os_string()))?;
            let interned_segment = self.intern_str(segment);
            segments.push(interned_segment);
        }

        Ok(ModulePath::from_vec(segments))
    }
}

impl From<CompilerCtxt> for StringInterner {
    fn from(value: CompilerCtxt) -> Self {
        value.string_interner
    }
}

impl Compiler {
    pub(crate) fn with_ctxt(compiler_ctxt: CompilerCtxt) -> Self {
        Self { compiler_ctxt }
    }

    pub(crate) fn compile(&mut self, application: Application) -> Result<ByteCode, CompileError> {
        let crates = self.parse_crates(&application)?;
        self.validate_crates(&crates)?;

        // TODO: Create crate resolution metadata and report initial resolution errors.
        // We cannot resolve everything in one pass, first we do use validation, var declaration validation, etc.
        let resolved_crates = self.resolve_crates(&crates)?;

        // TODO: Implement type inference algorithm
        /* We need to assign a type to every expression in the AST. This may require making changes to the AST
           classes to allow for types in every expression.
        */
        // TODO: Do type checking to ensure that all expressions evaluate to correct types

        // TODO: Lower the AST to HIR with the provided metadata.

        // TODO: Generate bytecode
        todo!()
    }

    pub(crate) fn parse_crates(
        &mut self,
        application: &Application,
    ) -> Result<StrMap<Crate>, CompileError> {
        let mut crates = HashMap::default();

        // Parse main crate
        let main_crate = self.parse_crate(&application.main_crate)?;
        let main_name = main_crate.name;

        // Insert crate into lookup map
        crates.insert(main_name, main_crate);

        // Collect all used crates from the main crate
        let crates_visited = HashSet::from([main_name]);
        let mut crates_to_visit: VecDeque<InternedStr> = VecDeque::from([main_name]);

        while let Some(crate_name) = crates_to_visit.pop_front() {
            let krate = crates.get(&crate_name).unwrap();

            let mut new_crates = Vec::new();
            for used_crate in krate.used_crates() {
                if !crates_visited.contains(&used_crate.crate_name) {
                    let crate_name = self.compiler_ctxt.resolve_str(used_crate.crate_name);
                    let mut crate_path = application.crate_path.to_path_buf();
                    crate_path.push(crate_name);

                    let krate = self.parse_crate(crate_path.as_path())?;
                    crates_to_visit.push_back(krate.name);

                    // Insert krate into lookup maps
                    new_crates.push(krate);
                }
            }

            for krate in new_crates {
                crates.insert(krate.name, krate);
            }
        }

        Ok(crates)
    }

    pub(crate) fn parse_crate(&mut self, path: &Path) -> Result<Crate, CompileError> {
        let krate_name = path
            .file_name()
            .ok_or_else(|| CompileError::InvalidCrateName(path.as_os_str().to_os_string()))
            .and_then(|name| {
                name.to_str()
                    .ok_or_else(|| CompileError::InvalidCrateName(name.to_os_string()))
            })?;
        let krate_name = self.compiler_ctxt.intern_str(krate_name);

        let code_files: Vec<DirEntry> = WalkDir::new(path)
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
            .collect();

        let mut krate = Crate::new(krate_name, self.compiler_ctxt.crate_id());

        for file in code_files {
            let local_path = local_to_root(file.path(), path)?;
            let module_path = self.compiler_ctxt.module_path(local_path)?;

            let mut ast = self.parse_ast(file.path())?;

            ast.path = module_path.clone();
            krate.add_module(module_path, ast);
        }

        // Assign the last used local def id to the crate
        krate.local_def_id = self.compiler_ctxt.crate_local_def_id();

        Ok(krate)
    }

    pub(crate) fn parse_ast(&mut self, path: &Path) -> Result<Module, CompileError> {
        let tokens = tokenize_file(&mut self.compiler_ctxt, path)?;
        parse(&mut self.compiler_ctxt, tokens)
    }

    pub(crate) fn validate_crates(&mut self, crates: &StrMap<Crate>) -> Result<(), CompileError> {
        let mut validation_errors = Vec::new();
        for krate in crates.values() {
            for module in krate.modules() {
                validation_errors.extend(validate(crates, krate, module));
            }
        }
        if !validation_errors.is_empty() {
            Err(CompileError::ValidationErrors(validation_errors))
        } else {
            Ok(())
        }
    }

    pub(crate) fn resolve_crates(
        &mut self,
        crates: &StrMap<Crate>,
    ) -> Result<StrMap<HirCrate>, CompileError> {
        resolve(crates)
    }
}

fn local_to_root<'a>(path: &'a Path, root: &Path) -> Result<&'a Path, CompileError> {
    path.strip_prefix(root)
        .map_err(|err| CompileError::Generic(Box::new(err)))
}

mod tests {
    use std::assert_matches::assert_matches;

    use snap::snapshot;

    use crate::compiler::ast::ItemKind;
    use crate::compiler::compiler::{Compiler, CompilerCtxt};
    use crate::compiler::krate::{Crate, CrateId};
    use crate::compiler::path::ModulePath;
    #[cfg(test)]
    use crate::util::utils::resolve_test_krate_path;

    #[test]
    #[snapshot]
    pub fn simple_arithmetic() -> Crate {
        let krate_path = resolve_test_krate_path("simple_arithmetic");
        let mut compiler = Compiler::default();
        let krate = compiler.parse_crate(&krate_path).unwrap();

        assert_eq!(
            "simple_arithmetic",
            compiler.compiler_ctxt.resolve_str(krate.name)
        );
        krate
    }

    #[test]
    #[snapshot]
    pub fn complex_arithmetic() -> Crate {
        let krate_path = resolve_test_krate_path("complex_arithmetic");
        let mut compiler = Compiler::default();
        let krate = compiler.parse_crate(&krate_path).unwrap();

        assert_eq!(
            "complex_arithmetic",
            compiler.compiler_ctxt.resolve_str(krate.name)
        );
        krate
    }
}
