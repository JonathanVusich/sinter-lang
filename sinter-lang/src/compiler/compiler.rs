use std::backtrace::Backtrace;
use std::collections::{HashMap, HashSet, VecDeque};
use std::ffi::{OsStr, OsString};
use std::fmt::{Debug, Display, Formatter};
use std::ops::Deref;
use std::os::windows::ffi::OsStrExt;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use itertools::Itertools;
use lasso::Key as K;
use serde::de::DeserializeOwned;
use serde::{Deserialize, Deserializer, Serialize};
use walkdir::{DirEntry, WalkDir};

use crate::compiler::ast::{AstPass, Module};
use crate::compiler::hir::{HirCrate, HirMap, LocalDefId};
use crate::compiler::krate::{Crate, CrateId};
use crate::compiler::parser::{parse, ParseErrKind};
use crate::compiler::path::ModulePath;
use crate::compiler::resolver::{resolve, ResolveErrKind};
use crate::compiler::tokens::tokenizer::{tokenize, tokenize_file};
use crate::compiler::type_inference::ty_infer::{CrateInference, Type, TypeErrKind};
use crate::compiler::types::{DefMap, InternedStr, LDefMap, StrMap};
use crate::compiler::validator::{validate, ValidationErrKind};
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

pub enum Application<'a> {
    Path {
        main_crate: &'a Path,
        crate_path: &'a Path,
    },
    Inline {
        code: &'a str,
    },
}

pub struct ByteCode {}

pub trait ErrorKind {
    type Err;

    fn backtrace(&self) -> &Backtrace;
    fn error(&self) -> &Self::Err;
}

macro_rules! backtrace_err {
    ($ty:ty, $name:ident) => {
        #[derive(Debug)]
        pub struct $name {
            inner: $ty,
            #[cfg(debug_assertions)]
            backtrace: Backtrace,
        }

        impl From<$ty> for $name {
            fn from(inner: $ty) -> Self {
                Self {
                    inner,
                    #[cfg(debug_assertions)]
                    backtrace: Backtrace::force_capture(),
                }
            }
        }

        impl $name {
            #[cfg(debug_assertions)]
            pub fn backtrace(&self) -> &Backtrace {
                &self.backtrace
            }

            pub fn into_inner(self) -> $ty {
                self.inner
            }
        }

        impl Deref for $name {
            type Target = $ty;

            fn deref(&self) -> &Self::Target {
                &self.inner
            }
        }
    };
}

backtrace_err!(Box<dyn std::error::Error>, Generic);
backtrace_err!(OsString, InvalidName);
backtrace_err!(ParseErrKind, ParseError);
backtrace_err!(ValidationErrKind, ValidationError);
backtrace_err!(ResolveErrKind, ResolveError);
backtrace_err!(TypeErrKind, TypeError);

pub enum CompileError {
    /// Generic error type
    Generic(Generic),
    /// Crate name contains non UTF-8 characters
    InvalidCrateName(InvalidName),
    /// Module name contains non UTF-8 characters
    InvalidModuleName(InvalidName),
    /// Errors found while parsing
    ParseErrors(Vec<ParseError>),

    /// Errors found while validating the AST.
    ValidationErrors(Vec<ValidationError>),

    /// Errors found during initial resolution
    ResolveErrors(Vec<ResolveError>),

    /// Errors found while inferring types
    TypeErrors(Vec<TypeError>),
}

impl Display for CompileError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            CompileError::Generic(error) => Debug::fmt(error, f),
            CompileError::InvalidCrateName(error) => Debug::fmt(error, f),
            CompileError::InvalidModuleName(error) => Debug::fmt(error, f),
            CompileError::ParseErrors(errors) => errors.fmt(f),
            CompileError::ValidationErrors(errors) => errors.fmt(f),
            CompileError::ResolveErrors(errors) => errors.fmt(f),
            CompileError::TypeErrors(errors) => errors.fmt(f),
        }
    }
}

impl Debug for CompileError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self, f)
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

    pub(crate) fn module_path<T: AsRef<Path>>(
        &mut self,
        path: T,
    ) -> Result<ModulePath, CompileError> {
        let mut segments = Vec::new();
        for segment in path.as_ref().iter() {
            let segment = segment.to_str().ok_or_else(|| {
                CompileError::InvalidModuleName(InvalidName::from(segment.to_os_string()))
            })?;
            let interned_segment = self.intern_str(segment);
            segments.push(interned_segment);
        }

        Ok(ModulePath::from_iter(segments))
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
        let mut crates = self.parse_crates(application)?;

        self.validate_crates(&crates)?;
        let resolved_crates = self.resolve_crates(&mut crates)?;

        // TODO: Add validation step to check for function parameter matching.

        self.infer_types(&resolved_crates)?;
        // TODO: Lower the AST to MIR with the provided metadata.

        // TODO: Generate bytecode
        todo!()
    }

    pub(crate) fn parse_crates(
        &mut self,
        application: Application,
    ) -> Result<StrMap<Crate>, CompileError> {
        match application {
            Application::Path {
                main_crate,
                crate_path,
            } => {
                let mut crates = StrMap::default();

                // Parse main crate
                let main_crate = self.parse_crate(main_crate)?;
                let main_name = main_crate.name;

                // Insert crate into lookup map
                crates.insert(main_name, main_crate);

                // Collect all used crates from the main crate
                let mut crates_visited = HashSet::from([main_name]);
                let mut crates_to_visit: VecDeque<InternedStr> = VecDeque::from([main_name]);

                while let Some(crate_name) = crates_to_visit.pop_front() {
                    let krate = crates.get(&crate_name).unwrap();

                    let mut new_crates = Vec::new();
                    for used_crate in krate.used_crates() {
                        if !crates_visited.contains(&used_crate.crate_name) {
                            let crate_name = self.compiler_ctxt.resolve_str(used_crate.crate_name);
                            let mut crate_path = crate_path.to_path_buf();
                            crate_path.push(crate_name);

                            let krate = self.parse_crate(crate_path.as_path())?;
                            crates_to_visit.push_back(krate.name);
                            crates_visited.insert(krate.name);

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
            Application::Inline { code } => {
                let krate = self.parse_inline_crate(code)?;
                Ok(StrMap::from([(krate.name, krate)]))
            }
        }
    }

    pub(crate) fn parse_inline_crate(&mut self, code: &str) -> Result<Crate, CompileError> {
        let krate_name = self.compiler_ctxt.intern_str("krate");

        let mut krate = Crate::new(krate_name, self.compiler_ctxt.crate_id());

        let tokens = tokenize(&mut self.compiler_ctxt, code);
        let mut ast = parse(&mut self.compiler_ctxt, tokens)?;
        let module_name = self.compiler_ctxt.intern_str("module");
        let module_path = ModulePath::from_iter([module_name]);

        ast.path = module_path.clone();
        krate.add_module(module_path, ast);

        Ok(krate)
    }

    pub(crate) fn parse_crate(&mut self, path: &Path) -> Result<Crate, CompileError> {
        let krate_name = path
            .file_name()
            .ok_or_else(|| {
                CompileError::InvalidCrateName(InvalidName::from(path.as_os_str().to_os_string()))
            })
            .and_then(|name| {
                name.to_str().ok_or_else(|| {
                    CompileError::InvalidCrateName(InvalidName::from(name.to_os_string()))
                })
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
            .sorted_by_key(|entry| entry.file_name().to_os_string()) // TODO: Maybe make this only apply to tests?
            .collect();

        let mut krate = Crate::new(krate_name, self.compiler_ctxt.crate_id());

        for file in code_files {
            let local_path = local_to_root(file.path(), path)?;
            let module_path = self.compiler_ctxt.module_path(local_path)?;

            let mut module = self.parse_ast(file.path())?;

            module.path = module_path.clone();
            krate.add_module(module_path, module);
        }

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
        crates: &mut StrMap<Crate>,
    ) -> Result<HirMap, CompileError> {
        resolve(&self.compiler_ctxt, crates)
    }

    pub(crate) fn infer_types(
        &mut self,
        hir_map: &HirMap,
    ) -> Result<StrMap<LDefMap<Type>>, CompileError> {
        let mut ty_map = StrMap::default();
        for krate in hir_map.krates() {
            ty_map.insert(krate.name, CrateInference::new(krate, hir_map).infer_tys()?);
        }
        Ok(ty_map)
    }
}

fn local_to_root(path: &Path, root: &Path) -> Result<PathBuf, CompileError> {
    path.strip_prefix(root)
        .map(|path| path.with_extension(""))
        .map_err(|err| {
            let boxed_error: Box<dyn std::error::Error> = Box::new(err);
            CompileError::Generic(Generic::from(boxed_error))
        })
}

mod tests {
    use snap::snapshot;

    use crate::compiler::compiler::Compiler;
    use crate::compiler::krate::Crate;
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
        enum Test {
            Var,
        }
        impl Test {
            pub fn diameter(self) -> u32 {
                5
            }
        }
        let diameter = Test::Var.diameter();

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
