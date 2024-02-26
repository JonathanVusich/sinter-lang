use std::collections::{HashSet, VecDeque};
use std::ffi::{OsStr, OsString};
use std::fmt::{Debug, Display};
use std::io;
use std::ops::Deref;
use std::path::{Path, PathBuf, StripPrefixError};

use ast::{AstPass, ModulePath};
use diagnostics::{Diagnostic, DiagnosticKind, Diagnostics, FatalError};
use hir::HirMap;
use id::IdGenerator;
use interner::{InternedStr, StringInterner};
use itertools::Itertools;
use lasso::{Key as K, Resolver};
use parser::parse;
use serde::de::DeserializeOwned;
use serde::{Deserialize, Deserializer, Serialize};
use source::{SourceCode, SourceMap};
use tokenizer::{tokenize, tokenize_file, TokenizedSource};
use types::StrMap;
use walkdir::{DirEntry, WalkDir};

use krate::Crate;
use validator::validate;

use crate::compiler::resolver::resolve;
use crate::compiler::type_inference::ty_infer::{CrateInference, TypeMap};

#[derive(Default)]
pub struct Compiler {
    compiler_ctxt: CompilerCtxt,
    string_interner: StringInterner,
    diagnostics: Diagnostics,
    source_map: SourceMap,
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
        code: String,
    },
}

pub struct ByteCode {}

#[derive(PartialEq, Debug, Default, Serialize, Deserialize)]
pub struct CompilerCtxt {
    pub(crate) string_interner: StringInterner,
    pub(crate) source_map: SourceMap,
    pub(crate) diagnostics: Diagnostics,
    pub(crate) id_generator: IdGenerator,
}

impl CompilerCtxt {
    pub(crate) fn intern_str(&mut self, str: &str) -> InternedStr {
        self.string_interner.intern(str)
    }

    pub(crate) fn resolve_str(&self, str: InternedStr) -> &str {
        self.string_interner.resolve(str)
    }

    pub(crate) fn module_path<T: AsRef<Path>>(&mut self, path: T) -> Option<ModulePath> {
        let mut segments = Vec::new();
        for segment in path.as_ref().iter() {
            let segment = segment.to_str().or_else(|| {
                self.diagnostics
                    .push(Diagnostic::Fatal(FatalError::InvalidOsStr));
                None
            })?;
            let interned_segment = self.intern_str(segment);
            segments.push(interned_segment);
        }

        Some(ModulePath::from_iter(segments))
    }
}

impl From<CompilerCtxt> for StringInterner {
    fn from(value: CompilerCtxt) -> Self {
        value.string_interner
    }
}

impl Compiler {
    pub(crate) fn compile(&mut self, application: Application) -> Result<ByteCode, Diagnostics> {
        let mut crates = self.parse_crates(application)?;
        crates = self.validate_crates(crates)?;

        let resolved_crates = self.resolve_crates(crates)?;
        let inferred_crates = self.infer_types(resolved_crates)?;
        // TODO: Lower the AST to MIR with the provided metadata.

        // TODO: Generate bytecode
        todo!()
    }

    pub fn parse_crates(&mut self, application: Application) -> Result<StrMap<Crate>, Diagnostics> {
        let crates = match application {
            Application::Path {
                main_crate,
                crate_path,
            } => {
                let mut crates = StrMap::default();

                // Parse main crate
                let main_crate = self
                    .parse_crate(main_crate)
                    .ok_or(self.diagnostics.clone())?;
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

                            let krate = self
                                .parse_crate(crate_path.as_path())
                                .ok_or(self.diagnostics.clone())?;
                            crates_to_visit.push_back(krate.name);
                            crates_visited.insert(krate.name);

                            // Insert crate into lookup maps
                            new_crates.push(krate);
                        }
                    }

                    for krate in new_crates {
                        crates.insert(krate.name, krate);
                    }
                }

                Some(crates)
            }
            Application::Inline { code } => {
                let krate = self
                    .parse_inline_crate(code)
                    .ok_or(self.diagnostics.clone())?;
                Some(StrMap::from([(krate.name, krate)]))
            }
        };
        crates.ok_or(self.diagnostics.clone())
    }

    fn parse_inline_crate(&mut self, code: String) -> Option<Crate> {
        let tokens = tokenize(&mut self.compiler_ctxt.string_interner, code);
        let TokenizedSource {
            tokens,
            line_map,
            token_source,
        } = tokens;

        let mut ast = parse(
            &mut self.compiler_ctxt.string_interner,
            &mut self.compiler_ctxt.diagnostics,
            &mut self.compiler_ctxt.id_generator,
            tokens,
        )?;
        let module_name = self.compiler_ctxt.intern_str("module");
        let module_path = ModulePath::from_iter([module_name]);

        let krate_name = self.compiler_ctxt.intern_str("crate");

        let crate_id = self.compiler_ctxt.id_generator.crate_id();
        let mut krate = Crate::new(krate_name, crate_id);

        ast.path = module_path.clone();
        let module_id = krate.add_module(module_path, ast);

        let source = SourceCode::new(token_source, line_map);
        self.source_map.intern(module_id, source);

        Some(krate)
    }

    pub(crate) fn parse_crate(&mut self, path: &Path) -> Option<Crate> {
        let krate_name: OsString =
            path.file_name()
                .map(|path| path.to_os_string())
                .or_else(|| {
                    self.compiler_ctxt
                        .diagnostics
                        .push(Diagnostic::Fatal(FatalError::InvalidOsStr));
                    None
                })?;

        let krate_name = krate_name
            .to_str()
            .map(|str| self.compiler_ctxt.intern_str(str))
            .or_else(|| {
                self.compiler_ctxt
                    .diagnostics
                    .push(Diagnostic::Fatal(FatalError::InvalidOsStr));
                None
            })?;

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

        let crate_id = self.compiler_ctxt.id_generator.crate_id();
        let mut krate = Crate::new(krate_name, crate_id);

        for file in code_files {
            let local_path = local_to_root(file.path(), path)
                .map_err(|err| {
                    self.compiler_ctxt
                        .diagnostics
                        .push(Diagnostic::Fatal(FatalError::InvalidOsStr));
                })
                .ok()?;
            let module_path = self.compiler_ctxt.module_path(local_path)?;

            let tokens = tokenize_file(
                &mut self.compiler_ctxt.string_interner,
                &mut self.compiler_ctxt.diagnostics,
                &file.into_path(),
            )?;
            let TokenizedSource {
                tokens,
                line_map,
                token_source,
            } = tokens;
            let mut module = parse(
                &mut self.compiler_ctxt.string_interner,
                &mut self.compiler_ctxt.diagnostics,
                &mut self.compiler_ctxt.id_generator,
                tokens,
            )?;

            module.path = module_path.clone();
            let module_id = krate.add_module(module_path, module);

            self.source_map
                .intern(module_id, SourceCode::new(token_source, line_map));
        }

        Some(krate)
    }

    pub fn validate_crates(&mut self, crates: StrMap<Crate>) -> Result<StrMap<Crate>, Diagnostics> {
        for krate in crates.values() {
            for module in krate.modules() {
                validate(
                    &self.string_interner,
                    &mut self.diagnostics,
                    &self.source_map,
                    module,
                );
            }
        }
        self.check_errors(crates)
    }

    pub fn resolve_crates(&mut self, crates: StrMap<Crate>) -> Result<HirMap, Diagnostics> {
        resolve(&self.compiler_ctxt, crates).ok_or(self.diagnostics.clone())
    }

    pub fn infer_types(&mut self, hir_map: HirMap) -> Result<StrMap<TypeMap>, Diagnostics> {
        let mut ty_map = StrMap::default();
        for krate in hir_map.krates() {
            if let Some(inferred_tys) =
                CrateInference::new(&mut self.compiler_ctxt, krate, &hir_map).infer_tys()
            {
                ty_map.insert(krate.name, inferred_tys);
            }
        }
        self.check_errors(ty_map)
    }

    fn check_errors<T>(&mut self, val: T) -> Result<T, Diagnostics> {
        if self
            .diagnostics
            .filter(DiagnosticKind::Error)
            .next()
            .is_some()
        {
            return Err(self.diagnostics.clone());
        }
        Ok(val)
    }
}

fn local_to_root(path: &Path, root: &Path) -> Result<PathBuf, StripPrefixError> {
    path.strip_prefix(root).map(|path| path.with_extension(""))
}

mod tests {
    use krate::Crate;
    use snap::snapshot;

    use crate::compiler::compiler::Compiler;
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
