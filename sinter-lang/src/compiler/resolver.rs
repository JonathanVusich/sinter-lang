use crate::compiler::ast::{
    AstPass, ClassStmt, EnumStmt, FnStmt, GlobalLetStmt, Ident, ItemKind, Module, TraitImplStmt,
    TraitStmt, Ty, UseStmt,
};
use crate::compiler::ast_passes::NameCollector;
use crate::compiler::hir::{DefId, HirItem, HirItemKind, LocalDefId};
use crate::compiler::krate::{Crate, ResolvedCrate};
use crate::compiler::path::ModulePath;
use crate::compiler::types::types::InternedStr;
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use crate::compiler::compiler::CompileError;

pub fn resolve(crates: HashMap<InternedStr, Crate>) -> Result<Vec<ResolvedCrate>, CompileError> {
    let mut resolver = Resolver::new(crates);
    resolver.resolve()
}

#[derive(Default)]
struct Scope {
    idents: HashSet<InternedStr>,
}

#[derive(Debug)]
pub enum ResolveError {
    /// Error from looking up definition in crate
    DefinitionNotFound,
}

impl Display for ResolveError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl Error for ResolveError {}

type ResolveResult<T> = Result<T, Vec<ResolveError>>;

#[derive(Default)]
struct Resolver {
    crates: HashMap<InternedStr, Crate>,
    resolved_crates: Vec<ResolvedCrate>,
    resolve_errors: Vec<ResolveError>,
    scopes: Vec<Scope>,
}

impl Resolver {
    fn new(crates: HashMap<InternedStr, Crate>) -> Self {
        Self {
            crates,
            resolved_crates: Default::default(),
            resolve_errors: Default::default(),
            scopes: Default::default(),
        }
    }

    fn process_crate(&mut self, krate: &Crate) -> ResolveResult<ResolvedCrate> {
        let mut items = Vec::new();
        let mut errors = Vec::new();
        for module in krate.module_lookup.values() {
            for item in module.items {
                let span = item.span;
                let id = item.id;
                let item_kind = match item.kind {
                    ItemKind::Use(use_stmt) => self.resolve_use_stmt(krate, use_stmt),
                    ItemKind::GlobalLet(let_stmt) => self.resolve_global_let_stmt(let_stmt),
                    ItemKind::Class(class_stmt) => self.resolve_class_stmt(class_stmt),
                    ItemKind::Enum(enum_stmt) => self.resolve_enum_stmt(enum_stmt),
                    ItemKind::Trait(trait_stmt) => self.resolve_trait_stmt(trait_stmt),
                    ItemKind::TraitImpl(trait_impl_stmt) => {
                        self.resolve_trait_impl_stmt(trait_impl_stmt)
                    }
                    ItemKind::Fn(fn_stmt) => self.resolve_fn_stmt(fn_stmt),
                };
                match item_kind {
                    Ok(kind) => {

                    }
                    Err(err) => errors.push(err),
                }
            }
        }

        if !errors.is_empty() {
            Err(errors)
        } else {
            Ok(ResolvedCrate::new(krate.name, krate.id, items))
        }
    }

    fn resolve(mut self) -> Result<Vec<ResolvedCrate>, CompileError> {
        let mut krates = Vec::new();
        let mut resolve_errors = Vec::new();
        for krate in self.crates.values() {
            match self.process_crate(krate) {
                Ok(krate) => krates.push(krate),
                Err(errors) => resolve_errors.extend(errors),
            }
        }
        if !errors.is_empty() {
            Err(CompileError::ResolveErrors(errors))
        } else {
            Ok(krates)
        }
    }

    fn resolve_use_stmt(
        &mut self,
        krate: &Crate,
        use_stmt: UseStmt,
    ) -> Result<HirItemKind, ResolveError> {
        let local_id = match use_stmt {
            UseStmt::Crate { mod_path, item } => {
                krate.find_definition(&mod_path.into(), item.ident)
                    .ok_or_else(|| ResolveError::DefinitionNotFound)?
            }
            UseStmt::Global { krate, mod_path, item } => {
                let krate = self.crates.get(&krate.ident).unwrap(); // Should be safe, if crate isn't found it will throw an error earlier
                krate.find_definition(&mod_path.into(), item.ident)
                    .ok_or_else(|| ResolveError::DefinitionNotFound)?
            }
        };

        HirItemKind::Use

        todo!()
    }

    fn resolve_global_let_stmt(
        &mut self,
        let_stmt: GlobalLetStmt,
    ) -> Result<HirItem, ResolveError> {
        todo!()
    }

    fn resolve_class_stmt(&mut self, class_stmt: ClassStmt) -> Result<HirItem, ResolveError> {
        todo!()
    }

    fn resolve_enum_stmt(&mut self, enum_stmt: EnumStmt) -> Result<HirItem, ResolveError> {
        todo!()
    }

    fn resolve_trait_stmt(&mut self, trait_stmt: TraitStmt) -> Result<HirItem, ResolveError> {
        todo!()
    }

    fn resolve_trait_impl_stmt(
        &mut self,
        trait_stmt: TraitImplStmt,
    ) -> Result<HirItem, ResolveError> {
        todo!()
    }

    fn resolve_fn_stmt(&mut self, fn_stmt: FnStmt) -> Result<HirItem, ResolveError> {
        todo!()
    }
}
