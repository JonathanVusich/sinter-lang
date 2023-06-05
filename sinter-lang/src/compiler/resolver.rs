use crate::compiler::ast::{AstPass, ClassStmt, EnumStmt, FnStmt, GlobalLetStmt, Ident, ItemKind, Module, TraitImplStmt, TraitStmt, Ty, UseStmt};
use crate::compiler::ast_passes::NameCollector;
use crate::compiler::compiler::ResolveError;
use crate::compiler::hir::{DefId, HirItem, LocalDefId};
use crate::compiler::krate::{Crate, ResolvedCrate};
use crate::compiler::types::types::InternedStr;
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use crate::compiler::path::ModulePath;

pub fn resolve(crates: HashMap<InternedStr, Crate>) -> Result<Vec<ResolvedCrate>, ResolveError> {
    let mut resolver = Resolver::new(crates);
    resolver.resolve()
}

#[derive(Default)]
struct Scope {
    idents: HashSet<InternedStr>,
}

#[derive(Debug)]
pub enum ResolveError {
    ModuleNotFound,
}

impl Display for ResolveError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl Error for ResolveError { }

#[derive(Default)]
struct Namespace {
    items: HashMap<ModulePath, HashMap<InternedStr, VisibleItem>>,
}

enum VisibleItem {
    Fn(DefId),
    Enum(DefId),
    Class(DefId),
    Trait(DefId),
    TraitImpl(DefId),
    Constant(DefId),
}

#[derive(Default)]
struct Resolver {
    namespace: Namespace,
    crates: HashMap<InternedStr, Crate>,
    scopes: Vec<Scope>,
}

impl Resolver {
    fn new(crates: HashMap<InternedStr, Crate>) -> Self {
        Self {
            namespace: Default::default(),
            crates,
            scopes: Default::default(),
        }
    }

    fn resolve(&mut self) -> Result<Vec<ResolvedCrate>, ResolveError> {
        let mut errors = Vec::new();
        for krate in self.crates.values() {
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
                        Ok(_) => {},
                        Err(err) => errors.push(err),
                    }
                }
            }
        }

        Ok(Vec::new())
    }

    fn resolve_use_stmt(&mut self, krate: &Crate, use_stmt: UseStmt) -> Result<HirItem, ResolveError> {
        match use_stmt {
            UseStmt::Crate(ident) => {
                let mut ident = Vec::with_capacity(ident.len());
                ident.push(krate.name);
                ident.extend(ident[0..ident.len() - 1].iter());
                

                // TODO: Improve error contents
                let item = self.namespace.items.get(&ident.into())
                    .ok_or_else(||ResolveError::ModuleNotFound)?
                    .get();
                let module = krate.module_lookup.get(&ident.into()).ok_or_else(|| ResolveError::ModuleNotFound)?;
            }
            UseStmt::Global(ident) => {
                let krate = self.crates.get(&ident.first().ident).unwrap();
            }
        }
        let krate = self.crates.get(use_stmt.)
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
