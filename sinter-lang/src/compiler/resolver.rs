use crate::compiler::ast::{ArrayExpr, AstPass, ClassStmt, EnumStmt, Expr, ExprKind, FnStmt, GlobalLetStmt, Ident, ItemKind, Module, TraitImplStmt, TraitStmt, Ty, UseStmt};
use crate::compiler::ast_passes::NameCollector;
use crate::compiler::compiler::CompileError;
use crate::compiler::hir::{DefId, HirItem, HirItemKind, LocalDefId};
use crate::compiler::krate::{Crate, ResolvedCrate};
use crate::compiler::path::ModulePath;
use crate::compiler::types::types::InternedStr;
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::ops::Deref;
use crate::compiler::tokens::tokenized_file::Span;

pub fn resolve(crates: HashMap<InternedStr, Crate>) -> Result<Vec<ResolvedCrate>, CompileError> {
    let mut resolver = Resolver::new(crates);
    resolver.resolve()
}

#[derive(Default)]
struct Scope {
    idents: HashMap<InternedStr, DefId>,
}

impl Deref for Scope {
    type Target = HashMap<InternedStr, DefId>;

    fn deref(&self) -> &Self::Target {
        &self.idents
    }
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

type ResolveResult = Result<(), ResolveError>;

#[derive(Default)]
struct Resolver {
    crates: HashMap<InternedStr, Crate>,
    resolved_crates: Vec<ResolvedCrate>,
    resolve_errors: Vec<ResolveError>,
    crate_resolver: CrateResolver,
}

impl Resolver {
    fn new(crates: HashMap<InternedStr, Crate>) -> Self {
        Self {
            crates,
            resolved_crates: Default::default(),
            resolve_errors: Default::default(),
            crate_resolver: Default::default(),
        }
    }

    fn process_crate(&mut self, krate: &Crate) -> Result<ResolvedCrate, Vec<ResolveError>> {
        let mut items = Vec::new();
        let mut errors = Vec::new();
        for module in krate.module_lookup.values() {
            for item in module.items {
                let span = item.span;
                let id = item.id;
                let resolved_items = match item.kind {
                    ItemKind::Use(use_stmt) => self.resolve_use_stmt(krate, use_stmt, span, id),
                    ItemKind::GlobalLet(let_stmt) => self.resolve_global_let_stmt(krate, let_stmt, span, id),
                    ItemKind::Class(class_stmt) => self.resolve_class_stmt(class_stmt),
                    ItemKind::Enum(enum_stmt) => self.resolve_enum_stmt(enum_stmt),
                    ItemKind::Trait(trait_stmt) => self.resolve_trait_stmt(trait_stmt),
                    ItemKind::TraitImpl(trait_impl_stmt) => {
                        self.resolve_trait_impl_stmt(trait_impl_stmt)
                    }
                    ItemKind::Fn(fn_stmt) => self.resolve_fn_stmt(fn_stmt),
                };
                match resolved_items {
                    Ok(resolved_items) => items.push(resolved_items),
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
        if !resolve_errors.is_empty() {
            Err(CompileError::ResolveErrors(resolve_errors))
        } else {
            Ok(krates)
        }
    }

    /// Use stmts are resolved to the corresponding crate definition and are not preserved past resolving.
    fn resolve_use_stmt(
        &mut self,
        krate: &Crate,
        use_stmt: UseStmt,
        span: Span,
        id: LocalDefId,
    ) -> ResolveResult {
        match use_stmt {
            UseStmt::Crate { mod_path, item } => {
                let def_id = id.to_def_id(krate.id.into());
                self.crate_resolver.crate_scope.insert(item.ident, def_id);
            }
            UseStmt::Global {
                krate,
                mod_path,
                item,
            } => {
                let krate = self.crates.get(&krate.ident).unwrap(); // Should be safe, if crate isn't found it will throw an error earlier
                let def_id = krate
                    .find_definition(&mod_path.into(), item.ident)
                    .ok_or_else(|| ResolveError::DefinitionNotFound)?;
                self.crate_resolver.crate_scope.insert(item.ident, def_id);
            }
        };
        Ok(())
    }

    fn resolve_global_let_stmt(
        &mut self,
        krate: &Crate,
        let_stmt: GlobalLetStmt,
        span: Span,
        id: LocalDefId,
    ) -> ResolveResult {
        // Lower expression
        self.lower_expr(expr)?;
        
        self.crate_resolver.crate_scope.insert(
            let_stmt.ident.ident,
            let_stmt.ident.id.to_def_id(krate.id.into()),
        );
        Ok(())
    }

    fn resolve_class_stmt(&mut self, class_stmt: ClassStmt) -> ResolveResult {
        todo!()
    }

    fn resolve_enum_stmt(&mut self, enum_stmt: EnumStmt) -> ResolveResult {
        todo!()
    }

    fn resolve_trait_stmt(&mut self, trait_stmt: TraitStmt) -> ResolveResult {
        todo!()
    }

    fn resolve_trait_impl_stmt(
        &mut self,
        trait_stmt: TraitImplStmt,
    ) -> ResolveResult {
        todo!()
    }

    fn resolve_fn_stmt(&mut self, fn_stmt: FnStmt) -> ResolveResult {
        todo!()
    }
    
    fn lower_expr(&mut self, krate: &Crate, expr: Expr) -> ResolveResult {
        let span = expr.span;
        let id = expr.id;
        match expr.kind {
            ExprKind::Array(array_expr) => {
                match array_expr {
                    ArrayExpr::SizedInitializer(initializer, size) => {
                        
                    }
                    ArrayExpr::Initializer(initializer) => {
                        
                    }
                }
            }
            ExprKind::Call(_) => {}
            ExprKind::Constructor(_) => {}
            ExprKind::Infix(_) => {}
            ExprKind::Unary(_) => {}
            ExprKind::None => {}
            ExprKind::Boolean(_) => {}
            ExprKind::Integer(_) => {}
            ExprKind::Float(_) => {}
            ExprKind::String(_) => {}
            ExprKind::Match(_) => {}
            ExprKind::Closure(_) => {}
            ExprKind::Assign(_) => {}
            ExprKind::Field(_) => {}
            ExprKind::Index(_) => {}
            ExprKind::Path(_) => {}
            ExprKind::Parentheses(_) => {}
            ExprKind::Break => {}
            ExprKind::Continue => {}
        }
        todo!()
    }
}

#[derive(Default)]
struct CrateResolver {
    items: Vec<HirItem>,
    errors: Vec<ResolveError>,
    crate_scope: Scope,
    inner_scopes: Vec<Scope>,
}
