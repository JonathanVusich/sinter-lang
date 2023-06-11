use crate::compiler::ast::Expr as AstExpr;
use crate::compiler::ast::{
    ArrayExpr as AstArrayExpr, AstPass, ClassStmt as AstClassStmt, EnumStmt, ExprKind, FnStmt,
    GlobalLetStmt, Ident, Item, ItemKind, MatchArm as AstMatchArm, Module, Pattern as AstPattern,
    Stmt as AstStmt, TraitImplStmt, TraitStmt, Ty, UseStmt,
};
use crate::compiler::ast_passes::NameCollector;
use crate::compiler::compiler::CompileError;
use crate::compiler::hir;
use crate::compiler::hir::{
    ArrayExpr, CallExpr, ClassStmt, DefId, Expr, HirItem, HirItemKind, InfixExpr, LocalDefId,
    MatchArm, MatchExpr, Pattern, Stmt, UnaryExpr,
};
use crate::compiler::krate::{Crate, ResolvedCrate};
use crate::compiler::path::ModulePath;
use crate::compiler::tokens::tokenized_file::Span;
use crate::compiler::types::types::InternedStr;
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::ops::Deref;

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
    /// Error for duplicate definitions
    DuplicateDefinition { existing: DefId, new: DefId },
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
    errors: Vec<ResolveError>,
}

impl Resolver {
    fn new(crates: HashMap<InternedStr, Crate>) -> Self {
        Self {
            crates,
            resolved_crates: Default::default(),
            errors: Default::default(),
        }
    }

    fn resolve(mut self) -> Result<Vec<ResolvedCrate>, CompileError> {
        let mut krates = Vec::new();
        let mut resolve_errors = Vec::new();
        for krate in self.crates.values() {
            let crate_resolver = CrateResolver::new(krate, &self.crates);
            match crate_resolver.resolve() {
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
}

struct CrateResolver<'a> {
    krate: &'a Crate,
    krates: &'a HashMap<InternedStr, Crate>,
    items: Vec<HirItem>,
    crate_scope: Scope,
    inner_scopes: Vec<Scope>,
}

impl<'a> CrateResolver<'a> {
    fn new(krate: &'a Crate, krates: &'a HashMap<InternedStr, Crate>) -> Self {
        Self {
            krate,
            krates,
            items: Default::default(),
            crate_scope: Default::default(),
            inner_scopes: Default::default(),
        }
    }

    fn resolve(mut self) -> Result<ResolvedCrate, Vec<ResolveError>> {
        let mut errors = Vec::new();
        for module in self.krate.module_lookup.values() {
            for item in module.items {
                let span = item.span;
                let id = item.id;

                let result = match item.kind {
                    ItemKind::Use(use_stmt) => self.resolve_use_stmt(use_stmt, span, id),
                    ItemKind::GlobalLet(let_stmt) => {
                        self.resolve_global_let_stmt(let_stmt, span, id)
                    }
                    ItemKind::Class(class_stmt) => self.resolve_class_stmt(class_stmt, span, id),
                    ItemKind::Enum(enum_stmt) => self.resolve_enum_stmt(enum_stmt, span, id),
                    ItemKind::Trait(trait_stmt) => self.resolve_trait_stmt(trait_stmt, span, id),
                    ItemKind::TraitImpl(trait_impl_stmt) => {
                        self.resolve_trait_impl_stmt(trait_impl_stmt, span, id)
                    }
                    ItemKind::Fn(fn_stmt) => self.resolve_fn_stmt(fn_stmt, span, id),
                };
                if let Err(error) = result {
                    errors.push(error);
                }
            }
        }
        if !errors.is_empty() {
            Err(errors)
        } else {
            Ok(ResolvedCrate::new(
                self.krate.name,
                self.krate.id,
                self.items,
            ))
        }
    }

    fn insert_crate_namespace(&mut self, ident: &Ident, id: DefId) -> ResolveResult {
        if let Some(existing) = self.crate_scope.insert(ident.ident, id) {
            Err(ResolveError::DuplicateDefinition { existing, new: id })
        } else {
            Ok(())
        }
    }

    fn resolve_use_stmt(&mut self, use_stmt: UseStmt, span: Span, id: LocalDefId) -> ResolveResult {
        match use_stmt {
            UseStmt::Crate { mod_path, item } => {
                let def_id = id.to_def_id(self.krate.id.into());
                self.crate_scope.insert(item.ident, def_id);
            }
            UseStmt::Global {
                krate,
                mod_path,
                item,
            } => {
                let krate = self.krates.get(&krate.ident).unwrap(); // Should be safe, if crate isn't found it will throw an error earlier
                let def_id = krate
                    .find_definition(&mod_path.into(), item.ident)
                    .ok_or_else(|| ResolveError::DefinitionNotFound)?;
                self.crate_scope.insert(item.ident, def_id);
            }
        };
        Ok(())
    }

    fn resolve_global_let_stmt(
        &mut self,
        let_stmt: GlobalLetStmt,
        span: Span,
        id: LocalDefId,
    ) -> ResolveResult {
        // Lower expression
        self.resolve_expr(let_stmt.initializer)?;

        self.crate_scope.insert(
            let_stmt.ident.ident,
            let_stmt.ident.id.to_def_id(self.krate.id.into()),
        );
        Ok(())
    }

    fn resolve_class_stmt(
        &mut self,
        class_stmt: AstClassStmt,
        span: Span,
        id: LocalDefId,
    ) -> ResolveResult {
        self.insert_crate_namespace(&class_stmt.name, id.to_def_id(self.krate.id.into()))?;
        for fn_stmt in class_stmt.fn_stmts {}
        todo!()
    }

    fn resolve_enum_stmt(
        &mut self,
        enum_stmt: EnumStmt,
        span: Span,
        id: LocalDefId,
    ) -> ResolveResult {
        todo!()
    }

    fn resolve_trait_stmt(
        &mut self,
        trait_stmt: TraitStmt,
        span: Span,
        id: LocalDefId,
    ) -> ResolveResult {
        todo!()
    }

    fn resolve_trait_impl_stmt(
        &mut self,
        trait_stmt: TraitImplStmt,
        span: Span,
        id: LocalDefId,
    ) -> ResolveResult {
        todo!()
    }

    fn resolve_fn_stmt(&mut self, fn_stmt: FnStmt, span: Span, id: LocalDefId) -> ResolveResult {
        todo!()
    }

    fn resolve_expr(&mut self, expr: AstExpr) -> Result<LocalDefId, ResolveError> {
        let span = expr.span;
        let id = expr.id;
        let resolved_expr = match expr.kind {
            ExprKind::Array(array_expr) => match array_expr {
                AstArrayExpr::SizedInitializer(initializer, size) => {
                    let initializer = self.resolve_expr(*initializer)?;
                    let size = self.resolve_expr(*size)?;

                    Expr::Array(ArrayExpr::Sized { initializer, size })
                }
                AstArrayExpr::Initializer(initializers) => {
                    let initializers = initializers
                        .into_iter()
                        .map(|expr| self.resolve_expr(expr))
                        .collect()?;

                    Expr::Array(ArrayExpr::Unsized { initializers })
                }
            },
            ExprKind::Call(call) => {
                let args = call
                    .args
                    .into_iter()
                    .map(|expr| self.resolve_expr(expr))
                    .collect()?;
                let target = self.resolve_expr(*call.target)?;

                Expr::Call(CallExpr::new(target, args))
            }
            ExprKind::Constructor(constructor) => {
                let args = constructor
                    .args
                    .into_iter()
                    .map(|expr| self.resolve_expr(expr))
                    .collect()?;
                let target = self.resolve_expr(*constructor.target)?;

                Expr::Constructor(CallExpr::new(target, args))
            }
            ExprKind::Infix(infix) => {
                let lhs = self.resolve_expr(*infix.lhs)?;
                let rhs = self.resolve_expr(*infix.rhs)?;

                Expr::Infix(InfixExpr::new(infix.operator, lhs, rhs))
            }
            ExprKind::Unary(unary) => {
                let expr = self.resolve_expr(*unary.expr)?;

                Expr::Unary(UnaryExpr::new(unary.operator, expr))
            }
            ExprKind::None => Expr::None,
            ExprKind::Boolean(boolean) => Expr::Boolean(boolean),
            ExprKind::Integer(integer) => Expr::Integer(integer),
            ExprKind::Float(float) => Expr::Float(float),
            ExprKind::String(string) => Expr::String(string),
            ExprKind::Match(match_expr) => {
                let source = self.resolve_expr(*match_expr.source)?;
                let arms = match_expr
                    .arms
                    .into_iter()
                    .map(|arm| self.resolve_match_arm(arm))
                    .collect()?;

                Expr::Match(MatchExpr::new(source, arms))
            }
            ExprKind::Closure(_) => {}
            ExprKind::Assign(_) => {}
            ExprKind::Field(_) => {}
            ExprKind::Index(_) => {}
            ExprKind::Path(_) => {}
            ExprKind::Parentheses(_) => {}
            ExprKind::Break => {}
            ExprKind::Continue => {}
        };

        self.items.push(HirItem::new(
            HirItemKind::Expr(resolved_expr),
            span,
            id.into(),
        ));
        Ok(id)
    }

    fn resolve_stmt(&mut self, stmt: AstStmt) -> Result<LocalDefId, ResolveError> {
        match stmt {
            AstStmt::Let(_) => {}
            AstStmt::For(_) => {}
            AstStmt::If(_) => {}
            AstStmt::Return(_) => {}
            AstStmt::While(_) => {}
            AstStmt::Block(_) => {}
            AstStmt::Expression(_) => {}
        }
        todo!()
    }

    fn resolve_match_arm(&mut self, arm: AstMatchArm) -> Result<MatchArm, ResolveError> {
        let body = self.resolve_stmt(arm.body)?;
        let pattern = self.resolve_pattern(arm.pattern)?;

        Ok(MatchArm::new(pattern, body))
    }

    fn resolve_pattern(&mut self, pattern: AstPattern) -> Result<LocalDefId, ResolveError> {
        todo!()
    }
}
