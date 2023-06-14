use crate::compiler::ast::{
    ArrayExpr as AstArrayExpr, AstPass, Block as AstBlock, Block, ClassStmt as AstClassStmt,
    ClosureParam as AstClosureParam, EnumStmt as AstEnumStmt, ExprKind as AstExprKind,
    FnSelfStmt as AstFnSelfStmt, FnSig as AstFnSig, FnStmt as AstFnStmt, Generics as AstGenerics,
    GlobalLetStmt, Ident, Item as AstItem, ItemKind as AstItemKind, MatchArm as AstMatchArm,
    Module, Pattern as AstPattern, Stmt as AstStmt, TraitImplStmt, TraitStmt, Ty, UseStmt,
};
use crate::compiler::ast::{Expr as AstExpr, Field as AstField};
use crate::compiler::ast_passes::NameCollector;
use crate::compiler::compiler::CompileError;
use crate::compiler::hir;
use crate::compiler::hir::{
    Args, ArrayExpr, AssignExpr, CallExpr, ClassStmt, ClosureExpr, ClosureParam, ClosureParams,
    DefId, Expr, Field, FieldExpr, Fields, FnSig, FnStmt, FnStmts, GenericParams, Generics,
    HirCrate, HirItem, HirNode, HirNodeKind, IndexExpr, InfixExpr, LocalDefId, MatchArm, MatchExpr,
    PathExpr, Pattern, Segment, Stmt, UnaryExpr,
};
use crate::compiler::krate::Crate;
use crate::compiler::path::ModulePath;
use crate::compiler::tokens::tokenized_file::Span;
use crate::compiler::types::types::InternedStr;
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::ops::Deref;

pub fn resolve(crates: HashMap<InternedStr, Crate>) -> Result<Vec<HirCrate>, CompileError> {
    let mut resolver = Resolver::new(crates);
    resolver.resolve()
}

#[derive(Default)]
struct TyScope {
    tys: HashMap<InternedStr, DefId>,
}

impl TyScope {
    pub fn insert(&mut self, name: InternedStr, definition: DefId) -> ResolveResult {
        match self.tys.insert(name, definition) {
            None => Ok(()),
            Some(existing) => Err(ResolveError::DuplicateDefinition {
                existing,
                new: definition,
            }),
        }
    }
}

#[derive(Default)]
struct Scope {
    variables: HashMap<InternedStr, DefId>,
    generics: HashMap<InternedStr, DefId>,
}

impl Scope {
    fn insert_var(&mut self, var: InternedStr, def: DefId) -> Result<(), ResolveError> {
        match self.variables.insert(var, def) {
            None => Ok(()),
            Some(existing) => Err(ResolveError::DuplicateDefinition { existing, new: def }),
        }
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
    hir_crates: Vec<HirCrate>,
    errors: Vec<ResolveError>,
}

impl Resolver {
    fn new(crates: HashMap<InternedStr, Crate>) -> Self {
        Self {
            crates,
            hir_crates: Default::default(),
            errors: Default::default(),
        }
    }

    fn resolve(mut self) -> Result<Vec<HirCrate>, CompileError> {
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
    items: Vec<LocalDefId>,
    nodes: HashMap<LocalDefId, HirNode>,
    ty_scope: TyScope,
    inner_scopes: Vec<Scope>,
}

impl<'a> CrateResolver<'a> {
    fn new(krate: &'a Crate, krates: &'a HashMap<InternedStr, Crate>) -> Self {
        Self {
            krate,
            krates,
            items: Default::default(),
            nodes: Default::default(),
            ty_scope: Default::default(),
            inner_scopes: Default::default(),
        }
    }

    fn resolve(mut self) -> Result<HirCrate, Vec<ResolveError>> {
        let mut errors = Vec::new();
        for module in self.krate.module_lookup.values() {
            for item in module.items {
                let span = item.span;
                let id = item.id;

                let result = match item.kind {
                    AstItemKind::Use(use_stmt) => self.resolve_use_stmt(use_stmt, span, id),
                    AstItemKind::GlobalLet(let_stmt) => {
                        self.resolve_global_let_stmt(let_stmt, span, id)
                    }
                    AstItemKind::Class(class_stmt) => self.resolve_class_stmt(class_stmt, span, id),
                    AstItemKind::Enum(enum_stmt) => self.resolve_enum_stmt(enum_stmt, span, id),
                    AstItemKind::Trait(trait_stmt) => self.resolve_trait_stmt(trait_stmt, span, id),
                    AstItemKind::TraitImpl(trait_impl_stmt) => {
                        self.resolve_trait_impl_stmt(trait_impl_stmt, span, id)
                    }
                    AstItemKind::Fn(fn_stmt) => self.resolve_fn_stmt(fn_stmt, span, id),
                };
                if let Err(error) = result {
                    errors.push(error);
                }
            }
        }
        if !errors.is_empty() {
            Err(errors)
        } else {
            Ok(HirCrate::new(
                self.krate.name,
                self.krate.id,
                self.items,
                self.nodes,
            ))
        }
    }

    fn create_scope(&mut self) {
        self.inner_scopes.push(Scope::default());
    }

    fn end_scope(&mut self) {
        self.inner_scopes.pop();
    }

    fn resolve_use_stmt(&mut self, use_stmt: UseStmt, span: Span, id: LocalDefId) -> ResolveResult {
        match use_stmt {
            UseStmt::Crate { mod_path, item } => {
                let def_id = id.to_def_id(self.krate.id.into());
                self.ty_scope.insert(item.ident, def_id)?;
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
                self.ty_scope.insert(item.ident, def_id)?;
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
        let expr_id = self.resolve_expr(let_stmt.initializer)?;

        self.ty_scope.insert(
            let_stmt.ident.ident,
            expr_id.to_def_id(self.krate.id.into()),
        )
    }

    fn resolve_class_stmt(
        &mut self,
        class_stmt: AstClassStmt,
        span: Span,
        id: LocalDefId,
    ) -> ResolveResult {
        self.ty_scope
            .insert(class_stmt.name.ident, id.to_def_id(self.krate.id.into()))?;

        let generic_params = self.resolve_generic_params(&class_stmt)?;
        let fields = self.resolve_fields(&class_stmt)?;
        let fn_stmts = self.resolve_fn_self_stmts(&class_stmt)?;

        let item = HirItem::Class(ClassStmt::new(
            class_stmt.name,
            class_stmt.class_type,
            generic_params,
            fields,
            fn_stmts,
        ));

        self.items.push(id);
        self.nodes
            .insert(id, HirNode::new(HirNodeKind::Item(item), span, id));

        todo!()
    }

    fn resolve_generic_params(
        &mut self,
        class_stmt: &AstClassStmt,
    ) -> Result<GenericParams, ResolveError> {
        todo!()
    }

    fn resolve_fields(&mut self, class_stmt: &AstClassStmt) -> Result<Fields, ResolveError> {
        let mut fields = Fields::default();
        for field in class_stmt.fields {
            let AstField {
                ident,
                ty,
                span,
                id,
            } = field;
            let resolved_ty = self.resolve_ty(ty)?;
            self.nodes.insert(
                id,
                HirNode::new(HirNodeKind::Field(Field::new(ident, resolved_ty)), span, id),
            );
            fields.insert(ident.ident, id);
        }
        Ok(fields)
    }

    fn resolve_enum_stmt(
        &mut self,
        enum_stmt: AstEnumStmt,
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

    fn resolve_fn_stmt(&mut self, fn_stmt: AstFnStmt, span: Span, id: LocalDefId) -> ResolveResult {
        todo!()
    }

    fn resolve_fn_self_stmt(
        &mut self,
        fn_stmt: AstFnSelfStmt,
    ) -> Result<(InternedStr, LocalDefId), ResolveError> {
        let AstFnSelfStmt {
            sig,
            body,
            span,
            id,
        } = fn_stmt;
        let resolved_body = match body {
            None => None,
            Some(block) => Some(self.resolve_block(block)?),
        };
        let resolved_sig = self.resolve_fn_sig(sig)?;
        let name = resolved_sig.name.ident;
        self.nodes.insert(
            id,
            HirNode::new(
                HirNodeKind::Fn(FnStmt::new(resolved_sig, resolved_body)),
                span,
                id,
            ),
        );
        Ok((name, id))
    }

    fn resolve_fn_self_stmts(
        &mut self,
        class_stmt: &AstClassStmt,
    ) -> Result<FnStmts, ResolveError> {
        let mut fn_stmts = FnStmts::default();
        for fn_stmt in class_stmt.fn_stmts {
            let (name, stmt) = self.resolve_fn_self_stmt(fn_stmt)?;
            fn_stmts.insert(name, stmt);
        }
        Ok(fn_stmts)
    }

    fn resolve_fn_sig(&mut self, fn_sig: AstFnSig) -> Result<FnSig, ResolveError> {
        todo!()
    }

    fn resolve_field(&mut self, field: AstField) -> Result<LocalDefId, ResolveError> {
        let ty = self.resolve_ty(field.ty);

        todo!()
    }

    fn resolve_expr(&mut self, expr: AstExpr) -> Result<LocalDefId, ResolveError> {
        let span = expr.span;
        let id = expr.id;
        let resolved_expr = match expr.kind {
            AstExprKind::Array(array_expr) => match array_expr {
                AstArrayExpr::SizedInitializer(initializer, size) => {
                    let initializer = self.resolve_expr(*initializer)?;
                    let size = self.resolve_expr(*size)?;

                    Expr::Array(ArrayExpr::Sized { initializer, size })
                }
                AstArrayExpr::Initializer(initializers) => {
                    let initializers = initializers
                        .into_iter()
                        .map(|expr| self.resolve_expr(expr))
                        .collect::<Result<Vec<LocalDefId>, ResolveError>>()?;

                    Expr::Array(ArrayExpr::Unsized { initializers })
                }
            },
            AstExprKind::Call(call) => {
                let args = call
                    .args
                    .into_iter()
                    .map(|expr| self.resolve_expr(expr))
                    .collect::<Result<Vec<LocalDefId>, ResolveError>>()?;
                let target = self.resolve_expr(*call.target)?;

                Expr::Call(CallExpr::new(target, Args::new(args)))
            }
            AstExprKind::Constructor(constructor) => {
                let args = constructor
                    .args
                    .into_iter()
                    .map(|expr| self.resolve_expr(expr))
                    .collect::<Result<Vec<LocalDefId>, ResolveError>>()?;
                let target = self.resolve_expr(*constructor.target)?;

                Expr::Constructor(CallExpr::new(target, Args::new(args)))
            }
            AstExprKind::Infix(infix) => {
                let lhs = self.resolve_expr(*infix.lhs)?;
                let rhs = self.resolve_expr(*infix.rhs)?;

                Expr::Infix(InfixExpr::new(infix.operator, lhs, rhs))
            }
            AstExprKind::Unary(unary) => {
                let expr = self.resolve_expr(*unary.expr)?;

                Expr::Unary(UnaryExpr::new(unary.operator, expr))
            }
            AstExprKind::None => Expr::None,
            AstExprKind::Boolean(boolean) => Expr::Boolean(boolean),
            AstExprKind::Integer(integer) => Expr::Integer(integer),
            AstExprKind::Float(float) => Expr::Float(float),
            AstExprKind::String(string) => Expr::String(string),
            AstExprKind::Match(match_expr) => {
                let source = self.resolve_expr(*match_expr.source)?;
                let arms = match_expr
                    .arms
                    .into_iter()
                    .map(|arm| self.resolve_match_arm(arm))
                    .collect::<Result<Vec<MatchArm>, ResolveError>>()?;

                Expr::Match(MatchExpr::new(source, arms))
            }
            AstExprKind::Closure(closure) => {
                let params = self.resolve_closure_params(closure.params)?;
                let stmt = self.resolve_stmt(closure.stmt)?;
                Expr::Closure(ClosureExpr::new(params, stmt))
            }
            AstExprKind::Assign(assign) => {
                let lhs = self.resolve_expr(*assign.lhs)?;
                let rhs = self.resolve_expr(*assign.rhs)?;
                Expr::Assign(AssignExpr::new(lhs, rhs))
            }
            AstExprKind::Field(field) => {
                let lhs = self.resolve_expr(*field.lhs)?;
                Expr::Field(FieldExpr::new(lhs, field.ident))
            }
            AstExprKind::Index(index) => {
                let expr = self.resolve_expr(*index.expr)?;
                let key = self.resolve_expr(*index.key)?;
                Expr::Index(IndexExpr::new(expr, key))
            }
            AstExprKind::Path(path) => {
                let mut segments = Vec::with_capacity(path.segments.len());
                for segment in path.segments {
                    let ident = segment.ident;
                    let generics = match segment.generics {
                        None => None,
                        Some(generics) => {
                            let mut resolved_generics = Generics::with_capacity(generics.len());
                            for generic in generics {
                                resolved_generics.push(self.resolve_ty(generic)?);
                            }
                            Some(resolved_generics)
                        }
                    };
                    segments.push(Segment::new(ident, generics))
                }
                Expr::Path(PathExpr::new(segments))
            }
            AstExprKind::Parentheses(parentheses) => {
                // Special logic for stripping parenthese (since they are just for pretty printing)
                let resolved_expr = self.resolve_expr(*parentheses.expr)?;
                return Ok(id);
            }
            AstExprKind::Break => Expr::Break,
            AstExprKind::Continue => Expr::Continue,
        };

        self.nodes
            .insert(id, HirNode::new(HirNodeKind::Expr(resolved_expr), span, id));
        Ok(id)
    }

    fn resolve_ty(&mut self, ty: Ty) -> Result<LocalDefId, ResolveError> {
        todo!()
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

    fn resolve_block(&mut self, block: AstBlock) -> Result<LocalDefId, ResolveError> {
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

    fn resolve_closure_params(
        &mut self,
        params: Vec<AstClosureParam>,
    ) -> Result<ClosureParams, ResolveError> {
        // TODO: Add vars to scope
        let mut params = params
            .into_iter()
            .map(|param| (param.ident.ident, ClosureParam::new(param.ident)))
            .collect();
        Ok(ClosureParams::new(params))
    }
}
