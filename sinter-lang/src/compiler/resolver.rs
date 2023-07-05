use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;
use std::ops::Deref;

use crate::compiler::ast::{
    ArrayExpr as AstArrayExpr, AstPass, Block as AstBlock, ClassStmt as AstClassStmt,
    ClosureParam as AstClosureParam, EnumMember as AstEnumMember, EnumStmt as AstEnumStmt,
    ExprKind as AstExprKind, Fields as AstFields, FnSelfStmt as AstFnSelfStmt, FnSelfStmt,
    FnSig as AstFnSig, FnStmt as AstFnStmt, GenericParams as AstGenericParams,
    Generics as AstGenerics, GlobalLetStmt, Ident, IdentType, Item as AstItem, Item,
    ItemKind as AstItemKind, ItemKind, MatchArm as AstMatchArm, Module, Params as AstParams,
    PathTy as AstPathTy, Pattern as AstPattern, QualifiedIdent, Stmt as AstStmt,
    StmtKind as AstStmtKind, TraitBound as AstTraitBound, TraitImplStmt as AstTraitImplStmt,
    TraitStmt as AstTraitStmt, Ty as AstTy, TyKind, TyKind as AstTyKind, UseStmt,
};
use crate::compiler::ast::{Expr as AstExpr, Field as AstField};
use crate::compiler::ast_passes::NameCollector;
use crate::compiler::compiler::CompileError;
use crate::compiler::hir;
use crate::compiler::hir::HirItem::Trait;
use crate::compiler::hir::{
    Args, ArrayExpr, AssignExpr, Block, CallExpr, ClassStmt, ClosureExpr, ClosureParam,
    ClosureParams, DefId, EnumMembers, EnumStmt, Expr, Expression, Field, FieldExpr, Fields, FnSig,
    FnStmt, FnStmts, ForStmt, GenericParam, GenericParams, Generics, HirCrate, HirItem, HirNode,
    HirNodeKind, IndexExpr, InfixExpr, LetStmt, LocalDefId, MatchArm, MatchExpr, Param, Params,
    PathExpr, PathTy, Pattern, ReturnStmt, Segment, Stmt, Stmts, TraitBound, TraitStmt, Ty,
    UnaryExpr,
};
use crate::compiler::krate::{Crate, CrateAttributes, CrateNamespace, ModuleMap, ModuleNamespace};
use crate::compiler::path::ModulePath;
use crate::compiler::resolver::ResolveError::{DefinitionNotFound, DuplicateLocalDefIds};
use crate::compiler::tokens::tokenized_file::Span;
use crate::compiler::types::types::InternedStr;

pub fn resolve(crates: HashMap<InternedStr, Crate>) -> Result<Vec<HirCrate>, CompileError> {
    let resolver = Resolver::new(crates);
    resolver.resolve()
}

#[derive(Default)]
struct ModuleScope {
    items: HashMap<InternedStr, DefId>,
}

impl ModuleScope {
    pub(crate) fn insert(&mut self, ident: InternedStr, id: DefId) -> ResolveResult {
        match self.items.insert(ident, id) {
            None => Ok(()),
            Some(existing) => Err(ResolveError::DuplicateDefinition { existing, new: id }),
        }
    }
    pub(crate) fn resolve(&self, ident: InternedStr) -> Option<DefId> {
        self.items.get(&ident).copied()
    }
}

pub enum Scope {
    Class {
        fields: HashMap<InternedStr, LocalDefId>,
        self_fns: HashMap<InternedStr, LocalDefId>,
        generics: HashMap<InternedStr, LocalDefId>,
    },
    Enum {
        members: HashMap<InternedStr, LocalDefId>,
        self_fns: HashMap<InternedStr, LocalDefId>,
        generics: HashMap<InternedStr, LocalDefId>,
    },
    EnumMember {
        fields: HashMap<InternedStr, LocalDefId>,
        self_fns: HashMap<InternedStr, LocalDefId>,
    },
    Fn {
        params: HashMap<InternedStr, LocalDefId>,
        generics: HashMap<InternedStr, LocalDefId>,
        vars: HashMap<InternedStr, LocalDefId>,
    },
    Block {
        vars: HashMap<InternedStr, LocalDefId>,
    },
    Trait {
        generics: HashMap<InternedStr, LocalDefId>,
        self_fns: HashMap<InternedStr, LocalDefId>,
    }, // TODO: Add traits
}

impl Scope {
    pub fn contains_generic(&self, ident: InternedStr) -> Option<LocalDefId> {
        match self {
            Scope::Class { generics, .. } => generics.get(&ident).copied(),
            Scope::Enum { generics, .. } => generics.get(&ident).copied(),
            Scope::Fn { generics, .. } => generics.get(&ident).copied(),
            _ => None,
        }
    }

    pub fn insert_field(&mut self, ident: InternedStr, id: LocalDefId) -> Option<LocalDefId> {
        let fields = match self {
            Scope::Class { fields, .. } => fields,
            Scope::EnumMember { fields, .. } => fields,
            _ => panic!("Cannot insert field into this scope!"),
        };
        fields.insert(ident, id)
    }

    pub fn insert_var(&mut self, ident: InternedStr, id: LocalDefId) -> Option<LocalDefId> {
        match self {
            Scope::Fn { vars, .. } => vars.insert(ident, id),
            _ => panic!("Cannot insert var into this scope!"),
        }
    }

    pub fn insert_self_fn(&mut self, ident: InternedStr, id: LocalDefId) -> Option<LocalDefId> {
        let self_fns = match self {
            Scope::Class { self_fns, .. } => self_fns,
            Scope::Enum { self_fns, .. } => self_fns,
            Scope::EnumMember { self_fns, .. } => self_fns,
            _ => panic!("Cannot insert self fn into this scope!"),
        };
        self_fns.insert(ident, id)
    }

    pub fn insert_param(&mut self, ident: InternedStr, id: LocalDefId) -> Option<LocalDefId> {
        let params = match self {
            Scope::Fn { params, .. } => params,
            _ => panic!("Cannot insert param into this scope!"),
        };
        params.insert(ident, id)
    }

    pub fn insert_generic_param(
        &mut self,
        ident: InternedStr,
        id: LocalDefId,
    ) -> Option<LocalDefId> {
        let generics = match self {
            Scope::Class { generics, .. } => generics,
            Scope::Enum { generics, .. } => generics,
            Scope::Fn { generics, .. } => generics,
            _ => panic!("Cannot insert generic into this scope!"),
        };
        generics.insert(ident, id)
    }
}

#[derive(Debug)]
pub enum ResolveError {
    /// Error from looking up definition in crate
    DefinitionNotFound,
    /// Duplicate nodes created for a single local definition.
    DuplicateLocalDefIds,
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
}

impl Resolver {
    fn new(crates: HashMap<InternedStr, Crate>) -> Self {
        Self { crates }
    }

    fn resolve(self) -> Result<Vec<HirCrate>, CompileError> {
        let mut resolve_errors = Vec::new();
        let mut crates = Vec::new();

        // Generate crate item attributes to enable fast lookups for all items and their properties.
        let mut crate_item_attributes = HashMap::default();
        for krate in self.crates.values() {
            let crate_attrs = generate_crate_attrs(krate);
            match crate_attrs {
                Ok(crate_attrs) => {
                    crate_item_attributes.insert(krate.name, crate_attrs);
                }
                Err(errors) => {
                    resolve_errors.extend(errors);
                }
            }
        }

        for krate in self.crates.values() {
            let crate_resolver = CrateResolver::new(krate, &crate_item_attributes);
            match crate_resolver.resolve() {
                Ok(crate_map) => {
                    crates.push(crate_map);
                }
                Err(errors) => match errors {
                    CompileError::ResolveErrors(errors) => resolve_errors.extend(errors),
                    _ => panic!("Invalid error type!"),
                },
            }
        }
        if !resolve_errors.is_empty() {
            Err(CompileError::ResolveErrors(resolve_errors))
        } else {
            Ok(crates)
        }
    }
}

fn generate_crate_attrs(krate: &Crate) -> Result<CrateAttributes, Vec<ResolveError>> {
    let mut errors = Vec::new();
    let mut crate_attrs = CrateAttributes::default();

    for (path, module) in krate.module_lookup.iter() {
        for item in &module.items {
            match &item.kind {
                ItemKind::Use(use_stmt) => {}
                ItemKind::GlobalLet(_) => {}
                ItemKind::Class(_) => {}
                ItemKind::Enum(_) => {}
                ItemKind::Trait(_) => {}
                ItemKind::TraitImpl(_) => {}
                ItemKind::Fn(_) => {}
            }
        }
    }

    if !errors.is_empty() {
        Err(errors)
    } else {
        Ok(crate_attrs)
    }
}

#[derive(Default)]
struct CrateMap<'a> {
    items: HashMap<LocalDefId, &'a AstItem>,
}

struct CrateResolver<'a> {
    krate: &'a Crate,
    krate_attrs: &'a HashMap<InternedStr, CrateAttributes>,
    items: Vec<LocalDefId>,
    nodes: HashMap<LocalDefId, HirNode>,
    module_scope: ModuleScope,
    scopes: Vec<Scope>,
}

impl<'a> CrateResolver<'a> {
    fn new(krate: &'a Crate, krate_attrs: &'a HashMap<InternedStr, CrateAttributes>) -> Self {
        Self {
            krate,
            krate_attrs,
            items: Default::default(),
            nodes: Default::default(),
            module_scope: ModuleScope::default(),
            scopes: Default::default(),
        }
    }

    fn resolve(mut self) -> Result<HirCrate, CompileError> {
        let mut errors = Vec::new();
        for module in self.krate.module_lookup.values() {
            // First need to populate the module scope (from used items + module items)
            errors.extend(self.populate_module_scope(module));
            errors.extend(self.initial_module_resolution(module));
            // TODO: Impl trait resolution

            // We have to remember to clear out all scopes and reset the module scope
            self.module_scope = ModuleScope::default();
            self.scopes.clear();
        }
        if !errors.is_empty() {
            Err(CompileError::ResolveErrors(errors))
        } else {
            Ok(HirCrate::new(
                self.krate.name,
                self.krate.id,
                self.items,
                self.nodes,
            ))
        }
    }

    fn initial_module_resolution(mut self, module: &Module) -> Vec<ResolveError> {
        let mut errors = Vec::new();
        for item in &module.items {
            let span = item.span;
            let id = item.id;

            let result = match &item.kind {
                AstItemKind::Use(use_stmt) => Ok(()),
                AstItemKind::GlobalLet(let_stmt) => {
                    self.resolve_global_let_stmt(let_stmt, span, id)
                }
                AstItemKind::Class(class_stmt) => self.resolve_class_stmt(class_stmt, span, id),
                AstItemKind::Enum(enum_stmt) => self.resolve_enum_stmt(enum_stmt, span, id),
                AstItemKind::Trait(trait_stmt) => self.resolve_trait_stmt(trait_stmt, span, id),
                /// Purposefully empty because we have to wait until all types are resolved before we can check trait impl bodies.
                AstItemKind::TraitImpl(trait_impl_stmt) => Ok(()),
                AstItemKind::Fn(fn_stmt) => self.resolve_fn_stmt(fn_stmt, span, id),
            };

            match result {
                Err(error) => {
                    errors.push(error);
                }
                _ => {}
            }
        }
        errors
    }

    fn duplicate_definition(&self, existing: LocalDefId, new: LocalDefId) -> ResolveResult {
        Err(ResolveError::DuplicateDefinition {
            existing: existing.to_def_id(self.krate.id),
            new: new.to_def_id(self.krate.id),
        })
    }

    fn populate_module_scope(&mut self, module: &Module) -> Vec<ResolveError> {
        let mut errors = Vec::new();
        for item in &module.items {
            if let Some((ident, id)) = match &item.kind {
                ItemKind::Use(use_stmt) => {
                    let item = use_stmt.path.last();
                    match use_stmt.path.ident_type {
                        IdentType::Crate => Some((item.ident, item.id.to_def_id(self.krate.id))),
                        IdentType::LocalOrUse => {
                            let krate_ident = use_stmt.path.first();
                            let krate_namespace = self.krate_attrs.get(&krate_ident.ident).unwrap(); // Should be safe since the crate should exist

                            let definition = krate_namespace
                                .find_definition(&use_stmt.path)
                                .ok_or(DefinitionNotFound);
                            match definition {
                                Ok(def_id) => {
                                    Some((item.ident, def_id.id.to_def_id(self.krate.id)))
                                }
                                Err(err) => {
                                    errors.push(err);
                                    None
                                }
                            }
                        }
                    }
                }
                ItemKind::GlobalLet(let_stmt) => {
                    Some((let_stmt.name.ident, item.id.to_def_id(self.krate.id)))
                }
                ItemKind::Class(class_stmt) => {
                    Some((class_stmt.name.ident, item.id.to_def_id(self.krate.id)))
                }
                ItemKind::Enum(enum_stmt) => {
                    Some((enum_stmt.name.ident, item.id.to_def_id(self.krate.id)))
                }
                ItemKind::Trait(trait_stmt) => {
                    Some((trait_stmt.name.ident, item.id.to_def_id(self.krate.id)))
                }
                ItemKind::TraitImpl(trait_impl_stmt) => None,
                ItemKind::Fn(fn_stmt) => {
                    Some((fn_stmt.sig.name.ident, item.id.to_def_id(self.krate.id)))
                }
            } {
                match self.module_scope.insert(ident, id) {
                    Ok(_) => {}
                    Err(err) => {
                        errors.push(err);
                    }
                };
            }
        }
        errors
    }

    fn resolve_global_let_stmt(
        &mut self,
        let_stmt: &GlobalLetStmt,
        span: Span,
        id: LocalDefId,
    ) -> ResolveResult {
        // Lower expression
        let expr_id = self.resolve_expr(&let_stmt.initializer)?;
        Ok(())
    }

    fn resolve_class_stmt(
        &mut self,
        class_stmt: &AstClassStmt,
        span: Span,
        id: LocalDefId,
    ) -> ResolveResult {
        self.scopes.push(Scope::Class {
            fields: Default::default(),
            self_fns: Default::default(),
            generics: Default::default(),
        });

        // We have to resolve generics params, fields, and fns in that order.
        let generic_params = self.resolve_generic_params(&class_stmt.generic_params)?;
        let fields = self.resolve_fields(&class_stmt.fields)?;
        let fn_stmts = self.resolve_self_fn_stmts(&class_stmt.self_fns)?;

        let item = HirItem::Class(ClassStmt::new(
            class_stmt.name,
            class_stmt.class_type,
            generic_params,
            fields,
            fn_stmts,
        ));
        let hir_node = HirNode::new(HirNodeKind::Item(item), span, id);
        self.items.push(id);
        self.insert_node(id, hir_node)?;
        Ok(())
    }

    fn resolve_generics(&mut self, generics: &AstGenerics) -> Result<Generics, ResolveError> {
        let mut hir_generics = Generics::with_capacity(generics.len());
        for generic in generics.iter() {
            hir_generics.push(self.resolve_ty(generic)?);
        }
        Ok(hir_generics)
    }

    fn resolve_params(&mut self, params: &AstParams) -> Result<Params, ResolveError> {
        let mut hir_params = Params::default();
        for param in params.iter() {
            self.insert_param(param.ident.ident, param.id)?;

            let ty = self.resolve_ty(&param.ty)?;

            self.insert_node(
                param.id,
                HirNode::new(
                    HirNodeKind::Param(Param::new(param.ident, ty, param.mutability)),
                    param.span,
                    param.id,
                ),
            )?;

            hir_params.insert(param.ident.ident, param.id);
        }
        Ok(hir_params)
    }

    fn resolve_generic_params(
        &mut self,
        generic_params: &AstGenericParams,
    ) -> Result<GenericParams, ResolveError> {
        let mut generics = GenericParams::default();
        for param in generic_params.iter() {
            self.insert_generic_param(param.ident.ident, param.id)?;

            let trait_bound = match &param.trait_bound {
                None => None,
                Some(AstTy {
                    kind: AstTyKind::TraitBound { trait_bound },
                    span,
                    id,
                }) => Some(self.resolve_trait_bound(trait_bound, *span, *id)?),
                _ => panic!("Only trait bound should be present!"),
            };
            /*
                We always want to insert the node, even if it clashes with another generic param.
                Otherwise we can't look it up later when handling errors.
            */
            self.insert_node(
                param.id,
                HirNode::new(
                    HirNodeKind::GenericParam(GenericParam::new(param.ident, trait_bound)),
                    param.span,
                    param.id,
                ),
            )?;

            generics.insert(param.ident.ident, param.id);
        }
        Ok(generics)
    }

    fn insert_field(&mut self, field_name: InternedStr, id: LocalDefId) -> ResolveResult {
        match self.scopes.last_mut().unwrap().insert_field(field_name, id) {
            None => Ok(()),
            Some(existing) => self.duplicate_definition(existing, id),
        }
    }

    fn insert_var(&mut self, var_name: InternedStr, id: LocalDefId) -> ResolveResult {
        match self.scopes.last_mut().unwrap().insert_var(var_name, id) {
            None => Ok(()),
            Some(existing) => self.duplicate_definition(existing, id),
        }
    }

    fn insert_self_fn(&mut self, fn_name: InternedStr, id: LocalDefId) -> ResolveResult {
        match self.scopes.last_mut().unwrap().insert_self_fn(fn_name, id) {
            None => Ok(()),
            Some(existing) => self.duplicate_definition(existing, id),
        }
    }

    fn insert_param(&mut self, ident: InternedStr, id: LocalDefId) -> ResolveResult {
        match self.scopes.last_mut().unwrap().insert_param(ident, id) {
            None => Ok(()),
            Some(existing) => self.duplicate_definition(existing, id),
        }
    }

    fn insert_node(&mut self, id: LocalDefId, hir_node: HirNode) -> ResolveResult {
        let index: usize = id.into();
        match self.nodes.insert(id, hir_node) {
            None => Ok(()),
            Some(_) => Err(DuplicateLocalDefIds),
        }
    }

    fn find_generic_param(&mut self, ident: InternedStr) -> Option<LocalDefId> {
        self.scopes
            .iter()
            .find_map(|scope| scope.contains_generic(ident))
    }

    fn insert_generic_param(&mut self, ident: InternedStr, id: LocalDefId) -> ResolveResult {
        // Don't need to check in reverse order since we are checking for existence, not last declared.
        if let Some(existing) = self
            .scopes
            .iter()
            .find_map(|scope| scope.contains_generic(ident))
        {
            return self.duplicate_definition(existing, id);
        }

        // Insert generic into last scope (will return error if this is not a valid scope)
        match self
            .scopes
            .last_mut()
            .unwrap()
            .insert_generic_param(ident, id)
        {
            None => Ok(()),
            Some(existing) => self.duplicate_definition(existing, id),
        }
    }

    fn resolve_fields(&mut self, ast_fields: &AstFields) -> Result<Fields, ResolveError> {
        let mut fields = Fields::default();
        for field in ast_fields.iter() {
            let AstField {
                ident,
                ty,
                span,
                id,
            } = field;

            self.insert_field(ident.ident, *id)?;

            let resolved_ty = self.resolve_ty(ty)?;
            /*
                We always want to insert the node, even if it clashes with another field.
                Otherwise we can't look it up later when handling errors.
            */
            self.insert_node(
                *id,
                HirNode::new(
                    HirNodeKind::Field(Field::new(*ident, resolved_ty)),
                    *span,
                    *id,
                ),
            )?;
            fields.insert(ident.ident, *id);
        }
        Ok(fields)
    }

    fn resolve_enum_stmt(
        &mut self,
        enum_stmt: &AstEnumStmt,
        span: Span,
        id: LocalDefId,
    ) -> ResolveResult {
        self.scopes.push(Scope::Enum {
            members: Default::default(),
            self_fns: Default::default(),
            generics: Default::default(),
        });

        let generic_params = self.resolve_generic_params(&enum_stmt.generic_params)?;
        let enum_members = self.resolve_enum_members(&enum_stmt.members)?;
        let member_fns = self.resolve_self_fn_stmts(&enum_stmt.self_fns)?;

        self.scopes.pop();

        let item = HirItem::Enum(EnumStmt::new(
            enum_stmt.name,
            generic_params,
            enum_members,
            member_fns,
        ));
        let hir_node = HirNode::new(HirNodeKind::Item(item), span, id);
        self.items.push(id);
        self.insert_node(id, hir_node)?;
        Ok(())
    }

    fn resolve_enum_members(
        &mut self,
        enum_stmt_members: &Vec<AstEnumMember>,
    ) -> Result<EnumMembers, ResolveError> {
        let enum_members = EnumMembers::default();
        for member in enum_stmt_members {
            self.scopes.push(Scope::EnumMember {
                fields: Default::default(),
                self_fns: Default::default(),
            });

            let self_fns = self.resolve_self_fn_stmts(&member.self_fns)?;
            let fields = self.resolve_fields(&member.fields)?;

            // TODO: Continue implementing this

            self.scopes.pop();
        }
        todo!()
    }

    fn resolve_trait_stmt(
        &mut self,
        trait_stmt: &AstTraitStmt,
        span: Span,
        id: LocalDefId,
    ) -> ResolveResult {
        let generic_params = self.resolve_generic_params(&trait_stmt.generic_params)?;
        let member_fns = self.resolve_self_fn_stmts(&trait_stmt.self_fns)?;

        self.scopes.push(Scope::Trait {
            self_fns: Default::default(),
            generics: Default::default(),
        });

        self.scopes.pop();

        let item = Trait(TraitStmt::new(trait_stmt.name, generic_params, member_fns));
        let hir_node = HirNode::new(HirNodeKind::Item(item), span, id);
        self.items.push(id);
        self.insert_node(id, hir_node)?;
        Ok(())
    }

    fn resolve_trait_impl_stmt(
        &mut self,
        trait_stmt: &AstTraitImplStmt,
        span: Span,
        id: LocalDefId,
    ) -> ResolveResult {
        let trait_to_impl = self.resolve_path_ty(&trait_stmt.trait_to_impl)?;
        let target_ty = self.resolve_qualified_ident(&trait_stmt.target_ty)?;
        let self_fns = self.resolve_self_fn_stmts(&trait_stmt.self_fns)?;

        todo!()
    }

    fn resolve_fn_stmt(
        &mut self,
        fn_stmt: &AstFnStmt,
        span: Span,
        id: LocalDefId,
    ) -> ResolveResult {
        let resolved_sig = self.resolve_fn_sig(&fn_stmt.sig)?;
        let resolved_body = self.maybe_resolve_block(&fn_stmt.body)?;

        let name = resolved_sig.name.ident;

        self.insert_node(
            id,
            HirNode::new(
                HirNodeKind::Fn(FnStmt::new(resolved_sig, resolved_body)),
                span,
                id,
            ),
        )?;
        Ok(())
    }

    fn resolve_self_fn_stmt(
        &mut self,
        fn_stmt: &AstFnSelfStmt,
    ) -> Result<(InternedStr, LocalDefId), ResolveError> {
        let AstFnSelfStmt {
            sig,
            body,
            span,
            id,
        } = fn_stmt;
        let resolved_body = self.maybe_resolve_block(body)?;
        let resolved_sig = self.resolve_fn_sig(sig)?;
        let name = resolved_sig.name.ident;
        self.insert_node(
            *id,
            HirNode::new(
                HirNodeKind::Fn(FnStmt::new(resolved_sig, resolved_body)),
                *span,
                *id,
            ),
        )?;
        Ok((name, *id))
    }

    fn resolve_self_fn_stmts(&mut self, stmts: &Vec<FnSelfStmt>) -> Result<FnStmts, ResolveError> {
        for fn_stmt in stmts {
            self.insert_self_fn(fn_stmt.sig.name.ident, fn_stmt.id)?;
        }
        // This is safe because we have already checked to ensure that there are no name collisions.
        let mut fn_stmts = FnStmts::default();
        for fn_stmt in stmts {
            let (name, stmt) = self.resolve_self_fn_stmt(fn_stmt)?;
            fn_stmts.insert(name, stmt);
        }
        Ok(fn_stmts)
    }

    fn resolve_fn_sig(&mut self, fn_sig: &AstFnSig) -> Result<FnSig, ResolveError> {
        self.scopes.push(Scope::Fn {
            params: Default::default(),
            generics: Default::default(),
            vars: Default::default(),
        });

        let generic_params = self.resolve_generic_params(&fn_sig.generic_params)?;
        let params = self.resolve_params(&fn_sig.params)?;

        let return_ty = self.maybe_resolve_ty(&fn_sig.return_type)?;

        Ok(FnSig::new(fn_sig.name, generic_params, params, return_ty))
    }

    fn resolve_field(&mut self, field: &AstField) -> Result<LocalDefId, ResolveError> {
        let ty = self.resolve_ty(&field.ty);

        todo!()
    }

    fn maybe_resolve_expr(
        &mut self,
        expr: &Option<Box<AstExpr>>,
    ) -> Result<Option<LocalDefId>, ResolveError> {
        if let Some(expr) = expr {
            self.resolve_expr(expr).map(Some)
        } else {
            Ok(None)
        }
    }

    fn resolve_expr(&mut self, expr: &AstExpr) -> Result<LocalDefId, ResolveError> {
        let span = expr.span;
        let id = expr.id;
        let resolved_expr = match &expr.kind {
            AstExprKind::Array(array_expr) => match array_expr {
                AstArrayExpr::SizedInitializer(initializer, size) => {
                    let initializer = self.resolve_expr(initializer)?;
                    let size = self.resolve_expr(size)?;

                    Expr::Array(ArrayExpr::Sized { initializer, size })
                }
                AstArrayExpr::Initializer(initializers) => {
                    let initializers = initializers
                        .iter()
                        .map(|expr| self.resolve_expr(expr))
                        .collect::<Result<Vec<LocalDefId>, ResolveError>>()?;

                    Expr::Array(ArrayExpr::Unsized { initializers })
                }
            },
            AstExprKind::Call(call) => {
                let args = call
                    .args
                    .iter()
                    .map(|expr| self.resolve_expr(expr))
                    .collect::<Result<Vec<LocalDefId>, ResolveError>>()?;
                let target = self.resolve_expr(&call.target)?;

                Expr::Call(CallExpr::new(target, Args::new(args)))
            }
            AstExprKind::Constructor(constructor) => {
                let args = constructor
                    .args
                    .iter()
                    .map(|expr| self.resolve_expr(expr))
                    .collect::<Result<Vec<LocalDefId>, ResolveError>>()?;
                let target = self.resolve_expr(&constructor.target)?;

                Expr::Constructor(CallExpr::new(target, Args::new(args)))
            }
            AstExprKind::Infix(infix) => {
                let lhs = self.resolve_expr(&infix.lhs)?;
                let rhs = self.resolve_expr(&infix.rhs)?;

                Expr::Infix(InfixExpr::new(infix.operator, lhs, rhs))
            }
            AstExprKind::Unary(unary) => {
                let expr = self.resolve_expr(&unary.expr)?;

                Expr::Unary(UnaryExpr::new(unary.operator, expr))
            }
            AstExprKind::None => Expr::None,
            AstExprKind::Boolean(boolean) => Expr::Boolean(*boolean),
            AstExprKind::Integer(integer) => Expr::Integer(*integer),
            AstExprKind::Float(float) => Expr::Float(*float),
            AstExprKind::String(string) => Expr::String(*string),
            AstExprKind::Match(match_expr) => {
                let source = self.resolve_expr(&match_expr.source)?;
                let arms = match_expr
                    .arms
                    .iter()
                    .map(|arm| self.resolve_match_arm(arm))
                    .collect::<Result<Vec<MatchArm>, ResolveError>>()?;

                Expr::Match(MatchExpr::new(source, arms))
            }
            AstExprKind::Closure(closure) => {
                let params = self.resolve_closure_params(&closure.params)?;
                let stmt = self.resolve_stmt(&closure.stmt)?;
                Expr::Closure(ClosureExpr::new(params, stmt))
            }
            AstExprKind::Assign(assign) => {
                let lhs = self.resolve_expr(&assign.lhs)?;
                let rhs = self.resolve_expr(&assign.rhs)?;
                Expr::Assign(AssignExpr::new(lhs, rhs))
            }
            AstExprKind::Field(field) => {
                let lhs = self.resolve_expr(&field.lhs)?;
                Expr::Field(FieldExpr::new(lhs, field.ident))
            }
            AstExprKind::Index(index) => {
                let expr = self.resolve_expr(&index.expr)?;
                let key = self.resolve_expr(&index.key)?;
                Expr::Index(IndexExpr::new(expr, key))
            }
            AstExprKind::Path(path) => {
                let mut segments = Vec::with_capacity(path.segments.len());
                for segment in path.segments.iter() {
                    let ident = segment.ident;
                    let generics = match &segment.generics {
                        None => None,
                        Some(generics) => {
                            let mut resolved_generics = Generics::with_capacity(generics.len());
                            for generic in generics.iter() {
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
                let resolved_expr = self.resolve_expr(&parentheses.expr)?;
                return Ok(id);
            }
            AstExprKind::Break => Expr::Break,
            AstExprKind::Continue => Expr::Continue,
        };

        self.insert_node(id, HirNode::new(HirNodeKind::Expr(resolved_expr), span, id))?;
        Ok(id)
    }

    fn maybe_resolve_ty(&mut self, ty: &Option<AstTy>) -> Result<Option<LocalDefId>, ResolveError> {
        if let Some(ty) = ty {
            self.resolve_ty(ty).map(Some)
        } else {
            Ok(None)
        }
    }

    fn resolve_ty(&mut self, ty: &AstTy) -> Result<LocalDefId, ResolveError> {
        let span = ty.span;
        let id = ty.id;
        let hir_ty = match &ty.kind {
            TyKind::Array { ty } => Ty::Array {
                ty: self.resolve_ty(ty)?,
            },
            TyKind::Path { path } => Ty::Path {
                path: self.resolve_path_ty(path)?,
            },
            TyKind::TraitBound { trait_bound } => {
                let mut paths = Vec::with_capacity(trait_bound.bounds.len());
                for path in trait_bound.bounds.iter() {
                    let def = self.resolve_qualified_ident(&path.ident)?;
                    let generics = self.resolve_generics(&path.generics)?;
                    paths.push(PathTy::new(def, generics));
                }
                Ty::TraitBound {
                    trait_bound: TraitBound::new(paths),
                }
            }
            TyKind::Closure { params, ret_ty } => {
                let params = params
                    .iter()
                    .map(|param| self.resolve_ty(param))
                    .collect::<Result<Vec<LocalDefId>, ResolveError>>()?;
                let ret_ty = self.resolve_ty(ret_ty)?;
                Ty::Closure { params, ret_ty }
            }
            TyKind::Infer => Ty::Infer,
            TyKind::QSelf => Ty::QSelf,
            TyKind::U8 => Ty::U8,
            TyKind::U16 => Ty::U16,
            TyKind::U32 => Ty::U32,
            TyKind::U64 => Ty::U64,
            TyKind::I8 => Ty::I8,
            TyKind::I16 => Ty::I16,
            TyKind::I32 => Ty::I32,
            TyKind::I64 => Ty::I64,
            TyKind::F32 => Ty::F32,
            TyKind::F64 => Ty::F64,
            TyKind::Str => Ty::Str,
            TyKind::None => Ty::None,
        };

        self.insert_node(id, HirNode::new(HirNodeKind::Ty(hir_ty), span, id))?;
        Ok(id)
    }

    fn resolve_path_ty(&mut self, path_ty: &AstPathTy) -> Result<PathTy, ResolveError> {
        let def = self.resolve_qualified_ident(&path_ty.ident)?;
        let generics = self.resolve_generics(&path_ty.generics)?;
        Ok(PathTy::new(def, generics))
    }

    fn resolve_trait_bound(
        &mut self,
        trait_bound: &AstTraitBound,
        span: Span,
        id: LocalDefId,
    ) -> Result<LocalDefId, ResolveError> {
        let mut hir_bound = TraitBound::default();
        for ty in trait_bound.bounds.iter() {
            let resolved_ty = self.resolve_path_ty(ty)?;
            hir_bound.push(resolved_ty);
        }

        self.insert_node(
            id,
            HirNode::new(
                HirNodeKind::Ty(Ty::TraitBound {
                    trait_bound: hir_bound,
                }),
                span,
                id,
            ),
        )?;

        Ok(id)
    }

    fn resolve_stmt(&mut self, stmt: &AstStmt) -> Result<LocalDefId, ResolveError> {
        let hir_stmt = match &stmt.kind {
            AstStmtKind::Let(let_stmt) => {
                self.insert_var(let_stmt.ident.ident, let_stmt.ident.id)?;

                let resolved_ty = self.maybe_resolve_ty(&let_stmt.ty)?;
                let resolved_initializer = self.maybe_resolve_expr(&let_stmt.initializer)?;

                let stmt = Stmt::Let(LetStmt::new(
                    let_stmt.ident,
                    let_stmt.mutability,
                    resolved_ty,
                    resolved_initializer,
                ));
                stmt
            }
            AstStmtKind::For(for_stmt) => {
                self.scopes.push(Scope::Block {
                    vars: HashMap::default(),
                });
                self.insert_var(for_stmt.ident.ident, for_stmt.ident.id)?;

                let range = self.resolve_expr(&for_stmt.range)?;
                let body = self.resolve_block(&for_stmt.body)?;
                let stmt = Stmt::For(ForStmt::new(for_stmt.ident, range, body));

                self.scopes.pop();
                stmt
            }
            AstStmtKind::If(if_stmt) => {
                todo!()
            }
            AstStmtKind::Return(return_stmt) => {
                let maybe_expr = self.maybe_resolve_expr(&return_stmt.value)?;
                Stmt::Return(ReturnStmt::new(maybe_expr))
            }
            AstStmtKind::While(while_stmt) => {
                todo!()
            }
            AstStmtKind::Block(block) => {
                todo!()
            }
            AstStmtKind::Expression(expression) => {
                let expr = self.resolve_expr(&expression.expr)?;

                Stmt::Expression(Expression::new(expr, expression.implicit_return))
            }
        };

        self.insert_node(
            stmt.id,
            HirNode::new(HirNodeKind::Stmt(hir_stmt), stmt.span, stmt.id),
        )?;

        Ok(stmt.id)
    }

    fn maybe_resolve_block(
        &mut self,
        block: &Option<AstBlock>,
    ) -> Result<Option<LocalDefId>, ResolveError> {
        match block {
            None => Ok(None),
            Some(block) => Ok(Some(self.resolve_block(block)?)),
        }
    }

    fn resolve_block(&mut self, block: &AstBlock) -> Result<LocalDefId, ResolveError> {
        let mut stmts = Stmts::default();
        for stmt in &block.stmts {
            stmts.push(self.resolve_stmt(stmt)?);
        }

        self.insert_node(
            block.id,
            HirNode::new(HirNodeKind::Block(Block::new(stmts)), block.span, block.id),
        )?;

        Ok(block.id)
    }

    fn resolve_match_arm(&mut self, arm: &AstMatchArm) -> Result<MatchArm, ResolveError> {
        let body = self.resolve_stmt(&arm.body)?;
        let pattern = self.resolve_pattern(&arm.pattern)?;

        Ok(MatchArm::new(pattern, body))
    }

    fn resolve_pattern(&mut self, pattern: &AstPattern) -> Result<LocalDefId, ResolveError> {
        todo!()
    }

    fn resolve_qualified_ident(&mut self, ident: &QualifiedIdent) -> Result<DefId, ResolveError> {
        match ident.ident_type {
            IdentType::Crate => self.krate.find_definition(ident).ok_or(DefinitionNotFound),
            IdentType::LocalOrUse => {
                if ident.is_local() {
                    self.module_scope
                        .items
                        .get(&ident.first().ident)
                        .copied()
                        .or_else(|| {
                            self.find_generic_param(ident.first().ident)
                                .map(|id| id.to_def_id(self.krate.id))
                        })
                        .ok_or(DefinitionNotFound)
                } else {
                    let krate_ident = ident.first().ident;
                    let namespace = self.krate_attrs.get(&krate_ident).unwrap();
                    namespace.find_definition(ident).ok_or(DefinitionNotFound)
                }
            }
        }
    }

    fn resolve_closure_params(
        &mut self,
        params: &[AstClosureParam],
    ) -> Result<ClosureParams, ResolveError> {
        // TODO: Add vars to scope
        let params = params
            .iter()
            .map(|param| (param.ident.ident, ClosureParam::new(param.ident)))
            .collect();
        Ok(ClosureParams::new(params))
    }
}

mod tests {
    use std::collections::HashMap;

    use snap::snapshot;

    use crate::compiler::compiler::{CompileError, Compiler};
    use crate::compiler::hir::HirCrate;
    use crate::compiler::resolver::{CrateResolver, Resolver};
    use crate::util::utils;

    #[cfg(test)]
    fn compile_crate(name: &str) -> HirCrate {
        let mut compiler = Compiler::default();
        let krate = compiler
            .parse_crate(&utils::resolve_test_krate_path(name))
            .unwrap();
        let map = HashMap::default();
        let crate_resolver = CrateResolver::new(&krate, &map);
        crate_resolver.resolve().unwrap()
    }

    #[test]
    #[snapshot]
    pub fn import_function_from_crate() -> HirCrate {
        compile_crate("complex_arithmetic")
    }

    #[test]
    #[snapshot]
    pub fn compile_simple_class() -> HirCrate {
        compile_crate("compile_simple_class")
    }
}
