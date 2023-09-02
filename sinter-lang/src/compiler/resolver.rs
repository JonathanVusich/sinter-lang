use std::any::Any;
use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;
use std::ops::Deref;
use std::sync::Arc;

use itertools::Itertools;
use serde::{Deserialize, Serialize};

use crate::compiler::ast::{
    ArrayExpr as AstArrayExpr, AstPass, Block as AstBlock, ClassStmt as AstClassStmt,
    ClosureParam as AstClosureParam, DestructureExpr as AstDestructureExpr,
    DestructureExprKind as AstDestructureExprKind, DestructureExprKind,
    DestructurePattern as AstDestructurePattern, EnumMember as AstEnumMember,
    EnumStmt as AstEnumStmt, Expr as AstExpr, ExprKind as AstExprKind, Field as AstField,
    Fields as AstFields, FnSelfStmt as AstFnSelfStmt, FnSelfStmt, FnSig as AstFnSig,
    FnStmt as AstFnStmt, GenericParams as AstGenericParams, Generics as AstGenerics,
    GlobalLetStmt as AstGlobalLetStmt, IdentType, Item as AstItem, Item, ItemKind as AstItemKind,
    ItemKind, MatchArm as AstMatchArm, Module, Params as AstParams, PathTy as AstPathTy,
    Pattern as AstPattern, QualifiedIdent, Stmt as AstStmt, StmtKind as AstStmtKind,
    TraitBound as AstTraitBound, TraitImplStmt as AstTraitImplStmt, TraitStmt as AstTraitStmt,
    Ty as AstTy, TyKind, TyKind as AstTyKind,
};
use crate::compiler::compiler::CompileError;
use crate::compiler::hir::{
    AnonParams, Args, ArrayExpr, AssignExpr, Block, Builtin, CallExpr, ClassStmt, ClosureExpr,
    ClosureParam, ClosureParams, DefId, DestructureExpr, DestructurePattern, EnumMember,
    EnumMembers, EnumStmt, Expr, Expression, Field, FieldExpr, Fields, FnSig, FnStmt, FnStmts,
    ForStmt, GenericParam, GenericParams, Generics, GlobalLetStmt, HirCrate, HirNode, HirNodeKind,
    IfStmt, IndexExpr, InfixExpr, LetStmt, LocalDefId, MatchArm, MatchExpr, ModuleId, OrPattern,
    Param, Params, PathExpr, PathTy, Pattern, PatternLocal, ReturnStmt, Segment, Stmt, Stmts,
    TraitBound, TraitImplStmt, TraitStmt, Ty, TyPattern, UnaryExpr, WhileStmt,
};
use crate::compiler::krate::{Crate, CrateDef};
use crate::compiler::path::ModulePath;
use crate::compiler::resolver::ResolveError::{
    DuplicateLocalDefIds, ExpectedValueWasModule, QualifiedIdentNotFound, VarNotFound,
};
use crate::compiler::tokens::tokenized_file::Span;
use crate::compiler::types::{IStrMap, InternedStr, LDefMap, StrMap};

pub fn resolve(crates: &mut StrMap<Crate>) -> Result<StrMap<HirCrate>, CompileError> {
    let resolver = Resolver::new(crates);
    resolver.resolve()
}

#[derive(PartialEq, Eq, Debug, Default, Serialize, Deserialize)]
pub struct ModuleNS {
    pub(crate) modules: StrMap<ModuleId>,
    // We have one namespace for structs, fns and constants because they are all callable and cannot
    // be disambiguated at resolve time by their usage.
    pub(crate) values: StrMap<DefId>,
    pub(crate) enums: StrMap<EnumDef>,
}

impl ModuleNS {
    pub fn find_ident(&self, ident: &QualifiedIdent, trim_module_name: bool) -> Option<DefId> {
        let mut module_path = ModulePath::from(ident);
        if trim_module_name {
            module_path.pop_front();
        }
        if module_path.len() == 1 {
            // Resolve to either an enum or a value in the current scope.
            module_path.front().and_then(|val| {
                self.values
                    .get(&val)
                    .copied()
                    .or_else(|| self.enums.get(&val).map(|def| def.id))
            })
        } else if module_path.len() == 2 {
            module_path
                .pop_front()
                // Retrieve the enum
                .and_then(|name| self.enums.get(&name))
                // Retrieve the corresponding enum member
                .and_then(|enum_def| {
                    let member = module_path.front().unwrap();
                    enum_def.members.get(&member)
                })
                .copied()
        } else {
            None
        }
    }

    pub fn find_value(&self, value: InternedStr) -> Option<DefId> {
        self.values.get(&value).copied()
    }

    pub fn find_enum(&self, value: InternedStr) -> Option<&EnumDef> {
        self.enums.get(&value)
    }

    pub fn find_module(&self, module: InternedStr) -> Option<ModuleId> {
        self.modules.get(&module).copied()
    }
}

/// Cloning this type should be cheap since it uses an immutable map.
#[derive(PartialEq, Eq, Debug, Clone, Serialize, Deserialize)]
pub struct EnumDef {
    pub(crate) id: DefId,
    pub(crate) members: IStrMap<DefId>,
}

impl EnumDef {
    pub fn new(id: DefId, members: IStrMap<DefId>) -> Self {
        Self { id, members }
    }
}

#[derive(Debug)]
pub enum Scope {
    Class {
        id: DefId,
        fields: StrMap<LocalDefId>,
        self_fns: StrMap<LocalDefId>,
        generics: StrMap<LocalDefId>,
    },
    Enum {
        id: DefId,
        members: StrMap<LocalDefId>,
        self_fns: StrMap<LocalDefId>,
        generics: StrMap<LocalDefId>,
    },
    EnumMember {
        fields: StrMap<LocalDefId>,
        self_fns: StrMap<LocalDefId>,
    },
    Fn {
        params: StrMap<LocalDefId>,
        generics: StrMap<LocalDefId>,
        vars: StrMap<LocalDefId>,
    },
    MatchArm {
        vars: StrMap<LocalDefId>,
    },
    Block {
        vars: StrMap<LocalDefId>,
    },
    Trait {
        id: DefId,
        generics: StrMap<LocalDefId>,
        self_fns: StrMap<LocalDefId>,
    },
    TraitImpl {
        target_id: DefId,
        self_fns: StrMap<LocalDefId>,
    },
}

impl Scope {
    pub fn contains_var(&self, ident: InternedStr) -> Option<LocalDefId> {
        match self {
            Scope::Block { vars } => vars.get(&ident).copied(),
            Scope::Fn { params, .. } => params.get(&ident).copied(),
            _ => None,
        }
    }

    pub fn contains_generic(&self, ident: InternedStr) -> Option<LocalDefId> {
        match self {
            Scope::Class { generics, .. } => generics.get(&ident).copied(),
            Scope::Enum { generics, .. } => generics.get(&ident).copied(),
            Scope::Fn { generics, .. } => generics.get(&ident).copied(),
            _ => None,
        }
    }

    pub fn contains_enum_member(&self, ident: InternedStr) -> Option<LocalDefId> {
        match self {
            Scope::Enum { members, .. } => members.get(&ident).copied(),
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

    pub fn insert_var(&mut self, ident: InternedStr, id: LocalDefId) {
        let vars = match self {
            Scope::Fn { vars, .. } => vars,
            Scope::MatchArm { vars } => vars,
            _ => panic!("Cannot insert var into this scope!"),
        };
        vars.insert(ident, id);
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

    pub fn insert_enum_member(&mut self, ident: InternedStr, id: LocalDefId) -> Option<LocalDefId> {
        let members = match self {
            Scope::Enum { members, .. } => members,
            _ => panic!("Cannot insert enum member into this scope!"),
        };
        members.insert(ident, id)
    }
}

#[derive(Debug, Clone)]
pub enum ResolveError {
    /// Error from looking up definition in crate
    QualifiedIdentNotFound(QualifiedIdent),
    VarNotFound(InternedStr),
    /// Error when resolving path, expected value but was module
    ExpectedValueWasModule,
    /// Duplicate nodes created for a single local definition.
    DuplicateLocalDefIds,
    /// Error for duplicate definitions
    DuplicateDefinition {
        existing: DefId,
        new: DefId,
    },
}

impl Display for ResolveError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl Error for ResolveError {}

type ResolveResult = Result<(), ResolveError>;

struct Resolver<'a> {
    krates: &'a mut StrMap<Crate>,
    errors: Vec<ResolveError>,
}

impl<'a> Resolver<'a> {
    fn new(krates: &'a mut StrMap<Crate>) -> Self {
        Self {
            krates,
            errors: Default::default(),
        }
    }

    fn resolve(mut self) -> Result<StrMap<HirCrate>, CompileError> {
        let mut crates = StrMap::default();
        // We are building lookup maps for each module so that we can query the types and constants in that module
        // when resolving use stmts later.
        self.build_crate_namespaces()?;

        for krate in self.krates.values() {
            let crate_resolver = CrateResolver::new(krate, self.krates);
            match crate_resolver.resolve() {
                Ok(crate_index) => {
                    crates.insert(krate.name, crate_index);
                }
                Err(errors) => match errors {
                    CompileError::ResolveErrors(errors) => self.errors.extend(errors),
                    _ => panic!("Invalid error type!"),
                },
            }
        }
        if !self.errors.is_empty() {
            Err(CompileError::ResolveErrors(self.errors))
        } else {
            Ok(crates)
        }
    }

    fn build_crate_namespaces(&mut self) -> Result<(), CompileError> {
        for krate in self.krates.values_mut() {
            let krate_id = krate.crate_id;
            for module in krate.modules_mut() {
                let mut ns = ModuleNS::default();
                // Initial pass we just want to collect
                for item in module.items.iter() {
                    let def_id = item.id.to_def_id(krate_id);
                    match &item.kind {
                        ItemKind::GlobalLet(global_let_stmt) => {
                            ns.values.insert(global_let_stmt.name.ident, def_id);
                        }
                        ItemKind::Class(class_stmt) => {
                            ns.values.insert(class_stmt.name.ident, def_id);
                        }
                        ItemKind::Enum(enum_stmt) => {
                            let members = Arc::new(
                                enum_stmt
                                    .members
                                    .iter()
                                    .map(|member| (member.name, member.id.to_def_id(krate_id)))
                                    .collect::<StrMap<DefId>>(),
                            );
                            ns.enums
                                .insert(enum_stmt.name.ident, EnumDef::new(def_id, members));
                        }
                        ItemKind::Trait(trait_stmt) => {
                            ns.values.insert(trait_stmt.name.ident, def_id);
                        }
                        ItemKind::Fn(fn_stmt) => {
                            ns.values.insert(fn_stmt.sig.name.ident, def_id);
                        }
                        ItemKind::Use(use_stmt) => {}
                        ItemKind::TraitImpl(trait_impl_stmt) => {}
                    };
                }
                module.ns = ns;
            }
        }

        // Second pass we need to process all use stmts since we now have all of the values
        // populated for each module.
        let mut module_crate_defs = HashMap::<ModuleId, Vec<CrateDef>>::default();
        for krate in self.krates.values() {
            for module in krate.modules() {
                for item in module.items.iter() {
                    if let ItemKind::Use(use_stmt) = &item.kind {
                        let maybe_crate_def = match &use_stmt.path.ident_type {
                            IdentType::Crate => krate.find_definition(&use_stmt.path, false),
                            IdentType::LocalOrUse => {
                                let krate = self.krates.get(&use_stmt.path.first()).unwrap();
                                krate.find_definition(&use_stmt.path, true)
                            }
                        };

                        match maybe_crate_def {
                            Some(crate_def) => module_crate_defs
                                .entry(module.id)
                                .or_default()
                                .push(crate_def),
                            None => self
                                .errors
                                .push(QualifiedIdentNotFound(use_stmt.path.clone())),
                        }
                    }
                }
            }
        }

        for krate in self.krates.values_mut() {
            for module in krate.modules_mut() {
                if let Some(crate_defs) = module_crate_defs.remove(&module.id) {
                    for crate_def in crate_defs {
                        match crate_def {
                            CrateDef::Module(name, mod_id) => {
                                module.ns.modules.insert(name, mod_id);
                            }
                            CrateDef::Value(name, def_id) => {
                                module.ns.values.insert(name, def_id);
                            }
                            CrateDef::Enum(name, enum_def) => {
                                module.ns.enums.insert(name, enum_def);
                            }
                        }
                    }
                }
            }
        }

        if !self.errors.is_empty() {
            Err(CompileError::ResolveErrors(self.errors.clone()))
        } else {
            Ok(())
        }
    }
}

#[derive(Default)]
struct CrateMap<'a> {
    items: HashMap<LocalDefId, &'a AstItem>,
}

struct CrateResolver<'a> {
    krate: &'a Crate,
    crates: &'a StrMap<Crate>,
    crate_lookup: Vec<&'a Crate>,
    module: Option<&'a Module>,
    errors: Vec<ResolveError>,
    items: Vec<LocalDefId>,
    nodes: LDefMap<HirNode>,
    scopes: Vec<Scope>,
}

impl<'a> CrateResolver<'a> {
    fn new(krate: &'a Crate, crates: &'a StrMap<Crate>) -> Self {
        let crate_lookup = crates
            .values()
            .sorted_by_key(|krate| krate.crate_id)
            .collect_vec();
        Self {
            krate,
            crates,
            crate_lookup,
            module: None,
            errors: Default::default(),
            items: Default::default(),
            nodes: Default::default(),
            scopes: Default::default(),
        }
    }

    fn resolve(mut self) -> Result<HirCrate, CompileError> {
        for module in self.krate.modules() {
            self.resolve_module(module);

            // We have to remember to clear out all scopes and reset the module scope
            self.scopes.clear();
        }
        if !self.errors.is_empty() {
            Err(CompileError::ResolveErrors(self.errors))
        } else {
            Ok(HirCrate::new(
                self.krate.name,
                self.krate.crate_id,
                self.items,
                self.nodes,
            ))
        }
    }

    fn resolve_module(&mut self, module: &'a Module) {
        self.module = Some(module);
        module.items.iter().for_each(|item| self.visit_item(item));
    }

    fn visit_item(&mut self, item: &Item) {
        let span = item.span;
        let id = item.id;

        let result = match &item.kind {
            AstItemKind::Use(use_stmt) => Ok(()),
            AstItemKind::GlobalLet(let_stmt) => self.resolve_global_let_stmt(let_stmt, span, id),
            AstItemKind::Class(class_stmt) => self.resolve_class_stmt(class_stmt, span, id),
            AstItemKind::Enum(enum_stmt) => self.resolve_enum_stmt(enum_stmt, span, id),
            AstItemKind::Trait(trait_stmt) => self.resolve_trait_stmt(trait_stmt, span, id),
            AstItemKind::TraitImpl(trait_impl_stmt) => {
                self.resolve_trait_impl_stmt(trait_impl_stmt, span, id)
            }
            AstItemKind::Fn(fn_stmt) => self.resolve_fn_stmt(fn_stmt, span, id),
        };

        if let Err(error) = result {
            self.errors.push(error);
        }
    }

    fn duplicate_definition(&self, existing: LocalDefId, new: LocalDefId) -> ResolveResult {
        Err(ResolveError::DuplicateDefinition {
            existing: existing.to_def_id(self.krate.crate_id),
            new: new.to_def_id(self.krate.crate_id),
        })
    }

    fn resolve_global_let_stmt(
        &mut self,
        let_stmt: &AstGlobalLetStmt,
        span: Span,
        id: LocalDefId,
    ) -> ResolveResult {
        // Lower expression
        let ty = self.maybe_resolve_ty(&let_stmt.ty)?;
        let expr = self.resolve_expr(&let_stmt.initializer)?;

        // We don't need to insert constants into a local scope since it is already part of the module ns.
        let hir_node = HirNode::new(
            HirNodeKind::GlobalLet(GlobalLetStmt::new(let_stmt.name.ident, ty, expr)),
            span,
            id,
        );
        self.items.push(id);
        self.insert_node(id, hir_node)?;
        Ok(())
    }

    fn resolve_class_stmt(
        &mut self,
        class_stmt: &AstClassStmt,
        span: Span,
        id: LocalDefId,
    ) -> ResolveResult {
        let def_id = id.to_def_id(self.krate.crate_id);
        self.scopes.push(Scope::Class {
            id: def_id,
            fields: Default::default(),
            self_fns: Default::default(),
            generics: Default::default(),
        });

        // We have to resolve generics params, fields, and fns in that order.
        let generic_params = self.resolve_generic_params(&class_stmt.generic_params)?;
        let fields = self.resolve_fields(&class_stmt.fields)?;
        let fn_stmts = self.resolve_self_fn_stmts(&class_stmt.self_fns)?;

        let hir_class = ClassStmt::new(
            class_stmt.name,
            class_stmt.class_type,
            generic_params,
            fields,
            fn_stmts,
        );
        let hir_node = HirNode::new(HirNodeKind::Class(hir_class), span, id);
        self.items.push(id);
        self.insert_node(id, hir_node)?;
        Ok(())
    }

    fn resolve_generics(&mut self, generics: &AstGenerics) -> Result<Generics, ResolveError> {
        let mut hir_generics = Vec::with_capacity(generics.len());
        for generic in generics.iter() {
            hir_generics.push(self.resolve_ty(generic)?);
        }
        Ok(Generics::from(hir_generics))
    }

    fn resolve_params(&mut self, params: &AstParams) -> Result<Params, ResolveError> {
        let mut hir_params = StrMap::default();
        for param in params.iter() {
            self.insert_param(param.name.ident, param.id)?;

            let ty = self.resolve_ty(&param.ty)?;

            self.insert_node(
                param.id,
                HirNode::new(
                    HirNodeKind::Param(Param::new(param.name, ty, param.mutability)),
                    param.span,
                    param.id,
                ),
            )?;

            hir_params.insert(param.name.ident, param.id);
        }
        Ok(Params::from(hir_params))
    }

    fn resolve_generic_params(
        &mut self,
        generic_params: &AstGenericParams,
    ) -> Result<GenericParams, ResolveError> {
        let mut generics = StrMap::default();
        for param in generic_params.iter() {
            self.insert_generic_param(param.name.ident, param.id)?;

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
                    HirNodeKind::GenericParam(GenericParam::new(param.name, trait_bound)),
                    param.span,
                    param.id,
                ),
            )?;

            generics.insert(param.name.ident, param.id);
        }
        Ok(GenericParams::from(generics))
    }

    fn get_module(&self, id: ModuleId) -> &Module {
        self.crate_lookup[id.crate_id()].module(id)
    }

    fn insert_field(&mut self, field_name: InternedStr, id: LocalDefId) -> ResolveResult {
        match self.scopes.last_mut().unwrap().insert_field(field_name, id) {
            None => Ok(()),
            Some(existing) => self.duplicate_definition(existing, id),
        }
    }

    fn insert_var(&mut self, var_name: InternedStr, id: LocalDefId) {
        self.scopes.last_mut().unwrap().insert_var(var_name, id);
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

    fn insert_enum_member(&mut self, ident: InternedStr, id: LocalDefId) -> ResolveResult {
        match self
            .scopes
            .last_mut()
            .unwrap()
            .insert_enum_member(ident, id)
        {
            None => Ok(()),
            Some(existing) => self.duplicate_definition(existing, id),
        }
    }

    fn find_enum_member(&mut self, ident: InternedStr) -> Option<LocalDefId> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.contains_enum_member(ident))
    }

    fn insert_node(&mut self, id: LocalDefId, hir_node: HirNode) -> ResolveResult {
        let index: usize = id.into();
        match self.nodes.insert(id, hir_node) {
            None => Ok(()),
            Some(_) => Err(DuplicateLocalDefIds),
        }
    }

    fn find_var(&mut self, ident: InternedStr) -> Option<LocalDefId> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.contains_var(ident))
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
        let mut fields = StrMap::default();
        for field in ast_fields.iter() {
            let AstField {
                name: ident,
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
        Ok(Fields::from(fields))
    }

    fn resolve_enum_stmt(
        &mut self,
        enum_stmt: &AstEnumStmt,
        span: Span,
        id: LocalDefId,
    ) -> ResolveResult {
        let def_id = id.to_def_id(self.krate.crate_id);
        self.scopes.push(Scope::Enum {
            id: def_id,
            members: Default::default(),
            self_fns: Default::default(),
            generics: Default::default(),
        });

        let generic_params = self.resolve_generic_params(&enum_stmt.generic_params)?;
        let enum_members = self.resolve_enum_members(&enum_stmt.members)?;
        let member_fns = self.resolve_self_fn_stmts(&enum_stmt.self_fns)?;

        self.scopes.pop();

        let hir_enum = EnumStmt::new(enum_stmt.name, generic_params, enum_members, member_fns);
        let hir_node = HirNode::new(HirNodeKind::Enum(hir_enum), span, id);
        self.items.push(id);
        self.insert_node(id, hir_node)?;
        Ok(())
    }

    fn resolve_enum_members(
        &mut self,
        enum_stmt_members: &Vec<AstEnumMember>,
    ) -> Result<EnumMembers, ResolveError> {
        let mut enum_members = HashMap::default();
        for member in enum_stmt_members {
            self.insert_enum_member(member.name, member.id)?;

            self.scopes.push(Scope::EnumMember {
                fields: Default::default(),
                self_fns: Default::default(),
            });

            let self_fns = self.resolve_self_fn_stmts(&member.self_fns)?;
            let fields = self.resolve_fields(&member.fields)?;

            enum_members.insert(member.name, member.id);

            self.nodes.insert(
                member.id,
                HirNode::new(
                    HirNodeKind::EnumMember(EnumMember::new(member.name, fields, self_fns)),
                    member.span,
                    member.id,
                ),
            );

            self.scopes.pop();
        }
        Ok(enum_members.into())
    }

    fn resolve_trait_stmt(
        &mut self,
        trait_stmt: &AstTraitStmt,
        span: Span,
        id: LocalDefId,
    ) -> ResolveResult {
        let def_id = id.to_def_id(self.krate.crate_id);
        self.scopes.push(Scope::Trait {
            id: def_id,
            self_fns: Default::default(),
            generics: Default::default(),
        });

        let generic_params = self.resolve_generic_params(&trait_stmt.generic_params)?;
        let member_fns = self.resolve_self_fn_stmts(&trait_stmt.self_fns)?;

        self.scopes.pop();

        let hir_trait = TraitStmt::new(trait_stmt.name, generic_params, member_fns);
        let hir_node = HirNode::new(HirNodeKind::Trait(hir_trait), span, id);
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
        let target_ty = self.resolve_qualified_ident(&trait_stmt.target_ty)?;
        let trait_to_impl = self.resolve_path_ty(&trait_stmt.trait_to_impl)?;

        self.scopes.push(Scope::TraitImpl {
            target_id: target_ty,
            self_fns: Default::default(),
        });

        let self_fns = self.resolve_self_fn_stmts(&trait_stmt.self_fns)?;

        self.items.push(id);
        self.insert_node(
            id,
            HirNode::new(
                HirNodeKind::TraitImpl(TraitImplStmt::new(trait_to_impl, target_ty, self_fns)),
                span,
                id,
            ),
        )?;
        Ok(())
    }

    fn resolve_fn_stmt(
        &mut self,
        fn_stmt: &AstFnStmt,
        span: Span,
        id: LocalDefId,
    ) -> ResolveResult {
        self.scopes.push(Scope::Fn {
            params: Default::default(),
            generics: Default::default(),
            vars: Default::default(),
        });

        let resolved_sig = self.resolve_fn_sig(&fn_stmt.sig)?;
        let resolved_body = self.maybe_resolve_block(&fn_stmt.body)?;

        self.scopes.pop();

        let name = resolved_sig.name.ident;

        self.items.push(id);
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

        self.scopes.push(Scope::Fn {
            params: Default::default(),
            generics: Default::default(),
            vars: Default::default(),
        });

        let resolved_sig = self.resolve_fn_sig(sig)?;
        let resolved_body = self.maybe_resolve_block(body)?;

        self.scopes.pop();

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
        let mut fn_stmts = StrMap::default();
        for fn_stmt in stmts {
            let (name, stmt) = self.resolve_self_fn_stmt(fn_stmt)?;
            fn_stmts.insert(name, stmt);
        }
        Ok(FnStmts::from(fn_stmts))
    }

    fn resolve_fn_sig(&mut self, fn_sig: &AstFnSig) -> Result<FnSig, ResolveError> {
        let generic_params = self.resolve_generic_params(&fn_sig.generic_params)?;
        let params = self.resolve_params(&fn_sig.params)?;

        let return_ty = self.maybe_resolve_ty(&fn_sig.return_type)?;

        Ok(FnSig::new(fn_sig.name, generic_params, params, return_ty))
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
                        .collect::<Result<Vec<LocalDefId>, ResolveError>>()?
                        .into();

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

                Expr::Call(CallExpr::new(target, Args::from(args)))
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
            AstExprKind::String(string) => Expr::String(*string),
            AstExprKind::Int(int) => Expr::Int(*int),
            AstExprKind::UInt(uint) => Expr::UInt(*uint),
            AstExprKind::Float(float) => Expr::Float(*float),
            AstExprKind::False => Expr::False,
            AstExprKind::True => Expr::True,
            AstExprKind::None => Expr::None,
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
                            let mut resolved_generics = Vec::with_capacity(generics.len());
                            for generic in generics.iter() {
                                resolved_generics.push(self.resolve_ty(generic)?);
                            }
                            Some(resolved_generics.into())
                        }
                    };
                    segments.push(Segment::new(ident, generics))
                }
                // TODO: Need to actually resolve this path!!
                Expr::Path(PathExpr::new(segments))
            }
            AstExprKind::Parentheses(parentheses) => {
                // Special logic for stripping parentheses (since they are just for pretty printing)
                let resolved_expr = self.resolve_expr(&parentheses.expr)?;
                return Ok(id);
            }
            AstExprKind::Break => Expr::Break,
            AstExprKind::Continue => Expr::Continue,
        };

        self.insert_node(id, HirNode::new(HirNodeKind::Expr(resolved_expr), span, id))?;
        Ok(id)
    }

    fn resolve_destructure_expr(
        &mut self,
        destructure_expr: &AstDestructureExpr,
    ) -> Result<LocalDefId, ResolveError> {
        let id = destructure_expr.id;
        let span = destructure_expr.span;
        let expr = match &destructure_expr.kind {
            AstDestructureExprKind::Pattern(pattern) => {
                let pattern = self.resolve_destructure_pattern(pattern)?;
                DestructureExpr::Pattern(pattern)
            }
            AstDestructureExprKind::Identifier(ident) => {
                self.insert_var(*ident, id);
                DestructureExpr::Identifier(*ident)
            }
            DestructureExprKind::True => DestructureExpr::True,
            DestructureExprKind::False => DestructureExpr::False,
            DestructureExprKind::Float(float) => DestructureExpr::Float(*float),
            DestructureExprKind::Int(int) => DestructureExpr::Int(*int),
            DestructureExprKind::UInt(uint) => DestructureExpr::UInt(*uint),
            DestructureExprKind::String(string) => DestructureExpr::String(*string),
            DestructureExprKind::None => DestructureExpr::None,
        };

        self.insert_node(
            id,
            HirNode::new(HirNodeKind::DestructureExpr(expr), span, id),
        )?;
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
                let mut paths = Vec::with_capacity(trait_bound.len());
                for path in trait_bound.iter() {
                    let def = self.resolve_qualified_ident(&path.ident)?;
                    let generics = self.resolve_generics(&path.generics)?;
                    paths.push(PathTy::new(def, generics));
                }
                Ty::TraitBound {
                    trait_bound: TraitBound::from(paths),
                }
            }
            TyKind::Closure { params, ret_ty } => {
                let params = params
                    .iter()
                    .map(|param| self.resolve_ty(param))
                    .collect::<Result<Vec<LocalDefId>, ResolveError>>()?;
                let ret_ty = self.resolve_ty(ret_ty)?;
                Ty::Closure {
                    params: AnonParams::from(params),
                    ret_ty,
                }
            }
            TyKind::QSelf => {
                let item_def = self
                    .scopes
                    .iter()
                    .rev()
                    .find_map(|scope| match scope {
                        Scope::Class { id, .. } => Some(id),
                        Scope::Enum { id, .. } => Some(id),
                        Scope::Trait { id, .. } => Some(id),
                        Scope::TraitImpl { target_id, .. } => Some(target_id),
                        _ => None,
                    })
                    .copied()
                    .unwrap();

                Ty::Path {
                    path: PathTy::new(item_def, Generics::empty()),
                }
            }
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
            TyKind::Boolean => Ty::Boolean,
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
        let mut hir_bound = Vec::with_capacity(trait_bound.len());
        for ty in trait_bound.iter() {
            let resolved_ty = self.resolve_path_ty(ty)?;
            hir_bound.push(resolved_ty);
        }

        self.insert_node(
            id,
            HirNode::new(
                HirNodeKind::Ty(Ty::TraitBound {
                    trait_bound: hir_bound.into(),
                }),
                span,
                id,
            ),
        )?;

        Ok(id)
    }

    fn resolve_stmt(&mut self, stmt: &AstStmt) -> Result<LocalDefId, ResolveError> {
        let span = stmt.span;
        let id = stmt.id;
        let hir_stmt = match &stmt.kind {
            AstStmtKind::Let(let_stmt) => {
                self.insert_var(let_stmt.ident.ident, id);

                let resolved_ty = self.maybe_resolve_ty(&let_stmt.ty)?;
                let resolved_initializer = self.maybe_resolve_expr(&let_stmt.initializer)?;

                Stmt::Let(LetStmt::new(
                    let_stmt.ident,
                    let_stmt.mutability,
                    resolved_ty,
                    resolved_initializer,
                ))
            }
            AstStmtKind::For(for_stmt) => {
                self.scopes.push(Scope::Block {
                    vars: HashMap::default(),
                });
                self.insert_var(for_stmt.ident.ident, id);

                let range = self.resolve_expr(&for_stmt.range)?;
                let body = self.resolve_block(&for_stmt.body)?;
                let stmt = Stmt::For(ForStmt::new(for_stmt.ident, range, body));

                self.scopes.pop();
                stmt
            }
            AstStmtKind::If(if_stmt) => {
                let condition = self.resolve_expr(&if_stmt.condition)?;
                self.scopes.push(Scope::Block {
                    vars: Default::default(),
                });
                let if_true = self.resolve_block(&if_stmt.if_true)?;
                self.scopes.pop();
                self.scopes.push(Scope::Block {
                    vars: Default::default(),
                });
                let if_false = self.maybe_resolve_block(&if_stmt.if_false)?;
                self.scopes.pop();

                Stmt::If(IfStmt::new(condition, if_true, if_false))
            }
            AstStmtKind::Return(return_stmt) => {
                let maybe_expr = self.maybe_resolve_expr(&return_stmt.value)?;
                Stmt::Return(ReturnStmt::new(maybe_expr))
            }
            AstStmtKind::While(while_stmt) => {
                let condition = self.resolve_expr(&while_stmt.condition)?;

                self.scopes.push(Scope::Block {
                    vars: Default::default(),
                });
                let block = self.resolve_block(&while_stmt.block_stmt)?;
                self.scopes.pop();

                Stmt::While(WhileStmt::new(condition, block))
            }
            AstStmtKind::Block(block) => {
                self.scopes.push(Scope::Block {
                    vars: Default::default(),
                });
                let block = self.resolve_block(block)?;
                self.scopes.pop();
                Stmt::Block(block)
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
        let mut stmts = Vec::with_capacity(block.stmts.len());
        for stmt in &block.stmts {
            stmts.push(self.resolve_stmt(stmt)?);
        }

        self.insert_node(
            block.id,
            HirNode::new(
                HirNodeKind::Block(Block::new(stmts.into())),
                block.span,
                block.id,
            ),
        )?;

        Ok(block.id)
    }

    fn resolve_match_arm(&mut self, arm: &AstMatchArm) -> Result<MatchArm, ResolveError> {
        self.scopes.push(Scope::MatchArm {
            vars: Default::default(),
        });

        let pattern = self.resolve_pattern(&arm.pattern)?;

        let body = self.resolve_stmt(&arm.body)?;

        self.scopes.pop();

        Ok(MatchArm::new(pattern, body))
    }

    fn resolve_pattern(&mut self, pattern: &AstPattern) -> Result<Pattern, ResolveError> {
        let hir_pattern = match pattern {
            AstPattern::Wildcard => Pattern::Wildcard,
            AstPattern::Or(or_pattern) => {
                let patterns = or_pattern
                    .patterns
                    .iter()
                    .map(|patt| self.resolve_pattern(patt))
                    .collect::<Result<Vec<Pattern>, ResolveError>>()?;
                Pattern::Or(OrPattern::new(patterns))
            }
            AstPattern::Ty(ty_patt) => {
                let resolved_ty = self.resolve_path_ty(&ty_patt.ty)?;
                let resolved_ident = ty_patt.ident.map(|pattern_local| {
                    self.insert_var(pattern_local.ident, pattern_local.id);
                    PatternLocal::new(pattern_local.ident)
                });
                Pattern::Ty(TyPattern::new(resolved_ty, resolved_ident))
            }
            AstPattern::Destructure(de_patt) => {
                let destructure_pattern = self.resolve_destructure_pattern(de_patt)?;
                Pattern::Destructure(destructure_pattern)
            }
            AstPattern::True => Pattern::True,
            AstPattern::False => Pattern::False,
            AstPattern::Float(float) => Pattern::Float(*float),
            AstPattern::Int(int) => Pattern::Int(*int),
            AstPattern::UInt(uint) => Pattern::UInt(*uint),
            AstPattern::String(string) => Pattern::String(*string),
            AstPattern::None => Pattern::None,
        };

        Ok(hir_pattern)
    }

    fn resolve_destructure_pattern(
        &mut self,
        de_patt: &AstDestructurePattern,
    ) -> Result<DestructurePattern, ResolveError> {
        let resolved_ty = self.resolve_path_ty(&de_patt.ty)?;
        let exprs = de_patt
            .exprs
            .iter()
            .map(|expr| self.resolve_destructure_expr(expr))
            .collect::<Result<Vec<LocalDefId>, ResolveError>>()?;
        Ok(DestructurePattern::new(resolved_ty, exprs))
    }

    fn resolve_qualified_ident(&mut self, ident: &QualifiedIdent) -> Result<DefId, ResolveError> {
        let module_ns = &self.module.unwrap().ns;
        match ident.ident_type {
            IdentType::Crate => match self.krate.find_definition(ident, false) {
                None => Err(QualifiedIdentNotFound(ident.clone())),
                Some(crate_def) => match crate_def {
                    CrateDef::Module(_, _) => Err(ExpectedValueWasModule),
                    CrateDef::Enum(_, enum_def) => Ok(enum_def.id),
                    CrateDef::Value(name, val) => Ok(val),
                },
            },
            IdentType::LocalOrUse => {
                if let Some(ident) = ident.is_single() {
                    // Has to be a local type or generic
                    // Check first if it is a generic and then if it is in the module ns
                    self.find_var(ident)
                        .or_else(|| self.find_generic_param(ident))
                        .or_else(|| self.find_enum_member(ident))
                        .map(|var| var.to_def_id(self.krate.crate_id))
                        .or_else(|| module_ns.find_value(ident))
                        .ok_or(VarNotFound(ident))
                } else if let Some(id) = module_ns.find_ident(ident, false) {
                    Ok(id)
                } else if let Some(module_id) = module_ns.find_module(ident.first()) {
                    // Else it could be from a used module
                    let module = self.get_module(module_id);
                    module
                        .ns
                        .find_ident(ident, true)
                        .ok_or(QualifiedIdentNotFound(ident.clone()))
                } else if let Some(krate) = self.crates.get(&ident.first()) {
                    // It must be from an external crate
                    match krate.find_definition(ident, true) {
                        None => Err(QualifiedIdentNotFound(ident.clone())),
                        Some(crate_def) => match crate_def {
                            CrateDef::Module(_, _) => Err(ExpectedValueWasModule),
                            CrateDef::Enum(_, enum_def) => Ok(enum_def.id),
                            CrateDef::Value(_, val) => Ok(val),
                        },
                    }
                } else {
                    Err(QualifiedIdentNotFound(ident.clone()))
                }
            }
        }
    }

    fn resolve_closure_params(
        &mut self,
        params: &[AstClosureParam],
    ) -> Result<ClosureParams, ResolveError> {
        // TODO: Add vars to scope
        let params: StrMap<ClosureParam> = params
            .iter()
            .map(|param| (param.ident.ident, ClosureParam::new(param.ident)))
            .collect();
        Ok(ClosureParams::from(params))
    }
}

mod tests {
    use itertools::Itertools;

    use snap::snapshot;

    use crate::compiler::compiler::{Application, CompileError, Compiler, CompilerCtxt};
    use crate::compiler::hir::HirCrate;
    use crate::compiler::resolver::ResolveError;
    use crate::compiler::types::StrMap;
    use crate::compiler::StringInterner;
    use crate::util::utils;

    #[cfg(test)]
    type ResolvedCrates = (StringInterner, StrMap<HirCrate>);

    #[cfg(test)]
    fn resolve_crate(name: &str) -> ResolvedCrates {
        let mut compiler = Compiler::default();
        let main_crate = utils::resolve_test_krate_path(name);
        let crate_path = main_crate.clone().parent().unwrap();
        let application = Application::new(&main_crate, main_crate.parent().unwrap());
        let mut crates = compiler.parse_crates(&application).unwrap();
        compiler.validate_crates(&crates).unwrap();

        let resolved_crates = compiler.resolve_crates(&mut crates);
        let string_interner = StringInterner::from(CompilerCtxt::from(compiler));
        match resolved_crates {
            Ok(crates) => (string_interner, crates),
            Err(error) => {
                if let CompileError::ResolveErrors(resolve_errors) = error {
                    for error in resolve_errors {
                        match error {
                            ResolveError::QualifiedIdentNotFound(qualified_ident) => {
                                let ident = qualified_ident
                                    .idents
                                    .iter()
                                    .map(|ident| ident.ident)
                                    .map(|ident| string_interner.resolve(&ident.into()))
                                    .collect_vec();
                                dbg!(ident);
                            }
                            ResolveError::VarNotFound(var) => {
                                let ident = string_interner.resolve(&var.into());
                                dbg!(ident);
                            }
                            _ => {}
                        };
                    }
                };
                panic!()
            }
        }
    }

    #[test]
    #[snapshot]
    pub fn hello_world() -> ResolvedCrates {
        resolve_crate("hello_world")
    }

    #[test]
    #[snapshot]
    pub fn mutable_assignment() -> ResolvedCrates {
        resolve_crate("mutable_assignment")
    }

    #[test]
    #[snapshot]
    pub fn geometry_classes() -> ResolvedCrates {
        resolve_crate("geometry_classes")
    }

    #[test]
    #[snapshot]
    pub fn import_function_from_crate() -> ResolvedCrates {
        resolve_crate("complex_arithmetic")
    }

    #[test]
    #[snapshot]
    pub fn rectangle_class() -> ResolvedCrates {
        resolve_crate("rectangle")
    }

    #[test]
    #[snapshot]
    pub fn enum_match() -> ResolvedCrates {
        resolve_crate("enum_matching")
    }

    #[test]
    #[snapshot]
    pub fn path_resolution() -> ResolvedCrates {
        resolve_crate("path_resolution")
    }
}
