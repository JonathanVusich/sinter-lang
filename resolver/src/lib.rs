use std::any::Any;
use std::collections::{HashMap, VecDeque};
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;
use std::ops::Deref;
use std::sync::Arc;

use itertools::Itertools;
use serde::{Deserialize, Serialize};

use ast::{
    ArrayExpr as AstArrayExpr, AstPass, Block as AstBlock, ClassDef, ClassStmt as AstClassStmt,
    ClosureParam as AstClosureParam, DestructureExpr as AstDestructureExpr,
    DestructureExprKind as AstDestructureExprKind, DestructureExprKind,
    DestructurePattern as AstDestructurePattern, EnumDef, EnumMember as AstEnumMember,
    EnumMemberDef, EnumStmt as AstEnumStmt, Expr as AstExpr, ExprKind as AstExprKind,
    Field as AstField, FieldDef, Fields as AstFields, FnDef, FnSelfStmt as AstFnSelfStmt,
    FnSelfStmt, FnSig as AstFnSig, FnStmt as AstFnStmt, GenericParams as AstGenericParams,
    Generics as AstGenerics, GlobalLetStmt as AstGlobalLetStmt, GlobalVarDef, IdentType, Item,
    ItemKind as AstItemKind, ItemKind, MatchArm as AstMatchArm, MaybeFnDef, Module, ModuleNS,
    ModulePath, Params as AstParams, PathExpr as AstPathExpr, PathTy as AstPathTy,
    Pattern as AstPattern, QualifiedIdent, Segment as AstSegment, Stmt as AstStmt,
    StmtKind as AstStmtKind, TraitBound as AstTraitBound, TraitDef,
    TraitImplStmt as AstTraitImplStmt, TraitStmt as AstTraitStmt, Ty as AstTy, TyKind,
    TyKind as AstTyKind, ValueDef,
};
use diagnostics::Diagnostics;
use hir::{
    AnonParams, Args, Array, ArrayExpr, AssignExpr, Block, CallExpr, ClassStmt, Closure,
    ClosureExpr, ClosureParam, ClosureParams, DestructureExpr, DestructurePattern, EnumMember,
    EnumMembers, EnumStmt, Expr, Expression, Field, FieldExpr, Fields, FnSig, FnStmt, FnStmts,
    ForStmt, GenericParam, GenericParams, Generics, GlobalLetStmt, HirCrate, HirMap, HirNode,
    HirNodeKind, IfStmt, IndexExpr, InfixExpr, LetStmt, LocalDef, MatchArm, MatchExpr, OrPattern,
    Param, Params, PathExpr, PathTy, Pattern, PatternLocal, Primitive, Res, ReturnStmt, Segment,
    Stmt, TraitBound, TraitImplStmt, TraitStmt, Ty, TyPattern, UnaryExpr, WhileStmt,
};
use id::{CrateId, DefId, LocalDefId, ModuleId};
use interner::{InternedStr, StringInterner};
use krate::{Crate, CrateDef, CrateLookup};
use span::Span;
use types::{LDefMap, StrMap};

pub fn resolve(
    string_interner: &StringInterner,
    diagnostics: &mut Diagnostics,
    crates: &mut StrMap<Crate>,
) -> Option<HirMap> {
    let resolver = Resolver::new(string_interner, diagnostics, crates);
    resolver.resolve()
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
            Scope::Fn { params, vars, .. } => {
                // We need to retrieve from the vars first because they may have redefined the params.
                vars.get(&ident).or_else(|| params.get(&ident)).copied()
            }
            Scope::MatchArm { vars } => vars.get(&ident).copied(),
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
pub enum ResolveErrKind {
    /// Error from looking up definition in crate
    QualifiedIdentNotFound(QualifiedIdent),
    /// Error for getting a fn instead of a class
    ExpectedDefinition(QualifiedIdent),
    /// Path not found
    PathNotFound(AstPathExpr),
    /// Invalid generics
    InvalidGenerics(Generics),

    /// Error for single segment path that points to crate
    InvalidPath,
    /// Error
    VarNotFound(InternedStr),
    /// Error
    FnNotFound(InternedStr),
    /// Error when resolving path, expected value but was module
    ExpectedValueWasModule,
    /// Duplicate nodes created for a single local definition.
    DuplicateLocalDefIds,
    /// Error for duplicate definitions
    DuplicateDefinition { existing: DefId, new: DefId },
}

impl Display for ResolveErrKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        // TODO: Implement pretty printing
        Debug::fmt(self, f)
    }
}

impl Error for ResolveErrKind {}

type ResolveResult = Option<()>;

struct Resolver<'a> {
    string_interner: &'a StringInterner,
    diagnostics: &'a mut Diagnostics,
    krates: &'a mut StrMap<Crate>,
}

impl<'a> Resolver<'a> {
    fn new(
        string_interner: &'a StringInterner,
        diagnostics: &'a mut Diagnostics,
        krates: &'a mut StrMap<Crate>,
    ) -> Self {
        Self {
            string_interner,
            diagnostics,
            krates,
        }
    }

    fn resolve(mut self) -> Option<HirMap> {
        let mut hir_map = HirMap::default();
        // We are building lookup maps for each module so that we can query the types and constants in that module
        // when resolving use stmts later.
        self.build_crate_ns()?;

        for krate in self.krates.values() {
            let crate_resolver = CrateResolver::new(self.string_interner, krate, self.krates);
            hir_map.insert(crate_resolver.resolve()?);
        }

        Some(hir_map)
    }

    fn build_crate_ns(&mut self) -> Option<()> {
        // Generate the initial module ns
        for krate in self.krates.values_mut() {
            let krate_id = krate.crate_id;
            for module in krate.modules_mut() {
                let module_ns = generate_mod_values(module, krate_id);
                module.namespace = module_ns;
            }
        }

        // Second pass we need to process all use stmts since we now have all of the values
        // populated for each module.
        let mut module_crate_defs = HashMap::<ModuleId, Vec<CrateDef>>::default();
        for krate in self.krates.values() {
            for module in krate.modules() {
                for item in module.items.iter() {
                    if let ItemKind::Use(use_stmt) = &item.kind {
                        let crate_def = match &use_stmt.path.ident_type {
                            IdentType::Crate => krate.find_definition(&use_stmt.path, false),
                            IdentType::LocalOrUse => {
                                let krate = self.krates.get(&use_stmt.path.first()).unwrap();
                                krate.find_definition(&use_stmt.path, true)
                            }
                        }?;
                        module_crate_defs
                            .entry(module.id)
                            .or_default()
                            .push(crate_def);
                    }
                }
            }
        }

        for krate in self.krates.values_mut() {
            for module in krate.modules_mut() {
                let mut modules = StrMap::default();
                let mut values = StrMap::default();
                modules.extend(module.namespace.modules.iter());
                values.extend(
                    module
                        .namespace
                        .values
                        .iter()
                        .map(|(val, def)| (*val, def.clone())),
                );

                if let Some(crate_defs) = module_crate_defs.remove(&module.id) {
                    for crate_def in crate_defs {
                        match crate_def {
                            CrateDef::Module(name, mod_id) => {
                                modules.insert(name, mod_id);
                            }
                            CrateDef::Value(name, def_id) => {
                                values.insert(name, def_id);
                            }
                        }
                    }
                }

                let ns = ModuleNS {
                    modules: modules.into(),
                    values: values.into(),
                };
                module.namespace = ns;
            }
        }

        Some(())
    }
}

fn generate_mod_values(module: &Module, krate_id: CrateId) -> ModuleNS {
    let mut values = StrMap::default();
    for item in &module.items {
        let def_id = item.id.to_def_id(krate_id);
        match &item.kind {
            ItemKind::GlobalLet(global_let_stmt) => {
                values.insert(
                    global_let_stmt.name.ident,
                    ValueDef::GlobalVar(GlobalVarDef::new(def_id)),
                );
            }
            ItemKind::Class(class_stmt) => {
                let fields = class_stmt
                    .fields
                    .into_iter()
                    .map(|field| {
                        (
                            field.name.ident,
                            FieldDef::new(field.id.to_def_id(krate_id)),
                        )
                    })
                    .collect::<StrMap<FieldDef>>();
                let fns = class_stmt
                    .self_fns
                    .iter()
                    .map(|self_fn| {
                        (
                            self_fn.sig.name.ident,
                            FnDef::new(self_fn.id.to_def_id(krate_id)),
                        )
                    })
                    .collect::<StrMap<FnDef>>();
                values.insert(
                    class_stmt.name.ident,
                    ValueDef::Class(ClassDef::new(def_id, fields, fns)),
                );
            }
            ItemKind::Enum(enum_stmt) => {
                let fns = Arc::new(
                    enum_stmt
                        .self_fns
                        .iter()
                        .map(|self_fn| {
                            (
                                self_fn.sig.name.ident,
                                FnDef::new(self_fn.id.to_def_id(krate_id)),
                            )
                        })
                        .collect(),
                );
                let members = Arc::new(
                    enum_stmt
                        .members
                        .iter()
                        .map(|member| {
                            let fns = Arc::new(
                                member
                                    .self_fns
                                    .iter()
                                    .map(|self_fn| {
                                        (
                                            self_fn.sig.name.ident,
                                            FnDef::new(self_fn.id.to_def_id(krate_id)),
                                        )
                                    })
                                    .collect(),
                            );
                            (
                                member.name,
                                EnumMemberDef::new(member.id.to_def_id(krate_id), fns),
                            )
                        })
                        .collect(),
                );
                values.insert(
                    enum_stmt.name.ident,
                    ValueDef::Enum(EnumDef::new(def_id, members, fns)),
                );
            }
            ItemKind::Trait(trait_stmt) => {
                let fns = Arc::new(
                    trait_stmt
                        .self_fns
                        .iter()
                        .map(|self_fn| {
                            (
                                self_fn.sig.name.ident,
                                FnDef::new(self_fn.id.to_def_id(krate_id)),
                            )
                        })
                        .collect(),
                );
                values.insert(
                    trait_stmt.name.ident,
                    ValueDef::Trait(TraitDef::new(def_id, fns)),
                );
            }
            ItemKind::Fn(fn_stmt) => {
                values.insert(fn_stmt.sig.name.ident, ValueDef::Fn(FnDef::new(def_id)));
            }
            ItemKind::Use(use_stmt) => {}
            ItemKind::TraitImpl(trait_impl_stmt) => {}
        }
    }
    ModuleNS {
        modules: Arc::new(Default::default()),
        values: values.into(),
    }
}

struct CrateResolver<'a> {
    string_interner: &'a StringInterner,
    krate: &'a Crate,
    crates: &'a StrMap<Crate>,
    crate_lookup: CrateLookup<'a>,
    module: Option<&'a Module>,
    items: Vec<LocalDefId>,
    nodes: LDefMap<HirNode>,
    scopes: Vec<Scope>,
}

impl<'a> CrateResolver<'a> {
    fn new(
        string_interner: &'a StringInterner,
        krate: &'a Crate,
        crates: &'a StrMap<Crate>,
    ) -> Self {
        let crate_lookup = crates
            .values()
            .sorted_by_key(|krate| krate.crate_id)
            .collect_vec()
            .into();
        Self {
            string_interner,
            krate,
            crates,
            crate_lookup,
            module: Default::default(),
            items: Default::default(),
            nodes: Default::default(),
            scopes: Default::default(),
        }
    }

    fn resolve(mut self) -> Option<HirCrate> {
        for module in self.krate.modules() {
            self.resolve_module(module);

            // We have to remember to clear out all scopes and reset the module scope
            self.scopes.clear();
        }

        Some(HirCrate::new(
            self.krate.name,
            self.krate.crate_id,
            self.items,
            self.nodes,
        ))
    }

    fn resolve_module(&mut self, module: &'a Module) {
        self.module = Some(module);
        module.items.iter().for_each(|item| self.visit_item(item));
    }

    fn visit_item(&mut self, item: &Item) {
        let span = item.span;
        let id = item.id;

        let hir_node = match &item.kind {
            AstItemKind::Use(use_stmt) => None,
            AstItemKind::GlobalLet(let_stmt) => self.resolve_global_let_stmt(let_stmt, span, id),
            AstItemKind::Class(class_stmt) => self.resolve_class_stmt(class_stmt, span, id),
            AstItemKind::Enum(enum_stmt) => self.resolve_enum_stmt(enum_stmt, span, id),
            AstItemKind::Trait(trait_stmt) => self.resolve_trait_stmt(trait_stmt, span, id),
            AstItemKind::TraitImpl(trait_impl_stmt) => {
                self.resolve_trait_impl_stmt(trait_impl_stmt, span, id)
            }
            AstItemKind::Fn(fn_stmt) => self.resolve_fn_stmt(fn_stmt, span, id),
        };

        match hir_node {
            Some(hir_node) => {
                self.items.push(id);
                self.nodes.insert(id, hir_node);
            }
            _ => {}
        }
    }

    fn duplicate_definition(&self, existing: LocalDefId, new: LocalDefId) {
        todo!();
    }

    fn resolve_global_let_stmt(
        &mut self,
        let_stmt: &AstGlobalLetStmt,
        span: Span,
        id: LocalDefId,
    ) -> Option<HirNode> {
        // Lower expression
        let ty = self.resolve_ty(&let_stmt.ty)?;
        let expr = self.resolve_expr(&let_stmt.initializer)?;

        // We don't need to insert constants into a local scope since it is already part of the module ns.
        let hir_node = HirNode::new(
            HirNodeKind::GlobalLet(GlobalLetStmt::new(let_stmt.name.ident, ty, expr)),
            span,
            id,
        );
        Some(hir_node)
    }

    fn resolve_class_stmt(
        &mut self,
        class_stmt: &AstClassStmt,
        span: Span,
        id: LocalDefId,
    ) -> Option<HirNode> {
        self.scopes.push(Scope::Class {
            id: id.to_def_id(self.krate.crate_id),
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
        Some(HirNode::new(HirNodeKind::Class(hir_class), span, id))
    }

    fn maybe_resolve_generics(
        &mut self,
        generics: &Option<AstGenerics>,
    ) -> Result<Option<Generics>, ()> {
        match generics {
            Some(generics) => match self.resolve_generics(generics) {
                Some(generics) => Ok(Some(generics)),
                None => Err(()),
            },
            None => Ok(None),
        }
    }

    fn resolve_generics(&mut self, generics: &AstGenerics) -> Option<Generics> {
        let mut hir_generics = Vec::with_capacity(generics.len());
        for generic in generics {
            hir_generics.push(self.resolve_ty(generic)?);
        }
        Some(Generics::from(hir_generics))
    }

    fn resolve_params(&mut self, params: &AstParams) -> Option<Params> {
        let mut hir_params = StrMap::default();
        for param in params {
            self.insert_param(param.name.ident, param.id)?;

            let ty = self.resolve_ty(&param.ty)?;

            self.insert_node(
                param.id,
                HirNode::new(
                    HirNodeKind::Param(Param::new(param.name, ty, param.mutability)),
                    param.span,
                    param.id,
                ),
            );

            hir_params.insert(param.name.ident, param.id);
        }
        Some(Params::from(hir_params))
    }

    fn resolve_generic_params(
        &mut self,
        generic_params: &AstGenericParams,
    ) -> Option<GenericParams> {
        let mut generics = StrMap::default();
        for param in generic_params {
            self.insert_generic_param(param.name.ident, param.id)?;

            let trait_bound = match &param.trait_bound {
                None => None,
                Some(AstTy {
                    kind: AstTyKind::TraitBound { trait_bound },
                    span,
                    id,
                }) => Some(self.resolve_trait_bound(trait_bound, *span, *id)?),
                _ => unreachable!(),
            };
            /*
                We always want to insert the node, even if it clashes with another generic param.
                Otherwise we can't look it up later when handling errors.
            */
            self.insert_node(
                param.id,
                HirNode::new(
                    HirNodeKind::Ty(Ty::GenericParam(GenericParam::new(param.name, trait_bound))),
                    param.span,
                    param.id,
                ),
            );

            generics.insert(param.name.ident, param.id);
        }
        Some(GenericParams::from(generics))
    }

    fn insert_field(&mut self, field_name: InternedStr, id: LocalDefId) -> Option<()> {
        match self.scopes.last_mut().unwrap().insert_field(field_name, id) {
            None => Some(()),
            Some(existing) => {
                self.duplicate_definition(existing, id);
                None
            }
        }
    }

    fn insert_var(&mut self, var_name: InternedStr, id: LocalDefId) {
        self.scopes.last_mut().unwrap().insert_var(var_name, id);
    }

    fn insert_self_fn(&mut self, fn_name: InternedStr, id: LocalDefId) -> Option<()> {
        match self.scopes.last_mut().unwrap().insert_self_fn(fn_name, id) {
            None => Some(()),
            Some(existing) => {
                self.duplicate_definition(existing, id);
                None
            }
        }
    }

    fn insert_param(&mut self, ident: InternedStr, id: LocalDefId) -> Option<()> {
        match self.scopes.last_mut().unwrap().insert_param(ident, id) {
            None => Some(()),
            Some(existing) => {
                self.duplicate_definition(existing, id);
                None
            }
        }
    }

    fn insert_enum_member(&mut self, ident: InternedStr, id: LocalDefId) -> Option<()> {
        match self
            .scopes
            .last_mut()
            .unwrap()
            .insert_enum_member(ident, id)
        {
            None => Some(()),
            Some(existing) => {
                self.duplicate_definition(existing, id);
                None
            }
        }
    }

    fn find_item_definition(&mut self, ident: InternedStr) -> Option<DefId> {
        self.scopes
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
    }

    fn find_enum_member(&mut self, ident: InternedStr) -> Option<LocalDefId> {
        self.scopes
            .iter()
            .rev()
            .find_map(|scope| scope.contains_enum_member(ident))
    }

    fn insert_node(&mut self, id: LocalDefId, hir_node: HirNode) {
        let index: usize = id.into();
        if self.nodes.contains_key(&id) {
            dbg!(&index);
            dbg!(&hir_node);
            dbg!(&self.nodes);
        }
        assert!(self.nodes.insert(id, hir_node).is_none());
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

    fn insert_generic_param(&mut self, ident: InternedStr, id: LocalDefId) -> Option<()> {
        // Don't need to check in reverse order since we are checking for existence, not last declared.
        if let Some(existing) = self
            .scopes
            .iter()
            .find_map(|scope| scope.contains_generic(ident))
        {
            self.duplicate_definition(existing, id);
            return None;
        }

        // Insert generic into last scope (will return error if this is not a valid scope)
        match self
            .scopes
            .last_mut()
            .unwrap()
            .insert_generic_param(ident, id)
        {
            None => Some(()),
            Some(existing) => {
                self.duplicate_definition(existing, id);
                None
            }
        }
    }

    fn resolve_fields(&mut self, ast_fields: &AstFields) -> Option<Fields> {
        let mut fields = StrMap::default();
        for field in ast_fields {
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
            );
            fields.insert(ident.ident, *id);
        }
        Some(Fields::from(fields))
    }

    fn resolve_enum_stmt(
        &mut self,
        enum_stmt: &AstEnumStmt,
        span: Span,
        id: LocalDefId,
    ) -> Option<HirNode> {
        self.scopes.push(Scope::Enum {
            id: id.to_def_id(self.krate.crate_id),
            members: Default::default(),
            self_fns: Default::default(),
            generics: Default::default(),
        });

        let generic_params = self.resolve_generic_params(&enum_stmt.generic_params)?;
        let enum_members = self.resolve_enum_members(&enum_stmt.members)?;
        let member_fns = self.resolve_self_fn_stmts(&enum_stmt.self_fns)?;

        self.scopes.pop();

        let hir_enum = EnumStmt::new(enum_stmt.name, generic_params, enum_members, member_fns);
        Some(HirNode::new(HirNodeKind::Enum(hir_enum), span, id))
    }

    fn resolve_enum_members(
        &mut self,
        enum_stmt_members: &Vec<AstEnumMember>,
    ) -> Option<EnumMembers> {
        let mut enum_members = StrMap::default();
        for member in enum_stmt_members {
            self.insert_enum_member(member.name, member.id)?;

            self.scopes.push(Scope::EnumMember {
                fields: Default::default(),
                self_fns: Default::default(),
            });

            let fields = self.resolve_fields(&member.fields)?;
            let self_fns = self.resolve_self_fn_stmts(&member.self_fns)?;

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
        Some(enum_members.into())
    }

    fn resolve_trait_stmt(
        &mut self,
        trait_stmt: &AstTraitStmt,
        span: Span,
        id: LocalDefId,
    ) -> Option<HirNode> {
        self.scopes.push(Scope::Trait {
            id: id.to_def_id(self.krate.crate_id),
            self_fns: Default::default(),
            generics: Default::default(),
        });

        let generic_params = self.resolve_generic_params(&trait_stmt.generic_params)?;
        let member_fns = self.resolve_self_fn_stmts(&trait_stmt.self_fns)?;

        self.scopes.pop();

        let hir_trait = TraitStmt::new(trait_stmt.name, generic_params, member_fns);
        Some(HirNode::new(HirNodeKind::Trait(hir_trait), span, id))
    }

    fn resolve_trait_impl_stmt(
        &mut self,
        trait_stmt: &AstTraitImplStmt,
        span: Span,
        id: LocalDefId,
    ) -> Option<HirNode> {
        let target_ty = self.resolve_qualified_ident(&trait_stmt.target_ty)?;
        let trait_to_impl = self.resolve_path_ty(&trait_stmt.trait_to_impl)?;

        self.scopes.push(Scope::TraitImpl {
            target_id: target_ty,
            self_fns: Default::default(),
        });

        let self_fns = self.resolve_self_fn_stmts(&trait_stmt.self_fns)?;

        Some(HirNode::new(
            HirNodeKind::TraitImpl(TraitImplStmt::new(trait_to_impl, target_ty, self_fns)),
            span,
            id,
        ))
    }

    fn resolve_fn_stmt(
        &mut self,
        fn_stmt: &AstFnStmt,
        span: Span,
        id: LocalDefId,
    ) -> Option<HirNode> {
        self.scopes.push(Scope::Fn {
            params: Default::default(),
            generics: Default::default(),
            vars: Default::default(),
        });

        let resolved_sig = self.resolve_fn_sig(&fn_stmt.sig)?;
        let resolved_body = self.maybe_resolve_block(&fn_stmt.body).ok()?;

        self.scopes.pop();

        let name = resolved_sig.name.ident;
        Some(HirNode::new(
            HirNodeKind::Fn(FnStmt::new(resolved_sig, resolved_body)),
            span,
            id,
        ))
    }

    fn resolve_self_fn_stmt(
        &mut self,
        fn_stmt: &AstFnSelfStmt,
    ) -> Option<(InternedStr, LocalDefId)> {
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
        let resolved_body = self.maybe_resolve_block(body).ok()?;

        self.scopes.pop();

        let name = resolved_sig.name.ident;
        self.insert_node(
            *id,
            HirNode::new(
                HirNodeKind::Fn(FnStmt::new(resolved_sig, resolved_body)),
                *span,
                *id,
            ),
        );
        Some((name, *id))
    }

    fn resolve_self_fn_stmts(&mut self, stmts: &Vec<FnSelfStmt>) -> Option<FnStmts> {
        for fn_stmt in stmts {
            self.insert_self_fn(fn_stmt.sig.name.ident, fn_stmt.id)?;
        }

        // This is safe because we have already checked to ensure that there are no name collisions.
        let mut fn_stmts = StrMap::default();
        for fn_stmt in stmts {
            let (name, stmt) = self.resolve_self_fn_stmt(fn_stmt)?;
            fn_stmts.insert(name, stmt);
        }
        Some(FnStmts::from(fn_stmts))
    }

    fn resolve_fn_sig(&mut self, fn_sig: &AstFnSig) -> Option<FnSig> {
        let generic_params = self.resolve_generic_params(&fn_sig.generic_params)?;
        let params = self.resolve_params(&fn_sig.params)?;

        let return_ty = self.maybe_resolve_ty(&fn_sig.return_type)?;

        Some(FnSig::new(fn_sig.name, generic_params, params, return_ty))
    }

    fn maybe_resolve_expr(
        &mut self,
        expr: &Option<Box<AstExpr>>,
    ) -> Result<Option<LocalDefId>, ()> {
        match expr {
            None => Ok(None),
            Some(expr) => match self.resolve_expr(expr) {
                Some(expr) => Ok(Some(expr)),
                None => Err(()),
            },
        }
    }

    fn resolve_expr(&mut self, expr: &AstExpr) -> Option<LocalDefId> {
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
                        .collect::<Option<Vec<_>>>()?
                        .into();

                    Expr::Array(ArrayExpr::Unsized { initializers })
                }
            },
            AstExprKind::Call(call) => {
                let args = call
                    .args
                    .into_iter()
                    .map(|expr| self.resolve_expr(expr))
                    .collect::<Option<Vec<_>>>()?;
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
                    .collect::<Option<Vec<_>>>()?;

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
                // These paths are richer and we have to preserve generic info for type inference
                let path = self.resolve_path(path)?;
                Expr::Path(path)
            }
            AstExprKind::Parentheses(parentheses) => {
                // Special logic for stripping parentheses (since they are just for pretty printing)
                let resolved_expr = self.resolve_expr(&parentheses.expr)?;
                return Some(id);
            }
            AstExprKind::Break => Expr::Break,
            AstExprKind::Continue => Expr::Continue,
        };

        self.insert_node(id, HirNode::new(HirNodeKind::Expr(resolved_expr), span, id));
        Some(id)
    }

    /// This method can resolve qualified idents as well.
    fn resolve_path(&mut self, path_expr: &AstPathExpr) -> Option<PathExpr> {
        let module_ns = &self.module.unwrap().namespace;
        let mut segments = Vec::with_capacity(path_expr.segments.len());
        match path_expr.ident_type {
            IdentType::Crate => {
                segments.push(Segment::new(Res::Crate(self.krate.crate_id), None));
                let mut path = VecDeque::from_iter(&path_expr.segments);
                while !path.is_empty() {
                    let prev_seg = segments.last().unwrap(); // Should be safe
                    let current = path.pop_front().unwrap(); // Should be safe
                    let curr_ident = current.ident.ident;
                    let segment = self.find_secondary_segment(&prev_seg.res, current)?;

                    segments.push(segment);
                }
                self.verify_last_segment(segments.last().unwrap())?;
            }
            IdentType::LocalOrUse => {
                if let Some(segment) = path_expr.is_single() {
                    let candidate = self.find_primary_segment(segment)?;
                    self.verify_last_segment(&candidate)?;
                    segments.push(candidate);
                } else {
                    let mut path = VecDeque::from_iter(&path_expr.segments);
                    let first_seg = self.find_primary_segment(path.pop_front().unwrap())?;
                    segments.push(first_seg);

                    while !path.is_empty() {
                        let prev_seg = segments.last().unwrap(); // Should be safe
                        let current = path.pop_front().unwrap(); // Should be safe
                        let curr_ident = current.ident.ident;
                        let segment = self.find_secondary_segment(&prev_seg.res, current)?;

                        segments.push(segment);
                    }

                    self.verify_last_segment(segments.last().unwrap())?;
                }
            }
        }
        Some(PathExpr::new(segments))
    }

    fn find_primary_segment(&mut self, segment: &AstSegment) -> Option<Segment> {
        let module_ns = &self.module.unwrap().namespace;
        let ident = segment.ident.ident;
        let generics = self.maybe_resolve_generics(&segment.generics).ok()?;

        self.find_var(ident)
            .map(LocalDef::Var)
            .or_else(|| self.find_generic_param(ident).map(LocalDef::Generic))
            .map(Res::Local)
            .or_else(|| module_ns.find_value(ident).cloned().map(Res::ValueDef))
            .or_else(|| {
                self.matches_primitive(ident)
                    .filter(|prim| generics.is_none())
                    .map(Res::Primitive)
            })
            .or_else(|| {
                self.matches_self_ty(ident)
                    .and_then(|()| self.find_item_definition(ident))
                    .and_then(|id| module_ns.values.values().find(|val_def| val_def.id() == id))
                    .cloned()
                    .map(Res::ValueDef)
            })
            .or_else(|| module_ns.find_module(ident).map(Res::Module))
            .or_else(|| {
                self.crates
                    .get(&ident)
                    .map(|krate| krate.crate_id)
                    .map(Res::Crate)
            })
            .map(|res| Segment::new(res, generics.clone()))
            .or_else(|| {
                // TODO: Emit var not found error!
                // VarNotFound(ident).into()
                // self.ctxt.emit_error()
                None
            })
    }

    fn verify_last_segment(&self, segment: &Segment) -> Option<()> {
        match &segment.res {
            Res::Crate(_) | Res::ModuleSegment(_, _) | Res::Module(_) => {
                // TODO: Emit invalid path error
                // Err(InvalidPath.into())
                None
            }
            Res::ValueDef(_) | Res::Fn(_) | Res::Local(_) | Res::Primitive(_) => Some(()),
        }
    }

    fn find_secondary_segment(&mut self, previous: &Res, segment: &AstSegment) -> Option<Segment> {
        let module_ns = &self.module.unwrap().namespace;
        let ident = segment.ident.ident;
        // TODO: Finish generics error handling
        let generics = self.maybe_resolve_generics(&segment.generics).ok()?;

        let res = match previous {
            Res::Crate(krate_id) => {
                let krate = &self.crate_lookup[*krate_id];
                krate
                    .module_trie()
                    .get(&ModulePath::from_iter([ident]))
                    .copied()
                    .map(Res::Module)
                    .unwrap_or_else(|| {
                        Res::ModuleSegment(*krate_id, ModulePath::from_iter([ident]))
                    })
            }
            Res::ModuleSegment(krate_id, path) => {
                let full_path = path.concat(ModulePath::from_iter([ident]));

                let krate = &self.crate_lookup[*krate_id];
                krate
                    .module_trie()
                    .get(&full_path)
                    .copied()
                    .map(Res::Module)
                    .unwrap_or_else(|| Res::ModuleSegment(*krate_id, full_path))
            }
            Res::Module(mod_id) => {
                let module = &self.crate_lookup[*mod_id];
                module
                    .namespace
                    .find_value(ident)
                    .cloned()
                    .map(Res::ValueDef)
                    .or_else(|| {
                        // TODO: Emit compiler error
                        // VarNotFound(ident)
                        None
                    })?
            }
            Res::ValueDef(ValueDef::Enum(enum_def)) => enum_def
                .members
                .get(&ident)
                .cloned()
                .map(|member| Res::ValueDef(ValueDef::EnumMember(member)))
                .or_else(|| {
                    enum_def
                        .fns
                        .get(&ident)
                        .cloned()
                        .map(|fn_def| Res::Fn(MaybeFnDef::Some(fn_def)))
                })
                .or_else(|| {
                    // TODO: Emit compiler error
                    // FnNotFound(ident)
                    None
                })?,
            Res::ValueDef(ValueDef::Class(class_def)) => class_def
                .fns
                .get(&ident)
                .copied()
                .map(|fn_def| Res::Fn(MaybeFnDef::Some(fn_def)))
                .or_else(|| {
                    // TODO: Emit compiler error
                    // FnNotFound(ident)
                    None
                })?,
            Res::ValueDef(ValueDef::EnumMember(member_def)) => member_def
                .fns
                .get(&ident)
                .copied()
                .map(|fn_def| Res::Fn(MaybeFnDef::Some(fn_def)))
                .or_else(|| {
                    // TODO: Emit compiler error
                    // FnNotFound(ident)
                    None
                })?,
            Res::ValueDef(ValueDef::Trait(trait_def)) => trait_def
                .fns
                .get(&ident)
                .copied()
                .map(|fn_def| Res::Fn(MaybeFnDef::Some(fn_def)))
                .or_else(|| {
                    // TODO: Emit compiler error
                    // FnNotFound(ident)
                    None
                })?,
            Res::Primitive(_) => {
                // TODO: Figure out how to handle methods on primitive types
                // Probably just create a stub method
                todo!()
            }
            Res::Local(LocalDef::Generic(generic)) => Res::Fn(MaybeFnDef::None(ident)),
            // Fns, locals and constants cannot have paths!
            Res::Fn(_)
            | Res::Local(_)
            | Res::ValueDef(ValueDef::GlobalVar(_))
            | Res::ValueDef(ValueDef::Fn(_)) => {
                // TODO: Emit compiler error
                // (VarNotFound(ident).into());
                return None;
            }
        };
        Some(Segment::new(res, generics))
    }

    fn matches_primitive(&self, ident: InternedStr) -> Option<Primitive> {
        match self.string_interner.resolve(ident) {
            "u8" => Some(Primitive::U8),
            "u16" => Some(Primitive::U16),
            "u32" => Some(Primitive::U32),
            "u64" => Some(Primitive::U64),
            "i8" => Some(Primitive::I8),
            "i16" => Some(Primitive::I16),
            "i32" => Some(Primitive::I32),
            "i64" => Some(Primitive::I64),
            "f32" => Some(Primitive::F32),
            "f64" => Some(Primitive::F64),
            "str" => Some(Primitive::Str),
            "bool" => Some(Primitive::Boolean),
            "None" => Some(Primitive::None),
            _ => None,
        }
    }

    fn matches_self_ty(&self, ident: InternedStr) -> Option<()> {
        match self.string_interner.resolve(ident) {
            "Self" => Some(()),
            _ => None,
        }
    }

    fn resolve_destructure_expr(
        &mut self,
        destructure_expr: &AstDestructureExpr,
    ) -> Option<LocalDefId> {
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
        );
        Some(id)
    }

    fn maybe_resolve_ty(&mut self, ty: &Option<AstTy>) -> Option<Option<LocalDefId>> {
        if let Some(ty) = ty {
            self.resolve_ty(ty).map(Some)
        } else {
            Some(None)
        }
    }

    fn resolve_ty(&mut self, ty: &AstTy) -> Option<LocalDefId> {
        let span = ty.span;
        let id = ty.id;
        let hir_ty = match &ty.kind {
            TyKind::Array { ty } => Ty::Array(Array::new(self.resolve_ty(ty)?)),
            TyKind::Path { path } => Ty::Path(self.resolve_path_ty(path)?),
            TyKind::TraitBound { trait_bound } => {
                let mut paths = Vec::with_capacity(trait_bound.len());
                for path in trait_bound {
                    let def = self.resolve_qualified_ident(&path.ident)?;
                    let generics = self.resolve_generics(&path.generics)?;
                    paths.push(PathTy::new(def, generics));
                }
                Ty::TraitBound(TraitBound::from(paths))
            }
            TyKind::Closure { params, ret_ty } => {
                let params = params
                    .iter()
                    .map(|param| self.resolve_ty(param))
                    .collect::<Option<Vec<_>>>()?;
                let ret_ty = self.resolve_ty(ret_ty)?;
                Ty::Closure(Closure::new(AnonParams::from(params), ret_ty))
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

                Ty::Path(PathTy::new(item_def, Generics::empty()))
            }
            TyKind::U8 => Ty::Primitive(Primitive::U8),
            TyKind::U16 => Ty::Primitive(Primitive::U16),
            TyKind::U32 => Ty::Primitive(Primitive::U32),
            TyKind::U64 => Ty::Primitive(Primitive::U64),
            TyKind::I8 => Ty::Primitive(Primitive::I8),
            TyKind::I16 => Ty::Primitive(Primitive::I16),
            TyKind::I32 => Ty::Primitive(Primitive::I32),
            TyKind::I64 => Ty::Primitive(Primitive::I64),
            TyKind::F32 => Ty::Primitive(Primitive::F32),
            TyKind::F64 => Ty::Primitive(Primitive::F64),
            TyKind::Boolean => Ty::Primitive(Primitive::Boolean),
            TyKind::Str => Ty::Primitive(Primitive::Str),
            TyKind::None => Ty::Primitive(Primitive::None),
        };

        self.insert_node(id, HirNode::new(HirNodeKind::Ty(hir_ty), span, id));
        Some(id)
    }

    fn resolve_path_ty(&mut self, path_ty: &AstPathTy) -> Option<PathTy> {
        let def = self.resolve_qualified_ident(&path_ty.ident)?;
        let generics = self.resolve_generics(&path_ty.generics)?;
        Some(PathTy::new(def, generics))
    }

    fn resolve_trait_bound(
        &mut self,
        trait_bound: &AstTraitBound,
        span: Span,
        id: LocalDefId,
    ) -> Option<LocalDefId> {
        let mut hir_bound = Vec::with_capacity(trait_bound.len());
        for ty in trait_bound {
            let resolved_ty = self.resolve_path_ty(ty)?;
            hir_bound.push(resolved_ty);
        }

        self.insert_node(
            id,
            HirNode::new(HirNodeKind::Ty(Ty::TraitBound(hir_bound.into())), span, id),
        );

        Some(id)
    }

    fn resolve_stmt(&mut self, stmt: &AstStmt) -> Option<LocalDefId> {
        let span = stmt.span;
        let id = stmt.id;
        let hir_stmt = match &stmt.kind {
            AstStmtKind::Let(let_stmt) => {
                self.insert_var(let_stmt.ident.ident, id);

                let resolved_ty = self.maybe_resolve_ty(&let_stmt.ty)?;
                let resolved_initializer = self.maybe_resolve_expr(&let_stmt.initializer).ok()?;

                Stmt::Let(LetStmt::new(
                    let_stmt.ident,
                    let_stmt.mutability,
                    resolved_ty,
                    resolved_initializer,
                ))
            }
            AstStmtKind::For(for_stmt) => {
                self.scopes.push(Scope::Block {
                    vars: StrMap::default(),
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
                let if_false = self.maybe_resolve_block(&if_stmt.if_false).ok()?;
                self.scopes.pop();

                Stmt::If(IfStmt::new(condition, if_true, if_false))
            }
            AstStmtKind::Return(return_stmt) => {
                let maybe_expr = self.maybe_resolve_expr(&return_stmt.value).ok()?;
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
        );

        Some(stmt.id)
    }

    fn maybe_resolve_block(&mut self, block: &Option<AstBlock>) -> Result<Option<LocalDefId>, ()> {
        match block {
            None => Ok(None),
            Some(block) => match self.resolve_block(block) {
                Some(block) => Ok(Some(block)),
                None => Err(()),
            },
        }
    }

    fn resolve_block(&mut self, block: &AstBlock) -> Option<LocalDefId> {
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
        );

        Some(block.id)
    }

    fn resolve_match_arm(&mut self, arm: &AstMatchArm) -> Option<MatchArm> {
        self.scopes.push(Scope::MatchArm {
            vars: Default::default(),
        });

        let pattern = self.resolve_pattern(&arm.pattern)?;

        let body = self.resolve_stmt(&arm.body)?;

        self.scopes.pop();

        Some(MatchArm::new(pattern, body))
    }

    fn resolve_pattern(&mut self, pattern: &AstPattern) -> Option<Pattern> {
        let hir_pattern = match pattern {
            AstPattern::Wildcard => Pattern::Wildcard,
            AstPattern::Or(or_pattern) => {
                let patterns = or_pattern
                    .patterns
                    .iter()
                    .map(|patt| self.resolve_pattern(patt))
                    .collect::<Option<Vec<_>>>()?;
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

        Some(hir_pattern)
    }

    fn resolve_destructure_pattern(
        &mut self,
        de_patt: &AstDestructurePattern,
    ) -> Option<DestructurePattern> {
        let resolved_ty = self.resolve_path_ty(&de_patt.ty)?;
        let exprs = de_patt
            .exprs
            .iter()
            .map(|expr| self.resolve_destructure_expr(expr))
            .collect::<Option<Vec<_>>>()?;
        Some(DestructurePattern::new(resolved_ty, exprs))
    }

    fn resolve_qualified_ident(&mut self, ident: &QualifiedIdent) -> Option<DefId> {
        let module_ns = &self.module.unwrap().namespace;
        match ident.ident_type {
            IdentType::Crate => match self.krate.find_definition(ident, false) {
                None => {
                    // TODO: Emit error!
                    // Err(QualifiedIdentNotFound(ident.clone()).into())
                    todo!()
                }
                Some(crate_def) => match crate_def {
                    CrateDef::Module(_, _) => {
                        // TODO: Emit error!
                        // Err(ExpectedValueWasModule.into())
                        todo!()
                    }
                    CrateDef::Value(_, val) => Some(val.id()),
                },
            },
            IdentType::LocalOrUse => {
                if let Some(ident) = ident.is_single() {
                    self.find_generic_param(ident)
                        .map(|param| param.to_def_id(self.krate.crate_id))
                        .or_else(|| module_ns.find_value(ident).map(|value_def| value_def.id()))
                        .or_else(|| {
                            // TODO: Emit compiler warning
                            // VarNotFound(ident).into()
                            None
                        })
                } else if let Some(id) = module_ns.find_ident(ident) {
                    Some(id)
                } else if let Some(module_id) = module_ns.find_module(ident.first()) {
                    // Else it could be from a used module
                    let module = &self.crate_lookup[module_id];
                    module.namespace.find_ident_with_module(ident).or_else(|| {
                        // QualifiedIdentNotFound(ident.clone()).into()
                        // self.ctxt.emit_error()
                        todo!()
                    })
                } else if let Some(krate) = self.crates.get(&ident.first()) {
                    // It must be from an external crate
                    match krate.find_definition(ident, true) {
                        None => {
                            // QualifiedIdentNotFound(ident.clone()).into()
                            // self.ctxt.emit_error()
                            todo!()
                        }
                        Some(crate_def) => match crate_def {
                            CrateDef::Module(_, _) => {
                                // ExpectedValueWasModule.into()
                                // self.ctxt.emit_error();
                                todo!()
                            }
                            CrateDef::Value(_, val) => Some(val.id()),
                        },
                    }
                } else {
                    // QualifiedIdentNotFound(ident.clone()).into()
                    // self.ctxt.emit_error()
                    todo!()
                }
            }
        }
    }

    fn resolve_closure_params(&mut self, params: &[AstClosureParam]) -> Option<ClosureParams> {
        // TODO: Add vars to scope
        let params: StrMap<ClosureParam> = params
            .iter()
            .map(|param| (param.ident.ident, ClosureParam::new(param.ident)))
            .collect();
        Some(ClosureParams::from(params))
    }
}

#[cfg(test)]
mod tests {
    use diagnostics::Diagnostics;
    use hir::HirMap;
    use interner::StringInterner;
    use snap::snapshot;

    type ResolvedCrates = (StringInterner, Diagnostics, HirMap);

    fn resolve_crate(name: &str) -> ResolvedCrates {
        let mut compiler = Compiler::default();
        let main_crate = &sources::resolve_test_krate_path(name);
        let crate_path = main_crate.parent().unwrap();
        let application = Application::Path {
            main_crate,
            crate_path,
        };
        let mut crates = compiler.parse_crates(application).unwrap();
        compiler.validate_crates(&crates).unwrap();

        let hir_map = compiler.resolve_crates(&mut crates).unwrap();
        let CompilerCtxt {
            string_interner,
            diagnostics,
            ..
        } = CompilerCtxt::from(compiler);
        (string_interner, diagnostics, hir_map)
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
