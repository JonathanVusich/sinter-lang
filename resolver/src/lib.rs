#![allow(unused)]

use bumpalo::Bump;
use std::collections::{HashMap, VecDeque};
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;
use std::ops::Deref;
use std::sync::Arc;

use arena::Arena;
use itertools::Itertools;

use diagnostics::{Diagnostic, Diagnostics};
use hir::{
    AnonParams, Args, ArrayExpr, AssignExpr, Block, CallExpr, ClassDef, Closure, ClosureExpr,
    ClosureParam, ClosureParams, Constant, DestructureExpr, DestructurePattern, EnumDef, Expr,
    ExprKind, Expression, Field, FieldExpr, Fields, FnDef, FnSig, FnStmts, ForStmt, GenericParam,
    GenericParams, Generics, HirCrate, HirMap, IfStmt, IndexExpr, InfixExpr, Item, ItemKind,
    LetStmt, LocalDef, LocalVar, MatchArm, MatchArms, MatchExpr, MemberDef, MemberDefs, Node,
    OrPattern, Param, Params, PathExpr, PathTy, Pattern, Primitive, Res, ReturnStmt, Segment, Stmt,
    TraitBound, TraitDef, TraitImplDef, Ty, TyKind, TyPattern, UnaryExpr, WhileStmt,
};
use id::{CrateId, DefId, LocalDefId, ModuleId};
use interner::{InternedStr, StringInterner};
use krate::{Crate, CrateDef, CrateLookup};
use span::Span;
use types::{LDefMap, StrMap};

pub fn resolve<'hir>(
    string_interner: &'hir StringInterner,
    diagnostics: &'hir mut Diagnostics,
    arena: &'hir mut Bump,
    crates: &'hir mut StrMap<Crate>,
) -> Option<HirMap<'hir>> {
    let resolver = Resolver::new(string_interner, diagnostics, arena, crates);
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
        params: StrMap<LocalVar>,
        generics: StrMap<LocalDefId>,
        vars: StrMap<LocalVar>,
    },
    MatchArm {
        vars: StrMap<LocalVar>,
    },
    Block {
        vars: StrMap<LocalVar>,
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
    pub fn contains_var(&self, ident: InternedStr) -> Option<LocalVar> {
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

    pub fn insert_var(&mut self, local_var: LocalVar) {
        let vars = match self {
            Scope::Fn { vars, .. } => vars,
            Scope::MatchArm { vars } => vars,
            _ => panic!("Cannot insert var into this scope!"),
        };
        vars.insert(local_var.ident, local_var);
    }

    pub fn insert_self_fn(&mut self, ident: InternedStr, id: LocalDefId) -> Option<LocalDefId> {
        let self_fns = match self {
            Scope::Class { self_fns, .. } => self_fns,
            Scope::Enum { self_fns, .. } => self_fns,
            Scope::EnumMember { self_fns, .. } => self_fns,
            Scope::Trait { self_fns, .. } => self_fns,
            Scope::TraitImpl { self_fns, .. } => self_fns,
            _ => {
                panic!("Cannot insert self fn into this scope!")
            }
        };
        self_fns.insert(ident, id)
    }

    pub fn insert_param(&mut self, local_var: LocalVar) -> Option<LocalVar> {
        let params = match self {
            Scope::Fn { params, .. } => params,
            _ => panic!("Cannot insert param into this scope!"),
        };
        params.insert(local_var.ident, local_var)
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

type ResolveResult = Option<()>;

struct Resolver<'hir> {
    string_interner: &'hir StringInterner,
    diagnostics: &'hir mut Diagnostics,
    allocator: &'hir Bump,
    krates: &'hir mut StrMap<Crate>,
}

impl<'hir> Resolver<'hir> {
    fn new(
        string_interner: &'hir StringInterner,
        diagnostics: &'hir mut Diagnostics,
        allocator: &'hir Bump,
        krates: &'hir mut StrMap<Crate>,
    ) -> Self {
        Self {
            string_interner,
            diagnostics,
            allocator,
            krates,
        }
    }

    fn resolve(mut self) -> Option<HirMap<'hir>> {
        let mut hir_map = HirMap::default();
        // We are building lookup maps for each module so that we can query the types and constants in that module
        // when resolving use stmts later.
        self.build_crate_ns()?;

        for krate in self.krates.values() {
            let crate_resolver =
                CrateResolver::new(self.string_interner, self.allocator, krate, self.krates);
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

        let mut missing_defs = false;
        // Second pass we need to process all use stmts since we now have all of the values
        // populated for each module.
        let mut module_crate_defs = HashMap::<ModuleId, Vec<CrateDef>>::default();
        for krate in self.krates.values() {
            for module in krate.modules() {
                for item in module.items.iter() {
                    if let ast::ItemKind::Use(use_stmt) = &item.kind {
                        let crate_def = match &use_stmt.path.ident_type {
                            ast::IdentType::Crate => krate.find_definition(&use_stmt.path, false),
                            ast::IdentType::LocalOrUse => self
                                .krates
                                .get(&use_stmt.path.first())
                                .and_then(|krate| krate.find_definition(&use_stmt.path, true)),
                        };
                        // Handle case where definition is not found.
                        match crate_def {
                            Some(crate_def) => {
                                module_crate_defs
                                    .entry(module.id)
                                    .or_default()
                                    .push(crate_def);
                            }
                            None => {
                                // We need to stop execution since we are unable to resolve all references.
                                missing_defs = true;
                                let error_message = format!(
                                    "Unable to resolve definition for {}",
                                    use_stmt.path.format(self.string_interner)
                                );
                                self.diagnostics.push(Diagnostic::Error(error_message))
                            }
                        }
                    }
                }
            }
        }

        // Do an early return since not all definitions were able to be resolved.
        if missing_defs {
            return None;
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

                let ns = ast::ModuleNS {
                    modules: modules.into(),
                    values: values.into(),
                };
                module.namespace = ns;
            }
        }

        Some(())
    }
}

fn generate_mod_values(module: &ast::Module, krate_id: CrateId) -> ast::ModuleNS {
    let mut values = StrMap::default();
    for item in &module.items {
        let def_id = item.id.to_def_id(krate_id);
        match &item.kind {
            ast::ItemKind::GlobalLet(global_let_stmt) => {
                values.insert(
                    global_let_stmt.local_var.ident,
                    ast::ValueDef::GlobalVar(def_id),
                );
            }
            ast::ItemKind::Class(class_stmt) => {
                let fields = class_stmt
                    .fields
                    .into_iter()
                    .map(|field| ast::FieldDef::new(field.id.to_def_id(krate_id), field.name.ident))
                    .collect();
                let fns = class_stmt
                    .self_fns
                    .iter()
                    .map(|self_fn| {
                        FnDef::new(self_fn.id.to_def_id(krate_id), self_fn.sig.name.ident)
                    })
                    .collect();
                values.insert(
                    class_stmt.name.ident,
                    ast::ValueDef::Class(ClassDef::new(def_id, fields, fns)),
                );
            }
            ast::ItemKind::Enum(enum_stmt) => {
                let members = enum_stmt
                    .members
                    .iter()
                    .map(|member| {
                        let fns = member
                            .self_fns
                            .iter()
                            .map(|self_fn| {
                                FnDef::new(self_fn.id.to_def_id(krate_id), self_fn.sig.name.ident)
                            })
                            .collect();
                        ast::EnumMemberDef::new(member.id.to_def_id(krate_id), member.name, fns)
                    })
                    .collect();
                let fns = enum_stmt
                    .self_fns
                    .iter()
                    .map(|self_fn| {
                        FnDef::new(self_fn.id.to_def_id(krate_id), self_fn.sig.name.ident)
                    })
                    .collect();
                values.insert(
                    enum_stmt.name.ident,
                    ast::ValueDef::Enum(EnumDef::new(def_id, members, fns)),
                );
            }
            ast::ItemKind::Trait(trait_stmt) => {
                let fns = trait_stmt
                    .self_fns
                    .iter()
                    .map(|self_fn| {
                        FnDef::new(self_fn.id.to_def_id(krate_id), self_fn.sig.name.ident)
                    })
                    .collect();
                values.insert(
                    trait_stmt.name.ident,
                    ast::ValueDef::Trait(TraitDef::new(def_id, fns)),
                );
            }
            ast::ItemKind::Fn(fn_stmt) => {
                values.insert(fn_stmt.sig.name.ident, ast::ValueDef::Fn(def_id));
            }
            ast::ItemKind::Use(use_stmt) => {}
            ast::ItemKind::TraitImpl(trait_impl_stmt) => {}
        }
    }
    ast::ModuleNS {
        modules: Arc::new(Default::default()),
        values: Arc::new(values),
    }
}

struct CrateResolver<'hir> {
    string_interner: &'hir StringInterner,
    allocator: &'hir Bump,
    krate: &'hir Crate,
    crates: &'hir StrMap<Crate>,
    crate_lookup: CrateLookup<'hir>,
    module: Option<&'hir ast::Module>,
    nodes: LDefMap<Node<'hir>>,
    items: Vec<LocalDefId>,
    scopes: Vec<Scope>,
}

impl<'hir> CrateResolver<'hir> {
    fn new(
        string_interner: &'hir StringInterner,
        arena: &'hir Bump,
        krate: &'hir Crate,
        crates: &'hir StrMap<Crate>,
    ) -> Self {
        let crate_lookup = crates
            .values()
            .sorted_by_key(|krate| krate.crate_id)
            .collect_vec()
            .into();
        Self {
            string_interner,
            allocator: arena,
            krate,
            crates,
            crate_lookup,
            module: Default::default(),
            nodes: Default::default(),
            items: Default::default(),
            scopes: Default::default(),
        }
    }

    fn resolve(mut self) -> Option<HirCrate<'hir>> {
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

    fn resolve_module(&mut self, module: &'hir ast::Module) {
        self.module = Some(module);
        module.items.iter().for_each(|item| self.visit_item(item));
    }

    fn visit_item(&mut self, item: &ast::Item) {
        let span = item.span;
        let id = item.id;

        let hir_node = match &item.kind {
            ast::ItemKind::Use(use_stmt) => None,
            ast::ItemKind::GlobalLet(let_stmt) => self.resolve_global_let_stmt(let_stmt, span, id),
            ast::ItemKind::Class(class_stmt) => self.resolve_class_stmt(class_stmt, span, id),
            ast::ItemKind::Enum(enum_stmt) => self.resolve_enum_stmt(enum_stmt, span, id),
            ast::ItemKind::Trait(trait_stmt) => self.resolve_trait_stmt(trait_stmt, span, id),
            ast::ItemKind::TraitImpl(trait_impl_stmt) => {
                self.resolve_trait_impl_stmt(trait_impl_stmt, span, id)
            }
            ast::ItemKind::Fn(fn_stmt) => self.resolve_fn_stmt(fn_stmt, span, id),
        };

        match hir_node {
            Some(node_id) => {
                self.items.push(node_id);
            }
            _ => {}
        }
    }

    fn duplicate_definition(&self, existing: LocalDefId, new: LocalDefId) {
        todo!();
    }

    fn resolve_global_let_stmt(
        &mut self,
        let_stmt: &ast::GlobalLetStmt,
        span: Span,
        id: LocalDefId,
    ) -> Option<LocalDefId> {
        // Lower expression
        let local_var = self.resolve_local_var(&let_stmt.local_var);
        let ty = self.resolve_ty(&let_stmt.ty)?;
        let expr = self.resolve_expr(&let_stmt.initializer)?;

        let global_let_stmt = self.allocator.alloc(Constant {
            local_var,
            ty,
            initializer,
        });
        let item = self.alloc(Item {
            kind: ItemKind::Constant(global_let_stmt),
            span,
            id,
        });

        // We don't need to insert constants into a local scope since it is already part of the module ns.
        self.insert_node(id, Node::Item(item));
        Some(id)
    }

    fn resolve_class_stmt(
        &mut self,
        class_stmt: &AstClassStmt,
        span: Span,
        id: LocalDefId,
    ) -> Option<LocalDefId> {
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

        let hir_class = self.alloc(ClassDef::new(
            class_stmt.name,
            class_stmt.class_type,
            generic_params,
            fields,
            fn_stmts,
        ));

        let item = Item {
            kind: ItemKind::Class(hir_class),
            span,
            id,
        };

        self.insert_node(id, Node::Item(hir_class));
        Some(id)
    }

    fn maybe_resolve_generics(
        &mut self,
        generics: &Option<ast::Generics>,
    ) -> Result<Option<Generics<'hir>>, ()> {
        match generics {
            Some(generics) => match self.resolve_generics(generics) {
                Some(generics) => Ok(Some(generics)),
                None => Err(()),
            },
            None => Ok(None),
        }
    }

    fn resolve_generics(&mut self, generics: &ast::Generics) -> Option<Generics<'hir>> {
        let mut hir_generics = Vec::with_capacity(generics.len());
        for generic in generics {
            hir_generics.push(self.resolve_ty(generic)?);
        }
        Some(self.alloc_slice(&*hir_generics))
    }

    fn resolve_params(&mut self, params: &ast::Params) -> Option<Params<'hir>> {
        let mut hir_params = Vec::<&'hir Param<'hir>>::with_capacity(params.len());
        for param in params {
            let local_var = self.resolve_local_param(&param.local_var);
            let ty = self.resolve_ty(&param.ty)?;
            let mutability = param.mutability;
            let span = param.span;
            let id = param.id;

            let hir_param = self.alloc(Param {
                local_var,
                ty,
                mutability,
                span,
                id,
            });
            hir_params.push(hir_param);

            self.insert_node(id, Node::Param(hir_param));
        }
        Some(self.alloc_slice(&*hir_params))
    }

    fn resolve_generic_params(
        &mut self,
        generic_params: &ast::GenericParams,
    ) -> Option<GenericParams<'hir>> {
        let mut generics = Vec::<&'hir GenericParam<'hir>>::with_capacity(generic_params.len());
        for param in generic_params {
            self.insert_generic_param(param.name.ident, param.id)?;

            let trait_bound = match &param.trait_bound {
                None => None,
                Some(ast::Ty {
                    kind: ast::TyKind::TraitBound { trait_bound },
                    span,
                    id,
                }) => Some(self.resolve_trait_bound(trait_bound, *span, *id)?),
                _ => unreachable!(),
            };
            let span = param.span;
            let id = param.id;

            let generic_param = self.alloc(GenericParam::new(param.name, trait_bound));
            let generic_ty = self.alloc(Ty {
                kind: TyKind::GenericParam(generic_param),
                span,
                id,
            });
            /*
                We always want to insert the node, even if it clashes with another generic param.
                Otherwise we can't look it up later when handling errors.
            */
            self.insert_node(param.id, Node::Ty(generic_ty));
            generics.push(generic_param);
        }
        Some(self.alloc_slice(&*generics))
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

    fn insert_var(&mut self, local_var: LocalVar) {
        // Need to handle global let case where there is no scope (maybe introduce a global scope?)
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert_var(local_var);
        }
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

    fn insert_param(&mut self, local_var: LocalVar) -> Option<()> {
        match self.scopes.last_mut().unwrap().insert_param(local_var) {
            None => Some(()),
            Some(existing) => {
                self.duplicate_definition(existing.id, local_var.id);
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

    fn find_var(&mut self, ident: InternedStr) -> Option<LocalVar> {
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

    fn resolve_fields(&mut self, ast_fields: &ast::Fields) -> Option<Fields<'hir>> {
        let mut fields = Vec::<&'hir Field<'hir>>::with_capacity(ast_fields.len());
        for field in ast_fields {
            let ast::Field {
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
            let field = self.allocator.alloc(Field {
                ident: *ident,
                ty: resolved_ty,
                span: *span,
                id: *id,
            });
            self.insert_node(*id, Node::Field(field));

            fields.push(field);
        }
        Some(self.allocator.alloc_slice_copy(&*fields))
    }

    fn resolve_enum_stmt(
        &mut self,
        enum_stmt: &ast::EnumStmt,
        span: Span,
        id: LocalDefId,
    ) -> Option<LocalDefId> {
        self.scopes.push(Scope::Enum {
            id: id.to_def_id(self.krate.crate_id),
            members: Default::default(),
            self_fns: Default::default(),
            generics: Default::default(),
        });

        let generic_params = self.resolve_generic_params(&enum_stmt.generic_params)?;
        let members = self.resolve_enum_members(&enum_stmt.members)?;
        let member_fns = self.resolve_self_fn_stmts(&enum_stmt.self_fns)?;

        self.scopes.pop();

        let hir_enum = self.allocator.alloc(EnumDef {
            name: enum_stmt.name,
            generic_params,
            members,
            member_fns,
        });

        let item = Item {
            kind: ItemKind::Enum(hir_enum),
            span,
            id,
        };

        self.insert_node(id, Node::Item(item));
        Some(id)
    }

    fn resolve_enum_members(
        &mut self,
        enum_stmt_members: &Vec<ast::EnumMember>,
    ) -> Option<MemberDefs<'hir>> {
        let mut enum_members = Vec::<&'hir MemberDef>::with_capacity(enum_stmt_members.len());
        for member in enum_stmt_members {
            self.insert_enum_member(member.name, member.id)?;

            self.scopes.push(Scope::EnumMember {
                fields: Default::default(),
                self_fns: Default::default(),
            });

            let fields = self.resolve_fields(&member.fields)?;
            let member_fns = self.resolve_self_fn_stmts(&member.self_fns)?;

            let member_def = self.alloc(MemberDef {
                name: member.name,
                fields,
                member_fns,
                span: member.span,
                id: member.id,
            });

            self.insert_node(member.id, Node::Member(member_def));

            enum_members.push(member_def);

            self.scopes.pop();
        }
        Some(self.alloc_slice(&*enum_members))
    }

    fn resolve_trait_stmt(
        &mut self,
        trait_stmt: &ast::TraitStmt,
        span: Span,
        id: LocalDefId,
    ) -> Option<LocalDefId> {
        self.scopes.push(Scope::Trait {
            id: id.to_def_id(self.krate.crate_id),
            self_fns: Default::default(),
            generics: Default::default(),
        });

        let generic_params = self.resolve_generic_params(&trait_stmt.generic_params)?;
        let member_fns = self.resolve_self_fn_stmts(&trait_stmt.self_fns)?;

        self.scopes.pop();

        let hir_trait = self.alloc(TraitDef {
            name: trait_stmt.name,
            generic_params,
            member_fns,
        });

        let item = Item {
            kind: ItemKind::Trait(hir_trait),
            span,
            id,
        };

        self.insert_node(id, Node::Item(item));

        Some(id)
    }

    fn resolve_trait_impl_stmt(
        &mut self,
        trait_stmt: &ast::TraitImplStmt,
        span: Span,
        id: LocalDefId,
    ) -> Option<LocalDefId> {
        let target_ty = self.resolve_qualified_ident(&trait_stmt.target_ty)?;
        let trait_to_impl = self.resolve_path_ty(&trait_stmt.trait_to_impl)?;

        self.scopes.push(Scope::TraitImpl {
            target_id: target_ty,
            self_fns: Default::default(),
        });

        let member_fns = self.resolve_self_fn_stmts(&trait_stmt.self_fns)?;
        let trait_impl_stmt = self.alloc(TraitImplDef {
            trait_to_impl,
            target_ty,
            member_fns,
        });
        let item = Item {
            kind: ItemKind::TraitImpl(trait_impl_stmt),
            span,
            id,
        };

        self.insert_node(id, Node::Item(item));
        Some(id)
    }

    fn resolve_fn_stmt(
        &mut self,
        fn_stmt: &ast::FnStmt,
        span: Span,
        id: LocalDefId,
    ) -> Option<LocalDefId> {
        self.scopes.push(Scope::Fn {
            params: Default::default(),
            generics: Default::default(),
            vars: Default::default(),
        });

        let sig = self.resolve_fn_sig(&fn_stmt.sig)?;
        let body = self.maybe_resolve_block(&fn_stmt.body).ok()?;

        self.scopes.pop();

        let fn_def = self.alloc(FnDef {
            sig,
            body,
            span,
            id,
        });
        self.insert_node(id, Node::Fn(fn_def));
        Some(id)
    }

    fn resolve_self_fn_stmt(&mut self, fn_stmt: &ast::FnSelfStmt) -> Option<&'hir FnDef<'hir>> {
        let ast::FnSelfStmt {
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

        let fn_def = self.alloc(FnDef {
            sig: resolved_sig,
            body: resolved_body,
            span: *span,
            id: *id,
        });
        self.insert_node(*id, Node::Fn(fn_def));
        Some(fn_def)
    }

    fn resolve_self_fn_stmts(&mut self, stmts: &Vec<ast::FnSelfStmt>) -> Option<FnStmts<'hir>> {
        for fn_stmt in stmts {
            self.insert_self_fn(fn_stmt.sig.name.ident, fn_stmt.id)?;
        }

        // This is safe because we have already checked to ensure that there are no name collisions.
        let mut fn_stmts = Vec::<&'hir FnDef<'hir>>::with_capacity(stmts.len());
        for fn_stmt in stmts {
            let fn_stmt = self.resolve_self_fn_stmt(fn_stmt)?;
            fn_stmts.push(fn_stmt);
        }
        Some(self.allocator.alloc_slice_copy(&*fn_stmts))
    }

    fn resolve_fn_sig(&mut self, fn_sig: &ast::FnSig) -> Option<FnSig<'hir>> {
        let generic_params = self.resolve_generic_params(&fn_sig.generic_params)?;
        let params = self.resolve_params(&fn_sig.params)?;

        let return_ty = self.maybe_resolve_ty(&fn_sig.return_type)?;

        Some(FnSig::new(fn_sig.name, generic_params, params, return_ty))
    }

    fn maybe_resolve_expr(
        &mut self,
        expr: &Option<Box<ast::Expr>>,
    ) -> Result<Option<&'hir Expr<'hir>>, ()> {
        match expr {
            None => Ok(None),
            Some(expr) => match self.resolve_expr(expr) {
                Some(expr) => Ok(Some(expr)),
                None => Err(()),
            },
        }
    }

    fn resolve_expr(&mut self, expr: &ast::Expr) -> Option<&'hir Expr<'hir>> {
        let span = expr.span;
        let id = expr.id;
        let resolved_expr = match &expr.kind {
            ast::ExprKind::Array(array_expr) => match array_expr {
                ast::ArrayExpr::SizedInitializer(initializer, size) => {
                    let initializer = self.resolve_expr(initializer)?;
                    let size = self.resolve_expr(size)?;

                    ExprKind::Array(ArrayExpr::Sized { initializer, size })
                }
                ast::ArrayExpr::Initializer(initializers) => {
                    let initializers = initializers
                        .iter()
                        .map(|expr| self.resolve_expr(expr))
                        .collect::<Option<Vec<_>>>()?;
                    let initializers = self.alloc_slice(&*initializers);

                    ExprKind::Array(ArrayExpr::Unsized { initializers })
                }
            },
            ast::ExprKind::Call(call) => {
                let args = self.resolve_args(&call.args)?;
                let target = self.resolve_expr(&call.target)?;

                ExprKind::Call(CallExpr::new(target, args))
            }
            ast::ExprKind::Infix(infix) => {
                let lhs = self.resolve_expr(&infix.lhs)?;
                let rhs = self.resolve_expr(&infix.rhs)?;

                ExprKind::Infix(InfixExpr::new(infix.operator, lhs, rhs))
            }
            ast::ExprKind::Unary(unary) => {
                let expr = self.resolve_expr(&unary.expr)?;

                ExprKind::Unary(UnaryExpr::new(unary.operator, expr))
            }
            ast::ExprKind::String(string) => ExprKind::String(*string),
            ast::ExprKind::Int(int) => ExprKind::Int(*int),
            ast::ExprKind::UInt(uint) => ExprKind::UInt(*uint),
            ast::ExprKind::Float(float) => ExprKind::Float(*float),
            ast::ExprKind::False => ExprKind::False,
            ast::ExprKind::True => ExprKind::True,
            ast::ExprKind::None => ExprKind::None,
            ast::ExprKind::Match(match_expr) => {
                let source = self.resolve_expr(&match_expr.source)?;
                let arms = self.resolve_match_arms(&match_expr.arms)?;
                ExprKind::Match(MatchExpr::new(source, arms))
            }
            ast::ExprKind::Closure(closure) => {
                let params = self.resolve_closure_params(&closure.params)?;
                let stmt = self.resolve_stmt(&closure.stmt)?;
                ExprKind::Closure(ClosureExpr::new(params, stmt))
            }
            ast::ExprKind::Assign(assign) => {
                let lhs = self.resolve_expr(&assign.lhs)?;
                let rhs = self.resolve_expr(&assign.rhs)?;
                ExprKind::Assign(AssignExpr::new(lhs, rhs))
            }
            ast::ExprKind::Field(field) => {
                let lhs = self.resolve_expr(&field.lhs)?;
                ExprKind::Field(FieldExpr::new(lhs, field.ident))
            }
            ast::ExprKind::Index(index) => {
                let expr = self.resolve_expr(&index.expr)?;
                let key = self.resolve_expr(&index.key)?;
                ExprKind::Index(IndexExpr::new(expr, key))
            }
            ast::ExprKind::Path(path) => {
                // These paths are richer and we have to preserve generic info for type inference
                let path = self.resolve_path(path)?;
                ExprKind::Path(path)
            }
            ast::ExprKind::Parentheses(parentheses) => {
                // Special logic for stripping parentheses (since they are just for pretty printing)
                let resolved_expr = self.resolve_expr(&parentheses.expr)?;
                return Some(resolved_expr.kind);
            }
            ast::ExprKind::Break => ExprKind::Break,
            ast::ExprKind::Continue => ExprKind::Continue,
        };
        let resolved_expr = self.alloc(resolved_expr);
        self.insert_node(HirNode::new(HirNodeKind::Expr(resolved_expr), span, id));
        Some(resolved_expr)
    }

    fn resolve_args(&mut self, args: &AstArgs) -> Option<Args<'hir>> {
        let mut hir_args = Vec::<&'hir Expr<'hir>>::with_capacity(args.len());
        for arg in args {
            hir_args.push(self.resolve_expr(arg)?);
        }
        Some(self.allocator.alloc_slice_copy(&*hir_args))
    }

    fn resolve_local_var(&mut self, local_var: &AstLocalVar) -> LocalVar {
        let hir_local_var = LocalVar::new(local_var.ident, local_var.id);

        self.insert_var(hir_local_var);

        self.insert_node(HirNode::new(
            HirNodeKind::LocalVar(hir_local_var),
            local_var.span,
            local_var.id,
        ));
        hir_local_var
    }

    fn resolve_local_param(&mut self, local_var: &AstLocalVar) -> LocalVar {
        let hir_local_var = LocalVar::new(local_var.ident, local_var.id);

        self.insert_param(hir_local_var);

        self.insert_node(HirNode::new(
            HirNodeKind::LocalVar(hir_local_var),
            local_var.span,
            local_var.id,
        ));
        hir_local_var
    }

    /// This method can resolve qualified idents as well.
    fn resolve_path(&mut self, path_expr: &AstPathExpr) -> Option<PathExpr<'hir>> {
        let mut segments = Vec::<&'hir Segment<'hir>>::with_capacity(path_expr.segments.len());
        match path_expr.ident_type {
            IdentType::Crate => {
                let res = self.alloc(Res::Crate(self.krate.crate_id));
                segments.push(self.alloc(Segment::new(res, None)));
                let mut path = VecDeque::from_iter(&path_expr.segments);
                while !path.is_empty() {
                    let prev_seg = segments.last().unwrap(); // Should be safe
                    let current = path.pop_front().unwrap(); // Should be safe
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
                        let segment = self.find_secondary_segment(&prev_seg.res, current)?;

                        segments.push(segment);
                    }

                    self.verify_last_segment(segments.last().unwrap())?;
                }
            }
        }
        Some(PathExpr::new(self.alloc_slice(&*segments)))
    }

    fn find_primary_segment(&mut self, segment: &AstSegment) -> Option<&'hir Segment<'hir>> {
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
            .map(|res| {
                let res = self.alloc(res);
                &*self.alloc(Segment::new(res, generics.clone()))
            })
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

    fn find_secondary_segment(
        &mut self,
        previous: &Res,
        segment: &AstSegment,
    ) -> Option<&'hir Segment<'hir>> {
        let module_ns = &self.module.unwrap().namespace;
        let ident = segment.ident.ident;
        // TODO: Finish generics error handling
        let generics = self.maybe_resolve_generics(&segment.generics).ok()?;

        let res = match previous {
            Res::Crate(krate_id) => {
                let krate = &self.crate_lookup[*krate_id];
                krate
                    .module_trie()
                    .get(&AstModulePath::from_iter([ident]))
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
                .iter()
                .find(|member_def| member_def.ident == ident)
                .cloned()
                .map(|member| Res::ValueDef(ValueDef::EnumMember(member)))
                .or_else(|| {
                    enum_def
                        .fns
                        .iter()
                        .find(|fn_def| fn_def.ident == ident)
                        .cloned()
                        .map(|fn_def| Res::Fn(MaybeFnDef::Some(fn_def)))
                })
                .or_else(|| {
                    // TODO: Emit compiler error
                    // FnNotFound(ident)
                    None
                })?,
            Res::ValueDef(ValueDef::Class(class_def)) => {
                class_def
                    .fns
                    .iter()
                    .find(|fn_def| fn_def.ident == ident)
                    .copied()
                    .map(|fn_def| Res::Fn(MaybeFnDef::Some(fn_def)))
                    .or_else(|| {
                        // TODO: Emit compiler error
                        // FnNotFound(ident)
                        None
                    })?
            }
            Res::ValueDef(ValueDef::EnumMember(member_def)) => {
                member_def
                    .fns
                    .iter()
                    .find(|fn_def| fn_def.ident == ident)
                    .copied()
                    .map(|fn_def| Res::Fn(MaybeFnDef::Some(fn_def)))
                    .or_else(|| {
                        // TODO: Emit compiler error
                        // FnNotFound(ident)
                        None
                    })?
            }
            Res::ValueDef(ValueDef::Trait(trait_def)) => trait_def
                .fns
                .iter()
                .find(|fn_def| fn_def.ident == ident)
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
        let res = self.alloc(res);
        let segment = self.alloc(Segment::new(res, generics));
        Some(segment)
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
    ) -> Option<&'hir DestructureExpr<'hir>> {
        let id = destructure_expr.id;
        let span = destructure_expr.span;
        let expr = match &destructure_expr.kind {
            AstDestructureExprKind::Pattern(pattern) => {
                let pattern = self.resolve_destructure_pattern(pattern)?;
                DestructureExpr::Pattern(pattern)
            }
            AstDestructureExprKind::Identifier(local_var) => {
                let hir_local_var = self.resolve_local_var(local_var);
                DestructureExpr::Identifier(hir_local_var)
            }
            DestructureExprKind::True => DestructureExpr::True,
            DestructureExprKind::False => DestructureExpr::False,
            DestructureExprKind::Float(float) => DestructureExpr::Float(*float),
            DestructureExprKind::Int(int) => DestructureExpr::Int(*int),
            DestructureExprKind::UInt(uint) => DestructureExpr::UInt(*uint),
            DestructureExprKind::String(string) => DestructureExpr::String(*string),
            DestructureExprKind::None => DestructureExpr::None,
        };
        let destructure_expr = self.alloc(expr);
        self.insert_node(HirNode::new(
            HirNodeKind::DestructureExpr(destructure_expr),
            span,
            id,
        ));
        Some(destructure_expr)
    }

    fn maybe_resolve_ty(&mut self, ty: &Option<AstTy>) -> Option<Option<&'hir Ty<'hir>>> {
        if let Some(ty) = ty {
            self.resolve_ty(ty).map(Some)
        } else {
            Some(None)
        }
    }

    fn resolve_ty(&mut self, ty: &AstTy) -> Option<&'hir Ty<'hir>> {
        let span = ty.span;
        let id = ty.id;
        let hir_ty = match &ty.kind {
            TyKind::Array { ty } => Ty::Array(self.resolve_ty(ty)?),
            TyKind::Path { path } => Ty::Path(self.resolve_path_ty(path)?),
            TyKind::TraitBound { trait_bound } => {
                let mut paths = Vec::<&'hir PathTy<'hir>>::with_capacity(trait_bound.len());
                for path in trait_bound {
                    let def = self.resolve_qualified_ident(&path.ident)?;
                    let generics = self.resolve_generics(&path.generics)?;
                    let path_ty = self.alloc(PathTy::new(def, generics));
                    paths.push(path_ty);
                }
                let paths = self.alloc_slice(&*paths);
                Ty::TraitBound(paths)
            }
            TyKind::Closure { params, ret_ty } => {
                let params = params
                    .iter()
                    .map(|param| self.resolve_ty(param))
                    .collect::<Option<Vec<_>>>()?;
                let ret_ty = self.resolve_ty(ret_ty)?;
                let params = self.allocator.alloc_slice_copy(&*params);
                Ty::Closure(Closure::new(params, ret_ty))
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

                let path_ty = PathTy::new(item_def, self.alloc_slice(&[]));
                Ty::Path(self.alloc(path_ty))
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

        let hir_ty = self.allocator.alloc(hir_ty);
        self.insert_node(HirNode::new(HirNodeKind::Ty(hir_ty), span, id));
        Some(hir_ty)
    }

    fn resolve_path_ty(&mut self, path_ty: &AstPathTy) -> Option<&'hir PathTy<'hir>> {
        let def = self.resolve_qualified_ident(&path_ty.ident)?;
        let generics = self.resolve_generics(&path_ty.generics)?;
        Some(self.alloc(PathTy::new(def, generics)))
    }

    fn resolve_trait_bound(
        &mut self,
        trait_bound: &AstTraitBound,
        span: Span,
        id: LocalDefId,
    ) -> Option<TraitBound<'hir>> {
        let mut hir_bound = Vec::<&'hir PathTy<'hir>>::with_capacity(trait_bound.len());
        for ty in trait_bound {
            let resolved_ty = self.resolve_path_ty(ty)?;
            hir_bound.push(resolved_ty);
        }
        let hir_bound: TraitBound<'hir> = self.alloc_slice(&*hir_bound);
        let hir_ty = self.alloc(Ty::TraitBound(hir_bound));
        self.insert_node(HirNode::new(HirNodeKind::Ty(hir_ty), span, id));
        Some(hir_bound)
    }

    fn resolve_stmt(&mut self, stmt: &AstStmt) -> Option<&'hir Stmt<'hir>> {
        let span = stmt.span;
        let id = stmt.id;
        let hir_stmt = match &stmt.kind {
            AstStmtKind::Let(let_stmt) => {
                let resolved_var = self.resolve_local_var(&let_stmt.local_var);
                let resolved_ty = self.maybe_resolve_ty(&let_stmt.ty)?;
                let resolved_initializer = self.maybe_resolve_expr(&let_stmt.initializer).ok()?;

                let let_stmt = LetStmt::new(
                    resolved_var,
                    let_stmt.mutability,
                    resolved_ty,
                    resolved_initializer,
                );

                Stmt::Let(self.alloc(let_stmt))
            }
            AstStmtKind::For(for_stmt) => {
                self.scopes.push(Scope::Block {
                    vars: StrMap::default(),
                });
                let hir_local_var = self.resolve_local_var(&for_stmt.local_var);

                let range = self.resolve_expr(&for_stmt.range)?;
                let body = self.resolve_block(&for_stmt.body)?;

                let for_stmt = ForStmt::new(hir_local_var, range, body);
                let stmt = Stmt::For(self.alloc(for_stmt));

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

                let if_stmt = IfStmt::new(condition, if_true, if_false);

                Stmt::If(self.alloc(if_stmt))
            }
            AstStmtKind::Return(return_stmt) => {
                let maybe_expr = self.maybe_resolve_expr(&return_stmt.value).ok()?;
                let return_stmt = ReturnStmt::new(maybe_expr);

                Stmt::Return(self.alloc(return_stmt))
            }
            AstStmtKind::While(while_stmt) => {
                let condition = self.resolve_expr(&while_stmt.condition)?;

                self.scopes.push(Scope::Block {
                    vars: Default::default(),
                });
                let block = self.resolve_block(&while_stmt.block_stmt)?;
                self.scopes.pop();

                let while_stmt = WhileStmt::new(condition, block);

                Stmt::While(self.alloc(while_stmt))
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
                let expression = Expression::new(expr, expression.implicit_return);

                Stmt::Expression(self.alloc(expression))
            }
        };

        let hir_stmt = self.alloc(hir_stmt);
        self.insert_node(HirNode::new(
            HirNodeKind::Stmt(hir_stmt),
            stmt.span,
            stmt.id,
        ));

        Some(hir_stmt)
    }

    fn maybe_resolve_block(
        &mut self,
        block: &Option<AstBlock>,
    ) -> Result<Option<&'hir Block<'hir>>, ()> {
        match block {
            None => Ok(None),
            Some(block) => match self.resolve_block(block) {
                Some(block) => Ok(Some(block)),
                None => Err(()),
            },
        }
    }

    fn resolve_block(&mut self, block: &AstBlock) -> Option<&'hir Block<'hir>> {
        let mut stmts = Vec::with_capacity(block.stmts.len());
        for stmt in &block.stmts {
            stmts.push(self.resolve_stmt(stmt)?);
        }
        let stmts = self.alloc_slice(&*stmts);
        let hir_block = self.alloc(Block::new(stmts));

        self.insert_node(HirNode::new(
            HirNodeKind::Block(hir_block),
            block.span,
            block.id,
        ));

        Some(hir_block)
    }

    fn resolve_match_arms(&mut self, arms: &[AstMatchArm]) -> Option<MatchArms<'hir>> {
        let mut hir_arms = Vec::<&'hir MatchArm<'hir>>::with_capacity(arms.len());
        for arm in arms {
            hir_arms.push(self.resolve_match_arm(arm)?);
        }
        Some(self.alloc_slice(&*hir_arms))
    }

    fn resolve_match_arm(&mut self, arm: &AstMatchArm) -> Option<&'hir MatchArm<'hir>> {
        self.scopes.push(Scope::MatchArm {
            vars: Default::default(),
        });

        let pattern = self.resolve_pattern(&arm.pattern)?;
        let body = self.resolve_stmt(&arm.body)?;

        self.scopes.pop();

        Some(self.alloc(MatchArm::new(pattern, body)))
    }

    fn resolve_pattern(&mut self, pattern: &AstPattern) -> Option<&'hir Pattern<'hir>> {
        let hir_pattern = match pattern {
            AstPattern::Wildcard => Pattern::Wildcard,
            AstPattern::Or(or_pattern) => {
                let patterns = or_pattern
                    .patterns
                    .iter()
                    .map(|patt| self.resolve_pattern(patt))
                    .collect::<Option<Vec<_>>>()?;
                let patterns = self.alloc_slice(&*patterns);
                Pattern::Or(OrPattern::new(patterns))
            }
            AstPattern::Ty(ty_patt) => {
                let resolved_ty = self.resolve_path_ty(&ty_patt.ty)?;
                let resolved_ident = ty_patt
                    .ident
                    .map(|pattern_local| self.resolve_local_var(&pattern_local));
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

        Some(self.alloc(hir_pattern))
    }

    fn resolve_destructure_pattern(
        &mut self,
        de_patt: &AstDestructurePattern,
    ) -> Option<DestructurePattern<'hir>> {
        let resolved_ty = self.resolve_path_ty(&de_patt.ty)?;
        let exprs = de_patt
            .exprs
            .iter()
            .map(|expr| self.resolve_destructure_expr(expr))
            .collect::<Option<Vec<_>>>()?;
        let exprs = self.alloc_slice(&*exprs);
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

    fn resolve_closure_params(
        &mut self,
        params: &[AstClosureParam],
    ) -> Option<ClosureParams<'hir>> {
        let closure_params: Vec<ClosureParam> = params
            .iter()
            .map(|param| ClosureParam::new(param.ident))
            .collect();
        Some(self.alloc_slice(&*closure_params))
    }

    fn insert_node(&mut self, id: LocalDefId, node: Node<'hir>) {
        assert!(self.nodes.insert(id, node).is_none());
    }

    fn alloc<T>(&self, val: T) -> &'hir T {
        self.allocator.alloc(val)
    }

    fn alloc_slice<T>(&self, slice: &[T]) -> &'hir mut [T]
    where
        T: Copy,
    {
        self.allocator.alloc_slice_copy(slice)
    }
}