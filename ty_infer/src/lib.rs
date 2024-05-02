#![allow(unused)]

use bumpalo::Bump;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::iter::zip;

use itertools::Itertools;
use serde::{Deserialize, Serialize};

use arena::Arena;
use ast::{CallExpr, EnumMemberDef, GlobalVarDef, InfixOp, TraitStmt, UnaryOp, ValueDef};
use diagnostics::{Diagnostic, Diagnostics};
use hir::{Closure, HirCrate, HirMap, Item, ItemKind, Node, Primitive};
use id::{DefId, LocalDefId};
use interner::{InternedStr, Interner, StringInterner};
use krate::Crate;
use macros::named_slice;
use typed_hir::{ArrayExpr, Block, ClassDef, EnumDef, Expr, ExprId, ExprKind, FloatTy, FnDef, GenericParam, GenericParams, Generics, IntTy, LocalVar, MatchArm, MemberDef, Stmt, Thir, ThirCrate, ThirMap, Trait, TraitBound, TraitDef, Ty, TyKind, UintTy};
use types::{DefMap, LDefMap, StrMap};

use crate::unification::UnificationTable;

mod trait_solver;
mod unification;

pub fn infer_types<'hir>(
    diagnostics: &'hir mut Diagnostics,
    hir_allocator: &'hir mut Bump,
    thir_allocator: &'hir mut Bump,
    hir_map: &'hir HirMap<'hir>,
) -> Option<ThirMap<'hir>> {
    let mut tydef_cache = TyDefCache::default();
    let mut crates = Vec::default();
    for krate in hir_map.krates() {
        let crate_inference = CrateInference::new(
            diagnostics,
            &mut tydef_cache,
            hir_allocator,
            thir_allocator,
            krate,
            hir_map,
        );
        let bodies = crate_inference.infer_bodies()?;
        let tkrate = ThirCrate {
            name: krate.name,
            id: krate.id,
            bodies,
        };
        crates.push(tkrate);
    }

    Some(ThirMap { crates })
}

/// Contains the data needed to infer types for a given HIR crate.
/// The HIR nodes are transformed into their typed HIR representation
/// which ensures that all nodes have correct type information.
///
/// TODO: All types SHOULD also be interned across crates using a ty interner.
#[derive(Debug)]
pub struct CrateInference<'hir, 'thir> {
    diagnostics: &'hir mut Diagnostics,
    ty_cache: &'hir TyDefCache<'hir>,
    hir_map: &'hir HirMap<'hir>,
    krate: &'hir HirCrate<'hir>,

    hir_allocator: &'hir mut Bump,
    thir_allocator: &'thir mut Bump,

    bodies: LDefMap<Thir<'hir>>,
}

#[derive(Default)]
pub struct TyDefCache<'a> {
    defs: HashMap<DefId, DefType<'a>>,
    interner: Interner<TyKind<'a>>,
}

pub enum DefType<'a> {
    Class(&'a ClassDef<'a>),
    Enum(&'a EnumDef<'a>),
    Member(&'a MemberDef<'a>),
    Trait(&'a TraitDef<'a>),
    Fn(&'a FnDef<'a>),
}

impl<'a> TyDefCache<'a> {
    pub fn resolve_def<F: FnOnce() -> DefType<'a>>(
        &mut self,
        def_id: DefId,
        resolver: F,
    ) -> &'a DefType<'a> {
        match self.defs.entry(def_id) {
            Entry::Occupied(occupied) => occupied.get(),
            Entry::Vacant(vacant) => vacant.insert(resolver()),
        }
    }

    pub fn intern(&mut self, kind: TyKind<'a>) -> Ty<'a> {
        let kind = self.interner.intern(kind);
        Ty { kind }
    }
}

fn resolve_def<'a, T, F: FnOnce() -> &'a T>(
    map: &'a mut HashMap<DefId, &'a T>,
    def_id: DefId,
    resolver: F,
) -> &'a T {
    match map.entry(def_id) {
        Entry::Occupied(occupied) => occupied.get(),
        Entry::Vacant(vacant) => vacant.insert(resolver()),
    }
}

#[derive(Debug)]
pub struct InferCtxt<'hir> {
    // Contains the current THIR body and information used to unify all the types.
    thir: Thir<'hir>,
    unify_table: UnificationTable<'hir>,

    hir_allocator: &'hir mut Bump,
    ty_cache: &'hir mut TyDefCache<'hir>,
}

impl<'hir> InferCtxt<'hir> {
    /// The main function that performs type inference. Every function body or
    /// expression should have a return type defined at compile time, and we can
    /// use this to infer and validate an entire expression/blocks' types.
    fn check_expr(mut self, expr: &hir::Expr) -> Thir<'hir> {
        let (_, ty) = self.check(expr, self.thir.ret_ty);
        self.thir
    }

    /// The main function that performs type inference. Every function body or
    /// expression should have a return type defined at compile time, and we can
    /// use this to infer and validate an entire expression/blocks' types.
    fn check_block(mut self, block: &hir::Block) -> Thir<'hir> {
        todo!()
    }

    fn check(&mut self, expr: &hir::Expr, ty: Ty) -> (ExprId, Ty) {
        let (ty, expr) = match (expr.kind, ty) {
            (hir::ExprKind::None, Ty { kind: TyKind::None }) => (
                ty,
                Expr {
                    kind: ExprKind::None,
                    ty,
                },
            ),
            (
                hir::ExprKind::True,
                Ty {
                    kind: TyKind::Boolean,
                },
            ) => (
                ty,
                Expr {
                    kind: ExprKind::True,
                    ty,
                },
            ),
            (
                hir::ExprKind::False,
                Ty {
                    kind: TyKind::Boolean,
                },
            ) => (
                ty,
                Expr {
                    kind: ExprKind::False,
                    ty,
                },
            ),
            (
                hir::ExprKind::Int(int),
                Ty {
                    kind: TyKind::Int(IntTy::I64),
                },
            ) => (
                ty,
                Expr {
                    kind: ExprKind::Int(int),
                    ty,
                },
            ),
            (
                hir::ExprKind::UInt(uint),
                Ty {
                    kind: TyKind::Uint(UintTy::U64),
                },
            ) => (
                ty,
                Expr {
                    kind: ExprKind::UInt(uint),
                    ty,
                },
            ),
            (
                hir::ExprKind::Float(float),
                Ty {
                    kind: TyKind::Float(FloatTy::F64),
                },
            ) => (
                ty,
                Expr {
                    kind: ExprKind::Float(float),
                    ty,
                },
            ),
            (hir::ExprKind::String(string), Ty { kind: TyKind::Str }) => (
                ty,
                Expr {
                    kind: ExprKind::String(string),
                    ty,
                },
            ),
            () => (
                ty,
                Expr {
                    kind: ExprKind::None,
                    ty,
                },
            ),
            _ => {
                let (mut constraints, expr) = self.infer_expr(expr);

                if self.unify_constraints(constraints) {
                    self.substitute_expr(expr)
                } else {
                    todo!()
                }
            }
        };
        let expr_id = ExprId(self.thir.exprs.len() as u32);
        self.thir.exprs.push(expr);
        (expr_id, ty)
    }

    fn infer_expr(&mut self, expr: &hir::Expr) -> (Constraints, ExprId) {
        let (constraints, expr) = match expr.kind {
            hir::ExprKind::Array(array) => self.infer_array(array),
            hir::ExprKind::Call(call) => self.infer_call(call),
            hir::ExprKind::Infix(infix) => self.infer_infix(infix),
            hir::ExprKind::Unary(unary) => self.infer_unary(unary),
            hir::ExprKind::None => self.infer_static(ExprKind::None, TyKind::None),
            hir::ExprKind::True => self.infer_static(ExprKind::True, TyKind::Boolean),
            hir::ExprKind::False => self.infer_static(ExprKind::False, TyKind::Boolean),
            hir::ExprKind::Int(int) => {
                self.infer_static(ExprKind::Int(int), TyKind::Int(IntTy::I64))
            }
            hir::ExprKind::UInt(uint) => {
                self.infer_static(ExprKind::UInt(uint), TyKind::Uint(UintTy::U64))
            }
            hir::ExprKind::Float(float) => {
                self.infer_static(ExprKind::Float(float), TyKind::Float(FloatTy::F64))
            }
            hir::ExprKind::String(string) => self.infer_string(string),
            hir::ExprKind::Match(match_expr) => self.infer_match(match_expr),
            hir::ExprKind::Closure(closure) => self.infer_closure(closure),
            hir::ExprKind::Assign(assign) => self.infer_assign(assign),
            hir::ExprKind::Field(field) => self.infer_field(field),
            hir::ExprKind::Index(index) => self.infer_index(index),
            hir::ExprKind::Path(path) => self.infer_path(path),
            hir::ExprKind::Block(block) => self.infer_block(block),
            hir::ExprKind::Break => self.infer_static(ExprKind::None, TyKind::None),
            hir::ExprKind::Continue => self.infer_static(ExprKind::None, TyKind::None),
        };
        let expr_id = ExprId(self.thir.exprs.len() as u32);
        self.thir.exprs.push(expr);
        (constraints, expr_id)
    }

    fn infer_array(&mut self, array_expr: hir::ArrayExpr) -> (Constraints, Expr) {
        todo!()
    }

    fn infer_call(&mut self, call: hir::CallExpr) -> (Constraints, Expr) {
        todo!()
    }

    fn infer_infix(&mut self, infix: hir::InfixExpr) -> (Constraints, Expr) {
        todo!()
    }

    fn infer_unary(&mut self, unary: hir::UnaryExpr) -> (Constraints, Expr) {
        todo!()
    }

    fn infer_static(&mut self, kind: ExprKind, ty_kind: TyKind) -> (Constraints, Expr) {
        let expr = Expr {
            kind,
            ty: self.ty_cache.intern(ty_kind),
        };
        (vec![], expr)
    }

    fn infer_int(&mut self, int: i64) -> (Constraints, Expr) {
        todo!()
    }

    fn infer_uint(&mut self, int: u64) -> (Constraints, Expr) {
        todo!()
    }

    fn infer_float(&mut self, float: f64) -> (Constraints, Expr) {
        todo!()
    }

    fn infer_string(&mut self, string: InternedStr) -> (Constraints, Expr) {
        todo!()
    }

    fn infer_match(&mut self, match_expr: hir::MatchExpr) -> (Constraints, Expr) {
        todo!()
    }

    fn infer_closure(&mut self, closure: hir::ClosureExpr) -> (Constraints, Expr) {
        todo!()
    }

    fn infer_assign(&mut self, assign: hir::AssignExpr) -> (Constraints, Expr) {
        todo!()
    }

    fn infer_field(&mut self, field: hir::FieldExpr) -> (Constraints, Expr) {
        todo!()
    }

    fn infer_index(&mut self, index_expr: hir::IndexExpr) -> (Constraints, Expr) {
        todo!()
    }

    fn infer_path(&mut self, path: hir::PathExpr) -> (Constraints, Expr) {
        todo!()
    }

    fn infer_block(&mut self, block: hir::Block) -> (Constraints, Expr) {
        todo!()
    }

    fn substitute_expr(&mut self, expr_id: ExprId) -> (Ty, Expr) {
        todo!()
    }

    fn unify_constraints(&mut self, constraints: Constraints) -> bool {
        constraints
            .into_iter()
            .all(|constraint| self.unify(constraint));
        todo!()
    }

    fn unify(&mut self, constraint: Constraint) -> bool {
        match constraint {
            Constraint::Equal(ty, ty) => self.unify_ty_ty(ty, ty),
            Constraint::Assignable(_, _) => {}
            Constraint::Array(_) => {}
        }
    }

    fn unify_ty_ty(&mut self, lhs: &TyKind, rhs: &TyKind) -> bool {
        let lhs = self.normalize_ty(lhs);
        let rhs = self.normalize_ty(rhs);
        
        match (lhs, rhs) {
            (TyKind::Infer(_) | TyKind::GenericParam(_), rhs)
            => {

            }
        }
        todo!()
    }

    fn normalize_ty(&mut self, ty: &TyKind) -> &TyKind {
        match ty {
            TyKind::Array(array) => {
                self.alloc(TyKind::Array(Ty { kind: self.normalize_ty(array.kind) }))
            }
            TyKind::Class(class_def, generics) => {
                let generics = self.normalize_generics(generics);
                self.alloc(TyKind::Class(class_def, generics))
            }
            TyKind::Enum(enum_def, generics) => {
                let generics = self.normalize_generics(generics);
                self.alloc(TyKind::Enum(enum_def, generics))
            }
            TyKind::Member(member_def, generics) => {
                let generics = self.normalize_generics(generics);
                self.alloc(TyKind::Member(member_def, generics))
            }
            TyKind::TraitBound(trait_bound) => {
                self.alloc(TyKind::TraitBound(self.normalize_trait_bound(trait_bound)))
            }
            TyKind::GenericParam(GenericParam { ident, trait_bound }) => {
                let trait_bound = match trait_bound {
                    Some(trait_bound) => Some(self.normalize_trait_bound(trait_bound)),
                    None => None
                };
                self.alloc(TyKind::GenericParam(GenericParam { ident: *ident, trait_bound }))
            }
            TyKind::Fn(fn_def, generics) => {
                let generics = self.normalize_generics(generics);
                self.alloc(TyKind::Fn(fn_def, generics))
            }
            TyKind::Infer(ty_var) => {
                match self.unify_table.probe(*ty_var) {
                    None => ty,
                    Some(ty_kind) => ty_kind,
                }
            }
            TyKind::Float(_) | TyKind::Int(_) | TyKind::Uint(_) | TyKind::Str | TyKind::Boolean | TyKind::None => ty,
        }
    }

    fn normalize_generics(&mut self, generics: Generics) -> Generics {
        self.alloc_slice(generics.iter()
            .map(|generic| Ty { kind: self.normalize_ty(generic.kind) }))
    }
    
    fn normalize_trait_bound(&mut self, trait_bound: TraitBound) -> TraitBound {
        self.alloc_slice(trait_bound.iter()
            .map(|trait_def| {
                self.alloc(Trait {
                    trait_def: trait_def.trait_def,
                    generics: self.normalize_generics(trait_def.generics),
                })
            }))
    }

    fn alloc<T>(&self, val: T) -> &'hir T {
        self.hir_allocator.alloc(val)
    }

    fn alloc_slice<I, T>(&self, slice: I) -> &'hir mut [T]
        where
            T: Copy,
            I: IntoIterator<Item = T>,
            I::IntoIter: ExactSizeIterator,
    {
        self.hir_allocator.alloc_slice_fill_iter(slice)
    }

    fn new(
        generic_params: GenericParams,
        ty_interner: &'hir mut TyDefCache<'hir>,
        hir_allocator: &'hir mut Bump,
        ret_ty: Ty<'hir>,
    ) -> Self {
        Self {
            thir: Thir::new(generic_params, ret_ty),
            unify_table: Default::default(),
            hir_allocator,
            ty_cache: ty_interner,
        }
    }
}

impl<'hir, 'thir> CrateInference<'hir, 'thir> {
    pub fn new(
        diagnostics: &'hir mut Diagnostics,
        ty_cache: &'hir mut TyDefCache<'hir>,
        hir_allocator: &'hir mut Bump,
        thir_allocator: &'thir mut Bump,
        krate: &'hir HirCrate,
        hir_map: &'hir HirMap,
    ) -> Self {
        Self {
            diagnostics,
            ty_cache,
            hir_allocator,
            thir_allocator,
            hir_map,
            krate,
            bodies: Default::default(),
        }
    }

    // TODO: Returned interned ty representation to reduce memory overhead.
    // TODO: Record existing types for class fields and other nodes whose types are known statically.
    pub fn infer_bodies(mut self) -> Option<LDefMap<Thir<'hir>>> {
        for item in self.krate.items.iter() {
            match item {
                ItemKind::Constant(constant) => self.infer_constant(constant),
                ItemKind::Class(class_def) => self.infer_class(class_def),
                ItemKind::Enum(enum_def) => self.infer_enum(enum_def),
                ItemKind::Fn(fn_def) => self.check_fn_def(fn_def),
                ItemKind::Trait(trait_def) => self.infer_trait_def(trait_def),
                ItemKind::TraitImpl(trait_impl_def) => self.check_trait_impl_def(trait_impl_def),
            }
        }
        Some(self.bodies)
    }

    fn infer_constant(&mut self, constant: &hir::Constant) {
        let ret_ty = self.resolve_ty(constant.ty);

        let mut infer_ctxt = InferCtxt::new(
            GenericParams::default(),
            &mut self.ty_cache,
            &mut self.hir_allocator,
            ret_ty,
        );
        let thir = infer_ctxt.check_expr(constant.initializer);
        self.bodies.insert(constant.initializer.id, thir);
    }

    fn infer_class(&mut self, class_def: &hir::ClassDef) {
        for function in class_def.fn_defs {
            self.check_fn_def(function);
        }
    }

    fn infer_enum(&mut self, node: &hir::EnumDef) {
        for member in node.members {
            for function in member.fn_defs {
                self.check_fn_def(function);
            }
        }
        for function in node.fn_defs {
            self.check_fn_def(function);
        }
    }

    fn infer_trait_def(&mut self, trait_def: &hir::TraitDef) {
        for function in trait_def.fn_defs {
            self.check_fn_def(function);
        }
    }

    fn check_trait_impl_def(&mut self, trait_impl_def: &hir::TraitImplDef) {
        for function in trait_impl_def.fn_defs {
            self.check_fn_def(function);
        }
    }

    fn check_fn_def(&mut self, fn_def: &hir::FnDef) {
        if let Some(block) = fn_def.body {
            let generic_params = self.alloc_slice(
                fn_def
                    .sig
                    .generic_params
                    .iter()
                    .map(|param| self.resolve_generic_param(param)),
            );
            let ret_ty = self
                .maybe_resolve_ty(fn_def.sig.ret_ty)
                .unwrap_or_else(|| self.intern_ty(TyKind::None));

            let infer_ctxt = InferCtxt::new(
                generic_params,
                &mut self.ty_cache,
                &mut self.hir_allocator,
                ret_ty,
            );
            let thir = infer_ctxt.check_block(block);
        }
    }

    fn check_block(&mut self, block: &hir::Block) -> Thir<'hir> {
        todo!()
    }

    /// This method is not fallible since all constraints should already have been satisfied.
    /// If this method panics, it is due to improper constraint generation.
    fn substitute_expr(&mut self, expr: ExprId) -> Ty<'hir> {
        todo!()
    }

    fn unify_constraints(&mut self, constraints: Constraints) -> bool {
        constraints
            .into_iter()
            .all(|constraint| self.unify(constraint))
    }

    fn unify(&mut self, constraint: Constraint) -> bool {
        match constraint {
            Constraint::Equal(lhs, rhs) => self.unify_ty_ty(lhs, rhs, Type::eq),

            Constraint::Array(ty) => self.unify_ty_array(ty),
        }
    }

    fn unify_fn_sig(&mut self, fn_sig: &FnSig, args: &[Type], ret_ty: Type) -> bool {
        if args.len() != fn_sig.params.len() {
            self.diagnostics.push(Diagnostic::BlankError);
            return false;
        }

        for x in 0..args.len() {
            let arg = args[x].clone();
            let param = fn_sig.params[x].clone();

            if !self.unify_ty_ty(param, arg, Type::assignable) {
                return false;
            }
        }

        self.unify_ty_ty(ret_ty, *fn_sig.ret_ty.clone(), Type::assignable)
    }

    fn unify_unary_op(&mut self, ty: Type, unary_op: UnaryOp) -> bool {
        let ty = self.normalize_ty(ty);

        // TODO: Add unary op checking
        true
    }

    fn unify_infix_op(&mut self, lhs: Type, rhs: Type, op: InfixOp) -> bool {
        // TODO: Add infix op checking
        self.unify_ty_ty(lhs, rhs, Type::assignable)
    }

    fn unify_ty_array(&mut self, ty: Type) -> bool {
        let ty = self.normalize_ty(ty);

        match ty {
            Type::Array(_) => true,
            _ => {
                self.diagnostics.push(Diagnostic::BlankError);
                return false;
            }
        }
    }

    fn unify_ty_ty<F: Fn(&Type, &Type) -> bool>(
        &mut self,
        lhs: Type,
        rhs: Type,
        assignable_check: F,
    ) -> bool {
        let lhs = self.normalize_ty(lhs);
        let rhs = self.normalize_ty(rhs);

        // Check for type equality
        match (lhs, rhs) {
            // If any type is unknown, we have to unify it against the other types.
            (
                Type::Infer(lhs) | Type::GenericParam(GenericParam { ty_var: lhs, .. }),
                Type::Infer(rhs) | Type::GenericParam(GenericParam { ty_var: rhs, .. }),
            ) => self.unify_var_var(lhs, rhs),
            (
                Type::Infer(unknown)
                | Type::GenericParam(GenericParam {
                    ty_var: unknown, ..
                }),
                ty,
            )
            | (
                ty,
                Type::Infer(unknown)
                | Type::GenericParam(GenericParam {
                    ty_var: unknown, ..
                }),
            ) => self.unify_var_ty(unknown, ty, assignable_check),
            // If both types are known, then we need to check for type compatibility.
            (lhs, rhs) => {
                if !assignable_check(&lhs, &rhs) {
                    self.diagnostics.push(Diagnostic::BlankError);
                    return false;
                }
                true
            }
        }
    }

    fn unify_var_var(&mut self, lhs: TyVar, rhs: TyVar) -> bool {
        if !self.unify_table.unify_var_var(lhs, rhs) {
            self.diagnostics.push(Diagnostic::BlankError);
            return false;
        }
        true
    }

    fn unify_var_ty<F: Fn(&Type, &Type) -> bool>(
        &mut self,
        var: TyVar,
        ty: Type,
        assignable_check: F,
    ) -> bool {
        if !self.unify_table.unify_var_ty(var, ty, assignable_check) {
            self.diagnostics.push(Diagnostic::BlankError);
            return false;
        }
        true
    }

    /// This function ensures that this type has been substituted with the most up to date version of itself.
    fn normalize_ty(&mut self, ty: Type) -> Type {
        match ty {
            Type::Array(array) => Type::Array(Array::new(self.normalize_ty(*array.ty))),
            Type::Fn(closure) => {
                let normalized_tys = closure
                    .params
                    .into_iter()
                    .map(|ty| self.normalize_ty(ty.clone()))
                    .collect_vec()
                    .into();
                let ret_ty = self.normalize_ty(*closure.ret_ty);
                Type::Fn(FnSig::new(normalized_tys, ret_ty))
            }
            Type::TraitBound(trait_bound) => {
                let normalized_tys = trait_bound
                    .bounds
                    .into_iter()
                    .map(|ty| self.normalize_ty(ty))
                    .collect();
                Type::TraitBound(TraitBound::new(normalized_tys))
            }
            Type::GenericParam(generic_param) => {
                match self.unify_table.probe(generic_param.ty_var) {
                    Some(ty) => self.normalize_ty(ty),
                    None => Type::GenericParam(generic_param),
                }
            }
            Type::Infer(ty_var) => {
                // Probe for the most recent parent of this value and update the path if it is no
                // no longer correct.
                match self.unify_table.probe(ty_var) {
                    Some(ty) => self.normalize_ty(ty),
                    None => Type::Infer(ty_var),
                }
            }
            _ => ty,
        }
    }

    fn substitute(&mut self, node_id: &LocalDefId) {
        let node = self.krate.node(node_id);
        match node {
            NodeKind::GlobalLet(let_stmt) => {
                // Don't need to check the associated ty
                self.substitute(&let_stmt.initializer);
            }
            NodeKind::Fn(fn_stmt) => {
                if let Some(body) = &fn_stmt.body {
                    self.substitute(body);
                }
            }
            NodeKind::EnumMember(enum_member) => {
                for member_fn in enum_member.member_fns.values() {
                    self.substitute(member_fn);
                }
            }
            NodeKind::Expr(expr) => {
                // TODO: Finish implementing this tree walk.
                match expr {
                    Expr::Array(ArrayExpr::Sized { initializer, size }) => {
                        self.substitute(initializer);
                        self.substitute(size);
                    }
                    Expr::Array(ArrayExpr::Unsized { initializers }) => {
                        initializers
                            .into_iter()
                            .for_each(|initializer| self.substitute(initializer));
                    }
                    Expr::Call(call) => {
                        self.substitute(&call.target);
                        call.args.into_iter().for_each(|arg| self.substitute(arg));
                    }
                    Expr::Infix(infix) => {
                        self.substitute(&infix.lhs);
                        self.substitute(&infix.rhs);
                    }
                    Expr::Unary(unary) => {
                        self.substitute(&unary.expr);
                    }
                    Expr::None
                    | Expr::True
                    | Expr::False
                    | Expr::Int(_)
                    | Expr::UInt(_)
                    | Expr::Float(_)
                    | Expr::String(_)
                    | Expr::Break
                    | Expr::Continue => {}
                    Expr::Match(match_expr) => {
                        todo!()
                    }
                    Expr::Closure(closure) => {
                        todo!()
                    }
                    Expr::Assign(assign) => {
                        self.substitute(&assign.lhs);
                        self.substitute(&assign.rhs);
                    }
                    Expr::Field(field) => {
                        self.substitute(&field.lhs);
                    }
                    Expr::Index(index) => {
                        self.substitute(&index.expr);
                        self.substitute(&index.key);
                    }
                    Expr::Path(path) => {
                        // Not sure what to do here.
                    }
                }
            }
            NodeKind::Ty(_) => {}
            NodeKind::DestructureExpr(_) => {}
            NodeKind::Stmt(stmt) => match stmt {
                Stmt::Let(let_stmt) => {
                    let_stmt
                        .initializer
                        .as_ref()
                        .map(|initializer| self.substitute(initializer));
                    self.substitute(&let_stmt.local_var);
                }
                Stmt::For(for_stmt) => {
                    self.substitute(&for_stmt.body);
                    self.substitute(&for_stmt.range);
                }
                Stmt::If(if_stmt) => {
                    self.substitute(&if_stmt.condition);
                    self.substitute(&if_stmt.if_true);
                    if_stmt
                        .if_false
                        .as_ref()
                        .map(|if_false| self.substitute(if_false));
                }
                Stmt::Return(return_stmt) => {
                    return_stmt
                        .value
                        .as_ref()
                        .map(|value| self.substitute(value));
                }
                Stmt::While(while_stmt) => {
                    self.substitute(&while_stmt.condition);
                    self.substitute(&while_stmt.block);
                }
                Stmt::Block(block) => self.substitute(block),
                Stmt::Expression(expr) => self.substitute(&expr.expr),
            },
            NodeKind::Block(block) => {
                block
                    .stmts
                    .into_iter()
                    .for_each(|stmt| self.substitute(stmt));
            }
            NodeKind::Param(_) => {}
            NodeKind::Field(_) => {}
            NodeKind::LocalVar(_) => {}
            NodeKind::Pattern(_) => {}
            NodeKind::MatchArm(_) => {}
            // These can safely be ignored since they should not be traversed.
            NodeKind::Class(_) => {}
            NodeKind::Enum(_) => {}
            NodeKind::Trait(_) => {}
            NodeKind::TraitImpl(_) => {}
        }

        let ty = self.ty_map.get(node_id).unwrap().clone();
        let ty = self.probe_ty(ty);
        self.ty_map.insert(node_id, ty);
    }

    fn probe_ty(&mut self, ty: &TyKind) -> &TyKind {
        // TODO: Intern types
        match ty {
            TyKind::Array(array) => self.alloc(TyKind::Array(self.probe_ty(array))),
            TyKind::TraitBound(trait_bound) => {
                todo!()
            }
            TyKind::Fn(closure) => {
                let params =
                    self.alloc_slice(closure.params.into_iter().map(|param| self.probe_ty(param)));
                let ret_ty = self.probe_ty(closure.ret_ty);
                self.alloc(TyKind::Fn(Fn::new(params, ret_ty)));
            }
            TyKind::Infer(ty_var) => {
                // Unwrap should be safe during substitution
                self.unify_table.probe(*ty_var).unwrap()
            }
            TyKind::Adt(_) | TyKind::Primitive(_) => ty,
        }
    }

    fn fresh_ty(&mut self) -> &TyKind {
        self.alloc(TyKind::Infer(self.unify_table.fresh_ty()))
    }

    fn translate_var(&mut self, existing_var: &hir::LocalVar) -> LocalVar {
        LocalVar::new(existing_var.ident, existing_var.id)
    }

    fn translate_generic_param(&mut self, generic_param: &hir::GenericParam) -> GenericParam {}

    fn maybe_resolve_ty(&mut self, existing_ty: Option<&hir::Ty>) -> Option<Ty<'hir>> {
        existing_ty.map(|ty| self.resolve_ty(ty))
    }

    fn resolve_ty(&mut self, existing_ty: &hir::Ty) -> Ty<'hir> {
        let ty_kind = match existing_ty.kind {
            hir::TyKind::Array(array_ty) => TyKind::Array(self.resolve_ty(array_ty)),
            hir::TyKind::Path(path) => self.resolve_path_ty(path),
            hir::TyKind::GenericParam(generic_param) => {
                let generic_param = self.resolve_generic_param(generic_param);
                TyKind::GenericParam(generic_param)
            }
            hir::TyKind::TraitBound(trait_bound) => {
                let trait_bound = self.resolve_trait_bound(trait_bound);
                TyKind::TraitBound(trait_bound)
            }
            hir::TyKind::Closure(closure) => {
                let params =
                    self.alloc_slice(closure.params.iter().map(|param| self.resolve_ty(param)));
                let ret_ty = self.resolve_ty(closure.ret_ty);
                let generics = self.alloc_slice([]);
                TyKind::Fn(Fn { params, ret_ty }, generics)
            }
            hir::TyKind::Primitive(primitive) => match primitive {
                Primitive::U8 => TyKind::Uint(UintTy::U8),
                Primitive::U16 => TyKind::Uint(UintTy::U16),
                Primitive::U32 => TyKind::Uint(UintTy::U32),
                Primitive::U64 => TyKind::Uint(UintTy::U64),
                Primitive::I8 => TyKind::Int(IntTy::I8),
                Primitive::I16 => TyKind::Int(IntTy::I16),
                Primitive::I32 => TyKind::Int(IntTy::I32),
                Primitive::I64 => TyKind::Int(IntTy::I64),
                Primitive::F32 => TyKind::Float(FloatTy::F32),
                Primitive::F64 => TyKind::Float(FloatTy::F64),
                Primitive::Str => TyKind::Str,
                Primitive::Boolean => TyKind::Boolean,
                Primitive::None => TyKind::None,
            },
        };
        self.intern_ty(ty_kind)
    }

    fn resolve_generic_param(&mut self, param: &hir::GenericParam) -> GenericParam<'hir> {
        let trait_bound = self.maybe_resolve_trait_bound(&param.trait_bound);
        GenericParam {
            ident: param.ident,
            trait_bound,
        }
    }

    fn resolve_path_ty(&mut self, path: &hir::PathTy) -> TyKind<'hir> {
        let trait_ty = self.ty_cache.resolve_def(path.definition, || {
            let node = self
                .hir_map
                .krate(&path.definition)
                .node(&path.definition.local_id());
            node
        });
        let generics = path
            .generics
            .iter()
            .map(|generic| self.resolve_ty(generic))
            .collect();
        match trait_ty {
            DefType::Class(class_def) => TyKind::Class(class_def, generics),
            DefType::Enum(enum_def) => TyKind::Enum(enum_def, generics),
            DefType::Member(member_def) => TyKind::Member(member_def, generics),
            DefType::Trait(trait_def) => {
                let trait_bound = self.alloc_slice([self.alloc(Trait {
                    trait_def,
                    generics,
                })]);
                TyKind::TraitBound(trait_bound)
            }
            DefType::Fn(fn_def) => TyKind::Fn(fn_def, generics),
        }
    }

    fn maybe_resolve_trait_bound(
        &mut self,
        trait_bound: &Option<hir::TraitBound>,
    ) -> Option<TraitBound<'hir>> {
        if let Some(trait_bound) = trait_bound {
            return Some(self.resolve_trait_bound(trait_bound));
        }
        None
    }

    fn resolve_trait_bound(&mut self, trait_bound: hir::TraitBound) -> TraitBound<'hir> {
        todo!()
    }

    fn intern_ty(&mut self, ty_kind: TyKind<'hir>) -> Ty<'hir> {
        let interned_kind = self.interner.intern(ty_kind);
        Ty {
            kind: interned_kind,
        }
    }

    fn alloc<T>(&self, val: T) -> &'hir T {
        self.thir_allocator.alloc(val)
    }

    fn alloc_slice<I, T>(&self, slice: I) -> &'hir mut [T]
    where
        T: Copy,
        I: IntoIterator<Item = T>,
        I::IntoIter: ExactSizeIterator,
    {
        self.thir_allocator.alloc_slice_fill_iter(slice)
    }
}

type Constraints<'hir> = Vec<Constraint<'hir>>;

#[derive(Debug)]
enum Constraint<'hir> {
    Equal(&'hir TyKind<'hir>, &'hir TyKind<'hir>),
    Assignable(&'hir TyKind<'hir>, &'hir TyKind<'hir>),
    Array(&'hir TyKind<'hir>),
    // Callable(CallableConstraint), // Arg types
}

#[derive(Debug)]
pub struct CallableConstraint<'hir> {
    target_ty: Ty<'hir>,
    args: Vec<Ty<'hir>>,
    ret_ty: Ty<'hir>,
}

impl CallableConstraint {
    pub fn new(target_ty: Type, args: Vec<Type>, ret_ty: Type) -> Self {
        Self {
            target_ty,
            args,
            ret_ty,
        }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize)]
pub struct Array<'hir> {
    ty: &'hir TyKind<'hir>,
}

impl<'hir> Array<'hir> {
    pub fn new(ty: &'hir TyKind<'hir>) -> Self {
        Self { ty }
    }
}

/// Defines a concrete type of a potentially generic Class.
#[derive(Clone, PartialEq, Debug, Serialize)]
pub struct Class<'hir> {
    definition: DefId,
    fields: Vec<&'hir TyKind<'hir>>,
    generics: LDefMap<Type>,
}

impl Class {
    pub fn new(definition: DefId, fields: Types, generics: LDefMap<Type>) -> Self {
        Self {
            definition,
            fields,
            generics,
        }
    }

    pub fn fn_sig(&self) -> FnSig {
        FnSig::new(self.fields.clone(), Type::Class(self.clone()))
    }
}

impl<'hir> TraitBound<'hir> {
    pub fn new(bounds: &'hir [&'hir TyKind<'hir>]) -> Self {
        Self { bounds }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize)]
pub struct FnSig<'hir> {
    params: &'hir [&'hir TyKind<'hir>],
    ret_ty: &'hir TyKind<'hir>,
}

impl<'hir> FnSig<'hir> {
    pub fn new(params: &'hir [&'hir TyKind<'hir>], ret_ty: &'hir TyKind<'hir>) -> Self {
        Self { params, ret_ty }
    }
}
