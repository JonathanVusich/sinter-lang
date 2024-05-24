#![allow(unused)]

use std::cell::RefCell;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt::{Debug, Display};

use bumpalo::Bump;
use itertools::Itertools;
use serde::{Deserialize, Serialize};

use ast::{InfixOp, UnaryOp};
use diagnostics::{Diagnostic, Diagnostics};

use hir::{HirCrate, HirMap, Item, ItemKind, Node, Primitive};
use id::{DefId, LocalDefId};
use interner::{InternedStr, Interner};
use typed_hir::{
    ArrayExpr, ClassDef, ClosureDef, EnumDef, Expr, ExprId, ExprKind, Fields, FloatTy, FnDef,
    FnDefs, GenericParam, GenericParams, Generics, IntTy, LocalVar, MemberDef, MemberDefs, Params,
    Stmt, Thir, ThirCrate, ThirMap, Trait, TraitBound, TraitDef, Ty, TyKind, TyVar, UintTy,
};
use types::LDefMap;

use crate::unification::UnificationTable;

mod trait_solver;
mod unification;

pub fn infer_types<'hir>(
    diagnostics: &'hir Diagnostics,
    hir_allocator: &'hir Bump,
    hir_map: &'hir HirMap<'hir>,
) -> Option<ThirMap<'hir>> {
    let mut crates = Vec::default();
    let ty_def_cache = TyDefCache::new(hir_allocator);
    for krate in hir_map.krates() {
        let crate_inference =
            CrateInference::new(diagnostics, &ty_def_cache, hir_allocator, krate, hir_map);
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
pub struct CrateInference<'hir, 'cache> {
    diagnostics: &'hir Diagnostics,
    ty_cache: &'cache TyDefCache<'hir>,
    hir_map: &'hir HirMap<'hir>,
    krate: &'hir HirCrate<'hir>,

    hir_allocator: &'hir Bump,
    bodies: LDefMap<Thir<'hir>>,
}

#[derive(Debug)]
pub struct TyDefCache<'a> {
    ty_allocator: &'a Bump,
    defs: RefCell<HashMap<DefId, &'a DefType<'a>>>,
    interner: Interner<'a, TyKind<'a>>,
}

#[derive(Debug)]
pub enum DefType<'a> {
    Class(&'a ClassDef<'a>),
    Enum(&'a EnumDef<'a>),
    Member(&'a MemberDef<'a>),
    Trait(&'a TraitDef<'a>),
    Fn(&'a FnDef<'a>),
    GenericParam(&'a GenericParam<'a>),
}

impl<'a> TyDefCache<'a> {
    pub fn new(ty_allocator: &'a Bump) -> Self {
        Self {
            ty_allocator,
            defs: Default::default(),
            interner: Default::default(),
        }
    }

    pub fn resolve_def<F: FnOnce() -> &'a DefType<'a>>(
        &self,
        def_id: DefId,
        resolver: F,
    ) -> &'a DefType<'a> {
        if let Some(occupied) = self.defs.borrow().get(&def_id) {
            return *occupied;
        }
        let resolved_value = resolver();
        self.defs.borrow_mut().insert(def_id, resolved_value);
        resolved_value
    }

    pub fn intern(&self, kind: TyKind<'a>) -> Ty<'a> {
        let kind: &'a TyKind<'a> = self
            .interner
            .intern(kind, |val| self.ty_allocator.alloc(val));
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
pub struct InferCtxt<'hir, 'cache> {
    // Contains the current THIR body and information used to unify all the types.
    thir: Thir<'hir>,
    unify_table: UnificationTable<'hir>,

    hir_allocator: &'hir Bump,
    ty_cache: &'cache TyDefCache<'hir>,
}

impl<'hir, 'cache> InferCtxt<'hir, 'cache> {
    /// The main function that performs type inference. Every function body or
    /// expression should have a return type defined at compile time, and we can
    /// use this to infer and validate an entire expression/blocks' types.
    fn check_expr(mut self, expr: &hir::Expr<'hir>, ret_ty: Ty<'hir>) -> Thir<'hir> {
        self.check(expr, ret_ty);
        self.thir
    }

    /// The main function that performs type inference. Every function body or
    /// expression should have a return type defined at compile time, and we can
    /// use this to infer and validate an entire expression/blocks' types.
    fn check_block(self, block: &hir::Block) -> Thir<'hir> {
        todo!()
    }

    fn check(&mut self, expr: &hir::Expr<'hir>, ty: Ty<'hir>) {
        let expr = match (expr.kind, ty) {
            (hir::ExprKind::None, Ty { kind: TyKind::None }) => Expr {
                kind: ExprKind::None,
                ty,
            },
            (
                hir::ExprKind::True,
                Ty {
                    kind: TyKind::Boolean,
                },
            ) => Expr {
                kind: ExprKind::True,
                ty,
            },
            (
                hir::ExprKind::False,
                Ty {
                    kind: TyKind::Boolean,
                },
            ) => Expr {
                kind: ExprKind::False,
                ty,
            },
            (
                hir::ExprKind::Int(int),
                Ty {
                    kind: TyKind::Int(IntTy::I64),
                },
            ) => Expr {
                kind: ExprKind::Int(int),
                ty,
            },
            (
                hir::ExprKind::UInt(uint),
                Ty {
                    kind: TyKind::Uint(UintTy::U64),
                },
            ) => Expr {
                kind: ExprKind::UInt(uint),
                ty,
            },
            (
                hir::ExprKind::Float(float),
                Ty {
                    kind: TyKind::Float(FloatTy::F64),
                },
            ) => Expr {
                kind: ExprKind::Float(float),
                ty,
            },
            (hir::ExprKind::String(string), Ty { kind: TyKind::Str }) => Expr {
                kind: ExprKind::String(string),
                ty,
            },
            (hir::ExprKind::None, Ty { kind: TyKind::None }) => Expr {
                kind: ExprKind::None,
                ty,
            },
            _ => {
                let (mut constraints, expr) = self.infer_expr(expr);
                constraints.push(Constraint::Assignable(ty, self.thir[expr].ty));
                self.unify_constraints(constraints);
                self.substitute_expr(expr);
                return;
            }
        };
        self.thir.insert_expr(expr);
    }

    fn infer_expr(&mut self, expr: &hir::Expr<'hir>) -> (Constraints<'hir>, ExprId) {
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
        let expr_id = self.thir.insert_expr(expr);
        (constraints, expr_id)
    }

    fn infer_array(&mut self, array_expr: hir::ArrayExpr<'hir>) -> (Constraints<'hir>, Expr<'hir>) {
        match array_expr {
            hir::ArrayExpr::Sized { initializer, size } => {
                let (mut constraints, initializer) = self.infer_expr(initializer);
                let (size_constraints, size) = self.infer_expr(size);
                constraints.extend(size_constraints);

                let kind = TyKind::Array(self.thir[initializer].ty);
                let expr = Expr {
                    kind: ExprKind::Array(ArrayExpr::Sized { initializer, size }),
                    ty: self.ty_cache.intern(kind),
                };
                (constraints, expr)
            }
            hir::ArrayExpr::Unsized { initializers } => {
                let mut constraints = Constraints::default();
                let mut inits = Vec::default();
                let array_ty = self
                    .ty_cache
                    .intern(TyKind::Infer(self.unify_table.fresh_ty()));

                for initializer in initializers {
                    let (c, initializer) = self.infer_expr(initializer);
                    constraints.extend(c);

                    let inferred_ty = self.thir[initializer].ty;
                    constraints.push(Constraint::Assignable(array_ty, inferred_ty));
                    inits.push(initializer);
                }
                let expr = Expr {
                    kind: ExprKind::Array(ArrayExpr::Unsized {
                        initializers: inits.into_boxed_slice(),
                    }),
                    ty,
                };
                (constraints, expr)
            }
        }
    }

    fn infer_call(&self, call: hir::CallExpr) -> (Constraints<'hir>, Expr<'hir>) {
        todo!()
    }

    fn infer_infix(&self, infix: hir::InfixExpr) -> (Constraints<'hir>, Expr<'hir>) {
        todo!()
    }

    fn infer_unary(&self, unary: hir::UnaryExpr) -> (Constraints<'hir>, Expr<'hir>) {
        todo!()
    }

    fn infer_static(
        &self,
        kind: ExprKind<'hir>,
        ty_kind: TyKind<'hir>,
    ) -> (Constraints<'hir>, Expr<'hir>) {
        let expr = Expr {
            kind,
            ty: self.ty_cache.intern(ty_kind),
        };
        (vec![], expr)
    }

    fn infer_int(&self, int: i64) -> (Constraints<'hir>, Expr) {
        todo!()
    }

    fn infer_uint(&self, int: u64) -> (Constraints<'hir>, Expr) {
        todo!()
    }

    fn infer_float(&self, float: f64) -> (Constraints<'hir>, Expr) {
        todo!()
    }

    fn infer_string(&self, string: InternedStr) -> (Constraints<'hir>, Expr<'hir>) {
        todo!()
    }

    fn infer_match(&self, match_expr: hir::MatchExpr) -> (Constraints<'hir>, Expr<'hir>) {
        todo!()
    }

    fn infer_closure(&self, closure: hir::ClosureExpr) -> (Constraints<'hir>, Expr<'hir>) {
        todo!()
    }

    fn infer_assign(&self, assign: hir::AssignExpr) -> (Constraints<'hir>, Expr<'hir>) {
        todo!()
    }

    fn infer_field(&self, field: hir::FieldExpr) -> (Constraints<'hir>, Expr<'hir>) {
        todo!()
    }

    fn infer_index(&self, index_expr: hir::IndexExpr) -> (Constraints<'hir>, Expr<'hir>) {
        todo!()
    }

    fn infer_path(&self, path: hir::PathExpr) -> (Constraints<'hir>, Expr<'hir>) {
        todo!()
    }

    fn infer_block(&self, block: hir::Block) -> (Constraints<'hir>, Expr<'hir>) {
        todo!()
    }

    /// Walk the expression tree and normalize the types
    /// which should generate a completely typed expression tree.
    fn substitute_expr(&mut self, expr_id: ExprId) {
        let expr = &self.thir[expr_id];
        match &expr.kind {
            ExprKind::Array(ArrayExpr::Sized { initializer, size }) => {
                self.substitute_expr(*initializer);
                self.substitute_expr(*size);
            }
            ExprKind::Array(ArrayExpr::Unsized { initializers }) => {
                initializers
                    .iter()
                    .for_each(|initializer| self.substitute_expr(*initializer));
            }
            ExprKind::Call(_) => {}
            ExprKind::Infix(_) => {}
            ExprKind::Unary(_) => {}
            ExprKind::None => {}
            ExprKind::True => {}
            ExprKind::False => {}
            ExprKind::Int(_) => {}
            ExprKind::UInt(_) => {}
            ExprKind::Float(_) => {}
            ExprKind::String(_) => {}
            ExprKind::Match(_) => {}
            ExprKind::Closure(_) => {}
            ExprKind::Assign(_) => {}
            ExprKind::Field(_) => {}
            ExprKind::Index(_) => {}
            ExprKind::Block(_) => {}
            ExprKind::Path(_) => {}
            ExprKind::Break => {}
            ExprKind::Continue => {}
        }
    }

    fn unify_constraints(&self, constraints: Constraints<'hir>) {
        constraints
            .into_iter()
            .for_each(|constraint| self.unify(constraint))
    }

    fn unify(&self, constraint: Constraint<'hir>) {
        match constraint {
            Constraint::Equal(lhs, rhs) => {
                self.unify_ty_ty(lhs, rhs, Ty::eq);
                // TODO: Handle type errors
            }
            Constraint::Assignable(lhs, rhs) => {
                todo!()
            }
        }
    }

    fn unify_ty_ty(
        &self,
        lhs: Ty<'hir>,
        rhs: Ty<'hir>,
        assignable_check: fn(&Ty, &Ty) -> bool,
    ) -> bool {
        let lhs = self.normalize_ty(lhs).unwrap_or(lhs);
        let rhs = self.normalize_ty(rhs).unwrap_or(rhs);

        match (lhs, rhs) {
            (TyKind::Infer(infer), other) | (other, TyKind::Infer(infer)) => {
                self.unify_var_ty(infer, other, assignable_check)
            }
            (TyKind::Infer(lhs), TyKind::Infer(rhs)) => self.unify_var_var(lhs, rhs),
            /// Both types are at least partially known, so we unify them.
            (lhs, rhs) => {
                if !assignable_check(&lhs, &rhs) {
                    return false;
                }
                true
            }
        }
    }

    fn unify_var_ty(
        &self,
        var: TyVar,
        ty: Ty<'hir>,
        assignable_check: fn(&Ty, &Ty) -> bool,
    ) -> bool {
        if !self.unify_table.unify_var_ty(var, ty, assignable_check) {
            // TODO: Record type error
            return false;
        }
        true
    }

    fn unify_var_var(&self, lhs: TyVar, rhs: TyVar) -> bool {
        if !self.unify_table.unify_var_var(lhs, rhs) {
            // TODO: Record type error
            return false;
        }
        true
    }

    /// This method inspects a given type and returns an optional new type
    /// if there is some aspect of the type that can be updated due to constraint solving.
    fn normalize_ty(&self, ty: Ty<'hir>) -> Option<Ty<'hir>> {
        match ty.kind {
            TyKind::Array(array) => self
                .normalize_ty(*array)
                .map(|inner_ty| self.ty_cache.intern(TyKind::Array(inner_ty))),
            TyKind::Class(class_def, generics) => self
                .normalize_tys(generics)
                .map(|generics| self.ty_cache.intern(TyKind::Class(class_def, generics))),
            TyKind::Enum(enum_def, generics) => self
                .normalize_tys(generics)
                .map(|generics| self.ty_cache.intern(TyKind::Enum(enum_def, generics))),
            TyKind::Member(member_def, generics) => self
                .normalize_tys(generics)
                .map(|generics| self.ty_cache.intern(TyKind::Member(member_def, generics))),
            TyKind::TraitBound(trait_bound) => self
                .normalize_trait_bound(trait_bound)
                .map(|trait_bound| self.ty_cache.intern(TyKind::TraitBound(trait_bound))),
            TyKind::GenericParam(GenericParam { ident, trait_bound }) => match trait_bound {
                Some(trait_bound) => self.normalize_trait_bound(trait_bound).map(|trait_bound| {
                    let generic_param = self.alloc(GenericParam {
                        ident: *ident,
                        trait_bound: Some(trait_bound),
                    });
                    self.ty_cache.intern(TyKind::GenericParam(generic_param))
                }),
                None => None,
            },
            TyKind::Fn(fn_def, generics) => self
                .normalize_tys(generics)
                .map(|generics| self.ty_cache.intern(TyKind::Fn(fn_def, generics))),
            TyKind::Closure(closure_def) => {
                // TODO: There is a bug here where the params may need normalization but not the return type.
                // Need to properly handle both cases.
                self.normalize_ty(closure_def.return_type)
                    .map(|return_type| {
                        let params = self
                            .normalize_tys(closure_def.params)
                            .unwrap_or(closure_def.params);
                        let closure_def = self.alloc(ClosureDef {
                            params,
                            return_type,
                        });
                        self.ty_cache.intern(TyKind::Closure(closure_def))
                    })
            }
            TyKind::Infer(ty_var) => {
                let probed = self.unify_table.probe(*ty_var);
                match probed {
                    None => None,
                    Some(ty_kind) => self.normalize_ty(ty_kind),
                }
            }
            TyKind::Float(_)
            | TyKind::Int(_)
            | TyKind::Uint(_)
            | TyKind::Str
            | TyKind::Boolean
            | TyKind::None => None,
        }
    }

    fn normalize_tys(&self, generics: &'hir [Ty<'hir>]) -> Option<&'hir [Ty<'hir>]> {
        let mut replaced_generics = Vec::with_capacity(generics.len());
        let mut any_replaced = false;
        for generic in generics {
            match self.normalize_ty(*generic) {
                None => {
                    replaced_generics.push(*generic);
                }
                Some(generic) => {
                    any_replaced = true;
                    replaced_generics.push(generic);
                }
            };
        }
        if any_replaced {
            Some(self.alloc_slice(replaced_generics))
        } else {
            None
        }
    }

    fn normalize_trait_bound(&self, trait_bound: TraitBound<'hir>) -> Option<TraitBound<'hir>> {
        let mut replaced_traits = Vec::with_capacity(trait_bound.len());
        let mut any_replaced = false;
        for tr in trait_bound {
            match self.normalize_tys(tr.generics) {
                None => {
                    replaced_traits.push(*tr);
                }
                Some(generics) => {
                    any_replaced = true;
                    let trait_def = tr.trait_def;
                    replaced_traits.push(self.alloc(Trait {
                        trait_def,
                        generics,
                    }));
                }
            };
        }
        if any_replaced {
            Some(self.alloc_slice(replaced_traits))
        } else {
            None
        }
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
        generic_params: GenericParams<'hir>,
        ty_interner: &'cache TyDefCache<'hir>,
        hir_allocator: &'hir Bump,
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

impl<'hir, 'cache> CrateInference<'hir, 'cache> {
    pub fn new(
        diagnostics: &'hir Diagnostics,
        ty_cache: &'cache TyDefCache<'hir>,
        hir_allocator: &'hir Bump,
        krate: &'hir HirCrate,
        hir_map: &'hir HirMap,
    ) -> Self {
        Self {
            diagnostics,
            ty_cache,
            hir_allocator,
            hir_map,
            krate,
            bodies: Default::default(),
        }
    }

    // TODO: Returned interned ty representation to reduce memory overhead.
    // TODO: Record existing types for class fields and other nodes whose types are known statically.
    pub fn infer_bodies(mut self) -> Option<LDefMap<Thir<'hir>>> {
        for item in self.krate.items.iter() {
            match item.kind {
                ItemKind::Constant(constant) => self.infer_constant(constant),
                ItemKind::Class(class_def) => self.infer_class(class_def),
                ItemKind::Enum(enum_def) => self.infer_enum(enum_def),
                ItemKind::Member(member_def) => self.infer_member(member_def),
                ItemKind::Fn(fn_def) => self.check_fn_def(fn_def),
                ItemKind::Trait(trait_def) => self.infer_trait_def(trait_def),
                ItemKind::TraitImpl(trait_impl_def) => self.check_trait_impl_def(trait_impl_def),
                _ => {
                    // Panic?
                }
            }
        }
        Some(self.bodies)
    }

    fn infer_constant(&mut self, constant: &hir::Constant<'hir>) {
        let ret_ty = self.resolve_ty(constant.ty);

        let mut infer_ctxt: InferCtxt = InferCtxt::new(
            GenericParams::default(),
            &self.ty_cache,
            &self.hir_allocator,
            ret_ty,
        );
        let thir = infer_ctxt.check_expr(constant.initializer, ret_ty);
        self.bodies.insert(constant.initializer.id, thir);
    }

    fn infer_class(&mut self, class_def: &hir::ClassDef<'hir>) {
        for function in class_def.fn_defs {
            self.check_fn_def(function);
        }
    }

    fn infer_enum(&mut self, node: &hir::EnumDef<'hir>) {
        for member in node.members {
            for function in member.fn_defs {
                self.check_fn_def(function);
            }
        }
        for function in node.fn_defs {
            self.check_fn_def(function);
        }
    }

    fn infer_trait_def(&mut self, trait_def: &hir::TraitDef<'hir>) {
        for function in trait_def.fn_defs {
            self.check_fn_def(function);
        }
    }

    fn check_trait_impl_def(&mut self, trait_impl_def: &hir::TraitImplDef<'hir>) {
        for function in trait_impl_def.fn_defs {
            self.check_fn_def(function);
        }
    }

    fn check_member_def(&mut self, member_def: &hir::MemberDef<'hir>) {
        for function in member_def.fn_defs {
            self.check_fn_def(function);
        }
    }

    fn check_fn_def(&mut self, fn_def: &hir::FnDef<'hir>) {
        if let Some(block) = fn_def.body {
            let generic_params = self.resolve_generic_params(fn_def.sig.generic_params);
            let ret_ty = self
                .maybe_resolve_ty(fn_def.sig.ret_ty)
                .unwrap_or_else(|| self.intern_ty(TyKind::None));

            let infer_ctxt =
                InferCtxt::new(generic_params, &self.ty_cache, &self.hir_allocator, ret_ty);
            let thir = infer_ctxt.check_block(block);
        }
    }

    fn check_block(&self, block: &hir::Block) -> Thir<'hir> {
        todo!()
    }

    /// This method is not fallible since all constraints should already have been satisfied.
    /// If this method panics, it is due to improper constraint generation.
    fn substitute_expr(&self, expr: ExprId) -> Ty<'hir> {
        todo!()
    }

    // fn unify_fn_sig(&mut self, fn_sig: &FnSig, args: &[Type], ret_ty: Type) -> bool {
    //     if args.len() != fn_sig.params.len() {
    //         self.diagnostics.push(Diagnostic::BlankError);
    //         return false;
    //     }
    //
    //     for x in 0..args.len() {
    //         let arg = args[x].clone();
    //         let param = fn_sig.params[x].clone();
    //
    //         if !self.unify_ty_ty(param, arg, Type::assignable) {
    //             return false;
    //         }
    //     }
    //
    //     self.unify_ty_ty(ret_ty, *fn_sig.ret_ty.clone(), Type::assignable)
    // }

    fn maybe_resolve_ty(&mut self, existing_ty: Option<&hir::Ty<'hir>>) -> Option<Ty<'hir>> {
        existing_ty.map(|ty| self.resolve_ty(ty))
    }

    fn resolve_ty(&mut self, existing_ty: &hir::Ty<'hir>) -> Ty<'hir> {
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
                let params = self.hir_allocator.alloc_slice_fill_iter(
                    closure.params.iter().map(|param| self.resolve_ty(param)),
                );
                let return_type = self.resolve_ty(closure.ret_ty);
                let closure_def = self.alloc(ClosureDef {
                    params,
                    return_type,
                });
                TyKind::Closure(closure_def)
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

    fn resolve_class_def(&mut self, class_def: &hir::ClassDef<'hir>) -> &'hir ClassDef<'hir> {
        let generic_params = self.resolve_generic_params(class_def.generic_params);
        let fields = self.resolve_fields(class_def.fields);
        let fns = self.resolve_fn_defs(class_def.fn_defs);
        let class_def = self.alloc(ClassDef {
            name: class_def.name,
            class_type: class_def.class_type,
            generic_params,
            fields,
            fns,
        });
        class_def
    }

    fn resolve_enum_def(&mut self, enum_def: &hir::EnumDef<'hir>) -> &'hir EnumDef<'hir> {
        let name = enum_def.name;
        let generic_params = self.resolve_generic_params(enum_def.generic_params);
        let members = self.resolve_members(enum_def.members);
        let fn_defs = self.resolve_fn_defs(enum_def.fn_defs);
        let enum_def = self.alloc(EnumDef {
            name,
            generic_params,
            members,
            fn_defs,
        });
        enum_def
    }

    fn resolve_members(&mut self, members: hir::MemberDefs<'hir>) -> MemberDefs<'hir> {
        self.hir_allocator.alloc_slice_fill_iter(
            members
                .iter()
                .map(|member| self.resolve_enum_member(member)),
        )
    }

    fn resolve_enum_member(&mut self, enum_member: &hir::MemberDef<'hir>) -> &'hir MemberDef<'hir> {
        let name = enum_member.name;
        let fields = self.resolve_fields(enum_member.fields);
        let fn_defs = self.resolve_fn_defs(enum_member.fn_defs);

        let member_def = self.alloc(MemberDef {
            name,
            fields,
            fn_defs,
        });
        member_def
    }

    fn resolve_trait_def(&mut self, trait_def: &hir::TraitDef<'hir>) -> &'hir TraitDef<'hir> {
        let name = trait_def.name;
        let generic_params = self.resolve_generic_params(trait_def.generic_params);
        let fn_defs = self.resolve_fn_defs(trait_def.fn_defs);
        let trait_def = self.alloc(TraitDef {
            name,
            generic_params,
            fn_defs,
        });
        trait_def
    }

    fn resolve_fn_defs(&mut self, fn_defs: hir::FnDefs<'hir>) -> FnDefs<'hir> {
        self.hir_allocator
            .alloc_slice_fill_iter(fn_defs.iter().map(|fn_def| self.resolve_fn_def(fn_def)))
    }

    fn resolve_fn_def(&mut self, fn_def: &hir::FnDef<'hir>) -> &'hir FnDef<'hir> {
        let name = fn_def.sig.name;
        let generic_params = self.resolve_generic_params(fn_def.sig.generic_params);
        let params = self.resolve_params(fn_def.sig.params);
        let return_type = fn_def
            .sig
            .ret_ty
            .map(|ty| self.resolve_ty(ty))
            .unwrap_or_else(|| self.intern_ty(TyKind::None));

        let fn_def = self.alloc(FnDef {
            name,
            generic_params,
            params,
            return_type,
        });
        fn_def
    }

    fn resolve_fields(&mut self, fields: hir::Fields<'hir>) -> Fields<'hir> {
        self.hir_allocator
            .alloc_slice_fill_iter(fields.iter().map(|field| self.resolve_ty(field.ty)))
    }

    fn resolve_params(&mut self, params: hir::Params<'hir>) -> Params<'hir> {
        self.hir_allocator
            .alloc_slice_fill_iter(params.iter().map(|param| self.resolve_ty(param.ty)))
    }

    fn resolve_generic_params(&mut self, params: hir::GenericParams<'hir>) -> GenericParams<'hir> {
        self.hir_allocator
            .alloc_slice_fill_iter(params.iter().map(|param| self.resolve_generic_param(param)))
    }

    fn resolve_generic_param(&self, param: &hir::GenericParam<'hir>) -> &'hir GenericParam<'hir> {
        let trait_bound = self.maybe_resolve_trait_bound(&param.trait_bound);
        self.alloc(GenericParam {
            ident: param.ident,
            trait_bound,
        })
    }

    fn resolve_path_ty(&mut self, path: &hir::PathTy<'hir>) -> TyKind<'hir> {
        let trait_ty = self
            .ty_cache
            .resolve_def(path.definition, || self.resolve_path(path.definition));
        let generics = self
            .hir_allocator
            .alloc_slice_fill_iter(path.generics.iter().map(|generic| self.resolve_ty(generic)));
        match trait_ty {
            DefType::Class(class_def) => TyKind::Class(class_def, generics),
            DefType::Enum(enum_def) => TyKind::Enum(enum_def, generics),
            DefType::Member(member_def) => TyKind::Member(member_def, generics),
            DefType::Trait(trait_def) => {
                let trait_bound = self.hir_allocator.alloc_slice_fill_iter([self.alloc(Trait {
                    trait_def,
                    generics,
                })]);
                TyKind::TraitBound(trait_bound)
            }
            DefType::Fn(fn_def) => TyKind::Fn(fn_def, generics),
            DefType::GenericParam(generic_param) => TyKind::GenericParam(generic_param),
        }
    }

    fn resolve_path(&mut self, id: DefId) -> &'hir DefType<'hir> {
        let node = self.hir_map.krate(&id).node(&id.local_id());
        let def_type = match node {
            Node::Item(Item {
                kind: ItemKind::Class(class_def),
                ..
            }) => DefType::Class(self.resolve_class_def(class_def)),
            Node::Item(Item {
                kind: ItemKind::Enum(enum_def),
                ..
            }) => DefType::Enum(self.resolve_enum_def(enum_def)),
            Node::Item(Item {
                kind: ItemKind::Member(member_def),
                ..
            }) => DefType::Member(self.resolve_enum_member(member_def)),
            Node::Item(Item {
                kind: ItemKind::Fn(fn_def),
                ..
            }) => DefType::Fn(self.resolve_fn_def(fn_def)),
            Node::Item(Item {
                kind: ItemKind::Trait(trait_def),
                ..
            }) => DefType::Trait(self.resolve_trait_def(trait_def)),
            Node::Ty(hir::Ty {
                kind: hir::TyKind::GenericParam(param),
                ..
            }) => DefType::GenericParam(self.resolve_generic_param(param)),
            node => {
                dbg!(node);
                panic!("Unsupported node type!");
            }
        };
        self.alloc(def_type)
    }

    fn maybe_resolve_trait_bound(
        &self,
        trait_bound: &Option<hir::TraitBound>,
    ) -> Option<TraitBound<'hir>> {
        if let Some(trait_bound) = trait_bound {
            return Some(self.resolve_trait_bound(trait_bound));
        }
        None
    }

    fn resolve_trait_bound(&self, trait_bound: hir::TraitBound) -> TraitBound<'hir> {
        todo!()
    }

    fn intern_ty(&self, ty_kind: TyKind<'hir>) -> Ty<'hir> {
        self.ty_cache.intern(ty_kind)
    }

    fn alloc<T>(&self, val: T) -> &'hir T {
        self.hir_allocator.alloc(val)
    }
}

type Constraints<'hir> = Vec<Constraint<'hir>>;

#[derive(Debug)]
enum Constraint<'hir> {
    Equal(Ty<'hir>, Ty<'hir>),
    Assignable(Ty<'hir>, Ty<'hir>),
}

#[derive(Debug)]
pub struct CallableConstraint<'hir> {
    target_ty: Ty<'hir>,
    args: Vec<Ty<'hir>>,
    ret_ty: Ty<'hir>,
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
pub struct FnSig<'hir> {
    params: &'hir [&'hir TyKind<'hir>],
    ret_ty: &'hir TyKind<'hir>,
}

impl<'hir> FnSig<'hir> {
    pub fn new(params: &'hir [&'hir TyKind<'hir>], ret_ty: &'hir TyKind<'hir>) -> Self {
        Self { params, ret_ty }
    }
}
