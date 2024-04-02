#![allow(unused)]

use std::collections::HashMap;
use bumpalo::Bump;
use std::fmt::{Debug, Display};
use std::iter::zip;

use itertools::Itertools;
use serde::{Deserialize, Serialize};

use arena::Arena;
use ast::{ClassDef, FnDef, GlobalVarDef, InfixOp, TraitStmt, UnaryOp, ValueDef};
use diagnostics::{Diagnostic, Diagnostics};
use hir::{HirCrate, HirMap, ItemKind, Node};
use id::{DefId, LocalDefId};
use interner::Interner;
use macros::named_slice;
use typed_hir::{Block, Expr, LocalVar, MatchArm, Stmt, TyKind, ThirBodies, Thir, Ty};
use types::{DefMap, LDefMap};

use crate::unification::{UnificationTable};

mod trait_solver;
mod unification;

/// Contains the data needed to infer types for a given HIR crate.
/// The HIR nodes are transformed into their typed HIR representation
/// which ensures that all nodes have correct type information.
///
/// TODO: All types SHOULD also be interned across crates using a ty interner.
#[derive(Debug)]
pub struct CrateInference<'hir, 'thir> {
    diagnostics: &'hir mut Diagnostics,
    ty_interner: &'hir mut Interner<TyKind<'hir>>,
    hir_map: &'hir HirMap<'hir>,
    krate: &'hir HirCrate<'hir>,

    hir_allocator: &'hir mut Bump,
    thir_allocator: &'thir mut Bump,

    bodies: ThirBodies<'hir>,

    unify_table: UnificationTable<'hir>,
}

impl<'hir, 'thir> CrateInference<'hir, 'thir> {
    pub fn new(
        diagnostics: &'hir mut Diagnostics,
        ty_interner: &'hir mut Interner<TyKind<'hir>>,
        hir_allocator: &'hir mut Bump,
        thir_allocator: &'thir mut Bump,
        krate: &'hir HirCrate,
        hir_map: &'hir HirMap,
    ) -> Self {
        Self {
            diagnostics,
            ty_interner,
            hir_allocator,
            thir_allocator,
            hir_map,
            krate,
            bodies: Default::default(),
            unify_table: Default::default(),
        }
    }

    // TODO: Returned interned ty representation to reduce memory overhead.
    // TODO: Record existing types for class fields and other nodes whose types are known statically.
    pub fn infer_bodies(mut self) -> Option<ThirBodies<'hir>> {
        for item in self.krate.items.iter() {
            // Unwrap is safe here since all items should have corresponding nodes.
            let node = self.krate.nodes.get(item).unwrap();
            let ty = match node {
                // Since global lets are the only top level items that have a type (other than fns), we infer on them directly.
                Node::Item(item) => {
                    match item.kind {
                        ItemKind::Constant(constant) => self.infer_constant(constant),
                        ItemKind::Class(class_def) => self.infer_class(class_def),
                        ItemKind::Enum(enum_def) => self.infer_enum(enum_def),
                        ItemKind::Fn(fn_def) => self.infer_fn_def(fn_def),
                        ItemKind::Trait(trait_def) => self.infer_trait_def(trait_def),
                        ItemKind::TraitImpl(trait_impl_def) => self.infer_trait_impl_def(trait_impl_def),
                    }
                },
                _ => unreachable!(),
            };
        }
        Some(self.bodies)
    }

    fn check_node(&mut self, node: &LocalDefId, ty: Type) {
        let constraints = self.check(node, ty);

        if self.unify_constraints(constraints) {
            self.substitute(node);
        }
    }

    fn infer_node(&mut self, node: &LocalDefId) {
        let (constraints, ty) = self.infer(node);
        if self.unify_constraints(constraints) {
            self.substitute(node);
        }
    }

    fn infer_constant(&mut self, constant: &hir::Constant) {
        let ret_ty = self.translate_ty(constant.ty);
        let body = Thir {
            generic_tys: Vec::new(),
            ret_ty,
            stmts: Vec::new(),
            arms: Vec::new(),
            exprs: Vec::new(),
            blocks: Vec::new(),
            destructure_exprs: Vec::new(),
        };
        self.bodies.push(body);
        self.check_expr(constant.initializer, ret_ty); // Can safely ignore return val
    }

    fn infer_class(&mut self, node: &hir::ClassDef) {
        todo!()
    }

    fn infer_enum(&mut self, node: &hir::EnumDef) {
        todo!()
    }

    fn infer_trait_def(&mut self, node: &hir::TraitDef) {
        todo!()
    }

    fn infer_trait_impl_def(&mut self, node: &hir::TraitImplDef) {
        todo!()
    }

    fn infer_fn_def(&mut self, node: &hir::FnDef) {
        todo!()
    }

    fn check_expr(&mut self, expr: &hir::Expr, ty: &Ty) -> &Ty {
        // TODO: Intern types
        match (expr, ty) {
            (hir::Expr::None, TyKind::Primitive(FloatTy::None)) => {
                (Constraints::default(), Expr::new(ExprKind::None, self.alloc(TyKind::Primitive(FloatTy::None))))
            }
            (hir::Expr::True, TyKind::Primitive(FloatTy::Boolean)) => {
                (Constraints::default(), Expr::new(ExprKind::True, self.alloc(TyKind::Primitive(FloatTy::Boolean))))
            }
            (hir::Expr::False, TyKind::Primitive(FloatTy::Boolean)) => {
                (Constraints::default(), Expr::new(ExprKind::False, self.alloc(TyKind::Primitive(FloatTy::Boolean))))
            }
            // TODO: Support shorter int types
            (hir::Expr::Int(int), TyKind::Primitive(FloatTy::I64)) => {
                (Constraints::default(), Expr::new(ExprKind::Int(*int), self.alloc(TyKind::Primitive(FloatTy::I64))))
            }
            (hir::Expr::UInt(uint), TyKind::Primitive(FloatTy::U64)) => {
                (Constraints::default(), Expr::new(ExprKind::UInt(*uint), self.alloc(TyKind::Primitive(FloatTy::U64))))
            }
            (hir::Expr::Float(float), TyKind::Primitive(FloatTy::F64)) => {
                (Constraints::default(), Expr::new(ExprKind::Float(*float), self.alloc(TyKind::Primitive(FloatTy::F64))))
            }
            (hir::Expr::String(string), TyKind::Primitive(FloatTy::Str)) => {
                (Constraints::default(), Expr::new(ExprKind::String(*string), self.alloc(TyKind::Primitive(FloatTy::Str))))
            }
            _ => {
                let (mut constraints, expr) = self.infer_expr(expr);
                constraints.push(Constraint::Assignable(ty, expr.ty));
                (constraints, expr)
            }
        }
    }

    /// This method is not fallible since all constraints should already have been satisfied.
    /// If this method panics, it is due to improper constraint generation.
    fn substitute_expr(&mut self, expr: Expr) -> Expr {
        match &expr.kind {
            ExprKind::Array(ArrayExpr::Sized { initializer, size }) => {
                self.substitute_expr(initializer);
                self.substitute_expr(size);
                
            }
            ExprKind::Array(ArrayExpr::Unsized { initializers }) => {
                initializers.into_iter().for_each(|expr| self.substitute_expr(expr));
            }
            ExprKind::Call(call) => {
                self.substitute_expr(call.target);
                call.args.iter().for_each(|arg| self.substitute_expr(arg));
            }
            ExprKind::Infix(infix) => {
                self.substitute_expr(infix.lhs);
                self.substitute_expr(infix.rhs);
            }
            ExprKind::Unary(unary) => {
                self.substitute_expr(unary.expr);
            }
            ExprKind::Match(_) => {}
            ExprKind::Closure(_) => {}
            ExprKind::Assign(_) => {}
            ExprKind::Field(_) => {}
            ExprKind::Index(_) => {}
            ExprKind::Path(_) => {}
            ExprKind::Break |
            ExprKind::Continue |
            ExprKind::None |
            ExprKind::True |
            ExprKind::False |
            ExprKind::Int(_) |
            ExprKind::UInt(_) |
            ExprKind::Float(_) |
            ExprKind::String(_) => {}
        }

        expr.ty = self.probe_ty(expr.ty);
    }

    fn infer_expr(&mut self, node: &hir::Expr) -> (Constraints, &'hir Expr<'hir>) {
        let typed_expr = match node {
            hir::Expr::Array(hir::ArrayExpr::Sized { initializer, size }) => {
                let (mut constraints, initializer) = self.infer_expr(initializer);
                let (size_constr, size) = self.check_expr(size, &TyKind::Primitive(FloatTy::U64));
                
                constraints.extend(size_constr);

                (constraints, Expr::new(ExprKind::Array(ArrayExpr::Sized { initializer, size })))
            }
            hir::Expr::Array(hir::ArrayExpr::Unsized { initializers }) => {
                let mut typed_initializers = Vec::with_capacity(initializers.len());
                typed_initializers.extend(initializers.into_iter().map(|expr| self.infer_expr(expr)));
                Expr::new(ExprKind::Array(ArrayExpr::Unsized { initializers }))
            }
            hir::Expr::Call(_) => {}
            hir::Expr::Infix(_) => {}
            hir::Expr::Unary(_) => {}
            hir::Expr::None => {}
            hir::Expr::True => {}
            hir::Expr::False => {}
            hir::Expr::Int(_) => {}
            hir::Expr::UInt(_) => {}
            hir::Expr::Float(_) => {}
            hir::Expr::String(_) => {}
            hir::Expr::Match(_) => {}
            hir::Expr::Closure(_) => {}
            hir::Expr::Assign(_) => {}
            hir::Expr::Field(_) => {}
            hir::Expr::Index(_) => {}
            hir::Expr::Path(_) => {}
            hir::Expr::Break => {}
            hir::Expr::Continue => {}
        }
        todo!()
    }

    fn infer(&mut self, node: &LocalDefId) -> (Constraints, &'hir TyKind<'hir>) {
        let (constraints, ty) = match self.krate.node(node) {
            NodeKind::GlobalLet(global_let) => {
                let ty = self.translate_ty(global_let.ty.to_def_id(self.krate.id));
                self.ty_map.insert(&global_let.ty, ty.clone());
                self.ty_map.insert(&global_let.local_var, ty.clone());

                let constraints = self.check(&global_let.initializer, ty);
                (constraints, Type::None)
            }
            NodeKind::Block(block) => {
                // Create a new scope to track constraints on the return type.
                let ret_ty = self.fresh_ty();
                self.ret_tys.push(ret_ty.clone());
                let mut constraints = Constraints::default();
                for stmt in &block.stmts {
                    let (c, _) = self.infer(stmt);
                    constraints.extend(c);
                }
                self.ret_tys.pop();
                (constraints, ret_ty)
            }
            NodeKind::Stmt(stmt) => {
                match stmt {
                    Stmt::Let(let_stmt) => {
                        // Create fresh type var for the var type. This will be either inferred or
                        // constrained to the assigned type.
                        let mut constraints = Constraints::default();

                        let ty_var;
                        if let Some(ty) = let_stmt.ty {
                            let var_ty = self.translate_ty(ty.to_def_id(self.krate.id));
                            ty_var = var_ty.clone();

                            // TODO: Handle late assignment correctly.
                            let initializer = let_stmt.initializer.unwrap();
                            let initializer_ty =
                                self.translate_ty(initializer.to_def_id(self.krate.id));
                            constraints.push(Constraint::Assignable(var_ty, initializer_ty));

                            return (constraints, TyKind::none());
                        }

                        ty_var = self.fresh_ty(&let_stmt.local_var);
                        let (c, initializer_ty) = self.infer(&let_stmt.initializer.unwrap());
                        constraints.extend(c);
                        constraints.push(Constraint::Assignable(ty_var, initializer_ty));

                        (constraints, Type::None)
                    }
                    Stmt::Return(return_stmt) => {
                        let ret_ty = self.ret_tys.last().unwrap().clone();
                        if let Some(node) = return_stmt.value {
                            let (mut constraints, ty) = self.infer(&node);
                            constraints.push(Constraint::Assignable(ret_ty, ty));
                            return (constraints, Type::None);
                        }
                        (vec![Constraint::Equal(ret_ty, Type::None)], Type::None)
                    }

                    Stmt::Expression(expression) => {
                        let (mut constraints, ty) = self.infer(&expression.expr);
                        if expression.implicit_return {
                            let ret_ty = self.ret_tys.last().unwrap();
                            constraints.push(Constraint::Assignable(ret_ty.clone(), ty));
                            return (constraints, Type::None);
                        }
                        (constraints, ty)
                    }
                    _ => (Constraints::default(), self.fresh_ty(node)),
                }
            }
            NodeKind::Expr(expr) => match expr {
                Expr::Array(array) => match array {
                    ArrayExpr::Sized { initializer, size } => {
                        let size_c = self.check(size, Type::U64); // Should always be zero?
                        let (mut constraints, init_ty) = self.infer(initializer);
                        constraints.extend(size_c);
                        (constraints, Type::Array(Array::new(init_ty)))
                    }
                    ArrayExpr::Unsized { initializers } => {
                        let inferred_ty = self.fresh_ty(node);
                        let mut constraints = Vec::default();
                        for initializer in initializers {
                            let (c, ty) = self.infer(initializer);
                            constraints.extend(c);
                            constraints.push(Constraint::Assignable(ty, inferred_ty.clone()));
                        }
                        (constraints, Type::Array(Array::new(inferred_ty)))
                    }
                },
                Expr::Infix(infix) => {
                    let (lhs_c, lhs_ty) = self.infer(&infix.lhs);
                    let (rhs_c, rhs_ty) = self.infer(&infix.rhs);
                    let mut new_constr = Vec::with_capacity(lhs_c.len() + rhs_c.len() + 1);
                    new_constr.extend(lhs_c);
                    new_constr.extend(rhs_c);
                    match &infix.operator {
                        InfixOp::Assign => {
                            new_constr.push(Constraint::Assignable(lhs_ty, rhs_ty));
                            (new_constr, Type::None)
                        }
                        InfixOp::Add => {
                            new_constr.push(Constraint::Assignable(lhs_ty.clone(), rhs_ty));
                            (new_constr, lhs_ty)
                        }
                        token => {
                            todo!("{:?}", token)
                        }
                    }
                }
                Expr::Unary(unary) => {
                    let (mut constraints, ty) = self.infer(&unary.expr);
                    constraints.push(Constraint::Unary(ty.clone(), unary.operator));
                    (constraints, ty)
                }
                Expr::None => (Constraints::default(), Type::None),
                Expr::True => (Constraints::default(), Type::Boolean),
                Expr::False => (Constraints::default(), Type::Boolean),
                Expr::Int(int) => (Constraints::default(), Type::I64),
                Expr::UInt(int) => (Constraints::default(), Type::U64),
                Expr::Float(float) => (Constraints::default(), Type::F64),
                Expr::String(str) => (Constraints::default(), Type::Str),
                Expr::Closure(closure) => {
                    let params = vec![self.fresh_ty(node); closure.params.len()].into();
                    let (constraints, ty) = self.infer(&closure.stmt);
                    (constraints, Type::Fn(FnSig::new(params, ty)))
                }
                Expr::Assign(assign) => {
                    let (mut constraints, lhs_ty) = self.infer(&assign.lhs);
                    let (rhs_constr, rhs_ty) = self.infer(&assign.rhs);

                    constraints.extend(rhs_constr);
                    constraints.push(Constraint::Assignable(lhs_ty.clone(), rhs_ty));

                    (constraints, lhs_ty)
                }
                Expr::Call(call) => {
                    let mut arg_tys = Vec::default();

                    let (mut constraints, target_ty) = self.infer(&call.target);

                    // Infer the args to the fn call and save their constraints
                    for arg in &call.args {
                        let (c, ty) = self.infer(arg);
                        constraints.extend(c);
                        arg_tys.push(ty);
                    }

                    let ret_ty = self.fresh_ty(node);

                    // Infer the fn expr type and save constraints
                    constraints.push(Constraint::Callable(CallableConstraint::new(
                        target_ty,
                        arg_tys,
                        ret_ty.clone(),
                    )));

                    (constraints, ret_ty)
                }
                Expr::Path(path) => {
                    let last_segment = path.segments.last().unwrap();
                    match &last_segment.res {
                        // This should be unreachable since these are not valid paths and should be caught
                        // by the resolver.
                        Res::Crate(_) | Res::ModuleSegment(_, _) | Res::Module(_) => unreachable!(),
                        Res::ValueDef(value_def) => {
                            match value_def {
                                ValueDef::GlobalVar(GlobalVarDef { id }) => {
                                    let krate = self.hir_map.krate(id);
                                    let global_let = krate.global_let_stmt(&id.local_id());
                                    let ty = self.translate_ty(global_let.ty.to_def_id(krate.id));
                                    (Constraints::default(), ty)
                                }
                                ValueDef::Fn(FnDef { id }) => {
                                    let krate = self.hir_map.krate(id);
                                    let fn_stmt = krate.fn_stmt(&id.local_id());

                                    let param_tys = fn_stmt
                                        .sig
                                        .params
                                        .values()
                                        .map(|param| krate.param(param))
                                        .map(|param| self.translate_ty(param.ty.to_def_id(krate.id)))
                                        .collect_vec()
                                        .into();
                                    let ret_ty = fn_stmt
                                        .sig
                                        .return_type
                                        .map(|ret_ty| self.translate_ty(ret_ty.to_def_id(krate.id)))
                                        .unwrap_or(Type::None);

                                    let ty = Type::Fn(FnSig::new(param_tys, ret_ty));
                                    (Constraints::default(), ty)
                                }
                                // TODO: Add support for callable class + enum member paths?
                                ValueDef::EnumMember(_) => {
                                    todo!()
                                }
                                ValueDef::Class(ClassDef { id, fields, fns }) => {
                                    let krate = self.hir_map.krate(id);
                                    let class_stmt = krate.class_stmt(&id.local_id());

                                    // Create ty vars for each generic param that will be reused through all of the
                                    // references in the class definition. This should ensure that constraints are
                                    // propagated correctly.

                                    // Either that or we need to ensure that constraints are propagated correctly through
                                    // the function definition.

                                    let generic_params: LDefMap<Type> = class_stmt
                                        .generic_params
                                        .values()
                                        .copied()
                                        .map(|generic_param| {
                                            (
                                                generic_param,
                                                self.translate_ty(generic_param.to_def_id(krate.id)),
                                            )
                                        })
                                        .collect();

                                    self.generic_tys.push(generic_params);
                                    let params = class_stmt
                                        .fields
                                        .values()
                                        .map(|field| krate.field(field))
                                        .map(|field| field.ty)
                                        .map(|field| self.translate_ty(field.to_def_id(krate.id)))
                                        .collect::<Vec<_>>()
                                        .into();
                                    let generic_params = self.generic_tys.pop().unwrap();

                                    let ty = Type::Class(Class::new(*id, params, generic_params));
                                    let ret_val = (Constraints::default(), ty);

                                    ret_val
                                }
                                ValueDef::Enum(_) => {
                                    todo!()
                                }
                                ValueDef::Trait(_) => {
                                    todo!()
                                }
                            }
                        }
                        Res::Fn(_) => {
                            todo!()
                        }
                        Res::Local(local) => match local {
                            LocalDef::Var(node) => {
                                // Unwrap should be safe here since we should always have a
                                // preceding let stmt.
                                (
                                    Constraints::default(),
                                    self.ty_map.get(node).unwrap().clone(),
                                )
                            }
                            LocalDef::Generic(_) => {
                                todo!()
                            }
                        },
                        Res::Primitive(_) => {
                            todo!()
                        }
                    }
                }
                _ => todo!(),
            },
            node => {
                dbg!(node);
                todo!()
            }
        };
        self.ty_map.insert(node, ty.clone());
        (constraints, ty)
    }

    fn check(&mut self, node_id: &LocalDefId, ty: TyKind) -> Constraints {
        let node = self.krate.node(node_id);
        let constraints = match (node, &ty) {
            (NodeKind::Expr(Expr::Float(_)), TyKind::F32 | TyKind::F64) => {
                Constraints::default()
            }
            (NodeKind::Expr(Expr::Int(_)), TyKind::I64) => Constraints::default(),
            (NodeKind::Expr(Expr::String(_)), TyKind::Str) => Constraints::default(),
            (NodeKind::Expr(Expr::True), TyKind::Boolean) => Constraints::default(),
            (NodeKind::Expr(Expr::False), TyKind::Boolean) => Constraints::default(),
            (node, ty) => {
                let (mut constraints, inferred_ty) = self.infer(node_id);
                constraints.push(Constraint::Assignable(ty.clone(), inferred_ty));
                constraints
            }
        };
        self.ty_map.insert(node_id, ty);
        constraints
    }

    fn unify_constraints(&mut self, constraints: Constraints) -> Result<(), TypeError> {
        constraints
            .into_iter()
            .all(|constraint| self.unify(constraint))
    }

    fn unify(&mut self, constraint: Constraint) -> bool {
        match constraint {
            Constraint::Equal(lhs, rhs) => self.unify_ty_ty(lhs, rhs, Type::eq),
            Constraint::Assignable(lhs, rhs) => self.unify_ty_ty(lhs, rhs, Type::assignable),
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
            TyKind::Array(array) => {
                self.alloc(TyKind::Array(self.probe_ty(array)))
            }
            TyKind::TraitBound(trait_bound) => { todo!() }
            TyKind::Fn(closure) => {
                let params = self.alloc_slice(closure.params.into_iter()
                    .map(|param| self.probe_ty(param)));
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

    fn translate_ty(&mut self, existing_ty: &hir::Ty) -> Ty<'hir> {
        let ty_kind = match existing_ty.kind {
            hir::TyKind::Array(array_ty) => {
                TyKind::Array(self.translate_ty(array_ty))
            }
            hir::TyKind::Path(path) => {
                todo!()
            }
            hir::TyKind::GenericParam(_) => {}
            hir::TyKind::TraitBound(_) => {}
            hir::TyKind::Closure(_) => {}
            hir::TyKind::Primitive(_) => {}
        };
        self.intern_ty(ty_kind)
    }
    
    fn intern_ty(&mut self, ty_kind: TyKind) -> Ty<'hir> {
        let interned_kind = self.ty_interner.intern(ty_kind);
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
            I: IntoIterator<Item=T>,
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
pub struct CallableConstraint {
    target_ty: Type,
    args: Vec<Type>,
    ret_ty: Type,
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

#[derive(Clone, PartialEq, Debug, Serialize)]
pub struct Path<'hir> {
    path: DefId,
    generics: Generics<'hir>,
}

impl<'hir> Path<'hir> {
    pub fn new(path: DefId, generics: Generics<'hir>) -> Self {
        Self { path, generics }
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

#[derive(Clone, PartialEq, Debug, Serialize)]
pub struct GenericParam<'hir> {
    pub(crate) ty_var: TyVar,
    pub(crate) trait_bound: Option<TraitBound<'hir>>,
}

impl<'hir> GenericParam<'hir> {
    pub fn new(ty_var: TyVar, trait_bound: Option<TraitBound<'hir>>) -> Self {
        Self {
            ty_var,
            trait_bound,
        }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize)]
pub struct TraitBound<'hir> {
    bounds: &'hir [&'hir TyKind<'hir>],
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
