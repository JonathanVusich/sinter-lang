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
use hir::{ArrayExpr, Block, FnStmts, Generics, HirCrate, HirMap, HirNode, HirNodeKind, LocalDef, Primitive, Res, Stmt};
use id::{DefId, LocalDefId};
use macros::named_slice;
use typed_hir::{ClassStmt, EnumStmt, Expr, FnStmt, GlobalLetStmt, LocalVar, Node, NodeKind, TraitImplStmt, Ty, TypedCrate};
use types::{DefMap, LDefMap};

use crate::unification::{TyVar, UnificationTable};

mod trait_solver;
mod unification;

/// Contains the data needed to infer types for a given HIR crate.
/// The HIR nodes are transformed into their typed HIR representation
/// which ensures that all nodes have correct type information.
/// 
/// TODO: All types SHOULD also be interned across crates using a ty interner.
#[derive(Debug)]
pub struct CrateInference<'hir> {
    diagnostics: &'hir mut Diagnostics,
    hir_map: &'hir HirMap<'hir>,
    krate: &'hir HirCrate<'hir>,
    allocator: &'hir mut Bump,

    unify_table: UnificationTable<'hir>,

    generic_tys: Vec<LDefMap<Ty<'hir>>>,
    ret_tys: Vec<Ty<'hir>>,
}

impl<'hir> CrateInference<'hir> {
    pub fn new(
        diagnostics: &'hir mut Diagnostics,
        arena: &'hir mut Bump,
        krate: &'hir HirCrate,
        hir_map: &'hir HirMap,
    ) -> Self {
        Self {
            diagnostics,
            allocator: arena,
            hir_map,
            krate,
            unify_table: Default::default(),
            generic_tys: Default::default(),
            ret_tys: Vec::default(),
        }
    }

    // TODO: Returned interned ty representation to reduce memory overhead.
    // TODO: Record existing types for class fields and other nodes whose types are known statically.
    pub fn type_crate(mut self) -> Option<TypedCrate<'hir>> {
        let mut items = Vec::with_capacity(self.krate.items.len());
        let mut nodes = LDefMap::with_capacity(self.krate.nodes.len());
        
        for item in self.krate.items.iter() {
            // Unwrap is safe here since all items should have corresponding nodes.
            let node = self.krate.nodes.get(item).unwrap();
            let typed_kind = match node.kind {
                // Since global lets are the only top level items that have a type (other than fns), we infer on them directly.
                HirNodeKind::GlobalLet(global_let_stmt) => self.infer_global_let_stmt(global_let_stmt),
                HirNodeKind::Class(class_stmt) => self.infer_class_stmt(class_stmt),
                HirNodeKind::Enum(enum_stmt) => self.infer_enum_stmt(enum_stmt),
                HirNodeKind::Trait(trait_stmt) => self.infer_trait_stmt(trait_stmt),
                HirNodeKind::TraitImpl(trait_impl_stmt) => self.infer_trait_impl_stmt(trait_impl_stmt),
                HirNodeKind::Fn(fn_stmt) => self.infer_fn_stmt(fn_stmt),
                _ => unreachable!(),
            };
            
            let typed_node = Node::new(typed_kind, node.span, node.id);
            nodes.insert(node.id, typed_node);
            items.push(node.id);
        }

        Some(TypedCrate::new(
            self.krate.name,
            self.krate.id,
            items,
            nodes,
        ))
    }

    fn infer_fns(&mut self, fn_stmts: &FnStmts) {
        for fn_stmt in fn_stmts.iter() {
            if let Some(body) = fn_stmt.body {
                self.infer_node(body);
            }
        }
    }
    
    fn infer_block(&mut self, block: &Block) {
        for stmt in block.stmts {
            self.infer_stmt(stmt);
        }
    }
    
    fn infer_stmt(&mut self, stmt: &Stmt) {
        todo!()
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
    
    fn infer_global_let_stmt(&mut self, node: &hir::GlobalLetStmt) -> NodeKind<'hir> {
        let local_var = self.existing_var(&node.local_var);
        let ty = self.existing_ty(node.ty);
        let expr = self.infer_expr(node.initializer);
        let global_let_stmt = GlobalLetStmt::new(local_var, ty, expr);
        
        NodeKind::GlobalLet(self.alloc(global_let_stmt))
    }
    
    fn infer_class_stmt(&mut self, node: &hir::ClassStmt) -> NodeKind<'hir> {
        todo!()
    }
    
    fn infer_enum_stmt(&mut self, node: &hir::EnumStmt) -> NodeKind<'hir> {
        todo!()
    }
    
    fn infer_trait_stmt(&mut self, node: &hir::TraitStmt) -> NodeKind<'hir> {
        todo!()
    }
    
    fn infer_trait_impl_stmt(&mut self, node: &hir::TraitImplStmt) -> NodeKind<'hir> {
        todo!()
    }
    
    fn infer_fn_stmt(&mut self, node: &hir::FnStmt) -> NodeKind<'hir> {
        if let Some(body) = node.body {
            let generic_params = node 
                .sig
                .generic_params
                .values()
                .copied()
                .map(|param| (param, self.existing_ty(param.to_def_id(self.krate.id))))
                .collect();

            self.generic_tys.push(generic_params);

            let ty = node
                .sig
                .return_type
                .map(|ret_ty| self.existing_ty(ret_ty.to_def_id(self.krate.id)))
                .unwrap_or_else(|| Type::None);
            self.check_node(&body, ty);

            self.generic_tys.pop();
        }
    }
    
    fn infer_expr(&mut self, node: &hir::Expr) -> &'hir Expr<'hir> {
        todo!()
    }

    fn infer(&mut self, node: &LocalDefId) -> (Constraints, &'hir Ty<'hir>) {
        let (constraints, ty) = match self.krate.node(node) {
            HirNodeKind::GlobalLet(global_let) => {
                let ty = self.existing_ty(global_let.ty.to_def_id(self.krate.id));
                self.ty_map.insert(&global_let.ty, ty.clone());
                self.ty_map.insert(&global_let.local_var, ty.clone());

                let constraints = self.check(&global_let.initializer, ty);
                (constraints, Type::None)
            }
            HirNodeKind::Block(block) => {
                // Create a new scope to track constraints on the return type.
                let ret_ty = self.fresh_ty(node);
                self.ret_tys.push(ret_ty.clone());
                let mut constraints = Constraints::default();
                for stmt in &block.stmts {
                    let (c, _) = self.infer(stmt);
                    constraints.extend(c);
                }
                self.ret_tys.pop();
                (constraints, ret_ty)
            }
            HirNodeKind::Stmt(stmt) => {
                match stmt {
                    Stmt::Let(let_stmt) => {
                        // Create fresh type var for the var type. This will be either inferred or
                        // constrained to the assigned type.
                        let mut constraints = Constraints::default();

                        let ty_var;
                        if let Some(ty) = let_stmt.ty {
                            let var_ty = self.existing_ty(ty.to_def_id(self.krate.id));
                            ty_var = var_ty.clone();

                            // TODO: Handle late assignment correctly.
                            let initializer = let_stmt.initializer.unwrap();
                            let initializer_ty =
                                self.existing_ty(initializer.to_def_id(self.krate.id));
                            constraints.push(Constraint::Assignable(var_ty, initializer_ty));

                            return (constraints, Ty::none());
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
            HirNodeKind::Expr(expr) => match expr {
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
                                    let ty = self.existing_ty(global_let.ty.to_def_id(krate.id));
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
                                        .map(|param| self.existing_ty(param.ty.to_def_id(krate.id)))
                                        .collect_vec()
                                        .into();
                                    let ret_ty = fn_stmt
                                        .sig
                                        .return_type
                                        .map(|ret_ty| self.existing_ty(ret_ty.to_def_id(krate.id)))
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
                                                self.existing_ty(generic_param.to_def_id(krate.id)),
                                            )
                                        })
                                        .collect();

                                    self.generic_tys.push(generic_params);
                                    let params = class_stmt
                                        .fields
                                        .values()
                                        .map(|field| krate.field(field))
                                        .map(|field| field.ty)
                                        .map(|field| self.existing_ty(field.to_def_id(krate.id)))
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
            (HirNodeKind::Expr(Expr::Float(_)), TyKind::F32 | TyKind::F64) => {
                Constraints::default()
            }
            (HirNodeKind::Expr(Expr::Int(_)), TyKind::I64) => Constraints::default(),
            (HirNodeKind::Expr(Expr::String(_)), TyKind::Str) => Constraints::default(),
            (HirNodeKind::Expr(Expr::True), TyKind::Boolean) => Constraints::default(),
            (HirNodeKind::Expr(Expr::False), TyKind::Boolean) => Constraints::default(),
            (node, ty) => {
                let (mut constraints, inferred_ty) = self.infer(node_id);
                constraints.push(Constraint::Assignable(ty.clone(), inferred_ty));
                constraints
            }
        };
        self.ty_map.insert(node_id, ty);
        constraints
    }

    fn unify_constraints(&mut self, constraints: Constraints) -> bool {
        constraints
            .into_iter()
            .all(|constraint| self.unify(constraint))
    }

    fn unify(&mut self, constraint: Constraint) -> bool {
        match constraint {
            Constraint::Equal(lhs, rhs) => self.unify_ty_ty(lhs, rhs, Type::eq),
            Constraint::Assignable(lhs, rhs) => self.unify_ty_ty(lhs, rhs, Type::assignable),
            Constraint::Array(ty) => self.unify_ty_array(ty),
            Constraint::Callable(callable_constraint) => {
                let CallableConstraint {
                    target_ty,
                    args,
                    ret_ty,
                } = callable_constraint;
                let ty = self.normalize_ty(target_ty);
                match &ty {
                    Type::GenericParam(_) => {
                        todo!()
                    }
                    Type::Class(class) => self.unify_fn_sig(&class.fn_sig(), &args, ret_ty),
                    Type::Fn(fn_sig) => self.unify_fn_sig(fn_sig, &args, ret_ty),
                    Type::Array(_)
                    | Type::TraitBound(_)
                    | Type::U8
                    | Type::U16
                    | Type::U32
                    | Type::U64
                    | Type::I8
                    | Type::I16
                    | Type::I32
                    | Type::I64
                    | Type::F32
                    | Type::F64
                    | Type::Str
                    | Type::Boolean
                    | Type::None
                    | Type::Infer(_) => {
                        self.diagnostics.push(Diagnostic::BlankError);
                        return false;
                    }
                }
            }
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
            HirNodeKind::GlobalLet(let_stmt) => {
                // Don't need to check the associated ty
                self.substitute(&let_stmt.initializer);
            }
            HirNodeKind::Fn(fn_stmt) => {
                if let Some(body) = &fn_stmt.body {
                    self.substitute(body);
                }
            }
            HirNodeKind::EnumMember(enum_member) => {
                for member_fn in enum_member.member_fns.values() {
                    self.substitute(member_fn);
                }
            }
            HirNodeKind::Expr(expr) => {
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
            HirNodeKind::Ty(_) => {}
            HirNodeKind::DestructureExpr(_) => {}
            HirNodeKind::Stmt(stmt) => match stmt {
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
            HirNodeKind::Block(block) => {
                block
                    .stmts
                    .into_iter()
                    .for_each(|stmt| self.substitute(stmt));
            }
            HirNodeKind::Param(_) => {}
            HirNodeKind::Field(_) => {}
            HirNodeKind::LocalVar(_) => {}
            HirNodeKind::Pattern(_) => {}
            HirNodeKind::MatchArm(_) => {}
            // These can safely be ignored since they should not be traversed.
            HirNodeKind::Class(_) => {}
            HirNodeKind::Enum(_) => {}
            HirNodeKind::Trait(_) => {}
            HirNodeKind::TraitImpl(_) => {}
        }

        let ty = self.ty_map.get(node_id).unwrap().clone();
        let ty = self.probe_ty(ty);
        self.ty_map.insert(node_id, ty);
    }

    fn probe_ty(&mut self, ty: Type) -> Type {
        match ty {
            Type::Array(array) => Type::Array(Array::new(self.probe_ty(*array.ty))),
            Type::TraitBound(trait_bound) => {
                let traits = trait_bound
                    .bounds
                    .into_iter()
                    .map(|ty| self.probe_ty(ty))
                    .collect();
                Type::TraitBound(TraitBound::new(traits))
            }
            Type::Fn(closure) => {
                let params = closure
                    .params
                    .into_iter()
                    .map(|ty| self.probe_ty(ty.clone()))
                    .collect_vec()
                    .into();
                let ret_ty = self.probe_ty(*closure.ret_ty);
                Type::Fn(FnSig::new(params, ret_ty))
            }
            Type::Class(class) => Type::Class(class),
            Type::Infer(ty_var) => self.unify_table.probe(ty_var).unwrap(),
            Type::GenericParam(generic_param) => {
                let p_ty = self.unify_table.probe(generic_param.ty_var);
                p_ty.unwrap()
            }
            Type::U8 => Type::U8,
            Type::U16 => Type::U16,
            Type::U32 => Type::U32,
            Type::U64 => Type::U64,
            Type::I8 => Type::I8,
            Type::I16 => Type::I16,
            Type::I32 => Type::I32,
            Type::I64 => Type::I64,
            Type::F32 => Type::F32,
            Type::F64 => Type::F64,
            Type::Str => Type::Str,
            Type::Boolean => Type::Boolean,
            Type::None => Type::None,
        }
    }

    fn fresh_ty(&mut self, node: &LocalDefId) -> Type {
        let ty = Type::Infer(self.unify_table.fresh_ty());
        self.ty_map.insert(node, ty.clone());
        ty
    }
    
    fn existing_var(&mut self, existing_var: &hir::LocalVar) -> LocalVar {
        LocalVar::new(existing_var.ident, existing_var.id)
    }

    fn existing_ty(&mut self, existing_ty: &hir::Ty) -> &'hir Ty<'hir> {
        match existing_ty {
            hir::Ty::Array(ty) => {
                Ty::new()
                let ty = self.existing_ty(ty);
                
            }
            Ty::GenericParam(generic_param) => {
                let trait_bound = generic_param.trait_bound.map(|bound| {
                    match self.existing_ty(bound.to_def_id(krate.id)) {
                        Type::TraitBound(trait_bound) => trait_bound,
                        _ => unreachable!(),
                    }
                });
                Type::GenericParam(GenericParam::new(self.unify_table.fresh_ty(), trait_bound))
            }
            Ty::TraitBound(trait_bound) => {
                let path_tys = trait_bound
                    .into_iter()
                    .map(|path_ty| self.existing_ty(path_ty.definition))
                    .collect();
                Type::TraitBound(TraitBound::new(path_tys))
            }
            Ty::Path(path) => {
                todo!()
            }
            Ty::Closure(closure) => {
                let params = closure
                    .params
                    .into_iter()
                    .map(|ty| self.existing_ty(ty.to_def_id(krate.id)))
                    .collect_vec()
                    .into();
                Type::Fn(FnSig::new(
                    params,
                    self.existing_ty(closure.ret_ty.to_def_id(krate.id)),
                ))
            }
            Ty::Primitive(primitive) => match primitive {
                Primitive::U8 => Type::U8,
                Primitive::U16 => Type::U16,
                Primitive::U32 => Type::U32,
                Primitive::U64 => Type::U64,
                Primitive::I8 => Type::I8,
                Primitive::I16 => Type::I16,
                Primitive::I32 => Type::I32,
                Primitive::I64 => Type::I64,
                Primitive::F32 => Type::F32,
                Primitive::F64 => Type::F64,
                Primitive::Str => Type::Str,
                Primitive::Boolean => Type::Boolean,
                Primitive::None => Type::None,
            },
        }
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

type Constraints<'hir> = Vec<Constraint<'hir>>;

#[derive(Debug)]
enum Constraint<'hir> {
    Equal(&'hir Ty<'hir>, &'hir Ty<'hir>),
    Assignable(&'hir Ty<'hir>, &'hir Ty<'hir>),
    Array(&'hir Ty<'hir>),
    Callable(CallableConstraint), // Arg types
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
    ty: &'hir Ty<'hir>,
}

impl<'hir> Array<'hir> {
    pub fn new(ty: &'hir Ty<'hir>) -> Self {
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
    fields: Vec<&'hir Ty<'hir>>,
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
    bounds: &'hir [&'hir Ty<'hir>],
}

impl<'hir> TraitBound<'hir> {
    pub fn new(bounds: &'hir [&'hir Ty<'hir>]) -> Self {
        Self { bounds }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize)]
pub struct FnSig<'hir> {
    params: &'hir [&'hir Ty<'hir>],
    ret_ty: &'hir Ty<'hir>,
}

impl<'hir> FnSig<'hir> {
    pub fn new(params: &'hir [&'hir Ty<'hir>], ret_ty: &'hir Ty<'hir>) -> Self {
        Self { params, ret_ty }
    }
}

/// This enum supports recursive types whose inner types are not yet known.
/// Allows full type definitions to be built incrementally from partial information.
#[derive(Clone, PartialEq, Debug, Serialize)]
pub enum TyKind<'hir> {
    Array(Array<'hir>),
    Class(Class<'hir>),
    Fn(FnSig<'hir>),
    GenericParam(GenericParam<'hir>),
    TraitBound(TraitBound<'hir>),
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
    F32,
    F64,
    Str,
    Boolean,
    None,
    Infer(TyVar),
}
