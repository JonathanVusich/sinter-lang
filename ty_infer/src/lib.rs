#![allow(unused)]

use std::collections::HashMap;
use std::f32::NAN;
use std::fmt::{Debug, Display, Formatter};
use std::iter::zip;

use itertools::Itertools;
use serde::{Deserialize, Serialize};

use ast::{ClassDef, FnDef, GlobalVarDef, InfixOp, UnaryOp, ValueDef};
use diagnostics::{Diagnostic, Diagnostics};
use hir::{
    ArrayExpr, Expr, FnStmts, HirCrate, HirMap, HirNodeKind, LocalDef, Primitive, Res, Stmt, Ty,
};
use id::{DefId, LocalDefId};
use macros::named_slice;
use types::LDefMap;

use crate::unification::{TyVar, UnificationTable};

mod trait_solver;
mod unification;

#[derive(Debug)]
pub enum TypeErrKind {
    NotEqual(Type, Type),
    NotAssignable(Type, Type),
    NotArray(Type),
    NotCallable(Type),
    InvalidArgs(Type, Vec<Type>),
    DuplicateTraitImpl,
    UnificationError,
    CyclicType,
}

impl Display for TypeErrKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        // TODO: Implement pretty printing
        Debug::fmt(self, f)
    }
}

#[derive(Serialize, Debug)]
pub struct TypeMap {
    tys: Box<[Option<Type>]>,
}

impl TypeMap {
    fn new(allocated_size: usize) -> Self {
        Self {
            tys: vec![None; allocated_size].into_boxed_slice(),
        }
    }

    fn insert(&mut self, node: &LocalDefId, ty: Type) {
        self.tys[usize::from(*node)] = Some(ty);
    }

    fn get(&self, node: &LocalDefId) -> Option<&Type> {
        self.tys[usize::from(*node)].as_ref()
    }
}

#[derive(Debug)]
struct LocalEnv {
    // locals: HashMap<>
}

#[derive(Debug)]
pub struct CrateInference<'a> {
    diagnostics: &'a mut Diagnostics,
    hir_map: &'a HirMap,
    krate: &'a HirCrate,

    unify_table: UnificationTable,
    ty_map: TypeMap,

    // environments: Vec<LocalEnv>,
    generic_tys: Vec<LDefMap<Type>>,
    ret_tys: Vec<Type>,
}

/// Need to store all types in
///
impl<'a> CrateInference<'a> {
    pub fn new(diagnostics: &'a mut Diagnostics, krate: &'a HirCrate, hir_map: &'a HirMap) -> Self {
        Self {
            diagnostics,
            hir_map,
            krate,
            unify_table: Default::default(),
            ty_map: TypeMap::new(krate.nodes.len()),

            generic_tys: Default::default(),
            ret_tys: Vec::default(),
        }
    }

    // TODO: Returned interned ty representation to reduce memory overhead.
    // TODO: Record existing types for class fields and other nodes whose types are known statically.
    pub fn infer_tys(mut self) -> Option<TypeMap> {
        for item in self.krate.items.iter() {
            let node = self.krate.node(item);
            match node {
                // Since global lets are the only top level items that have a type (other than fns), we infer on them directly.
                HirNodeKind::GlobalLet(_) => {
                    self.infer_node(item);
                }
                HirNodeKind::Class(class_stmt) => {
                    self.infer_fns(&class_stmt.fn_stmts);
                }
                HirNodeKind::Enum(enum_stmt) => {
                    for (name, member) in enum_stmt.members.iter() {
                        let enum_member = self.krate.enum_member(member);
                        self.infer_fns(&enum_member.member_fns);
                    }
                    self.infer_fns(&enum_stmt.member_fns);
                }
                HirNodeKind::Trait(trait_stmt) => {
                    self.infer_fns(&trait_stmt.member_fns);
                }
                HirNodeKind::TraitImpl(trait_impl_stmt) => {
                    self.infer_fns(&trait_impl_stmt.member_fns);
                }
                HirNodeKind::Fn(fn_stmt) => {
                    if let Some(body) = fn_stmt.body {
                        let generic_params = fn_stmt
                            .sig
                            .generic_params
                            .values()
                            .copied()
                            .map(|param| (param, self.existing_ty(param.to_def_id(self.krate.id))))
                            .collect();

                        self.generic_tys.push(generic_params);

                        let ty = fn_stmt
                            .sig
                            .return_type
                            .map(|ret_ty| self.existing_ty(ret_ty.to_def_id(self.krate.id)))
                            .unwrap_or_else(|| Type::None);
                        self.check_node(&body, ty);

                        self.generic_tys.pop();
                    }
                }
                _ => unreachable!(),
            }
        }

        Some(self.ty_map)
    }

    fn infer_fns(&mut self, fn_stmts: &FnStmts) {
        for (name, fn_stmt) in fn_stmts.iter() {
            let fn_stmt = self.krate.fn_stmt(fn_stmt);
            if let Some(body) = fn_stmt.body {
                self.infer_node(&body);
            }
        }
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

    fn infer(&mut self, node: &LocalDefId) -> (Constraints, Type) {
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

                            return (constraints, Type::None);
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

    fn check(&mut self, node_id: &LocalDefId, ty: Type) -> Constraints {
        let node = self.krate.node(node_id);
        let constraints = match (node, &ty) {
            (HirNodeKind::Expr(Expr::Float(_)), Type::F32 | Type::F64) => Constraints::default(),
            (HirNodeKind::Expr(Expr::Int(_)), Type::I64) => Constraints::default(),
            (HirNodeKind::Expr(Expr::String(_)), Type::Str) => Constraints::default(),
            (HirNodeKind::Expr(Expr::True), Type::Boolean) => Constraints::default(),
            (HirNodeKind::Expr(Expr::False), Type::Boolean) => Constraints::default(),
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
            Constraint::Infix(lhs, rhs, op) => self.unify_infix_op(lhs, rhs, op),
            Constraint::Unary(ty, unary_op) => self.unify_unary_op(ty, unary_op),
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
        dbg!(&lhs);
        dbg!(&rhs);

        let lhs = self.normalize_ty(lhs);
        let rhs = self.normalize_ty(rhs);

        dbg!(&lhs);
        dbg!(&rhs);

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

    fn existing_ty(&mut self, definition: DefId) -> Type {
        let krate = self.hir_map.krate(&definition);
        let ty = krate.ty_stmt(&definition.local_id());
        match ty {
            Ty::Array(array) => {
                Type::Array(Array::new(self.existing_ty(array.ty.to_def_id(krate.id))))
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
}

type Constraints = Vec<Constraint>;

#[derive(Debug)]
enum Constraint {
    Equal(Type, Type),
    Assignable(Type, Type),
    Array(Type),
    Infix(Type, Type, InfixOp),
    Unary(Type, UnaryOp),
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

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct Array {
    ty: Box<Type>,
}

impl Array {
    pub fn new(array_ty: Type) -> Self {
        Self {
            ty: Box::new(array_ty),
        }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct Path {
    path: DefId,
    generics: Vec<Type>,
}

impl Path {
    pub fn new(path: DefId, generics: Vec<Type>) -> Self {
        Self { path, generics }
    }
}

named_slice!(Types, Type);

/// Defines a concrete type of a potentially generic Class.
#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct Class {
    definition: DefId,
    fields: Types,
    generics: LDefMap<Type>,
}

pub struct ClassInner {}

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

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct GenericParam {
    pub(crate) ty_var: TyVar,
    pub(crate) trait_bound: Option<TraitBound>,
}

impl GenericParam {
    pub fn new(ty_var: TyVar, trait_bound: Option<TraitBound>) -> Self {
        Self {
            ty_var,
            trait_bound,
        }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct TraitBound {
    bounds: Vec<Type>,
}

impl TraitBound {
    pub fn new(trait_bound: Vec<Type>) -> Self {
        Self {
            bounds: trait_bound,
        }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct FnSig {
    params: Types,
    ret_ty: Box<Type>,
}

impl FnSig {
    pub fn new(params: Types, ret_ty: Type) -> Self {
        Self {
            params,
            ret_ty: Box::new(ret_ty),
        }
    }
}

/// This enum supports recursive types whose inner types are not yet known.
/// Allows full type definitions to be built incrementally from partial information.
#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub enum Type {
    Array(Array),
    Class(Class),
    Fn(FnSig),
    GenericParam(GenericParam),
    TraitBound(TraitBound),
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

impl Type {
    /// Whether or not the right hand type is a valid type that can be assigned to this type.
    fn assignable(&self, rhs: &Type) -> bool {
        match self {
            Type::Array(lhs) => {
                if let Type::Array(rhs) = rhs {
                    return lhs.ty.assignable(&rhs.ty);
                }
                false
            }
            Type::Class(lhs) => {
                if let Type::Class(rhs) = rhs {
                    return lhs.definition == rhs.definition
                        && zip(lhs.generics.values(), rhs.generics.values())
                            .all(|(lhs, rhs)| lhs.assignable(rhs));
                }
                false
            }
            Type::TraitBound(trait_bound) => {
                return trait_bound.bounds.iter().all(|bound| bound.assignable(rhs));
            }
            Type::GenericParam(param) => {
                if let Some(trait_bound) = &param.trait_bound {
                    return trait_bound.bounds.iter().all(|bound| bound.assignable(rhs));
                }
                true
            }
            Type::Fn(lhs_sig) => match rhs {
                Type::Fn(rhs_sig) => lhs_sig == rhs_sig,
                _ => false,
            },
            Type::U8 => rhs.uint_width() <= 1,
            Type::U16 => rhs.uint_width() <= 2,
            Type::U32 => rhs.uint_width() <= 3,
            Type::U64 => rhs.uint_width() <= 4,
            Type::I8 => rhs.int_width() <= 1,
            Type::I16 => rhs.int_width() <= 2,
            Type::I32 => rhs.int_width() <= 3,
            Type::I64 => rhs.int_width() <= 4,
            Type::F32 => rhs.floating_width() <= 1,
            Type::F64 => rhs.floating_width() <= 2,
            Type::Str => rhs == &Type::Str,
            Type::Boolean => rhs == &Type::Boolean,
            Type::None => rhs == &Type::None,
            Type::Infer(_) => false,
        }
    }
    fn uint_width(&self) -> usize {
        match self {
            Type::U8 => 1,
            Type::U16 => 2,
            Type::U32 => 3,
            Type::U64 => 4,
            _ => 0,
        }
    }

    fn int_width(&self) -> usize {
        match self {
            Type::I8 => 1,
            Type::I16 => 2,
            Type::I32 => 3,
            Type::I64 => 4,
            _ => usize::MAX,
        }
    }

    fn floating_width(&self) -> usize {
        match self {
            Type::F32 => 1,
            Type::F64 => 2,
            _ => usize::MAX,
        }
    }
}
