use indexmap::Equivalent;
use std::backtrace::Backtrace;
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::iter::zip;
use std::ops::Deref;
use std::sync::Arc;

use itertools::{zip_eq, Itertools, Position};
use phf::Set;
use serde::{Deserialize, Serialize};

use crate::compiler::ast::{InfixOp, Mutability, TraitImplStmt, UnaryOp};
use crate::compiler::compiler::{CompileError, TypeError};
use crate::compiler::hir::{
    ArrayExpr, DefId, Expr, Expression, Field, FnStmts, Generics, HirCrate, HirMap, HirNodeKind,
    LocalDefId, Param, PathTy, Primitive, Res, ReturnStmt, Stmt, TraitBound as HirTraitBound, Ty,
};
use crate::compiler::resolver::{ClassDef, FnDef, GlobalVarDef, ValueDef};
use crate::compiler::types::{DefMap, LDefMap, StrMap};
use crate::traits::traits::Trait;

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

pub struct TypeInference<'a> {
    hir_map: &'a HirMap,
}

/// High level steps for ty inference.
/// 2. Do fns need to have their return types canonicalized?
impl<'a> TypeInference<'a> {
    pub fn new(hir_map: &'a HirMap) -> Self {
        Self { hir_map }
    }

    pub fn infer_tys(self) -> Result<(), CompileError> {
        for krate in self.hir_map.krates() {
            let crate_inference = CrateInference::new(krate, &self.hir_map);
            crate_inference.infer_tys()?;
        }
        Ok(())
    }
}

pub struct TraitSolver {
    impls: DefMap<TraitMap>,
}

#[derive(Default)]
struct TraitMap {
    traits: DefMap<Vec<Generics>>,
}

impl TraitMap {
    fn add_impl(&mut self, trait_impl: &PathTy) -> Result<(), TypeErrKind> {
        let generic_impls = self
            .traits
            .entry(trait_impl.definition)
            .or_insert_with(Vec::new);
        if generic_impls.contains(&trait_impl.generics) {
            Err(TypeErrKind::DuplicateTraitImpl)
        } else {
            generic_impls.push(trait_impl.generics.clone());
            Ok(())
        }
    }
}

impl TraitSolver {
    pub fn new(crates: &Vec<HirCrate>) -> Self {
        let mut impls = DefMap::default();
        // Generate trait map
        for krate in crates {
            for item in krate.items.iter().copied() {
                if let HirNodeKind::TraitImpl(trait_impl) = krate.node(item) {
                    let trait_map = impls
                        .entry(trait_impl.target_ty)
                        .or_insert_with(TraitMap::default);
                    trait_map.add_impl(&trait_impl.trait_to_impl);
                }
            }
        }
        Self { impls }
    }
}

#[derive(Debug)]
pub struct CrateInference<'a> {
    unify_table: UnificationTable,
    ty_map: LDefMap<Type>,
    errors: Vec<TypeErrKind>,
    krate: &'a HirCrate,
    hir_map: &'a HirMap,
    scopes: Vec<Type>,
}

impl<'a> CrateInference<'a> {
    pub fn new(krate: &'a HirCrate, crate_lookup: &'a HirMap) -> Self {
        Self {
            unify_table: Default::default(),
            ty_map: Default::default(),
            errors: Default::default(),
            krate,
            hir_map: crate_lookup,
            scopes: Vec::default(),
        }
    }

    pub fn infer_tys(mut self) -> Result<LDefMap<Type>, CompileError> {
        for item in self.krate.items.iter().copied() {
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
                        let ty = fn_stmt
                            .sig
                            .return_type
                            .map(|ret_ty| Type::from(self.krate, ret_ty))
                            .unwrap_or_else(|| Type::None);
                        self.check_node(body, ty);
                    }
                }
                _ => unreachable!(),
            }
        }

        if !self.errors.is_empty() {
            return Err(CompileError::TypeErrors(
                self.errors.into_iter().map(TypeError::from).collect(),
            ));
        }
        Ok(self.ty_map)
    }

    fn infer_fns(&mut self, fn_stmts: &FnStmts) {
        for (name, fn_stmt) in fn_stmts.iter() {
            let fn_stmt = self.krate.fn_stmt(fn_stmt);
            if let Some(body) = fn_stmt.body {
                self.infer_node(body);
            }
        }
    }

    fn check_node(&mut self, node: LocalDefId, ty: Type) {
        let constraints = self.check(node, ty.clone());
        if let Err(error) = self.unify(constraints) {
            self.errors.push(error)
        } else {
            self.substitute(node, ty);
        }
    }

    fn infer_node(&mut self, node: LocalDefId) {
        let (constraints, ty) = self.infer(node);
        if let Err(error) = self.unify(constraints) {
            self.errors.push(error)
        } else {
            self.substitute(node, ty);
        }
    }

    fn infer(&mut self, node: LocalDefId) -> (Constraints, Type) {
        let node = self.krate.node(node);
        match node {
            HirNodeKind::GlobalLet(global_let) => {
                let ty = Type::from(self.krate, global_let.ty);
                let constraints = self.check(global_let.initializer, ty.clone());
                (constraints, ty)
            }
            HirNodeKind::Block(block) => {
                // Create a new scope to track constraints on the return type.
                let ret_ty = self.fresh_ty();
                self.scopes.push(ret_ty.clone());
                let mut constraints = Constraints::default();
                for stmt in block.stmts.iter() {
                    let (c, _) = self.infer(*stmt);
                    constraints.extend(c);
                }
                self.scopes.pop();
                (constraints, ret_ty)
            }
            HirNodeKind::Stmt(stmt) => {
                match stmt {
                    // TODO: Infer other stmts
                    Stmt::Let(let_stmt) => {
                        if let Some(ty) = let_stmt.ty {
                            if let Some(initializer) = let_stmt.initializer {
                                let ret_ty = Type::from(self.krate, ty);
                                return (self.check(initializer, ret_ty), Type::None);
                            }
                        }
                        // Create fresh type var for the var type that needs to be inferred
                        let ty_var = self.fresh_ty();
                        let (mut c, ty) = self.infer(let_stmt.initializer.unwrap()); // Unwrap should be safe because it is invalid to have no type and no stmt.
                        c.push(Constraint::Assignable(ty_var, ty));
                        (c, Type::None)
                    }
                    Stmt::Return(return_stmt) => {
                        let ret_ty = self.scopes.last().unwrap().clone();
                        if let Some(node) = return_stmt.value {
                            let (mut constraints, ty) = self.infer(node);
                            constraints.push(Constraint::Assignable(ret_ty, ty));
                            return (constraints, Type::None);
                        }
                        (vec![Constraint::Equal(ret_ty, Type::None)], Type::None)
                    }

                    Stmt::Expression(expression) => {
                        let (mut constraints, ty) = self.infer(expression.expr);
                        if expression.implicit_return {
                            let ret_ty = self.scopes.last().unwrap();
                            constraints.push(Constraint::Assignable(ret_ty.clone(), ty));
                            return (constraints, Type::None);
                        }
                        (constraints, ty)
                    }
                    _ => (Constraints::default(), self.fresh_ty()),
                }
            }
            HirNodeKind::Expr(expr) => match expr {
                Expr::Array(array) => match array {
                    ArrayExpr::Sized { initializer, size } => {
                        let size_c = self.check(*size, Type::U64); // Should always be zero?
                        let (mut constraints, init_ty) = self.infer(*initializer);
                        constraints.extend(size_c);
                        (constraints, Type::Array(Array::new(init_ty)))
                    }
                    ArrayExpr::Unsized { initializers } => {
                        let inferred_ty = self.fresh_ty();
                        let mut constraints = Vec::default();
                        for initializer in initializers.iter() {
                            let (c, ty) = self.infer(*initializer);
                            constraints.extend(c);
                            constraints.push(Constraint::Assignable(ty, inferred_ty.clone()));
                        }
                        (constraints, Type::Array(Array::new(inferred_ty)))
                    }
                },
                Expr::Infix(infix) => {
                    let (lhs_c, lhs_ty) = self.infer(infix.lhs);
                    let (rhs_c, rhs_ty) = self.infer(infix.rhs);
                    let mut new_constr = Vec::with_capacity(lhs_c.len() + rhs_c.len() + 1);
                    new_constr.extend(lhs_c);
                    new_constr.extend(rhs_c);
                    new_constr.push(Constraint::Infix(lhs_ty.clone(), rhs_ty, infix.operator));
                    (new_constr, lhs_ty)
                }
                Expr::Unary(unary) => {
                    let (mut constraints, ty) = self.infer(unary.expr);
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
                    let params = vec![self.fresh_ty(); closure.params.len()];
                    let (constraints, ty) = self.infer(closure.stmt);
                    (constraints, Type::Fn(FnSig::new(params, ty)))
                }
                Expr::Assign(assign) => {
                    let (mut constraints, lhs_ty) = self.infer(assign.lhs);
                    let (rhs_constr, rhs_ty) = self.infer(assign.rhs);

                    constraints.extend(rhs_constr);
                    constraints.push(Constraint::Assignable(lhs_ty.clone(), rhs_ty));

                    (constraints, lhs_ty)
                }
                Expr::Call(call) => {
                    let mut arg_tys = Vec::default();

                    let (mut constraints, target_ty) = self.infer(call.target);

                    // Infer the args to the fn call and save their constraints
                    for arg in call.args.iter() {
                        let (c, ty) = self.infer(*arg);
                        constraints.extend(c);
                        arg_tys.push(ty);
                    }

                    let ret_ty = self.fresh_ty();

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
                                    (Constraints::default(), Type::from(krate, global_let.ty))
                                }
                                ValueDef::Fn(FnDef { id }) => {
                                    let krate = self.hir_map.krate(id);
                                    let fn_stmt = krate.fn_stmt(&id.local_id());

                                    let param_tys = fn_stmt
                                        .sig
                                        .params
                                        .values()
                                        .map(|param| krate.param(param))
                                        .map(|param| Type::from(krate, param.ty))
                                        .collect();
                                    let ret_ty = fn_stmt
                                        .sig
                                        .return_type
                                        .map(|ret_ty| Type::from(krate, ret_ty))
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

                                    let param_tys = class_stmt
                                        .fields
                                        .values()
                                        .map(|field| krate.field(field))
                                        .map(|field| Type::from(krate, field.ty))
                                        .collect();

                                    let generic_params = class_stmt
                                        .generic_params
                                        .values()
                                        .map(|generic_param| {
                                            Type::Path(Path::new(
                                                generic_param.to_def_id(self.krate.id),
                                                Vec::default(),
                                            ))
                                        })
                                        .collect();

                                    let ret_ty = Type::Path(Path::new(*id, generic_params));

                                    let ty = Type::Fn(FnSig::new(param_tys, ret_ty));
                                    (Constraints::default(), ty)
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
                        Res::Local(_) => {
                            todo!()
                        }
                        Res::Primitive(_) => {
                            todo!()
                        }
                    }
                }
                _ => {
                    dbg!(&expr);
                    (Constraints::default(), self.fresh_ty())
                }
            },
            _ => (Constraints::default(), self.fresh_ty()),
        }
    }

    fn check(&mut self, node_id: LocalDefId, ty: Type) -> Constraints {
        enum Op<'a> {
            Check(LocalDefId, Type),
            CheckMultiple(&'a [LocalDefId], Type),
            None,
            Infer(LocalDefId, Type),
        }

        let node = self.krate.node(node_id);
        let op = match (node, &ty) {
            (
                HirNodeKind::Expr(Expr::Array(ArrayExpr::Unsized { initializers })),
                Type::Array(array),
            ) => Op::CheckMultiple(initializers, *array.ty.clone()),
            (
                HirNodeKind::Expr(Expr::Array(ArrayExpr::Sized { initializer, size })),
                Type::Array(array),
            ) => Op::Check(*initializer, *array.ty.clone()),
            (HirNodeKind::Expr(Expr::Float(_)), Type::F32 | Type::F64) => Op::None,
            (HirNodeKind::Expr(Expr::Int(_)), Type::I64) => Op::None,
            (HirNodeKind::Expr(Expr::String(_)), Type::Str) => Op::None,
            (HirNodeKind::Expr(Expr::True), Type::Boolean) => Op::None,
            (HirNodeKind::Expr(Expr::False), Type::Boolean) => Op::None,
            // TODO: Cover other explicit cases
            (node, ty) => Op::Infer(node_id, ty.clone()), // Why do we have to clone here?
        };

        match op {
            Op::Check(node, ty) => self.check(node, ty),
            Op::CheckMultiple(nodes, ty) => nodes
                .iter()
                .copied()
                .flat_map(|expr| self.check(expr, ty.clone()))
                .collect_vec(),
            Op::None => Constraints::default(),
            Op::Infer(node, ty) => {
                let (mut constraints, inferred_ty) = self.infer(node);
                constraints.push(Constraint::Assignable(ty, inferred_ty));
                constraints
            }
        }
    }

    fn unify(&mut self, constraints: Constraints) -> Result<(), TypeErrKind> {
        for constr in constraints {
            match constr {
                Constraint::Equal(lhs, rhs) => self.unify_ty_ty(lhs, rhs)?,
                Constraint::Assignable(lhs, rhs) => self.unify_ty_ty(lhs, rhs)?,
                Constraint::Array(ty) => self.unify_ty_array(ty)?,
                Constraint::Infix(lhs, rhs, op) => self.unify_infix_op(lhs, rhs, op)?,
                Constraint::Unary(ty, unary_op) => self.unify_unary_op(ty, unary_op)?,
                Constraint::Callable(callable_constraint) => {
                    let CallableConstraint {
                        target_ty,
                        args,
                        ret_ty,
                    } = callable_constraint;
                    let ty = self.normalize_ty(target_ty);
                    if let Type::Fn(fn_sig) = &ty {
                        if args.len() != fn_sig.params.len() {
                            return Err(TypeErrKind::InvalidArgs(ty, args));
                        }
                        for x in 0..args.len() {
                            let arg = self.normalize_ty(args[x].clone());
                            let param = self.normalize_ty(fn_sig.params[x].clone());

                            if !self.is_assignable(&param, &arg) {
                                // TODO: Improve error messages for invalid fn args
                                return Err(TypeErrKind::InvalidArgs(ty, args));
                            }
                        }

                        let ret_ty = self.normalize_ty(ret_ty);
                        let fn_ret_ty = self.normalize_ty(*fn_sig.ret_ty.clone());

                        if ret_ty != fn_ret_ty {
                            return Err(TypeErrKind::NotEqual(ret_ty, fn_ret_ty));
                        }
                    }
                    return Err(TypeErrKind::NotCallable(ty));
                }
            }
        }
        Ok(())
    }

    fn is_assignable(&self, lhs: &Type, rhs: &Type) -> bool {
        match lhs {
            Type::Array(lhs) => {
                if let Type::Array(rhs) = rhs {
                    return self.is_assignable(&lhs.ty, &rhs.ty);
                }
                false
            }
            Type::Path(path) => {
                let node = self.hir_map.node(&path.path);
                match node {
                    // TODO: Implement param + arg checks
                    HirNodeKind::Ty(_) => {
                        todo!()
                    }
                    HirNodeKind::Param(_) => {
                        todo!()
                    }
                    HirNodeKind::GenericParam(generic_param) => {
                        todo!()
                    }
                    _ => unreachable!(),
                }
            }
            Type::TraitBound(_) => {
                todo!()
            }
            Type::Fn(_) => {
                todo!()
            }
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

    fn unify_unary_op(&mut self, ty: Type, unary_op: UnaryOp) -> Result<(), TypeErrKind> {
        let ty = self.normalize_ty(ty);

        // TODO: Add unary op checking
        Ok(())
    }

    fn unify_infix_op(&mut self, lhs: Type, rhs: Type, op: InfixOp) -> Result<(), TypeErrKind> {
        self.unify_ty_ty(lhs, rhs)?;

        // TODO: Add infix op checking
        Ok(())
    }

    fn unify_ty_array(&mut self, ty: Type) -> Result<(), TypeErrKind> {
        let ty = self.normalize_ty(ty);

        match ty {
            Type::Array(_) => Ok(()),
            _ => Err(TypeErrKind::NotArray(ty)),
        }
    }

    fn unify_ty_ty(&mut self, lhs: Type, rhs: Type) -> Result<(), TypeErrKind> {
        let lhs = self.normalize_ty(lhs);
        let rhs = self.normalize_ty(rhs);

        // Check for type equality
        match (lhs, rhs) {
            // If any type is unknown, we have to unify it against the other types.
            (Type::Infer(lhs), Type::Infer(rhs)) => self.unify_table.unify_var_var(lhs, rhs),
            (Type::Infer(unknown), ty) | (ty, Type::Infer(unknown)) => {
                // TODO:  Check for cyclic type
                self.unify_table.unify_var_ty(unknown, ty)
            }
            // If both types are known, then we need to check for type compatibility.
            (lhs, rhs) => {
                if lhs != rhs {
                    return Err(TypeErrKind::NotEqual(lhs, rhs));
                }
                Ok(())
            }
        }
    }

    fn unify_ty_ty_assignable(&mut self, lhs: Type, rhs: Type) -> Result<(), TypeErrKind> {
        let lhs = self.normalize_ty(lhs);
        let rhs = self.normalize_ty(rhs);

        // Check for type equality
        match (lhs, rhs) {
            // If any type is unknown, we have to unify it against the other types.
            (Type::Infer(lhs), Type::Infer(rhs)) => self.unify_table.unify_var_var(lhs, rhs),
            (Type::Infer(unknown), ty) | (ty, Type::Infer(unknown)) => {
                // TODO:  Check for cyclic type
                self.unify_table.unify_var_ty(unknown, ty)
            }
            // If both types are known, then we need to check for type compatibility.
            (lhs, rhs) => {
                if !self.is_assignable(&lhs, &rhs) {
                    return Err(TypeErrKind::NotEqual(lhs, rhs));
                }
                Ok(())
            }
        }
    }

    fn normalize_ty(&mut self, ty: Type) -> Type {
        match ty {
            Type::Array(array) => Type::Array(Array::new(self.normalize_ty(*array.ty))),
            Type::Fn(closure) => {
                let normalized_tys = closure
                    .params
                    .into_iter()
                    .map(|ty| self.normalize_ty(ty))
                    .collect();
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

    fn substitute(&mut self, node_id: LocalDefId, ty: Type) {
        let ty = self.probe_ty(ty);
        self.ty_map.insert(node_id, ty);
    }

    fn probe_ty(&mut self, ty: Type) -> Type {
        match ty {
            Type::Array(array) => Type::Array(Array::new(self.probe_ty(*array.ty))),
            Type::Path(path) => {
                let generics = path
                    .generics
                    .into_iter()
                    .map(|ty| self.probe_ty(ty))
                    .collect();
                Type::Path(Path::new(path.path, generics))
            }
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
                    .map(|ty| self.probe_ty(ty))
                    .collect();
                let ret_ty = self.probe_ty(*closure.ret_ty);
                Type::Fn(FnSig::new(params, ret_ty))
            }
            Type::Infer(ty_var) => self.unify_table.probe(ty_var).unwrap(),
            ty => ty,
        }
    }

    fn fresh_ty(&mut self) -> Type {
        Type::Infer(self.unify_table.fresh_ty())
    }
}

#[derive(Default, Debug)]
struct UnificationTable {
    table: Vec<Entry>,
}

#[derive(Debug)]
struct Entry {
    parent: TyVar,
    value: Option<Type>,
}

impl Entry {
    pub fn new(parent: TyVar) -> Self {
        Self {
            parent,
            value: None,
        }
    }
}

impl UnificationTable {
    fn unify_var_var(&mut self, lhs: TyVar, rhs: TyVar) -> Result<(), TypeErrKind> {
        let lhs = self.get_root_key(lhs);
        let rhs = self.get_root_key(rhs);

        // They are already unified, return early.
        if lhs == rhs {
            Ok(())
        } else {
            Err(TypeErrKind::UnificationError)
        }
    }

    fn unify_var_ty(&mut self, var: TyVar, ty: Type) -> Result<(), TypeErrKind> {
        let root = self.get_root_key(var);
        let entry = self.entry(root);
        match &entry.value {
            None => {
                entry.value = Some(ty);
                Ok(())
            }
            Some(prev_ty) => {
                if prev_ty != &ty {
                    return Err(TypeErrKind::UnificationError);
                }
                Ok(())
            }
        }
    }

    fn probe(&mut self, key: TyVar) -> Option<Type> {
        let root_key = self.get_root_key(key);
        self.entry(root_key).value.clone()
    }

    fn get_root_key(&mut self, key: TyVar) -> TyVar {
        let entry = self.entry(key);
        if entry.parent == key {
            return key;
        }

        let redirect = entry.parent;
        let root = self.get_root_key(key);

        if root != redirect {
            // Compress the paths
            self.entry(key).parent = root;
        }

        root
    }

    fn entry(&mut self, key: TyVar) -> &mut Entry {
        &mut self.table[key.id as usize]
    }

    // Creates a self-referential index ptr into the vec.
    fn fresh_ty(&mut self) -> TyVar {
        let index = self.table.len();
        let key = TyVar::new(index as u32);
        self.table.push(Entry::new(key));
        key
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

#[derive(Copy, Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
#[repr(transparent)]
#[serde(transparent)]
pub struct TyVar {
    id: u32,
}

impl TyVar {
    fn new(id: u32) -> Self {
        Self { id }
    }
}

#[derive(Clone, PartialEq, Eq, Debug, Serialize, Deserialize)]
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

#[derive(Clone, PartialEq, Eq, Debug, Serialize, Deserialize)]
pub struct Path {
    path: DefId,
    generics: Vec<Type>,
}

impl Path {
    pub fn new(path: DefId, generics: Vec<Type>) -> Self {
        Self { path, generics }
    }
}

#[derive(Clone, PartialEq, Eq, Debug, Serialize, Deserialize)]
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

#[derive(Clone, PartialEq, Eq, Debug, Serialize, Deserialize)]
pub struct FnSig {
    params: Vec<Type>,
    ret_ty: Box<Type>,
}

impl FnSig {
    pub fn new(params: Vec<Type>, ret_ty: Type) -> Self {
        Self {
            params,
            ret_ty: Box::new(ret_ty),
        }
    }
}

/// This enum supports recursive types whose inner types are not yet known.
/// Allows full type definitions to be built incrementally from partial information.
#[derive(Clone, PartialEq, Eq, Debug, Serialize, Deserialize)]
pub enum Type {
    Array(Array),
    Path(Path),
    TraitBound(TraitBound),
    Fn(FnSig),
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
    fn from_path(krate: &HirCrate, path: &PathTy) -> Self {
        // This path points to either a generic param or references a defined ty.
        // We need to disambiguate between the two here so that we can accurately perform ty inference.
        let generics = path
            .generics
            .iter()
            .copied()
            .map(|generic| Self::from(krate, generic))
            .collect();
        // TODO: Maybe disambiguate between generics and definitions here??
        // Might potentially need to do this during resolution somehow to keep the type inference code more straightforward.
        Self::Path(Path::new(path.definition, generics))
    }

    fn from_trait_bound(krate: &HirCrate, trait_bound: &HirTraitBound) -> Self {
        let traits = trait_bound
            .iter()
            .map(|bound| Self::from_path(krate, bound))
            .collect();
        Self::TraitBound(TraitBound::new(traits))
    }

    pub fn from(krate: &HirCrate, ty: LocalDefId) -> Self {
        let ty = krate.ty_stmt(&ty);
        match ty {
            Ty::Array { ty } => {
                let inner_ty = Self::from(krate, *ty);
                Self::Array(Array::new(inner_ty))
            }
            Ty::Path { path } => Self::from_path(krate, path),
            Ty::TraitBound { trait_bound } => Self::from_trait_bound(krate, trait_bound),
            Ty::Closure { params, ret_ty } => {
                let params = params
                    .iter()
                    .copied()
                    .map(|param| Self::from(krate, param))
                    .collect();
                let ret_ty = Self::from(krate, *ret_ty);
                Self::Fn(FnSig::new(params, ret_ty))
            }
            Ty::Primitive(Primitive::U8) => Self::U8,
            Ty::Primitive(Primitive::U16) => Self::U16,
            Ty::Primitive(Primitive::U32) => Self::U32,
            Ty::Primitive(Primitive::U64) => Self::U64,
            Ty::Primitive(Primitive::I8) => Self::I8,
            Ty::Primitive(Primitive::I16) => Self::I16,
            Ty::Primitive(Primitive::I32) => Self::I32,
            Ty::Primitive(Primitive::I64) => Self::I64,
            Ty::Primitive(Primitive::F32) => Self::F32,
            Ty::Primitive(Primitive::F64) => Self::F64,
            Ty::Primitive(Primitive::Str) => Self::Str,
            Ty::Primitive(Primitive::Boolean) => Self::Boolean,
            Ty::Primitive(Primitive::None) => Self::None,
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
            _ => 0,
        }
    }

    fn floating_width(&self) -> usize {
        match self {
            Type::F32 => 1,
            Type::F64 => 2,
            _ => 0,
        }
    }
}

mod tests {
    use itertools::Itertools;

    use crate::compiler::ast::Ident;
    use crate::compiler::compiler::{Application, Compiler, CompilerCtxt};
    use crate::compiler::hir::{
        ArrayExpr, Expr, GlobalLetStmt, HirCrate, HirNode, HirNodeKind, LocalDefId, Primitive, Ty,
    };
    use crate::compiler::krate::CrateId;
    use crate::compiler::tokens::tokenized_file::Span;
    use crate::compiler::ty_infer::{Array, CrateInference, Type};
    use crate::compiler::types::{InternedStr, LDefMap};
    use crate::compiler::StringInterner;
    use crate::util::utils;

    type InferResult = (StringInterner, HirCrate, LDefMap<Type>);

    #[cfg(test)]
    fn parse_module(code: &str) -> InferResult {
        let mut compiler = Compiler::default();
        let main_crate = utils::resolve_test_krate_path(code);
        let crate_path = main_crate.parent().unwrap();
        let application = Application::Inline { code };
        let mut crates = compiler.parse_crates(application).unwrap();
        compiler.validate_crates(&crates).unwrap();

        let resolved_crates = compiler.resolve_crates(&mut crates).unwrap();
        let tys = compiler.infer_types(&resolved_crates).unwrap();

        let krate = resolved_crates.into_krates().exactly_one().unwrap();
        let tys = tys.into_values().exactly_one().unwrap();

        let string_interner = StringInterner::from(CompilerCtxt::from(compiler));
        (string_interner, krate, tys)
    }

    #[test]
    pub fn infer_integer() {
        // let mut hir = HirBuilder::default();
        //
        // let initializer = hir.add(HirNodeKind::Expr(Expr::Int(123)));
        // let global_let = hir.add(HirNodeKind::GlobalLet(GlobalLetStmt::new(
        //     InternedStr::default(),
        //     None,
        //     initializer,
        // )));
        // let krate = hir.infer_tys();
        // assert_eq!(&Type::I64, krate.get(&global_let).unwrap());
    }

    #[test]
    #[should_panic]
    #[ignore]
    pub fn infer_integer_mismatch() {
        // let mut hir = HirBuilder::default();
        // let initializer = hir.add(HirNodeKind::Expr(Expr::Int(123)));
        // let f64_ty = hir.add(HirNodeKind::Ty(Ty::Primitive(Primitive::F64)));
        // let global_let = hir.add(HirNodeKind::GlobalLet(GlobalLetStmt::new(
        //     InternedStr::default(),
        //     Some(f64_ty),
        //     initializer,
        // )));
        // let krate = hir.infer_tys();
    }

    #[test]
    pub fn infer_array_usize() {
        // let mut hir = HirBuilder::default();
        // let one = hir.add(HirNodeKind::Expr(Expr::Int(1)));
        // let two = hir.add(HirNodeKind::Expr(Expr::Int(-2)));
        // let initializer = hir.add(HirNodeKind::Expr(Expr::Array(ArrayExpr::Unsized {
        //     initializers: vec![one, two].into(),
        // })));
        // let global_let = hir.add(HirNodeKind::GlobalLet(GlobalLetStmt::new(
        //     InternedStr::default(),
        //     None,
        //     initializer,
        // )));
        //
        // let types = hir.infer_tys();
        // assert_eq!(
        //     &Type::Array(Array::new(Type::I64)),
        //     types.get(&global_let).unwrap()
        // );
    }

    #[test]
    pub fn infer_generics() {
        let code = r###"
            class Option<T> {
                inner: T,
            } 
        
            fn options() {
                let option_f64 = Option(123);
                let option_str = Option("hi there"); 
            }
        "###;

        let (si, krate, tys) = parse_module(code);
        dbg!(&tys);
    }
}
