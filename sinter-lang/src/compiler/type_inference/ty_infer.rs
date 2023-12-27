use std::collections::HashMap;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::ops::Deref;

use indexmap::Equivalent;
use itertools::Itertools;
use serde::{Deserialize, Serialize};

use crate::compiler::ast::{InfixOp, UnaryOp};
use crate::compiler::compiler::{CompileError, TypeError};
use crate::compiler::hir::{
    ArrayExpr, DefId, Expr, FnStmts, HirCrate, HirMap, HirNodeKind, LocalDefId, Pattern, Primitive,
    Res, Stmt, TraitBound as HirTraitBound, Ty,
};
use crate::compiler::resolver::{ClassDef, FnDef, GlobalVarDef, ValueDef};
use crate::compiler::type_inference::unification::{TyVar, UnificationTable};
use crate::compiler::types::{InternedStr, LDefMap};

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
            let crate_inference = CrateInference::new(krate, self.hir_map);
            crate_inference.infer_tys()?;
        }
        Ok(())
    }
}

#[derive(Debug)]
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

    fn get(&self, node: &LocalDefId) -> &Type {
        self.tys[usize::from(*node)].as_ref().unwrap()
    }
}

#[derive(Debug)]
pub struct CrateInference<'a> {
    unify_table: UnificationTable,
    ty_map: TypeMap,
    errors: Vec<TypeErrKind>,
    krate: &'a HirCrate,
    hir_map: &'a HirMap,
    scopes: Vec<Type>,
}

impl<'a> CrateInference<'a> {
    pub fn new(krate: &'a HirCrate, crate_lookup: &'a HirMap) -> Self {
        Self {
            unify_table: Default::default(),
            ty_map: TypeMap::new(krate.nodes.len()),
            errors: Default::default(),
            krate,
            hir_map: crate_lookup,
            scopes: Vec::default(),
        }
    }

    // TODO: Returned interned ty representation to reduce memory overhead.
    // TODO: Record existing types for class fields and other nodes whose types are known statically.
    pub fn infer_tys(mut self) -> Result<TypeMap, CompileError> {
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
                        let ty = fn_stmt
                            .sig
                            .return_type
                            .map(|ret_ty| self.existing_ty(ret_ty.to_def_id(self.krate.id)))
                            .unwrap_or_else(|| Type::None);
                        self.check_node(&body, ty);
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
                self.infer_node(&body);
            }
        }
    }

    fn check_node(&mut self, node: &LocalDefId, ty: Type) {
        let constraints = self.check(node, ty.clone());
        if let Err(error) = self.unify(constraints) {
            self.errors.push(error)
        } else {
            self.substitute(node);
        }
    }

    fn infer_node(&mut self, node: &LocalDefId) {
        let (constraints, ty) = self.infer(node);
        if let Err(error) = self.unify(constraints) {
            self.errors.push(error)
        } else {
            self.substitute(node);
        }
    }

    fn infer(&mut self, node: &LocalDefId) -> (Constraints, Type) {
        let (constraints, ty) = match self.krate.node(node) {
            HirNodeKind::GlobalLet(global_let) => {
                let ty = self.existing_ty(global_let.ty.to_def_id(self.krate.id));
                let constraints = self.check(&global_let.initializer, ty.clone());
                (constraints, ty)
            }
            HirNodeKind::Block(block) => {
                // Create a new scope to track constraints on the return type.
                let ret_ty = self.fresh_ty(node);
                self.scopes.push(ret_ty.clone());
                let mut constraints = Constraints::default();
                for stmt in block.stmts.iter() {
                    let (c, _) = self.infer(stmt);
                    constraints.extend(c);
                }
                self.scopes.pop();
                (constraints, ret_ty)
            }
            HirNodeKind::Stmt(stmt) => {
                match stmt {
                    Stmt::Let(let_stmt) => {
                        if let Some(ty) = let_stmt.ty {
                            let initializer = let_stmt.initializer.unwrap();
                            let ret_ty = self.existing_ty(initializer.to_def_id(self.krate.id));
                            return (self.check(&initializer, ret_ty), Type::None);
                        }
                        // Create fresh type var for the var type that needs to be inferred
                        let ty_var = self.fresh_ty(node);
                        let (mut c, ty) = self.infer(&let_stmt.initializer.unwrap()); // Unwrap should be safe because it is invalid to have no type and no stmt.
                        c.push(Constraint::Assignable(ty_var, ty));
                        (c, Type::None)
                    }
                    Stmt::Return(return_stmt) => {
                        let ret_ty = self.scopes.last().unwrap().clone();
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
                            let ret_ty = self.scopes.last().unwrap();
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
                        for initializer in initializers.iter() {
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
                    new_constr.push(Constraint::Infix(lhs_ty.clone(), rhs_ty, infix.operator));
                    (new_constr, lhs_ty)
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
                    let params = vec![self.fresh_ty(node); closure.params.len()];
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
                    for (x, arg) in call.args.iter().enumerate() {
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
                                        .collect();
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

                                    let generic_params = class_stmt
                                        .generic_params
                                        .values()
                                        .map(|generic_param| {
                                            self.existing_ty(generic_param.to_def_id(krate.id))
                                        })
                                        .collect();

                                    let param_tys = class_stmt
                                        .fields
                                        .values()
                                        .map(|field| krate.field(field))
                                        .map(|field| self.existing_ty(field.ty.to_def_id(krate.id)))
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
                _ => todo!(),
            },
            _ => todo!(),
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

    fn unify(&mut self, constraints: Constraints) -> Result<(), TypeErrKind> {
        for constr in constraints {
            match constr {
                Constraint::Equal(lhs, rhs) => self.unify_ty_ty(lhs, rhs)?,
                Constraint::Assignable(lhs, rhs) => {
                    if lhs.assignable(&rhs) {
                        self.unify_ty_ty(lhs, rhs)?;
                    } else {
                        return Err(TypeErrKind::NotAssignable(lhs, rhs));
                    }
                }
                Constraint::Array(ty) => self.unify_ty_array(ty)?,
                Constraint::Infix(lhs, rhs, op) => self.unify_infix_op(lhs, rhs, op)?,
                Constraint::Unary(ty, unary_op) => self.unify_unary_op(ty, unary_op)?,
                Constraint::Callable(callable_constraint) => {
                    let CallableConstraint {
                        target_ty, // There should be proper constraints to ensure we know this type by now.
                        args,
                        ret_ty, // This type is always inferred?
                    } = callable_constraint;
                    let ty = self.normalize_ty(target_ty);
                    if let Type::Fn(fn_sig) = &ty {
                        if args.len() != fn_sig.params.len() {
                            return Err(TypeErrKind::InvalidArgs(ty, args));
                        }

                        // TODO: Fix generic args possibly being of a mixed type
                        // i.e fn add<T: Number>(lhs: T, rhs: T) { ... }
                        // add(1usize, 2u32);
                        for x in 0..args.len() {
                            let arg = args[x].clone();
                            let param = fn_sig.params[x].clone();

                            if param.assignable(&arg) {
                                self.unify_ty_ty(param, arg)?;
                            } else {
                                // TODO: Improve error messages for invalid fn args
                                return Err(TypeErrKind::InvalidArgs(ty, args));
                            }
                        }

                        return self.unify_ty_ty(ret_ty, *fn_sig.ret_ty.clone());
                    }
                    return Err(TypeErrKind::NotCallable(ty));
                }
            }
        }
        Ok(())
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
            (
                Type::Infer(lhs) | Type::GenericParam(GenericParam { ty_var: lhs, .. }),
                Type::Infer(rhs) | Type::GenericParam(GenericParam { ty_var: rhs, .. }),
            ) => self.unify_table.unify_var_var(lhs, rhs),
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
            ) => {
                // TODO: Check for cyclic type
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

    /// This function ensures that this type has been substituted with the most up to date version of itself.
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
                            .iter()
                            .for_each(|initializer| self.substitute(initializer));
                    }
                    Expr::Call(call) => {
                        self.substitute(&call.target);
                        call.args.iter().for_each(|arg| self.substitute(arg));
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
                block.stmts.iter().for_each(|stmt| self.substitute(stmt));
            }
            HirNodeKind::Param(_) => {}
            HirNodeKind::Field(_) => {}
            HirNodeKind::Pattern(_) => {}
            HirNodeKind::MatchArm(_) => {}
            // These can safely be ignored since they should not be traversed.
            HirNodeKind::Class(_) => {}
            HirNodeKind::Enum(_) => {}
            HirNodeKind::Trait(_) => {}
            HirNodeKind::TraitImpl(_) => {}
        }

        let ty = self.ty_map.get(node_id).clone();
        dbg!(node);
        dbg!(&ty);
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
            Type::GenericParam(generic_param) => {
                let p_ty = self.unify_table.probe(generic_param.ty_var);
                dbg!(&generic_param);
                dbg!(&p_ty);
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

    fn existing_ty(&mut self, node: DefId) -> Type {
        let krate = self.hir_map.krate(&node);
        let local_id = node.local_id();
        let ty = krate.ty_stmt(&local_id);
        match ty {
            Ty::Array(array) => {
                Type::Array(Array::new(self.existing_ty(array.ty.to_def_id(krate.id))))
            }
            Ty::Path(path) => {
                // TODO: Look up generics from the existing scope
                self.existing_ty(path.definition)
            }
            Ty::GenericParam(generic_param) => {
                let trait_bound = generic_param.trait_bound.map(|bound| {
                    let krate = self.hir_map.krate(&bound);
                    let ty = krate.ty_stmt(&bound.local_id());
                    match ty {
                        Ty::TraitBound(trait_bound) => self.trait_bound(trait_bound),
                        _ => unreachable!(),
                    }
                });

                // TODO: Look up generic param from the existing scope
                Type::GenericParam(GenericParam::new(self.unify_table.fresh_ty(), trait_bound))
            }
            Ty::TraitBound(trait_bound) => {
                let path_tys = trait_bound
                    .iter()
                    // TODO: Handle generics
                    .map(|path_ty| self.existing_ty(path_ty.definition))
                    .collect();
                Type::TraitBound(TraitBound::new(path_tys))
            }
            Ty::Closure(closure) => {
                let params = closure
                    .params
                    .iter()
                    .map(|ty| self.existing_ty(ty.to_def_id(krate.id)))
                    .collect();
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

    fn trait_bound(&mut self, trait_bound: &HirTraitBound) -> TraitBound {
        let path_tys = trait_bound
            .iter()
            // TODO: Handle generics
            .map(|path_ty| self.existing_ty(path_ty.definition))
            .collect();
        TraitBound::new(path_tys)
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
    GenericParam(GenericParam),
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
    /// Whether or not the right hand type is a valid type that can be assigned to this type.
    fn assignable(&self, rhs: &Type) -> bool {
        match self {
            Type::Array(lhs) => {
                if let Type::Array(rhs) = rhs {
                    return lhs.ty.assignable(&rhs.ty);
                }
                false
            }
            Type::Path(lhs_path) => {
                // TODO: Handle assignability?
                match rhs {
                    Type::Path(rhs_path) => lhs_path == rhs_path,
                    _ => false,
                }
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

    use crate::compiler::compiler::{Application, Compiler, CompilerCtxt};
    use crate::compiler::hir::HirCrate;
    use crate::compiler::type_inference::ty_infer::{Type, TypeMap};
    use crate::compiler::types::LDefMap;
    use crate::compiler::StringInterner;
    use crate::util::utils;

    type InferResult = (StringInterner, HirCrate, TypeMap);

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
    }
}
