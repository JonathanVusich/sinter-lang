#![allow(unused)]

use std::cell::RefCell;
use std::fmt::{Debug, Display};

use bumpalo::Bump;
use itertools::{EitherOrBoth, Itertools};
use serde::{Deserialize, Serialize};

use ast::ValueDef;
use diagnostics::{Diagnostic, Diagnostics};
use hir::{
    ForStmt, HirCrate, HirMap, InfixOp, InfixOpClass, ItemKind, LocalDef, Res, StmtKind, WhileStmt,
};
use interner::StringInterner;
use typed_hir::{
    ArrayExpr, Block, BlockStmt, CallExpr, ClassDef, ClosureDef, EnumDef, Expr, ExprId, ExprKind,
    Field, Fields, FloatTy, FnDef, GenericParam, GenericParams, Generics, IfStmt, InfixExpr, IntTy,
    LetStmt, LocalVar, MemberDef, Params, PathExpr, ReturnStmt, Stmt, StmtId, Thir, ThirCrate,
    ThirMap, Trait, TraitBound, TraitDef, Ty, TyKind, TyVar, UintTy,
};
use types::{DefMap, LDefMap, StrMap};

use crate::resolution::TyResolver;
use crate::unification::UnificationTable;

mod resolution;
mod trait_solver;
mod unification;

pub fn infer_types<'hir>(
    string_interner: &'hir StringInterner,
    diagnostics: &'hir Diagnostics,
    hir_allocator: &'hir Bump,
    hir_map: &'hir HirMap<'hir>,
) -> ThirMap<'hir> {
    let mut crates = Vec::default();
    let ty_resolver: TyResolver<'hir> = TyResolver::new(hir_map, hir_allocator);
    for krate in hir_map.krates() {
        let crate_inference = CrateInference::new(
            string_interner,
            diagnostics,
            &ty_resolver,
            hir_allocator,
            krate,
            hir_map,
        );
        let bodies = crate_inference.infer_bodies();
        let tkrate = ThirCrate {
            name: krate.name,
            id: krate.id,
            bodies,
        };
        crates.push(tkrate);
    }

    ThirMap { crates }
}

/// Contains the data needed to infer types for a given HIR crate.
/// The HIR nodes are transformed into their typed HIR representation
/// which ensures that all nodes have correct type information.
///
/// TODO: All types SHOULD also be interned across crates using a ty interner.
#[derive(Debug)]
pub struct CrateInference<'a, 'hir> {
    string_interner: &'hir StringInterner,
    diagnostics: &'hir Diagnostics,
    ty_resolver: &'a TyResolver<'hir>,

    hir_map: &'hir HirMap<'hir>,
    krate: &'hir HirCrate<'hir>,

    hir_allocator: &'hir Bump,
    bodies: DefMap<Thir<'hir>>,
}

/// TODO: Need to figure out an alternate representation for these types
/// that will work with type resolution and will avoid the need for recursive
/// type building.
#[derive(Debug)]
pub enum DefType<'a> {
    Class(ClassDef<'a>),
    Enum(EnumDef<'a>),
    Member(MemberDef<'a>),
    Trait(TraitDef<'a>),
    Fn(FnDef<'a>),
    GenericParam(&'a GenericParam<'a>),
}

static GENERICS: hir::Generics<'static> = &[];

#[derive(Debug)]
pub struct InferCtxt<'a, 'hir> {
    // Contains the current THIR body and information used to unify all the types.
    unify_table: UnificationTable<'hir>,
    ty_resolver: &'a TyResolver<'hir>,
    string_interner: &'a StringInterner,
    diagnostics: &'a Diagnostics,
    ty_map: LDefMap<Ty<'hir>>,
    constraints: Constraints<'hir>,
    type_envs: TypeEnvs<'hir>,
    hir_allocator: &'hir Bump,
    thir: RefCell<Thir<'hir>>,
}

impl<'a, 'hir> InferCtxt<'a, 'hir> {
    /// The main function that performs type inference. Every function body or
    /// expression should have a return type defined at compile time, and we can
    /// use this to infer and validate an entire expression/blocks' types.
    fn check_expr(&self, expr: hir::Expr<'hir>, ret_ty: Ty<'hir>) -> ExprId {
        self.type_envs.push(TypeEnv::new(ret_ty));

        let expr_id = match (expr.kind, ret_ty.kind) {
            (hir::ExprKind::None, TyKind::None) => self.insert_expr(ExprKind::None, ret_ty),
            (hir::ExprKind::True, TyKind::Boolean) => self.insert_expr(ExprKind::True, ret_ty),
            (hir::ExprKind::False, TyKind::Boolean) => self.insert_expr(ExprKind::False, ret_ty),
            (hir::ExprKind::Int(int), TyKind::Int(IntTy::I64)) => {
                self.insert_expr(ExprKind::Int(int), ret_ty)
            }
            (hir::ExprKind::UInt(uint), TyKind::Uint(UintTy::U64)) => {
                self.insert_expr(ExprKind::UInt(uint), ret_ty)
            }
            (hir::ExprKind::Float(float), TyKind::Float(FloatTy::F64)) => {
                self.insert_expr(ExprKind::Float(float), ret_ty)
            }
            (hir::ExprKind::String(string), TyKind::Str) => {
                self.insert_expr(ExprKind::String(string), ret_ty)
            }
            (hir::ExprKind::None, TyKind::None) => self.insert_expr(ExprKind::None, ret_ty),
            _ => {
                let (expr_id, ty) = self.infer_expr(&expr);
                self.add_constraint(Constraint::Assignable(ty, ret_ty));
                expr_id
            }
        };
        self.type_envs.pop();
        expr_id
    }

    /// The main function that performs type inference. Every function body or
    /// expression should have a return type defined at compile time, and we can
    /// use this to infer and validate an entire expression/blocks' types.
    fn check_block(&self, block: &hir::Block<'hir>, ret_ty: Ty<'hir>) {
        self.type_envs.push(TypeEnv::new(ret_ty));
        for stmt in block.stmts {
            self.infer_stmt(stmt);
        }
        self.type_envs.pop();
    }

    fn check_stmt(&self, stmt: &hir::Stmt<'hir>, ret_ty: Ty<'hir>) {
        todo!()
    }

    fn insert_expr(&self, kind: ExprKind<'hir>, ty: Ty<'hir>) -> ExprId {
        let expr = Expr { kind, ty };
        self.thir.borrow_mut().insert_expr(expr)
    }

    fn infer_stmt(&self, stmt: &hir::Stmt<'hir>) -> (StmtId, Ty<'hir>) {
        match stmt.kind {
            StmtKind::Let(let_stmt) => self.infer_let_stmt(let_stmt),
            StmtKind::For(for_stmt) => self.infer_for_stmt(for_stmt),
            StmtKind::If(if_stmt) => self.infer_if_stmt(if_stmt),
            StmtKind::Return(return_stmt) => self.infer_return_stmt(return_stmt),
            StmtKind::While(while_stmt) => self.infer_while_stmt(while_stmt),
            StmtKind::Block(block) => self.infer_block_stmt(block),
            StmtKind::Expression(expr) => self.infer_expr_stmt(expr),
        }
    }

    fn infer_let_stmt(&self, let_stmt: &'hir hir::LetStmt<'hir>) -> (StmtId, Ty<'hir>) {
        let ty = match let_stmt.ty {
            Some(explicit_ty) => self.ty_resolver.resolve_ty(explicit_ty),
            None => self
                .ty_resolver
                .intern(TyKind::Infer(self.unify_table.fresh_ty())),
        };
        let local_var = LocalVar {
            ident: let_stmt.local_var.ident,
            ty,
        };

        self.type_envs.insert(local_var, ty);

        let mutability = let_stmt.mutability;

        let mut initializer = None;
        if let Some(expr) = let_stmt.initializer {
            let (expr_id, expr_ty) = self.infer_expr(expr);
            self.add_constraint(Constraint::Assignable(ty, expr_ty));
            initializer = Some(expr_id);
        }
        let stmt = LetStmt {
            local_var,
            mutability,
            initializer,
        };
        let stmt_id = self.thir.borrow_mut().insert_stmt(Stmt::Let(stmt));
        (stmt_id, ty)
    }

    fn infer_for_stmt(&self, for_stmt: &'hir ForStmt<'hir>) -> (StmtId, Ty<'hir>) {
        todo!()
    }

    fn infer_if_stmt(&self, if_stmt: &'hir hir::IfStmt<'hir>) -> (StmtId, Ty<'hir>) {
        let bool_ty = self.ty_resolver.intern(TyKind::Boolean);
        let none_ty = self.ty_resolver.intern(TyKind::None);
        let condition = self.check_expr(if_stmt.condition, bool_ty);
        let (if_true, true_ty) = self.infer_block(&if_stmt.if_true);

        // If stmt blocks (for now) should not yield a value.
        self.add_constraint(Constraint::Equal(none_ty, true_ty));

        let (if_false, false_ty) = if_stmt
            .if_false
            .map(|block| {
                let (false_id, false_ty) = self.infer_block(&block);
                self.add_constraint(Constraint::Equal(none_ty, true_ty));
                (Some(false_id), Some(false_ty))
            })
            .unwrap_or_else(|| (None, None));

        let if_stmt = IfStmt {
            condition,
            if_true,
            if_false,
        };
        let stmt_id = self.thir.borrow_mut().insert_stmt(Stmt::If(if_stmt));
        (stmt_id, none_ty)
    }

    fn infer_return_stmt(&self, return_stmt: &'hir hir::ReturnStmt<'hir>) -> (StmtId, Ty<'hir>) {
        let (expr, ty) = return_stmt
            .value
            .map(|expr| {
                let (id, ty) = self.infer_expr(expr);
                (Some(id), ty)
            })
            .unwrap_or_else(|| (None, self.ty_resolver.intern(TyKind::None)));

        self.add_constraint(Constraint::Assignable(self.type_envs.root_ty(), ty));

        let return_stmt = ReturnStmt { expr };
        let stmt_id = self
            .thir
            .borrow_mut()
            .insert_stmt(Stmt::Return(return_stmt));
        (stmt_id, ty)
    }

    fn infer_while_stmt(&self, while_stmt: &'hir WhileStmt<'hir>) -> (StmtId, Ty<'hir>) {
        todo!()
    }

    fn infer_expr_stmt(&self, expression: &hir::Expression) -> (StmtId, Ty<'hir>) {
        todo!()
    }

    fn build_thir(self) -> Thir<'hir> {
        // Solve constraints (if possible)
        self.unify_constraints();
        self.substitute_thir();

        self.thir.into_inner()
    }

    fn infer_expr(&self, expr: &hir::Expr<'hir>) -> (ExprId, Ty<'hir>) {
        match expr.kind {
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
            hir::ExprKind::String(string) => {
                self.infer_static(ExprKind::String(string), TyKind::Str)
            }
            hir::ExprKind::Match(match_expr) => self.infer_match(match_expr),
            hir::ExprKind::Closure(closure) => self.infer_closure(closure),
            hir::ExprKind::Assign(assign) => self.infer_assign(assign),
            hir::ExprKind::Field(field) => self.infer_field(field),
            hir::ExprKind::Index(index) => self.infer_index(index),
            hir::ExprKind::Path(path) => self.infer_path(path),
            hir::ExprKind::Block(block) => self.infer_block(&block),
            hir::ExprKind::Break => self.infer_static(ExprKind::Break, TyKind::None),
            hir::ExprKind::Continue => self.infer_static(ExprKind::Continue, TyKind::None),
        }
    }

    fn infer_array(&self, array_expr: hir::ArrayExpr<'hir>) -> (ExprId, Ty<'hir>) {
        match array_expr {
            hir::ArrayExpr::Sized { initializer, size } => {
                let (init_id, init_ty) = self.infer_expr(&initializer);
                let (size_id, size_ty) = self.infer_expr(size);

                let kind = TyKind::Array(init_ty);
                let ty = self.ty_resolver.intern(kind);

                let expr = Expr {
                    kind: ExprKind::Array(ArrayExpr::Sized {
                        initializer: init_id,
                        size: size_id,
                    }),
                    ty,
                };
                let expr_id = self.thir.borrow_mut().insert_expr(expr);

                (expr_id, ty)
            }
            hir::ArrayExpr::Unsized { initializers } => {
                let array_ty = self
                    .ty_resolver
                    .intern(TyKind::Infer(self.unify_table.fresh_ty()));

                let mut init_ids = Vec::with_capacity(initializers.len());
                for initializer in initializers {
                    let (init_id, init_ty) = self.infer_expr(initializer);
                    init_ids.push(init_id);
                    self.constraints
                        .borrow_mut()
                        .push(Constraint::Assignable(array_ty, init_ty));
                }
                let ty = self.ty_resolver.intern(TyKind::Array(array_ty));
                let expr = Expr {
                    kind: ExprKind::Array(ArrayExpr::Unsized {
                        initializers: init_ids.into_boxed_slice(),
                    }),
                    ty,
                };
                let expr_id = self.thir.borrow_mut().insert_expr(expr);
                (expr_id, ty)
            }
        }
    }

    fn infer_call(&self, call: hir::CallExpr<'hir>) -> (ExprId, Ty<'hir>) {
        let (target, target_ty) = self.infer_expr(call.target);

        let mut args = Vec::with_capacity(call.args.len());
        let mut tys = Vec::with_capacity(call.args.len());
        for arg in call.args {
            let (expr_id, ty) = self.infer_expr(arg);
            args.push(expr_id);
            tys.push(ty);
        }
        let boxed_args = args.into_boxed_slice();

        self.add_constraint(Constraint::Callable(target_ty, tys));

        let expr_kind = ExprKind::Call(CallExpr {
            target,
            args: boxed_args,
        });

        let expr_id = self.insert_expr(expr_kind, target_ty);
        (expr_id, target_ty)
    }

    fn infer_infix(&self, infix: hir::InfixExpr<'hir>) -> (ExprId, Ty<'hir>) {
        let (lhs, lhs_ty) = self.infer_expr(infix.lhs);
        let (rhs, rhs_ty) = self.infer_expr(infix.rhs);

        self.add_constraint(Constraint::Infix(lhs_ty, rhs_ty, infix.operator));

        let resultant_ty = match infix.operator.class() {
            InfixOpClass::Assignment | InfixOpClass::Arithmetic | InfixOpClass::Bitwise => lhs_ty,
            InfixOpClass::Logical | InfixOpClass::Comparison => {
                self.ty_resolver.intern(TyKind::Boolean)
            }
        };

        let expr = Expr {
            kind: ExprKind::Infix(InfixExpr {
                operator: infix.operator,
                lhs,
                rhs,
            }),
            // Use the LHS ty since the right side should be coercable to the LHS ty.
            ty: resultant_ty,
        };
        let expr_id = self.thir.borrow_mut().insert_expr(expr);

        (expr_id, resultant_ty)
    }

    fn infer_unary(&self, unary: hir::UnaryExpr) -> (ExprId, Ty<'hir>) {
        todo!()
    }

    fn infer_static(&self, expr_kind: ExprKind<'hir>, ty_kind: TyKind<'hir>) -> (ExprId, Ty<'hir>) {
        let ty = self.ty_resolver.intern(ty_kind);
        let expr_id = self.insert_expr(expr_kind, ty);
        (expr_id, ty)
    }

    fn infer_int(&self, int: i64) -> (ExprId, Ty<'hir>) {
        todo!()
    }

    fn infer_uint(&self, int: u64) -> (ExprId, Ty<'hir>) {
        todo!()
    }

    fn infer_float(&self, float: f64) -> (ExprId, Ty<'hir>) {
        todo!()
    }

    fn infer_match(&self, match_expr: hir::MatchExpr) -> (ExprId, Ty<'hir>) {
        todo!()
    }

    fn infer_closure(&self, closure: hir::ClosureExpr) -> (ExprId, Ty<'hir>) {
        todo!()
    }

    fn infer_assign(&self, assign: hir::AssignExpr) -> (ExprId, Ty<'hir>) {
        todo!()
    }

    fn infer_field(&self, field: hir::FieldExpr) -> (ExprId, Ty<'hir>) {
        todo!()
    }

    fn infer_index(&self, index_expr: hir::IndexExpr) -> (ExprId, Ty<'hir>) {
        todo!()
    }

    fn infer_path(&self, path: hir::PathExpr<'hir>) -> (ExprId, Ty<'hir>) {
        let last_segment = path.segments.last().unwrap();
        let (res, generics) = (last_segment.res, last_segment.generics.unwrap_or(GENERICS));
        match res {
            Res::Crate(_) | Res::ModuleSegment(_, _) | Res::Module(_) | Res::Primitive(_) => {
                panic!("I think this is a logic bug that should be caught by the resolver.")
            }
            Res::Local(LocalDef::Var(local_var)) => {
                // Unwrap should be safe since we have already validated the HIR and local vars should resolve.
                let ty = self.type_envs.find(local_var).unwrap();
                let path_expr = PathExpr::Var(LocalVar {
                    ident: local_var.ident,
                    ty,
                });
                let expr = Expr {
                    kind: ExprKind::Path(path_expr),
                    ty,
                };
                let expr_id = self.thir.borrow_mut().insert_expr(expr);
                (expr_id, ty)
            }
            Res::ValueDef(ValueDef::Class(class_def)) => {
                let ty = self.ty_resolver.type_of(class_def.id, generics);
                let expr = PathExpr::Class(class_def.id);
                let expr_id = self.insert_expr(ExprKind::Path(expr), ty);
                return (expr_id, ty);
            }
            any => {
                dbg!(any);
                todo!()
            }
        }
    }

    fn infer_block_stmt(&self, block_stmt: &hir::BlockStmt<'hir>) -> (StmtId, Ty<'hir>) {
        let (block, ty) = self.infer_block(&block_stmt.block);
        let block_stmt = BlockStmt { block };
        let block = self.thir.borrow_mut().insert_stmt(Stmt::Block(block_stmt));
        (block, ty)
    }

    fn infer_block(&self, block: &hir::Block<'hir>) -> (ExprId, Ty<'hir>) {
        self.type_envs.push(TypeEnv::new(self.fresh_ty()));

        let mut stmts = Vec::with_capacity(block.stmts.len());
        for stmt in block.stmts {
            let (stmt_id, stmt_ty) = self.infer_stmt(stmt);
            stmts.push(stmt_id);
        }

        let ret_ty = self.type_envs.pop().unwrap().ret_ty;

        let block = Block {
            stmts: stmts.into_boxed_slice(),
        };
        let expr_id = self.insert_expr(ExprKind::Block(block), ret_ty);
        (expr_id, ret_ty)
    }

    fn fresh_ty(&self) -> Ty<'hir> {
        let ty_kind = TyKind::Infer(self.unify_table.fresh_ty());
        self.ty_resolver.intern(ty_kind)
    }

    /// Walk the expression tree and normalize the types
    /// which should generate a completely typed expression tree.
    fn substitute_thir(&self) {
        let mut borrowed_thir = self.thir.borrow_mut();
        for expr in borrowed_thir.exprs() {
            // TODO: Identify why blocks have a separately defined ret_ty and fix the type solving for it or consolidate it into the expr.ty field.
            dbg!(&expr);
            expr.ty = self.normalize_ty(expr.ty).unwrap_or(expr.ty);
        }
        for stmt in borrowed_thir.stmts() {
            match stmt {
                Stmt::Let(let_stmt) => {
                    let normalized_ty = self
                        .normalize_ty(let_stmt.local_var.ty)
                        .unwrap_or(let_stmt.local_var.ty);
                    let_stmt.local_var.ty = normalized_ty;
                }
                Stmt::For(for_stmt) => {
                    for_stmt.ident.ty = self
                        .normalize_ty(for_stmt.ident.ty)
                        .unwrap_or(for_stmt.ident.ty);
                }
                _ => {}
            }
        }
    }

    fn add_constraint(&self, constraint: Constraint<'hir>) {
        self.constraints.borrow_mut().push(constraint);
    }

    fn unify_constraints(&self) {
        self.constraints
            .borrow()
            .iter()
            .map(|constraint| self.unify(constraint))
            .filter(|eval| eval.is_error())
            // TODO: Add diagnostic information for each failure.
            .for_each(|eval| {
                let diagnostic_str = match eval {
                    ConstraintEvaluation::Success => unreachable!(),
                    ConstraintEvaluation::NotEqual(lhs, rhs) => {
                        format!("{:?} is not equal to {:?}", *lhs, *rhs)
                    }
                    ConstraintEvaluation::NotAssignable(lhs, rhs) => {
                        format!("{:?} is not assignable to {:?}", *lhs, *rhs)
                    }
                    ConstraintEvaluation::InfixOpUnsupported(ty, infix) => {
                        format!("{:?} does not support infix operator {:?}", *ty, infix)
                    }
                    ConstraintEvaluation::NotCallable(ty) => {
                        format!("{:?} is not a callable type!", *ty)
                    }
                    ConstraintEvaluation::MissingArg(field) => {
                        let field_name = self.string_interner.resolve(field.name.ident);
                        format!("Missing an argument for field {:?}", field_name)
                    }
                    ConstraintEvaluation::ExtraArg(arg) => {
                        format!("Extra argument of {:?} supplied!", *arg)
                    }
                };
                self.diagnostics.push(Diagnostic::Error(diagnostic_str));
            });
    }

    fn unify(&self, constraint: &Constraint<'hir>) -> ConstraintEvaluation<'hir> {
        dbg!(constraint);
        match constraint {
            Constraint::Equal(lhs, rhs) => {
                self.unify_ty_ty(*lhs, *rhs, |lhs, rhs| self.eq(*lhs, *rhs))
            }
            Constraint::Assignable(lhs, rhs) => {
                self.unify_ty_ty(*lhs, *rhs, |lhs, rhs| self.assignable(*lhs, *rhs))
            }
            Constraint::Callable(target, args) => self.unify_callable(*target, args),
            Constraint::Infix(lhs, rhs, infix_op) => self.unify_ty_ty(*lhs, *rhs, |lhs, rhs| {
                self.assignable(*lhs, *rhs)
                    .and(self.supports_infix(*lhs, *infix_op))
            }),
        }
    }

    fn supports_infix(&self, ty: Ty<'hir>, infix_op: InfixOp) -> ConstraintEvaluation<'hir> {
        let supported = match ty.kind {
            TyKind::Float(_) => match infix_op {
                InfixOp::Assign
                | InfixOp::Add
                | InfixOp::Subtract
                | InfixOp::Multiply
                | InfixOp::Divide
                | InfixOp::Less
                | InfixOp::Greater
                | InfixOp::LessEqual
                | InfixOp::GreaterEqual => true,
                _ => false,
            },
            TyKind::Int(_) => true,
            TyKind::Uint(_) => true,
            TyKind::Boolean => match infix_op {
                InfixOp::Or | InfixOp::And | InfixOp::Equal | InfixOp::NotEqual => true,
                _ => false,
            },
            TyKind::Str => match infix_op {
                InfixOp::Equal | InfixOp::NotEqual => true,
                _ => false,
            },
            _ => false,
        };
        if supported {
            ConstraintEvaluation::Success
        } else {
            ConstraintEvaluation::InfixOpUnsupported(ty, infix_op)
        }
    }

    fn eq(&self, lhs: Ty<'hir>, rhs: Ty<'hir>) -> ConstraintEvaluation<'hir> {
        return if (lhs.eq(&rhs)) {
            ConstraintEvaluation::Success
        } else {
            ConstraintEvaluation::NotEqual(lhs, rhs)
        };
    }

    fn assignable(&self, lhs: Ty<'hir>, rhs: Ty<'hir>) -> ConstraintEvaluation<'hir> {
        match *lhs {
            TyKind::Infer(_) => ConstraintEvaluation::Success,
            TyKind::Array(inner_lhs) => {
                return if let TyKind::Array(inner_rhs) = *rhs {
                    self.assignable(inner_lhs, inner_rhs)
                } else {
                    ConstraintEvaluation::NotAssignable(lhs, rhs)
                }
            }
            TyKind::TraitBound(trait_bound) => {
                todo!()
                // Implement trait mapping logic
            }
            TyKind::GenericParam(param) => {
                // TODO: Implement trait bound logic
                ConstraintEvaluation::Success
            }
            TyKind::Float(FloatTy::F32) => {
                if matches!(*rhs, TyKind::Float(_)) {
                    ConstraintEvaluation::Success
                } else {
                    ConstraintEvaluation::NotAssignable(lhs, rhs)
                }
            }
            TyKind::Float(FloatTy::F64) => {
                if matches!(*rhs, TyKind::Float(FloatTy::F64)) {
                    ConstraintEvaluation::Success
                } else {
                    ConstraintEvaluation::NotAssignable(lhs, rhs)
                }
            }
            TyKind::Int(IntTy::I8) => {
                if matches!(*rhs, TyKind::Int(_)) {
                    ConstraintEvaluation::Success
                } else {
                    ConstraintEvaluation::NotAssignable(lhs, rhs)
                }
            }
            TyKind::Int(IntTy::I16) => {
                if matches!(
                    *rhs,
                    TyKind::Int(IntTy::I16) | TyKind::Int(IntTy::I32) | TyKind::Int(IntTy::I64)
                ) {
                    ConstraintEvaluation::Success
                } else {
                    ConstraintEvaluation::NotAssignable(lhs, rhs)
                }
            }
            TyKind::Int(IntTy::I32) => {
                if matches!(*rhs, TyKind::Int(IntTy::I32) | TyKind::Int(IntTy::I64)) {
                    ConstraintEvaluation::Success
                } else {
                    ConstraintEvaluation::NotAssignable(lhs, rhs)
                }
            }
            TyKind::Int(IntTy::I64) => {
                if matches!(*rhs, TyKind::Int(IntTy::I64)) {
                    ConstraintEvaluation::Success
                } else {
                    ConstraintEvaluation::NotAssignable(lhs, rhs)
                }
            }
            TyKind::Uint(UintTy::U8) => {
                if matches!(*rhs, TyKind::Uint(_)) {
                    ConstraintEvaluation::Success
                } else {
                    ConstraintEvaluation::NotAssignable(lhs, rhs)
                }
            }
            TyKind::Uint(UintTy::U16) => {
                if matches!(
                    *rhs,
                    TyKind::Uint(UintTy::U16)
                        | TyKind::Uint(UintTy::U32)
                        | TyKind::Uint(UintTy::U64)
                ) {
                    ConstraintEvaluation::Success
                } else {
                    ConstraintEvaluation::NotAssignable(lhs, rhs)
                }
            }
            TyKind::Uint(UintTy::U32) => {
                if matches!(*rhs, TyKind::Uint(UintTy::U32) | TyKind::Uint(UintTy::U64)) {
                    ConstraintEvaluation::Success
                } else {
                    ConstraintEvaluation::NotAssignable(lhs, rhs)
                }
            }
            TyKind::Uint(UintTy::U64) => {
                if matches!(*rhs, TyKind::Uint(UintTy::U64)) {
                    ConstraintEvaluation::Success
                } else {
                    ConstraintEvaluation::NotAssignable(lhs, rhs)
                }
            }
            _ => self.eq(lhs, rhs),
        }
    }

    fn callable(&self, ty: Ty<'hir>, args: Vec<Ty<'hir>>) -> ConstraintEvaluation<'hir> {
        match *ty {
            TyKind::Class(class_def, generics) => {
                let field_tys =
                    self.substitute_fields(class_def.fields, class_def.generic_params, generics);
                dbg!(field_tys);
                return field_tys
                    .iter()
                    .zip_longest(args.iter())
                    .map(|item| match item {
                        EitherOrBoth::Both(field, rhs) => {
                            self.unify_ty_ty(field.ty, *rhs, |lhs, rhs| self.assignable(*lhs, *rhs))
                        }
                        EitherOrBoth::Left(field) => ConstraintEvaluation::MissingArg(*field),
                        EitherOrBoth::Right(arg) => ConstraintEvaluation::ExtraArg(*arg),
                    })
                    .reduce(|lhs, rhs| {
                        return if let ConstraintEvaluation::Success = lhs {
                            rhs
                        } else {
                            lhs
                        };
                    })
                    .unwrap_or(ConstraintEvaluation::Success);
            }
            TyKind::Member(_, _) | TyKind::Fn(_, _) | TyKind::Closure(_) => {
                ConstraintEvaluation::Success
            }
            _ => ConstraintEvaluation::NotCallable(ty),
        }
    }

    fn unify_ty_ty<F: Fn(&Ty<'hir>, &Ty<'hir>) -> ConstraintEvaluation<'hir>>(
        &self,
        lhs: Ty<'hir>,
        rhs: Ty<'hir>,
        assignable_check: F,
    ) -> ConstraintEvaluation<'hir> {
        let lhs_ty = self.normalize_ty(lhs).unwrap_or(lhs);
        let rhs_ty = self.normalize_ty(rhs).unwrap_or(rhs);

        match (lhs_ty, rhs_ty) {
            (
                Ty {
                    kind: TyKind::Infer(infer),
                },
                other,
            )
            | (
                other,
                Ty {
                    kind: TyKind::Infer(infer),
                },
            ) => self.unify_var_ty(*infer, other, assignable_check),
            (
                Ty {
                    kind: TyKind::Infer(lhs),
                },
                Ty {
                    kind: TyKind::Infer(rhs),
                },
            ) => {
                if self.unify_var_var(*lhs, *rhs) {
                    ConstraintEvaluation::Success
                } else {
                    ConstraintEvaluation::NotEqual(lhs_ty, rhs_ty)
                }
            }
            /// Both types are at least partially known, so we unify them.
            (lhs, rhs) => assignable_check(&lhs, &rhs),
        }
    }

    fn unify_var_ty<F: Fn(&Ty<'hir>, &Ty<'hir>) -> ConstraintEvaluation<'hir>>(
        &self,
        var: TyVar,
        ty: Ty<'hir>,
        assignable_check: F,
    ) -> ConstraintEvaluation<'hir> {
        self.unify_table.unify_var_ty(var, ty, assignable_check)
    }

    fn unify_var_var(&self, lhs: TyVar, rhs: TyVar) -> bool {
        self.unify_table.unify_var_var(lhs, rhs)
    }

    fn unify_callable(&self, target: Ty<'hir>, args: &Vec<Ty<'hir>>) -> ConstraintEvaluation<'hir> {
        let target = self.normalize_ty(target).unwrap_or(target);
        let args = args
            .iter()
            .copied()
            .map(|arg| self.normalize_ty(arg).unwrap_or(arg))
            .collect();
        return self.callable(target, args);
    }

    /// This method inspects a given type and returns an optional new type
    /// if there is some aspect of the type that can be updated due to constraint solving.
    fn normalize_ty(&self, ty: Ty<'hir>) -> Option<Ty<'hir>> {
        match ty.kind {
            TyKind::Array(array) => self
                .normalize_ty(*array)
                .map(|inner_ty| self.ty_resolver.intern(TyKind::Array(inner_ty))),
            TyKind::Class(class_def, generics) => self
                .normalize_tys(generics)
                .map(|generics| self.ty_resolver.intern(TyKind::Class(class_def, generics))),
            TyKind::Enum(enum_def, generics) => self
                .normalize_tys(generics)
                .map(|generics| self.ty_resolver.intern(TyKind::Enum(enum_def, generics))),
            TyKind::Member(member_def, generics) => self.normalize_tys(generics).map(|generics| {
                self.ty_resolver
                    .intern(TyKind::Member(member_def, generics))
            }),
            TyKind::TraitBound(trait_bound) => self
                .normalize_trait_bound(trait_bound)
                .map(|trait_bound| self.ty_resolver.intern(TyKind::TraitBound(trait_bound))),
            TyKind::GenericParam(GenericParam { ident, trait_bound }) => match trait_bound {
                Some(trait_bound) => self.normalize_trait_bound(trait_bound).map(|trait_bound| {
                    let generic_param = self.alloc(GenericParam {
                        ident: *ident,
                        trait_bound: Some(trait_bound),
                    });
                    self.ty_resolver.intern(TyKind::GenericParam(generic_param))
                }),
                None => None,
            },
            TyKind::Fn(fn_def, generics) => self
                .normalize_tys(generics)
                .map(|generics| self.ty_resolver.intern(TyKind::Fn(fn_def, generics))),
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
                        self.ty_resolver.intern(TyKind::Closure(closure_def))
                    })
            }
            TyKind::Infer(ty_var) => {
                let probed = self.unify_table.probe(*ty_var);
                match probed {
                    None => None,
                    Some(ty_kind) => Some(self.normalize_ty(ty_kind).unwrap_or(ty_kind)),
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

    fn substitute_fields(
        &self,
        fields: Fields<'hir>,
        generic_params: GenericParams<'hir>,
        generics: Generics<'hir>,
    ) -> Fields<'hir> {
        assert_eq!(generic_params.len(), generics.len());
        self.hir_allocator
            .alloc_slice_fill_iter(fields.iter().map(|field| match *field.ty {
                TyKind::GenericParam(param) => {
                    let generic_index = generic_params
                        .iter()
                        .copied()
                        .enumerate()
                        .find(|(index, inner)| *inner == param)
                        .map(|(index, param)| index)
                        .unwrap();
                    return Field {
                        name: field.name,
                        ty: generics[generic_index],
                    };
                }
                _ => *field,
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
        generic_params: GenericParams<'hir>,
        params: Params<'hir>,
        ty_resolver: &'a TyResolver<'hir>,
        string_interner: &'a StringInterner,
        diagnostics: &'a Diagnostics,
        hir_allocator: &'hir Bump,
        ret_ty: Ty<'hir>,
    ) -> Self {
        let mut type_envs = TypeEnvs::default();
        let mut type_env = TypeEnv::new(ret_ty);
        for param in params {
            let ty = ty_resolver.type_of(param.id);
            type_env.local_vars.insert(param.name.ident, param.ty);
        }
        type_envs.push(type_env);
        let thir = Thir::new(generic_params, ret_ty);
        Self {
            unify_table: Default::default(),
            ty_resolver,
            string_interner,
            diagnostics,
            ty_map: Default::default(),
            constraints: Default::default(),
            type_envs,
            hir_allocator,
            thir: RefCell::new(thir),
        }
    }
}

impl<'a, 'hir> CrateInference<'a, 'hir> {
    pub fn new(
        string_interner: &'hir StringInterner,
        diagnostics: &'hir Diagnostics,
        ty_resolver: &'a TyResolver<'hir>,
        hir_allocator: &'hir Bump,
        krate: &'hir HirCrate,
        hir_map: &'hir HirMap,
    ) -> Self {
        Self {
            string_interner,
            diagnostics,
            ty_resolver,
            hir_allocator,
            hir_map,
            krate,
            bodies: Default::default(),
        }
    }

    // TODO: Returned interned ty representation to reduce memory overhead.
    // TODO: Record existing types for class fields and other nodes whose types are known statically.
    pub fn infer_bodies(mut self) -> LDefMap<Thir<'hir>> {
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
        self.bodies
    }

    fn infer_constant(&mut self, constant: &hir::Constant<'hir>) {
        let ret_ty = self.ty_resolver.resolve_ty(constant.ty);

        let mut infer_ctxt: InferCtxt = InferCtxt::new(
            GenericParams::default(),
            Params::default(),
            &self.ty_resolver,
            &self.string_interner,
            &self.diagnostics,
            &self.hir_allocator,
            ret_ty,
        );
        infer_ctxt.check_expr(*constant.initializer, ret_ty);
        let thir = infer_ctxt.build_thir();

        self.bodies.insert(constant.initializer.id, thir);
    }

    fn infer_class(&mut self, class_def: &hir::ClassDef<'hir>) {
        // TODO: Add type checking for proper usage of generic parameters and type construction of field types.
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

    fn infer_member(&mut self, node: &hir::MemberDef<'hir>) {
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
            let generic_params = self
                .ty_resolver
                .resolve_generic_params(fn_def.sig.generic_params);
            let params = self.ty_resolver.resolve_params(fn_def.sig.params);

            let ret_ty = self
                .ty_resolver
                .maybe_resolve_ty(fn_def.sig.ret_ty)
                .unwrap_or_else(|| self.ty_resolver.intern(TyKind::None));

            let infer_ctxt = InferCtxt::new(
                generic_params,
                params,
                &self.ty_resolver,
                &self.string_interner,
                &self.diagnostics,
                &self.hir_allocator,
                ret_ty,
            );
            infer_ctxt.check_block(block, ret_ty);
            let thir = infer_ctxt.build_thir();
            self.bodies.insert(fn_def.id, thir);
        }
    }
}

type Constraints<'hir> = RefCell<Vec<Constraint<'hir>>>;

#[derive(Debug, Default)]
struct TypeEnvs<'hir> {
    envs: RefCell<Vec<TypeEnv<'hir>>>,
}

impl<'hir> TypeEnvs<'hir> {
    pub fn from(type_env: TypeEnv<'hir>) -> Self {
        Self {
            envs: RefCell::new(vec![type_env]),
        }
    }
    pub fn push(&self, type_env: TypeEnv<'hir>) {
        self.envs.borrow_mut().push(type_env);
    }

    pub fn pop(&self) -> Option<TypeEnv<'hir>> {
        self.envs.borrow_mut().pop()
    }

    pub fn insert(&self, local_var: LocalVar, ty: Ty<'hir>) {
        let mut borrowed_envs = self.envs.borrow_mut();
        let env = borrowed_envs.last_mut().unwrap();
        env.local_vars.insert(local_var.ident, ty);
    }

    pub fn find(&self, local_var: &hir::LocalVar) -> Option<Ty<'hir>> {
        for env in self.envs.borrow().iter().rev() {
            if let Some(ty) = env.local_vars.get(&local_var.ident) {
                return Some(*ty);
            }
        }
        return None;
    }

    /// This represents the ret_ty of the type env, which is always a function return type.
    pub fn root_ty(&self) -> Ty<'hir> {
        self.envs.borrow().first().map(|env| env.ret_ty).unwrap()
    }
}

#[derive(Debug)]
struct TypeEnv<'hir> {
    local_vars: StrMap<Ty<'hir>>,
    ret_ty: Ty<'hir>,
}

impl<'hir> TypeEnv<'hir> {
    pub fn new(ret_ty: Ty<'hir>) -> Self {
        Self {
            local_vars: Default::default(),
            ret_ty,
        }
    }
}

#[derive(Copy, Clone, Debug)]
enum ConstraintEvaluation<'hir> {
    Success,
    NotEqual(Ty<'hir>, Ty<'hir>),
    NotAssignable(Ty<'hir>, Ty<'hir>),
    NotCallable(Ty<'hir>),
    MissingArg(Field<'hir>),
    ExtraArg(Ty<'hir>),
    InfixOpUnsupported(Ty<'hir>, InfixOp),
}

impl<'hir> ConstraintEvaluation<'hir> {
    pub fn and(&self, other: ConstraintEvaluation<'hir>) -> ConstraintEvaluation<'hir> {
        match self {
            ConstraintEvaluation::Success => other,
            _ => *self,
        }
    }
    pub fn is_error(&self) -> bool {
        match self {
            ConstraintEvaluation::Success => false,
            _ => true,
        }
    }
}

#[derive(Debug)]
enum Constraint<'hir> {
    Equal(Ty<'hir>, Ty<'hir>),
    Assignable(Ty<'hir>, Ty<'hir>),
    Callable(Ty<'hir>, Vec<Ty<'hir>>),
    Infix(Ty<'hir>, Ty<'hir>, InfixOp),
}
