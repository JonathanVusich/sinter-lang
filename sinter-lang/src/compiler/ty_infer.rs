use std::backtrace::Backtrace;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::ops::Deref;
use std::sync::Arc;

use itertools::Itertools;
use serde::{Deserialize, Serialize};

use crate::compiler::ast::{InfixOp, UnaryOp};
use crate::compiler::compiler::{CompileError, TypeError};
use crate::compiler::hir::{
    ArrayExpr, DefId, Expr, FnStmts, HirCrate, HirNodeKind, LocalDefId, PathTy, Primitive, Ty,
};
use crate::compiler::types::{LDefMap, StrMap};

#[derive(Debug)]
pub enum TypeErrKind {
    TypesNotEqual(Type, Type),
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
    crates: &'a StrMap<HirCrate>,
}

impl<'a> TypeInference<'a> {
    pub fn new(crates: &'a StrMap<HirCrate>) -> Self {
        Self { crates }
    }

    pub fn infer_tys(self) -> Result<(), CompileError> {
        for krate in self.crates.values() {
            let crate_inference = CrateInference::new(krate);
            crate_inference.infer_tys()?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub struct CrateInference<'a> {
    unify_table: UnificationTable,
    ty_map: LDefMap<Type>,
    errors: Vec<TypeErrKind>,
    krate: &'a HirCrate,
}

impl<'a> CrateInference<'a> {
    pub fn new(krate: &'a HirCrate) -> Self {
        Self {
            unify_table: Default::default(),
            ty_map: Default::default(),
            errors: Default::default(),
            krate,
        }
    }

    pub fn infer_tys(mut self) -> Result<LDefMap<Type>, CompileError> {
        // dbg!(&self);
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
                        let enum_member = self.krate.enum_member(*member);
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
                        self.infer_node(body);
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
            let fn_stmt = self.krate.fn_stmt(*fn_stmt);
            if let Some(body) = fn_stmt.body {
                self.infer_node(body);
            }
        }
    }

    fn infer_node(&mut self, node: LocalDefId) {
        let (constraints, ty) = self.infer(node);
        dbg!(&constraints);
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
                match global_let.ty {
                    Some(ty) => {
                        let ty = Type::from(self.krate, ty);
                        let constraints = self.check(global_let.initializer, ty.clone());
                        (constraints, ty)
                    }
                    // We do not need a separate check for built in types since they are not present until after type inference.
                    _ => {
                        let ty = self.fresh_ty();
                        let (mut constraints, inferred_ty) = self.infer(global_let.initializer);
                        constraints.push(Constraint::Equal(ty.clone(), inferred_ty));
                        (constraints, ty)
                    }
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
                            constraints.push(Constraint::Equal(ty, inferred_ty.clone()));
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
                    let ty = self.fresh_ty();
                    let constraints = vec![Constraint::Fn(ty.clone())];
                    (constraints, ty)
                }
                Expr::Assign(assign) => {
                    let lhs_ty = self.fresh_ty();
                    let rhs_ty = self.fresh_ty();

                    let constraints = vec![Constraint::Equal(lhs_ty.clone(), rhs_ty)];
                    (constraints, lhs_ty)
                }
                Expr::Call(call) => {
                    let (mut constraints, inferred_ty) = self.infer(call.target);
                    constraints.push(Constraint::Fn(inferred_ty.clone()));
                    for initializer in call.args.iter() {
                        let (c, ty) = self.infer(*initializer);
                        constraints.extend(c);
                        constraints.push(Constraint::Equal(ty, inferred_ty.clone()));
                    }
                    (constraints, inferred_ty)
                }
                // Expr::Path(path) => {
                //     // TODO: Handle paths
                // }
                // TODO: Figure out how to handle index expressions
                _ => {
                    dbg!(&expr);
                    (Constraints::default(), self.fresh_ty())
                }
            },
            _ => (Constraints::default(), self.fresh_ty()),
        }
    }

    fn check(&mut self, node_id: LocalDefId, ty: Type) -> Constraints {
        enum Op {
            Check(LocalDefId, Type),
            CheckMultiple(Arc<[LocalDefId]>, Type),
            None,
            Infer(LocalDefId, Type),
        }

        let node = self.krate.node(node_id);
        let op = match (node, &ty) {
            (
                HirNodeKind::Expr(Expr::Array(ArrayExpr::Unsized { initializers })),
                Type::Array(array),
            ) => {
                let exprs_to_check = initializers.clone();
                Op::CheckMultiple(exprs_to_check, *array.ty.clone())
            }
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
                constraints.push(Constraint::Equal(ty, inferred_ty));
                constraints
            }
        }
    }

    fn unify(&mut self, constraints: Constraints) -> Result<(), TypeErrKind> {
        for constr in constraints {
            match constr {
                Constraint::Equal(lhs, rhs) => self.unify_ty_ty(lhs, rhs)?,
                Constraint::Array(ty) => {}
                Constraint::Infix(lhs, rhs, op) => {}
                Constraint::Unary(_, _) => {}
                Constraint::Fn(_) => {}
            }
        }
        Ok(())
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
                    return Err(TypeErrKind::TypesNotEqual(lhs, rhs));
                }
                Ok(())
            }
        }
    }

    fn normalize_ty(&mut self, ty: Type) -> Type {
        match ty {
            Type::Array(array) => Type::Array(Array::new(self.normalize_ty(*array.ty))),
            // TODO: Normalize the other recursive types
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
            Type::Closure(closure) => {
                let params = closure
                    .params
                    .into_iter()
                    .map(|ty| self.probe_ty(ty))
                    .collect();
                let ret_ty = self.probe_ty(*closure.ret_ty);
                Type::Closure(Closure::new(params, ret_ty))
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
    Array(Type),
    Infix(Type, Type, InfixOp),
    Unary(Type, UnaryOp),
    Fn(Type),
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
pub struct Closure {
    params: Vec<Type>,
    ret_ty: Box<Type>,
}

impl Closure {
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
    Closure(Closure),
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
        let generics = path
            .generics
            .iter()
            .copied()
            .map(|generic| Self::from(krate, generic))
            .collect();
        Self::Path(Path::new(path.definition, generics))
    }

    pub fn from(krate: &HirCrate, ty: LocalDefId) -> Self {
        let ty = krate.ty_stmt(ty);
        match ty {
            Ty::Array { ty } => {
                let inner_ty = Self::from(krate, *ty);
                Self::Array(Array::new(inner_ty))
            }
            Ty::Path { path } => Self::from_path(krate, path),
            Ty::TraitBound { trait_bound } => {
                let traits = trait_bound
                    .iter()
                    .map(|bound| Self::from_path(krate, bound))
                    .collect();
                Self::TraitBound(TraitBound::new(traits))
            }
            Ty::Closure { params, ret_ty } => {
                let params = params
                    .iter()
                    .copied()
                    .map(|param| Self::from(krate, param))
                    .collect();
                let ret_ty = Self::from(krate, *ret_ty);
                Self::Closure(Closure::new(params, ret_ty))
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

    type TypedResult = HirBuilder;

    #[derive(Default)]
    struct HirBuilder {
        ctxt: CompilerCtxt,
        items: Vec<LocalDefId>,
        nodes: LDefMap<HirNode>,
    }

    impl HirBuilder {
        fn ident(&self) -> Ident {
            Ident::new(InternedStr::default(), Span::default())
        }
        fn add(&mut self, hir_node: HirNodeKind) -> LocalDefId {
            let def_id = self.ctxt.local_def_id();
            match &hir_node {
                HirNodeKind::GlobalLet(_)
                | HirNodeKind::Class(_)
                | HirNodeKind::Enum(_)
                | HirNodeKind::Trait(_)
                | HirNodeKind::TraitImpl(_)
                | HirNodeKind::Fn(_) => {
                    self.items.push(def_id);
                }
                _ => {}
            }
            let hir_node = HirNode::new(hir_node, Span::default(), def_id);
            self.nodes.insert(def_id, hir_node);
            def_id
        }

        fn infer_tys(self) -> LDefMap<Type> {
            let mut hir_crate = HirCrate::new(
                InternedStr::default(),
                CrateId::default(),
                self.items,
                self.nodes,
            );
            let crate_infer = CrateInference::new(&mut hir_crate);
            crate_infer.infer_tys().unwrap()
        }
    }

    type InferResult = (StringInterner, HirCrate, LDefMap<Type>);

    #[cfg(test)]
    fn parse_module(code: &str) -> InferResult {
        let mut compiler = Compiler::default();
        let main_crate = utils::resolve_test_krate_path(code);
        let crate_path = main_crate.clone().parent().unwrap();
        let application = Application::Inline { code };
        let mut crates = compiler.parse_crates(application).unwrap();
        compiler.validate_crates(&crates).unwrap();

        let resolved_crates = compiler.resolve_crates(&mut crates).unwrap();
        let tys = compiler.infer_types(&resolved_crates).unwrap();

        let krate = resolved_crates.into_values().exactly_one().unwrap();
        let tys = tys.into_values().exactly_one().unwrap();

        let string_interner = StringInterner::from(CompilerCtxt::from(compiler));
        (string_interner, krate, tys)
    }

    #[test]
    pub fn infer_integer() {
        let mut hir = HirBuilder::default();

        let initializer = hir.add(HirNodeKind::Expr(Expr::Int(123)));
        let global_let = hir.add(HirNodeKind::GlobalLet(GlobalLetStmt::new(
            InternedStr::default(),
            None,
            initializer,
        )));
        let krate = hir.infer_tys();
        assert_eq!(&Type::I64, krate.get(&global_let).unwrap());
    }

    #[test]
    #[should_panic]
    pub fn infer_integer_mismatch() {
        let mut hir = HirBuilder::default();
        let initializer = hir.add(HirNodeKind::Expr(Expr::Int(123)));
        let f64_ty = hir.add(HirNodeKind::Ty(Ty::Primitive(Primitive::F64)));
        let global_let = hir.add(HirNodeKind::GlobalLet(GlobalLetStmt::new(
            InternedStr::default(),
            Some(f64_ty),
            initializer,
        )));
        let krate = hir.infer_tys();
    }

    #[test]
    pub fn infer_array_usize() {
        let mut hir = HirBuilder::default();
        let one = hir.add(HirNodeKind::Expr(Expr::Int(1)));
        let two = hir.add(HirNodeKind::Expr(Expr::Int(-2)));
        let initializer = hir.add(HirNodeKind::Expr(Expr::Array(ArrayExpr::Unsized {
            initializers: vec![one, two].into(),
        })));
        let global_let = hir.add(HirNodeKind::GlobalLet(GlobalLetStmt::new(
            InternedStr::default(),
            None,
            initializer,
        )));

        let types = hir.infer_tys();
        assert_eq!(
            &Type::Array(Array::new(Type::I64)),
            types.get(&global_let).unwrap()
        );
    }

    #[test]
    pub fn infer_generics() {
        let code = r###"
            class Option<T> {
                field: T,
            }

            let option_f64 = Option(1.23);
            let option_str = Option("Hello"); 
        "###;

        let (si, krate, tys) = parse_module(code);
        dbg!(&tys);
    }
}
