use std::error::Error;
use std::fmt::{Display, Formatter};
use std::ops::Deref;
use std::sync::Arc;

use itertools::Itertools;
use region::page::size;
use serde::{Deserialize, Serialize};

use crate::compiler::ast::{InfixOp, UnaryOp};
use crate::compiler::compiler::CompileError;
use crate::compiler::compiler::CompileError::TypeErrors;
use crate::compiler::hir::{ArrayExpr, DefId, Expr, HirCrate, HirNodeKind, LocalDefId, PathTy, Ty};
use crate::compiler::types::{LDefMap, StrMap};

#[derive(Debug)]
pub enum TypeError {
    TypesNotEqual(Type, Type),
    UnificationError,
    CyclicType,
}

impl Display for TypeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl Error for TypeError {}

pub struct TypeInference<'a> {
    crates: &'a mut StrMap<HirCrate>,
}

impl<'a> TypeInference<'a> {
    pub fn new(crates: &'a mut StrMap<HirCrate>) -> Self {
        Self { crates }
    }

    pub fn infer_tys(self) -> Result<(), CompileError> {
        for krate in self.crates.values_mut() {
            let crate_inference = CrateInference::new(krate);
            crate_inference.infer_tys()?;
        }
        Ok(())
    }
}

struct CrateInference<'a> {
    unify_table: UnificationTable,
    ty_map: LDefMap<Type>,
    krate: &'a HirCrate,
}

impl<'a> CrateInference<'a> {
    fn new(krate: &'a HirCrate) -> Self {
        Self {
            unify_table: Default::default(),
            ty_map: Default::default(),
            krate,
        }
    }

    fn infer_tys(mut self) -> Result<LDefMap<Type>, CompileError> {
        let mut errors = Vec::default();
        let items = self.krate.iter_items().collect_vec();
        for item in items {
            let (constraints, ty) = self.infer(item);

            if let Err(error) = self.unify(constraints) {
                errors.push(error)
            } else {
                self.substitute(item, ty);
            }
        }

        if !errors.is_empty() {
            return Err(TypeErrors(errors));
        }
        Ok(self.ty_map)
    }

    fn infer(&mut self, node: LocalDefId) -> (Constraints, Type) {
        let node = self.krate.get_node(node);
        match node {
            HirNodeKind::GlobalLet(global_let) => {
                match global_let.ty {
                    Some(ty) => {
                        let ty = Type::from(self.krate, ty);
                        let constraints = self.check(global_let.initializer, ty.clone());
                        (constraints, ty)
                    }
                    // We do not need a separate check for built in types since they are not present until after type inference.
                    _ => self.infer(global_let.initializer),
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
                // TODO: Figure out how to handle index expressions
                _ => (Constraints::default(), self.fresh_ty()),
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

        let node = self.krate.get_node(node_id);
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

    fn unify(&mut self, constraints: Constraints) -> Result<(), TypeError> {
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

    fn unify_ty_ty(&mut self, lhs: Type, rhs: Type) -> Result<(), TypeError> {
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
                    return Err(TypeError::TypesNotEqual(lhs, rhs));
                }
                Ok(())
            }
        }
    }

    fn normalize_ty(&mut self, ty: Type) -> Type {
        match ty {
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
        let substituted_ty = match ty {
            Type::Infer(ty_var) => self.unify_table.probe(ty_var).unwrap(),
            ty => ty,
        };
        self.ty_map.insert(node_id, substituted_ty);
    }

    fn fresh_ty(&mut self) -> Type {
        Type::Infer(self.unify_table.fresh_ty())
    }
}

#[derive(Default)]
struct UnificationTable {
    table: Vec<Entry>,
}

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
    fn unify_var_var(&mut self, lhs: TyVar, rhs: TyVar) -> Result<(), TypeError> {
        let lhs = self.get_root_key(lhs);
        let rhs = self.get_root_key(rhs);

        // They are already unified, return early.
        if lhs == rhs {
            Ok(())
        } else {
            Err(TypeError::UnificationError)
        }
    }

    fn unify_var_ty(&mut self, var: TyVar, ty: Type) -> Result<(), TypeError> {
        let root = self.get_root_key(var);
        if self.entry(root).value == Some(ty) {
            Ok(())
        } else {
            Err(TypeError::UnificationError)
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

#[derive(Clone, PartialEq, Eq, Debug)]
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

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Path {
    path: DefId,
    generics: Vec<Type>,
}

impl Path {
    pub fn new(path: DefId, generics: Vec<Type>) -> Self {
        Self { path, generics }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct TraitBound {
    trait_bound: Vec<Type>,
}

impl TraitBound {
    pub fn new(trait_bound: Vec<Type>) -> Self {
        Self { trait_bound }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
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
#[derive(Clone, PartialEq, Eq, Debug)]
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
        let ty = krate.get_ty(ty);
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
            Ty::U8 => Self::U8,
            Ty::U16 => Self::U16,
            Ty::U32 => Self::U32,
            Ty::U64 => Self::U64,
            Ty::I8 => Self::I8,
            Ty::I16 => Self::I16,
            Ty::I32 => Self::I32,
            Ty::I64 => Self::I64,
            Ty::F32 => Self::F32,
            Ty::F64 => Self::F64,
            Ty::Str => Self::Str,
            Ty::Boolean => Self::Boolean,
            Ty::None => Self::None,
        }
    }
}

mod tests {
    use crate::compiler::compiler::{Application, Compiler, CompilerCtxt};
    use crate::compiler::hir::{
        ArrayExpr, Expr, GlobalLetStmt, HirCrate, HirNode, HirNodeKind, LocalDefId, Ty,
    };
    use crate::compiler::krate::CrateId;
    use crate::compiler::tokens::tokenized_file::Span;
    use crate::compiler::ty_infer::{CrateInference, Type, TypeInference};
    use crate::compiler::types::{InternedStr, LDefMap, StrMap};
    use crate::compiler::StringInterner;
    use crate::util::utils;

    type TypedResult = (StringInterner, StrMap<HirCrate>);

    #[derive(Default)]
    struct HirBuilder {
        ctxt: CompilerCtxt,
        items: Vec<LocalDefId>,
        nodes: LDefMap<HirNode>,
    }

    impl HirBuilder {
        fn add(&mut self, hir_node: HirNodeKind) -> LocalDefId {
            let def_id = self.ctxt.local_def_id();
            match &hir_node {
                HirNodeKind::Class(_) | HirNodeKind::Enum(_) | HirNodeKind::Trait(_) => {
                    self.items.push(def_id);
                }
                _ => {}
            }
            let hir_node = HirNode::new(hir_node, Span::default(), def_id);
            self.nodes.insert(def_id, hir_node);
            def_id
        }

        fn build(self) -> HirCrate {
            HirCrate::new(
                InternedStr::default(),
                CrateId::default(),
                self.items,
                self.nodes,
            )
        }
    }

    #[cfg(test)]
    fn infer_types(name: &str) -> TypedResult {
        let mut compiler = Compiler::default();
        let main_crate = utils::resolve_test_krate_path(name);
        let crate_path = main_crate.clone().parent().unwrap();
        let application = Application::new(&main_crate, main_crate.parent().unwrap());
        let mut crates = compiler.parse_crates(&application).unwrap();
        compiler.validate_crates(&crates).unwrap();

        let mut resolved_crates = compiler.resolve_crates(&mut crates).unwrap();
        TypeInference::new(&mut resolved_crates).infer_tys();
        let string_interner = StringInterner::from(CompilerCtxt::from(compiler));
        (string_interner, resolved_crates)
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
        let mut krate = hir.build();
        let crate_inference = CrateInference::new(&mut krate);
        match crate_inference.infer_tys() {
            Err(_) => panic!(),
            Ok(map) => {
                let ty = map.get(&global_let);
                match ty {
                    Some(&Type::I64) => {}
                    _ => panic!(),
                }
            }
        }
    }

    #[test]
    pub fn infer_array_usize() {
        let mut hir = HirBuilder::default();
        let one = hir.add(HirNodeKind::Expr(Expr::Int(1)));
        let two = hir.add(HirNodeKind::Expr(Expr::Int(2)));
        let initializer = hir.add(HirNodeKind::Expr(Expr::Array(ArrayExpr::Unsized {
            initializers: vec![one, two].into(),
        })));
        let global_let = hir.add(HirNodeKind::GlobalLet(GlobalLetStmt::new(
            InternedStr::default(),
            None,
            initializer,
        )));

        let mut krate = hir.build();
        let crate_inference = CrateInference::new(&mut krate);
        match crate_inference.infer_tys() {
            Err(_) => panic!(),
            Ok(map) => {
                let ty = map.get(&global_let);
                match ty {
                    Some(&Type::Array(ref array)) => {}
                    _ => panic!(),
                }
            }
        }
    }
}
