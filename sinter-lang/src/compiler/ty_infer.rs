use std::error::Error;
use std::fmt::{Display, Formatter};
use std::sync::Arc;

use itertools::Itertools;
use serde::{Deserialize, Serialize};

use crate::compiler::compiler::CompileError;
use crate::compiler::compiler::CompileError::TypeErrors;
use crate::compiler::hir::{ArrayExpr, Expr, HirCrate, HirNodeKind, LocalDefId, Ty};
use crate::compiler::types::{LDefMap, StrMap};

#[derive(Debug)]
pub enum TypeError {
    TypesNotEqual(Ty, Ty),
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
    ty_map: LDefMap<Ty>,
    krate: &'a mut HirCrate,
}

impl<'a> CrateInference<'a> {
    fn new(krate: &'a mut HirCrate) -> Self {
        Self {
            unify_table: Default::default(),
            ty_map: Default::default(),
            krate,
        }
    }

    fn infer_tys(mut self) -> Result<LDefMap<Ty>, CompileError> {
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
        match &node {
            HirNodeKind::GlobalLet(global_let) => {
                match global_let.ty {
                    Some(ty) => {
                        let ty = self.krate.get_ty(ty).clone();
                        let constraints = self.check(global_let.initializer, ty.clone());
                        (constraints, Type::Concrete(ty))
                    }
                    // We do not need a separate check for built in types since they are not present until after type inference.
                    _ => self.infer(global_let.initializer),
                }
            }
            HirNodeKind::Expr(expr) => match expr {
                Expr::Float(float) => (Constraints::default(), Type::Concrete(Ty::F64)),
                Expr::Integer(int) => (Constraints::default(), Type::Concrete(Ty::I64)),
                _ => (Constraints::default(), self.fresh_ty()),
            },
            _ => (Constraints::default(), self.fresh_ty()),
        }
    }

    fn check(&mut self, node_id: LocalDefId, ty: Ty) -> Constraints {
        enum Op {
            Check(LocalDefId, Ty),
            CheckMultiple(Arc<[LocalDefId]>, Ty),
            None,
            Infer(LocalDefId, Ty),
        }

        let node = self.krate.get_node(node_id);
        let op = match (node, ty) {
            (
                HirNodeKind::Expr(Expr::Array(ArrayExpr::Unsized { initializers })),
                Ty::Array { ty },
            ) => {
                let ty = self.krate.get_ty(ty).clone();
                let exprs_to_check = initializers.clone();
                Op::CheckMultiple(exprs_to_check, ty)
            }
            (
                HirNodeKind::Expr(Expr::Array(ArrayExpr::Sized { initializer, size })),
                Ty::Array { ty },
            ) => {
                let ty = self.krate.get_ty(ty).clone();
                Op::Check(*initializer, ty)
            }
            (HirNodeKind::Expr(Expr::Float(float)), Ty::F32 | Ty::F64) => Op::None,
            (HirNodeKind::Expr(Expr::Integer(int)), Ty::I64) => Op::None,
            // TODO: Cover other explicit cases
            // TODO: Implement fallthrough case that asserts the types are equal
            (node, ty) => Op::Infer(node_id, ty),
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
                constraints.push(Constraint::Equal(Type::Concrete(ty), inferred_ty));
                constraints
            }
        }
    }

    fn unify(&mut self, constraints: Constraints) -> Result<(), TypeError> {
        for constr in constraints {
            match constr {
                Constraint::Equal(lhs, rhs) => self.unify_ty_ty(lhs, rhs)?,
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
            (Type::Infer(unknown), Type::Concrete(ty))
            | (Type::Concrete(ty), Type::Infer(unknown)) => {
                // TODO:  Check for cyclic type
                self.unify_table.unify_var_ty(unknown, ty)
            }
            // If both types are known, then we need to check for type compatibility.
            (Type::Concrete(lhs), Type::Concrete(rhs)) => {
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
                    Some(ty) => self.normalize_ty(Type::Concrete(ty)),
                    None => Type::Infer(ty_var),
                }
            }
            _ => ty,
        }
    }

    fn substitute(&mut self, node_id: LocalDefId, ty: Type) {
        let node = self.krate.get_node_mut(node_id);
        let substituted_ty = match ty {
            Type::Infer(ty_var) => self.unify_table.probe(ty_var).unwrap(),
            Type::Concrete(ty) => ty,
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
    value: Option<Ty>,
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

    fn unify_var_ty(&mut self, var: TyVar, ty: Ty) -> Result<(), TypeError> {
        let root = self.get_root_key(var);
        if self.entry(root).value == Some(ty) {
            Ok(())
        } else {
            Err(TypeError::UnificationError)
        }
    }

    fn probe(&mut self, key: TyVar) -> Option<Ty> {
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
}

#[derive(Copy, Clone, Eq, PartialEq, Serialize, Deserialize)]
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

enum Type {
    Infer(TyVar),
    Concrete(Ty),
}

mod tests {
    use std::collections::HashMap;

    use crate::compiler::compiler::{Application, CompileError, Compiler, CompilerCtxt};
    use crate::compiler::hir::{Expr, GlobalLetStmt, HirCrate, HirNode, HirNodeKind, Ty};
    use crate::compiler::krate::CrateId;
    use crate::compiler::tokens::tokenized_file::Span;
    use crate::compiler::ty_infer::{CrateInference, TypeInference};
    use crate::compiler::types::{InternedStr, StrMap};
    use crate::compiler::StringInterner;
    use crate::util::utils;

    type TypedResult = (StringInterner, StrMap<HirCrate>);

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
    pub fn infer_crate_tys() {
        let mut compiler_ctxt = CompilerCtxt::default();

        let initializer_id = compiler_ctxt.local_def_id();
        let global_let_id = compiler_ctxt.local_def_id();

        let global_let = HirNode::new(
            HirNodeKind::GlobalLet(GlobalLetStmt::new(
                InternedStr::default(),
                None,
                initializer_id,
            )),
            Span::default(),
            global_let_id,
        );

        let initializer = HirNode::new(
            HirNodeKind::Expr(Expr::Integer(123)),
            Span::default(),
            initializer_id,
        );

        let mut krate = HirCrate::new(
            InternedStr::default(),
            CrateId::default(),
            vec![global_let_id],
            HashMap::from([(global_let_id, global_let), (initializer_id, initializer)]),
        );

        let crate_inference = CrateInference::new(&mut krate);
        match crate_inference.infer_tys() {
            Err(_) => panic!(),
            Ok(map) => {
                let ty = map.get(&global_let_id);
                match ty {
                    Some(&Ty::I64) => {}
                    _ => panic!(),
                }
            }
        }
    }
}
