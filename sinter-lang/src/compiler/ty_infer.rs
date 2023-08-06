use itertools::Itertools;

use crate::compiler::hir::{ArrayExpr, Expr, HirCrate, HirNodeKind, Ty};
use crate::compiler::types::StrMap;

pub struct TypeInference<'a> {
    crates: &'a StrMap<HirCrate>,
}

impl<'a> TypeInference<'a> {
    pub fn new(krate: &'a StrMap<HirCrate>) -> Self {
        Self { crates: krate }
    }

    pub fn infer_tys(self) {
        for krate in self.crates.values() {
            let crate_inference = CrateInference::new(krate);
            crate_inference.infer_tys()
        }
    }
}

struct CrateInference<'a> {
    unify_table: UnificationTable,
    krate: &'a HirCrate,
}

impl<'a> CrateInference<'a> {
    fn new(krate: &'a HirCrate) -> Self {
        Self {
            unify_table: Default::default(),
            krate,
        }
    }

    fn infer_tys(mut self) {
        for item in self.krate.iter_items() {
            let node = self.krate.get_node(item);
            let (constraints, ty) = self.infer(node);

            self.unify(constraints)?;

            self.substitute(ty);
        }
    }

    fn infer(&mut self, node: &HirNodeKind) -> (Constraints, Type) {
        match &node {
            HirNodeKind::GlobalLet(global_let) => {
                let initializer = self.krate.get_node(&global_let.initializer);
                match global_let.ty {
                    // REFACTOR: I think this can be abstracted into its own function
                    // We infer if no type was supplied
                    None => self.infer(initializer),
                    // If there is a type, we need to check that the sub expression matches.
                    Some(ty) => {
                        let ty = self.krate.get_ty(&ty);
                        let constraints = self.check(initializer, ty);
                        (constraints, Type::Known(ty.clone()))
                    }
                }
            }
            HirNodeKind::Expr(expr) => match expr {
                Expr::Float(float) => (Constraints::default(), Type::Known(Ty::F64)),
                Expr::Integer(int) => (Constraints::default(), Type::Known(Ty::I64)),
                _ => (Constraints::default(), self.fresh_ty()),
            },
            _ => (Constraints::default(), self.fresh_ty()),
        }
    }

    fn check(&mut self, node: &HirNodeKind, expected_ty: &Ty) -> Constraints {
        match (node, expected_ty) {
            (
                HirNodeKind::Expr(Expr::Array(ArrayExpr::Unsized { initializers })),
                Ty::Array { ty },
            ) => {
                let ty = self.krate.get_ty(ty);
                initializers
                    .iter()
                    .flat_map(|expr| {
                        let expr = self.krate.get_node(expr);
                        self.check(expr, ty)
                    })
                    .collect_vec()
            }
            (
                HirNodeKind::Expr(Expr::Array(ArrayExpr::Sized { initializer, size })),
                Ty::Array { ty },
            ) => {
                let initializer = self.krate.get_node(initializer);
                let size = self.krate.get_node(size);
                let ty = self.krate.get_ty(ty);

                let mut constraints = self.check(initializer, ty);
                constraints.extend(self.check(size, &Ty::I64));
                constraints
            }
            (HirNodeKind::Expr(Expr::Float(float)), Ty::F32 | Ty::F64) => Constraints::default(),
            (HirNodeKind::Expr(Expr::Integer(int)), Ty::I64) => Constraints::default(),
            // TODO: Cover other explicit cases
            // TODO: Implement fallthrough case that asserts the types are equal
            (node, ty) => {
                let (mut constraints, inferred_ty) = self.infer(node);
                constraints.push(Constraint::Equal(Type::Known(ty.clone()), inferred_ty));
                constraints
            }
        }
    }

    fn fresh_ty(&mut self) -> Type {
        Type::Unknown(self.unify_table.fresh_ty())
    }
}

#[derive(Default)]
struct UnificationTable {
    table: Vec<TyVar>,
}

impl UnificationTable {
    // Creates a self-referential index ptr into the vec.
    fn fresh_ty(&mut self) -> TyVar {
        let index = self.table.len();
        let ty_var = TyVar::new(index as u32);
        self.table.push(ty_var);
        ty_var
    }
}

type Constraints = Vec<Constraint>;

enum Constraint {
    Equal(Type, Type),
}

enum Type {
    Known(Ty),
    Unknown(TyVar),
}

#[derive(Copy, Clone)]
#[repr(transparent)]
struct TyVar {
    id: u32,
}

impl TyVar {
    fn new(id: u32) -> Self {
        Self { id }
    }
}

mod tests {
    use lasso::Spur;
    use snap::snapshot;
    use std::collections::HashMap;

    use crate::compiler::compiler::{Application, Compiler, CompilerCtxt};
    use crate::compiler::hir::{Expr, GlobalLetStmt, HirCrate, HirNode, HirNodeKind};
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

        let resolved_crates = compiler.resolve_crates(&mut crates).unwrap();
        TypeInference::new(&resolved_crates).infer_tys();
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

        let krate = HirCrate::new(
            InternedStr::default(),
            CrateId::default(),
            vec![global_let_id],
            HashMap::from([(global_let_id, global_let), (initializer_id, initializer)]),
        );

        let crate_inference = CrateInference::new(&krate);
        crate_inference.infer_tys();
        CrateInference::new(krate).infer_tys();
    }
}
