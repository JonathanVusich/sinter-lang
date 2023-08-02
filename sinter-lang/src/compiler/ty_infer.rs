use crate::compiler::hir::{HirCrate, Ty};

pub struct TyInfer {
    unify_table: UnificationTable,
}

impl TyInfer {
    pub fn new_ty_var(&mut self) -> TyVar {
        todo!()
    }

    pub fn infer(&mut self, krate: &HirCrate) -> ! {
        todo!()
    }

    pub fn check(&mut self, krate: &HirCrate) -> ! {
        todo!()
    }
}

struct UnificationTable {}

enum Constraint {
    Equal(Ty, Ty),
}

pub struct TyVar {}
