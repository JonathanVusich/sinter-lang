use crate::compiler::hir::{HirCrate, HirNodeKind, Ty};

pub struct TyInfer<'a> {
    unify_table: UnificationTable,
    krate: &'a HirCrate,
}

impl<'a> TyInfer<'a> {
    pub fn new(krate: &'a HirCrate) -> Self {
        Self {
            unify_table: Default::default(),
            krate,
        }
    }

    pub fn new_ty_var(&mut self) -> TyVar {
        todo!()
    }

    pub fn infer(&mut self, node: &HirNodeKind) -> ! {
        todo!()
    }

    pub fn check(&mut self, node: &HirNodeKind, ty: Ty) -> ! {
        todo!()
    }
}

#[derive(Default)]
struct UnificationTable {}

enum Constraint {
    Equal(Ty, Ty),
}

pub struct TyVar {
    id: u32,
}
