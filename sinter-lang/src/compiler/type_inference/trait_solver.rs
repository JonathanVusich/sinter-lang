use crate::compiler::hir::{Generics, HirCrate, HirNodeKind, PathTy};
use crate::compiler::type_inference::ty_infer::TypeErrKind;
use crate::compiler::types::DefMap;

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
            for item in krate.items.iter() {
                if let HirNodeKind::TraitImpl(trait_impl) = krate.node(item) {
                    let trait_map = impls
                        .entry(trait_impl.target_ty)
                        .or_insert_with(TraitMap::default);
                    let _ = trait_map.add_impl(&trait_impl.trait_to_impl);
                }
            }
        }
        Self { impls }
    }
}
