use arena::Arena;
use hir::HirNode;
use ty_infer::TyKind;

#[derive(Debug, Default)]
pub(crate) struct Arenas<'a> {
    pub(crate) hir_nodes: Arena<HirNode<'a>>,
    pub(crate) tys: Arena<TyKind<'a>>,
}
