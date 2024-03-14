use arena::Arena;
use hir::HirNode;
use ty_infer::Ty;

#[derive(Debug, Default)]
pub(crate) struct Arenas<'a> {
    pub(crate) hir_nodes: Arena<HirNode>,
    pub(crate) tys: Arena<Ty<'a>>,
}
