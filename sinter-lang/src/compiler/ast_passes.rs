use crate::compiler::ast::{AstPass, Expr, Ident, Item, ItemKind, NodeId, QualifiedIdent};
use std::collections::{HashMap, HashSet};

#[derive(Default)]
pub struct UsedModuleCollector {
    pub(crate) modules: HashSet<QualifiedIdent>,
}

impl From<UsedModuleCollector> for HashSet<QualifiedIdent> {
    fn from(collector: UsedModuleCollector) -> Self {
        collector.modules
    }
}

impl AstPass<HashSet<QualifiedIdent>> for UsedModuleCollector {
    fn visit_item(&mut self, node: &mut Item) {
        if let ItemKind::Use(use_stmt) = &node.kind {
            self.modules.insert(use_stmt.ident.clone());
        }
    }

    fn visit_expr(&mut self, expr: &mut Expr) {}
}

#[derive(Default)]
pub struct VisibilityCollector {
    constants: HashMap<Ident, NodeId>,
    fns: HashMap<Ident, NodeId>,
    types: HashMap<Ident, NodeId>,
}

impl AstPass<VisibilityCollector> for VisibilityCollector {
    fn visit_item(&mut self, node: &mut Item) {
        match &node.kind {
            ItemKind::GlobalLet(let_stmt) => {
                self.constants.insert(let_stmt.ident, node.id);
            }
            ItemKind::Class(class_stmt) => {
                self.types.insert(class_stmt.name, node.id);
            }
            ItemKind::Enum(enum_stmt) => {
                self.types.insert(enum_stmt.name, node.id);
            }
            ItemKind::Trait(trait_stmt) => {
                self.types.insert(trait_stmt.name, node.id);
            }
            ItemKind::Fn(fn_stmt) => {
                self.fns.insert(fn_stmt.sig.name, node.id);
            }
            _ => {}
        }
    }

    fn visit_expr(&mut self, expr: &mut Expr) {}
}

mod tests {}
