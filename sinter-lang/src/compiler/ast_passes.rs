use crate::compiler::ast::{AstPass, Expr, Ident, Item, ItemKind, NodeId, QualifiedIdent};
use std::collections::{HashMap, HashSet};

pub struct UsedModuleCollector {
    pub(crate) modules: HashSet<QualifiedIdent>,
}

impl Default for UsedModuleCollector {
    fn default() -> Self {
        Self {
            modules: HashSet::default(),
        }
    }
}

impl AstPass for UsedModuleCollector {
    fn visit_item(&mut self, node: &mut Item) {
        match &node.kind {
            ItemKind::Use(use_stmt) => {
                self.modules.insert(use_stmt.ident.clone());
            }
            _ => {}
        }
    }

    fn visit_expr(&mut self, expr: &mut Expr) {}
}

pub struct VisibilityCollector {
    constants: HashMap<Ident, NodeId>,
    fns: HashMap<Ident, NodeId>,
    types: HashMap<Ident, NodeId>,
}

impl Default for VisibilityCollector {
    fn default() -> Self {
        Self {
            constants: HashMap::default(),
            fns: HashMap::default(),
            types: HashMap::default(),
        }
    }
}

impl AstPass for VisibilityCollector {
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
