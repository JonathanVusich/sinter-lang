use crate::compiler::ast::{AstPass, Expr, Ident, Item, ItemKind, NodeId, QualifiedIdent, UseStmt};
use crate::compiler::path::ModulePath;
use crate::compiler::types::types::InternedStr;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};

#[derive(PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct UsedCrate {
    pub(crate) crate_name: InternedStr,
    pub(crate) module_path: Vec<InternedStr>,
}

impl UsedCrate {
    pub fn new(crate_name: InternedStr, module_path: Vec<InternedStr>) -> Self {
        Self {
            crate_name,
            module_path,
        }
    }
}

#[derive(Default)]
pub struct UsedCrateCollector {
    pub(crate) used_crates: HashSet<UsedCrate>,
}

impl From<UsedCrateCollector> for HashSet<UsedCrate> {
    fn from(collector: UsedCrateCollector) -> Self {
        collector.used_crates
    }
}

impl AstPass<HashSet<UsedCrate>> for UsedCrateCollector {
    fn visit_item(&mut self, node: &mut Item) {
        if let ItemKind::Use(use_stmt) = &node.kind {
            match use_stmt {
                UseStmt::Global(ident) => {
                    let mut ident = ident.clone();
                    let crate_name = ident.remove(0).ident;
                    let module_path = ident.iter().map(|ident| ident.ident).collect();
                    self.used_crates
                        .insert(UsedCrate::new(crate_name, module_path));
                }
                _ => {}
            };
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
