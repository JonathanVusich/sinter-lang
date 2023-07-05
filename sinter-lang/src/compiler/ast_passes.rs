use std::collections::{HashMap, HashSet};

use serde::{Deserialize, Serialize};

use crate::compiler::ast::{
    AstPass, Expr, Ident, IdentType, Item, ItemKind, QualifiedIdent, UseStmt,
};
use crate::compiler::hir::{HirItem, LocalDefId};
use crate::compiler::path::ModulePath;
use crate::compiler::types::types::InternedStr;

#[derive(PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct UsedCrate {
    pub(crate) crate_name: InternedStr,
    pub(crate) module_path: ModulePath,
}

impl UsedCrate {
    pub fn new(crate_name: InternedStr, module_path: ModulePath) -> Self {
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
    fn visit_item(&mut self, node: &Item) {
        if let ItemKind::Use(use_stmt) = &node.kind {
            match use_stmt.path.ident_type {
                IdentType::Crate => {}
                IdentType::LocalOrUse => {
                    let krate = use_stmt.path.first();
                    let module_path = Into::<ModulePath>::into(&use_stmt.path);
                    self.used_crates
                        .insert(UsedCrate::new(krate.ident, module_path));
                }
            }
        }
    }
}

#[derive(Default)]
pub struct HirVisitor {
    ast: Vec<HirItem>,
}

#[derive(Default)]
pub struct NameCollector {
    constants: HashMap<Ident, LocalDefId>,
    fns: HashMap<Ident, LocalDefId>,
    types: HashMap<Ident, LocalDefId>,
}

impl AstPass<NameCollector> for NameCollector {
    fn visit_item(&mut self, node: &Item) {
        match &node.kind {
            ItemKind::GlobalLet(let_stmt) => {
                self.constants.insert(let_stmt.name, node.id);
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
}

mod tests {}
