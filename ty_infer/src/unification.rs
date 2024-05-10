use crate::TyKind;
use serde::{Deserialize, Serialize};
use std::cell::RefCell;
use typed_hir::TyVar;

#[derive(Default, Debug)]
pub(crate) struct UnificationTable<'a> {
    table: RefCell<InnerTable<'a>>,
}

#[derive(Default, Debug)]
struct InnerTable<'a> {
    table: Vec<Entry<'a>>,
}

impl<'a> InnerTable<'a> {
    fn entry(&self, ty_var: TyVar) -> &Entry<'a> {
        &self.table[ty_var.id as usize]
    }

    fn entry_mut(&mut self, ty_var: TyVar) -> &mut Entry<'a> {
        &mut self.table[ty_var.id as usize]
    }

    fn push(&mut self, entry: Entry<'a>) {
        self.table.push(entry);
    }

    fn len(&self) -> usize {
        self.table.len()
    }
}

#[derive(Copy, Clone, Debug)]
struct Entry<'a> {
    parent: TyVar,
    value: Option<&'a TyKind<'a>>,
}

impl<'a> Entry<'a> {
    pub fn new(parent: TyVar) -> Self {
        Self {
            parent,
            value: None,
        }
    }
}

impl<'a> UnificationTable<'a> {
    pub(crate) fn unify_var_var(&self, lhs: TyVar, rhs: TyVar) -> bool {
        let lhs = self.get_root_key(lhs);
        let rhs = self.get_root_key(rhs);

        lhs == rhs
    }

    pub(crate) fn unify_var_ty<F: Fn(&'a TyKind<'a>, &'a TyKind<'a>) -> bool>(
        &self,
        var: TyVar,
        ty: &'a TyKind<'_>,
        assignable_check: F,
    ) -> bool {
        let root = self.get_root_key(var);
        let mut table = self.table.borrow_mut();
        let entry = table.entry_mut(root);
        match &entry.value {
            None => {
                entry.value = Some(ty);
                true
            }
            Some(prev_ty) => assignable_check(prev_ty, &ty),
        }
    }

    pub(crate) fn probe(&self, key: TyVar) -> Option<&'a TyKind<'a>> {
        let root_key = self.get_root_key(key);
        self.table.borrow().entry(root_key).value.clone()
    }

    fn get_root_key(&self, key: TyVar) -> TyVar {
        let parent = self.table.borrow().entry(key).parent;

        if parent == key {
            return key;
        }

        let redirect = parent;
        let root = self.get_root_key(key);

        if root != redirect {
            // Compress the paths
            self.table.borrow_mut().entry_mut(key).parent = root;
        }

        root
    }

    // Creates a self-referential index ptr into the vec.
    pub(crate) fn fresh_ty(&mut self) -> TyVar {
        let index = self.table.borrow().len();
        let key = TyVar { id: index as u32 };
        self.table.borrow_mut().push(Entry::new(key));
        key
    }
}
