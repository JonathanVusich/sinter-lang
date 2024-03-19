use crate::Ty;
use serde::{Deserialize, Serialize};

#[derive(Default, Debug)]
pub(crate) struct UnificationTable<'a> {
    table: Vec<Entry<'a>>,
}

#[derive(Debug)]
struct Entry<'a> {
    parent: TyVar,
    value: Option<Ty<'a>>,
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
    pub(crate) fn unify_var_var(&mut self, lhs: TyVar, rhs: TyVar) -> bool {
        let lhs = self.get_root_key(lhs);
        let rhs = self.get_root_key(rhs);

        lhs == rhs
    }

    pub(crate) fn unify_var_ty<'b, F: Fn(&Ty<'b>, &Ty<'b>) -> bool>(
        &mut self,
        var: TyVar,
        ty: Ty<'b>,
        assignable_check: F,
    ) -> bool {
        let root = self.get_root_key(var);
        let entry = self.entry(root);
        match &entry.value {
            None => {
                entry.value = Some(ty);
                true
            }
            Some(prev_ty) => assignable_check(prev_ty, &ty),
        }
    }

    pub(crate) fn probe(&mut self, key: TyVar) -> Option<Ty<'a>> {
        let root_key = self.get_root_key(key);
        self.entry(root_key).value.clone()
    }

    pub(crate) fn get_root_key(&mut self, key: TyVar) -> TyVar {
        let entry = self.entry(key);
        if entry.parent == key {
            return key;
        }

        let redirect = entry.parent;
        let root = self.get_root_key(key);

        if root != redirect {
            // Compress the paths
            self.entry(key).parent = root;
        }

        root
    }

    fn entry(&mut self, key: TyVar) -> &mut Entry {
        &mut self.table[key.id as usize]
    }

    // Creates a self-referential index ptr into the vec.
    pub(crate) fn fresh_ty(&mut self) -> TyVar {
        let index = self.table.len();
        let key = TyVar::new(index as u32);
        self.table.push(Entry::new(key));
        key
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Serialize, Deserialize)]
#[repr(transparent)]
#[serde(transparent)]
pub struct TyVar {
    id: u32,
}

impl TyVar {
    pub(crate) fn new(id: u32) -> Self {
        Self { id }
    }
}
