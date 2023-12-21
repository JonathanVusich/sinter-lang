use serde::{Deserialize, Serialize};
use crate::compiler::type_inference::ty_infer::{Type, TypeErrKind};

#[derive(Default, Debug)]
pub(crate) struct UnificationTable {
    table: Vec<Entry>,
}

#[derive(Debug)]
struct Entry {
    parent: TyVar,
    value: Option<Type>,
}

impl Entry {
    pub fn new(parent: TyVar) -> Self {
        Self {
            parent,
            value: None,
        }
    }
}

impl UnificationTable {
    pub(crate) fn unify_var_var(&mut self, lhs: TyVar, rhs: TyVar) -> Result<(), TypeErrKind> {
        let lhs = self.get_root_key(lhs);
        let rhs = self.get_root_key(rhs);

        // They are already unified, return early.
        if lhs == rhs {
            Ok(())
        } else {
            Err(TypeErrKind::UnificationError)
        }
    }

    pub(crate) fn unify_var_ty(&mut self, var: TyVar, ty: Type) -> Result<(), TypeErrKind> {
        let root = self.get_root_key(var);
        let entry = self.entry(root);
        match &entry.value {
            None => {
                entry.value = Some(ty);
                Ok(())
            }
            Some(prev_ty) => {
                if prev_ty != &ty {
                    return Err(TypeErrKind::UnificationError);
                }
                Ok(())
            }
        }
    }

    pub(crate) fn probe(&mut self, key: TyVar) -> Option<Type> {
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

    pub(crate) fn entry(&mut self, key: TyVar) -> &mut Entry {
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
