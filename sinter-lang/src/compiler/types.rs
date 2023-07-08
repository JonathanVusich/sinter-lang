use std::collections::HashMap;
use crate::compiler::ast::{PathTy, QualifiedIdent, TraitBound, Ty};
use crate::compiler::interner::Key;
use lasso::Spur;
use serde::{Deserialize, Serialize};
use crate::compiler::hir::LocalDefId;

#[repr(transparent)]
#[derive(Copy, Clone, PartialEq, Eq, Debug, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct InternedStr {
    str: Spur,
}

impl From<InternedStr> for Spur {
    fn from(interned_str: InternedStr) -> Self {
        interned_str.str
    }
}

impl From<Spur> for InternedStr {
    fn from(value: Spur) -> Self {
        InternedStr { str: value }
    }
}

impl InternedStr {
    pub fn new(str: Spur) -> Self {
        Self { str }
    }
}

pub(crate) type StrMap<T> = HashMap<InternedStr, T>;
pub(crate) type LocalDefIdMap<T> = HashMap<LocalDefId, T>;

#[repr(transparent)]
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InternedTy {
    ty: Key,
}

impl From<InternedTy> for Key {
    fn from(interned_ty: InternedTy) -> Self {
        interned_ty.ty
    }
}

impl InternedTy {
    pub fn new(ty: Key) -> Self {
        Self { ty }
    }
}
