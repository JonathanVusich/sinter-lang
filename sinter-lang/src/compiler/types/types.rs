use crate::compiler::ast::{PathTy, QualifiedIdent, TraitBound, Ty};
use lasso::Spur;
use serde::{Deserialize, Serialize};
use crate::compiler::interner::Key;

#[repr(transparent)]
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InternedStr {
    str: Spur,
}

impl From<InternedStr> for Spur {
    fn from(interned_str: InternedStr) -> Self {
        interned_str.str
    }
}

impl InternedStr {
    pub fn new(str: Spur) -> Self {
        Self {
            str,
        }
    }
}

#[repr(transparent)]
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct InternedTy {
    ty: Key
}

impl From<InternedTy> for Key {
    fn from(interned_ty: InternedTy) -> Self {
        interned_ty.ty
    }
}

impl InternedTy {
    pub fn new(ty: Key) -> Self {
        Self {
            ty,
        }
    }
}