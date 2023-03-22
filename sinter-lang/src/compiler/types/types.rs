use crate::compiler::ast::{PathTy, QualifiedIdent, TraitBound};
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

#[derive(Clone, Eq, PartialEq, Debug, Hash, Serialize, Deserialize)]
pub enum Type {
    Array {
        ty: InternedTy
    },
    Path {
        path: PathTy,
    },
    Union {
        tys: Vec<InternedTy>,
    },
    TraitBound {
        trait_bound: TraitBound,
    },
    Closure {
        params: Vec<InternedTy>,
        ret_ty: InternedTy
    },
    Infer,
    QSelf,
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
    F32,
    F64,
    Str,
    None
}