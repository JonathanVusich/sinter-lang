use crate::compiler::ast::{PathTy, QualifiedIdent, TraitBound};
use lasso::Spur;
use serde::{Deserialize, Serialize};
use crate::compiler::interner::Key;

pub type InternedStr = Spur;
pub type InternedTy = Key;

#[derive(Clone, Eq, PartialEq, Debug, Hash, Serialize, Deserialize)]
pub enum Type {
    Array {
        ty: Key
    },
    Path {
        path: PathTy,
    },
    Union {
        tys: Vec<Key>,
    },
    TraitBound {
        trait_bound: TraitBound,
    },
    Closure {
        params: Vec<Key>,
        ret_ty: Key
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