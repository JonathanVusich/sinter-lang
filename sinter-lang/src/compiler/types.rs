use crate::compiler::ast::{PathTy, QualifiedIdent, TraitBound, Ty};
use crate::compiler::hir::LocalDefId;
use crate::compiler::interner::Key;
use lasso::Spur;
use serde::ser::SerializeMap;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::collections::HashMap;

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

pub(crate) type StrMap<T> = HashMap<InternedStr, T>;
pub(crate) type LocalDefIdMap<T> = HashMap<LocalDefId, T>;

pub(crate) trait Named {
    fn name(&self) -> InternedStr;
}

#[derive(Default)]
pub(crate) struct Lut<T: Named> {
    lut: StrMap<usize>,
    index: Vec<T>,
}

impl<T> Serialize for Lut<T>
where
    T: Serialize + Named,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.index.serialize(serializer)
    }
}
impl<'de, T> Deserialize<'de> for Lut<T>
where
    T: Deserialize<'de> + Named,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let index = Vec::<T>::deserialize(deserializer)?;
        let lut = index
            .iter()
            .enumerate()
            .map(|(index, val)| (val.name(), index))
            .collect();

        Ok(Self { lut, index })
    }
}
