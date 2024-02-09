use std::sync::Arc;

use indexmap::{IndexMap, IndexSet};
use multimap::MultiMap;

use id::{DefId, LocalDefId};
use interner::InternedStr;

pub(crate) type StrSet = IndexSet<InternedStr>;

pub(crate) type StrMap<T> = IndexMap<InternedStr, T>;
pub(crate) type IStrMap<T> = Arc<StrMap<T>>;
pub(crate) type IMultiMap<K, V> = Arc<MultiMap<K, V>>;
pub(crate) type LDefMap<T> = IndexMap<LocalDefId, T>;
pub(crate) type DefMap<T> = IndexMap<DefId, T>;
