use std::sync::Arc;

use indexmap::{IndexMap, IndexSet};
use multimap::MultiMap;

use id::{DefId, LocalDefId};
use interner::InternedStr;

pub type StrSet = IndexSet<InternedStr>;

pub type StrMap<T> = IndexMap<InternedStr, T>;
pub type IStrMap<T> = Arc<StrMap<T>>;
pub type IMultiMap<K, V> = Arc<MultiMap<K, V>>;
pub type LDefMap<T> = IndexMap<LocalDefId, T>;
pub type DefMap<T> = IndexMap<DefId, T>;
