#[cfg(loom)]
pub (crate) use loom::sync::{Mutex, RwLock};

#[cfg(not(loom))]
pub (crate) use std::sync::{Mutex, RwLock};

pub (crate) mod atomic {

    #[cfg(loom)]
    pub (crate) use loom::sync::atomic::{AtomicUsize, AtomicBool};

    #[cfg(not(loom))]
    pub (crate) use std::sync::atomic::{AtomicUsize, AtomicBool};
}
