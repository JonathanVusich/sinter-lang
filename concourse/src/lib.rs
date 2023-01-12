#![feature(strict_provenance_atomic_ptr)]

use std::hint;
use std::sync::atomic::{AtomicBool, AtomicUsize};
use std::sync::atomic::Ordering::{Acquire, SeqCst};
use std::sync::{Mutex, RwLock};

const DEFAULT_CAPACITY: usize = 512;

pub struct Concourse<T> {
    buffer: RwLock<Buffer<T>>,
    buffers: Mutex<Vec<Buffer<T>>>,
    full: AtomicBool,
}

impl<T> Concourse<T> {

    pub fn new() -> Self {
        Self {
            buffer: RwLock::new(Buffer::with_capacity(DEFAULT_CAPACITY)),
            buffers: Mutex::new(Vec::new()),
            full: AtomicBool::new(false),
        }
    }

    pub fn alloc(&self, val: T) -> &T {
        let buffer = self.buffer.read().unwrap();
        match buffer.alloc() {
            Some(dest) => {
                self.store(val, dest)
            },
            None => {
                if let Ok(false) = self.full.compare_exchange(false, true, SeqCst, SeqCst) {
                    drop(buffer);
                    let mut buffer = self.buffer.write().unwrap();
                    let mut new_buffer = Buffer::with_capacity(DEFAULT_CAPACITY);
                    std::mem::swap(&mut *buffer, &mut new_buffer);
                    self.buffers.lock().unwrap().push(new_buffer);
                } else {
                    while self.full.load(Acquire) {
                        hint::spin_loop();
                    }
                }
                self.alloc(val)
            }
        }
    }

    #[inline(always)]
    fn store(&self, val: T, dest: *mut T) -> &T {
        unsafe {
            dest.write(val);
            &mut *dest
        }
    }
}

impl<T> Default for Concourse<T> {
    fn default() -> Self {
        Concourse::new()
    }
}

struct Buffer<T> {
    buffer: *mut T,
    counter: AtomicUsize,
    capacity: usize,
}

impl<T> Buffer<T> {
    fn with_capacity(capacity: usize) -> Self {
        let buffer = Vec::<T>::with_capacity(capacity).as_mut_ptr();
        Self {
            buffer,
            counter: AtomicUsize::new(0),
            capacity,
        }
    }

    fn alloc(&self) -> Option<*mut T> {
        let pos = self.counter.fetch_add(1, SeqCst);
        if pos < self.capacity {
            let dest = unsafe { self.buffer.add(pos) };
            Some(dest)
        } else {
            None
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn allocate_single() {
        let concourse = Concourse::new();
        let reference = concourse.alloc(123);
        assert_eq!(123, *reference);
    }

    #[test]
    fn sanity_check() {
        let concourse = Concourse::new();
        for i in 0..1000 {
            let reference = concourse.alloc(i);
            assert_eq!(i, *reference);
        }
    }
}
