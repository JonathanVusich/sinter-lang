#![feature(strict_provenance_atomic_ptr)]

mod sync;

use std::{hint, ptr};
use std::marker::PhantomData;
use std::mem::forget;
use std::sync::atomic::Ordering::{Acquire, SeqCst, Release};
use crate::sync::{Mutex, RwLock};
use crate::sync::atomic::{AtomicUsize, AtomicBool};

const DEFAULT_CAPACITY: usize = 512;

#[derive(Debug)]
pub struct Concourse<'c, T> {
    buffer: RwLock<Buffer<T>>,
    buffers: Mutex<Vec<Buffer<T>>>,
    full: AtomicBool,
    marker: PhantomData<&'c T>,
}

impl<'c, T> Concourse<'c, T> {

    pub fn new() -> Self {
        Self {
            buffer: RwLock::new(Buffer::with_capacity(DEFAULT_CAPACITY)),
            buffers: Mutex::new(Vec::new()),
            full: AtomicBool::new(false),
            marker: PhantomData::default(),
        }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            buffer: RwLock::new(Buffer::with_capacity(capacity)),
            buffers: Mutex::new(Vec::new()),
            full: AtomicBool::new(false),
            marker: PhantomData::default(),
        }

    }

    pub fn alloc(&'c self, val: T) -> &'c T {
        let buffer = self.buffer.read().unwrap();
        let ptr = buffer.alloc();
        match ptr {
            Some(dest) => {
                self.store(val, dest)
            },
            None => {
                drop(buffer);
                if let Ok(false) = self.full.compare_exchange(false, true, SeqCst, SeqCst) {
                    let mut buffer = self.buffer.write().unwrap();
                    let mut new_buffer = Buffer::with_capacity(DEFAULT_CAPACITY);
                    std::mem::swap(&mut *buffer, &mut new_buffer);
                    self.buffers.lock().unwrap().push(new_buffer);
                    self.full.store(false, Release);
                } else {
                    while self.full.load(Acquire) {
                        hint::spin_loop();
                    }
                }
                self.alloc(val)
            }
        }
    }

    pub fn to_vec(&self) -> Vec<T> {
        todo!()
    }

    #[inline(always)]
    fn store(&self, val: T, dest: *mut T) -> &T {
        unsafe {
            ptr::write(dest, val);
            &mut *dest
        }
    }
}

impl<'c, T> Default for Concourse<'c, T> {
    fn default() -> Self {
        Concourse::<T>::new()
    }
}

unsafe impl<'c, T> Send for Concourse<'c, T> { }
unsafe impl<'c, T> Sync for Concourse<'c, T> { }

#[derive(Debug)]
struct Buffer<T> {
    buffer: *mut T,
    counter: AtomicUsize,
    capacity: usize,
}

impl<T> Buffer<T> {
    fn with_capacity(capacity: usize) -> Self {
        let mut buffer = Vec::<T>::with_capacity(capacity);
        let ptr = buffer.as_mut_ptr();
        forget(buffer);
        Self {
            buffer: ptr,
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

impl<T> Drop for Buffer<T> {
    fn drop(&mut self) {
        let vector = unsafe { Vec::from_raw_parts(self.buffer, self.capacity, self.capacity) };
        drop(vector)
    }
}


#[cfg(test)]
mod tests {
    use std::sync::Arc;
    use super::*;

    #[cfg(not(loom))]
    #[test]
    fn allocate_single() {
        let concourse = Concourse::new();
        let reference = concourse.alloc(123);
        assert_eq!(123, *reference);
    }

    #[cfg(not(loom))]
    #[test]
    fn sanity_check() {
        let concourse = Concourse::new();
        for i in 0..1000 {
            let reference = concourse.alloc(i);
            assert_eq!(i, *reference);
        }
    }

    #[test]
    fn concurrent_writes() {
        loom::model(|| {
            let concourse = Arc::new(Concourse::<usize>::new());
            let allocator_1 = concourse.clone();
            let allocator_2 = concourse.clone();
            let allocator_3 = concourse.clone();

            loom::thread::spawn(move || {
                let reference = allocator_1.alloc(128);
                assert_eq!(128, *reference);
            });

            loom::thread::spawn(move || {
                let reference = allocator_2.alloc(256);
                assert_eq!(256, *reference);
            });

            loom::thread::spawn(move || {
                let reference = allocator_3.alloc(512);
                assert_eq!(512, *reference);
            });
        });
    }

    #[test]
    pub fn multithreaded_writes() {
        let concourse = Arc::new(Concourse::<i128>::new());

        let mut join_handles = Vec::new();
        for _ in 0..8 {
            let conc_ref = concourse.clone();
            let thread = std::thread::spawn(move || {
                for i in 0..10_000 {
                    conc_ref.alloc(i);
                }
            });
            join_handles.push(thread);
        }
        join_handles.into_iter().for_each(|handle| handle.join().unwrap());
    }
}
