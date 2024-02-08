use std::cell::{Cell, RefCell};
use std::cmp::max;
use std::mem::MaybeUninit;
use std::ptr::NonNull;
use std::{mem, ptr, slice};

const DEFAULT_SIZE: usize = 256;
const GROW_FACTOR: usize = 2;

pub struct Arena<T> {
    ptr: Cell<*mut T>,
    end: Cell<*mut T>,
    chunks: RefCell<Vec<Chunk<T>>>,
}

impl<T> Default for Arena<T> {
    fn default() -> Self {
        Self {
            ptr: Cell::new(ptr::null_mut()),
            end: Cell::new(ptr::null_mut()),
            chunks: Default::default(),
        }
    }
}

impl<T> Arena<T> {
    pub fn alloc(&self, object: T) -> &mut T {
        if self.ptr == self.end {
            self.grow(1);
        }
        let ptr = self.ptr.get();
        unsafe {
            self.ptr.set(ptr.add(1));
            ptr.write(object);
            &mut *ptr
        }
    }

    pub fn alloc_slice(&self, objects: &[T]) -> &mut [T] {
        if !self.can_alloc(objects.len()) {
            self.grow(objects.len());
        }

        let ptr = self.ptr.get();
        unsafe {
            self.ptr.set(ptr.add(objects.len()));
            objects.as_ptr().copy_to_nonoverlapping(ptr, objects.len());
            slice::from_raw_parts_mut(ptr, objects.len())
        }
    }

    fn can_alloc(&self, num: usize) -> bool {
        let remaining_entries = unsafe { self.end.get().sub_ptr(self.ptr.get()) };
        return remaining_entries >= num;
    }

    fn grow(&self, min: usize) {
        let min_chunk_size = min.next_power_of_two();
        let mut chunks = self.chunks.borrow_mut();
        let next_chunk_size = if let Some(chunk) = chunks.last_mut() {
            let chunk_len = unsafe { chunk.end().sub_ptr(chunk.start()) };
            let num_entries = unsafe { self.end.get().sub_ptr(self.ptr.get()) };
            chunk.entries = num_entries;

            chunk_len * GROW_FACTOR
        } else {
            DEFAULT_SIZE
        };
        let chunk_size = max(min_chunk_size, next_chunk_size);
        let mut chunk = Chunk::new(chunk_size);

        self.ptr.set(chunk.start());
        self.end.set(chunk.end());
        chunks.push(chunk);
    }
}

struct Chunk<T> {
    chunk: NonNull<[MaybeUninit<T>]>,
    entries: usize,
}

impl<T> Chunk<T> {
    fn new(len: usize) -> Self {
        let chunk = NonNull::from(Box::leak(Box::new_uninit_slice(len)));
        Self { chunk, entries: 0 }
    }

    fn start(&mut self) -> *mut T {
        self.chunk.as_ptr() as *mut T
    }

    fn end(&mut self) -> *mut T {
        let len = self.chunk.len();
        unsafe { (self.chunk.as_ptr() as *mut T).add(len) }
    }
}

impl<T> Drop for Chunk<T> {
    fn drop(&mut self) {
        if mem::needs_drop::<T>() {
            unsafe {
                let slice = self.chunk.as_mut();
                ptr::drop_in_place(MaybeUninit::slice_assume_init_mut(
                    &mut slice[..self.entries],
                ))
            }
        }
    }
}

mod tests {
    use crate::util::arena::Arena;
    use std::mem;

    #[test]
    pub fn alloc() {
        let arena = Arena::default();
        for x in 0..512 {
            let ptr = arena.alloc(x);
            assert_eq!(*ptr, x);
        }
    }

    #[test]
    pub fn alloc_slice() {
        let arena = Arena::default();
        let slice: Vec<usize> = (0..512).into_iter().collect();
        let interned_slice = arena.alloc_slice(&slice);

        drop(slice);
        for x in 0..512 {
            let interned_val = interned_slice[x];
            assert_eq!(x, interned_val);
        }
    }
}
