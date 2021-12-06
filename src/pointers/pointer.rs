use std::ops::{Deref, DerefMut};
use crate::class::class::Class;
use crate::pointers::heap_pointer::HeapPointer;
use crate::pointers::tagged_pointer::TaggedPointer;

pub struct Pointer<T> {
    /// The underlying raw pointer.
    pointer: *mut T
}

impl<T> Pointer<T> {
    pub fn new(value: &T) -> Self {
        Self {
            pointer: value as *const T as *mut T,
        }
    }

    pub fn from_raw(ptr: *mut T) -> Self {
        Self {
            pointer: ptr as _
        }
    }

    pub fn add(&self, offset: usize) -> Self {
        let offset_ptr = unsafe { self.pointer.add(offset) };
        Self {
            pointer: offset_ptr
        }
    }

    pub fn cast<U>(&self) -> Pointer<U> {
        let cast_ptr: *mut U = self.pointer.cast();
        Pointer {
            pointer: cast_ptr
        }
    }

    pub fn read(&self) -> T {
        unsafe { self.pointer.read() }
    }

    pub fn write(&self, value: T) {
        unsafe { self.pointer.write(value) }
    }

    pub fn to_raw(&self) -> *mut T {
        self.pointer
    }
}

impl<T> Deref for Pointer<T> {
    type Target = T;

    fn deref(&self) -> &T {
        unsafe { &*self.pointer }
    }
}

impl<T> DerefMut for Pointer<T> {
    fn deref_mut(&mut self) -> &mut T {
        unsafe { &mut *self.pointer }
    }
}

impl<T> Clone for Pointer<T> {
    fn clone(&self) -> Pointer<T> {
        Pointer {
            pointer: self.pointer,
        }
    }
}

impl<T> Copy for Pointer<T> {}

impl<T> PartialEq for Pointer<T> {
    fn eq(&self, other: &Pointer<T>) -> bool {
        self.pointer == other.pointer
    }
}

impl<T> Eq for Pointer<T> {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_deref() {
        let value = "hello";
        let ptr = Pointer::new(&value);

        assert_eq!(ptr.to_uppercase(), "HELLO");
    }

    #[test]
    fn test_deref_mut() {
        let value = "hello".to_string();
        let mut ptr = Pointer::new(&value);

        ptr.push_str(" world");

        assert_eq!(value, "hello world".to_string());
    }

    #[test]
    fn test_eq() {
        let value = "hello".to_string();
        let ptr1 = Pointer::new(&value);
        let ptr2 = Pointer::new(&value);

        assert!(ptr1 == ptr2);
    }
}