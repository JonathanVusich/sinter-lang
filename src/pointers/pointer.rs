use std::ops::{Deref, DerefMut};
use std::ptr::NonNull;
use crate::class::class::Class;
use crate::pointers::heap_pointer::HeapPointer;
use crate::util::constants::WORD;

#[derive(Debug)]
#[repr(transparent)]
pub struct Pointer<T> {
    /// The underlying raw pointer.
    pointer: *mut T
}

impl<T> Pointer<T> {
    pub fn new(value: &T) -> Self {
        Self { pointer: value as *const T as *mut T }
    }

    pub fn from_raw(ptr: *mut T) -> Self {
        Self { pointer: ptr as _ }
    }

    pub fn to_raw(&self) -> *mut T {
        self.pointer
    }

    pub fn add(&self, offset: usize) -> Self {
        unsafe { Self { pointer: self.pointer.add(offset) } }
    }

    pub fn offset(&self, offset: isize) -> Self {
        unsafe { Self { pointer: self.pointer.offset(offset) } }
    }

    pub fn cast<U>(&self) -> Pointer<U> {
        Pointer { pointer: self.pointer.cast::<U>() }
    }

    pub fn read(&self) -> T {
        unsafe { self.pointer.read() }
    }

    pub fn write(&self, value: T) {
        unsafe { self.pointer.write(value) }
    }
}

impl<T> From<Pointer<T>> for [u8; WORD] {

    fn from(pointer: Pointer<T>) -> Self {
        (pointer.pointer as usize).to_ne_bytes().try_into().unwrap()
    }
}

impl<T> From<Pointer<T>> for usize {

    fn from(pointer: Pointer<T>) -> Self {
        pointer.pointer as usize
    }
}

impl<T> From<[u8; WORD]> for Pointer<T> {

    fn from(bytes: [u8; WORD]) -> Pointer<T> {
        Pointer { pointer: usize::from_ne_bytes(bytes) as *mut T }
    }
}

impl<T> From<Pointer<T>> for NonNull<T> {

    fn from(pointer: Pointer<T>) -> Self {
        unsafe { NonNull::new_unchecked(pointer.pointer) }
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

        assert_eq!(ptr1, ptr2);
    }
}