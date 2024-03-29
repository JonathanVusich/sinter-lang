use std::collections::HashMap;

use crate::pool::internal_string::InternalString;

pub struct StringPool {
    map: HashMap<&'static str, InternalString>,
    vec: Vec<&'static str>,
    buf: String,
    full: Vec<String>,
}

impl StringPool {
    pub fn with_capacity(cap: usize) -> Self {
        let cap = cap.next_power_of_two();
        Self {
            map: HashMap::default(),
            vec: Vec::new(),
            buf: String::with_capacity(cap),
            full: Vec::new(),
        }
    }

    pub fn intern<T: AsRef<str>>(&mut self, name: T) -> InternalString {
        if let Some(&id) = self.map.get(name.as_ref()) {
            return id;
        }
        let name = self.alloc(&name);
        let id = InternalString(self.map.len() as u32);
        self.map.insert(name, id);
        self.vec.push(name);

        debug_assert!(self.lookup(id) == name);
        debug_assert!(self.intern(name) == id);

        id
    }

    pub fn lookup(&self, id: InternalString) -> &'static str {
        self.vec[id.0 as usize]
    }

    fn alloc<T: AsRef<str>>(&mut self, name: T) -> &'static str {
        let str = name.as_ref();
        let cap = self.buf.capacity();
        if cap < self.buf.len() + str.len() {
            let new_cap = (cap.max(str.len()) + 1).next_power_of_two();
            let new_buf = String::with_capacity(new_cap);
            let old_buf = std::mem::replace(&mut self.buf, new_buf);
            self.full.push(old_buf);
        }

        let interned = {
            let start = self.buf.len();
            self.buf.push_str(str);
            &self.buf[start..]
        };

        unsafe { &*(interned as *const str) }
    }
}
