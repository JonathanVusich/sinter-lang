use crate::class::class::Class;

pub struct ClassLoader {
    classes: Vec<Class>
}

impl ClassLoader {
    
    pub fn classes<'a>(&'a self) -> impl Iterator<Item = &Class> + 'a {
        self.classes.iter()
    }
}