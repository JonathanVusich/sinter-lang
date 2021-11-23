use crate::class::class::Class;

pub struct ClassLoader {
    classes: Vec<Class>
}

impl ClassLoader {

    pub fn classes(&self) -> &Vec<Class> {
        &self.classes
    }
}