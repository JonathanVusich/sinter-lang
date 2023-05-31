use crate::compiler::ast::Ast;

pub struct VarResolver<'a> {
    ast: &'a mut Ast,
    used_asts: &'a mut Vec<Ast>,
}

impl<'a> VarResolver<'a> {
    pub fn new(ast: &'a mut Ast, used_asts: &'a mut Vec<Ast>) -> Self {
        Self {
            ast,
            used_asts,
        }
    }
}