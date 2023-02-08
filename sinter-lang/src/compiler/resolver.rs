use std::collections::HashSet;
use std::error::Error;
use std::fmt::{Display, Formatter};
use crate::compiler::ast::{Module, Stmt};
use crate::compiler::compiler::CompilerCtxt;
use anyhow::Result;
use crate::compiler::resolver::ResolutionError::{DuplicateTyDecl, DuplicateVarDecl};
use crate::compiler::types::types::InternedStr;

pub struct ResolvedModule {

}

#[derive(Debug)]
pub enum ResolutionError {
    DuplicateVarDecl(InternedStr),
    DuplicateTyDecl(InternedStr),
}

impl Display for ResolutionError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl Error for ResolutionError {}

pub fn resolve(ctxt: CompilerCtxt, module: Module) -> Result<(CompilerCtxt, ResolvedModule)> {
    let mut environment = Environment::new();

    for stmt in module.stmts() {
        match stmt {
            Stmt::Let(let_stmt) => {
                if environment.var_name_exists(let_stmt.ident) {
                    return Err(DuplicateVarDecl(let_stmt.ident).into());
                }
                environment.add_var_name(let_stmt.ident);
            }
            Stmt::Class(class_stmt) => {
                if environment.ty_name_exists(class_stmt.name) {
                    return Err(DuplicateTyDecl(class_stmt.name).into());
                }
                for function in &class_stmt.member_fns {
                    if let Some(block_stmt) = &function.body {
                        environment.begin_scope();
                        for stmt in &block_stmt.stmts {

                        }
                    }

                }
            }
            Stmt::Enum(_) => {}
            Stmt::Trait(_) => {}
            Stmt::TraitImpl(_) => {}
            Stmt::Fn(_) => {}
            Stmt::For(_) => {}
            Stmt::If(_) => {}
            Stmt::Return(_) => {}
            Stmt::While(_) => {}
            Stmt::Block(_) => {}
            Stmt::Expression { .. } => {}
            _ => {}
        }
    }
    todo!()
}

struct Environment {
    type_names: HashSet<InternedStr>,
    var_scopes: Vec<HashSet<InternedStr>>
}

impl Environment {
    fn new() -> Self {
        Self {
            type_names: HashSet::default(),
            var_scopes: vec![HashSet::default()]
        }
    }

    fn ty_name_exists(&self, name: InternedStr) -> bool {
        self.type_names.contains(&name)
    }

    fn add_ty_name(&mut self, name: InternedStr) {
        self.type_names.insert(name);
    }

    fn var_name_exists(&self, name: InternedStr) -> bool {
        for scope in self.var_scopes.iter().rev() {
            if scope.contains(&name) {
                return true;
            }
        }
        false
    }

    fn add_var_name(&mut self, name: InternedStr) {
        self.var_scopes.last_mut().unwrap().insert(name);
    }

    fn begin_scope(&mut self) {
        self.var_scopes.push(HashSet::new());
    }

    fn end_scope(&mut self) {
        if self.var_scopes.len() < 2 {
            panic!()
        }
        self.var_scopes.pop();
    }
}

mod tests {
    use lasso::Key;
    use crate::compiler::resolver::Environment;
    use crate::compiler::types::types::InternedStr;

    #[test]
    pub fn environment() {
        let string = InternedStr::default();
        let mut environment = Environment::new();

        assert!(!environment.var_name_exists(string));
        environment.add_var_name(string);
        assert!(environment.var_name_exists(string));

        environment.begin_scope();
        assert!(environment.var_name_exists(string));

        let second_string = InternedStr::try_from_usize(1).unwrap();

        assert!(!environment.var_name_exists(second_string));
        environment.add_var_name(second_string);
        assert!(environment.var_name_exists(second_string));

        environment.end_scope();
        assert!(environment.var_name_exists(string));
        assert!(!environment.var_name_exists(second_string));
    }

    #[test]
    #[should_panic]
    pub fn popping_top_scope() {
        let mut environment = Environment::new();
        environment.end_scope();
    }
}
