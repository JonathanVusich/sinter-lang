use std::collections::HashSet;
use std::error::Error;
use std::fmt::{Display, Formatter};
use crate::compiler::ast::{BlockStmt, ClassStmt, DeclaredType, FnStmt, GenericParam, GenericParams, IntoStr, LetStmt, Module, Param, Stmt};
use crate::compiler::compiler::CompilerCtxt;
use anyhow::Result;
use crate::compiler::resolver::ResolutionError::{DuplicateFnName, DuplicateGenericParam, DuplicateParam, DuplicateTyDecl, DuplicateVarDecl};
use crate::compiler::types::types::InternedStr;

#[derive(Debug)]
pub enum ResolutionError {
    DuplicateVarDecl(InternedStr),
    DuplicateTyDecl(InternedStr),
    DuplicateGenericParam(InternedStr),
    DuplicateParam(InternedStr),
    DuplicateFnName(InternedStr),
}

impl Display for ResolutionError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl Error for ResolutionError {}

pub fn check_names(ctxt: CompilerCtxt, module: Module) -> Result<(CompilerCtxt, Module)> {
    let mut environment = ModuleEnv::new();

    for stmt in module.stmts() {
        check_stmt(&mut environment, stmt)?;
    }
    todo!()
}

fn check_block_stmt(environment: &mut ModuleEnv, block_stmt: &BlockStmt) -> Result<()> {
    environment.begin_scope();
    for stmt in &block_stmt.stmts {
        check_stmt(environment, stmt)?;
    }
    environment.end_scope();
    Ok(())
}

fn check_stmt(environment: &mut ModuleEnv, stmt: &Stmt) -> Result<()> {
    match stmt {
        Stmt::Let(let_stmt) => check_let_stmt(environment, let_stmt),
        Stmt::Class(class_stmt) => check_class_stmt(environment, class_stmt),
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
    Ok(())
}

fn check_let_stmt(module_env: &mut ModuleEnv, let_stmt: &LetStmt) -> Result<()> {
    if !module_env.add_var_name(let_stmt.ident) {
        return Err(DuplicateVarDecl(let_stmt.ident).into());
    }
    Ok(())
}

fn check_class_stmt(module_env: &mut ModuleEnv, class_stmt: &ClassStmt) -> Result<()> {
    module_env.begin_scope();
    if !module_env.add_ty_name(class_stmt.name) {
        return Err(DuplicateTyDecl(class_stmt.name).into());
    }

    check_generic_params(module_env, &class_stmt.generic_params)?;
    check_params(module_env, &class_stmt.members)?;
    check_fns(module_env, &class_stmt.member_fns)?;

    module_env.end_scope();
    Ok(())
}

fn check_fns(module_env: &mut ModuleEnv, fns: &[FnStmt]) -> Result<()> {
    let mut used_names = HashSet::new();
    for fn_stmt in fns {
        // Check to ensure names are unique
        if used_names.contains(&fn_stmt.sig.ident) {
            return Err(DuplicateFnName(fn_stmt.sig.ident).into());
        } else {
            used_names.insert(fn_stmt.sig.ident);
        }

        check_params(&fn_stmt.sig.params)?;
        check_generic_params(&fn_stmt.sig.generic_params, )?;
        if let Some(block_stmt) = &fn_stmt.body {
            check_block_stmt(module_env, &block_stmt)?;
        }
    }
    Ok(())
}

fn check_params(module_env: &mut ModuleEnv, params: &[Param]) -> Result<()> {
    for param in params {
        if used_params.contains(&param.ident) {
            return Err(DuplicateParam(param.ident).into());
        } else {
            used_params.insert(&param.ident);
        }
    }
    Ok(())
}

fn check_generic_params(module_env: &mut ModuleEnv, generic_params: &[GenericParam]) -> Result<()> {
    for param in generic_params {
        if !module_env.add_generic(param.ident) {
            return Err(DuplicateGenericParam(param.ident).into());
        }
    }
    Ok(())
}

struct ModuleEnv {
    type_names: HashSet<InternedStr>,
    var_scopes: Vec<HashSet<InternedStr>>,
    generic_scopes: Vec<HashSet<InternedStr>>,
}

impl ModuleEnv {
    fn new() -> Self {
        Self {
            type_names: HashSet::default(),
            var_scopes: vec![HashSet::default()],
            generic_scopes: vec![HashSet::default()],
        }
    }

    fn add_ty_name(&mut self, name: InternedStr) -> bool {
        if self.type_names.contains(&name) {
            return false;
        }
        self.type_names.insert(name);
        return true;
    }

    fn add_var_name(&mut self, name: InternedStr) -> bool {
        for scope in self.var_scopes.iter().rev() {
            if scope.contains(&name) {
                return false;
            }
        }

        self.var_scopes.last_mut().unwrap().insert(name);
        return true;
    }

    fn add_generic(&mut self, name: InternedStr) -> bool {
        for scope in self.generic_scopes.iter().rev() {
            if scope.contains(&name) {
                return false;
            }
        }
        self.generic_scopes.last_mut().unwrap().insert(name);
        return true;
    }

    fn begin_scope(&mut self) {
        self.var_scopes.push(HashSet::new());
        self.generic_scopes.push(HashSet::new());
    }

    fn end_scope(&mut self) {
        if self.var_scopes.len() < 2 || self.generic_scopes.len() < 2 {
            panic!()
        }
        self.var_scopes.pop();
        self.generic_scopes.pop();
    }
}

struct ClassEnv {
    generic_params: HashSet<InternedStr>,
    function_names: HashSet<InternedStr>,

}

mod tests {
    use lasso::Key;
    use crate::compiler::resolver::ModuleEnv;
    use crate::compiler::types::types::InternedStr;

    #[test]
    pub fn var_scoping() {
        let string = InternedStr::default();
        let mut environment = ModuleEnv::new();

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
        let mut environment = ModuleEnv::new();
        environment.end_scope();
    }
}
