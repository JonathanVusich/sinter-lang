use std::collections::HashSet;
use std::error::Error;
use std::fmt::{Display, Formatter};

use anyhow::Result;

use crate::compiler::ast::{BlockStmt, ClassStmt, DeclaredType, EnumMemberStmt, EnumStmt, Field, FnStmt, GenericParam, GenericParams, LetStmt, Module, OuterStmt, Param, Stmt, TraitStmt};
use crate::compiler::compiler::CompilerCtxt;
use crate::compiler::resolver::ResolutionError::{DuplicateEnumMember, DuplicateFieldName, DuplicateFnName, DuplicateGenericParam, DuplicateParam, DuplicateTyDecl, DuplicateVarDecl};
use crate::compiler::types::types::InternedStr;

#[derive(Debug)]
pub enum ResolutionError {
    DuplicateVarDecl(InternedStr),
    DuplicateTyDecl(InternedStr),
    DuplicateGenericParam(InternedStr),
    DuplicateParam(InternedStr),
    DuplicateFieldName(InternedStr),
    DuplicateEnumMember(InternedStr),
    DuplicateFnName(InternedStr),
}

impl Display for ResolutionError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl Error for ResolutionError {}

pub fn check_names(ctxt: CompilerCtxt, module: Module) -> Result<(CompilerCtxt, Module)> {
    let mut module_env = ModuleEnv::new();

    for stmt in module.stmts() {
        check_outer_stmt(&mut module_env, stmt)?;
    }
    todo!()
}

fn check_block_stmt(module_env: &mut ModuleEnv, block_stmt: &BlockStmt) -> Result<()> {
    module_env.begin_scope();
    for stmt in &block_stmt.stmts {
        check_stmt(module_env, stmt)?;
    }
    module_env.end_scope();
    Ok(())
}

fn check_outer_stmt(module_env: &mut ModuleEnv, stmt: &OuterStmt) -> Result<()> {
    match stmt {
        OuterStmt::Let(let_stmt) => check_let_stmt(module_env, let_stmt),
        OuterStmt::Class(class_stmt) => check_class_stmt(module_env, class_stmt),
        OuterStmt::Enum(enum_stmt) => check_enum_stmt(module_env, enum_stmt),
        OuterStmt::Trait(trait_stmt) => check_trait_stmt(module_env, trait_stmt),
        OuterStmt::TraitImpl(trait_impl_stmt) => check_trait_impl_stmt(module_env, trait_impl_stmt),
        OuterStmt::Fn(fn_stmt) => check_fn_stmt(module_env, fn_stmt),
        _ => {}
    }
}

fn check_stmt(module_env: &mut ModuleEnv, stmt: &Stmt) -> Result<()> {
    todo!()
}

fn check_let_stmt(module_env: &mut ModuleEnv, let_stmt: &LetStmt) -> Result<()> {
    if !module_env.add_var(let_stmt.ident) {
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
    check_fields(module_env, &class_stmt.fields)?;
    check_member_fns(module_env, &class_stmt.member_fns)?;

    module_env.end_scope();
    module_env.clear_class_fields();
    Ok(())
}

fn check_enum_stmt(module_env: &mut ModuleEnv, enum_stmt: &EnumStmt) -> Result<()> {
    module_env.begin_scope();
    if !module_env.add_ty_name(enum_stmt.name) {
        return Err(DuplicateTyDecl(enum_stmt.name).into());
    }

    check_generic_params(module_env, &enum_stmt.generic_params)?;
    check_enum_members(module_env, &enum_stmt.members)?;
    check_member_fns(module_env, &enum_stmt.member_fns)?;

    module_env.end_scope();
    module_env.clear_class_fields();
    Ok(())
}

fn check_trait_stmt(module_env: &mut ModuleEnv, trait_stmt: &TraitStmt) -> Result<()> {
    module_env.begin_scope();
    if !module_env.add_ty_name(trait_stmt.name) {
        return Err(DuplicateTyDecl(trait_stmt.name).into());
    }

    check_generic_params(module_env, &trait_stmt.generic_params)?;
    check_member_fns(module_env, &trait_stmt.member_fns)?;

    module_env.end_scope();
    module_env.clear_member_fns();

    Ok(())
}

fn check_member_fns(module_env: &mut ModuleEnv, fns: &[FnStmt]) -> Result<()> {
    for fn_stmt in fns {
        if !module_env.add_member_fn(fn_stmt.sig.ident) {
            return Err(DuplicateFnName(fn_stmt.sig.ident).into());
        }

        module_env.begin_scope();


        check_params(module_env, &fn_stmt.sig.params)?;
        check_generic_params(module_env, &fn_stmt.sig.generic_params, )?;
        if let Some(block_stmt) = &fn_stmt.body {
            check_block_stmt(module_env, &block_stmt)?;
        }
    }
    Ok(())
}

fn check_fields(module_env: &mut ModuleEnv, params: &[Field]) -> Result<()> {
    for param in params {
        if !module_env.add_field(param.ident) {
            return Err(DuplicateFieldName(param.ident).into());
        }
    }
    Ok(())
}

fn check_params(module_env: &mut ModuleEnv, params: &[Param]) -> Result<()> {
    for param in params {
        if !module_env.add_var(param.ident) {
            return Err(DuplicateParam(param.ident).into());
        }
    }
    Ok(())
}

fn check_enum_members(module_env: &mut ModuleEnv, members: &[EnumMemberStmt]) -> Result<()> {
    for member in members {
        if !module_env.add_enum_member(member.name) {
            return Err(DuplicateEnumMember(member.name).into());
        }
        check_fields(module_env, &member.fields)?;

        check_member_fns(module_env, &member.member_fns)?;

        module_env.clear_class_fields();
    }

    module_env.clear_enum_members();
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
    class_fields: HashSet<InternedStr>,
    member_fns: HashSet<InternedStr>,
    enum_members: HashSet<InternedStr>,
}

impl ModuleEnv {
    fn new() -> Self {
        Self {
            type_names: HashSet::default(),
            var_scopes: vec![HashSet::default()],
            generic_scopes: vec![HashSet::default()],
            class_fields: HashSet::default(),
            member_fns: HashSet::default(),
            enum_members: HashSet::default(),
        }
    }

    fn add_ty_name(&mut self, name: InternedStr) -> bool {
        if self.type_names.contains(&name) {
            return false;
        }
        self.type_names.insert(name);
        return true;
    }

    fn add_var(&mut self, name: InternedStr) -> bool {
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

    fn add_field(&mut self, field: InternedStr) -> bool {
        if self.class_fields.contains(&field) {
            return false;
        }
        self.class_fields.insert(field);
        true
    }

    fn add_member_fn(&mut self, function: InternedStr) -> bool {
        if self.member_fns.contains(&function) {
            return false;
        }
        self.member_fns.insert(function);
        true
    }

    fn add_enum_member(&mut self, member: InternedStr) -> bool {
        if self.class_fields.contains(&member) {
            return false;
        }
        self.class_fields.insert(member);
        true
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

    fn clear_class_fields(&mut self) {
        self.class_fields.clear();
    }

    fn clear_member_fns(&mut self) {
        self.member_fns.clear();
    }

    fn clear_enum_members(&mut self) {
        self.enum_members.clear();
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
        // let string = InternedStr::default();
        // let mut environment = ModuleEnv::new();
        //
        // assert!(!environment.var_name_exists(string));
        // environment.add_var(string);
        // assert!(environment.var_name_exists(string));
        //
        // environment.begin_scope();
        // assert!(environment.var_name_exists(string));
        //
        // let second_string = InternedStr::try_from_usize(1).unwrap();
        //
        // assert!(!environment.var_name_exists(second_string));
        // environment.add_var(second_string);
        // assert!(environment.var_name_exists(second_string));
        //
        // environment.end_scope();
        // assert!(environment.var_name_exists(string));
        // assert!(!environment.var_name_exists(second_string));
    }

    #[test]
    #[should_panic]
    pub fn popping_top_scope() {
        let mut environment = ModuleEnv::new();
        environment.end_scope();
    }
}
