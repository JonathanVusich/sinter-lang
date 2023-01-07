use std::collections::{HashMap, HashSet};
use crate::compiler::ast::{ClassStmt, EnumStmt, FnStmt, GenericParam, GenericParams, Module, DeclaredType, Params, Stmt, Param};
use crate::compiler::types::types::InternedStr;

struct TypeChecker<'a> {
    module: &'a Module,
    named_types: HashMap<InternedStr, &'a dyn DeclaredType>,
    named_fns: HashMap<InternedStr, &'a FnStmt>,
    type_errors: Vec<TypeError<'a>>,
}

enum TypeError<'a> {
    DuplicateTypeName(&'a dyn DeclaredType, &'a dyn DeclaredType),
    DuplicateFnName(&'a FnStmt, &'a FnStmt),
    DuplicateParamName(&'a Param, &'a Param),
}


impl<'a> TypeChecker<'a> {
    fn new(module: &'a Module) -> Self {
        Self {
            module,
            named_types: HashMap::default(),
            named_fns: HashMap::default(),
            type_errors: Vec::new(),
        }
    }

    fn check(mut self) {
        for stmt in self.module.stmts() {
            match stmt {
                Stmt::Class(class_stmt) => {
                    self.type_name_shadow_check(&class_stmt);
                    self.unique_generic_params(&class_stmt);
                    self.unique_class_members(&class_stmt);
                    self.unique_member_fns(&class_stmt);
                }
                Stmt::Enum(enum_stmt) => {
                    self.type_name_shadow_check(&enum_stmt);
                    self.unique_generic_params(&enum_stmt);
                    self.unique_member_fns(&enum_stmt);
                }
                Stmt::Trait(trait_stmt) => {
                    self.type_name_shadow_check(&trait_stmt);
                    self.unique_generic_params(&trait_stmt);
                    self.unique_member_fns(&trait_stmt);
                }
                Stmt::Fn(fn_stmt) => {
                    self.fn_name_shadow_check(&fn_stmt);
                    self.unique_fn_params(&fn_stmt);
                }
                Stmt::Use(_) => {}
                Stmt::Let(_) => {}
                Stmt::TraitImpl(_) => {}
                Stmt::For(_) => {}
                Stmt::If(_) => {}
                Stmt::Return(_) => {}
                Stmt::While(_) => {}
                Stmt::Block(_) => {}
                Stmt::Expression { .. } => {}
            }
        }
        todo!()
    }

    fn type_name_shadow_check(&mut self, named_type: &dyn DeclaredType) {
        if let Some(existing_type) = self.named_types.get(&named_type.name()) {
            self.type_errors.push(TypeError::DuplicateTypeName(existing_type, named_type));
        } else {
            self.named_types.insert(named_type.name(), named_type);
        }
    }

    fn fn_name_shadow_check(&mut self, fn_stmt: &FnStmt) {
        if let Some(existing_fn) = self.named_fns.get(&fn_stmt.sig.name) {
            self.type_errors.push(TypeError::DuplicateFnName(existing_fn, &fn_stmt));
        } else {
            self.named_fns.insert(fn_stmt.sig.name, &fn_stmt);
        }
    }

    fn unique_class_members(&mut self, class_stmt: &ClassStmt) {
        let mut class_members = HashMap::new();
        for member in class_stmt.members {
            if let Some(existing_member) = class_members.get(&member.name) {
                self.type_errors.push(TypeError::DuplicateParamName(existing_member, &member));
            } else {
                class_members.insert(member.name, member);
            }
        }
    }

    fn unique_fn_params(&mut self, fn_stmt: &FnStmt) {
        todo!()
    }

    fn unique_member_fns(&mut self, declared_type: &dyn DeclaredType) {
        todo!()
    }

    fn unique_generic_params(&mut self, declared_type: &dyn DeclaredType) {
        let mut declared_params = HashMap::<InternedStr, GenericParam>::new();
        for param in declared_type.generic_params() {
            if let Some(existing_param) = declared_params.get(param.ident) {
                self.type_errors.push(TypeError::DuplicateGenericParam(existing_param, param));
            } else {
                declared_params.insert(param.ident, param);
            }
        }
    }
}

pub fn type_check(module: &Module) -> Vec<TypeError> {
    let type_checker = TypeChecker::new(module);
    type_checker.check();
    type_checker.type_errors
}