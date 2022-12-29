use std::collections::{HashMap, HashSet};
use crate::compiler::ast::{ClassStmt, EnumStmt, FnStmt, GenericParam, GenericParams, Module, DeclaredType, Params, Stmt};
use crate::compiler::types::types::InternedStr;

struct TypeChecker<'a> {
    module: &'a Module,
    named_types: HashMap<InternedStr, &'a dyn DeclaredType>,
    named_fns: HashMap<InternedStr, &'a FnStmt>,
    report: TypeReport<'a>,
}

struct TypeShadow<'a> {
    first: &'a dyn DeclaredType,
    second: &'a dyn DeclaredType,
}

struct FnShadow<'a> {
    first: &'a FnStmt,
    second: &'a FnStmt,
}

impl<'a> TypeChecker<'a> {
    fn new(module: &'a Module) -> Self {
        Self {
            module,
            named_types: HashMap::default(),
            named_fns: HashMap::default(),
            report: TypeReport::default(),
        }
    }

    fn check(mut self) -> TypeReport<'a> {
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
            self.report.add_type_shadow(TypeShadow { first: existing_type, second: named_type });
        } else {
            self.named_types.insert(named_type.name(), named_type);
        }
    }

    fn fn_name_shadow_check(&mut self, fn_stmt: &FnStmt) {
        if let Some(existing_fn) = self.named_fns.get(&fn_stmt.sig.name) {
            self.report.add_fn_shadow(FnShadow { first: existing_fn, second: &fn_stmt });
        } else {
            self.named_fns.insert(fn_stmt.sig.name, &fn_stmt);
        }
    }

    fn unique_class_members(&mut self, class_stmt: &ClassStmt) {
        todo!()
    }

    fn unique_fn_params(&mut self, fn_stmt: &FnStmt) {
        todo!()
    }

    fn unique_member_fns(&mut self, declared_type: &dyn DeclaredType) {
        todo!()
    }

    fn unique_generic_params(&mut self, declared_type: &dyn DeclaredType) {
        todo!()
    }
}

#[derive(Default)]
struct TypeReport<'a> {
    type_shadows: Vec<TypeShadow<'a>>,
    fn_shadows: Vec<FnShadow<'a>>
}

impl<'a> TypeReport<'a> {
    fn new() -> Self {
        Self {
            type_shadows: Vec::new(),
            fn_shadows: Vec::new(),
        }
    }

    fn add_type_shadow(&mut self, type_shadow: TypeShadow) {
        self.type_shadows.push(type_shadow);
    }

    fn add_fn_shadow(&mut self, fn_shadow: FnShadow) {
        self.fn_shadows.push(fn_shadow);
    }
}


pub fn type_check(module: &Module) -> TypeReport {
    let type_checker = TypeChecker::new(module);
    type_checker.check()
}