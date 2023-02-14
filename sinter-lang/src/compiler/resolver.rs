use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::{mem, vec};

use anyhow::Result;

use crate::compiler::ast::{ArrayExpr, BlockStmt, ClassStmt, DeclaredType, EnumMemberStmt, EnumStmt, Expr, Field, FnStmt, ForStmt, GenericParam, GenericParams, IfStmt, LetStmt, Module, OuterStmt, Param, PathExpr, PathSegment, Pattern, QualifiedIdent, ReturnStmt, Stmt, TraitImplStmt, TraitStmt, WhileStmt};
use crate::compiler::ast::OuterStmt::Use;
use crate::compiler::compiler::CompilerCtxt;
use crate::compiler::interner::Key;
use crate::compiler::resolver::ResolutionError::{DuplicateEnumMember, DuplicateFieldName, DuplicateFnName, DuplicateGenericParam, DuplicateParam, DuplicateTyDecl, DuplicateUseStmt, DuplicateVarDecl};
use crate::compiler::types::types::{InternedStr, InternedTy, Type};

pub fn check_names(ctxt: CompilerCtxt, module: Module) -> Result<(CompilerCtxt, Module)> {
    let resolver = Resolver::new(ctxt);
    resolver.resolve(module)
}

struct ResolvedModule {
    stmts: Vec<OuterStmt>,
}


struct Resolver {
    module_env: ModuleEnv,
    ctxt: CompilerCtxt,
}

#[derive(Debug)]
pub enum ResolutionError {
    DuplicateVarDecl(InternedStr),
    DuplicateTyDecl(InternedStr),
    DuplicateGenericParam(InternedStr),
    DuplicateParam(InternedStr),
    DuplicateFieldName(InternedStr),
    DuplicateEnumMember(InternedStr),
    DuplicateFnName(InternedStr),
    DuplicateUseStmt(QualifiedIdent),
}

impl Display for ResolutionError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "Resolution error!")
    }
}

impl Error for ResolutionError {}


impl Resolver {

    fn new(ctxt: CompilerCtxt) -> Self {
        Self {
            module_env: ModuleEnv::new(),
            ctxt,
        }
    }

    fn resolve(mut self, module: Module) -> Result<(CompilerCtxt, Module)> {
        for stmt in module.stmts() {
            if let Use(use_stmt) = stmt {
                if !self.module_env.add_module_use(use_stmt.ident.clone()) {
                    return Err(DuplicateUseStmt(use_stmt.ident.clone()).into());
                }
            }
        }
        for stmt in module.stmts() {
            self.check_outer_stmt(stmt)?;
        }
        Ok((self.ctxt, module))
    }

    fn check_block_stmt(&mut self, block_stmt: &BlockStmt) -> Result<()> {
        self.module_env.begin_scope();
        for stmt in &block_stmt.stmts {
            self.check_stmt(stmt)?;
        }
        self.module_env.end_scope();
        Ok(())
    }

    fn check_outer_stmt(&mut self, stmt: &OuterStmt) -> Result<()> {
        match stmt {
            OuterStmt::Let(let_stmt) => self.check_let_stmt(let_stmt),
            OuterStmt::Class(class_stmt) => self.check_class_stmt(class_stmt),
            OuterStmt::Enum(enum_stmt) => self.check_enum_stmt(enum_stmt),
            OuterStmt::Trait(trait_stmt) => self.check_trait_stmt(trait_stmt),
            OuterStmt::TraitImpl(trait_impl_stmt) => self.check_trait_impl_stmt(trait_impl_stmt),
            OuterStmt::Fn(fn_stmt) => self.check_fn_stmt(fn_stmt),
            // We have already scanned all use statements, no further action necessary
            Use(_) => Ok(())
        }
    }

    fn check_stmt(&mut self, stmt: &Stmt) -> Result<()> {
        match stmt {
            Stmt::Let(let_stmt) => self.check_let_stmt(let_stmt),
            Stmt::For(for_stmt) => self.check_for_stmt(for_stmt),
            Stmt::If(if_stmt) => self.check_if_stmt(if_stmt),
            Stmt::Return(return_stmt) => self.check_return_stmt(return_stmt),
            Stmt::While(while_stmt) => self.check_while_stmt(while_stmt),
            Stmt::Block(block_stmt) => self.check_block_stmt(block_stmt),
            Stmt::Expression { expr, implicit_return } => self.check_expr(expr),
        }
    }

    fn check_let_stmt(&mut self, let_stmt: &LetStmt) -> Result<()> {
        if !self.module_env.add_var(let_stmt.ident) {
            return Err(DuplicateVarDecl(let_stmt.ident).into());
        }
        if let Some(expr) = &let_stmt.initializer {
            self.check_expr(expr)?;
        }

        Ok(())
    }

    fn check_for_stmt(&mut self, for_stmt: &ForStmt) -> Result<()> {
        self.module_env.begin_scope();

        // Add for loop identifier
        if !self.module_env.add_var(for_stmt.ident) {
            return Err(DuplicateVarDecl(for_stmt.ident).into());
        }
        self.check_expr(&for_stmt.range)?;
        self.check_block_stmt(&for_stmt.body)?;

        self.module_env.end_scope();
        Ok(())
    }

    fn check_if_stmt(&mut self, if_stmt: &IfStmt) -> Result<()> {
        self.check_expr(&if_stmt.condition)?;
        self.check_block_stmt(&if_stmt.if_true)?;

        if let Some(block_stmt) = &if_stmt.if_false {
            self.check_block_stmt(block_stmt)?;
        }
        Ok(())
    }

    fn check_return_stmt(&mut self, return_stmt: &ReturnStmt) -> Result<()> {
        if let Some(return_val) = &return_stmt.value {
            self.check_expr(return_val)?;
        }
        Ok(())
    }

    fn check_while_stmt(&mut self, while_stmt: &WhileStmt) -> Result<()> {
        self.check_expr(&while_stmt.condition)?;
        self.check_block_stmt(&while_stmt.block_stmt)?;
        Ok(())
    }

    fn check_expr(&mut self, expr: &Expr) -> Result<()> {
        self.module_env.begin_scope();
        match expr {
            Expr::Array(array_expr) => {
                match &**array_expr {
                    ArrayExpr::SizedInitializer(initializer, size) => {
                        self.check_expr(initializer)?;
                        self.check_expr(size)?;
                    }
                    ArrayExpr::Initializer(initializers) => {
                        for initializer in initializers {
                            self.check_expr(initializer)?;
                        }
                    }
                }
            }
            Expr::Call(call) => {
                self.check_expr(&call.func)?;
                for arg in &*call.args {
                    self.check_expr(arg)?;
                }
            }
            Expr::Infix(infix) => {
                self.check_expr(&infix.lhs)?;
                self.check_expr(&infix.rhs)?;
            }
            Expr::Unary(unary) => {
                self.check_expr(&unary.expr)?;
            }
            Expr::Match(match_expr) => {
                self.check_expr(&match_expr.source)?;
                for arm in &match_expr.arms {
                    self.check_pattern(&arm.pattern)?;
                    self.check_stmt(&arm.body)?;
                }
            }
            Expr::Closure(closure) => {
                for param in &closure.params {
                    if !self.module_env.add_var(*param) {
                        return Err(DuplicateVarDecl(*param).into());
                    }
                }
                self.check_stmt(&closure.stmt)?;
            }
            Expr::Assign(assign) => {
                self.check_expr(&assign.lhs)?;
                self.check_expr(&assign.rhs)?;
            }
            Expr::Field(field) => {
                self.check_expr(&field.lhs)?;
            }
            Expr::Index(index) => {
                self.check_expr(&index.expr)?;
                self.check_expr(&index.key)?;
            }
            Expr::Path(path) => {
                self.check_path(path)?;
            }
            Expr::None | Expr::Boolean(_) | Expr::Integer(_) | Expr::Float(_) | Expr::String(_) | Expr::Break | Expr::Continue => { }
        }
        self.module_env.end_scope();
        Ok(())
    }

    fn check_class_stmt(&mut self, class_stmt: &ClassStmt) -> Result<()> {
        self.module_env.begin_scope();
        if !self.module_env.add_ty_name(class_stmt.name) {
            return Err(DuplicateTyDecl(class_stmt.name).into());
        }

        self.check_generic_params(&class_stmt.generic_params)?;
        self.check_fields(&class_stmt.fields)?;
        self.check_member_fns(&class_stmt.member_fns)?;

        self.module_env.end_scope();
        self.module_env.clear_class_fields();
        Ok(())
    }

    fn check_enum_stmt(&mut self, enum_stmt: &EnumStmt) -> Result<()> {
        self.module_env.begin_scope();
        if !self.module_env.add_ty_name(enum_stmt.name) {
            return Err(DuplicateTyDecl(enum_stmt.name).into());
        }

        self.check_generic_params(&enum_stmt.generic_params)?;
        self.check_enum_members(&enum_stmt.members)?;
        self.check_member_fns(&enum_stmt.member_fns)?;

        self.module_env.end_scope();
        self.module_env.clear_class_fields();
        Ok(())
    }

    fn check_trait_stmt(&mut self, trait_stmt: &TraitStmt) -> Result<()> {
        self.module_env.begin_scope();
        if !self.module_env.add_ty_name(trait_stmt.name) {
            return Err(DuplicateTyDecl(trait_stmt.name).into());
        }

        self.check_generic_params(&trait_stmt.generic_params)?;
        self.check_member_fns(&trait_stmt.member_fns)?;

        self.module_env.end_scope();
        self.module_env.clear_member_fns();

        Ok(())
    }

    fn check_trait_impl_stmt(&mut self, trait_impl_stmt: &TraitImplStmt) -> Result<()> {
        self.module_env.begin_scope();

        // TODO: Ensure trait and type are in scope
        self.check_member_fns(&trait_impl_stmt.member_fns)?;

        self.module_env.end_scope();
        self.module_env.clear_member_fns();

        Ok(())
    }

    fn check_fn_stmt(&mut self, fn_stmt: &FnStmt) -> Result<()> {
        if self.module_env.add_fn_name(fn_stmt.sig.name) {
            return Err(DuplicateFnName(fn_stmt.sig.name).into());
        }

        self.module_env.begin_scope();

        self.check_generic_params(&fn_stmt.sig.generic_params)?;
        self.check_params(&fn_stmt.sig.params)?;

        if let Some(block_stmt) = &fn_stmt.body {
            self.check_block_stmt(block_stmt)?;
        }

        self.module_env.end_scope();
        Ok(())
    }

    fn check_member_fns(&mut self, fns: &[FnStmt]) -> Result<()> {
        for fn_stmt in fns {
            if !self.module_env.add_member_fn(fn_stmt.sig.name) {
                return Err(DuplicateFnName(fn_stmt.sig.name).into());
            }

            self.module_env.begin_scope();


            self.check_params(&fn_stmt.sig.params)?;
            self.check_generic_params(&fn_stmt.sig.generic_params, )?;
            if let Some(block_stmt) = &fn_stmt.body {
                self.check_block_stmt(block_stmt)?;
            }
        }
        Ok(())
    }

    fn check_fields(&mut self, params: &[Field]) -> Result<()> {
        for param in params {
            if !self.module_env.add_field(param.ident) {
                return Err(DuplicateFieldName(param.ident).into());
            }
        }
        Ok(())
    }

    fn check_params(&mut self, params: &[Param]) -> Result<()> {
        for param in params {
            if !self.module_env.add_var(param.ident) {
                return Err(DuplicateParam(param.ident).into());
            }
        }
        Ok(())
    }

    fn check_enum_members(&mut self, members: &[EnumMemberStmt]) -> Result<()> {
        for member in members {
            if !self.module_env.add_enum_member(member.name) {
                return Err(DuplicateEnumMember(member.name).into());
            }
            self.check_fields(&member.fields)?;

            self.check_member_fns(&member.member_fns)?;

            self.module_env.clear_class_fields();
        }

        self.module_env.clear_enum_members();
        Ok(())
    }

    fn check_generic_params(&mut self, generic_params: &[GenericParam]) -> Result<()> {
        for param in generic_params {
            if !self.module_env.add_generic(param.ident) {
                return Err(DuplicateGenericParam(param.ident).into());
            }
        }
        Ok(())
    }

    fn check_pattern(&mut self, pattern: &Pattern) -> Result<()> {
        match pattern {
            Pattern::Wildcard | Pattern::Boolean(_) | Pattern::Integer(_) | Pattern::String(_) => Ok(()),
            Pattern::Or(or_pattern) => {
                for pattern in &or_pattern.patterns {
                    self.check_pattern(pattern)?;
                }
                Ok(())
            }
            Pattern::Ty(ty, optional_name) => {
                // Check ty
                self.check_ty(*ty)?;
                if let Some(name) = optional_name && !self.module_env.add_var(*name) {
                    return Err(DuplicateVarDecl(*name).into());
                }
                Ok(())
            }
            Pattern::Destructure(ty, exprs) => {
                self.check_ty(*ty)?;
                for expr in exprs {
                    self.check_expr(expr)?;
                }
                Ok(())
            }
        }
    }

    fn check_ty(&mut self, ty: InternedTy) -> Result<()> {
        let interned_ty = self.ctxt.resolve_ty(ty).clone();
        match interned_ty {
            Type::Array { ty } => {
                self.check_ty(ty)?;
            }
            Type::Path { path } => {
                self.check_qualified_path(&path.ident)?;
                for generic in &path.generics {
                    self.check_ty(*generic)?;
                }
            }

            Type::Union { tys } => {
                for ty in tys {
                    self.check_ty(ty)?;
                }
            }
            Type::TraitBound { trait_bound } => {}
            Type::Closure { params, ret_ty } => {}
            Type::Infer | Type::QSelf | Type::U8 | Type::U16 | Type::U32 | Type::U64 | Type::I8 | Type::I16 | Type::I32 | Type::I64 | Type::F32 | Type::F64 | Type::Str | Type::None => {}
        }
        Ok(())
    }

    fn check_path(&mut self, path: &PathExpr) -> Result<()> {
        let qualified_ident = path.qualified_path();
        if !self.module_env.add_module_use(qualified_ident) {

        }
        todo!()
    }

    fn check_qualified_path(&mut self, path: &QualifiedIdent) -> Result<()> {
        todo!()
    }
}



struct ModuleEnv {
    used_modules: HashMap<InternedStr, QualifiedIdent>,
    type_names: HashSet<InternedStr>,
    fn_names: HashSet<InternedStr>,
    var_scopes: Vec<HashSet<InternedStr>>,
    generic_scopes: Vec<HashSet<InternedStr>>,
    class_fields: HashSet<InternedStr>,
    member_fns: HashSet<InternedStr>,
    enum_members: HashSet<InternedStr>,
}

impl ModuleEnv {
    fn new() -> Self {
        Self {
            used_modules: HashMap::default(),
            type_names: HashSet::default(),
            fn_names: HashSet::default(),
            var_scopes: vec![HashSet::default()],
            generic_scopes: vec![HashSet::default()],
            class_fields: HashSet::default(),
            member_fns: HashSet::default(),
            enum_members: HashSet::default(),
        }
    }

    fn add_module_use(&mut self, module_use: QualifiedIdent) -> bool {
        if self.used_modules.contains_key(&module_use.last()) {
            return false;
        }
        self.used_modules.insert(module_use.last(), module_use);
        true
    }

    fn resolve_module(&mut self, name: PathExpr) -> PathExpr {
        if let Some(module) = self.used_modules.get(&name.first()) {
            name.prefix(module)
        } else {
            name
        }
    }

    fn add_ty_name(&mut self, name: InternedStr) -> bool {
        if self.type_names.contains(&name) {
            return false;
        }
        self.type_names.insert(name);
        true
    }

    fn add_fn_name(&mut self, name: InternedStr) -> bool {
        if self.fn_names.contains(&name) {
            return false;
        }
        self.fn_names.insert(name);
        true
    }

    fn add_var(&mut self, name: InternedStr) -> bool {
        for scope in self.var_scopes.iter().rev() {
            if scope.contains(&name) {
                return false;
            }
        }

        self.var_scopes.last_mut().unwrap().insert(name);
        true
    }

    fn add_generic(&mut self, name: InternedStr) -> bool {
        for scope in self.generic_scopes.iter().rev() {
            if scope.contains(&name) {
                return false;
            }
        }
        self.generic_scopes.last_mut().unwrap().insert(name);
        true
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
