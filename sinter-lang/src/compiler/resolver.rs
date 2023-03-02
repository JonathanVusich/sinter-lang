use std::{mem, vec};
use std;
use std::collections::{HashMap, HashSet};
use std::env::var;
use std::error::Error;
use std::fmt::{Display, Formatter};

use anyhow::Result;

use crate::compiler::ast::{ArrayExpr, BlockStmt, ClassStmt, ClosureParam, DeclaredType, EnumMemberStmt, EnumStmt, Expr, Field, FnStmt, ForStmt, GenericParam, GenericParams, GlobalLetStmt, IfStmt, LetStmt, Module, OuterStmt, Param, PathExpr, PathTy, Pattern, PatternLocal, QualifiedIdent, ReturnStmt, Segment, Stmt, TraitImplStmt, TraitStmt, UseStmt, WhileStmt};
use crate::compiler::ast::OuterStmt::Use;
use crate::compiler::compiler::CompilerCtxt;
use crate::compiler::interner::Key;
use crate::compiler::resolver::ResolutionError::{DuplicateEnumMember, DuplicateFieldName, DuplicateFnName, DuplicateGenericParam, DuplicateParam, DuplicateTyDecl, DuplicateUseStmt, DuplicateVarDecl};
use crate::compiler::types::types::{InternedStr, InternedTy, Type};

pub fn resolve_module(ctxt: CompilerCtxt, module: Module) -> Result<(CompilerCtxt, Module)> {
    let resolver = Resolver::new(ctxt);
    resolver.resolve(module)
}

struct ResolvedModule {
    pub const_let_stmts: Vec<GlobalLetStmt>,
    pub class_stmts: Vec<ClassStmt>,
    pub enum_stmts: Vec<EnumStmt>,
    pub trait_stmts: Vec<TraitStmt>,
    pub trait_impl_stmts: Vec<TraitImplStmt>,
    pub fn_stmts: Vec<FnStmt>,
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
        for use_stmt in &module.use_stmts {
            self.module_env.add_module_use(use_stmt);
        }
        for stmt in &module.global_let_stmts {
            self.check_global_let_stmt(stmt);
        }
        for stmt in &module.class_stmts {
            self.check_class_stmt(stmt);
        }
        for stmt in &module.enum_stmts {
            self.check_enum_stmt(stmt);
        }
        for stmt in &module.trait_stmts {
            self.check_trait_stmt(stmt);
        }
        for stmt in &module.trait_impl_stmts {
            self.check_trait_impl_stmt(stmt);
        }
        for stmt in &module.fn_stmts {
            self.check_fn_stmt(stmt);
        }
        Ok((self.ctxt, module))
    }

    fn check_block_stmt(&mut self, block_stmt: &BlockStmt) {
        self.module_env.begin_scope();
        for stmt in &block_stmt.stmts {
            self.check_stmt(stmt);
        }
        self.module_env.end_scope();
    }

    fn check_outer_stmt(&mut self, stmt: &OuterStmt) {
        match stmt {
            OuterStmt::GlobalLet(const_let_stmt) => self.check_global_let_stmt(const_let_stmt),
            OuterStmt::Class(class_stmt) => self.check_class_stmt(class_stmt),
            OuterStmt::Enum(enum_stmt) => self.check_enum_stmt(enum_stmt),
            OuterStmt::Trait(trait_stmt) => self.check_trait_stmt(trait_stmt),
            OuterStmt::TraitImpl(trait_impl_stmt) => self.check_trait_impl_stmt(trait_impl_stmt),
            OuterStmt::Fn(fn_stmt) => self.check_fn_stmt(fn_stmt),
            // We have already scanned all use statements, no further action necessary
            Use(_) => {},
        }
    }

    fn check_stmt(&mut self, stmt: &Stmt) {
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

    fn check_global_let_stmt(&mut self, global_let_stmt: &GlobalLetStmt) {
        self.module_env.add_var_decl(global_let_stmt);
        if let Some(ty) = global_let_stmt.ty {
            self.check_ty(ty);
        }
        self.check_expr(&global_let_stmt.initializer);
    }

    fn check_let_stmt(&mut self, let_stmt: &LetStmt) {
        self.module_env.add_var_decl(let_stmt);
        if let Some(ty) = let_stmt.ty {
            self.check_ty(ty);
        }
        if let Some(expr) = &let_stmt.initializer {
            self.check_expr(expr);
        }
    }

    fn check_for_stmt(&mut self, for_stmt: &ForStmt) {
        self.module_env.begin_scope();
        // Add for loop identifier

        self.module_env.add_var_decl(for_stmt);
        self.check_expr(&for_stmt.range);
        self.check_block_stmt(&for_stmt.body);

        self.module_env.end_scope();
    }

    fn check_if_stmt(&mut self, if_stmt: &IfStmt) {
        self.check_expr(&if_stmt.condition);
        self.check_block_stmt(&if_stmt.if_true);

        if let Some(block_stmt) = &if_stmt.if_false {
            self.check_block_stmt(block_stmt);
        }
    }

    fn check_return_stmt(&mut self, return_stmt: &ReturnStmt) {
        if let Some(return_val) = &return_stmt.value {
            self.check_expr(return_val);
        }
    }

    fn check_while_stmt(&mut self, while_stmt: &WhileStmt) {
        self.check_expr(&while_stmt.condition);
        self.check_block_stmt(&while_stmt.block_stmt);
    }

    fn check_expr(&mut self, expr: &Expr) {
        self.module_env.begin_scope();
        match expr {
            Expr::Array(array_expr) => {
                match &**array_expr {
                    ArrayExpr::SizedInitializer(initializer, size) => {
                        self.check_expr(initializer);
                        self.check_expr(size);
                    }
                    ArrayExpr::Initializer(initializers) => {
                        for initializer in initializers {
                            self.check_expr(initializer);
                        }
                    }
                }
            }
            Expr::Call(call) => {
                self.check_expr(&call.func);
                for arg in &*call.args {
                    self.check_expr(arg);
                }
            }
            Expr::Infix(infix) => {
                self.check_expr(&infix.lhs);
                self.check_expr(&infix.rhs);
            }
            Expr::Unary(unary) => {
                self.check_expr(&unary.expr);
            }
            Expr::Match(match_expr) => {
                self.check_expr(&match_expr.source);
                for arm in &match_expr.arms {
                    self.check_pattern(&arm.pattern);
                    self.check_stmt(&arm.body);
                }
            }
            Expr::Closure(closure) => {
                for param in &closure.params {
                    self.module_env.add_var_decl(param);
                }
                self.check_stmt(&closure.stmt);
            }
            Expr::Assign(assign) => {
                self.check_expr(&assign.lhs);
                self.check_expr(&assign.rhs);
            }
            Expr::Field(field) => {
                self.check_expr(&field.lhs);
            }
            Expr::Index(index) => {
                self.check_expr(&index.expr);
                self.check_expr(&index.key);
            }
            Expr::Path(path) => {
                self.check_path(path);
            }
            Expr::None | Expr::Boolean(_) | Expr::Integer(_) | Expr::Float(_) | Expr::String(_) | Expr::Break | Expr::Continue => { }
        }
        self.module_env.end_scope();
    }

    fn check_class_stmt(&mut self, class_stmt: &ClassStmt) {
        self.module_env.begin_scope();
        self.module_env.add_ty_decl(class_stmt);

        self.check_generic_params(&class_stmt.generic_params);
        self.check_fields(&class_stmt.fields);
        self.check_member_fns(&class_stmt.member_fns);

        self.module_env.end_scope();
        self.module_env.clear_class_fields();
    }

    fn check_enum_stmt(&mut self, enum_stmt: &EnumStmt) {
        self.module_env.begin_scope();
        self.module_env.add_ty_decl(enum_stmt);

        self.check_generic_params(&enum_stmt.generic_params);
        self.check_enum_members(&enum_stmt.members);
        self.check_member_fns(&enum_stmt.member_fns);

        self.module_env.end_scope();
        self.module_env.clear_class_fields();
    }

    fn check_trait_stmt(&mut self, trait_stmt: &TraitStmt) {
        self.module_env.begin_scope();
        self.module_env.add_ty_decl(trait_stmt);

        self.check_generic_params(&trait_stmt.generic_params);
        self.check_member_fns(&trait_stmt.member_fns);

        self.module_env.end_scope();
        self.module_env.clear_member_fns();
    }

    fn check_trait_impl_stmt(&mut self, trait_impl_stmt: &TraitImplStmt) {
        self.module_env.begin_scope();

        // TODO: Ensure trait and type are in scope
        self.check_member_fns(&trait_impl_stmt.member_fns);

        self.module_env.end_scope();
        self.module_env.clear_member_fns();
    }

    fn check_fn_stmt(&mut self, fn_stmt: &FnStmt) {
        self.module_env.add_module_fn(fn_stmt);

        self.module_env.begin_scope();

        self.check_generic_params(&fn_stmt.sig.generic_params);
        self.check_params(&fn_stmt.sig.params);

        if let Some(block_stmt) = &fn_stmt.body {
            self.check_block_stmt(block_stmt);
        }

        self.module_env.end_scope();
    }

    fn check_member_fns(&mut self, fns: &[FnStmt]) {
        for fn_stmt in fns {
            self.module_env.add_member_fn(fn_stmt);

            self.module_env.begin_scope();


            self.check_params(&fn_stmt.sig.params);
            self.check_generic_params(&fn_stmt.sig.generic_params);
            if let Some(block_stmt) = &fn_stmt.body {
                self.check_block_stmt(block_stmt);
            }
        }
    }

    fn check_fields(&mut self, fields: &[Field]) {
        for field in fields {
            self.module_env.add_field(field);
        }
    }

    fn check_params(&mut self, params: &[Param]) {
        for param in params {
            self.module_env.add_var_decl(param);
        }
    }

    fn check_enum_members(&mut self, members: &[EnumMemberStmt]) {
        for member in members {
            self.module_env.add_enum_member(member);
            self.check_fields(&member.fields);
            self.check_member_fns(&member.member_fns);

            self.module_env.clear_class_fields();
        }

        self.module_env.clear_enum_members();
    }

    fn check_generic_params(&mut self, generic_params: &[GenericParam]) {
        for param in generic_params {
            self.module_env.add_generic(param);
        }
    }

    fn check_pattern(&mut self, pattern: &Pattern) {
        match pattern {
            Pattern::Wildcard | Pattern::Boolean(_) | Pattern::Integer(_) | Pattern::String(_) => {},
            Pattern::Or(or_pattern) => {
                for pattern in &or_pattern.patterns {
                    self.check_pattern(pattern);
                }
            }
            Pattern::Ty(ty_pat) => {
                // Check ty
                self.check_ty(ty_pat.ty);
                if let Some(name) = &ty_pat.ident {
                    self.module_env.add_var_decl(name);
                }
            }
            Pattern::Destructure(destructure_pat) => {
                self.check_ty(destructure_pat.ty);
                for expr in &destructure_pat.exprs {
                    self.check_expr(expr);
                }
            }
        }
    }

    fn check_ty(&mut self, ty: InternedTy) {
        let interned_ty = self.ctxt.resolve_ty(ty).clone();
        match interned_ty {
            Type::Array { ty } => {
                self.check_ty(ty);
            }
            Type::Path { path } => {
                self.check_qualified_path(&path);
                for generic in &path.generics {
                    self.check_ty(*generic);
                }
            }

            Type::Union { tys } => {
                for ty in tys {
                    self.check_ty(ty);
                }
            }
            Type::TraitBound { trait_bound } => {}
            Type::Closure { params, ret_ty } => {}
            Type::Infer | Type::QSelf | Type::U8 | Type::U16 | Type::U32 | Type::U64 | Type::I8 | Type::I16 | Type::I32 | Type::I64 | Type::F32 | Type::F64 | Type::Str | Type::None => {}
        }
    }

    fn check_path(&mut self, path: &PathExpr) {
        self.module_env.resolve_module(path);
    }

    fn check_qualified_path(&mut self, path: &PathTy) {
        self.module_env.resolve_module(path);
        for generic in &path.generics {
            self.check_ty(*generic);
        }
    }
}



struct ModuleEnv {
    used_modules: HashMap<InternedStr, QualifiedIdent>,
    qualified_modules: HashSet<QualifiedIdent>,

    duplicate_uses: Vec<UseStmt>,
    unknown_uses: Vec<PathKind>,

    module_tys: HashSet<InternedStr>,
    duplicate_tys: Vec<TyKind>,

    module_fns: HashSet<InternedStr>,
    duplicate_fns: Vec<FnStmt>,

    var_scopes: Vec<HashSet<InternedStr>>,
    duplicate_var_decls: Vec<VarDeclKind>,

    generic_scopes: Vec<HashSet<InternedStr>>,
    duplicate_generics: Vec<GenericParam>,
    class_fields: HashSet<InternedStr>,

    duplicate_class_fields: Vec<Field>,
    duplicate_enum_members: Vec<EnumMemberStmt>,
    member_fns: HashSet<InternedStr>,
    duplicate_member_fns: Vec<FnStmt>,
    enum_members: HashSet<InternedStr>,
}

pub enum PathKind {
    Ty(PathTy),
    Expr(PathExpr),
}

pub trait PathDecl {
    fn first(&self) -> InternedStr;
    fn module_path(&self) -> QualifiedIdent;
    fn into(&self) -> PathKind;
}

pub enum TyKind {
    Class(ClassStmt),
    Enum(EnumStmt),
    Trait(TraitStmt),
}

pub trait TyDecl {
    fn ident(&self) -> InternedStr;
    fn into(&self) -> TyKind;
}

pub trait VarDecl {
    fn ident(&self) -> InternedStr;
    fn is_global(&self) -> bool;
    fn into(&self) -> VarDeclKind;
}

pub enum VarDeclKind {
    Global(GlobalLetStmt),
    NonGlobal(LetStmt),
    For(ForStmt),
    Param(Param),
    ClosureParam(ClosureParam),
    PatternLocal(PatternLocal),
}

impl ModuleEnv {
    fn new() -> Self {
        Self {
            used_modules: HashMap::default(),
            qualified_modules: Default::default(),
            duplicate_uses: Vec::new(),
            unknown_uses: Vec::new(),
            module_tys: HashSet::default(),
            duplicate_tys: Vec::new(),
            module_fns: HashSet::default(),
            duplicate_fns: Vec::new(),
            var_scopes: vec![HashSet::default()],
            duplicate_var_decls: Vec::new(),
            generic_scopes: vec![HashSet::default()],
            duplicate_generics: Vec::new(),
            class_fields: HashSet::default(),
            duplicate_class_fields: Vec::new(),
            duplicate_enum_members: Vec::new(),
            member_fns: HashSet::default(),
            duplicate_member_fns: Vec::new(),
            enum_members: HashSet::default(),
        }
    }

    fn add_module_use(&mut self, use_stmt: &UseStmt) {
        if self.used_modules.contains_key(&use_stmt.ident.last()) {
            self.duplicate_uses.push(use_stmt.clone());
        } else {
            self.used_modules.insert(use_stmt.ident.last(), use_stmt.ident.clone());
        }
    }

    fn resolve_module<T: PathDecl>(&mut self, name: &T) {
        if !self.used_modules.contains_key(&name.first()) {
            let module_path = name.module_path();
            if module_path.is_empty() {
                self.unknown_uses.push(name.into());
            } else {
                self.qualified_modules.insert(name.module_path());
            }
        }
    }

    fn add_ty_decl<T: TyDecl>(&mut self, ty_decl: &T) {
        if !self.module_tys.insert(ty_decl.ident()) {
            self.duplicate_tys.push(ty_decl.into());
        }
    }

    fn add_module_fn(&mut self, fn_stmt: &FnStmt) {
        if !self.module_fns.insert(fn_stmt.sig.name) {
            self.duplicate_fns.push(fn_stmt.clone());
        }
    }

    fn add_var_decl<T: VarDecl>(&mut self, var_decl: &T) {
        if var_decl.is_global() {
            if !self.var_scopes[0].insert(var_decl.ident()) {
                self.duplicate_var_decls.push(var_decl.into());
            }
        } else {
            for scope in self.var_scopes.iter().rev() {
                if scope.contains(&var_decl.ident()) {
                    self.duplicate_var_decls.push(var_decl.into());
                    return;
                }
            }
            self.var_scopes.last_mut().unwrap().insert(var_decl.ident());
        }
    }

    fn add_generic(&mut self, param: &GenericParam) {
        for scope in self.generic_scopes.iter().rev() {
            if scope.contains(&param.ident) {
                self.duplicate_generics.push(param.clone());
                return;
            }
        }
        self.generic_scopes.last_mut().unwrap().insert(param.ident);
    }

    fn add_field(&mut self, field: &Field) {
        if !self.class_fields.insert(field.ident) {
            self.duplicate_class_fields.push(field.clone());
        }
    }

    fn add_member_fn(&mut self, function: &FnStmt) {
        if !self.member_fns.insert(function.sig.name) {
            self.duplicate_member_fns.push(function.clone());
        }
    }

    fn add_enum_member(&mut self, member: &EnumMemberStmt) {
        if !self.class_fields.insert(member.name) {
            self.duplicate_enum_members.push(member.clone());
        }
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
