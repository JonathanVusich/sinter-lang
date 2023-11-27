use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};

use multimap::MultiMap;

use crate::compiler::ast::{
    Block, EnumMember, Field, Fields, FnSelfStmt, FnSig, FnStmt, GenericParam, GenericParams,
    GlobalLetStmt, IdentType, Item, ItemKind, Module, Param, Params, QualifiedIdent, Stmt,
    StmtKind, UseStmt,
};
use crate::compiler::compiler::{CompileError, ValidationError};
use crate::compiler::hir::LocalDefId;
use crate::compiler::krate::Crate;
use crate::compiler::types::{InternedStr, StrMap};
use serde::{Deserialize, Serialize};

const MAX_NUM: usize = 65535;

#[derive(PartialEq, Eq, Debug, Serialize, Deserialize)]
pub enum ValidationErrKind {
    DuplicateUseStmts(QualifiedIdent, Vec<LocalDefId>),
    DuplicateLetStmts(InternedStr, Vec<LocalDefId>),
    DuplicateTys(InternedStr, Vec<LocalDefId>),
    DuplicateFns(InternedStr, Vec<LocalDefId>),
    DuplicateGenericParam(InternedStr, Vec<LocalDefId>),
    DuplicateParam(InternedStr, Vec<LocalDefId>),
    DuplicateField(InternedStr, Vec<LocalDefId>),
    DuplicateEnumMember(InternedStr, Vec<LocalDefId>),
    NoTypeProvided(LocalDefId),
}

impl Display for ValidationErrKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        // TODO: Implement pretty printing
        Debug::fmt(self, f)
    }
}

pub fn validate(crates: &StrMap<Crate>, krate: &Crate, module: &Module) -> Vec<ValidationError> {
    let validator = Validator::default();
    validator.validate(crates, krate, module)
}

#[derive(Default)]
struct Validator {
    use_stmts: MultiMap<QualifiedIdent, LocalDefId>,
    global_lets: MultiMap<InternedStr, LocalDefId>,
    tys: MultiMap<InternedStr, LocalDefId>,
    fns: MultiMap<InternedStr, LocalDefId>,
    errors: Vec<ValidationErrKind>,
}

impl Validator {
    fn validate(
        mut self,
        crates: &StrMap<Crate>,
        krate: &Crate,
        module: &Module,
    ) -> Vec<ValidationError> {
        for item in &module.items {
            match &item.kind {
                ItemKind::Use(use_stmt) => {
                    self.use_stmts.insert(use_stmt.path.clone(), item.id);
                    // TODO: Add check to verify that the imported ty does not clash with an existing ty.
                }
                ItemKind::GlobalLet(global_let_stmt) => {
                    self.global_lets.insert(global_let_stmt.name.ident, item.id)
                }
                ItemKind::Class(class_stmt) => {
                    self.tys.insert(class_stmt.name.ident, item.id);
                    self.validate_generic_params(&class_stmt.generic_params);
                    self.validate_self_fns(&class_stmt.self_fns);
                    self.validate_fields(&class_stmt.fields);
                }
                ItemKind::Enum(enum_stmt) => {
                    self.tys.insert(enum_stmt.name.ident, item.id);
                    self.validate_generic_params(&enum_stmt.generic_params);
                    self.validate_enum_members(&enum_stmt.members);
                    self.validate_self_fns(&enum_stmt.self_fns);
                }
                ItemKind::Trait(trait_stmt) => {
                    self.tys.insert(trait_stmt.name.ident, item.id);
                    self.validate_generic_params(&trait_stmt.generic_params);
                    self.validate_self_fns(&trait_stmt.self_fns);
                }
                ItemKind::TraitImpl(trait_impl) => {}
                ItemKind::Fn(fn_stmt) => {
                    self.fns.insert(fn_stmt.sig.name.ident, item.id);
                    self.validate_generic_params(&fn_stmt.sig.generic_params);
                    self.validate_params(&fn_stmt.sig.params);
                }
            }
        }

        for (path, use_stmts) in self.use_stmts.iter_all() {
            if use_stmts.len() > 1 {
                self.errors.push(ValidationErrKind::DuplicateUseStmts(
                    path.clone(),
                    use_stmts.clone(),
                ));
            }
        }
        for (name, global_lets) in self.global_lets.iter_all() {
            if global_lets.len() > 1 {
                self.errors.push(ValidationErrKind::DuplicateLetStmts(
                    *name,
                    global_lets.clone(),
                ))
            }
        }
        for (name, tys) in self.tys.iter_all() {
            if tys.len() > 1 {
                self.errors
                    .push(ValidationErrKind::DuplicateTys(*name, tys.clone()))
            }
        }
        for (name, fns) in self.fns.iter_all() {
            if fns.len() > 1 {
                self.errors
                    .push(ValidationErrKind::DuplicateFns(*name, fns.clone()))
            }
        }

        self.errors.into_iter().map(ValidationError::from).collect()
    }
    
    fn validate_block(&mut self, block: &Block) {
        for stmt in &block.stmts {
            self.validate_stmt(stmt);
        }
    }
    
    fn validate_stmt(&mut self, stmt: &Stmt) {
        match &stmt.kind {
            StmtKind::Let(let_stmt) => {
                if let_stmt.ty.is_none() && let_stmt.initializer.is_none() {
                    self.errors.push(ValidationErrKind::NoTypeProvided(stmt.id));
                }
            }
            StmtKind::For(_) => {}
            StmtKind::If(_) => {}
            StmtKind::Return(_) => {}
            StmtKind::While(_) => {}
            StmtKind::Block(block) => self.validate_block(block),
            StmtKind::Expression(_) => {}
        }
    }

    fn validate_generic_params(&mut self, generic_params: &GenericParams) {
        self.report_name_clashes(
            generic_params,
            |param| param.name.ident,
            |param| param.id,
            ValidationErrKind::DuplicateGenericParam,
        );
    }

    fn validate_params(&mut self, params: &Params) {
        self.report_name_clashes(
            params,
            |param| param.name.ident,
            |param| param.id,
            ValidationErrKind::DuplicateParam,
        );
    }

    fn validate_self_fns(&mut self, self_fns: &Vec<FnSelfStmt>) {
        self.report_name_clashes(
            self_fns,
            |self_fn| self_fn.sig.name.ident,
            |self_fn| self_fn.id,
            ValidationErrKind::DuplicateFns,
        );

        for self_fn in self_fns {
            self.validate_fn_sig(&self_fn.sig);
        }
    }

    fn validate_fields(&mut self, fields: &Fields) {
        self.report_name_clashes(
            fields,
            |field| field.name.ident,
            |field| field.id,
            ValidationErrKind::DuplicateField,
        );
    }

    fn validate_enum_members(&mut self, members: &Vec<EnumMember>) {
        self.report_name_clashes(
            members,
            |member| member.name,
            |member| member.id,
            ValidationErrKind::DuplicateEnumMember,
        );

        for member in members {
            self.validate_fields(&member.fields);
            self.validate_self_fns(&member.self_fns);
        }
    }

    fn validate_fn_sig(&mut self, fn_sig: &FnSig) {
        self.validate_generic_params(&fn_sig.generic_params);
        self.validate_params(&fn_sig.params);
    }

    fn report_name_clashes<
        T,
        M: Fn(&T) -> InternedStr,
        I: Fn(&T) -> LocalDefId,
        E: Fn(InternedStr, Vec<LocalDefId>) -> ValidationErrKind,
    >(
        &mut self,
        items: &[T],
        mapper: M,
        id: I,
        error: E,
    ) {
        items
            .iter()
            .map(|item| (mapper(item), item))
            .collect::<MultiMap<InternedStr, &T>>()
            .iter_all()
            .filter(|entry| entry.1.len() > 1)
            .map(|entry| {
                let ids = entry.1.iter().map(|item| id(item)).collect();
                error(*entry.0, ids)
            })
            .for_each(|error| self.errors.push(error));
    }
}

mod tests {
    use crate::compiler::compiler::{CompilerCtxt, ValidationError};
    use crate::compiler::hir::ModuleId;
    use crate::compiler::krate::{Crate, CrateId, ModuleMap};
    use crate::compiler::parser::parse;
    use crate::compiler::path::ModulePath;
    use crate::compiler::tokens::tokenizer::tokenize;
    use crate::compiler::types::StrMap;
    use crate::compiler::validator::ValidationErrKind;
    use crate::compiler::{compiler, StringInterner};
    use snap::snapshot;
    use std::collections::HashMap;
    use std::ops::Deref;

    type ValidationOutput = (StringInterner, Vec<ValidationErrKind>);

    #[cfg(test)]
    fn validate<T: AsRef<str>>(code: T) -> ValidationOutput {
        let mut compiler_ctxt = CompilerCtxt::default();
        let tokenized_input = tokenize(&mut compiler_ctxt, code);
        let module = parse(&mut compiler_ctxt, tokenized_input).unwrap();
        let krate_name = compiler_ctxt.intern_str("crate");
        let mut krate = Crate::new(krate_name, CrateId::new(0));
        krate.add_module(
            ModulePath::from_iter([compiler_ctxt.intern_str("module")]),
            module,
        );
        let krates = StrMap::from([(krate_name, krate)]);
        let krate = krates.get(&krate_name).unwrap();
        let module = krate.module(ModuleId::new(0, 0));
        (
            StringInterner::from(compiler_ctxt),
            crate::compiler::validator::validate(&krates, krate, module)
                .into_iter()
                .map(ValidationError::into_inner)
                .collect(),
        )
    }

    #[test]
    #[snapshot]
    pub fn valid_fn() -> ValidationOutput {
        validate("fn valid_fn() { }")
    }

    #[test]
    #[snapshot]
    pub fn fn_with_duplicate_param() -> ValidationOutput {
        validate("fn duplicate_param(param: u32, param: u32) { }")
    }

    #[test]
    #[snapshot]
    pub fn duplicate_generic_params() -> ValidationOutput {
        validate("fn duplicate_generic_params<T, T>() { }")
    }

    #[test]
    #[snapshot]
    pub fn duplicate_class_fields() -> ValidationOutput {
        validate("class Rectangle { width: u64, width: u64 }")
    }

    #[test]
    #[snapshot]
    pub fn duplicate_enum_members() -> ValidationOutput {
        validate("enum Planet { Mercury, Mercury }")
    }

    #[test]
    #[snapshot]
    pub fn duplicate_enum_member_field() -> ValidationOutput {
        validate("enum Planet { Mercury(diameter: u64, diameter: u64), }")
    }

    #[test]
    #[snapshot]
    pub fn duplicate_enum_member_self_fns() -> ValidationOutput {
        validate(
            r#"
            enum Planet {
                Mercury {
                    fn relative_gravity(planet: Planet) { }
                    fn relative_gravity() { }
                }
            }
            "#,
        )
    }
}
