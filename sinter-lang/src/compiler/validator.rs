use std::collections::HashMap;

use multimap::MultiMap;

use crate::compiler::ast::{
    Block, EnumMember, Field, Fields, FnSelfStmt, FnSig, FnStmt, GenericParam, GenericParams,
    GlobalLetStmt, IdentType, Item, ItemKind, Module, Param, Params, QualifiedIdent, Stmt,
    StmtKind, UseStmt,
};
use crate::compiler::compiler::CompileError;
use crate::compiler::hir::LocalDefId;
use crate::compiler::krate::Crate;
use crate::compiler::types::{InternedStr, StrMap};
use serde::{Deserialize, Serialize};

const MAX_NUM: usize = 65535;

#[derive(PartialEq, Eq, Debug, Serialize, Deserialize)]
pub enum ValidationError {
    DuplicateUseStmts(QualifiedIdent, Vec<LocalDefId>),
    DuplicateLetStmts(InternedStr, Vec<LocalDefId>),
    DuplicateTys(InternedStr, Vec<LocalDefId>),
    DuplicateFns(InternedStr, Vec<LocalDefId>),
    DuplicateGenericParam(InternedStr, Vec<LocalDefId>),
    DuplicateParam(InternedStr, Vec<LocalDefId>),
    DuplicateField(InternedStr, Vec<LocalDefId>),
    DuplicateEnumMember(InternedStr, Vec<LocalDefId>),
    TooManyFns,
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

    errors: Vec<ValidationError>,
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
                self.errors.push(ValidationError::DuplicateUseStmts(
                    path.clone(),
                    use_stmts.clone(),
                ));
            }
        }
        for (name, global_lets) in self.global_lets.iter_all() {
            if global_lets.len() > 1 {
                self.errors.push(ValidationError::DuplicateLetStmts(
                    *name,
                    global_lets.clone(),
                ))
            }
        }
        for (name, tys) in self.tys.iter_all() {
            if tys.len() > 1 {
                self.errors
                    .push(ValidationError::DuplicateTys(*name, tys.clone()))
            }
        }
        for (name, fns) in self.fns.iter_all() {
            if fns.len() > 1 {
                self.errors
                    .push(ValidationError::DuplicateFns(*name, fns.clone()))
            }
        }

        self.errors
    }

    fn validate_generic_params(&mut self, generic_params: &GenericParams) {
        self.report_name_clashes(
            generic_params,
            |param| param.name.ident,
            |param| param.id,
            ValidationError::DuplicateGenericParam,
        );
    }

    fn validate_params(&mut self, params: &Params) {
        self.report_name_clashes(
            params,
            |param| param.name.ident,
            |param| param.id,
            ValidationError::DuplicateParam,
        );
    }

    fn validate_self_fns(&mut self, self_fns: &Vec<FnSelfStmt>) {
        self.report_name_clashes(
            self_fns,
            |self_fn| self_fn.sig.name.ident,
            |self_fn| self_fn.id,
            ValidationError::DuplicateFns,
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
            ValidationError::DuplicateField,
        );
    }

    fn validate_enum_members(&mut self, members: &Vec<EnumMember>) {
        self.report_name_clashes(
            members,
            |member| member.name,
            |member| member.id,
            ValidationError::DuplicateEnumMember,
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
        E: Fn(InternedStr, Vec<LocalDefId>) -> ValidationError,
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
    use crate::compiler::compiler::CompilerCtxt;
    use crate::compiler::hir::ModuleId;
    use crate::compiler::krate::{Crate, CrateId, ModuleMap};
    use crate::compiler::parser::parse;
    use crate::compiler::path::ModulePath;
    use crate::compiler::tokens::tokenizer::tokenize;
    use crate::compiler::types::StrMap;
    use crate::compiler::validator::ValidationError;
    use crate::compiler::{compiler, StringInterner};
    use snap::snapshot;
    use std::collections::HashMap;

    type ValidationOutput = (StringInterner, Vec<ValidationError>);

    #[cfg(test)]
    fn validate<T: AsRef<str>>(code: T) -> ValidationOutput {
        let mut compiler_ctxt = CompilerCtxt::default();
        let tokenized_input = tokenize(&mut compiler_ctxt, code);
        let module = parse(&mut compiler_ctxt, tokenized_input).unwrap();
        let krate_name = compiler_ctxt.intern_str("crate");
        let mut krate = Crate::new(krate_name, CrateId::new(0));
        krate.add_module(
            ModulePath::from_array([compiler_ctxt.intern_str("module")]),
            module,
        );
        let krates = StrMap::from([(krate_name, krate)]);
        let krate = krates.get(&krate_name).unwrap();
        let module = krate.module(ModuleId::new(0, 0));
        (
            StringInterner::from(compiler_ctxt),
            crate::compiler::validator::validate(&krates, krate, module),
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
