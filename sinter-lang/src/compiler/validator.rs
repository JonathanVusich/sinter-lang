use std::collections::HashMap;

use multimap::MultiMap;

use crate::compiler::ast::{
    EnumMember, Field, Fields, FnSelfStmt, FnSig, FnStmt, GenericParam, GenericParams,
    GlobalLetStmt, Item, ItemKind, Module, Param, Params, QualifiedIdent, UseStmt,
};
use crate::compiler::compiler::CompileError;
use crate::compiler::hir::LocalDefId;
use crate::compiler::krate::Crate;
use crate::compiler::types::types::InternedStr;

const MAX_NUM: usize = 65535;

#[derive(Debug)]
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

#[derive(Default)]
struct Validator {
    use_stmts: MultiMap<QualifiedIdent, LocalDefId>,
    global_lets: MultiMap<InternedStr, LocalDefId>,
    tys: MultiMap<InternedStr, LocalDefId>,
    fns: MultiMap<InternedStr, LocalDefId>,

    errors: Vec<ValidationError>,
}

impl Validator {
    fn validate(mut self, module: &Module) -> Result<(), Vec<ValidationError>> {
        for item in &module.items {
            match &item.kind {
                ItemKind::Use(use_stmt) => self.use_stmts.insert(use_stmt.path.clone(), item.id),
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

        if !self.errors.is_empty() {
            Err(self.errors)
        } else {
            Ok(())
        }
    }

    fn validate_generic_params(&mut self, generic_params: &GenericParams) {
        self.report_name_clashes(
            generic_params,
            |param| param.name.ident,
            |param| param.id,
            |name, ids| ValidationError::DuplicateGenericParam(name, ids),
        );
    }

    fn validate_params(&mut self, params: &Params) {
        self.report_name_clashes(
            params,
            |param| param.name.ident,
            |param| param.id,
            |name, ids| ValidationError::DuplicateParam(name, ids),
        );
    }

    fn validate_self_fns(&mut self, self_fns: &Vec<FnSelfStmt>) {
        self.report_name_clashes(
            self_fns,
            |self_fn| self_fn.sig.name.ident,
            |self_fn| self_fn.id,
            |name, ids| ValidationError::DuplicateFns(name, ids),
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
            |name, ids| ValidationError::DuplicateField(name, ids),
        );
    }

    fn validate_enum_members(&mut self, members: &Vec<EnumMember>) {
        self.report_name_clashes(
            members,
            |member| member.name,
            |member| member.id,
            |name, ids| ValidationError::DuplicateEnumMember(name, ids),
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

pub fn validate(module: &Module) -> Result<(), Vec<ValidationError>> {
    let validator = Validator::default();
    validator.validate(module)
}
