use itertools::Itertools;
use multimap::MultiMap;

use ast::{
    EnumMember, Fields, FnSelfStmt, FnSig, GenericParams, ItemKind, Module, Params, QualifiedIdent,
};
use diagnostics::{Diagnostic, Diagnostics};
use id::LocalDefId;
use interner::{InternedStr, StringInterner};
use source::{SourceCode, SourceMap};
use span::Span;

pub fn validate(
    string_interner: &StringInterner,
    diagnostics: &mut Diagnostics,
    source_map: &SourceMap,
    module: &Module,
) {
    let validator = Validator::new(string_interner, diagnostics, source_map, module);
    validator.validate()
}

struct Validator<'a> {
    string_interner: &'a StringInterner,
    diagnostics: &'a mut Diagnostics,
    source_map: &'a SourceMap,
    module: &'a Module,
    use_stmts: MultiMap<QualifiedIdent, LocalDefId>,
    global_lets: MultiMap<InternedStr, LocalDefId>,
    tys: MultiMap<InternedStr, LocalDefId>,
    fns: MultiMap<InternedStr, LocalDefId>,
}

impl<'a> Validator<'a> {
    pub(crate) fn new(
        string_interner: &'a StringInterner,
        diagnostics: &'a mut Diagnostics,
        source_map: &'a SourceMap,
        module: &'a Module,
    ) -> Self {
        Self {
            string_interner,
            diagnostics,
            source_map,
            module,
            use_stmts: Default::default(),
            global_lets: Default::default(),
            tys: Default::default(),
            fns: Default::default(),
        }
    }

    fn validate(mut self) {
        for item in &self.module.items {
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
                ItemKind::TraitImpl(_) => {}
                ItemKind::Fn(fn_stmt) => {
                    self.fns.insert(fn_stmt.sig.name.ident, item.id);
                    self.validate_generic_params(&fn_stmt.sig.generic_params);
                    self.validate_params(&fn_stmt.sig.params);
                }
            }
        }

        for (_, use_stmts) in self.use_stmts.iter_all() {
            if use_stmts.len() > 1 {
                self.diagnostics.push(Diagnostic::BlankError);
                // self.errors.push(ValidationErrKind::DuplicateUseStmts(
                //     path.clone(),
                //     use_stmts.clone(),
                // ));
            }
        }
        for (_, global_lets) in self.global_lets.iter_all() {
            if global_lets.len() > 1 {
                self.diagnostics.push(Diagnostic::BlankError);
                // self.errors.push(ValidationErrKind::DuplicateLetStmts(
                //     *name,
                //     global_lets.clone(),
                // ))
            }
        }
        for (_, tys) in self.tys.iter_all() {
            if tys.len() > 1 {
                // TODO: Emit better diagnostics
                self.diagnostics.push(Diagnostic::BlankError);
                // self.errors
                //     .push(ValidationErrKind::DuplicateTys(*name, tys.clone()))
            }
        }
        for (_, fns) in self.fns.iter_all() {
            if fns.len() > 1 {
                // TODO: Emit better diagnostics
                self.diagnostics.push(Diagnostic::BlankError);
                // self.errors
                //     .push(ValidationErrKind::DuplicateFns(*name, fns.clone()))
            }
        }
    }

    fn validate_generic_params(&mut self, generic_params: &GenericParams) {
        self.report_name_clashes(
            generic_params.as_slice(),
            |param| param.name.ident,
            |param| param.span,
            "generic parameters",
        );
    }

    fn validate_params(&mut self, params: &Params) {
        self.report_name_clashes(
            params.as_slice(),
            |param| param.name.ident,
            |param| param.span,
            "parameters",
        );
    }

    fn validate_self_fns(&mut self, self_fns: &Vec<FnSelfStmt>) {
        self.report_name_clashes(
            self_fns,
            |self_fn| self_fn.sig.name.ident,
            |self_fn| self_fn.span,
            "functions",
        );

        for self_fn in self_fns {
            self.validate_fn_sig(&self_fn.sig);
        }
    }

    fn validate_fields(&mut self, fields: &Fields) {
        self.report_name_clashes(
            fields.as_slice(),
            |field| field.name.ident,
            |field| field.span,
            "fields",
        );
    }

    fn validate_enum_members(&mut self, members: &Vec<EnumMember>) {
        self.report_name_clashes(
            members,
            |member| member.name,
            |member| member.span,
            "enum members",
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

    fn source_code(&self) -> &SourceCode {
        self.source_map.source(&self.module.id)
    }

    fn report_name_clashes<T, M: Fn(&T) -> InternedStr, S: Fn(&T) -> Span>(
        &mut self,
        items: &[T],
        mapper: M,
        span: S,
        item_name: &'static str,
    ) {
        let name_clashes = find_name_clashes(items, mapper);
        let diagnostics = name_clashes
            .iter_all()
            .filter(|(_, vals)| vals.len() > 1)
            .map(|(name, vals)| {
                let source_code = self.source_code();
                let name = self.string_interner.resolve(*name);
                let mut message = format!("ERROR: Duplicate {item_name} `{name}` in scope\n");
                let explanation = vals
                    .iter()
                    .map(|val| span(val))
                    .map(|span| source_code.source.span(&span).unwrap_or("".to_string()))
                    .join("\n\n");
                message.push_str(&explanation);
                Diagnostic::Error(message)
            })
            .collect::<Vec<_>>();

        diagnostics
            .into_iter()
            .for_each(|diagnostic| self.diagnostics.push(diagnostic));
    }
}

fn find_name_clashes<T, M: Fn(&T) -> InternedStr>(
    items: &[T],
    mapper: M,
) -> MultiMap<InternedStr, &T> {
    items
        .iter()
        .map(|item| (mapper(item), item))
        .collect::<MultiMap<InternedStr, &T>>()
}

mod tests {
    #[cfg(test)]
    use ast::ModulePath;
    #[cfg(test)]
    use diagnostics::{Diagnostic, DiagnosticKind, Diagnostics};
    #[cfg(test)]
    use id::CrateId;
    #[cfg(test)]
    use id::IdGenerator;
    #[cfg(test)]
    use id::ModuleId;
    #[cfg(test)]
    use interner::StringInterner;
    #[cfg(test)]
    use krate::Crate;
    #[cfg(test)]
    use parser::parse;
    #[cfg(test)]
    use snap::snapshot;
    #[cfg(test)]
    use source::SourceCode;
    #[cfg(test)]
    use source::SourceMap;
    #[cfg(test)]
    use tokenizer::tokenize;
    #[cfg(test)]
    use tokenizer::TokenizedSource;
    #[cfg(test)]
    use types::StrMap;

    #[cfg(test)]
    type ValidationOutput = (StringInterner, Vec<Diagnostic>);

    #[cfg(test)]
    fn validate<T: AsRef<str>>(code: T) -> ValidationOutput {
        let code = code.as_ref().to_string();
        let mut string_interner = StringInterner::default();
        let mut diagnostics = Diagnostics::default();
        let mut id_generator = IdGenerator::default();
        let mut source_map = SourceMap::default();

        let TokenizedSource {
            tokens,
            line_map,
            token_source,
        } = tokenize(&mut string_interner, code);
        let module = parse(
            &mut string_interner,
            &mut diagnostics,
            &mut id_generator,
            tokens,
        )
        .unwrap();
        let krate_name = string_interner.intern("crate");
        let mut krate = Crate::new(krate_name, CrateId::new(0));
        let module_id = krate.add_module(
            ModulePath::from_iter([string_interner.intern("module")]),
            module,
        );
        let source_code = SourceCode::new(token_source, line_map);
        source_map.intern(module_id, source_code);
        let krates = StrMap::from([(krate_name, krate)]);
        let krate = krates.get(&krate_name).unwrap();
        let module = krate.module(ModuleId::new(0, 0));

        crate::validate(&string_interner, &mut diagnostics, &source_map, module);
        (
            string_interner,
            diagnostics.filter(DiagnosticKind::Error).collect(),
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
