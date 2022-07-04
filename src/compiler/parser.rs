use std::sync::Arc;
use anyhow::{anyhow, Result};
use crate::class::compiled_class::CompiledClass;
use crate::compiler::ast::{ClassStatement, FunctionStatement, UseStatement, Module, TypeStatement, QualifiedIdent, GenericTypeDecl, MemberDecl, MemberFunctionDecl};
use crate::compiler::StringInterner;
use crate::compiler::tokens::token::{Token, TokenType};
use crate::compiler::tokens::token::TokenType::Identifier;
use crate::compiler::tokens::tokenized_file::TokenizedInput;
use crate::compiler::types::types::{Ident, Type};

pub fn parse(string_interner: StringInterner, input: TokenizedInput) -> Result<Module> {
    let parser = Parser::new(string_interner, input);
    parser.parse()
}

struct Parser {
    string_interner: StringInterner,
    tokenized_input: TokenizedInput,
    pos: usize,
}

struct TysAndFns {
    tys: Vec<TypeStatement>,
    fns: Vec<FunctionStatement>,
}

enum ClassType {
    Reference,
    Inline,
}

impl Parser {
    fn new(string_interner: StringInterner, tokenized_input: TokenizedInput) -> Self {
        Self {
            string_interner,
            tokenized_input,
            pos: 0,
        }
    }

    fn parse(mut self) -> Result<Module> {
        let use_stmts = self.parse_use_stmts()?;
        let tys_and_fns = self.parse_tys_fns()?;
        Ok(Module::new(use_stmts, tys_and_fns.tys, tys_and_fns.fns))
    }

    fn parse_use_stmts(&mut self) -> Result<Vec<UseStatement>> {
        let mut stmts = Vec::<UseStatement>::new();
        while self.matches(TokenType::Use) {
            let identifier = self.qualified_ident()?;
            stmts.push(UseStatement::new(identifier));
            self.expect(TokenType::Semicolon)?;
        }
        Ok(stmts)
    }

    fn parse_tys_fns(&mut self) -> Result<TysAndFns> {
        let mut tys_and_fns = TysAndFns::default();
        while !self.is_at_end() {
            match self.current_type() {
                TokenType::Inline => tys_and_fns.tys.push(self.parse_inline_class()?),
                TokenType::Class => tys_and_fns.tys.push(self.parse_reference_class()?),
                TokenType::Enum => tys_and_fns.tys.push(self.parse_enum()),
                TokenType::Trait => tys_and_fns.tys.push(self.parse_trait()),
                TokenType::Fn => tys_and_fns.fns.push(self.parse_fn()),
                _ => {
                    return Err(anyhow!("Unrecognized token!"));
                },
            }
        }
        Ok(tys_and_fns)
    }

    fn parse_inline_class(&mut self) -> Result<TypeStatement> {
        self.advance();
        self.parse_class(ClassType::Inline)
    }

    fn parse_reference_class(&mut self) -> Result<TypeStatement> {
        self.parse_class(ClassType::Reference)
    }

    fn parse_class(&mut self, class_type: ClassType) -> Result<TypeStatement> {
        self.expect(TokenType::Class)?;
        let name = self.identifier()?;
        let generic_types = self.generic_tys()?;
        let members = self.members()?;
        let member_functions = self.member_functions()?;

        let class_stmt = Box::new(ClassStatement::new(
            name,
            generic_types,
            members,
            member_functions,
        ));

        Ok(TypeStatement::Class(class_stmt))
    }

    fn parse_enum(&mut self) -> TypeStatement {
        todo!()
    }

    fn parse_trait(&mut self) -> TypeStatement {
        todo!()
    }

    fn parse_fn(&mut self) -> FunctionStatement {
        todo!()
    }

    fn identifier(&mut self) -> Result<Ident> {
        if self.is_at_end() {
            return Err(anyhow!("Expected identifier!"));
        }
        return match self.current_type() {
            Identifier(ident) => {
                Ok(self.string_interner.get_or_intern(ident))
            }
            _ => {
                Err(anyhow!("Expected identifier!"))
            }
        }
    }

    fn qualified_ident(&mut self) -> Result<QualifiedIdent> {
        let mut idents = Vec::new();

        loop {
            if let Identifier(ident) = self.current().token_type {
                idents.push(self.string_interner.get_or_intern(ident));
                if self.remaining() > 2 {
                    if self.next(1).token_type == TokenType::Colon &&
                        self.next(2).token_type == TokenType::Colon {
                        self.pos += 3;
                    } else {
                        self.pos += 1;
                        break;
                    }
                } else {
                    self.pos += 1;
                    break;
                }
            } else {
                return Err(anyhow!("Expected qualified identifier!"));
            }
        }

        Ok(QualifiedIdent::new(idents))
    }

    fn generic_tys(&mut self) -> Result<Vec<GenericTypeDecl>> {
        if self.matches(TokenType::Less) {
            let mut type_decls = Vec::new();
            while !self.matches(TokenType::Comma) || !self.matches(TokenType::Greater) {
                let ident = self.identifier()?;
                if self.matches(TokenType::Colon) {
                    self.advance();
                    let trait_bound = self.trait_bound()?;
                    let type_decl = GenericTypeDecl::new(ident, Some(trait_bound));
                    type_decls.push(type_decl);
                }
            }
            Ok(type_decls)
        } else {
            Ok(Vec::new())
        }
    }

    fn members(&mut self) -> Result<Vec<MemberDecl>> {
        todo!()
    }

    fn member_functions(&mut self) -> Result<Vec<MemberFunctionDecl>> {
        todo!()
    }

    fn trait_bound(&mut self) -> Result<Type> {
        todo!()
    }

    fn current(&mut self) -> Token {
        self.tokenized_input.tokens()[self.pos]
    }

    fn current_type(&mut self) -> TokenType {
        self.tokenized_input.tokens()[self.pos].token_type
    }

    fn advance(&mut self) {
        self.pos += 1;
    }

    fn expect(&mut self, token_type: TokenType) -> Result<()> {
        if self.is_at_end() || self.current_type() != token_type {
            Err(anyhow!(format!("Expected {:?}!", token_type)))
        } else {
            self.pos += 1;
            Ok(())
        }
    }

    fn matches(&mut self, token_type: TokenType) -> bool {
        !self.is_at_end() && self.current_type() == token_type
    }

    fn is_at_end(&self) -> bool {
        self.pos >= self.tokenized_input.tokens().len()
    }

    fn remaining(&self) -> usize {
        self.tokenized_input.tokens().len() - self.pos
    }

    fn next(&self, delta: usize) -> Token {
        self.tokenized_input.tokens()[self.pos + delta]
    }
}

impl TysAndFns {
    pub fn new() -> Self {
        Self {
            tys: Vec::new(),
            fns: Vec::new(),
        }
    }
}

impl Default for TysAndFns {
    fn default() -> Self {
        Self::new()
    }
}

mod tests {
    use std::sync::Arc;
    use anyhow::Result;
    use crate::compiler::ast::{Module, QualifiedIdent, UseStatement};
    use crate::compiler::parser::Parser;
    use crate::compiler::StringInterner;
    use crate::compiler::tokens::tokenized_file::TokenizedInput;
    use crate::compiler::tokens::tokenizer::tokenize;

    #[test]
    pub fn use_statements() {
        let string_interner = StringInterner::default();

        let code = concat!(
            "use std::vector;",
            "use std::array;"
        );

        let parser = Parser::new(string_interner.clone(), tokenize(code).unwrap());

        let module = parser.parse().unwrap();

        let std_ident = string_interner.get("std").unwrap();
        let vector_ident = string_interner.get("vector").unwrap();
        let array_ident = string_interner.get("array").unwrap();

        let first_use_qualified_ident = QualifiedIdent::new(vec![std_ident, vector_ident]);
        let second_use_qualified_ident = QualifiedIdent::new(vec![std_ident, array_ident]);

        assert_eq!(vec![
            UseStatement::new(first_use_qualified_ident),
            UseStatement::new(second_use_qualified_ident)
        ], module.use_statements());
    }
}
