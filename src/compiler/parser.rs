use crate::class::compiled_class::CompiledClass;
use crate::compiler::ast::{ClassStatement, FunctionStatement, UseStatement, Module, TypeStatement};
use anyhow::{anyhow, Result};
use string_interner::StringInterner;
use crate::compiler::tokens::token::{Token, TokenType};
use crate::compiler::tokens::tokenized_file::TokenizedInput;
use crate::compiler::tokens::tokenizer::read_tokens;
use crate::compiler::types::types::{Identifier, Type};

pub fn parse(input: TokenizedInput) -> Result<Module> {
    let parser = Parser::new(input);
    parser.parse()
}

struct Parser {
    string_interner: StringInterner,
    tokenized_input: TokenizedInput,
    pos: usize,
}

impl Parser {

    fn new(tokenized_input: TokenizedInput) -> Self {
        Self {
            string_interner: StringInterner::default(),
            tokenized_input,
            pos: 0,
        }
    }

    fn parse(mut self) -> Result<Module> {
        let use_stmts = self.parse_use_stmts()?;
        let tys_and_fns = self.parse_tys_fns()?;
        Ok(Module::new(use_stmts, tys_and_fns.0, tys_and_fns.1))
    }

    fn parse_use_stmts(&mut self) -> Result<Vec<UseStatement>> {
        let mut stmts = Vec::<UseStatement>::new();
        while self.matches(TokenType::Use) {
            self.munch();
            let identifier = self.identifier()?;
            stmts.push(UseStatement::new(identifier));
            self.munch_semicolon()?;
        }
        Ok(stmts)
    }

    fn parse_tys_fns(&mut self) -> Result<(Vec<TypeStatement>, Vec<FunctionStatement>)> {
        todo!()
    }

    fn identifier(&mut self) -> Result<Identifier> {
        todo!()
    }

    fn current(&mut self) -> Token {
        self.tokenized_input.tokens()[self.pos]
    }

    fn munch(&mut self) {
        self.pos += 1;
    }

    fn munch_semicolon(&mut self) -> Result<()> {
        if self.current().token_type != TokenType::Semicolon {
            Err(anyhow!("Expected semicolon"))
        } else {
            self.munch();
            Ok(())
        }
    }

    fn matches(&mut self, token_type: TokenType) -> bool {
        self.current().token_type == token_type
    }
}