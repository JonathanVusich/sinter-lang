use crate::class::compiled_class::CompiledClass;
use crate::compiler::ast::{
    ClassStatement, FunctionStatement, GenericTypeDecl, MemberDecl, MemberFunctionDecl, Module,
    QualifiedIdent, TypeStatement, UseStatement,
};
use crate::compiler::parser::ParseError::{ExpectedIdent, ExpectedToken, UnrecognizedToken};
use crate::compiler::tokens::token::TokenType::Identifier;
use crate::compiler::tokens::token::{Token, TokenType};
use crate::compiler::tokens::tokenized_file::{TokenPosition, TokenizedInput};
use crate::compiler::types::types::{Ident, Type};
use crate::compiler::StringInterner;
use anyhow::{anyhow, Result};
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::sync::Arc;

pub fn parse(string_interner: StringInterner, input: TokenizedInput) -> Result<Module> {
    let parser = Parser::new(string_interner, input);
    parser.parse()
}

struct Parser {
    string_interner: StringInterner,
    tokenized_input: TokenizedInput,
    pos: usize,
}

#[derive(Debug)]
enum ParseError {
    ExpectedToken(TokenType, TokenPosition),
    ExpectedIdent(TokenPosition),
    UnrecognizedToken(TokenPosition),
}

#[derive(Default)]
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
            self.advance();
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
                    return Err(UnrecognizedToken(self.current_position()).into());
                }
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
            return Err(ExpectedIdent(self.current_position()).into());
        }
        return match self.current_type() {
            Identifier(ident) => Ok(self.string_interner.get_or_intern(ident)),
            _ => Err(anyhow!("Expected identifier!")),
        };
    }

    fn qualified_ident(&mut self) -> Result<QualifiedIdent> {
        let mut idents = Vec::new();

        loop {
            if self.is_at_end() {
                return Err(ExpectedIdent(self.last_position()).into());
            } else if let Identifier(ident) = self.current_type() {
                idents.push(self.string_interner.get_or_intern(ident));
                if self.remaining() >= 2 {
                    if self.next(1).token_type == TokenType::Colon
                        && self.next(2).token_type == TokenType::Colon
                    {
                        self.advance_multiple(3);
                    } else {
                        self.advance();
                        break;
                    }
                } else {
                    self.advance();
                    break;
                }
            } else {
                return Err(ExpectedIdent(self.current_position()).into());
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

    fn current_position(&mut self) -> TokenPosition {
        let token = self.current();
        self.tokenized_input.token_position(token.start)
    }

    fn last(&mut self) -> Token {
        self.tokenized_input.tokens()[self.tokenized_input.tokens().len() - 1]
    }

    fn last_position(&mut self) -> TokenPosition {
        let token = self.last();
        self.tokenized_input.token_position(token.end)
    }

    fn advance(&mut self) {
        self.pos += 1;
    }

    fn advance_multiple(&mut self, amount: usize) {
        self.pos += amount;
    }

    fn expect(&mut self, token_type: TokenType) -> Result<()> {
        if self.is_at_end() || self.current_type() != token_type {
            Err(anyhow!(format!("Expected {:?}!", token_type)))
        } else {
            self.advance();
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
        self.tokenized_input.tokens().len() - (self.pos + 1)
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

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        return match self {
            ExpectedIdent(err) => write!(f, "expected identifier at {}:{}!", err.line, err.pos),
            ExpectedToken(token_type, err) => write!(
                f,
                "expected token {} at {}:{}!",
                token_type, err.line, err.pos
            ),
            UnrecognizedToken(err) => write!(f, "unrecognized token at {}:{}!", err.line, err.pos),
        };
    }
}

impl Error for ParseError {}

unsafe impl Send for ParseError {}
unsafe impl Sync for ParseError {}

mod tests {
    use crate::compiler::ast::{Module, QualifiedIdent, UseStatement};
    use crate::compiler::parser::ParseError::{ExpectedIdent, ExpectedToken};
    use crate::compiler::parser::{ParseError, Parser};
    use crate::compiler::tokens::token::TokenType;
    use crate::compiler::tokens::tokenized_file::{TokenPosition, TokenizedInput};
    use crate::compiler::tokens::tokenizer::tokenize;
    use crate::compiler::StringInterner;
    use anyhow::Result;
    use std::any::Any;
    use std::error::Error;
    use std::sync::Arc;

    macro_rules! qualified_ident {
        ($interner:expr, $($string:literal),*) => {
                QualifiedIdent::new(vec![
                    $(
                        $interner.get($string).unwrap(),
                    )*
                ])
        }
    }

    fn parse_code(code: &str) -> Result<(StringInterner, Module)> {
        let string_interner = StringInterner::default();
        let tokens = tokenize(code).unwrap();
        let parser = Parser::new(string_interner.clone(), tokens);
        let (string_interner, parser) = (string_interner, parser);
        let module = parser.parse()?;

        Ok((string_interner, module))
    }

    #[test]
    pub fn invalid_qualified_ident() {
        let code = "use std::vector::";
        match parse_code(code) {
            Ok((string_interner, module)) => {
                assert!(false);
            }
            Err(error) => match error.downcast_ref::<ParseError>() {
                Some(ExpectedIdent(_)) => {}
                _ => {
                    assert!(false);
                }
            },
        }
    }

    #[test]
    pub fn expected_semicolon() {
        let code = "use std::vector::Vector";
        match parse_code(code) {
            Ok(_) => assert!(false),
            Err(error) => match error.downcast_ref::<ParseError>() {
                Some(ExpectedToken(token, _)) => {
                    assert_eq!(*token, TokenType::Semicolon)
                }
                _ => assert!(false),
            },
        }
    }

    #[test]
    pub fn use_statements() {
        let code = concat!(
            "use std::vector;",
            "use std::array;",
            "use std::map::HashMap;"
        );

        let (string_interner, module) = parse_code(code).unwrap();

        assert_eq!(
            vec![
                UseStatement::new(qualified_ident!(string_interner, "std", "vector")),
                UseStatement::new(qualified_ident!(string_interner, "std", "array")),
                UseStatement::new(qualified_ident!(string_interner, "std", "map", "HashMap"))
            ],
            module.use_statements()
        );
    }
}
