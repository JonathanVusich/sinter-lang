use crate::class::compiled_class::CompiledClass;
use crate::compiler::ast::{
    BlockStatement, ClassStatement, EnumMemberDecl, EnumStatement, FunctionSignature,
    FunctionStatement, GenericTypeDecl, MemberDecl, MemberFunctionDecl, Module, ParameterDecl,
    QualifiedIdent, TypeStatement, UseStatement,
};
use crate::compiler::parser::ParseError::{
    ExpectedIdent, ExpectedToken, UnexpectedEof, UnexpectedToken,
};
use crate::compiler::tokens::token::TokenType::{
    Colon, Comma, Fn, Identifier, LeftBrace, LeftParentheses, RightBrace, RightBracket,
    RightParentheses,
};
use crate::compiler::tokens::token::{Token, TokenType};
use crate::compiler::tokens::tokenized_file::{TokenPosition, TokenizedInput};
use crate::compiler::types::types::{Ident, Type};
use crate::compiler::StringInterner;
use anyhow::{anyhow, Result};
use std::error::Error;
use std::fmt::Alignment::Right;
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
    UnexpectedEof(TokenPosition),
    ExpectedToken(TokenType, TokenPosition),
    ExpectedIdent(TokenPosition),
    UnexpectedToken(TokenType, TokenPosition),
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
                TokenType::Enum => tys_and_fns.tys.push(self.parse_enum()?),
                TokenType::Trait => tys_and_fns.tys.push(self.parse_trait()),
                TokenType::Fn => tys_and_fns.fns.push(self.parse_fn()),
                token => {
                    return Err(UnexpectedToken(token, self.current_position()).into());
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

    fn parse_enum(&mut self) -> Result<TypeStatement> {
        self.expect(TokenType::Enum)?;
        let name = self.identifier()?;
        let generic_types = self.generic_tys()?;
        let enum_members = self.enum_members()?;

        let enum_stmt = Box::new(EnumStatement::new(name, generic_types, enum_members));

        Ok(TypeStatement::Enum(enum_stmt))
    }

    fn parse_trait(&mut self) -> TypeStatement {
        todo!()
    }

    fn parse_fn(&mut self) -> FunctionStatement {
        todo!()
    }

    fn identifier(&mut self) -> Result<Ident> {
        if self.is_at_end() {
            return Err(ExpectedIdent(self.last_position()).into());
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
        let mut funcs = Vec::new();
        loop {
            if self.is_at_end() {
                return Err(ExpectedToken(Fn, self.last_position()).into());
            } else {
                match self.current_type() {
                    Fn => {
                        self.advance();
                        let name = self.identifier()?;
                        let signature = self.function_signature()?;
                        let stmt = self.block_statement()?;

                        funcs.push(MemberFunctionDecl::new(name, signature, stmt));
                    }
                    token => {
                        return Err(ExpectedToken(Fn, self.current_position()).into());
                    }
                }
            }
        }
    }

    fn enum_member_functions(&mut self) -> Result<Vec<MemberFunctionDecl>> {
        let mut member_funcs = Vec::new();
        if self.matches(LeftBrace) {
            self.advance();
            member_funcs = self.member_functions()?;
            self.expect(RightBrace)?;
        }
        Ok(member_funcs)
    }

    fn trait_bound(&mut self) -> Result<Type> {
        todo!()
    }

    fn enum_members(&mut self) -> Result<Vec<EnumMemberDecl>> {
        let mut members = Vec::new();
        loop {
            if self.is_at_end() {
                return Err(ExpectedIdent(self.last_position()).into());
            } else {
                match self.current_type() {
                    Identifier(ident) => {
                        let name = self.string_interner.get_or_intern(ident);
                        self.advance();

                        let params = self.parenthesized_parameters()?;
                        let member_funcs = self.enum_member_functions()?;

                        members.push(EnumMemberDecl::new(name, params, member_funcs));

                        if !self.matches(Comma) {
                            self.expect(RightBrace)?;
                            break;
                        } else {
                            // Consume the comma
                            self.advance();
                            // If the enum declaration ends, break
                            if self.matches(RightBrace) {
                                self.advance();
                                break;
                            }
                        }
                    }
                    token => {
                        return Err(UnexpectedToken(token, self.current_position()).into());
                    }
                }
            }
        }
        Ok(members)
    }

    fn parenthesized_parameters(&mut self) -> Result<Vec<ParameterDecl>> {
        let mut params = Vec::new();
        if self.matches(LeftParentheses) {
            self.advance();
            params = self.parameters()?;
            self.expect(RightParentheses)?;
        }
        Ok(params)
    }

    fn parameters(&mut self) -> Result<Vec<ParameterDecl>> {
        let mut parameters = Vec::new();
        loop {
            if self.is_at_end() {
                return Err(ExpectedIdent(self.last_position()).into());
            } else {
                parameters.push(self.parameter()?);
                if !self.matches(Comma) {
                    break;
                } else {
                    self.advance();
                }
            }
        }
        Ok(parameters)
    }

    fn parameter(&mut self) -> Result<ParameterDecl> {
        let ident = self.identifier()?;
        self.expect(Colon)?;
        let ty = self.parse_ty()?;

        Ok(ParameterDecl::new(ident, ty))
    }

    fn function_signature(&mut self) -> Result<FunctionSignature> {
        todo!()
    }

    fn block_statement(&mut self) -> Result<BlockStatement> {
        todo!()
    }

    fn parse_ty(&mut self) -> Result<Type> {
        todo!()
    }

    fn parse_multiple<T>(
        &mut self,
        parse_rule: fn() -> Result<T>,
        delimiter: TokenType,
        end_of_scope: TokenType,
    ) -> Result<Vec<T>> {
        let mut items = Vec::new();
        loop {
            if self.is_at_end() {
                return self.unexpected_end();
            } else {
                items.push(parse_rule()?);
                if self.matches(delimiter) {
                    self.advance();
                    if self.matches(end_of_scope) {
                        self.advance();
                        break;
                    }
                }
            }
        }
        Ok(items)
    }

    fn expected_token<T>(
        &mut self,
        token_type: TokenType,
        token_position: TokenPosition,
    ) -> Result<T> {
        Err(ExpectedToken(token_type, token_position).into())
    }

    fn unexpected_end<T>(&mut self) -> Result<T> {
        Err(UnexpectedEof(self.last_position()).into())
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
        if self.is_at_end() {
            Err(ExpectedToken(token_type, self.last_position()).into())
        } else if self.current_type() != token_type {
            Err(ExpectedToken(token_type, self.current_position()).into())
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
            UnexpectedEof(pos) => write!(f, "unexpected eof at {}:{}!", pos.line, pos.pos),
            ExpectedIdent(pos) => write!(f, "expected identifier at {}:{}!", pos.line, pos.pos),
            ExpectedToken(token_type, pos) => write!(
                f,
                "expected token {} at {}:{}!",
                token_type, pos.line, pos.pos
            ),
            UnexpectedToken(token_type, pos) => write!(
                f,
                "unrecognized token {} at {}:{}!",
                token_type, pos.line, pos.pos
            ),
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

    #[cfg(test)]
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
    pub fn invalid_use_stmts() {
        match parse_code("use std::vector::") {
            Ok((string_interner, module)) => {
                panic!();
            }
            Err(error) => match error.downcast_ref::<ParseError>() {
                Some(ExpectedIdent(_)) => {}
                _ => {
                    panic!();
                }
            },
        }

        match parse_code("use std::vector::Vector") {
            Ok(_) => panic!(),
            Err(error) => match error.downcast_ref::<ParseError>() {
                Some(ExpectedToken(token, _)) => {
                    assert_eq!(*token, TokenType::Semicolon)
                }
                _ => panic!(),
            },
        }

        match parse_code("use;") {
            Ok(_) => panic!(),
            Err(error) => match error.downcast_ref::<ParseError>() {
                Some(ExpectedIdent(_)) => {}
                _ => panic!(),
            },
        }

        match parse_code("use") {
            Ok(_) => panic!(),
            Err(error) => match error.downcast_ref::<ParseError>() {
                Some(ExpectedIdent(_)) => {}
                _ => panic!(),
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

    #[test]
    pub fn tys() {
        let code = concat!(
            "enum Planet {",
            "    Mercury,",
            "    Venus,",
            "    Earth,",
            "    Mars,",
            "    Jupiter,",
            "    Saturn,",
            "    Uranus,",
            "    Neptune,",
            "    Pluto,",
            "}"
        );

        let (string_interner, module) = parse_code(code).unwrap();
    }
}
