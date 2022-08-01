use crate::class::compiled_class::CompiledClass;
use crate::compiler::ast::ast::Stmt::Enum;
use crate::compiler::ast::ast::{
    BlockStmt, ClassStmt, EnumMemberStmt, EnumStmt, FnSig, FnStmt, GenericTy, Generics, Module,
    Mutability, Param, Params, QualifiedIdent, Stmt, UseStmt,
};
use crate::compiler::parser::ParseError::{ExpectedToken, UnexpectedEof, UnexpectedToken};
use crate::compiler::tokens::token::TokenType::{
    Colon, Comma, Fn, Greater, Identifier, LeftBrace, LeftParentheses, Less, Mut, Plus, RightBrace,
    RightBracket, RightParentheses, Semicolon, Use,
};
use crate::compiler::tokens::token::{Token, TokenType};
use crate::compiler::tokens::tokenized_file::{TokenPosition, TokenizedInput};
use crate::compiler::types::types::BasicType::U8;
use crate::compiler::types::types::Type::Basic;
use crate::compiler::types::types::{Ident, Type};
use crate::compiler::StringInterner;
use crate::gc::block::Block;
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
    UnexpectedToken(TokenType, TokenPosition),
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
        let stmts = self.parse_stmts()?;
        Ok(Module::new(stmts))
    }

    fn parse_use_stmt(&mut self) -> Result<Stmt> {
        self.expect(Use)?;
        let identifier = self.qualified_ident()?;
        self.expect(Semicolon)?;
        Ok(Stmt::Use(Box::new(UseStmt::new(identifier))))
    }

    fn parse_stmts(&mut self) -> Result<Vec<Stmt>> {
        let mut stmts = Vec::new();
        while !self.is_at_end() {
            match self.current_type() {
                TokenType::Inline => stmts.push(self.parse_inline_class()?),
                TokenType::Class => stmts.push(self.parse_reference_class()?),
                TokenType::Enum => stmts.push(self.parse_enum()?),
                TokenType::Trait => stmts.push(self.parse_trait()?),
                TokenType::Fn => stmts.push(self.parse_fn()?),
                TokenType::Use => stmts.push(self.parse_use_stmt()?),
                token => {
                    return Err(UnexpectedToken(token, self.current_position()).into());
                }
            }
        }
        Ok(stmts)
    }

    fn parse_inline_class(&mut self) -> Result<Stmt> {
        self.advance();
        self.parse_class(ClassType::Inline)
    }

    fn parse_reference_class(&mut self) -> Result<Stmt> {
        self.parse_class(ClassType::Reference)
    }

    fn parse_class(&mut self, class_type: ClassType) -> Result<Stmt> {
        self.expect(TokenType::Class)?;
        let name = self.identifier()?;
        let generic_types = self.generics()?;
        let members = self.parenthesized_parameters()?;
        let member_functions = self.fn_stmts()?;

        let class_stmt = Box::new(ClassStmt::new(
            name,
            generic_types,
            members,
            member_functions,
        ));

        Ok(Stmt::Class(class_stmt))
    }

    fn parse_enum(&mut self) -> Result<Stmt> {
        self.expect(TokenType::Enum)?;
        let name = self.identifier()?;
        let generics = self.generics()?;
        let enum_members = self.enum_members()?;

        let enum_stmt = Box::new(EnumStmt::new(name, generics, enum_members));

        Ok(Stmt::Enum(enum_stmt))
    }

    fn parse_trait(&mut self) -> Result<Stmt> {
        todo!()
    }

    fn parse_fn(&mut self) -> Result<Stmt> {
        todo!()
    }

    fn identifier(&mut self) -> Result<Ident> {
        self.bounds_check()?;
        match self.current_type() {
            Identifier(ident) => {
                let identifier = self.string_interner.get_or_intern(ident);
                self.advance();
                Ok(identifier)
            }
            _ => {
                let pos = self.current_position();
                self.expected_token(Identifier(""), pos)
            }
        }
    }

    fn qualified_ident(&mut self) -> Result<QualifiedIdent> {
        let mut idents = Vec::new();
        loop {
            self.bounds_check()?;
            match self.current_type() {
                Identifier(ident) => {
                    idents.push(self.string_interner.get_or_intern(ident));
                    if self.remaining() >= 2 {
                        if self.next(1).token_type == Colon && self.next(2).token_type == Colon {
                            self.advance_multiple(3);
                        } else {
                            self.advance();
                            break;
                        }
                    } else {
                        self.advance();
                        break;
                    }
                }
                token => {
                    let pos = self.current_position();
                    return self.expected_token(Identifier(""), pos);
                }
            }
        }

        Ok(QualifiedIdent::new(idents))
    }

    fn generics(&mut self) -> Result<Generics> {
        let generic_tys = self.parse_multiple_with_delimiter::<GenericTy, 1>(
            Self::generic_ty,
            Comma,
            Less,
            Greater,
        )?;
        Ok(Generics::new(generic_tys))
    }

    fn generic_ty(&mut self) -> Result<GenericTy> {
        let ident = self.identifier()?;
        let mut trait_bound: Option<Type> = None;
        if self.matches(Colon) {
            self.advance();
            trait_bound = Some(self.trait_bound()?);
        }
        Ok(GenericTy::new(ident, trait_bound))
    }

    fn fn_stmts(&mut self) -> Result<Vec<FnStmt>> {
        self.parse_multiple(Self::fn_stmt, LeftBrace, RightBrace)
    }

    fn fn_stmt(&mut self) -> Result<FnStmt> {
        match self.current_type() {
            Fn => {
                self.advance();
                let signature = self.function_signature()?;
                let stmt = self.block_statement()?;

                Ok(FnStmt::new(signature, stmt))
            }
            token => {
                let pos = self.current_position();
                self.expected_token(Fn, pos)
            }
        }
    }

    fn enum_fn_stmts(&mut self) -> Result<Vec<FnStmt>> {
        let mut member_funcs = Vec::new();
        if self.matches(LeftBrace) {
            self.advance();
            member_funcs = self.fn_stmts()?;
            self.expect(RightBrace)?;
        }
        Ok(member_funcs)
    }

    fn trait_bound(&mut self) -> Result<Type> {
        let mut trait_bounds = Vec::new();
        trait_bounds.push(self.qualified_ident()?);
        while self.matches(Plus) {
            self.advance();
            trait_bounds.push(self.qualified_ident()?);
        }
        Ok(Type::TraitBounds(trait_bounds))
    }

    fn enum_members(&mut self) -> Result<Vec<EnumMemberStmt>> {
        self.parse_multiple_with_delimiter::<EnumMemberStmt, 1>(
            Self::enum_member,
            Comma,
            LeftBrace,
            RightBrace,
        )
        // loop {
        //     if self.is_at_end() {
        //         return Err(ExpectedIdent(self.last_position()).into());
        //     } else {
        //         match self.current_type() {
        //             Identifier(ident) => {
        //                 let name = self.string_interner.get_or_intern(ident);
        //                 self.advance();
        //
        //                 let params = self.parenthesized_parameters()?;
        //                 let member_funcs = self.enum_member_functions()?;
        //
        //                 members.push(EnumMemberDecl::new(name, params, member_funcs));
        //
        //                 if !self.matches(Comma) {
        //                     self.expect(RightBrace)?;
        //                     break;
        //                 } else {
        //                     // Consume the comma
        //                     self.advance();
        //                     // If the enum declaration ends, break
        //                     if self.matches(RightBrace) {
        //                         self.advance();
        //                         break;
        //                     }
        //                 }
        //             }
        //             token => {
        //                 return Err(UnexpectedToken(token, self.current_position()).into());
        //             }
        //         }
        //     }
        // }
        // Ok(members)
    }

    fn enum_member(&mut self) -> Result<EnumMemberStmt> {
        self.bounds_check()?;
        match self.current_type() {
            Identifier(ident) => {
                let name = self.string_interner.get_or_intern(ident);
                self.advance();

                let params = self.parenthesized_parameters()?;
                let member_funcs = self.enum_fn_stmts()?;

                Ok(EnumMemberStmt::new(name, params, member_funcs))
            }
            token => {
                let pos = self.current_position();
                self.expected_token(Identifier(""), pos)
            }
        }
    }

    fn parenthesized_parameters(&mut self) -> Result<Params> {
        let params = self.parse_multiple_with_delimiter::<Param, 1>(
            Self::parameter,
            LeftParentheses,
            RightParentheses,
            Comma,
        )?;
        Ok(Params::new(params))
    }

    fn parameter(&mut self) -> Result<Param> {
        let ident = self.identifier()?;
        self.expect(Colon)?;
        let mutability = self.mutability();
        let ty = self.parse_ty()?;

        Ok(Param::new(ident, ty, mutability))
    }

    fn function_signature(&mut self) -> Result<FnSig> {
        todo!()
    }

    fn block_statement(&mut self) -> Result<Option<BlockStmt>> {
        todo!()
    }

    fn mutability(&mut self) -> Mutability {
        if self.matches(Mut) {
            self.advance();
            Mutability::Mutable
        } else {
            Mutability::Immutable
        }
    }

    fn parse_ty(&mut self) -> Result<Type> {
        todo!()
    }

    fn parse_multiple<T>(
        &mut self,
        parse_rule: fn(&mut Parser) -> Result<T>,
        scope_start: TokenType,
        scope_end: TokenType,
    ) -> Result<Vec<T>> {
        let mut items = Vec::new();
        if self.matches(scope_start) {
            loop {
                self.bounds_check()?;
                items.push(parse_rule(self)?);
                if self.matches(scope_end) {
                    self.advance();
                    break;
                }
            }
        }
        Ok(items)
    }

    fn parse_multiple_with_delimiter<T, const N: usize>(
        &mut self,
        parse_rule: fn(&mut Parser) -> Result<T>,
        delimiter: TokenType,
        scope_start: TokenType,
        scope_end: TokenType,
    ) -> Result<Vec<T>> {
        let mut items = Vec::new();
        if self.matches(scope_start) {
            self.advance();
            loop {
                if self.is_at_end() {
                    return self.unexpected_end();
                } else {
                    items.push(parse_rule(self)?);
                    for i in 0..N {
                        if self.matches(delimiter) {
                            self.advance();
                        } else if i < N - 1 {
                            let pos = self.current_position();
                            return self.expected_token(delimiter, pos);
                        }
                    }
                    if self.matches(scope_end) {
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

    fn unexpected_token<T>(
        &mut self,
        token_type: TokenType,
        token_position: TokenPosition,
    ) -> Result<T> {
        Err(UnexpectedToken(token_type, token_position).into())
    }

    fn bounds_check(&mut self) -> Result<()> {
        if self.is_at_end() {
            self.unexpected_end()
        } else {
            Ok(())
        }
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

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        return match self {
            UnexpectedEof(pos) => write!(f, "unexpected eof at {}:{}!", pos.line, pos.pos),
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
    use crate::compiler::ast::ast::Mutability::{Immutable, Mutable};
    use crate::compiler::ast::ast::Stmt::Enum;
    use crate::compiler::ast::ast::{
        Args, BlockStmt, EnumMemberStmt, EnumStmt, Expr, FnCall, FnSig, FnStmt, GenericTy,
        Generics, Mutability, Param, Params, QualifiedIdent, Stmt,
    };
    use crate::compiler::ast::ast::{Module, UseStmt};
    use crate::compiler::parser::ParseError::{ExpectedToken, UnexpectedEof};
    use crate::compiler::parser::{ParseError, Parser};
    use crate::compiler::tokens::token::TokenType;
    use crate::compiler::tokens::token::TokenType::Identifier;
    use crate::compiler::tokens::tokenized_file::{TokenPosition, TokenizedInput};
    use crate::compiler::tokens::tokenizer::tokenize;
    use crate::compiler::types::types::Type;
    use crate::compiler::types::types::Type::{Generic, TraitBounds};
    use crate::compiler::StringInterner;
    use anyhow::{anyhow, Result};
    use libc::strncat;
    use std::any::Any;
    use std::error::Error;
    use std::fs::File;
    use std::io::{BufReader, BufWriter};
    use std::path::{Path, PathBuf};
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

    fn create_path(path: &Path) -> Box<Path> {
        let mut pathbuf = PathBuf::new();
        let manifest_dir = env!("CARGO_MANIFEST_DIR");
        pathbuf.push(Path::new(manifest_dir));
        pathbuf.push(path);
        pathbuf.into_boxed_path()
    }

    fn load_module(path: &Path) -> Result<Module> {
        let file = File::open(create_path(path))?;
        let reader = BufReader::new(file);
        Ok(serde_json::from_reader(reader)?)
    }

    fn save_module(module_path: &Path, module: Module) -> Result<()> {
        let file = File::open(create_path(module_path))?;
        let writer = BufWriter::new(file);
        Ok(serde_json::to_writer(writer, &module)?)
    }

    fn compare_modules(path: &str, module: Module) {
        let module_path = Path::new(path);
        if let Ok(loaded) = load_module(module_path) {
            assert_eq!(loaded, module);
        } else {
            save_module(module_path, module).expect("Error saving module!");
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
                Some(UnexpectedEof(pos)) => {}
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
                Some(ExpectedToken(Identifier(_), pos)) => {}
                _ => panic!(),
            },
        }

        match parse_code("use") {
            Ok(_) => panic!(),
            Err(error) => match error.downcast_ref::<ParseError>() {
                Some(UnexpectedEof(pos)) => {}
                _ => {
                    println!("{}", error);
                    panic!()
                }
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
        compare_modules("use_stmts", module);
    }

    #[test]
    pub fn basic_enum() {
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

        let enum_stmts = module
            .stmts()
            .into_iter()
            .filter_map(|x| match x {
                Enum(enum_stmt) => Some(enum_stmt),
                _ => None,
            })
            .collect::<Vec<&Box<EnumStmt>>>();

        assert_eq!(1, enum_stmts.len());

        let planet_enum = enum_stmts[0];
        assert_eq!(Generics::default(), *planet_enum.generics());

        assert_eq!(
            vec![
                EnumMemberStmt::new(
                    string_interner.get("Mercury").unwrap(),
                    Params::default(),
                    vec![],
                ),
                EnumMemberStmt::new(
                    string_interner.get("Venus").unwrap(),
                    Params::default(),
                    vec![],
                ),
                EnumMemberStmt::new(
                    string_interner.get("Earth").unwrap(),
                    Params::default(),
                    vec![],
                ),
                EnumMemberStmt::new(
                    string_interner.get("Mars").unwrap(),
                    Params::default(),
                    vec![],
                ),
                EnumMemberStmt::new(
                    string_interner.get("Jupiter").unwrap(),
                    Params::default(),
                    vec![],
                ),
                EnumMemberStmt::new(
                    string_interner.get("Saturn").unwrap(),
                    Params::default(),
                    vec![],
                ),
                EnumMemberStmt::new(
                    string_interner.get("Uranus").unwrap(),
                    Params::default(),
                    vec![],
                ),
                EnumMemberStmt::new(
                    string_interner.get("Neptune").unwrap(),
                    Params::default(),
                    vec![],
                ),
                EnumMemberStmt::new(
                    string_interner.get("Pluto").unwrap(),
                    Params::default(),
                    vec![],
                ),
            ],
            planet_enum.members()
        );
    }

    #[test]
    pub fn complex_enum() {
        let code = concat!(
            "enum Vector<X: Number + Display, Y: Number + Display> {",
            "    Normalized(x: X, y: Y),",
            "    Absolute(x: X, y: Y) {",
            "        pub fn to_normalized(self) -> Vector {",
            "            return Normalized(self.x, self.y);",
            "        }",
            "    }",
            "}"
        );

        let (string_interner, module) = parse_code(code).unwrap();
        compare_modules("complex_enum", module);
    }
}
