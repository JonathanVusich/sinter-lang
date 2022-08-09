use crate::class::compiled_class::CompiledClass;
use crate::compiler::ast::ast::Stmt::Enum;
use crate::compiler::ast::ast::{BlockStmt, ClassStmt, EnumMemberStmt, EnumStmt, FnSig, FnStmt, GenericTy, Generics, Module, Mutability, Param, Params, QualifiedIdent, Stmt, UseStmt, PathStmt, LocalStmt, Expr};
use crate::compiler::parser::ParseError::{ExpectedToken, UnexpectedEof, UnexpectedToken};
use crate::compiler::tokens::token::TokenType::{Colon, Comma, Fn, Greater, Identifier, LeftBrace, LeftParentheses, Less, Mut, Plus, RightBrace, LeftBracket, RightBracket, RightParentheses, Semicolon, Use, Let, Equal, RightArrow, SelfLowercase};
use crate::compiler::tokens::token::{Token, TokenType};
use crate::compiler::tokens::tokenized_file::{TokenPosition, TokenizedInput};
use crate::compiler::types::types::BasicType::{F32, F64, I16, I32, I64, I8, U16, U32, U64, U8};
use crate::compiler::types::types::Type::{Basic, Infer, Path, TraitBounds, Union};
use crate::compiler::types::types::{BasicType, InternedStr, Type};
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

    fn parse_let_stmt(&mut self) -> Result<Stmt> {
        self.expect(Let)?;
        let identifier = self.identifier()?;
        let mut ty = None;
        let mut initializer = None;
        if self.matches(Colon) {
            self.advance();
            ty = Some(self.parse_ty()?);
        }
        if self.matches(Equal) {
            self.advance();
            initializer = Some(self.parse_expression()?);
        }
        self.expect(Semicolon)?;
        Ok(Stmt::Local(Box::new(LocalStmt::new(identifier, ty, initializer))))
    }

    fn parse_use_stmt(&mut self) -> Result<Stmt> {
        self.expect(Use)?;
        let identifier = self.qualified_ident()?;
        self.expect(Semicolon)?;
        Ok(Stmt::Use(Box::new(UseStmt::new(identifier))))
    }

    fn parse_stmts(&mut self) -> Result<Vec<Stmt>> {
        todo!()
    }

    fn parse_stmt(&mut self) -> Result<Stmt> {
        self.bounds_check()?;
        match self.current_type() {
            Let => self.parse_let_stmt(),
            TokenType::Inline => self.parse_inline_class(),
            TokenType::Class => self.parse_reference_class(),
            TokenType::Enum => self.parse_enum(),
            TokenType::Trait => self.parse_trait(),
            TokenType::Impl => self.parse_trait_impl(),
            Fn => self.parse_fn(),
            Use => self.parse_use_stmt(),
            token => self.parse_expression()
        }
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
        let members = self.parenthesized_params()?;
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

        Ok(Enum(enum_stmt))
    }

    fn parse_trait(&mut self) -> Result<Stmt> {
        todo!()
    }

    fn parse_trait_impl(&mut self) -> Result<Stmt> {
        todo!()
    }

    fn parse_fn(&mut self) -> Result<Stmt> {
        Ok(Stmt::Fn(Box::new(self.fn_stmt()?)))
    }

    fn parse_expression(&mut self) -> Result<Expr> {

        todo!()
    }

    fn identifier(&mut self) -> Result<InternedStr> {
        self.bounds_check()?;
        match self.current_type() {
            Identifier(ident) => {
                self.advance();
                Ok(ident)
            }
            _ => {
                let pos = self.current_position();
                self.expected_token(Identifier(self.string_interner.get_or_intern("")), pos)
            }
        }
    }

    fn qualified_ident(&mut self) -> Result<QualifiedIdent> {
        let mut idents = Vec::new();
        loop {
            self.bounds_check()?;
            match self.current_type() {
                Identifier(ident) => {
                    idents.push(ident);
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
                    return self.expected_token(Identifier(self.string_interner.get_or_intern("")), pos);
                }
            }
        }

        Ok(QualifiedIdent::new(idents))
    }

    fn generics(&mut self) -> Result<Generics> {
        let generic_tys = self.parse_multiple_with_scope_delimiter::<GenericTy, 1>(
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
        self.parse_multiple_with_scope(Self::fn_stmt, LeftBrace, RightBrace)
    }

    fn fn_stmt(&mut self) -> Result<FnStmt> {
        match self.current_type() {
            Fn => {
                self.advance();
                let signature = self.function_signature()?;
                let stmt = self.block_statement()?;

                Ok(FnStmt::new(signature, Some(stmt)))
            }
            token => {
                let pos = self.current_position();
                self.expected_token(Fn, pos)
            }
        }
    }

    fn trait_bound(&mut self) -> Result<Type> {
        let mut paths = Vec::new();
        paths.push(self.qualified_path()?);
        while self.matches(Plus) {
            self.advance();
            paths.push(self.qualified_path()?);
        }
        Ok(TraitBounds(paths))
    }

    fn enum_members(&mut self) -> Result<Vec<EnumMemberStmt>> {
        self.parse_multiple_with_scope_delimiter::<EnumMemberStmt, 1>(
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
                self.advance();

                let params = self.parenthesized_params()?;
                let member_funcs = self.fn_stmts()?;

                Ok(EnumMemberStmt::new(ident, params, member_funcs))
            }
            token => {
                let pos = self.current_position();
                self.expected_token(Identifier(self.string_interner.get_or_intern("")), pos)
            }
        }
    }

    fn parenthesized_params(&mut self) -> Result<Params> {
        let params = self.parse_multiple_with_scope_delimiter::<Param, 1>(
            Self::parameter,
            Comma,
            LeftParentheses,
            RightParentheses,
        )?;
        Ok(Params::new(params))
    }

    fn parameter(&mut self) -> Result<Param> {
        let mutability = self.mutability();
        let ident;
        let ty;
        if self.matches(SelfLowercase) {
            self.advance();
            ident = self.string_interner.get_or_intern("self");
            ty = Infer;
        } else {
            ident = self.identifier()?;
            self.expect(Colon)?;
            ty = self.parse_ty()?;
        }

        Ok(Param::new(ident, ty, mutability))
    }

    fn function_signature(&mut self) -> Result<FnSig> {
        let identifier = self.identifier()?;
        let generics = self.generics()?;
        let params = self.parenthesized_params()?;
        let mut ty = None;
        if self.matches(RightArrow) {
            self.advance();
            ty = Some(self.parse_ty()?);
        }

        Ok(FnSig::new(identifier, generics, params, ty))
    }

    fn block_statement(&mut self) -> Result<BlockStmt> {
        self.expect(LeftBrace)?;
        let stmts = self.parse_stmts()?;
        self.expect(RightBrace)?;
        Ok(BlockStmt::new(stmts))
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
        self.bounds_check()?;
        match self.current_type() {
            LeftBracket => {
                self.parse_array_ty()
            }
            Identifier(ident) => {
                match self.string_interner.resolve(&ident) {
                    "u8" => {
                        self.advance();
                        Ok(Basic(U8))
                    }
                    "u16" => {
                        self.advance();
                        Ok(Basic(U16))
                    }
                    "u32" => {
                        self.advance();
                        Ok(Basic(U32))
                    }
                    "u64" => {
                        self.advance();
                        Ok(Basic(U64))
                    }
                    "i8" => {
                        self.advance();
                        Ok(Basic(I8))
                    }
                    "i16" => {
                        self.advance();
                        Ok(Basic(I16))
                    }
                    "i32" => {
                        self.advance();
                        Ok(Basic(I32))
                    }
                    "i64" => {
                        self.advance();
                        Ok(Basic(I64))
                    }
                    "f32" => {
                        self.advance();
                        Ok(Basic(F32))
                    }
                    "f64" => {
                        self.advance();
                        Ok(Basic(F64))
                    }
                    "None" => {
                        self.advance();
                        Ok(Basic(BasicType::None))
                    }
                    token => {
                        self.parse_qualified_ty()
                    }
                }
            }
            TokenType::None => {
                self.advance();
                Ok(Basic(BasicType::None))
            }
            _ => {
                Ok(Infer)
            }
        }
    }

    fn parse_array_ty(&mut self) -> Result<Type> {
        self.expect(LeftBracket)?;
        let ty = self.parse_ty()?;
        self.expect(RightBracket)?;
        Ok(Type::Array(Box::new(ty)))
    }

    fn parse_qualified_ty(&mut self) -> Result<Type> {
        let path = self.qualified_path()?;
        self.bounds_check()?;

        match self.current_type() {
            TokenType::Pipe => {
                self.advance();
                let mut types = self.parse_multiple_with_delimiter(Self::parse_qualified_ty, TokenType::Pipe)?;
                types.insert(0, Path(path));
                Ok(Union(types))
            }
            Plus => {
                self.advance();
                let mut paths = self.parse_multiple_with_delimiter(Self::qualified_path, Plus)?;
                paths.insert(0, path);
                Ok(TraitBounds(paths))
            }
            _ => {
                Ok(Path(path))
            }
        }
    }

    fn qualified_path(&mut self) -> Result<PathStmt> {
        let ident = self.qualified_ident()?;
        let generics = self.generics()?;
        Ok(PathStmt::new(ident, generics))
    }

    fn parse_multiple_with_delimiter<T>(&mut self,
                                        parse_rule: fn(&mut Parser) -> Result<T>,
                                        delimiter: TokenType) -> Result<Vec<T>> {
        let mut items = Vec::new();
        loop {
            self.bounds_check()?;
            items.push(parse_rule(self)?);
            if self.matches(delimiter) {
                self.advance();
            } else {
                break;
            }
        }
        Ok(items)
    }

    fn parse_multiple_with_scope<T>(
        &mut self,
        parse_rule: fn(&mut Parser) -> Result<T>,
        scope_start: TokenType,
        scope_end: TokenType,
    ) -> Result<Vec<T>> {
        let mut items = Vec::new();
        if self.matches(scope_start) {
            self.advance();
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

    fn parse_multiple_with_scope_delimiter<T, const N: usize>(
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
                self.bounds_check()?;
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
        println!("{}", self.current_type());
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
        match self {
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
        }
    }
}

impl Error for ParseError {}

unsafe impl Send for ParseError {}
unsafe impl Sync for ParseError {}

mod tests {
    use crate::compiler::ast::ast::Mutability::{Immutable, Mutable};
    use crate::compiler::ast::ast::Stmt::Enum;
    use crate::compiler::ast::ast::{Args, BlockStmt, EnumMemberStmt, EnumStmt, Expr, FnCall, FnSig, FnStmt, GenericTy, Generics, Mutability, Param, Params, QualifiedIdent, Stmt, LocalStmt};
    use crate::compiler::ast::ast::{Module, UseStmt};
    use crate::compiler::parser::ParseError::{ExpectedToken, UnexpectedEof};
    use crate::compiler::parser::{ParseError, Parser};
    use crate::compiler::tokens::token::TokenType;
    use crate::compiler::tokens::token::TokenType::Identifier;
    use crate::compiler::tokens::tokenized_file::{TokenPosition, TokenizedInput};
    use crate::compiler::tokens::tokenizer::tokenize;
    use crate::compiler::types::types::Type;
    use crate::compiler::types::types::Type::{Array, Basic, TraitBounds};
    use crate::compiler::StringInterner;
    use anyhow::{anyhow, Result};
    use std::any::Any;
    use std::error::Error;
    use std::fs::File;
    use std::io::{BufReader, BufWriter};
    use std::path::{Path, PathBuf};
    use std::sync::Arc;
    use cfg_if::cfg_if;
    use ::function_name::named;
    use crate::compiler::types::types::BasicType::{U8, U16, U32, U64, I8, I16, I32, I64, F32, F64};
    use crate::compiler::types::types::BasicType;

    cfg_if! {
        if #[cfg(test)] {
            use crate::util::utils::{load, save};
            use crate::util::utils::resolve_test_path;
        }
    }

    #[cfg(test)]
    fn load_module(path: &str) -> Result<Module> {
        let file = File::open(resolve_test_path("parser", path))?;
        let reader = BufReader::new(file);
        Ok(serde_json::from_reader(reader)?)
    }

    #[cfg(test)]
    fn save_module(path: &str, module: Module) -> Result<()> {
        let file = File::open(resolve_test_path("parser", path))?;
        let writer = BufWriter::new(file);
        Ok(serde_json::to_writer(writer, &module)?)
    }

    #[cfg(test)]
    fn compare_modules(test: &str, code: &str) {
        let (string_interner, module) = parse_code(code).unwrap();
        if let Ok(loaded) = load::<Module>("parser", test) {
            assert_eq!(loaded, module);
        } else {
            save("parser", test, module).expect("Error saving module!");
        }
    }

    fn parse_code(code: &str) -> Result<(StringInterner, Module)> {
        let string_interner = StringInterner::default();
        let tokens = tokenize(string_interner.clone(), code).unwrap();
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

    macro_rules! simple_type {
        ($typ:expr, $fn_name:ident, $code:literal) => {
            #[test]
            pub fn $fn_name() {
                let (string_interner, module) = parse_code(concat!("let x: ", $code, ";")).unwrap();
                let ty = Some($typ);

                assert_eq!(vec![
                    Stmt::Local(Box::new(LocalStmt::new(string_interner.get("x").unwrap(), ty, None)))
                ], module.stmts());
            }
        }
    }

    simple_type!(Basic(U8), u8_type, "u8");
    simple_type!(Basic(U16), u16_type, "u16");
    simple_type!(Basic(U32), u32_type, "u32");
    simple_type!(Basic(U64), u64_type, "u64");
    simple_type!(Basic(I8), i8_type, "i8");
    simple_type!(Basic(I16), i16_type, "i16");
    simple_type!(Basic(I32), i32_type, "i32");
    simple_type!(Basic(I64), i64_type, "i64");
    simple_type!(Basic(F32), f32_type, "f32");
    simple_type!(Basic(F64), f64_type, "f64");
    simple_type!(Basic(BasicType::None), none_type, "None");
    simple_type!(Array(Box::new(Basic(U8))), u8_array_type, "[u8]");
    simple_type!(Array(Box::new(Basic(U16))), u16_array_type, "[u16]");
    simple_type!(Array(Box::new(Basic(U32))), u32_array_type, "[u32]");
    simple_type!(Array(Box::new(Basic(U64))), u64_array_type, "[u64]");
    simple_type!(Array(Box::new(Basic(I8))), i8_array_type, "[i8]");
    simple_type!(Array(Box::new(Basic(I16))), i16_array_type, "[i16]");
    simple_type!(Array(Box::new(Basic(I32))), i32_array_type, "[i32]");
    simple_type!(Array(Box::new(Basic(I64))), i64_array_type, "[i64]");
    simple_type!(Array(Box::new(Basic(F32))), f32_array_type, "[f32]");
    simple_type!(Array(Box::new(Basic(F64))), f64_array_type, "[f64]");
    simple_type!(Array(Box::new(Basic(BasicType::None))), none_array_type, "[None]");

    #[test]
    #[named]
    pub fn use_statements() {
        let code = concat!(
            "use std::vector;",
            "use std::array;",
            "use std::map::HashMap;"
        );

        compare_modules(function_name!(), code);
    }

    #[test]
    #[named]
    pub fn basic_enum() {
        let code = concat!(
            "enum Planet {\n",
            "    Mercury,\n",
            "    Venus,\n",
            "    Earth,\n",
            "    Mars,\n",
            "    Jupiter,\n",
            "    Saturn,\n",
            "    Uranus,\n",
            "    Neptune,\n",
            "    Pluto,\n",
            "}"
        );

        compare_modules(function_name!(), code);
    }

    #[test]
    #[named]
    pub fn complex_enum() {
        let code = concat!(
            "enum Vector<X: Number + Display, Y: Number + Display> {\n",
            "    Normalized(x: X, y: Y),\n",
            "    Absolute(x: X, y: Y) {\n",
            "        fn to_normalized(self) => Vector {\n",
            "            return Normalized(self.x, self.y);\n",
            "        }\n",
            "    }\n",
            "}"
        );

        compare_modules(function_name!(), code);
    }
}
