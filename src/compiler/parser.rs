use std::error::Error;
use std::fmt::Alignment::Right;
use std::fmt::{Display, Formatter};
use std::sync::Arc;

use anyhow::{anyhow, Result};
use serde::{Deserialize, Serialize};

use crate::class::compiled_class::CompiledClass;
use crate::compiler::ast::ast::Stmt::{Enum, For, If, Return, While};
use crate::compiler::ast::ast::{BlockStmt, ClassStmt, ClosureExpr, EnumMemberStmt, EnumStmt, Expr, FnSig, FnStmt, ForStmt, GenericCallSite, GenericDecl, GenericDecls, IfStmt, LetStmt, Literal, Module, Mutability, Param, Params, PathTy, QualifiedIdent, ReturnStmt, Stmt, TraitImplStmt, TraitStmt, UnaryExpr, UnaryOp, UseStmt, WhileStmt};
use crate::compiler::parser::ParseError::{ExpectedToken, UnexpectedEof, UnexpectedToken};
use crate::compiler::tokens::token::{Token, TokenType};
use crate::compiler::tokens::tokenized_file::{TokenPosition, TokenizedInput};
use crate::compiler::types::types::BasicType::{F32, F64, I16, I32, I64, I8, U16, U32, U64, U8};
use crate::compiler::types::types::Type::{
    Basic, Closure, ImplicitSelf, Infer, Path, TraitBounds, Union,
};
use crate::compiler::types::types::{BasicType, InternedStr, Type};
use crate::compiler::StringInterner;
use crate::gc::block::Block;

pub fn parse(string_interner: StringInterner, input: TokenizedInput) -> Result<Module> {
    let mut parser = Parser::new(string_interner, input);
    parser.parse()
}

struct Parser {
    string_interner: StringInterner,
    tokenized_input: TokenizedInput,
    token_types: Vec<TokenType>,
    pos: usize,
}

#[derive(Debug)]
enum ParseError {
    UnexpectedEof(TokenPosition),
    ExpectedToken(TokenType, TokenPosition),
    UnexpectedToken(TokenType, TokenPosition),
}

#[derive(PartialEq, Eq, Debug, Serialize, Deserialize)]
pub enum ClassType {
    Reference,
    Inline,
}

impl Parser {
    fn new(string_interner: StringInterner, tokenized_input: TokenizedInput) -> Self {
        let token_types = tokenized_input
            .tokens
            .iter()
            .map(|f| f.token_type)
            .collect();
        Self {
            string_interner,
            tokenized_input,
            token_types,
            pos: 0,
        }
    }

    fn parse(&mut self) -> Result<Module> {
        let stmts = self.parse_outer_stmts()?;
        Ok(Module::new(stmts))
    }

    fn parse_outer_stmts(&mut self) -> Result<Vec<Stmt>> {
        let mut stmts = Vec::new();
        while !self.is_at_end() {
            self.bounds_check()?;
            stmts.push(self.parse_outer_stmt()?);
        }
        Ok(stmts)
    }

    fn parse_outer_stmt(&mut self) -> Result<Stmt> {
        self.bounds_check()?;
        match self.current_type() {
            TokenType::Use => self.parse_use_stmt(),
            TokenType::Class => self.parse_class_stmt(),
            TokenType::Fn => self.parse_fn_stmt(),
            TokenType::Let => self.parse_let_stmt(),
            TokenType::Enum => self.parse_enum_stmt(),
            TokenType::Trait => self.parse_trait_stmt(),
            TokenType::Impl => self.parse_trait_impl_stmt(),
            TokenType::For => self.parse_for_stmt(),
            TokenType::Inline => self.parse_class_stmt(),
            TokenType::If => self.parse_if_stmt(),
            TokenType::Return => self.parse_return_stmt(),
            TokenType::While => self.parse_while_stmt(),
            TokenType::LeftBracket => self.parse_block_stmt(),

            token => {
                let pos = self.current_position();
                self.unexpected_token(token, pos)
            }
        }
    }

    fn parse_use_stmt(&mut self) -> Result<Stmt> {
        Ok(Stmt::Use(self.use_stmt()?))
    }

    fn use_stmt(&mut self) -> Result<UseStmt> {
        self.expect(TokenType::Use)?;
        let identifier = self.qualified_ident()?;
        self.expect(TokenType::Semicolon)?;
        Ok(UseStmt::new(identifier))
    }

    fn parse_class_stmt(&mut self) -> Result<Stmt> {
        Ok(Stmt::Class(self.class_stmt()?))
    }

    fn class_stmt(&mut self) -> Result<ClassStmt> {
        let class_type = if self.matches(TokenType::Inline) {
            self.advance();
            self.expect(TokenType::Class)?;
            ClassType::Inline
        } else {
            self.expect(TokenType::Class)?;
            ClassType::Reference
        };
        let name = self.identifier()?;
        let generic_types = self.generic_decls()?;
        let members = self.parenthesized_params()?;
        let member_functions = self.fn_stmts()?;

        let class_stmt = ClassStmt::new(name, class_type, generic_types, members, member_functions);

        Ok(class_stmt)
    }

    fn parse_fn_stmt(&mut self) -> Result<Stmt> {
        Ok(Stmt::Fn(self.fn_stmt()?))
    }

    fn fn_stmt(&mut self) -> Result<FnStmt> {
        self.expect(TokenType::Fn)?;
        let signature = self.function_signature()?;
        let stmt = self.block_stmt()?;

        Ok(FnStmt::new(signature, Some(stmt)))
    }

    fn parse_let_stmt(&mut self) -> Result<Stmt> {
        Ok(Stmt::Let(self.let_stmt()?))
    }

    fn let_stmt(&mut self) -> Result<LetStmt> {
        self.expect(TokenType::Let)?;
        let identifier = self.identifier()?;
        let mut ty = None;
        let mut initializer = None;
        if self.matches(TokenType::Colon) {
            self.advance();
            ty = Some(self.parse_ty()?);
        }
        if self.matches(TokenType::Equal) {
            self.advance();
            initializer = Some(self.expr()?);
        }
        self.expect(TokenType::Semicolon)?;
        let let_stmt = LetStmt::new(identifier, ty, initializer);

        Ok(let_stmt)
    }

    fn parse_enum_stmt(&mut self) -> Result<Stmt> {
        Ok(Enum(self.enum_stmt()?))
    }

    fn enum_stmt(&mut self) -> Result<EnumStmt> {
        self.expect(TokenType::Enum)?;
        let name = self.identifier()?;
        let generics = self.generic_decls()?;
        let enum_members = self.enum_members()?;

        let enum_stmt = EnumStmt::new(name, generics, enum_members);

        Ok(enum_stmt)
    }

    fn parse_trait_stmt(&mut self) -> Result<Stmt> {
        Ok(Stmt::Trait(self.trait_stmt()?))
    }

    fn trait_stmt(&mut self) -> Result<TraitStmt> {
        self.expect(TokenType::Trait)?;
        let identifier = self.identifier()?;
        let generics = self.generic_decls()?;
        if self.matches(TokenType::Semicolon) {
            Ok(TraitStmt::new(identifier, generics, Vec::new()))
        } else {
            Ok(TraitStmt::new(identifier, generics, self.fn_stmts()?))
        }
    }

    fn parse_trait_impl_stmt(&mut self) -> Result<Stmt> {
        Ok(Stmt::TraitImpl(self.trait_impl_stmt()?))
    }

    fn trait_impl_stmt(&mut self) -> Result<TraitImplStmt> {
        self.expect(TokenType::Impl)?;
        let trait_to_impl = self.parse_path_ty()?;
        self.expect(TokenType::For)?;
        let target_ty = self.parse_ty()?;

        todo!()
    }

    fn parse_expression(&mut self) -> Result<Stmt> {
        Ok(Stmt::Expression(self.expr()?))
    }

    fn expr(&mut self) -> Result<Expr> {
        self.bounds_check()?;
        if self.matches_closure() {
            return self.parse_closure_expr();
        }
        self.parse_assignment_expr(0)
    }

    fn matches_closure(&mut self) -> bool {
        if !self.matches(TokenType::LeftParentheses) {
            println!("Did not match parentheses");
            return false;
        }
        let mut lookahead = self.pos + 1;
        let matches_closure;
        loop {
            match self.next(lookahead) {
                TokenType::Identifier(ident) => match self.next(lookahead + 1) {
                    TokenType::Comma => {
                        println!("Matched ident and comma");
                        lookahead += 2
                    }
                    TokenType::RightParentheses => {
                        println!("Matched ident and right parentheses");
                        lookahead += 2;
                        matches_closure = true;
                        break;
                    }
                    token => {
                        println!("Unexpected token: {}", token);
                        matches_closure = false;
                        break;
                    }
                },
                token => {
                    println!("Unexpected token: {}", token);
                    matches_closure = false;
                    break;
                }
            }
        }
        if !matches_closure {
            println!("Did not match closure!");
            return false;
        }

        self.next(lookahead) == TokenType::RightArrow
    }

    fn parse_closure_expr(&mut self) -> Result<Expr> {
        let vars = self.parse_multiple_with_scope_delimiter::<InternedStr, 1>(
            Self::identifier,
            TokenType::Comma,
            TokenType::LeftParentheses,
            TokenType::RightParentheses,
        )?;

        self.expect(TokenType::RightArrow)?;

        let stmt = if self.matches(TokenType::LeftBrace) {
            self.parse_block_stmt()?
        } else {
            self.parse_expression()?
        };

        Ok(Expr::Closure(Box::new(ClosureExpr::new(vars, stmt))))
    }

    fn identifier(&mut self) -> Result<InternedStr> {
        self.bounds_check()?;
        match self.current_type() {
            TokenType::Identifier(ident) => {
                self.advance();
                Ok(ident)
            }
            _ => {
                let pos = self.current_position();
                self.expected_token(
                    TokenType::Identifier(self.string_interner.get_or_intern("")),
                    pos,
                )
            }
        }
    }

    fn float_literal(&mut self, float: f64) -> Result<Expr> {
        Ok(Expr::Literal(Literal::FloatLiteral(float)))
    }

    fn integer_literal(&mut self, integer: i64) -> Result<Expr> {
        Ok(Expr::Literal(Literal::IntLiteral(integer)))
    }

    fn string_literal(&mut self, interned: InternedStr) -> Result<Expr> {
        Ok(Expr::Literal(Literal::StringLiteral(interned)))
    }

    fn qualified_ident(&mut self) -> Result<QualifiedIdent> {
        let mut idents = Vec::new();
        loop {
            self.bounds_check()?;
            match self.current_type() {
                TokenType::Identifier(ident) => {
                    idents.push(ident);
                    if self.matches_multiple([
                        TokenType::Identifier(ident),
                        TokenType::Colon,
                        TokenType::Colon,
                    ]) {
                        self.advance_multiple(3);
                    } else {
                        self.advance();
                        break;
                    }
                }
                token => {
                    let pos = self.current_position();
                    return self.expected_token(
                        TokenType::Identifier(self.string_interner.get_or_intern("")),
                        pos,
                    );
                }
            }
        }

        Ok(QualifiedIdent::new(idents))
    }

    fn generic_call_site(&mut self) -> Result<GenericCallSite> {
        let generic_tys = self.parse_multiple_with_scope_delimiter::<Type, 1>(
            Self::parse_ty,
            TokenType::Comma,
            TokenType::Less,
            TokenType::Greater,
        )?;
        Ok(GenericCallSite::new(generic_tys))
    }

    fn generic_decls(&mut self) -> Result<GenericDecls> {
        let generic_tys = self.parse_multiple_with_scope_delimiter::<GenericDecl, 1>(
            Self::generic_decl,
            TokenType::Comma,
            TokenType::Less,
            TokenType::Greater,
        )?;
        Ok(GenericDecls::new(generic_tys))
    }

    fn generic_decl(&mut self) -> Result<GenericDecl> {
        let ident = self.identifier()?;
        let mut trait_bound: Option<Type> = None;
        if self.matches(TokenType::Colon) {
            self.advance();
            trait_bound = Some(self.trait_bound()?);
        }
        Ok(GenericDecl::new(ident, trait_bound))
    }

    fn fn_stmts(&mut self) -> Result<Vec<FnStmt>> {
        self.parse_multiple_with_scope(Self::fn_stmt, TokenType::LeftBrace, TokenType::RightBrace)
    }

    fn trait_bound(&mut self) -> Result<Type> {
        let mut paths = Vec::new();
        paths.push(self.parse_path_ty()?);
        while self.matches(TokenType::Plus) {
            self.advance();
            paths.push(self.parse_path_ty()?);
        }
        Ok(TraitBounds(paths))
    }

    fn enum_members(&mut self) -> Result<Vec<EnumMemberStmt>> {
        self.parse_multiple_with_scope_delimiter::<EnumMemberStmt, 1>(
            Self::enum_member,
            TokenType::Comma,
            TokenType::LeftBrace,
            TokenType::RightBrace,
        )
    }

    fn enum_member(&mut self) -> Result<EnumMemberStmt> {
        self.bounds_check()?;
        match self.current_type() {
            TokenType::Identifier(ident) => {
                self.advance();

                let params = self.parenthesized_params()?;
                let member_funcs = self.fn_stmts()?;

                Ok(EnumMemberStmt::new(ident, params, member_funcs))
            }
            token => {
                let pos = self.current_position();
                self.expected_token(
                    TokenType::Identifier(self.string_interner.get_or_intern("")),
                    pos,
                )
            }
        }
    }

    fn parenthesized_params(&mut self) -> Result<Params> {
        let params = self.parse_multiple_with_scope_delimiter::<Param, 1>(
            Self::parameter,
            TokenType::Comma,
            TokenType::LeftParentheses,
            TokenType::RightParentheses,
        )?;
        Ok(Params::new(params))
    }

    fn parameter(&mut self) -> Result<Param> {
        let mutability = self.mutability();
        let ident;
        let ty;
        if self.matches(TokenType::SelfLowercase) {
            self.advance();
            ident = self.string_interner.get_or_intern("self");
            ty = Infer;
        } else {
            ident = self.identifier()?;
            self.expect(TokenType::Colon)?;
            ty = self.parse_ty()?;
        }

        Ok(Param::new(ident, ty, mutability))
    }

    fn function_signature(&mut self) -> Result<FnSig> {
        let identifier = self.identifier()?;
        let generics = self.generic_decls()?;
        let params = self.parenthesized_params()?;
        let mut ty = None;
        if self.matches(TokenType::RightArrow) {
            self.advance();
            ty = Some(self.parse_ty()?);
        }

        Ok(FnSig::new(identifier, generics, params, ty))
    }

    fn block_stmt(&mut self) -> Result<BlockStmt> {
        self.expect(TokenType::LeftBrace)?;
        let stmts = self.parse_outer_stmts()?;
        self.expect(TokenType::RightBrace)?;
        Ok(BlockStmt::new(stmts))
    }

    fn parse_block_stmt(&mut self) -> Result<Stmt> {
        Ok(Stmt::Block(self.block_stmt()?))
    }

    fn mutability(&mut self) -> Mutability {
        if self.matches(TokenType::Mut) {
            self.advance();
            Mutability::Mutable
        } else {
            Mutability::Immutable
        }
    }

    fn parse_ty(&mut self) -> Result<Type> {
        self.bounds_check()?;
        let mut tys = self.parse_multiple_with_delimiter(Self::parse_any_ty, TokenType::BitwiseOr)?;
        if tys.len() > 1 {
            Ok(Union(tys))
        } else {
            Ok(tys.remove(0))
        }
    }

    fn parse_any_ty(&mut self) -> Result<Type> {
        match self.current_type() {
            TokenType::LeftParentheses => self.parse_closure_ty(),
            TokenType::LeftBracket => self.parse_array_ty(),
            TokenType::Identifier(ident) => match self.string_interner.resolve(&ident) {
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
                token => self.parse_qualified_ty(),
            },
            TokenType::None => {
                self.advance();
                Ok(Basic(BasicType::None))
            }
            TokenType::SelfCapitalized => {
                self.advance();
                Ok(ImplicitSelf)
            }
            _ => Ok(Infer),
        }
    }

    fn parse_closure_ty(&mut self) -> Result<Type> {
        let tys = self.parse_multiple_with_scope_delimiter::<Type, 1>(
            Self::parse_ty,
            TokenType::Comma,
            TokenType::LeftParentheses,
            TokenType::RightParentheses,
        )?;
        self.expect(TokenType::RightArrow)?;

        let return_ty = match self.current_type() {
            TokenType::LeftParentheses => {
                self.expect(TokenType::LeftParentheses)?;
                let closure_ty = self.parse_closure_ty()?;
                self.expect(TokenType::RightParentheses)?;
                closure_ty
            }
            _ => self.parse_ty()?,
        };

        Ok(Closure(tys, Box::new(return_ty)))
    }

    fn parse_array_ty(&mut self) -> Result<Type> {
        self.expect(TokenType::LeftBracket)?;
        let ty = self.parse_ty()?;
        self.expect(TokenType::RightBracket)?;
        Ok(Type::Array(Box::new(ty)))
    }

    fn parse_qualified_ty(&mut self) -> Result<Type> {
        let path = self.parse_path_ty()?;
        self.bounds_check()?;

        match self.current_type() {
            TokenType::Plus => {
                self.advance();
                let mut paths =
                    self.parse_multiple_with_delimiter(Self::parse_path_ty, TokenType::Plus)?;
                paths.insert(0, path);
                Ok(TraitBounds(paths))
            }
            _ => Ok(Path(path)),
        }
    }

    fn parse_array_expr(&mut self) -> Result<Expr> {
        todo!()
    }

    fn parse_if_stmt(&mut self) -> Result<Stmt> {
        self.expect(TokenType::If)?;
        let condition = self.expr()?;
        let block_stmt = self.block_stmt()?;
        let optional_stmt = if self.matches(TokenType::Else) {
            self.advance();
            Some(self.block_stmt()?)
        } else {
            None
        };

        Ok(If(IfStmt::new(condition, block_stmt, optional_stmt)))
    }

    fn parse_while_stmt(&mut self) -> Result<Stmt> {
        Ok(While(self.while_stmt()?))
    }

    fn while_stmt(&mut self) -> Result<WhileStmt> {
        self.expect(TokenType::While)?;
        let condition = self.expr()?;
        let block_stmt = self.block_stmt()?;

        Ok(WhileStmt::new(condition, block_stmt))
    }

    fn parse_for_stmt(&mut self) -> Result<Stmt> {
        self.expect(TokenType::For)?;
        let identifier = self.identifier()?;
        self.expect(TokenType::In)?;

        let range_expr = self.expr()?;
        let body = self.block_stmt()?;
        Ok(For(ForStmt::new(range_expr, body)))
    }

    fn parse_match_expr(&mut self) -> Result<Expr> {
        todo!()
    }

    fn parse_break_expr(&mut self) -> Result<Expr> {
        Ok(Expr::Break)
    }

    fn parse_continue_expr(&mut self) -> Result<Expr> {
        Ok(Expr::Continue)
    }

    fn parse_return_stmt(&mut self) -> Result<Stmt> {
        self.expect(TokenType::Return)?;
        if self.matches(TokenType::Semicolon) {
            self.advance();
            Ok(Return(ReturnStmt::new(None)))
        } else {
            let expression = self.expr()?;
            let return_stmt = ReturnStmt::new(Some(expression));

            Ok(Return(return_stmt))
        }
    }

    fn parse_path_expr(&mut self, ident: InternedStr) -> Result<Expr> {
        let path = self.parse_path_ty()?;
        Ok(Expr::Path(Box::new(path)))
    }

    fn parse_unary_expr(&mut self, operator: UnaryOp) -> Result<Expr> {
        Ok(Expr::Unary(Box::new(UnaryExpr::new(
            operator,
            self.expr()?,
        ))))
    }


    fn parse_assignment_expr(&mut self, min_binding_power: u8) -> Result<Expr> {
        // Check for prefix operator
        let lhs = if let Some(unaryOp) = prefix_op(self.current_type()) {
            let ((), prefix_bp) = unaryOp.prefix_binding_power();
            let rhs = self.parse_assignment_expr(prefix_bp)?;
            Expr::Unary(Box::new(UnaryExpr::new(unaryOp, rhs)))
        } else {
            match self.current_type() {
                TokenType::SelfLowercase => Expr::SelfRef,
                
            }
        }
        let lhs = match self.current_type() {
            if let Some(unaryOp) = prefix_op()
            TokenType::Minus | TokenType::Plus | TokenType::BitwiseComplement | TokenType::Bang => {
                let ((), prefix_bp) = prefix_op(self.current_type());
                self.parse_assignment_expr(prefix_bp);
                Expr::Unary(Box::new(UnaryExpr()))
            }
            token => {

            }
        }

        let mut lhs = match self.current_type() {

            // Handle literals
            TokenType::True => {
                Expr::Literal(Literal::BooleanLiteral(true))
            }
            TokenType::False => {
                Expr::Literal(Literal::BooleanLiteral(false))
            }
            TokenType::None => {
                Expr::Literal(Literal::None)
            }
            TokenType::SignedInteger(int) => {
                Expr::Literal(Literal::IntLiteral(int))
            }
            TokenType::Float(float) => {
                Expr::Literal(Literal::FloatLiteral(float))
            }
            TokenType::String(string) => {
                Expr::Literal(Literal::StringLiteral(string))
            }

            // Handle self
            TokenType::SelfLowercase => {
                Expr::SelfRef
            }

            // Handle ident
            TokenType::Identifier(ident) => {
                Expr::Identifier(ident)
            }

            // Handle parenthesized expression
            TokenType::LeftParentheses => {
                let inner_lhs = self.parse_assignment_expr(0)?;
                self.expect(TokenType::RightParentheses)?;
                inner_lhs
            }

            // Handle index expression
            TokenType::LeftBracket => {
                let inner_lhs = self.parse_assignment_expr(0)?;
                inner_lhs
            }

            // TODO: Handle operators
            TokenType::Equal ||

                token => {
            let pos = self.current_position();
            return self.unexpected_token(token, pos);
            }
        };
        todo!()
    }

    fn parse_parenthesized_expr(&mut self) -> Result<Expr> {
        todo!()
    }

    fn parse_path_ty(&mut self) -> Result<PathTy> {
        let ident = self.qualified_ident()?;
        let generic_decls = self.generic_decls()?;
        Ok(PathTy::new(ident, generic_decls))
    }

    fn parse_multiple_with_delimiter<T>(
        &mut self,
        parse_rule: fn(&mut Parser) -> Result<T>,
        delimiter: TokenType,
    ) -> Result<Vec<T>> {
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

            if self.matches(scope_end) {
                self.advance();
                return Ok(items);
            }

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

            if self.matches(scope_end) {
                self.advance();
                return Ok(items);
            }

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
        self.tokenized_input.tokens[self.pos]
    }

    fn current_type(&mut self) -> TokenType {
        self.tokenized_input.tokens[self.pos].token_type
    }

    fn current_position(&mut self) -> TokenPosition {
        let token = self.current();
        self.tokenized_input.token_position(token.start)
    }

    fn last(&mut self) -> Token {
        self.tokenized_input.tokens[&self.tokenized_input.tokens.len() - 1]
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

    fn matches_multiple<const N: usize>(&mut self, tokens: [TokenType; N]) -> bool {
        if self.token_types.len() < self.pos + tokens.len() {
            return false;
        }
        tokens == self.token_types[self.pos..self.pos + tokens.len()]
    }

    fn is_at_end(&self) -> bool {
        self.pos >= self.token_types.len()
    }

    fn remaining(&self) -> usize {
        self.token_types.len() - (self.pos + 1)
    }

    fn next(&self, delta: usize) -> TokenType {
        self.token_types[self.pos + delta]
    }
}

fn infix_bp(token: TokenType) -> (u8, u8) {
    match token {
        TokenType::Plus | TokenType::Minus => (1, 2),
        TokenType::Star | TokenType::Slash => (3, 4),
        token => panic!("Unexpected infix operator {}!", token)
    }
}

fn prefix_op(token: TokenType) -> Option<UnaryOp> {
    match token {
        TokenType::Bang => Some(UnaryOp::Bang),
        TokenType::Plus => Some(UnaryOp::Plus),
        TokenType::Minus => Some(UnaryOp::Minus),
        TokenType::BitwiseComplement => Some(UnaryOp::BitwiseComplement),
        _ => None
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
    use std::any::Any;
    use std::error::Error;
    use std::fmt::Debug;
    use std::fs::File;
    use std::io::{BufReader, BufWriter};
    use std::path::{Path, PathBuf};
    use std::sync::Arc;

    use ::function_name::named;
    use anyhow::{anyhow, Result};
    use cfg_if::cfg_if;
    use lasso::ThreadedRodeo;
    use serde::de::DeserializeOwned;
    use serde::{Deserialize, Serialize};

    use crate::compiler::ast::ast::Mutability::{Immutable, Mutable};
    use crate::compiler::ast::ast::Stmt::Enum;
    use crate::compiler::ast::ast::{
        Args, BlockStmt, EnumMemberStmt, EnumStmt, Expr, FnCall, FnSig, FnStmt, GenericDecl,
        GenericDecls, LetStmt, Mutability, Param, Params, QualifiedIdent, Stmt,
    };
    use crate::compiler::ast::ast::{Module, UseStmt};
    use crate::compiler::parser::ParseError::{ExpectedToken, UnexpectedEof};
    use crate::compiler::parser::{ParseError, Parser};
    use crate::compiler::tokens::token::TokenType;
    use crate::compiler::tokens::token::TokenType::Identifier;
    use crate::compiler::tokens::tokenized_file::{TokenPosition, TokenizedInput};
    use crate::compiler::tokens::tokenizer::tokenize;
    use crate::compiler::types::types::BasicType;
    use crate::compiler::types::types::BasicType::{
        F32, F64, I16, I32, I64, I8, U16, U32, U64, U8,
    };
    use crate::compiler::types::types::Type;
    use crate::compiler::types::types::Type::{Array, Basic, Closure, TraitBounds};
    use crate::compiler::StringInterner;

    cfg_if! {
        if #[cfg(test)] {
            use crate::util::utils::{load, save, resolve_test_path};
        }
    }

    #[cfg(test)]
    enum CodeUnit {
        Module,
        Expression,
        Ty,
    }

    #[cfg(test)]
    impl CodeUnit {
        fn folder_path(self, test_name: &str) -> Box<Path> {
            let folder = match self {
                CodeUnit::Module => "module",
                CodeUnit::Ty => "type",
                CodeUnit::Expression => "expression",
            };

            resolve_test_path(["parser", folder, test_name])
        }
    }

    #[cfg(test)]
    fn run_test(test_name: &str, code: &str, code_unit: CodeUnit) {
        match code_unit {
            CodeUnit::Module => {
                inner_compare(
                    parse_code(code, Parser::parse).unwrap(),
                    code_unit.folder_path(test_name),
                );
            }
            CodeUnit::Ty => inner_compare(
                parse_code(code, Parser::parse_ty).unwrap(),
                code_unit.folder_path(test_name),
            ),
            CodeUnit::Expression => {
                inner_compare(
                    parse_code(code, Parser::expr).unwrap(),
                    code_unit.folder_path(test_name),
                );
            }
        }
    }

    #[cfg(test)]
    fn inner_compare<T: DeserializeOwned + Serialize + Debug + PartialEq>(
        value: T,
        path: Box<Path>,
    ) {
        if let Ok(loaded) = load::<T>(&path) {
            assert_eq!(loaded, value);
        } else {
            save(&path, value).expect("Error saving data!");
        }
    }

    #[cfg(test)]
    fn parse_code<T>(code: &str, parser_func: fn(&mut Parser) -> Result<T>) -> Result<T> {
        let (interner, mut parser) = create_parser(code);
        parser_func(&mut parser)
    }

    #[cfg(test)]
    fn parse_module(code: &str) -> Result<(StringInterner, Module)> {
        let (string_interner, mut parser) = create_parser(code);
        let module = parser.parse()?;

        Ok((string_interner, module))
    }

    #[cfg(test)]
    fn create_parser(code: &str) -> (Arc<ThreadedRodeo>, Parser) {
        let string_interner = StringInterner::default();
        let tokens = tokenize(string_interner.clone(), code).unwrap();
        let parser = Parser::new(string_interner.clone(), tokens);
        (string_interner, parser)
    }

    #[test]
    pub fn invalid_use_stmts() {
        match parse_module("use std::vector::") {
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

        match parse_module("use std::vector::Vector") {
            Ok(_) => panic!(),
            Err(error) => match error.downcast_ref::<ParseError>() {
                Some(ExpectedToken(token, _)) => {
                    assert_eq!(*token, TokenType::Semicolon)
                }
                _ => panic!(),
            },
        }

        match parse_module("use;") {
            Ok(_) => panic!(),
            Err(error) => match error.downcast_ref::<ParseError>() {
                Some(ExpectedToken(Identifier(_), pos)) => {}
                _ => panic!(),
            },
        }

        match parse_module("use") {
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
    #[named]
    pub fn closure_return_closure() {
        let code = "() => (() => None)";
        run_test(function_name!(), code, CodeUnit::Ty);
    }

    #[test]
    #[named]
    pub fn closure_returns_trait_bound_or_none() {
        let code = "() => [first::party::package::Send<V: std::Copy + std::Clone> + third::party::package::Sync<T> + std::Copy + std::Clone] | None";
        run_test(function_name!(), code, CodeUnit::Ty);
    }

    #[test]
    #[named]
    pub fn closure_expression() {
        let code = "(x, y) => x + y";
        run_test(function_name!(), code, CodeUnit::Expression)
    }

    macro_rules! simple_type {
        ($typ:expr, $fn_name:ident, $code:literal) => {
            #[test]
            pub fn $fn_name() {
                let (string_interner, module) =
                    parse_module(concat!("let x: ", $code, ";")).unwrap();
                let ty = Some($typ);

                assert_eq!(
                    vec![Stmt::Let(LetStmt::new(
                        string_interner.get("x").unwrap(),
                        ty,
                        None
                    ))],
                    module.stmts()
                );
            }
        };
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
    simple_type!(
        Array(Box::new(Basic(BasicType::None))),
        none_array_type,
        "[None]"
    );

    #[test]
    #[named]
    pub fn use_statements() {
        let code = concat!(
        "use std::vector;",
        "use std::array;",
        "use std::map::HashMap;"
        );

        run_test(function_name!(), code, CodeUnit::Module);
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

        run_test(function_name!(), code, CodeUnit::Module);
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

        // compare(function_name!(), code, CodeUnit::Module);
    }
}
