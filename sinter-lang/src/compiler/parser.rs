use std::error::Error;
use std::fmt::Alignment::Right;
use std::fmt::{Display, Formatter};
use std::ops::Deref;
use std::sync::Arc;

use anyhow::{anyhow, Result};
use serde::{Deserialize, Serialize};

use crate::class::compiled_class::CompiledClass;
use crate::compiler::ast::Expr::Infix;
use crate::compiler::ast::Mutability::{Immutable, Mutable};
use crate::compiler::ast::Stmt::{For, If, Return, While};
use crate::compiler::ast::{Args, ArrayExpr, BlockStmt, Call, ClassStmt, ClosureExpr, EnumMemberStmt, EnumStmt, Expr, Field, FieldExpr, Fields, FnSig, FnStmt, ForStmt, GenericCallSite, GenericParam, GenericParams, IfStmt, IndexExpr, InfixExpr, InfixOp, LetStmt, MatchArm, MatchExpr, Module, Mutability, OrPattern, OuterStmt, Param, Params, PathExpr, PathSegment, PathTy, Pattern, PostfixOp, QualifiedIdent, Range, ReturnStmt, Stmt, TraitBound, TraitImplStmt, TraitStmt, UnaryExpr, UnaryOp, UseStmt, WhileStmt};
use crate::compiler::interner::{Interner, Key};
use crate::compiler::parser::ParseError::{
    ExpectedToken, ExpectedTokens, UnexpectedEof, UnexpectedToken,
};
use crate::compiler::tokens::token::{Token, TokenType};
use crate::compiler::tokens::tokenized_file::{TokenSpan, TokenizedInput};
use crate::compiler::types::types::Type::{Closure, F32, F64, I16, I32, I64, I8, Infer, Path, QSelf, Str, U16, U32, U64, U8, Union};
use crate::compiler::types::types::{InternedStr, InternedTy, Type};
use crate::compiler::{StringInterner, TyInterner};
use crate::compiler::ast::OuterStmt::{Class, Enum, Fn, Trait, TraitImpl, Use};
use crate::compiler::compiler::CompilerCtxt;
use crate::gc::block::Block;

pub fn parse(ctxt: CompilerCtxt, input: TokenizedInput) -> Result<(CompilerCtxt, Module)> {
    let parser = Parser::new(ctxt, input);
    parser.parse()
}

struct Parser {
    compiler_ctxt: CompilerCtxt,
    tokenized_input: TokenizedInput,
    token_types: Vec<TokenType>,
    pos: usize,
}

#[derive(Debug)]
enum ParseError {
    UnexpectedEof(TokenSpan),
    ExpectedToken(TokenType, TokenSpan),
    ExpectedTokens(TokenTypes, TokenSpan),
    UnexpectedToken(TokenType, TokenSpan),
}

#[derive(Debug)]
struct TokenTypes {
    token_types: Vec<TokenType>,
}

impl TokenTypes {
    pub fn new(token_types: Vec<TokenType>) -> Self {
        Self { token_types }
    }
}

impl Display for TokenTypes {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;
        self.token_types.iter().fold(Ok(()), |result, token_type| {
            result.and_then(|_| write!(f, "{token_type}"))
        })?;
        write!(f, "]")
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Serialize, Deserialize)]
pub enum ClassType {
    Reference,
    Inline,
}

impl Parser {
    fn new(ctxt: CompilerCtxt,
           tokenized_input: TokenizedInput) -> Self {
        let token_types = tokenized_input
            .tokens
            .iter()
            .map(|f| f.token_type)
            .collect();
        Self {
            compiler_ctxt: ctxt,
            tokenized_input,
            token_types,
            pos: 0,
        }
    }

    fn parse(mut self) -> Result<(CompilerCtxt, Module)> {
        let stmts = self.parse_outer_stmts()?;
        Ok((self.compiler_ctxt, Module::new(stmts)))
    }

    fn parse_outer_stmts(&mut self) -> Result<Vec<OuterStmt>> {
        let mut stmts = Vec::new();
        while self.pos < self.token_types.len() {
            stmts.push(self.parse_outer_stmt()?);
        }
        Ok(stmts)
    }

    fn parse_outer_stmt(&mut self) -> Result<OuterStmt> {
        if let Some(current) = self.current() {
            match current {
                TokenType::Use => self.parse_use_stmt(),
                TokenType::Ref => self.parse_class_stmt(),
                TokenType::Class => self.parse_class_stmt(),
                TokenType::Fn => self.parse_fn_stmt(),
                TokenType::Let => self.parse_outer_let_stmt(),
                TokenType::Enum => self.parse_enum_stmt(),
                TokenType::Trait => self.parse_trait_stmt(),
                TokenType::Impl => self.parse_trait_impl_stmt(),

                token => self.unexpected_token(token),
            }
        } else {
            self.unexpected_end()
        }
    }

    fn parse_inner_stmt(&mut self) -> Result<Stmt> {
        if let Some(current) = self.current() {
            match current {
                TokenType::Let => self.parse_let_stmt(),
                TokenType::For => self.parse_for_stmt(),
                TokenType::While => self.parse_while_stmt(),
                TokenType::LeftBrace => self.parse_block_stmt(),
                TokenType::Return => self.parse_return_stmt(),
                TokenType::If => self.parse_if_stmt(),
                token => self.parse_expression(),
            }
        } else {
            self.unexpected_end()
        }
    }

    fn parse_use_stmt(&mut self) -> Result<OuterStmt> {
        Ok(Use(self.use_stmt()?))
    }

    fn use_stmt(&mut self) -> Result<UseStmt> {
        self.expect(TokenType::Use)?;
        let identifier = self.qualified_ident()?;
        self.expect(TokenType::Semicolon)?;
        Ok(UseStmt::new(identifier))
    }

    fn parse_type_definition(&mut self) -> Result<OuterStmt> {
        if let Some(current) = self.current() {
            match current {
                TokenType::Ref => self.parse_class_stmt(),
                TokenType::Class => self.parse_class_stmt(),
                TokenType::Enum => self.parse_enum_stmt(),
                TokenType::Trait => self.parse_trait_stmt(),
                token => self.unexpected_token(token),
            }
        } else {
            self.unexpected_end()
        }
    }

    fn parse_class_stmt(&mut self) -> Result<OuterStmt> {
        Ok(Class(self.class_stmt()?))
    }

    fn class_stmt(&mut self) -> Result<ClassStmt> {
        let class_type = if self.matches(TokenType::Ref) {
            self.advance();
            self.expect(TokenType::Class)?;
            ClassType::Reference
        } else {
            self.expect(TokenType::Class)?;
            ClassType::Inline
        };
        let name = self.identifier()?;
        let generic_types = self.generic_params()?;
        let fields = self.fields()?;
        let member_functions = if self.matches(TokenType::LeftBrace) {
            self.fn_stmts()?
        } else if self.matches(TokenType::Semicolon) {
            self.advance();
            Vec::new()
        } else {
            return self.expected_tokens(vec![TokenType::LeftBrace, TokenType::Semicolon]);
        };

        let class_stmt = ClassStmt::new(name, class_type, generic_types, fields, member_functions);

        Ok(class_stmt)
    }

    fn parse_fn_stmt(&mut self) -> Result<OuterStmt> {
        Ok(Fn(self.fn_stmt()?))
    }

    fn fn_stmt(&mut self) -> Result<FnStmt> {
        let signature = self.fn_signature(false)?;
        let stmt = self.block_stmt()?;

        Ok(FnStmt::new(signature, Some(stmt)))
    }

    fn fn_trait_stmt(&mut self) -> Result<FnStmt> {
        let signature = self.fn_signature(true)?;
        match self.current() {
            Some(TokenType::Semicolon) => {
                self.advance();
                Ok(FnStmt::new(signature, None))
            }
            Some(TokenType::LeftBrace) => {
                let stmt = self.block_stmt()?;
                Ok(FnStmt::new(signature, Some(stmt)))
            }
            Some(token) => self.unexpected_token(token),
            None => self.unexpected_end(),
        }
    }

    fn parse_outer_let_stmt(&mut self) -> Result<OuterStmt> {
        Ok(OuterStmt::Let(self.let_stmt()?))
    }

    fn parse_let_stmt(&mut self) -> Result<Stmt> {
        Ok(Stmt::Let(self.let_stmt()?))
    }

    fn let_stmt(&mut self) -> Result<LetStmt> {
        self.expect(TokenType::Let)?;
        let mutability = match self.current() {
            Some(TokenType::Mut) => {
                self.advance();
                Mutable
            }
            Some(TokenType::Identifier(_)) => Immutable,
            _ => {
                let ident = self.intern_str("");
                return self.expected_tokens(vec![
                    TokenType::Mut,
                    TokenType::Identifier(ident),
                ]);
            }
        };
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
        let let_stmt = LetStmt::new(identifier, mutability, ty, initializer);

        Ok(let_stmt)
    }

    fn parse_enum_stmt(&mut self) -> Result<OuterStmt> {
        Ok(Enum(self.enum_stmt()?))
    }

    fn enum_stmt(&mut self) -> Result<EnumStmt> {
        self.expect(TokenType::Enum)?;
        let name = self.identifier()?;
        let generics = self.generic_params()?;
        let enum_members = self.enum_members()?;

        let object_fns = if self.matches(TokenType::LeftBrace) {
            self.fn_stmts()?
        } else {
            self.expect(TokenType::Semicolon)?;
            Vec::new()
        };

        let enum_stmt = EnumStmt::new(name, generics, enum_members, object_fns);

        Ok(enum_stmt)
    }

    fn parse_trait_stmt(&mut self) -> Result<OuterStmt> {
        Ok(Trait(self.trait_stmt()?))
    }

    fn trait_stmt(&mut self) -> Result<TraitStmt> {
        self.expect(TokenType::Trait)?;
        let identifier = self.identifier()?;
        let generics = self.generic_params()?;
        match self.current() {
            Some(TokenType::Semicolon) => Ok(TraitStmt::new(identifier, generics, Vec::new())),
            Some(TokenType::LeftBrace) => {
                Ok(TraitStmt::new(identifier, generics, self.fn_trait_stmts()?))
            }
            Some(token) => self.unexpected_token(token),
            None => self.unexpected_end(),
        }
    }

    fn parse_trait_impl_stmt(&mut self) -> Result<OuterStmt> {
        Ok(TraitImpl(self.trait_impl_stmt()?))
    }

    fn trait_impl_stmt(&mut self) -> Result<TraitImplStmt> {
        self.expect(TokenType::Impl)?;
        let trait_to_impl = self.parse_path_ty()?;
        self.expect(TokenType::For)?;
        let target_ty = self.parse_ty()?;

        match self.current() {
            Some(TokenType::Semicolon) => {
                self.advance();
                Ok(TraitImplStmt::new(trait_to_impl, target_ty, Vec::new()))
            }
            Some(TokenType::LeftBrace) => Ok(TraitImplStmt::new(
                trait_to_impl,
                target_ty,
                self.fn_trait_stmts()?,
            )),
            Some(token) => self.unexpected_token(token),
            None => self.unexpected_end(),
        }
    }

    fn parse_expression(&mut self) -> Result<Stmt> {
        let expr = self.expr()?;
        let implicit_return = if self.matches(TokenType::Semicolon) {
            self.advance();
            true
        } else {
            false
        };
        Ok(Stmt::Expression {
            expr,
            implicit_return,
        })
    }

    fn expr(&mut self) -> Result<Expr> {
        if self.matches_closure() {
            return self.parse_closure_expr();
        }
        self.parse_expr(0)
    }

    fn matches_closure(&mut self) -> bool {
        if !self.matches(TokenType::LeftParentheses) {
            return false;
        }
        let mut lookahead = 1;
        let matches_closure;
        loop {
            if self.remaining() < 2 {
                return false;
            }
            match self.next_type(lookahead) {
                Some(TokenType::Identifier(ident)) => match self.next_type(lookahead + 1) {
                    Some(TokenType::Comma) => lookahead += 2,
                    Some(TokenType::RightParentheses) => {
                        lookahead += 2;
                        matches_closure = true;
                        break;
                    }
                    token => {
                        matches_closure = false;
                        break;
                    }
                },
                token => {
                    matches_closure = false;
                    break;
                }
            }
        }
        if !matches_closure {
            return false;
        }

        self.next_type(lookahead) == Some(TokenType::RightArrow)
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
            let expr = self.expr()?;
            Stmt::Expression {
                expr,
                implicit_return: true,
            }
        };

        Ok(Expr::Closure(Box::new(ClosureExpr::new(vars, stmt))))
    }

    fn identifier(&mut self) -> Result<InternedStr> {
        if let Some(TokenType::Identifier(ident)) = self.current() {
            self.advance();
            Ok(ident)
        } else {
            let ident = self.intern_str("");
            self.expected_token(TokenType::Identifier(
                ident
            ))
        }
    }

    fn qualified_ident(&mut self) -> Result<QualifiedIdent> {
        let mut idents = Vec::new();
        loop {
            if let Some(TokenType::Identifier(ident)) = self.current() {
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
            } else {
                let ident = self.intern_str("");
                return self.expected_token(TokenType::Identifier(
                    ident
                ));
            }
        }

        if idents.is_empty() {
            let ident = self.intern_str("");
            self.expected_token(TokenType::Identifier(
                ident
            ))
        } else {
            Ok(QualifiedIdent::new(idents))
        }
    }

    fn generic_call_site(&mut self) -> Result<GenericCallSite> {
        let generic_tys = self.parse_multiple_with_scope_delimiter::<Key, 1>(
            Self::parse_ty,
            TokenType::Comma,
            TokenType::Less,
            TokenType::Greater,
        )?;
        Ok(GenericCallSite::new(generic_tys))
    }

    fn generic_params(&mut self) -> Result<GenericParams> {
        let generic_tys = self.parse_multiple_with_scope_delimiter::<GenericParam, 1>(
            Self::generic_param,
            TokenType::Comma,
            TokenType::Less,
            TokenType::Greater,
        )?;
        Ok(GenericParams::new(generic_tys))
    }

    fn generic_param(&mut self) -> Result<GenericParam> {
        let ident = self.identifier()?;
        let mut trait_bound: Option<TraitBound> = None;
        if self.matches(TokenType::Colon) {
            self.advance();
            trait_bound = Some(self.trait_bound()?);
        }
        Ok(GenericParam::new(ident, trait_bound))
    }

    fn fn_stmts(&mut self) -> Result<Vec<FnStmt>> {
        self.parse_multiple_with_scope(Self::fn_stmt, TokenType::LeftBrace, TokenType::RightBrace)
    }

    fn fn_trait_stmts(&mut self) -> Result<Vec<FnStmt>> {
        self.parse_multiple_with_scope(
            Self::fn_trait_stmt,
            TokenType::LeftBrace,
            TokenType::RightBrace,
        )
    }

    fn trait_bound(&mut self) -> Result<TraitBound> {
        let mut paths = Vec::new();
        paths.push(self.parse_path_ty()?);
        while self.matches(TokenType::Plus) {
            self.advance();
            paths.push(self.parse_path_ty()?);
        }
        let trait_bound = TraitBound::new(paths);
        Ok(trait_bound)
    }

    fn enum_members(&mut self) -> Result<Vec<EnumMemberStmt>> {
        self.parse_multiple_with_scope_delimiter::<EnumMemberStmt, 1>(
            Self::enum_member,
            TokenType::Comma,
            TokenType::LeftParentheses,
            TokenType::RightParentheses,
        )
    }

    fn enum_member(&mut self) -> Result<EnumMemberStmt> {
        match self.current() {
            Some(TokenType::Identifier(ident)) => {
                self.advance();
                let fields = self.fields()?;
                let fn_stmts = if self.matches(TokenType::LeftBrace) {
                    self.fn_stmts()?
                } else {
                    Vec::new()
                };
                Ok(EnumMemberStmt::new(ident, fields, fn_stmts))
            }
            Some(token) => {
                let ident = self.intern_str("");
                self.expected_token(TokenType::Identifier(
                    ident
                ))
            },
            None => self.unexpected_end(),
        }
    }

    fn fields(&mut self) -> Result<Fields> {
        let fields = self.parse_multiple_with_scope_delimiter::<Field, 1>(
            Self::field,
            TokenType::Comma,
            TokenType::LeftParentheses,
            TokenType::RightParentheses,
        )?;
        Ok(Fields::new(fields))
    }

    fn params(&mut self) -> Result<Params> {
        let params = self.parse_multiple_with_scope_delimiter::<Param, 1>(
            Self::param,
            TokenType::Comma,
            TokenType::LeftParentheses,
            TokenType::RightParentheses,
        )?;
        Ok(Params::new(params))
    }

    fn self_params(&mut self) -> Result<Params> {
        let mut params = Vec::new();
        self.expect(TokenType::LeftParentheses)?;
        params.push(self.self_param()?);
        if self.matches(TokenType::Comma) {
            self.advance();
            params.extend(self.parse_multiple_with_delimiter(Self::param, TokenType::Comma)?);
        }
        self.expect(TokenType::RightParentheses)?;
        Ok(Params::new(params))
    }

    fn field(&mut self) -> Result<Field> {
        let (mutability, ident, ty) = self.mut_ident_ty()?;
        Ok(Field::new(ident, ty, mutability))
    }

    fn param(&mut self) -> Result<Param> {
        let (mutability, ident, ty) = self.mut_ident_ty()?;
        Ok(Param::new(ident, ty, mutability))
    }

    fn mut_ident_ty(&mut self) -> Result<(Mutability, InternedStr, Key)> {
        let mutability = self.mutability();
        let ident;
        let ty: Key;
        if self.matches(TokenType::SelfLowercase) {
            self.advance();
            ident = self.intern_str("self");
            ty = self.intern_ty(QSelf);
        } else {
            ident = self.identifier()?;
            self.expect(TokenType::Colon)?;
            ty = self.parse_ty()?;
        }
        Ok((mutability, ident, ty))
    }

    fn self_param(&mut self) -> Result<Param> {
        let mutability = self.mutability();
        if !self.matches(TokenType::SelfLowercase) {
            self.expected_token(TokenType::SelfLowercase)
        } else {
            self.advance();
            Ok(Param::new(
                self.intern_str("self"),
                self.intern_ty(QSelf),
                mutability,
            ))
        }
    }

    fn fn_signature(&mut self, require_self: bool) -> Result<FnSig> {
        self.expect(TokenType::Fn)?;
        let identifier = self.identifier()?;
        let generics = self.generic_params()?;
        let params = if require_self {
            self.self_params()?
        } else {
            self.params()?
        };
        let mut ty = None;
        if self.matches(TokenType::RightArrow) {
            self.advance();
            ty = Some(self.parse_ty()?);
        }

        Ok(FnSig::new(identifier, generics, params, ty))
    }

    fn block_stmt(&mut self) -> Result<BlockStmt> {
        let stmts = self.parse_multiple_with_scope(
            Self::parse_inner_stmt,
            TokenType::LeftBrace,
            TokenType::RightBrace,
        )?;
        Ok(BlockStmt::new(stmts))
    }

    fn parse_block_stmt(&mut self) -> Result<Stmt> {
        Ok(Stmt::Block(self.block_stmt()?))
    }

    fn mutability(&mut self) -> Mutability {
        if self.matches(TokenType::Mut) {
            self.advance();
            Mutable
        } else {
            Immutable
        }
    }

    fn parse_ty(&mut self) -> Result<Key> {
        let mut tys =
            self.parse_multiple_with_delimiter(Self::parse_any_ty, TokenType::BitwiseOr)?;
        if tys.len() > 1 {
            let ty = Union { tys };
            Ok(self.intern_ty(ty))
        } else {
            Ok(tys.remove(0))
        }
    }

    fn parse_any_ty(&mut self) -> Result<Key> {
        match self.current() {
            Some(TokenType::LeftParentheses) => self.parse_closure_ty(),
            Some(TokenType::LeftBracket) => self.parse_array_ty(),
            Some(TokenType::None) => {
                self.advance();
                Ok(self.intern_ty(Type::None))
            }
            Some(TokenType::Identifier(ident)) => {
                // These built in types are officially encoded as strings to avoid them being
                // tokenized as keywords.
                let ident = self.resolve_str(ident);
                match ident {
                    "u8" => {
                        self.advance();
                        Ok(self.intern_ty(U8))
                    }
                    "u16" => {
                        self.advance();
                        Ok(self.intern_ty(U16))
                    }
                    "u32" => {
                        self.advance();
                        Ok(self.intern_ty(U32))
                    }
                    "u64" => {
                        self.advance();
                        Ok(self.intern_ty(U64))
                    }
                    "i8" => {
                        self.advance();
                        Ok(self.intern_ty(I8))
                    }
                    "i16" => {
                        self.advance();
                        Ok(self.intern_ty(I16))
                    }
                    "i32" => {
                        self.advance();
                        Ok(self.intern_ty(I32))
                    }
                    "i64" => {
                        self.advance();
                        Ok(self.intern_ty(I64))
                    }
                    "f32" => {
                        self.advance();
                        Ok(self.intern_ty(F32))
                    }
                    "f64" => {
                        self.advance();
                        Ok(self.intern_ty(F64))
                    }
                    "str" => {
                        self.advance();
                        Ok(self.intern_ty(Str))
                    }
                    other => self.parse_qualified_ty(),
                }
            }
            Some(TokenType::SelfCapitalized) => {
                self.advance();
                Ok(self.intern_ty(QSelf))
            }
            Some(token) => Ok(self.intern_ty(Infer)),
            None => self.unexpected_end(),
        }
    }

    fn parse_closure_ty(&mut self) -> Result<Key> {
        let params = self.parse_multiple_with_scope_delimiter::<Key, 1>(
            Self::parse_ty,
            TokenType::Comma,
            TokenType::LeftParentheses,
            TokenType::RightParentheses,
        )?;
        self.expect(TokenType::RightArrow)?;

        if let Some(current) = self.current() {
            let ret_ty = match current {
                TokenType::LeftParentheses => {
                    self.expect(TokenType::LeftParentheses)?;
                    let closure_ty = self.parse_closure_ty()?;
                    self.expect(TokenType::RightParentheses)?;
                    closure_ty
                }
                _ => self.parse_ty()?,
            };
            Ok(self.intern_ty(Closure { params, ret_ty }))
        } else {
            self.unexpected_end()
        }
    }

    fn parse_array_ty(&mut self) -> Result<Key> {
        self.expect(TokenType::LeftBracket)?;
        let ty = Type::Array { ty: self.parse_ty()? };
        self.expect(TokenType::RightBracket)?;
        Ok(self.intern_ty(ty))
    }

    fn parse_qualified_ty(&mut self) -> Result<Key> {
        let path = self.parse_path_ty()?;

        if let Some(TokenType::Plus) = self.current() {
            self.advance();
            let mut paths =
                self.parse_multiple_with_delimiter(Self::parse_path_ty, TokenType::Plus)?;
            paths.insert(0, path);
            Ok(self.intern_ty(Type::TraitBound { trait_bound: TraitBound::new(paths) }))
        } else {
            Ok(self.intern_ty(Path { path }))
        }
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
        Ok(For(ForStmt::new(identifier, range_expr, body)))
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
            self.expect(TokenType::Semicolon)?;

            Ok(Return(ReturnStmt::new(Some(expression))))
        }
    }

    fn parse_path_expr(&mut self) -> Result<PathExpr> {
        let mut path_segments = Vec::new();
        let first_ident = self.identifier()?;
        path_segments.push(PathSegment::Identifier(first_ident));

        loop {
            if self.matches_multiple([TokenType::Colon, TokenType::Colon]) {
                self.advance_multiple(2);
                match self.current() {
                    Some(TokenType::Less) => {
                        let generic_paths = self.parse_multiple_with_scope_delimiter::<Key, 1>(
                            Self::parse_ty,
                            TokenType::Comma,
                            TokenType::Less,
                            TokenType::Greater,
                        )?;
                        path_segments.push(PathSegment::Generic(generic_paths));
                        if !self.matches_multiple([TokenType::Colon, TokenType::Colon]) {
                            return self.expected_token(TokenType::Colon);
                        }
                    }
                    Some(TokenType::Identifier(ident)) => {
                        path_segments.push(PathSegment::Identifier(ident));
                        self.advance();
                    }
                    Some(token) => {
                        let ident = self.intern_str("");
                        return self.expected_tokens(vec![
                            TokenType::Less,
                            TokenType::Identifier(ident),
                        ])
                    }
                    None => return self.unexpected_end(),
                }
            } else {
                break;
            }
        }

        Ok(PathExpr::new(path_segments))
    }

    fn parse_unary_expr(&mut self, operator: UnaryOp) -> Result<Expr> {
        Ok(Expr::Unary(Box::new(UnaryExpr::new(
            operator,
            self.expr()?,
        ))))
    }

    fn parse_expr(&mut self, min_bp: u8) -> Result<Expr> {
        // Check for prefix operator
        let mut lhs = if let Some(prefix_op) = self.prefix_op() {
            self.advance();
            let ((), prefix_bp) = prefix_op.binding_power();
            let rhs = self.parse_expr(prefix_bp)?;
            Expr::Unary(Box::new(UnaryExpr::new(prefix_op, rhs)))
        } else if let Some(current) = self.current() {
            let expr = match current {
                TokenType::SelfLowercase => {
                    self.advance();
                    let ident = self.intern_str("self");
                    Expr::Path(PathExpr::new(vec![PathSegment::Identifier(ident)]))
                }
                TokenType::True => {
                    self.advance();
                    Expr::Boolean(true)
                }
                TokenType::False => {
                    self.advance();
                    Expr::Boolean(false)
                }
                TokenType::SignedInteger(int) => {
                    self.advance();
                    Expr::Integer(int)
                }
                TokenType::Float(float) => {
                    self.advance();
                    Expr::Float(float)
                }
                TokenType::String(string) => {
                    self.advance();
                    Expr::String(string)
                }
                TokenType::Identifier(ident) => Expr::Path(self.parse_path_expr()?),
                TokenType::SelfCapitalized => {
                    self.advance();
                    let ident = self.intern_str("Self");
                    Expr::Path(PathExpr::new(vec![PathSegment::Identifier(ident)]))
                }
                TokenType::None => {
                    self.advance();
                    Expr::None
                }

                // Handle parenthesized expression
                TokenType::LeftParentheses => {
                    self.advance();
                    let expr = self.parse_expr(min_bp)?;
                    self.expect(TokenType::RightParentheses)?;
                    expr
                }

                // Handle array expression
                TokenType::LeftBracket => {
                    self.advance();
                    let expr = self.expr()?;
                    match self.current() {
                        Some(TokenType::Semicolon) => {
                            self.advance();
                            let size = self.expr()?;
                            self.expect(TokenType::RightBracket)?;
                            Expr::Array(Box::new(ArrayExpr::SizedInitializer(expr, size)))
                        }
                        Some(TokenType::Comma) => {
                            self.advance();
                            let mut exprs = vec![expr];
                            exprs.extend(
                                self.parse_multiple_with_delimiter(Self::expr, TokenType::Comma)?,
                            );
                            self.expect(TokenType::RightBracket)?;
                            Expr::Array(Box::new(ArrayExpr::Initializer(exprs)))
                        }
                        Some(token) => {
                            return self
                                .expected_tokens(vec![TokenType::Semicolon, TokenType::Comma]);
                        }
                        None => {
                            return self.unexpected_end();
                        }
                    }
                }

                // Handle match expression
                TokenType::Match => {
                    self.advance();
                    let source = self.expr()?;
                    let match_arms = self.parse_multiple_with_scope_delimiter::<MatchArm, 1>(
                        Self::parse_match_arm,
                        TokenType::Comma,
                        TokenType::LeftBrace,
                        TokenType::RightBrace,
                    )?;
                    Expr::Match(Box::new(MatchExpr::new(source, match_arms)))
                }

                // Handle unexpected token
                token => {
                    return self.unexpected_token(token);
                }
            };
            expr
        } else {
            return self.unexpected_end();
        };

        while let Some(current) = self.current() {
            if let Some(postfix_op) = self.postfix_op() {
                let (left_bp, ()) = postfix_op.binding_power();
                if left_bp < min_bp {
                    break;
                }

                lhs = match postfix_op {
                    PostfixOp::LeftParentheses => {
                        let args = self.parse_multiple_with_scope_delimiter::<Expr, 1>(
                            Self::expr,
                            TokenType::Comma,
                            TokenType::LeftParentheses,
                            TokenType::RightParentheses,
                        )?;
                        Expr::Call(Box::new(Call::new(lhs, Args::new(args))))
                    }
                    PostfixOp::LeftBracket => {
                        self.advance();
                        let rhs = self.expr()?;
                        self.expect(TokenType::RightBracket)?;
                        Expr::Index(Box::new(IndexExpr::new(lhs, rhs)))
                    }
                    PostfixOp::Dot => {
                        self.advance();
                        let ident = self.identifier()?;
                        Expr::Field(Box::new(FieldExpr::new(lhs, ident)))
                    }
                };
                continue;
            }

            if let Some(infix_op) = self.infix_op() {
                let (left_bp, right_bp) = infix_op.binding_power();

                if left_bp < min_bp {
                    break;
                }
                self.advance_multiple(infix_op.token_len());

                let rhs = self.parse_expr(right_bp)?;

                lhs = Infix(Box::new(InfixExpr::new(lhs, rhs, infix_op)));
                continue;
            }

            break;
        }

        Ok(lhs)
    }

    fn prefix_op(&mut self) -> Option<UnaryOp> {
        match self.current() {
            Some(TokenType::Bang) => Some(UnaryOp::Bang),
            Some(TokenType::Plus) => Some(UnaryOp::Plus),
            Some(TokenType::Minus) => Some(UnaryOp::Minus),
            Some(TokenType::BitwiseComplement) => Some(UnaryOp::BitwiseComplement),
            _ => None,
        }
    }

    fn infix_op(&mut self) -> Option<InfixOp> {
        match self.current() {
            Some(TokenType::Equal) => Some(InfixOp::Assign),
            Some(TokenType::Plus) => Some(InfixOp::Add),
            Some(TokenType::Minus) => Some(InfixOp::Subtract),
            Some(TokenType::Star) => Some(InfixOp::Multiply),
            Some(TokenType::Slash) => Some(InfixOp::Divide),
            Some(TokenType::Percent) => Some(InfixOp::Modulo),
            Some(TokenType::And) => Some(InfixOp::And),
            Some(TokenType::Or) => Some(InfixOp::Or),
            Some(TokenType::Less) => {
                if self.matches_multiple([TokenType::Less, TokenType::Less]) {
                    Some(InfixOp::LeftShift)
                } else {
                    Some(InfixOp::Less)
                }
            }
            Some(TokenType::LessEqual) => Some(InfixOp::LessEqual),
            Some(TokenType::Greater) => {
                if self.matches_multiple([
                    TokenType::Greater,
                    TokenType::Greater,
                    TokenType::Greater,
                ]) {
                    Some(InfixOp::TripleRightShift)
                } else if self.matches_multiple([TokenType::Greater, TokenType::Greater]) {
                    Some(InfixOp::RightShift)
                } else {
                    Some(InfixOp::Greater)
                }
            }
            Some(TokenType::GreaterEqual) => Some(InfixOp::GreaterEqual),
            Some(TokenType::EqualEqual) => Some(InfixOp::Equal),
            Some(TokenType::BangEqual) => Some(InfixOp::NotEqual),
            Some(TokenType::BitwiseOr) => Some(InfixOp::BitwiseOr),
            Some(TokenType::BitwiseAnd) => Some(InfixOp::BitwiseAnd),
            Some(TokenType::BitwiseComplement) => Some(InfixOp::BitwiseComplement),
            Some(TokenType::BitwiseXor) => Some(InfixOp::BitwiseXor),
            _ => None,
        }
    }

    fn postfix_op(&mut self) -> Option<PostfixOp> {
        match self.current() {
            Some(TokenType::LeftBracket) => Some(PostfixOp::LeftBracket),
            Some(TokenType::LeftParentheses) => Some(PostfixOp::LeftParentheses),
            Some(TokenType::Dot) => Some(PostfixOp::Dot),
            _ => None,
        }
    }

    fn parse_match_arm(&mut self) -> Result<MatchArm> {
        let mut patterns =
            self.parse_multiple_with_delimiter(Self::parse_pattern, TokenType::BitwiseOr)?;
        let pattern = if patterns.len() > 1 {
            Pattern::Or(OrPattern::new(patterns))
        } else {
            patterns.remove(0)
        };

        self.expect(TokenType::RightArrow)?;
        let stmt = if self.matches(TokenType::LeftBrace) {
            self.parse_block_stmt()?
        } else {
            Stmt::Expression {
                expr: self.expr()?,
                implicit_return: true,
            }
        };
        Ok(MatchArm::new(pattern, stmt))
    }

    fn parse_pattern(&mut self) -> Result<Pattern> {
        // TODO: Support range patterns and guards
        match self.current() {
            Some(TokenType::String(str)) => {
                self.advance();
                Ok(Pattern::String(str))
            }
            Some(TokenType::SignedInteger(integer)) => {
                self.advance();
                Ok(Pattern::Integer(integer))
            }
            Some(TokenType::True) => {
                self.advance();
                Ok(Pattern::Boolean(true))
            }
            Some(TokenType::False) => {
                self.advance();
                Ok(Pattern::Boolean(false))
            }
            Some(TokenType::Underscore) => {
                self.advance();
                Ok(Pattern::Wildcard)
            }
            Some(token_type) => {
                let ty = self.parse_any_ty()?;
                match self.current() {
                    Some(TokenType::LeftParentheses) => {
                        self.advance();
                        let exprs =
                            self.parse_multiple_with_delimiter(Self::expr, TokenType::Comma)?;
                        self.expect(TokenType::RightParentheses)?;
                        Ok(Pattern::Destructure(ty, exprs))
                    }
                    Some(TokenType::Identifier(str)) => {
                        self.advance();
                        Ok(Pattern::Ty(ty, Some(str)))
                    }
                    Some(TokenType::RightArrow) => Ok(Pattern::Ty(ty, None)),
                    Some(token) => self.unexpected_token(token),
                    None => self.unexpected_end(),
                }
            }
            None => self.unexpected_end(),
        }
    }

    fn parse_path_ty(&mut self) -> Result<PathTy> {
        let ident = self.qualified_ident()?;
        let generics = self.parse_multiple_with_scope_delimiter::<Key, 1>(
            Self::parse_ty,
            TokenType::Comma,
            TokenType::Less,
            TokenType::Greater,
        )?;
        Ok(PathTy::new(ident, generics))
    }

    fn parse_multiple_with_delimiter<'b, T>(
        &mut self,
        parse_rule: fn(&mut Parser) -> Result<T>,
        delimiter: TokenType,
    ) -> Result<Vec<T>> {
        let mut items = Vec::new();
        loop {
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
                items.push(parse_rule(self)?);
                if self.matches_multiple([delimiter; N]) {
                    self.advance_multiple(N);
                    if self.matches(scope_end) {
                        self.advance();
                        break;
                    }
                } else if self.matches(scope_end) {
                    self.advance();
                    break;
                } else {
                    return match self.current() {
                        Some(token) => self.unexpected_token(token),
                        None => self.unexpected_end(),
                    };
                }
            }
        }
        Ok(items)
    }
    
    fn intern_str(&mut self, str: &str) -> InternedStr {
        self.compiler_ctxt.intern_str(str)
    }

    fn resolve_str(&mut self, key: InternedStr) -> &str {
        self.compiler_ctxt.resolve_str(key)
    }
    
    fn intern_ty(&mut self, ty: Type) -> InternedTy {
        self.compiler_ctxt.intern_ty(ty)
    }

    fn expected_token<T>(&mut self, token_type: TokenType) -> Result<T> {
        let token_position = self.current_position().unwrap_or(self.last_position());
        Err(ExpectedToken(token_type, token_position).into())
    }

    fn expected_tokens<T>(&mut self, token_types: Vec<TokenType>) -> Result<T> {
        let token_position = self.current_position().unwrap_or(self.last_position());
        Err(ExpectedTokens(TokenTypes::new(token_types), token_position).into())
    }

    fn unexpected_token<T>(&mut self, token_type: TokenType) -> Result<T> {
        let token_position = self.current_position().unwrap_or(self.last_position());
        Err(UnexpectedToken(token_type, token_position).into())
    }

    fn unexpected_end<T>(&mut self) -> Result<T> {
        Err(UnexpectedEof(self.last_position()).into())
    }

    fn current_token(&mut self) -> Option<Token> {
        self.tokenized_input.tokens.get(self.pos).copied()
    }

    fn current(&self) -> Option<TokenType> {
        self.token_types.get(self.pos).copied()
    }

    fn next(&self, delta: usize) -> Option<Token> {
        self.tokenized_input.tokens.get(self.pos + delta).copied()
    }

    fn next_type(&self, delta: usize) -> Option<TokenType> {
        self.token_types.get(self.pos + delta).copied()
    }

    fn current_position(&mut self) -> Option<TokenSpan> {
        if let Some(token) = self.current_token() {
            Some(self.tokenized_input.token_position(token.span.start))
        } else {
            None
        }
    }

    fn next_position(&mut self, delta: usize) -> Option<TokenSpan> {
        self.next(delta)
            .map(|token| self.tokenized_input.token_position(token.span.start))
    }

    fn last(&mut self) -> Option<Token> {
        self.tokenized_input.tokens.last().copied()
    }

    fn last_position(&mut self) -> TokenSpan {
        self.last()
            .map(|token| self.tokenized_input.token_position(token.span.end))
            .unwrap_or(TokenSpan::new(0, 0))
    }

    fn advance(&mut self) {
        self.pos += 1;
    }

    fn advance_multiple(&mut self, amount: usize) {
        self.pos += amount;
    }

    fn expect(&mut self, token_type: TokenType) -> Result<()> {
        if let Some(current) = self.current() && current == token_type {
            self.advance();
            Ok(())
        } else {
            self.expected_token(token_type)
        }
    }

    fn matches(&self, token_type: TokenType) -> bool {
        if let Some(current) = self.current() && current == token_type {
            true
        } else {
            false
        }
    }

    fn matches_multiple<const N: usize>(&mut self, tokens: [TokenType; N]) -> bool {
        let end = self.pos + tokens.len();
        if self.token_types.len() < end {
            return false;
        }
        tokens == self.token_types[self.pos..end]
    }

    fn remaining(&self) -> usize {
        self.token_types.len() - (self.pos + 1)
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
            ExpectedTokens(token_types, pos) => write!(
                f,
                "expected tokens {} at {}:{}!",
                token_types, pos.line, pos.pos
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

    use anyhow::{anyhow, Result};
    use lasso::ThreadedRodeo;

    use snap::snapshot;

    use crate::compiler::ast::Mutability::{Immutable, Mutable};
    use crate::compiler::ast::{
        Args, BlockStmt, Call, EnumMemberStmt, EnumStmt, Expr, FnSig, FnStmt, LetStmt, Mutability,
        Param, Params, PathExpr, QualifiedIdent, Stmt,
    };
    use crate::compiler::ast::{Module, UseStmt, OuterStmt};
    use crate::compiler::parser::ParseError::{ExpectedToken, UnexpectedEof};
    use crate::compiler::parser::{ParseError, Parser};
    use crate::compiler::tokens::token::TokenType;
    use crate::compiler::tokens::token::TokenType::Identifier;
    use crate::compiler::tokens::tokenized_file::{TokenSpan, TokenizedInput};
    use crate::compiler::tokens::tokenizer::tokenize;
    use crate::compiler::types::types::Type;
    use crate::compiler::types::types::Type::{Array, Closure, TraitBound};
    use crate::compiler::{StringInterner, TyInterner};
    use crate::compiler::compiler::CompilerCtxt;
    use crate::util::utils;

    #[cfg(test)]
    fn create_parser(code: &str) -> Parser {
        let (ctxt, tokens) = tokenize(code).unwrap();
        Parser::new(ctxt, tokens)
    }

    #[cfg(test)]
    fn parse_module<T: AsRef<str>>(code: T) -> Result<(CompilerCtxt, Module)> {
        let parser = create_parser(code.as_ref());
        Ok(parser.parse()?)
    }

    #[cfg(test)]
    fn parse_code<T, I: AsRef<str>>(
        code: I,
        parser_func: fn(&mut Parser) -> Result<T>,
    ) -> Result<(CompilerCtxt, T)> {
        let mut parser = create_parser(code.as_ref());
        let parsed_val = parser_func(&mut parser)?;
        Ok((parser.compiler_ctxt, parsed_val))
    }

    #[cfg(test)]
    macro_rules! parse {
        ($code:expr) => {{
            parse_module($code).unwrap()
        }};
    }

    #[cfg(test)]
    macro_rules! parse_ty {
        ($code:expr) => {{
            let (ctxt, key) = parse_code($code, Parser::parse_ty).unwrap();
            let ty = ctxt.resolve_ty(key).clone();
            (StringInterner::from(ctxt), ty)
        }};
    }

    #[cfg(test)]
    macro_rules! parse_path {
        ($code:expr) => {{
            let (ctxt, path) = parse_code($code, Parser::parse_path_expr).unwrap();
            (StringInterner::from(ctxt), path)
        }};
    }

    #[cfg(test)]
    macro_rules! parse_expr {
        ($code:expr) => {{
            parse_code($code, Parser::expr).unwrap()
        }};
    }

    #[test]
    pub fn invalid_use_stmts() {
        match parse_module("use std::vector::") {
            Ok(module) => {
                panic!();
            }
            Err(error) => match error.downcast_ref::<ParseError>() {
                Some(ExpectedToken(Identifier(_), pos)) => {}
                _ => {
                    panic!();
                }
            },
        }

        match parse_module("use std::vector::Vector") {
            Ok(_) => panic!(),
            Err(error) => match error.downcast_ref::<ParseError>() {
                Some(ExpectedToken(TokenType::Semicolon, _)) => {}
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
                Some(ExpectedToken(Identifier(_), pos)) => {}
                _ => {
                    panic!()
                }
            },
        }
    }

    #[test]
    #[snapshot]
    pub fn closure_return_closure() -> (StringInterner, Type) {
        parse_ty!("() => (() => None)")
    }

    #[test]
    #[should_panic]
    pub fn bad_closure_returns_trait_bound_or_none() {
        parse_ty!("() => [first::party::package::Send<V: std::Copy + std::Clone> + third::party::package::Sync<T> + std::Copy + std::Clone] | None");
    }

    #[test]
    #[snapshot]
    pub fn closure_returns_trait_bound_or_none() -> (StringInterner, Type) {
        parse_ty!("() => [first::party::package::Send<K, V> + third::party::package::Sync<T> + std::Copy + std::Clone] | None")
    }

    #[test]
    #[snapshot]
    pub fn generic_type() -> (StringInterner, Type) {
        parse_ty!("List<List<i64>>")
    }

    #[test]
    #[snapshot]
    pub fn single_value() -> (CompilerCtxt, Expr) {
        parse_expr!("1")
    }

    #[test]
    #[snapshot]
    pub fn add_and_multiply() -> (CompilerCtxt, Expr) {
        parse_expr!("1 + 2 * 3")
    }

    #[test]
    #[snapshot]
    pub fn add_and_multiply_idents() -> (CompilerCtxt, Expr) {
        parse_expr!("a + b * c * d + e")
    }

    #[test]
    #[snapshot]
    pub fn function_composition() -> (CompilerCtxt, Expr) {
        parse_expr!("f(g(h()))")
    }

    #[test]
    #[snapshot]
    pub fn complex_function_composition() -> (CompilerCtxt, Expr) {
        parse_expr!("1 + 2 + f(g(h())) * 3 * 4")
    }

    #[test]
    #[snapshot]
    pub fn double_infix() -> (CompilerCtxt, Expr) {
        parse_expr!("--1 * 2")
    }

    #[test]
    #[snapshot]
    pub fn double_infix_call() -> (CompilerCtxt, Expr) {
        parse_expr!("--f(g)")
    }

    #[test]
    #[snapshot]
    pub fn parenthesized_expr() -> (CompilerCtxt, Expr) {
        parse_expr!("(((0)))")
    }

    #[test]
    #[snapshot]
    pub fn closure_expression() -> (CompilerCtxt, Expr) {
        parse_expr!("(x, y) => x + y")
    }

    #[test]
    #[snapshot]
    pub fn double_index_expression() -> (CompilerCtxt, Expr) {
        parse_expr!("x[0][1]")
    }

    #[test]
    #[snapshot]
    pub fn double_negate_and_multiply() -> (CompilerCtxt, Expr) {
        parse_expr!("--1 * 2")
    }

    #[test]
    #[snapshot]
    pub fn comparison() -> (CompilerCtxt, Expr) {
        parse_expr!("1 < 2")
    }

    #[test]
    #[snapshot]
    pub fn parenthesized_comparison() -> (CompilerCtxt, Expr) {
        parse_expr!("(1 + 2 * 4) < (2 - 1)")
    }

    #[test]
    #[snapshot]
    pub fn complex_conditional() -> (CompilerCtxt, Expr) {
        parse_expr!("year % 4 == 0 && year % 100 != 0 || year % 400 == 0")
    }

    #[test]
    #[snapshot]
    pub fn bit_operations() -> (CompilerCtxt, Expr) {
        parse_expr!("x + x * x / x - --x + 3 >> 1 | 2")
    }

    #[test]
    #[snapshot]
    pub fn single_path_expr() -> (StringInterner, PathExpr) {
        parse_path!("std")
    }

    #[test]
    #[snapshot]
    pub fn simple_path_expr() -> (StringInterner, PathExpr) {
        parse_path!("std::Clone")
    }

    #[test]
    #[snapshot]
    pub fn generic_path_expr() -> (StringInterner, PathExpr) {
        parse_path!("std::HashMap::<T>::new")
    }

    #[test]
    #[snapshot]
    pub fn nested_generic_path() -> (StringInterner, PathExpr) {
        parse_path!("List::<List<f64>>::new")
    }

    #[test]
    #[snapshot]
    pub fn generic_trait_bound_path() -> (StringInterner, PathExpr) {
        parse_path!("List::<Loggable + Serializable>::new")
    }

    #[test]
    #[snapshot]
    pub fn generic_union_path() -> (StringInterner, PathExpr) {
        parse_path!("List::<str | i64 | f64>::new")
    }
    macro_rules! simple_type {
        ($typ:expr, $fn_name:ident, $code:literal) => {
            #[test]
            pub fn $fn_name() {
                let (mut ctxt, module) = parse_module(concat!("let x: ", $code, ";")).unwrap();
                let ty = Some(ctxt.intern_ty($typ));

                assert_eq!(
                    vec![OuterStmt::Let(LetStmt::new(
                        ctxt.intern_str("x"),
                        Mutability::Immutable,
                        ty,
                        None
                    ))],
                    module.stmts()
                );
            }
        };
    }

    simple_type!(Type::Str, str_type, "str");
    simple_type!(Type::U8, u8_type, "u8");
    simple_type!(Type::U16, u16_type, "u16");
    simple_type!(Type::U32, u32_type, "u32");
    simple_type!(Type::U64, u64_type, "u64");
    simple_type!(Type::I8, i8_type, "i8");
    simple_type!(Type::I16, i16_type, "i16");
    simple_type!(Type::I32, i32_type, "i32");
    simple_type!(Type::I64, i64_type, "i64");
    simple_type!(Type::F32, f32_type, "f32");
    simple_type!(Type::F64, f64_type, "f64");
    simple_type!(Type::None, none_type, "None");
    // simple_type!(Array(Box::new(Basic(U8))), u8_array_type, "[u8]");
    // simple_type!(Array(Box::new(Basic(U16))), u16_array_type, "[u16]");
    // simple_type!(Array(Box::new(Basic(U32))), u32_array_type, "[u32]");
    // simple_type!(Array(Box::new(Basic(U64))), u64_array_type, "[u64]");
    // simple_type!(Array(Box::new(Basic(I8))), i8_array_type, "[i8]");
    // simple_type!(Array(Box::new(Basic(I16))), i16_array_type, "[i16]");
    // simple_type!(Array(Box::new(Basic(I32))), i32_array_type, "[i32]");
    // simple_type!(Array(Box::new(Basic(I64))), i64_array_type, "[i64]");
    // simple_type!(Array(Box::new(Basic(F32))), f32_array_type, "[f32]");
    // simple_type!(Array(Box::new(Basic(F64))), f64_array_type, "[f64]");
    // simple_type!(
    //     Array(Box::new(Basic(BasicType::None))),
    //     none_array_type,
    //     "[None]"
    // );
    // simple_type!(Array(Box::new(Basic(Str))), str_array_type, "[str]");

    #[test]
    #[snapshot]
    pub fn use_statements() -> (CompilerCtxt, Module) {
        let code = concat!(
            "use std::vector;",
            "use std::array;",
            "use std::map::HashMap;"
        );

        parse!(code)
    }

    #[test]
    #[snapshot]
    pub fn basic_enum() -> (CompilerCtxt, Module) {
        let code = concat!(
            "enum Planet(\n",
            "    Mercury,\n",
            "    Venus,\n",
            "    Earth,\n",
            "    Mars,\n",
            "    Jupiter,\n",
            "    Saturn,\n",
            "    Uranus,\n",
            "    Neptune,\n",
            "    Pluto,\n",
            ");"
        );

        parse!(code)
    }

    #[test]
    #[snapshot]
    pub fn vector_enum() -> (CompilerCtxt, Module) {
        parse!(utils::read_file(["short_examples", "vector_enum.si"]))
    }

    #[test]
    #[snapshot]
    pub fn main_fn() -> (CompilerCtxt, Module) {
        let code = concat!("fn main() {\n", "    print(\"Hello world!\");\n", "}");

        parse!(code)
    }

    #[test]
    #[snapshot]
    pub fn main_fn_with_args() -> (CompilerCtxt, Module) {
        let code = concat!(
            "fn main(args: [str]) {\n",
            "    print(args.to_str());\n",
            "}"
        );

        parse!(code)
    }

    #[test]
    #[snapshot]
    pub fn declare_classes_and_vars() -> (CompilerCtxt, Module) {
        let code = concat!(
            "class Point(x: f64, y: f64);\n",
            "ref class Node;\n",
            "let i32_array: [i32] = [1, 2, 3];\n",
            "let point_array = [Point(1.0, 2.0), Point(1.5, 2.5)];\n",
            "let node_array = [Node(), Node()];"
        );

        parse!(code)
    }

    #[test]
    #[snapshot]
    pub fn simple_add_func() -> (CompilerCtxt, Module) {
        let code = concat!("fn sum(a: i64, b: i64) => i64 {\n", "    a + b\n", "}");

        parse!(code)
    }

    #[test]
    #[snapshot]
    pub fn var_declarations() -> (CompilerCtxt, Module) {
        let code = concat!(
            "let a: i64 = 1; // Immediate assignment\n",
            "let b = 2; // `i64` type is inferred\n"
        );
        parse!(code)
    }

    #[test]
    #[snapshot]
    pub fn mutable_assignment() -> (CompilerCtxt, Module) {
        let code = concat!(
            "fn mut_var() {\n",
            "    let mut x = 5; // `i64` type is inferred\n",
            "    x = x + 1;\n",
            "}"
        );
        parse!(code)
    }

    #[test]
    #[snapshot]
    pub fn print_fn() -> (CompilerCtxt, Module) {
        let code = concat!("fn print(text: str) {\n", "    println(text);\n", "}");
        parse!(code)
    }

    #[test]
    #[snapshot]
    pub fn returning_error_union() -> (CompilerCtxt, Module) {
        parse!(utils::read_file([
            "short_examples",
            "returning_error_union.si"
        ]))
    }

    #[test]
    #[snapshot]
    pub fn trait_vs_generic() -> (CompilerCtxt, Module) {
        parse!(utils::read_file(["short_examples", "trait_vs_generic.si"]))
    }

    #[test]
    #[snapshot]
    pub fn generic_lists() -> (CompilerCtxt, Module) {
        parse!(utils::read_file(["short_examples", "generic_lists.si"]))
    }

    #[test]
    #[snapshot]
    pub fn rectangle_class() -> (CompilerCtxt, Module) {
        parse!(utils::read_file(["short_examples", "rectangle_class.si"]))
    }

    #[test]
    #[snapshot]
    pub fn enum_message() -> (CompilerCtxt, Module) {
        parse!(utils::read_file(["short_examples", "enum_message.si"]))
    }

    #[test]
    #[snapshot]
    pub fn int_match() -> (CompilerCtxt, Expr) {
        parse_expr!(utils::read_file(["short_examples", "int_match.si"]))
    }

    #[test]
    #[snapshot]
    pub fn enum_match() -> (CompilerCtxt, Module) {
        parse!(utils::read_file(["short_examples", "enum_match.si"]))
    }
}
