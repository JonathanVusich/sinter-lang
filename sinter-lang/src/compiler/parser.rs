use std::error::Error;
use std::fmt::Alignment::Right;
use std::fmt::{Display, Formatter};
use std::ops::Deref;
use std::sync::Arc;

use serde::{Deserialize, Serialize};

use crate::class::compiled_class::CompiledClass;
use crate::compiler::ast::Mutability::{Immutable, Mutable};
use crate::compiler::ast::{
    Args, ArrayExpr, Block, CallExpr, ClassStmt, ClosureExpr, ClosureParam, DestructureExpr,
    DestructureExprKind, DestructurePattern, EnumMember, EnumStmt, Expr, ExprKind, Expression,
    Field, FieldExpr, Fields, FnSelfStmt, FnSig, FnStmt, ForStmt, GenericCallSite, GenericParam,
    GenericParams, Generics, GlobalLetStmt, Ident, IdentType, IfStmt, IndexExpr, InfixExpr,
    InfixOp, Item, ItemKind, LetStmt, MatchArm, MatchExpr, Module, Mutability, OrPattern, Param,
    Params, Parentheses, PathExpr, PathTy, Pattern, PatternLocal, PostfixOp, QualifiedIdent, Range,
    ReturnStmt, Segment, Stmt, StmtKind, TraitBound, TraitImplStmt, TraitStmt, Ty, TyKind,
    TyPattern, UnaryExpr, UnaryOp, UseStmt, WhileStmt,
};
use crate::compiler::compiler::{CompileError, CompilerCtxt};
use crate::compiler::hir::LocalDefId;
use crate::compiler::interner::{Interner, Key};
use crate::compiler::parser::ParseError::{
    ExpectedToken, ExpectedTokens, UnexpectedEof, UnexpectedToken,
};
use crate::compiler::tokens::token::{Token, TokenType};
use crate::compiler::tokens::tokenized_file::{NormalizedSpan, Span, TokenizedInput};
use crate::compiler::types::{InternedStr, InternedTy};
use crate::compiler::StringInterner;

pub fn parse(ctxt: &mut CompilerCtxt, input: TokenizedInput) -> Result<Module, CompileError> {
    let parser = Parser::new(ctxt, input);
    parser.parse()
}

struct Parser<'ctxt> {
    compiler_ctxt: &'ctxt mut CompilerCtxt,
    tokenized_input: TokenizedInput,

    pos: usize,
    spans: Vec<usize>,
}

type ParseResult<T, E = ParseError> = Result<T, E>;

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub enum ParseError {
    UnexpectedEof(NormalizedSpan, ),
    ExpectedToken(TokenType, NormalizedSpan),
    ExpectedTokens(TokenTypes, NormalizedSpan),
    UnexpectedToken(TokenType, NormalizedSpan),

    /// Use statement with less than three segments (crate, module, item).
    InvalidUseStmt(NormalizedSpan),

    /// Duplicate definition in module
    DuplicateDefinition,
    /// Duplicate module name
    DuplicateModuleName,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct TokenTypes {
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

impl<'ctxt> Parser<'ctxt> {
    fn new(ctxt: &'ctxt mut CompilerCtxt, tokenized_input: TokenizedInput) -> Self {
        Self {
            compiler_ctxt: ctxt,
            tokenized_input,
            pos: 0,
            spans: Vec::new(),
        }
    }

    fn parse(mut self) -> Result<Module, CompileError> {
        self.parse_module()
    }

    fn get_id(&mut self) -> LocalDefId {
        self.compiler_ctxt.local_def_id()
    }

    fn track_span(&mut self) {
        self.spans.push(self.pos);
    }

    fn get_span(&mut self) -> Span {
        let first = self.spans.pop().expect("No start token found!");

        self.compute_span(first)
    }

    fn compute_span(&self, start: usize) -> Span {
        let start_token = self
            .tokenized_input
            .tokens
            .get(start)
            .expect("No valid start token!");
        let end_token = self
            .tokenized_input
            .tokens
            .get(self.pos - 1)
            .expect("No valid end token!");

        start_token.span.to(end_token.span)
    }

    fn parse_module(&mut self) -> Result<Module, CompileError> {
        let mut items = Vec::new();
        let mut errors = Vec::new();

        while self.pos < self.tokenized_input.tokens.len() {
            match self.parse_outer_item() {
                Ok(item) => items.push(item),
                Err(err) => {
                    errors.push(err);
                    // Clear out all currently tracked spans to avoid polluting future parsed items.
                    self.spans.clear();
                    // Ignore all tokens to the next semicolon since we have a malformed file
                    // TODO: Improve the error handling here
                    loop {
                        match self.current() {
                            Some(TokenType::Semicolon) => {
                                self.advance();
                                break;
                            }
                            None => {
                                break;
                            }
                            _ => self.advance(),
                        }
                    }
                }
            }
        }

        if errors.is_empty() {
            Ok(Module::new(items))
        } else {
            Err(CompileError::ParseErrors(errors))
        }
    }

    fn parse_node<T, I>(
        &mut self,
        parse_fn: for<'rf> fn(&'rf mut Parser<'ctxt>) -> ParseResult<T>,
        constructor: fn(T, Span, LocalDefId) -> I,
    ) -> ParseResult<I> {
        self.track_span();
        let value = parse_fn(self)?;
        Ok(constructor(value, self.get_span(), self.get_id()))
    }

    fn parse_item<U>(
        &mut self,
        parse_fn: fn(&mut Parser<'_>) -> ParseResult<U>,
        kind: fn(U) -> ItemKind,
    ) -> ParseResult<Item> {
        self.track_span();
        let item = parse_fn(self)?;
        let span = self.get_span();
        let id = self.get_id();

        let item_kind = kind(item);
        Ok(Item::new(item_kind, span, id))
    }

    fn parse_outer_item(&mut self) -> ParseResult<Item> {
        match self.current() {
            Some(TokenType::Use) => self.parse_use_stmt(),
            Some(TokenType::Ref) => self.parse_class_stmt(),
            Some(TokenType::Class) => self.parse_class_stmt(),
            Some(TokenType::Fn) => self.parse_fn_stmt(),
            Some(TokenType::Let) => self.parse_global_let_stmt(),
            Some(TokenType::Enum) => self.parse_enum_stmt(),
            Some(TokenType::Trait) => self.parse_trait_stmt(),
            Some(TokenType::Impl) => self.parse_trait_impl_stmt(),
            Some(token) => self.unexpected_token(token),
            None => self.unexpected_end(),
        }
    }

    fn parse_inner_stmt(&mut self) -> ParseResult<Stmt> {
        match self.current() {
            Some(TokenType::Let) => self.parse_let_stmt(),
            Some(TokenType::For) => self.parse_for_stmt(),
            Some(TokenType::While) => self.parse_while_stmt(),
            Some(TokenType::LeftBrace) => self.parse_block_stmt(),
            Some(TokenType::Return) => self.parse_return_stmt(),
            Some(TokenType::If) => self.parse_if_stmt(),
            Some(token) => self.parse_expression(),
            None => self.unexpected_end(),
        }
    }

    fn parse_use_stmt(&mut self) -> ParseResult<Item> {
        self.parse_item(|parser| parser.use_stmt(), ItemKind::Use)
    }

    fn use_stmt(&mut self) -> ParseResult<UseStmt> {
        self.expect(TokenType::Use)?;

        let qualified_ident = self.qualified_ident()?;
        self.expect(TokenType::Semicolon)?;

        Ok(UseStmt::new(qualified_ident))
    }

    fn parse_class_stmt(&mut self) -> ParseResult<Item> {
        self.parse_item(|parser| parser.class_stmt(), ItemKind::Class)
    }

    fn class_stmt(&mut self) -> ParseResult<ClassStmt> {
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
        let (fields, fn_stmts) = if self.matches(TokenType::LeftBrace) {
            self.class_stmt_inner()?
        } else {
            self.expect(TokenType::Semicolon)?;
            (Vec::new(), Vec::new())
        };

        let class_stmt = ClassStmt::new(name, class_type, generic_types, fields.into(), fn_stmts);
        Ok(class_stmt)
    }

    fn class_stmt_inner(&mut self) -> ParseResult<(Vec<Field>, Vec<FnSelfStmt>)> {
        self.expect(TokenType::LeftBrace)?;
        let fields = self.parse_multiple_with_delimiter_and_matcher(
            |parser| parser.field(),
            TokenType::Comma,
            |token| matches!(token, TokenType::Identifier(_)),
        )?;
        let fn_stmts = self.parse_multiple(
            |parser| parser.fn_self_stmt(),
            |token| matches!(token, TokenType::Fn),
        )?;
        self.expect(TokenType::RightBrace)?;
        Ok((fields, fn_stmts))
    }

    fn parse_fn_stmt(&mut self) -> ParseResult<Item> {
        self.parse_item(|parser| parser.fn_stmt(), ItemKind::Fn)
    }

    fn fn_self_stmt(&mut self) -> ParseResult<FnSelfStmt> {
        self.track_span();
        let signature = self.fn_signature(false)?;
        let stmt = self.block_stmt()?;

        Ok(FnSelfStmt::new(
            signature,
            Some(stmt),
            self.get_span(),
            self.get_id(),
        ))
    }

    fn fn_stmt(&mut self) -> ParseResult<FnStmt> {
        let signature = self.fn_signature(false)?;
        let stmt = self.block_stmt()?;

        Ok(FnStmt::new(signature, Some(stmt)))
    }

    fn fn_trait_stmt(&mut self) -> ParseResult<FnSelfStmt> {
        self.track_span();
        let signature = self.fn_signature(true)?;
        match self.current() {
            Some(TokenType::Semicolon) => {
                self.advance();
                Ok(FnSelfStmt::new(
                    signature,
                    None,
                    self.get_span(),
                    self.get_id(),
                ))
            }
            Some(TokenType::LeftBrace) => {
                let stmt = self.block_stmt()?;
                Ok(FnSelfStmt::new(
                    signature,
                    Some(stmt),
                    self.get_span(),
                    self.get_id(),
                ))
            }
            Some(token) => self.unexpected_token(token),
            None => self.unexpected_end(),
        }
    }

    fn parse_global_let_stmt(&mut self) -> ParseResult<Item> {
        self.parse_item(|parser| parser.global_let_stmt(), ItemKind::GlobalLet)
    }

    fn parse_let_stmt(&mut self) -> ParseResult<Stmt> {
        self.track_span();
        Ok(Stmt::new(
            StmtKind::Let(self.let_stmt()?),
            self.get_span(),
            self.get_id(),
        ))
    }

    fn global_let_stmt(&mut self) -> ParseResult<GlobalLetStmt> {
        self.expect(TokenType::Let)?;
        let identifier = self.identifier()?;
        let mut ty = None;
        if self.matches(TokenType::Colon) {
            self.advance();
            ty = Some(self.parse_ty()?);
        }
        self.expect(TokenType::Equal)?;
        let initializer = self.expr()?;
        self.expect(TokenType::Semicolon)?;
        let global_let_stmt = GlobalLetStmt::new(identifier, ty, initializer);
        Ok(global_let_stmt)
    }

    fn let_stmt(&mut self) -> ParseResult<LetStmt> {
        self.expect(TokenType::Let)?;
        let mutability: Mutability = match self.current() {
            Some(TokenType::Mut) => {
                self.advance();
                Mutable
            }
            Some(TokenType::Identifier(_)) => Immutable,
            _ => {
                let ident = self.intern_str("");
                return self.expected_tokens(vec![TokenType::Mut, TokenType::Identifier(ident)]);
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

    fn parse_enum_stmt(&mut self) -> ParseResult<Item> {
        self.parse_item(|parser| parser.enum_stmt(), ItemKind::Enum)
    }

    fn enum_stmt(&mut self) -> ParseResult<EnumStmt> {
        self.expect(TokenType::Enum)?;
        let name = self.identifier()?;
        let generics = self.generic_params()?;
        let (enum_members, fn_stmts) = if self.matches(TokenType::LeftBrace) {
            self.enum_stmt_inner()?
        } else {
            self.expect(TokenType::Semicolon)?;
            (Vec::new(), Vec::new())
        };

        Ok(EnumStmt::new(name, generics, enum_members, fn_stmts))
    }

    fn enum_stmt_inner(&mut self) -> ParseResult<(Vec<EnumMember>, Vec<FnSelfStmt>)> {
        self.expect(TokenType::LeftBrace)?;

        let enum_members = self.parse_multiple_with_delimiter_and_matcher(
            |parser| parser.enum_member(),
            TokenType::Comma,
            |token| matches!(token, TokenType::Identifier(_)),
        )?;

        let fn_stmts = self.parse_multiple(
            |parser| parser.fn_self_stmt(),
            |token| matches!(token, TokenType::Fn),
        )?;

        self.expect(TokenType::RightBrace)?;
        Ok((enum_members, fn_stmts))
    }

    fn parse_trait_stmt(&mut self) -> ParseResult<Item> {
        self.parse_item(|parser| parser.trait_stmt(), ItemKind::Trait)
    }

    fn trait_stmt(&mut self) -> ParseResult<TraitStmt> {
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

    fn parse_trait_impl_stmt(&mut self) -> ParseResult<Item> {
        self.parse_item(|parser| parser.trait_impl_stmt(), ItemKind::TraitImpl)
    }

    fn trait_impl_stmt(&mut self) -> ParseResult<TraitImplStmt> {
        self.expect(TokenType::Impl)?;
        let trait_to_impl = self.parse_path_ty()?;
        self.expect(TokenType::For)?;
        let target_ty = self.qualified_ident()?;

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

    fn parse_expression(&mut self) -> ParseResult<Stmt> {
        self.track_span();
        let expr = self.expr()?;
        let implicit_return = if self.matches(TokenType::Semicolon) {
            self.advance();
            true
        } else {
            false
        };
        Ok(Stmt::new(
            StmtKind::Expression(Expression::new(expr, implicit_return)),
            self.get_span(),
            self.get_id(),
        ))
    }

    fn expr(&mut self) -> ParseResult<Expr> {
        if self.matches_closure() {
            return self.parse_node(Self::parse_closure, Expr::new);
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

    fn parse_closure(&mut self) -> ParseResult<ExprKind> {
        let vars = self.parse_multiple_with_scope_delimiter::<Ident, 1>(
            |parser| parser.identifier(),
            TokenType::Comma,
            TokenType::LeftParentheses,
            TokenType::RightParentheses,
        )?;
        let closure_params = vars
            .iter()
            .map(|f| ClosureParam::new(*f))
            .collect::<Vec<ClosureParam>>();

        self.expect(TokenType::RightArrow)?;

        let stmt = self.parse_block_or_expr()?;

        Ok(ExprKind::Closure(ClosureExpr::new(closure_params, stmt)))
    }

    fn identifier(&mut self) -> ParseResult<Ident> {
        match self.current_token() {
            Some(Token {
                token_type: TokenType::Identifier(ident),
                span,
            }) => {
                self.advance();
                Ok(Ident::new(ident, span))
            }
            Some(token) => {
                let ident = self.intern_str("");
                self.expected_token(TokenType::Identifier(ident))
            }
            None => self.unexpected_end(),
        }
    }

    fn qualified_ident(&mut self) -> ParseResult<QualifiedIdent> {
        let mut idents = Vec::new();
        loop {
            match self.current_token() {
                Some(Token {
                    token_type: TokenType::Identifier(identifier),
                    span,
                }) => {
                    let ident = Ident::new(identifier, span);
                    idents.push(ident);
                    if self.matches_multiple([
                        TokenType::Identifier(identifier),
                        TokenType::Colon,
                        TokenType::Colon,
                    ]) {
                        self.advance_multiple(3);
                    } else {
                        self.advance();
                        break;
                    }
                }
                _ => {
                    let ident = self.intern_str("");
                    return self.expected_token(TokenType::Identifier(ident));
                }
            }
        }

        if idents.is_empty() {
            let ident = self.intern_str("");
            self.expected_token(TokenType::Identifier(ident))
        } else {
            let first = idents.first().unwrap();
            let ident_type = if self.compiler_ctxt.resolve_str(first.ident) == "crate" {
                idents.remove(0);
                IdentType::Crate
            } else {
                IdentType::LocalOrUse
            };

            Ok(QualifiedIdent::new(ident_type, idents))
        }
    }

    fn generic_call_site(&mut self) -> ParseResult<GenericCallSite> {
        let generic_call_site = self
            .parse_multiple_with_scope_delimiter::<Ty, 1>(
                |parser| parser.parse_ty(),
                TokenType::Comma,
                TokenType::Less,
                TokenType::Greater,
            )?
            .into();
        Ok(generic_call_site)
    }

    fn generic_params(&mut self) -> ParseResult<GenericParams> {
        let generic_params = self
            .parse_multiple_with_scope_delimiter::<GenericParam, 1>(
                |parser| parser.generic_param(),
                TokenType::Comma,
                TokenType::Less,
                TokenType::Greater,
            )?
            .into();
        Ok(generic_params)
    }

    fn generic_param(&mut self) -> ParseResult<GenericParam> {
        self.track_span();
        let ident = self.identifier()?;
        let mut trait_bound: Option<Ty> = None;
        if self.matches(TokenType::Colon) {
            self.advance();
            self.track_span();
            trait_bound = Some(Ty::new(
                TyKind::TraitBound {
                    trait_bound: self.trait_bound()?,
                },
                self.get_span(),
                self.get_id(),
            ));
        }
        Ok(GenericParam::new(
            ident,
            trait_bound,
            self.get_span(),
            self.get_id(),
        ))
    }

    fn fn_stmts(&mut self) -> ParseResult<Vec<FnStmt>> {
        self.parse_multiple_with_scope(
            |parser| parser.fn_stmt(),
            TokenType::LeftBrace,
            TokenType::RightBrace,
        )
    }

    fn fn_trait_stmts(&mut self) -> ParseResult<Vec<FnSelfStmt>> {
        self.parse_multiple_with_scope(
            |parser| parser.fn_trait_stmt(),
            TokenType::LeftBrace,
            TokenType::RightBrace,
        )
    }

    fn trait_bound(&mut self) -> ParseResult<TraitBound> {
        let mut paths = Vec::new();
        paths.push(self.parse_path_ty()?);
        while self.matches(TokenType::Plus) {
            self.advance();
            paths.push(self.parse_path_ty()?);
        }
        Ok(TraitBound::from(paths))
    }

    fn enum_member(&mut self) -> ParseResult<EnumMember> {
        self.track_span();
        match self.current() {
            Some(TokenType::Identifier(ident)) => {
                self.advance();
                let fields = self.fields()?;
                let fn_self_stmts = self.parse_multiple_with_scope(
                    |parser| parser.fn_self_stmt(),
                    TokenType::LeftBrace,
                    TokenType::RightBrace,
                )?;

                let span = self.get_span();
                let id = self.get_id();
                Ok(EnumMember::new(ident, fields, fn_self_stmts, span, id))
            }
            Some(token) => {
                let ident = self.intern_str("");
                self.expected_token(TokenType::Identifier(ident))
            }
            None => self.unexpected_end(),
        }
    }

    fn fields(&mut self) -> ParseResult<Fields> {
        let fields = self.parse_multiple_with_scope_delimiter::<Field, 1>(
            |parser| parser.field(),
            TokenType::Comma,
            TokenType::LeftParentheses,
            TokenType::RightParentheses,
        )?;
        Ok(Fields::from(fields))
    }

    fn params(&mut self) -> ParseResult<Params> {
        let params = self.parse_multiple_with_scope_delimiter::<Param, 1>(
            |parser| parser.param(),
            TokenType::Comma,
            TokenType::LeftParentheses,
            TokenType::RightParentheses,
        )?;
        Ok(Params::from(params))
    }

    fn self_params(&mut self) -> ParseResult<Params> {
        let mut params = Vec::new();
        self.expect(TokenType::LeftParentheses)?;
        params.push(self.self_param()?);
        if self.matches(TokenType::Comma) {
            self.advance();
            params.extend(
                self.parse_multiple_with_delimiter(|parser| parser.param(), TokenType::Comma)?,
            );
        }
        self.expect(TokenType::RightParentheses)?;
        Ok(Params::from(params))
    }

    fn field(&mut self) -> ParseResult<Field> {
        self.track_span();
        let ident = self.identifier()?;
        self.expect(TokenType::Colon)?;
        let ty = self.parse_ty()?;
        Ok(Field::new(ident, ty, self.get_span(), self.get_id()))
    }

    fn param(&mut self) -> ParseResult<Param> {
        self.track_span();
        let (mutability, ident, ty) = self.mut_ident_ty()?;
        Ok(Param::new(
            ident,
            ty,
            mutability,
            self.get_span(),
            self.get_id(),
        ))
    }

    fn mut_ident_ty(&mut self) -> ParseResult<(Mutability, Ident, Ty)> {
        let mutability = self.mutability();
        let ident;
        let ty;
        if let Some(Token {
            token_type: TokenType::SelfLowercase,
            span,
        }) = self.current_token()
        {
            self.advance();
            ident = Ident::new(self.intern_str("self"), span);
            ty = Ty::new(TyKind::QSelf, span, self.get_id());
        } else {
            ident = self.identifier()?;
            self.expect(TokenType::Colon)?;
            ty = self.parse_ty()?;
        }

        Ok((mutability, ident, ty))
    }

    fn self_param(&mut self) -> ParseResult<Param> {
        self.track_span();
        let mutability = self.mutability();

        match self.current_token() {
            Some(Token {
                token_type: TokenType::SelfLowercase,
                span,
            }) => {
                self.advance();
                let ident = Ident::new(self.intern_str("self"), span);
                let ty = Ty::new(TyKind::QSelf, span, self.get_id());

                Ok(Param::new(
                    ident,
                    ty,
                    mutability,
                    self.get_span(),
                    self.get_id(),
                ))
            }
            Some(token) => self.expected_token(TokenType::SelfLowercase),
            None => self.unexpected_end(),
        }
    }

    fn fn_signature(&mut self, require_self: bool) -> ParseResult<FnSig> {
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

    fn block_stmt(&mut self) -> ParseResult<Block> {
        self.track_span();
        let stmts = self.parse_multiple_with_scope(
            |parser| parser.parse_inner_stmt(),
            TokenType::LeftBrace,
            TokenType::RightBrace,
        )?;
        Ok(Block::new(stmts, self.get_span(), self.get_id()))
    }

    fn parse_block_stmt(&mut self) -> ParseResult<Stmt> {
        self.track_span();
        Ok(Stmt::new(
            StmtKind::Block(self.block_stmt()?),
            self.get_span(),
            self.get_id(),
        ))
    }

    /// This function returns whether the following attribute is
    /// marked as mutable or immutable.
    fn mutability(&mut self) -> Mutability {
        if let Some(TokenType::Mut) = self.current() {
            Mutable
        } else {
            Immutable
        }
    }

    fn parse_ty(&mut self) -> ParseResult<Ty> {
        match self.current_token() {
            Some(Token {
                token_type: TokenType::LeftParentheses,
                ..
            }) => self.parse_closure_ty(),
            Some(Token {
                token_type: TokenType::LeftBracket,
                ..
            }) => self.parse_array_ty(),
            Some(Token {
                token_type: TokenType::None,
                span,
            }) => {
                self.advance();
                Ok(Ty::new(TyKind::None, span, self.get_id()))
            }
            Some(Token {
                token_type: TokenType::Identifier(ident),
                span,
            }) => {
                // These built in types are officially encoded as strings to avoid them being
                // tokenized as keywords.
                let ident = self.resolve_str(ident);
                match ident {
                    "u8" => {
                        self.advance();
                        Ok(Ty::new(TyKind::U8, span, self.get_id()))
                    }
                    "u16" => {
                        self.advance();
                        Ok(Ty::new(TyKind::U16, span, self.get_id()))
                    }
                    "u32" => {
                        self.advance();
                        Ok(Ty::new(TyKind::U32, span, self.get_id()))
                    }
                    "u64" => {
                        self.advance();
                        Ok(Ty::new(TyKind::U64, span, self.get_id()))
                    }
                    "i8" => {
                        self.advance();
                        Ok(Ty::new(TyKind::I8, span, self.get_id()))
                    }
                    "i16" => {
                        self.advance();
                        Ok(Ty::new(TyKind::I16, span, self.get_id()))
                    }
                    "i32" => {
                        self.advance();
                        Ok(Ty::new(TyKind::I32, span, self.get_id()))
                    }
                    "i64" => {
                        self.advance();
                        Ok(Ty::new(TyKind::I64, span, self.get_id()))
                    }
                    "f32" => {
                        self.advance();
                        Ok(Ty::new(TyKind::F32, span, self.get_id()))
                    }
                    "f64" => {
                        self.advance();
                        Ok(Ty::new(TyKind::F64, span, self.get_id()))
                    }
                    "str" => {
                        self.advance();
                        Ok(Ty::new(TyKind::Str, span, self.get_id()))
                    }
                    "bool" => {
                        self.advance();
                        Ok(Ty::new(TyKind::Boolean, span, self.get_id()))
                    }
                    other => self.parse_qualified_ty(),
                }
            }
            Some(Token {
                token_type: TokenType::SelfCapitalized,
                span,
            }) => {
                self.advance();
                Ok(Ty::new(TyKind::QSelf, span, self.get_id()))
            }
            Some(token) => self.unexpected_token(token.token_type),
            None => self.unexpected_end(),
        }
    }

    /// Parses a closure type. This method assumes that a valid closure signature
    /// already exists.
    fn parse_closure_ty(&mut self) -> ParseResult<Ty> {
        self.track_span();
        let params = self.parse_multiple_with_scope_delimiter::<Ty, 1>(
            |parser| parser.parse_ty(),
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

            let ty_kind = TyKind::Closure {
                params,
                ret_ty: Box::new(ret_ty),
            };
            Ok(Ty::new(ty_kind, self.get_span(), self.get_id()))
        } else {
            self.unexpected_end()
        }
    }

    fn parse_array_ty(&mut self) -> ParseResult<Ty> {
        self.track_span();
        self.expect(TokenType::LeftBracket)?;
        let ty_kind = TyKind::Array {
            ty: Box::new(self.parse_ty()?),
        };
        let ty = Ty::new(ty_kind, self.get_span(), self.get_id());
        self.expect(TokenType::RightBracket)?;
        Ok(ty)
    }

    fn parse_qualified_ty(&mut self) -> ParseResult<Ty> {
        self.track_span();

        let path = self.parse_path_ty()?;

        if let Some(TokenType::Plus) = self.current() {
            self.advance();
            let mut paths = self
                .parse_multiple_with_delimiter(|parser| parser.parse_path_ty(), TokenType::Plus)?;
            paths.insert(0, path);

            let span = self.get_span();
            Ok(Ty::new(
                TyKind::TraitBound {
                    trait_bound: TraitBound::from(paths),
                },
                span,
                self.get_id(),
            ))
        } else {
            let span = self.get_span();
            Ok(Ty::new(TyKind::Path { path }, span, self.get_id()))
        }
    }

    fn parse_if_stmt(&mut self) -> ParseResult<Stmt> {
        self.track_span();
        self.expect(TokenType::If)?;
        let condition = self.expr()?;
        let block_stmt = self.block_stmt()?;
        let optional_stmt = if self.matches(TokenType::Else) {
            self.advance();
            Some(self.block_stmt()?)
        } else {
            None
        };
        Ok(Stmt::new(
            StmtKind::If(IfStmt::new(condition, block_stmt, optional_stmt)),
            self.get_span(),
            self.get_id(),
        ))
    }

    fn parse_while_stmt(&mut self) -> ParseResult<Stmt> {
        self.track_span();
        Ok(Stmt::new(
            StmtKind::While(self.while_stmt()?),
            self.get_span(),
            self.get_id(),
        ))
    }

    fn while_stmt(&mut self) -> ParseResult<WhileStmt> {
        self.expect(TokenType::While)?;
        let condition = self.expr()?;
        let block_stmt = self.block_stmt()?;

        Ok(WhileStmt::new(condition, block_stmt))
    }

    fn parse_for_stmt(&mut self) -> ParseResult<Stmt> {
        self.track_span();
        self.expect(TokenType::For)?;
        let identifier = self.identifier()?;
        self.expect(TokenType::In)?;

        let range_expr = self.expr()?;
        let body = self.block_stmt()?;
        Ok(Stmt::new(
            StmtKind::For(ForStmt::new(identifier, range_expr, body)),
            self.get_span(),
            self.get_id(),
        ))
    }

    fn parse_return_stmt(&mut self) -> ParseResult<Stmt> {
        self.track_span();
        self.expect(TokenType::Return)?;
        let expr = if self.matches(TokenType::Semicolon) {
            self.advance();
            None
        } else {
            let expression = self.expr()?;
            self.expect(TokenType::Semicolon)?;
            Some(expression)
        };
        Ok(Stmt::new(
            StmtKind::Return(ReturnStmt::new(expr)),
            self.get_span(),
            self.get_id(),
        ))
    }

    fn parse_path_expr(&mut self) -> ParseResult<PathExpr> {
        let mut path_segments = Vec::new();
        let first_ident = self.identifier()?;
        path_segments.push(Segment::new(first_ident, None));

        loop {
            if self.matches_multiple([TokenType::Colon, TokenType::Colon]) {
                self.advance_multiple(2);
                match self.current_token() {
                    Some(Token {
                        token_type: TokenType::Less,
                        ..
                    }) => {
                        let generic_paths = self.parse_multiple_with_scope_delimiter::<Ty, 1>(
                            |parser| parser.parse_ty(),
                            TokenType::Comma,
                            TokenType::Less,
                            TokenType::Greater,
                        )?;
                        path_segments.last_mut().unwrap().generics =
                            Some(Generics::from(generic_paths));
                        self.expect(TokenType::Colon)?;
                        self.expect(TokenType::Colon)?;

                        let last = self.identifier()?;

                        path_segments.push(Segment::new(last, None));
                        break;
                    }
                    Some(Token {
                        token_type: TokenType::Identifier(ident),
                        span,
                    }) => {
                        let ident = Ident::new(ident, span);
                        path_segments.push(Segment::new(ident, None));
                        self.advance();
                    }
                    Some(token) => {
                        let ident = self.intern_str("");
                        return self
                            .expected_tokens(vec![TokenType::Less, TokenType::Identifier(ident)]);
                    }
                    None => return self.unexpected_end(),
                }
            } else {
                break;
            }
        }

        let ident_type = if self.matches_str(first_ident.ident, "crate") {
            path_segments.remove(0);
            IdentType::Crate
        } else {
            IdentType::LocalOrUse
        };
        Ok(PathExpr::new(ident_type, path_segments))
    }

    fn parse_expr(&mut self, min_bp: u8) -> ParseResult<Expr> {
        let start = self.pos;

        // Check for prefix operator
        let lhs = if let Some(prefix_op) = self.prefix_op() {
            self.advance();
            let ((), prefix_bp) = prefix_op.binding_power();
            let rhs = self.parse_expr(prefix_bp)?;
            ExprKind::Unary(UnaryExpr::new(prefix_op, rhs))
        } else if let Some(current) = self.current_token() {
            let expr = match current.token_type {
                TokenType::SelfLowercase => {
                    self.advance();
                    let ident = Ident::new(self.intern_str("self"), current.span);
                    ExprKind::Path(PathExpr::new(
                        IdentType::LocalOrUse,
                        vec![Segment::new(ident, None)],
                    ))
                }
                TokenType::True => {
                    self.advance();
                    ExprKind::True
                }
                TokenType::False => {
                    self.advance();
                    ExprKind::False
                }
                TokenType::Int(int) => {
                    self.advance();
                    ExprKind::Int(int)
                }
                TokenType::UInt(uint) => {
                    self.advance();
                    ExprKind::UInt(uint)
                }
                TokenType::Float(float) => {
                    self.advance();
                    ExprKind::Float(float)
                }
                TokenType::String(string) => {
                    self.advance();
                    ExprKind::String(string)
                }
                TokenType::Identifier(ident) => ExprKind::Path(self.parse_path_expr()?),
                TokenType::SelfCapitalized => {
                    self.advance();
                    let ident = Ident::new(self.intern_str("Self"), current.span);
                    ExprKind::Path(PathExpr::new(
                        IdentType::LocalOrUse,
                        vec![Segment::new(ident, None)],
                    ))
                }
                TokenType::None => {
                    self.advance();
                    ExprKind::None
                }

                // Handle parenthesized expression
                TokenType::LeftParentheses => {
                    self.advance();
                    let expr = self.parse_expr(min_bp)?;
                    self.expect(TokenType::RightParentheses)?;
                    ExprKind::Parentheses(Parentheses::new(expr))
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
                            ExprKind::Array(ArrayExpr::SizedInitializer(
                                Box::new(expr),
                                Box::new(size),
                            ))
                        }
                        Some(TokenType::Comma) => {
                            self.advance();
                            let mut exprs = vec![expr];
                            exprs.extend(self.parse_multiple_with_delimiter(
                                |parser| parser.expr(),
                                TokenType::Comma,
                            )?);
                            self.expect(TokenType::RightBracket)?;
                            ExprKind::Array(ArrayExpr::Initializer(exprs))
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
                        |parser| parser.parse_match_arm(),
                        TokenType::Comma,
                        TokenType::LeftBrace,
                        TokenType::RightBrace,
                    )?;
                    ExprKind::Match(MatchExpr::new(source, match_arms))
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

        // We don't assign IDs to parentheses because they are discarded later.
        let mut lhs_expr = if let ExprKind::Parentheses(_) = lhs {
            Expr::new(lhs, self.compute_span(start), LocalDefId::default())
        } else {
            Expr::new(lhs, self.compute_span(start), self.get_id())
        };

        while let Some(current) = self.current() {
            if let Some(postfix_op) = self.postfix_op() {
                let (left_bp, ()) = postfix_op.binding_power();
                if left_bp < min_bp {
                    break;
                }

                lhs_expr = match postfix_op {
                    PostfixOp::LeftParentheses => {
                        let args = self.parse_multiple_with_scope_delimiter::<Expr, 1>(
                            |parser| parser.expr(),
                            TokenType::Comma,
                            TokenType::LeftParentheses,
                            TokenType::RightParentheses,
                        )?;
                        let expr_kind = ExprKind::Call(CallExpr::new(lhs_expr, Args::from(args)));
                        Expr::new(expr_kind, self.compute_span(start), self.get_id())
                    }
                    PostfixOp::LeftBracket => {
                        self.advance();
                        let rhs = self.expr()?;
                        self.expect(TokenType::RightBracket)?;
                        let expr_kind = ExprKind::Index(IndexExpr::new(lhs_expr, rhs));
                        Expr::new(expr_kind, self.compute_span(start), self.get_id())
                    }
                    PostfixOp::Dot => {
                        self.advance();
                        let ident = self.identifier()?;
                        let expr_kind = ExprKind::Field(FieldExpr::new(lhs_expr, ident));
                        Expr::new(expr_kind, self.compute_span(start), self.get_id())
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

                let expr_kind = ExprKind::Infix(InfixExpr::new(lhs_expr, rhs, infix_op));
                lhs_expr = Expr::new(expr_kind, self.compute_span(start), self.get_id());
                continue;
            }

            break;
        }

        Ok(lhs_expr)
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

    fn parse_match_arm(&mut self) -> ParseResult<MatchArm> {
        self.track_span();
        let mut patterns = self
            .parse_multiple_with_delimiter(|parser| parser.parse_pattern(), TokenType::BitwiseOr)?;
        let pattern = if patterns.len() > 1 {
            Pattern::Or(OrPattern::new(patterns))
        } else {
            patterns.remove(0)
        };

        self.expect(TokenType::RightArrow)?;
        let stmt = self.parse_block_or_expr()?;
        Ok(MatchArm::new(pattern, stmt, self.get_span(), self.get_id()))
    }

    fn parse_block_or_expr(&mut self) -> ParseResult<Stmt> {
        if self.matches(TokenType::LeftBrace) {
            self.parse_block_stmt()
        } else {
            self.track_span();
            self.expr().map(|expr| {
                Stmt::new(
                    StmtKind::Expression(Expression::new(expr, true)),
                    self.get_span(),
                    self.get_id(),
                )
            })
        }
    }

    fn parse_pattern(&mut self) -> ParseResult<Pattern> {
        // TODO: Support range patterns and guards
        match self.current() {
            Some(TokenType::String(str)) => {
                self.advance();
                Ok(Pattern::String(str))
            }
            Some(TokenType::Int(int)) => {
                self.advance();
                Ok(Pattern::Int(int))
            }
            Some(TokenType::UInt(uint)) => {
                self.advance();
                Ok(Pattern::UInt(uint))
            }
            Some(TokenType::True) => {
                self.advance();
                Ok(Pattern::True)
            }
            Some(TokenType::False) => {
                self.advance();
                Ok(Pattern::False)
            }
            Some(TokenType::None) => {
                self.advance();
                Ok(Pattern::None)
            }
            Some(TokenType::Underscore) => {
                self.advance();
                Ok(Pattern::Wildcard)
            }
            Some(token_type) => {
                let ty = self.parse_path_ty()?;
                match self.current() {
                    Some(TokenType::LeftParentheses) => {
                        self.advance();
                        let exprs = self.parse_multiple_with_delimiter(
                            |parser| parser.destructure_expr(),
                            TokenType::Comma,
                        )?;
                        self.expect(TokenType::RightParentheses)?;
                        let destructure_pat = DestructurePattern::new(ty, exprs);
                        Ok(Pattern::Destructure(destructure_pat))
                    }
                    Some(TokenType::Identifier(str)) => {
                        self.track_span();
                        self.advance();
                        let ty_pat = TyPattern::new(
                            ty,
                            Some(PatternLocal::new(str, self.get_span(), self.get_id())),
                        );
                        Ok(Pattern::Ty(ty_pat))
                    }
                    Some(TokenType::RightArrow) | Some(TokenType::BitwiseXor) => {
                        let ty_pat = TyPattern::new(ty, None);
                        Ok(Pattern::Ty(ty_pat))
                    }
                    Some(token) => self.unexpected_token(token),
                    None => self.unexpected_end(),
                }
            }
            None => self.unexpected_end(),
        }
    }

    fn destructure_expr(&mut self) -> ParseResult<DestructureExpr> {
        self.track_span();
        match self.current() {
            Some(TokenType::Identifier(ident)) => {
                // If the next token is a comma, we know that this is an identifier not a path.
                match self.next_type(1) {
                    Some(TokenType::Comma) | Some(TokenType::RightParentheses) => {
                        self.advance();
                        Ok(DestructureExpr::new(
                            DestructureExprKind::Identifier(ident),
                            self.get_span(),
                            self.get_id(),
                        ))
                    }
                    Some(_) => {
                        let path_ty = self.parse_path_ty()?;
                        let exprs = self
                            .parse_multiple_with_scope_delimiter::<DestructureExpr, 1>(
                                |parser| parser.destructure_expr(),
                                TokenType::Comma,
                                TokenType::LeftParentheses,
                                TokenType::RightParentheses,
                            )?;
                        Ok(DestructureExpr::new(
                            DestructureExprKind::Pattern(DestructurePattern::new(path_ty, exprs)),
                            self.get_span(),
                            self.get_id(),
                        ))
                    }
                    None => self.unexpected_end(),
                }
            }
            Some(TokenType::True) => {
                self.advance();
                Ok(DestructureExpr::new(
                    DestructureExprKind::True,
                    self.get_span(),
                    self.get_id(),
                ))
            }
            Some(TokenType::False) => {
                self.advance();
                Ok(DestructureExpr::new(
                    DestructureExprKind::False,
                    self.get_span(),
                    self.get_id(),
                ))
            }
            Some(TokenType::String(string)) => {
                self.advance();
                Ok(DestructureExpr::new(
                    DestructureExprKind::String(string),
                    self.get_span(),
                    self.get_id(),
                ))
            }
            Some(TokenType::Int(integer)) => {
                self.advance();
                Ok(DestructureExpr::new(
                    DestructureExprKind::Int(integer),
                    self.get_span(),
                    self.get_id(),
                ))
            }
            Some(TokenType::UInt(uinteger)) => {
                self.advance();
                Ok(DestructureExpr::new(
                    DestructureExprKind::UInt(uinteger),
                    self.get_span(),
                    self.get_id(),
                ))
            }
            Some(TokenType::Float(float)) => {
                self.advance();
                Ok(DestructureExpr::new(
                    DestructureExprKind::Float(float),
                    self.get_span(),
                    self.get_id(),
                ))
            }
            Some(TokenType::None) => {
                self.advance();
                Ok(DestructureExpr::new(
                    DestructureExprKind::None,
                    self.get_span(),
                    self.get_id(),
                ))
            }
            Some(token) => self.unexpected_token(token),
            None => self.unexpected_end(),
        }
    }

    fn parse_path_ty(&mut self) -> ParseResult<PathTy> {
        let ident = self.qualified_ident()?;
        let generics = self.parse_multiple_with_scope_delimiter::<Ty, 1>(
            |parser| parser.parse_ty(),
            TokenType::Comma,
            TokenType::Less,
            TokenType::Greater,
        )?;
        Ok(PathTy::new(ident, Generics::from(generics)))
    }

    fn parse_multiple<T>(
        &mut self,
        parse_rule: fn(&mut Parser<'_>) -> ParseResult<T>,
        matcher: fn(&TokenType) -> bool,
    ) -> ParseResult<Vec<T>> {
        let mut items = Vec::new();
        while self.current().filter(matcher).is_some() {
            items.push(parse_rule(self)?);
        }
        Ok(items)
    }

    fn parse_multiple_with_delimiter_and_matcher<T>(
        &mut self,
        parse_rule: fn(&mut Parser<'_>) -> ParseResult<T>,
        delimiter: TokenType,
        matcher: fn(&TokenType) -> bool,
    ) -> ParseResult<Vec<T>> {
        let mut items = Vec::new();
        loop {
            if self.current().filter(matcher).is_some() {
                items.push(parse_rule(self)?);
            }
            if self.matches(delimiter) {
                self.advance();
            } else {
                break;
            }
        }
        Ok(items)
    }

    fn parse_multiple_with_delimiter<T>(
        &mut self,
        parse_rule: fn(&mut Parser<'_>) -> ParseResult<T>,
        delimiter: TokenType,
    ) -> ParseResult<Vec<T>> {
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
        parse_rule: fn(&mut Parser<'_>) -> ParseResult<T>,
        scope_start: TokenType,
        scope_end: TokenType,
    ) -> ParseResult<Vec<T>> {
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
        parse_rule: fn(&mut Parser<'_>) -> ParseResult<T>,
        delimiter: TokenType,
        scope_start: TokenType,
        scope_end: TokenType,
    ) -> ParseResult<Vec<T>> {
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

    fn matches_str(&mut self, key: InternedStr, string: &str) -> bool {
        self.compiler_ctxt.resolve_str(key) == string
    }

    fn expected_token<T>(&mut self, token_type: TokenType) -> ParseResult<T> {
        let token_position = self.current_position().unwrap_or(self.last_position());
        Err(ExpectedToken(token_type, token_position))
    }

    fn expected_tokens<T>(&mut self, token_types: Vec<TokenType>) -> ParseResult<T> {
        let token_position = self.current_position().unwrap_or(self.last_position());
        Err(ExpectedTokens(TokenTypes::new(token_types), token_position))
    }

    fn unexpected_token<T>(&mut self, token_type: TokenType) -> ParseResult<T> {
        let token_position = self.current_position().unwrap_or(self.last_position());
        Err(UnexpectedToken(token_type, token_position))
    }

    fn unexpected_end<T>(&mut self) -> ParseResult<T> {
        Err(UnexpectedEof(self.last_position()))
    }

    fn current_token(&self) -> Option<Token> {
        self.tokenized_input.tokens.get(self.pos).copied()
    }

    fn current(&self) -> Option<TokenType> {
        self.tokenized_input
            .tokens
            .get(self.pos)
            .map(|token| token.token_type)
    }

    fn next(&self, delta: usize) -> Option<Token> {
        self.tokenized_input.tokens.get(self.pos + delta).copied()
    }

    fn next_type(&self, delta: usize) -> Option<TokenType> {
        self.next(delta).map(|token| token.token_type)
    }

    fn current_position(&mut self) -> Option<NormalizedSpan> {
        if let Some(token) = self.current_token() {
            Some(self.tokenized_input.token_position(token.span))
        } else {
            None
        }
    }

    fn last(&mut self) -> Option<Token> {
        self.tokenized_input.tokens.last().copied()
    }

    fn last_position(&mut self) -> NormalizedSpan {
        self.last()
            .map(|token| self.tokenized_input.token_position(token.span))
            .unwrap_or(NormalizedSpan::default())
    }

    fn expect(&mut self, token_type: TokenType) -> ParseResult<()> {
        if self
            .current()
            .filter(|current| current == &token_type)
            .is_some()
        {
            self.advance();
            Ok(())
        } else {
            self.expected_token(token_type)
        }
    }

    fn matches(&self, token_type: TokenType) -> bool {
        matches!(self.current().filter(|tt| *tt == token_type), Some(current))
    }

    fn matches_multiple<const N: usize>(&mut self, tokens: [TokenType; N]) -> bool {
        let end = self.pos + tokens.len();
        if self.tokenized_input.tokens.len() < end {
            return false;
        }

        let token_types: Vec<TokenType> = self.tokenized_input.tokens[self.pos..end]
            .iter()
            .map(|token| token.token_type)
            .collect();

        // TODO: Map to token types
        tokens.as_slice() == token_types
    }

    fn advance_multiple(&mut self, num: usize) {
        self.pos += num;
    }

    fn advance(&mut self) {
        self.pos += 1;
    }

    fn remaining(&self) -> usize {
        self.tokenized_input.tokens.len() - (self.pos + 1)
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            UnexpectedEof(span) => write!(
                f,
                "unexpected eof at {}:{}!",
                span.start_line, span.start_pos
            ),
            ExpectedToken(token_type, span) => write!(
                f,
                "expected token {} at {}:{}!",
                token_type, span.start_line, span.start_pos
            ),
            ExpectedTokens(token_types, span) => write!(
                f,
                "expected tokens {} at {}:{}!",
                token_types, span.start_line, span.start_pos
            ),
            UnexpectedToken(token_type, span) => write!(
                f,
                "unrecognized token {} at {}:{}!",
                token_type, span.start_line, span.start_pos
            ),
            ParseError::InvalidUseStmt(span) => write!(
                f,
                "invalid use stmt at {}:{}!",
                span.start_line, span.start_pos
            ),
            ParseError::DuplicateDefinition => write!(f, "duplicate definition!"),
            ParseError::DuplicateModuleName => write!(f, "duplicate module name!"),
        }
    }
}

impl Error for ParseError {}

unsafe impl Send for ParseError {}

unsafe impl Sync for ParseError {}

mod tests {
    use std::any::Any;
    use std::assert_matches::assert_matches;
    use std::error::Error;
    use std::fmt::Debug;
    use std::fs::File;
    use std::io::{BufReader, BufWriter};
    use std::path::{Path, PathBuf};
    use std::sync::Arc;

    use lasso::ThreadedRodeo;

    use snap::snapshot;

    use crate::compiler::ast::Mutability::{Immutable, Mutable};
    use crate::compiler::ast::{
        Args, Block, CallExpr, EnumMember, EnumStmt, Expr, FnSig, FnStmt, LetStmt, Mutability,
        Param, Params, PathExpr, QualifiedIdent, Stmt, Ty, TyKind,
    };
    use crate::compiler::ast::{Module, UseStmt};
    use crate::compiler::compiler::{CompileError, CompilerCtxt};
    use crate::compiler::hir::LocalDefId;
    use crate::compiler::parser::ParseError::{ExpectedToken, UnexpectedEof};
    use crate::compiler::parser::{ParseError, ParseResult, Parser};
    use crate::compiler::tokens::token::TokenType;
    use crate::compiler::tokens::token::TokenType::{Identifier, Semicolon};
    use crate::compiler::tokens::tokenized_file::{NormalizedSpan, Span, TokenizedInput};
    use crate::compiler::tokens::tokenizer::tokenize;
    use crate::compiler::types::InternedStr;
    use crate::compiler::StringInterner;
    use crate::util::utils;

    #[cfg(test)]
    fn create_parser<'ctxt>(compiler_ctxt: &'ctxt mut CompilerCtxt, code: &str) -> Parser<'ctxt> {
        let tokens = tokenize(compiler_ctxt, code);
        Parser::new(compiler_ctxt, tokens)
    }

    #[cfg(test)]
    fn parse_errors<T: AsRef<str>>(code: T) -> (StringInterner, Vec<ParseError>) {
        let mut compiler_ctxt = CompilerCtxt::default();
        let parser = create_parser(&mut compiler_ctxt, code.as_ref());

        if let Some(CompileError::ParseErrors(parse_errors)) = parser.parse().err() {
            (StringInterner::from(compiler_ctxt), parse_errors)
        } else {
            panic!("Expected parsing to fail!")
        }
    }

    #[cfg(test)]
    fn parse_module<T: AsRef<str>>(code: T) -> Result<ModuleOutput, CompileError> {
        let mut compiler_ctxt = CompilerCtxt::default();
        let parser = create_parser(&mut compiler_ctxt, code.as_ref());
        parser.parse().map(|ast| (compiler_ctxt, ast))
    }

    #[cfg(test)]
    fn parse_code<T, I: AsRef<str>>(
        code: I,
        parser_func: fn(&mut Parser) -> ParseResult<T>,
    ) -> ParseResult<(CompilerCtxt, T)> {
        let mut compiler_ctxt = CompilerCtxt::default();
        let mut parser = create_parser(&mut compiler_ctxt, code.as_ref());
        let parsed_val = parser_func(&mut parser)?;
        Ok((compiler_ctxt, parsed_val))
    }

    type ModuleOutput = (CompilerCtxt, Module);
    type ExprOutput = (CompilerCtxt, Expr);

    #[cfg(test)]
    fn parse<T: AsRef<str>>(code: T) -> ModuleOutput {
        let (ctxt, ast) = parse_module(code).unwrap();
        (ctxt, ast)
    }

    #[cfg(test)]
    macro_rules! parse_ty {
        ($code:expr) => {{
            let (ctxt, ty) = parse_code($code, |parser| parser.parse_ty()).unwrap();
            (StringInterner::from(ctxt), ty)
        }};
    }

    #[cfg(test)]
    fn parse_path(code: &str) -> (StringInterner, PathExpr) {
        let (ctxt, path) = parse_code(code, |parser| parser.parse_path_expr()).unwrap();
        (StringInterner::from(ctxt), path)
    }

    #[cfg(test)]
    fn parse_stmt(code: &str) -> (StringInterner, Stmt) {
        let (ctxt, path) = parse_code(code, |parser| parser.parse_inner_stmt()).unwrap();
        (StringInterner::from(ctxt), path)
    }

    #[cfg(test)]
    macro_rules! parse_expr {
        ($code:expr) => {{
            parse_code($code, |parser| parser.expr()).unwrap()
        }};
    }

    #[test]
    pub fn invalid_use_stmts() {
        let result = parse_module("use std::vector::");
        assert_matches!(result, Err(CompileError::ParseErrors(_)));
        let result = parse_module("use std::vector::Vector");
        assert_matches!(result, Err(CompileError::ParseErrors(_)));
        let result = parse_module("use;");
        assert_matches!(result, Err(CompileError::ParseErrors(_)));
        let result = parse_module("use");
        assert_matches!(result, Err(CompileError::ParseErrors(_)));
    }

    #[test]
    #[snapshot]
    pub fn closure_return_closure() -> (StringInterner, Ty) {
        parse_ty!("() => (() => None)")
    }

    #[test]
    #[should_panic]
    pub fn bad_closure_returns_trait_bound_or_none() {
        parse_ty!("() => [first::party::package::Send<V: std::Copy + std::Clone> + third::party::package::Sync<T> + std::Copy + std::Clone] | None");
    }

    #[test]
    #[snapshot]
    pub fn closure_returns_trait_bound_or_none() -> (StringInterner, Ty) {
        parse_ty!("() => [first::party::package::Send<K, V> + third::party::package::Sync<T> + std::Copy + std::Clone] | None")
    }

    #[test]
    #[snapshot]
    pub fn generic_type() -> (StringInterner, Ty) {
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
    pub fn class_constructor() -> (CompilerCtxt, Expr) {
        parse_expr!("Point(1.0, 2.0)")
    }

    #[test]
    #[snapshot]
    pub fn class_def_no_trailing_field_comma() -> ModuleOutput {
        parse("class Point { x: u64, y: u64 }")
    }

    #[test]
    #[snapshot]
    pub fn class_def_no_trailing_field_comma_with_self_fns() -> ModuleOutput {
        parse(
            r#"
            class Point {
                x: u64,
                y: u64

                fn copy() => Point { }
            }
            "#,
        )
    }

    #[test]
    #[snapshot]
    pub fn class_def_trailing_field_comma() -> ModuleOutput {
        parse("class Point { x: u64, y: u64, }")
    }

    #[test]
    #[snapshot]
    pub fn class_def_trailing_field_comma_with_self_fns() -> ModuleOutput {
        parse(
            r#"
            class Point {
                x: u64,
                y: u64,

                fn copy() => Point { }
            }
            "#,
        )
    }

    #[test]
    #[snapshot]
    pub fn single_path_expr() -> (StringInterner, PathExpr) {
        parse_path("std")
    }

    #[test]
    #[snapshot]
    pub fn simple_path_expr() -> (StringInterner, PathExpr) {
        parse_path("std::Clone")
    }

    #[test]
    #[snapshot]
    pub fn generic_path_expr() -> (StringInterner, PathExpr) {
        parse_path("std::HashMap::<T>::new")
    }

    #[test]
    #[snapshot]
    pub fn nested_generic_path() -> (StringInterner, PathExpr) {
        parse_path("List::<List<f64>>::new")
    }

    #[test]
    #[snapshot]
    pub fn generic_trait_bound_path() -> (StringInterner, PathExpr) {
        parse_path("List::<Loggable + Serializable>::new")
    }

    #[test]
    #[snapshot]
    pub fn while_stmt() -> (StringInterner, Stmt) {
        parse_stmt("while x < 100 { }")
    }

    #[test]
    #[snapshot]
    pub fn for_stmt() -> (StringInterner, Stmt) {
        parse_stmt("for index in range(0, 100) { }")
    }

    #[test]
    #[snapshot]
    pub fn if_stmt() -> (StringInterner, Stmt) {
        parse_stmt("if x < 100 { let y = 30; print(y); }")
    }

    #[test]
    #[snapshot]
    pub fn block_stmt() -> (StringInterner, Stmt) {
        parse_stmt("{ let y = 30; } ")
    }

    macro_rules! simple_type {
        ($typ:expr, $fn_name:ident, $code:literal) => {
            #[test]
            pub fn $fn_name() {
                let actual_ty = Ty::new($typ, Span::new(0, $code.len() as u32), LocalDefId::new(0));
                let (ctxt, ty) = parse_ty!($code);
                assert_eq!(actual_ty, ty);
            }
        };
    }

    simple_type!(TyKind::Str, str_type, "str");
    simple_type!(TyKind::U8, u8_type, "u8");
    simple_type!(TyKind::U16, u16_type, "u16");
    simple_type!(TyKind::U32, u32_type, "u32");
    simple_type!(TyKind::U64, u64_type, "u64");
    simple_type!(TyKind::I8, i8_type, "i8");
    simple_type!(TyKind::I16, i16_type, "i16");
    simple_type!(TyKind::I32, i32_type, "i32");
    simple_type!(TyKind::I64, i64_type, "i64");
    simple_type!(TyKind::F32, f32_type, "f32");
    simple_type!(TyKind::F64, f64_type, "f64");
    simple_type!(TyKind::None, none_type, "None");
    simple_type!(TyKind::Boolean, bool_type, "bool");

    #[test]
    #[snapshot]
    pub fn use_statements() -> ModuleOutput {
        parse(utils::read_code_example("use_stmts.si"))
    }

    #[test]
    #[snapshot]
    pub fn basic_enum() -> ModuleOutput {
        parse(utils::read_code_example("basic_enum.si"))
    }

    #[test]
    #[snapshot]
    pub fn vector_enum() -> ModuleOutput {
        parse(utils::read_code_example("vector_enum.si"))
    }

    #[test]
    #[snapshot]
    pub fn main_fn() -> ModuleOutput {
        parse(utils::read_code_example("hello_world.si"))
    }

    #[test]
    #[snapshot]
    pub fn main_fn_with_args() -> ModuleOutput {
        parse(utils::read_code_example("main_fn.si"))
    }

    #[test]
    #[snapshot]
    pub fn declare_classes_and_vars() -> ModuleOutput {
        parse(utils::read_code_example("classes_and_vars.si"))
    }

    #[test]
    #[snapshot]
    pub fn simple_add_func() -> ModuleOutput {
        parse(utils::read_code_example("sum_fn.si"))
    }

    #[test]
    #[snapshot]
    pub fn var_declarations() -> ModuleOutput {
        parse(utils::read_code_example("var_declarations.si"))
    }

    #[test]
    #[snapshot]
    pub fn mutable_assignment() -> ModuleOutput {
        parse(utils::read_code_example("mutable_assignment.si"))
    }

    #[test]
    #[snapshot]
    pub fn print_fn() -> ModuleOutput {
        parse(utils::read_code_example("print_fn.si"))
    }

    #[test]
    #[snapshot]
    pub fn returning_error_union() -> ModuleOutput {
        parse(utils::read_code_example("returning_error_union.si"))
    }

    #[test]
    #[snapshot]
    pub fn trait_vs_generic() -> ModuleOutput {
        parse(utils::read_code_example("trait_vs_generic.si"))
    }

    #[test]
    #[snapshot]
    pub fn generic_lists() -> ModuleOutput {
        parse(utils::read_code_example("generic_lists.si"))
    }

    #[test]
    #[snapshot]
    pub fn rectangle_class() -> ModuleOutput {
        parse(utils::read_code_example("rectangle_class.si"))
    }

    #[test]
    #[snapshot]
    pub fn enum_message() -> ModuleOutput {
        parse(utils::read_code_example("enum_message.si"))
    }

    #[test]
    #[snapshot]
    pub fn int_match() -> ExprOutput {
        parse_expr!(utils::read_code_example("int_match.si"))
    }

    #[test]
    #[snapshot]
    pub fn enum_match() -> ModuleOutput {
        parse(utils::read_code_example("enum_match.si"))
    }

    #[test]
    #[snapshot]
    pub fn impl_trait() -> ModuleOutput {
        parse(utils::read_code_example("impl_trait.si"))
    }

    #[test]
    #[snapshot]
    pub fn unexpected_outer_stmt_token() -> (StringInterner, Vec<ParseError>) {
        parse_errors("enum Stmt { } aflatoxin")
    }
}
