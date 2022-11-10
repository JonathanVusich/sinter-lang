use crate::compiler::types::types::{InternedStr, Type};
use crate::gc::block::Block;
use crate::traits::traits::Trait;
use serde::{Deserialize, Serialize};
use std::path::Prefix;
use crate::compiler::parser::ClassType;

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct Module {
    stmts: Vec<Stmt>,
}

impl Module {
    pub fn new(stmts: Vec<Stmt>) -> Self {
        Self { stmts }
    }

    pub fn stmts(&self) -> &[Stmt] {
        &self.stmts
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct QualifiedIdent {
    idents: Vec<InternedStr>,
}

impl QualifiedIdent {
    pub fn new(idents: Vec<InternedStr>) -> Self {
        Self { idents }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct PathTy {
    ident: QualifiedIdent,
    generics: GenericDecls,
}

impl PathTy {

    pub fn new(ident: QualifiedIdent, generics: GenericDecls) -> Self {
        Self {
            ident,
            generics,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct UseStmt {
    ident: QualifiedIdent,
}

impl UseStmt {
    pub fn new(ident: QualifiedIdent) -> Self {
        Self { ident }
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Debug, Default, Serialize, Deserialize)]
pub struct GenericDecls {
    generics: Vec<GenericDecl>,
}

impl GenericDecls {
    pub fn new(generics: Vec<GenericDecl>) -> Self {
        Self { generics }
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Debug, Serialize, Deserialize)]
pub struct GenericCallSite {
    generics: Vec<Type>,
}

impl GenericCallSite {
    pub fn new(generics: Vec<Type>) -> Self {
        Self {
            generics,
        }
    }
}

#[derive(PartialEq, Eq, Debug, Default, Clone, Serialize, Deserialize)]
pub struct Params {
    params: Vec<Param>,
}

impl Params {
    pub fn new(params: Vec<Param>) -> Self {
        Self { params }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct Args {
    args: Vec<Expr>,
}

impl Args {
    pub fn new(args: Vec<Expr>) -> Self {
        Self { args }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Serialize, Deserialize)]
pub enum Mutability {
    Mutable,
    Immutable,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct ClassStmt {
    name: InternedStr,
    class_type: ClassType,
    generic_types: GenericDecls,
    members: Params,
    member_functions: Vec<FnStmt>,
}

impl ClassStmt {
    pub fn new(
        name: InternedStr,
        class_type: ClassType,
        generic_types: GenericDecls,
        members: Params,
        member_functions: Vec<FnStmt>,
    ) -> Self {
        Self {
            name,
            class_type,
            generic_types,
            members,
            member_functions,
        }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct EnumStmt {
    name: InternedStr,
    generics: GenericDecls,
    members: Vec<EnumMemberStmt>,
}

impl EnumStmt {
    pub fn new(name: InternedStr, generic_types: GenericDecls, members: Vec<EnumMemberStmt>) -> Self {
        Self {
            name,
            generics: generic_types,
            members,
        }
    }

    pub fn generics(&self) -> &GenericDecls {
        &self.generics
    }

    pub fn members(&self) -> &[EnumMemberStmt] {
        &self.members
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct TraitStmt {
    ident: InternedStr,
    generics: GenericDecls,
    functions: Vec<FnStmt>,
}

impl TraitStmt {

    pub fn new(ident: InternedStr, generics: GenericDecls, functions: Vec<FnStmt>) -> Self {
        Self {
            ident,
            generics,
            functions
        }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct TraitImplStmt {
    trait_to_impl: PathTy,
    target_ty: Type,
    functions: Vec<FnStmt>,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct FnStmt {
    sig: FnSig,
    body: Option<BlockStmt>,
}

impl FnStmt {
    pub fn new(sig: FnSig, body: Option<BlockStmt>) -> Self {
        Self { sig, body }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct ForStmt {
    range: Expr,
    body: BlockStmt,
}

impl ForStmt {
    pub fn new(range: Expr, body: BlockStmt) -> Self {
        Self { range, body }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct ReturnStmt {
    value: Option<Expr>,
}

impl ReturnStmt {
    pub fn new(value: Option<Expr>) -> Self {
        Self { value }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct IfStmt {
    condition: Expr,
    if_true: BlockStmt,
    if_false: Option<BlockStmt>,
}

impl IfStmt {
    pub fn new(condition: Expr, if_true: BlockStmt, if_false: Option<BlockStmt>) -> Self {
        Self { condition, if_true, if_false }
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Debug, Serialize, Deserialize)]
pub struct GenericDecl {
    ident: InternedStr,
    trait_bound: Option<Type>,
}

impl GenericDecl {
    pub fn new(ident: InternedStr, trait_bound: Option<Type>) -> Self {
        Self { ident, trait_bound }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct EnumMemberStmt {
    name: InternedStr,
    parameters: Params,
    member_functions: Vec<FnStmt>,
}

impl EnumMemberStmt {
    pub fn new(name: InternedStr, parameters: Params, member_functions: Vec<FnStmt>) -> Self {
        Self {
            name,
            parameters,
            member_functions,
        }
    }
}

#[derive(PartialEq, Eq, Debug, Serialize, Deserialize)]
pub struct FnSig {
    name: InternedStr,
    generic_types: GenericDecls,
    parameters: Params,
    return_type: Option<Type>,
}

impl FnSig {
    pub fn new(
        name: InternedStr,
        generic_types: GenericDecls,
        parameters: Params,
        return_type: Option<Type>,
    ) -> Self {
        Self {
            name,
            generic_types,
            parameters,
            return_type,
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug, Serialize, Deserialize)]
pub struct Param {
    name: InternedStr,
    ty: Type,
    mutability: Mutability,
}

impl Param {
    pub fn new(name: InternedStr, ty: Type, mutability: Mutability) -> Self {
        Self {
            name,
            ty,
            mutability,
        }
    }
}

#[derive(PartialEq, Eq, Debug)]
pub struct ArgumentDecl {
    ident: InternedStr,
    ty: Type,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct LetStmt {
    ident: InternedStr,
    ty: Option<Type>,
    initializer: Option<Expr>,
}

impl LetStmt {
    
    pub fn new(ident: InternedStr, ty: Option<Type>, initializer: Option<Expr>) -> Self {
        LetStmt {
            ident,
            ty,
            initializer,
        }
    }
}

#[derive(PartialEq, Debug)]
pub enum VarInitializer {
    Array(Vec<VarInitializer>),
    Statement(Stmt),
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub enum Expr {
    Array(Vec<Expr>),
    Call(Box<FnCall>),
    Binary(Box<BinaryExpr>),
    Unary(Box<UnaryExpr>),
    Literal(Literal),
    Match(Box<MatchExpr>),
    Closure(Box<ClosureExpr>),
    Assign(Box<AssignExpr>),
    AssignOp(Box<AssignOpExpr>),
    Field(Box<FieldExpr>),
    Index(Box<IndexExpr>),
    Path(Box<PathTy>),
    Break,
    Continue,
    Try(Box<Expr>),
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct FnCall {
    func: Box<Expr>,
    generic_tys: GenericDecls,
    args: Args,
}

impl FnCall {
    pub fn new(func: Box<Expr>, generic_tys: GenericDecls, args: Args) -> Self {
        Self {
            func,
            generic_tys,
            args,
        }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct BinaryExpr {
    operator: BinaryOp,
    lhs: Expr,
    rhs: Expr,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct UnaryExpr {
    operator: UnaryOp,
    expr: Expr,
}

impl UnaryExpr {
    pub fn new(operator: UnaryOp, expr: Expr) -> Self {
        Self {
            operator,
            expr,
        }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct MatchExpr {
    expr: Expr,
    arms: Vec<MatchArm>,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct MatchArm {
    pattern: Pattern,
    guard: Option<Expr>,
    body: Expr,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct ClosureExpr {
    params: Params,
    expr: Expr,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct AssignExpr {
    lhs: Expr,
    rhs: Expr,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct AssignOpExpr {
    op: BinaryOp,
    lhs: Expr,
    rhs: Expr,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct FieldExpr {
    lhs: Expr,
    ident: InternedStr,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct IndexExpr {
    lhs: Expr,
    rhs: Expr,
}


#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct RangeExpr {
    condition: Expr,
    increment: Expr,
}

impl RangeExpr {
    pub fn new(condition: Expr, increment: Expr) -> Self {
        Self {
            condition,
            increment,
        }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub enum Pattern {
    Wildcard,
    Range(RangePattern),
    Or(OrPattern),
    Literal(Literal),
    Ty(TypePattern),
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct RangePattern {
    start: Expr,
    end: Expr,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct OrPattern {
    patterns: Vec<Pattern>,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct TypePattern {
    tys: Vec<Type>,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct WhileStmt {
    condition: Expr,
    block_stmt: BlockStmt
}

impl WhileStmt {
    
    pub fn new(condition: Expr, block_stmt: BlockStmt) -> Self {
        Self { condition, block_stmt }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct BlockStmt {
    stmts: Vec<Stmt>,
}

impl BlockStmt {
    pub fn new(stmts: Vec<Stmt>) -> Self {
        Self { stmts }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub enum Literal {
    IntegerLiteral(i64),
    FloatingPointerLiteral(f64),
    BooleanLiteral(bool),
    StringLiteral(InternedStr),
    None,
}

#[derive(PartialEq, Eq, Debug)]
pub enum AssignmentOp {
    Assign,
    PlusAssign,
    SubtractAssign,
    MultiplyAssign,
    DivideAssign,
    ModuloAssign,
}

#[derive(PartialEq, Eq, Debug, Serialize, Deserialize)]
pub enum BinaryOp {
    Or,
    And,
    Equal,
    NotEqual,
    Less,
    Greater,
    LessEq,
    GreaterEq,
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo,
}

#[derive(PartialEq, Eq, Debug, Serialize, Deserialize)]
pub enum UnaryOp {
    Bang,
    Minus,
    Plus,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub enum Stmt {
    Use(UseStmt),
    Let(LetStmt),
    Class(ClassStmt),
    Enum(EnumStmt),
    Trait(TraitStmt),
    TraitImpl(TraitImplStmt),
    Fn(FnStmt),
    For(ForStmt),
    If(IfStmt),
    Return(ReturnStmt),
    While(WhileStmt),
    Block(BlockStmt),
    Expression(Expr),
}

mod tests {}
