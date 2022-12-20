use crate::compiler::parser::ClassType;
use crate::compiler::types::types::{InternedStr, Type};
use crate::gc::block::Block;
use crate::traits::traits::Trait;
use serde::{Deserialize, Serialize};
use std::path::{Path, Prefix};

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
pub struct TraitBound {
    bounds: Vec<PathTy>,
}

impl TraitBound {
    pub fn new(bounds: Vec<PathTy>) -> Self {
        Self {
            bounds,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct PathTy {
    ident: QualifiedIdent,
    generics: Generics,
}

impl PathTy {
    pub fn new(ident: QualifiedIdent, generics: Generics) -> Self {
        Self { ident, generics, }
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
pub struct Generics {
    generics: Vec<Generic>,
}

const EMPTY_GENERIC_DECL: Generics = Generics::new(Vec::new());

impl Generics {
    pub const fn new(generics: Vec<Generic>) -> Self {
        Self { generics }
    }

    pub fn empty() -> Self {
        EMPTY_GENERIC_DECL
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Debug, Serialize, Deserialize)]
pub struct GenericCallSite {
    generics: Vec<Type>,
}

impl GenericCallSite {
    pub fn new(generics: Vec<Type>) -> Self {
        Self { generics }
    }
}

#[derive(PartialEq, Eq, Debug, Default, Clone, Serialize, Deserialize)]
pub struct Params {
    params: Vec<Param>,
}

impl Params {
    pub const fn new(params: Vec<Param>) -> Self {
        Self { params }
    }
}

pub const EMPTY_ARGS: Args = Args::new(Vec::new());

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct Args {
    args: Vec<Expr>,
}

impl Args {
    pub const fn new(args: Vec<Expr>) -> Self {
        Self { args }
    }

    pub fn empty() -> Self {
        EMPTY_ARGS
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
    generic_types: Generics,
    members: Params,
    member_functions: Vec<FnStmt>,
}

impl ClassStmt {
    pub fn new(
        name: InternedStr,
        class_type: ClassType,
        generic_types: Generics,
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
    generics: Generics,
    members: Vec<EnumMemberStmt>,
    fn_stmts: Vec<FnStmt>,
}

impl EnumStmt {
    pub fn new(
        name: InternedStr,
        generics: Generics,
        members: Vec<EnumMemberStmt>,
        fn_stmts: Vec<FnStmt>,
    ) -> Self {
        Self {
            name,
            generics,
            members,
            fn_stmts,
        }
    }

    pub fn generics(&self) -> &Generics {
        &self.generics
    }

    pub fn members(&self) -> &[EnumMemberStmt] {
        &self.members
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct TraitStmt {
    ident: InternedStr,
    generics: Generics,
    functions: Vec<FnStmt>,
}

impl TraitStmt {
    pub fn new(
        ident: InternedStr,
        generics: Generics,
        functions: Vec<FnStmt>,
    ) -> Self {
        Self {
            ident,
            generics,
            functions,
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
        Self {
            condition,
            if_true,
            if_false,
        }
    }
}

pub enum ParameterizedType {
    Concrete(Type),
    Generic(Generic),
}

#[derive(PartialEq, Eq, Hash, Clone, Debug, Serialize, Deserialize)]
pub struct Generic {
    ident: InternedStr,
    trait_bound: Option<TraitBound>,
}

impl Generic {
    pub fn new(ident: InternedStr, trait_bound: Option<TraitBound>) -> Self {
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
    generic_types: Generics,
    parameters: Params,
    return_type: Option<Type>,
}

impl FnSig {
    pub fn new(
        name: InternedStr,
        generic_types: Generics,
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
    mutability: Mutability,
    ty: Option<Type>,
    initializer: Option<Expr>,
}

impl LetStmt {
    pub fn new(ident: InternedStr, mutability: Mutability, ty: Option<Type>, initializer: Option<Expr>) -> Self {
        LetStmt {
            ident,
            mutability,
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
    Array(Box<ArrayExpr>),
    Call(Box<Call>),
    Infix(Box<InfixExpr>),
    Unary(Box<UnaryExpr>),
    None,
    Boolean(bool),
    Integer(i64),
    Float(f64),
    String(InternedStr),
    Match(Box<MatchExpr>),
    Closure(Box<ClosureExpr>),
    Assign(Box<AssignExpr>),
    Field(Box<FieldExpr>),
    Index(Box<IndexExpr>),
    Path(PathExpr),
    Break,
    Continue,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct Call {
    func: Expr,
    generic_tys: Generics,
    args: Args,
}

impl Call {
    pub fn new(func: Expr, generic_tys: Generics, args: Args) -> Self {
        Self {
            func,
            generic_tys,
            args,
        }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct InfixExpr {
    operator: InfixOp,
    lhs: Expr,
    rhs: Expr,
}

impl InfixExpr {
    pub fn new(lhs: Expr, rhs: Expr, operator: InfixOp) -> Self {
        Self { operator, lhs, rhs }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct UnaryExpr {
    operator: UnaryOp,
    expr: Expr,
}

impl UnaryExpr {
    pub fn new(operator: UnaryOp, expr: Expr) -> Self {
        Self { operator, expr }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct MatchExpr {
    source: Expr,
    arms: Vec<MatchArm>,
}

impl MatchExpr {
    pub fn new(source: Expr, arms: Vec<MatchArm>) -> Self {
        Self {
            source,
            arms
        }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct MatchArm {
    pattern: Pattern,
    body: Stmt,
}

impl MatchArm {
    pub fn new(pattern: Pattern, body: Stmt) -> Self {
        Self {
            pattern,
            body
        }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct ClosureExpr {
    params: Vec<InternedStr>,
    stmt: Stmt,
}

impl ClosureExpr {
    pub fn new(params: Vec<InternedStr>, stmt: Stmt) -> Self {
        Self { params, stmt }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct AssignExpr {
    lhs: Expr,
    rhs: Expr,
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct FieldExpr {
    lhs: Expr,
    ident: InternedStr,
}

impl FieldExpr {
    pub fn new(lhs: Expr, ident: InternedStr) -> Self {
        Self { lhs, ident }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct IndexExpr {
    expr: Expr,
    key: Expr,
}

impl IndexExpr {
    pub fn new(expr: Expr, key: Expr) -> Self {
        Self { expr, key }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub enum ArrayExpr {
    SizedInitializer(Expr, Expr),
    Initializer(Vec<Expr>)
}

#[derive(Clone, PartialEq, Eq, Hash, Debug, Serialize, Deserialize)]
pub enum PathSegment {
    Identifier(InternedStr),
    Generic(Vec<Type>),
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct PathExpr {
    segments: Vec<PathSegment>,
}

impl PathExpr {
    pub fn new(segments: Vec<PathSegment>) -> Self {
        Self { segments }
    }
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
    Or(OrPattern),
    Boolean(bool),
    Integer(i64),
    Float(f64),
    String(InternedStr),
    Ty(Type, Option<InternedStr>),
    None
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub enum Range {
    Default(i64, i64), // 1..3
    Full, // ..
    From(i64), // 1..
    FromInclusive(i64, i64), // 1..=3
    To(i64), // ..3
    ToInclusive(i64) // ..=3
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct OrPattern {
    patterns: Vec<Pattern>,
}

impl OrPattern {
    pub fn new(patterns: Vec<Pattern>) -> Self {
        Self {
            patterns
        }
    }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub struct WhileStmt {
    condition: Expr,
    block_stmt: BlockStmt,
}

impl WhileStmt {
    pub fn new(condition: Expr, block_stmt: BlockStmt) -> Self {
        Self {
            condition,
            block_stmt,
        }
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

#[derive(PartialEq, Eq, Debug)]
pub enum AssignmentOp {
    Assign,
    PlusAssign,
    SubtractAssign,
    MultiplyAssign,
    DivideAssign,
    ModuloAssign,
}

#[derive(PartialEq, Eq, Debug, Serialize, Deserialize, Copy, Clone)]
pub enum UnaryOp {
    Bang,
    Minus,
    Plus,
    BitwiseComplement,
}

impl UnaryOp {
    pub fn binding_power(self) -> ((), u8) {
        ((), 23)
    }
}

#[derive(PartialEq, Eq, Debug, Serialize, Deserialize, Copy, Clone)]
pub enum PostfixOp {
    LeftBracket,
    LeftParentheses,
    Dot,
}

impl PostfixOp {
    pub fn binding_power(self) -> (u8, ()) {
        (24, ())
    }
}

#[derive(PartialEq, Eq, Debug, Serialize, Deserialize, Copy, Clone)]
pub enum InfixOp {
    Assign,
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Or,
    And,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    BitwiseOr,
    BitwiseAnd,
    BitwiseComplement,
    BitwiseXor,
    Equal,
    NotEqual,
    LeftShift,
    RightShift,
    TripleRightShift,
}

impl InfixOp {
    pub fn binding_power(self) -> (u8, u8) {
        match self {
            InfixOp::Assign => (2, 1),
            InfixOp::Or => (4, 3),
            InfixOp::And => (5, 6),
            InfixOp::BitwiseOr => (7, 8),
            InfixOp::BitwiseXor => (9, 10),
            InfixOp::BitwiseAnd => (11, 12),
            InfixOp::Equal | InfixOp::NotEqual => (13, 14),
            InfixOp::Less | InfixOp::Greater | InfixOp::LessEqual | InfixOp::GreaterEqual => {
                (15, 16)
            },
            InfixOp::LeftShift | InfixOp::RightShift | InfixOp::TripleRightShift => {
                (17, 18)
            },
            InfixOp::Add | InfixOp::Subtract => (19, 20),
            InfixOp::Multiply | InfixOp::Divide | InfixOp::Modulo => (21, 22),
            _ => panic!(),
        }
    }
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
    Expression {
        expr: Expr,
        implicit_return: bool
    },
}

mod tests {}
