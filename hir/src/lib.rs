#![allow(unused)]

use std::collections::BTreeMap;
use std::fmt::Debug;

use serde::{Deserialize, Serialize};

use ast::{ClassType, Ident, MaybeFnDef, ModulePath, Mutability, UnaryOp, ValueDef};
use id::{CrateId, DefId, LocalDefId, ModuleId};
use interner::InternedStr;
use span::Span;
use types::{LDefMap, StrSet};

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub enum Node<'a> {
    Item(&'a Item<'a>),

    Ty(&'a Ty<'a>),
    // Separate type for GenericParam since the definition of the param is separate from a reference
    GenericParam(&'a GenericParam<'a>),

    Expr(&'a Expr<'a>),
    DestructureExpr(&'a DestructureExpr<'a>),
    Stmt(&'a Stmt<'a>),
    Block(&'a Block<'a>),

    Param(&'a Param),
    Field(&'a Field),
    LocalVar(LocalVar),
    Pattern(&'a Pattern<'a>),
    MatchArm(&'a MatchArm<'a>),
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct Item<'a> {
    pub kind: ItemKind<'a>,
    pub span: Span,
    pub id: LocalDefId,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub enum ItemKind<'a> {
    Constant(&'a Constant<'a>),
    Class(&'a ClassDef<'a>),
    Enum(&'a EnumDef<'a>),
    Member(&'a MemberDef<'a>),
    Fn(&'a FnDef<'a>),
    Trait(&'a TraitDef<'a>),
    TraitImpl(&'a TraitImplDef<'a>),
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct Expr<'a> {
    pub kind: ExprKind<'a>,
    pub span: Span,
    pub id: LocalDefId,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub enum ExprKind<'a> {
    Array(ArrayExpr<'a>),
    Call(CallExpr<'a>),
    Infix(InfixExpr<'a>),
    Unary(UnaryExpr<'a>),
    None,
    True,
    False,
    Int(i64),
    UInt(u64),
    Float(f64),
    String(InternedStr),
    Match(MatchExpr<'a>),
    Closure(ClosureExpr<'a>),
    Assign(AssignExpr<'a>),
    Field(FieldExpr<'a>),
    Index(IndexExpr<'a>),
    Path(PathExpr<'a>),
    Block(Block<'a>),
    Break,
    Continue,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct Ty<'a> {
    pub kind: TyKind<'a>,
    pub span: Span,
    pub id: LocalDefId,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub enum TyKind<'a> {
    Array(&'a Ty<'a>),
    Path(&'a PathTy<'a>),
    GenericParam(&'a GenericParam<'a>),
    TraitBound(TraitBound<'a>),
    Closure(Closure<'a>),
    Primitive(Primitive),
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct DestructureExpr<'a> {
    pub kind: DestructureExprKind<'a>,
    pub span: Span,
    pub id: LocalDefId,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub enum DestructureExprKind<'a> {
    Pattern(DestructurePattern<'a>),
    Identifier(LocalVar),
    None,
    True,
    False,
    Int(i64),
    UInt(u64),
    Float(f64),
    String(InternedStr),
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct Stmt<'a> {
    pub kind: StmtKind<'a>,
    pub span: Span,
    pub id: LocalDefId,
}

impl<'a> Stmt<'a> {
    pub fn is_return(&self) -> bool {
        match &self.kind {
            StmtKind::Return(_) => true,
            StmtKind::Expression(Expression {
                expr,
                implicit_return: true,
            }) => true,
            _ => false,
        }
    }
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub enum StmtKind<'a> {
    Let(&'a LetStmt<'a>),
    For(&'a ForStmt<'a>),
    If(&'a IfStmt<'a>),
    Return(&'a ReturnStmt<'a>),
    While(&'a WhileStmt<'a>),
    Block(&'a BlockStmt<'a>),
    Expression(&'a Expression<'a>),
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct Pattern<'a> {
    pub kind: PatternKind<'a>,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub enum PatternKind<'a> {
    Wildcard,
    Or(OrPattern<'a>),
    None,
    True,
    False,
    Int(i64),
    UInt(u64),
    Float(f64),
    String(InternedStr),
    Ty(TyPattern<'a>),
    Destructure(DestructurePattern<'a>),
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct Constant<'a> {
    pub local_var: LocalVar,
    pub ty: &'a Ty<'a>,
    pub initializer: &'a Expr<'a>,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct ClassDef<'a> {
    pub name: Ident,
    pub class_type: ClassType,
    pub generic_params: GenericParams<'a>,
    pub fields: Fields<'a>,
    pub fn_defs: FnDefs<'a>,
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct EnumDef<'a> {
    pub name: Ident,
    pub generic_params: GenericParams<'a>,
    pub members: MemberDefs<'a>,
    pub fn_defs: FnDefs<'a>,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct MemberDef<'a> {
    pub name: InternedStr,
    pub fields: Fields<'a>,
    pub fn_defs: FnDefs<'a>,
    pub span: Span,
    pub id: LocalDefId,
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct TraitDef<'a> {
    pub name: Ident,
    pub generic_params: GenericParams<'a>,
    pub fn_defs: FnDefs<'a>,
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct TraitImplDef<'a> {
    pub trait_to_impl: &'a PathTy<'a>,
    pub target_ty: DefId,
    pub fn_defs: FnDefs<'a>,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct FnDef<'a> {
    pub sig: FnSig<'a>,
    pub body: Option<&'a Block<'a>>,
    pub span: Span,
    pub id: LocalDefId,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct FnSig<'a> {
    pub name: Ident,
    pub generic_params: GenericParams<'a>,
    pub params: Params<'a>,
    pub ret_ty: Option<&'a Ty<'a>>,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct Param {
    pub local_var: LocalVar,
    pub ty: DefId,
    pub mutability: Mutability,
    pub span: Span,
    pub id: LocalDefId,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct BlockStmt<'a> {
    pub block: Block<'a>,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct Block<'a> {
    pub stmts: Stmts<'a>,
    pub span: Span,
    pub id: LocalDefId,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct Expression<'a> {
    pub expr: &'a Expr<'a>,
    pub implicit_return: bool,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub enum Primitive {
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
    F32,
    F64,
    Str,
    Boolean,
    None,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct PathTy<'a> {
    pub definition: DefId,
    pub generics: Generics<'a>,
}

pub type TraitBound<'a> = &'a [&'a PathTy<'a>];
pub type Generics<'a> = &'a [&'a Ty<'a>];
pub type Args<'a> = &'a [&'a Expr<'a>];
pub type Stmts<'a> = &'a [&'a Stmt<'a>];
pub type AnonParams<'a> = &'a [&'a Ty<'a>];
pub type Initializers<'a> = &'a [&'a Expr<'a>];
pub type Exprs<'a> = &'a [&'a Expr<'a>];
pub type DestructureExprs<'a> = &'a [&'a DestructureExpr<'a>];
pub type GenericParams<'a> = &'a [&'a GenericParam<'a>];
pub type Fields<'a> = &'a [&'a Field];
pub type ClosureParams<'a> = &'a [ClosureParam];
pub type Params<'a> = &'a [&'a Param];
pub type FnDefs<'a> = &'a [&'a FnDef<'a>];
pub type MemberDefs<'a> = &'a [&'a MemberDef<'a>];
pub type MatchArms<'a> = &'a [&'a MatchArm<'a>];
pub type Segments<'a> = &'a [&'a Segment<'a>];
pub type Patterns<'a> = &'a [&'a Pattern<'a>];

#[derive(PartialEq, Debug, Clone, Copy, Serialize)]
pub enum Literal {
    None,
    True,
    False,
    Integer(i64),
    Float(f64),
    String(InternedStr),
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub enum ArrayExpr<'a> {
    Sized {
        initializer: &'a Expr<'a>,
        size: &'a Expr<'a>,
    },
    Unsized {
        initializers: Initializers<'a>,
    },
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct CallExpr<'a> {
    pub target: &'a Expr<'a>,
    pub args: Args<'a>,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct UnaryExpr<'a> {
    pub operator: UnaryOp,
    pub expr: &'a Expr<'a>,
}

#[derive(PartialEq, Eq, Hash, Debug, Serialize, Deserialize, Copy, Clone)]
pub enum InfixOpClass {
    Assignment,
    Arithmetic,
    Logical,
    Comparison,
    Bitwise,
}

impl From<ast::InfixOp> for InfixOp {
    fn from(value: ast::InfixOp) -> Self {
        match value {
            ast::InfixOp::Assign => InfixOp::Assign,
            ast::InfixOp::Add => InfixOp::Add,
            ast::InfixOp::Subtract => InfixOp::Subtract,
            ast::InfixOp::Multiply => InfixOp::Multiply,
            ast::InfixOp::Divide => InfixOp::Divide,
            ast::InfixOp::Modulo => InfixOp::Modulo,
            ast::InfixOp::Or => InfixOp::Or,
            ast::InfixOp::And => InfixOp::And,
            ast::InfixOp::Less => InfixOp::Less,
            ast::InfixOp::Greater => InfixOp::Greater,
            ast::InfixOp::LessEqual => InfixOp::LessEqual,
            ast::InfixOp::GreaterEqual => InfixOp::GreaterEqual,
            ast::InfixOp::BitwiseOr => InfixOp::BitwiseOr,
            ast::InfixOp::BitwiseAnd => InfixOp::BitwiseAnd,
            ast::InfixOp::BitwiseComplement => InfixOp::BitwiseComplement,
            ast::InfixOp::BitwiseXor => InfixOp::BitwiseXor,
            ast::InfixOp::Equal => InfixOp::Equal,
            ast::InfixOp::NotEqual => InfixOp::NotEqual,
            ast::InfixOp::LeftShift => InfixOp::LeftShift,
            ast::InfixOp::RightShift => InfixOp::RightShift,
            ast::InfixOp::TripleRightShift => InfixOp::TripleRightShift,
        }
    }
}

#[derive(PartialEq, Eq, Hash, Debug, Serialize, Deserialize, Copy, Clone)]
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
    pub fn class(&self) -> InfixOpClass {
        match self {
            InfixOp::Assign => InfixOpClass::Assignment,
            InfixOp::Add
            | InfixOp::Subtract
            | InfixOp::Multiply
            | InfixOp::Divide
            | InfixOp::Modulo => InfixOpClass::Arithmetic,
            InfixOp::Or | InfixOp::And => InfixOpClass::Logical,
            InfixOp::Equal
            | InfixOp::NotEqual
            | InfixOp::LessEqual
            | InfixOp::Less
            | InfixOp::LessEqual
            | InfixOp::Greater
            | InfixOp::GreaterEqual => InfixOpClass::Comparison,
            InfixOp::BitwiseOr
            | InfixOp::BitwiseAnd
            | InfixOp::BitwiseComplement
            | InfixOp::BitwiseXor
            | InfixOp::LeftShift
            | InfixOp::RightShift
            | InfixOp::TripleRightShift => InfixOpClass::Bitwise,
        }
    }
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct InfixExpr<'a> {
    pub operator: InfixOp,
    pub lhs: &'a Expr<'a>,
    pub rhs: &'a Expr<'a>,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct MatchExpr<'a> {
    pub source: &'a Expr<'a>,
    pub arms: MatchArms<'a>,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct MatchArm<'a> {
    pub pattern: &'a Pattern<'a>,
    pub body: &'a Stmt<'a>,
    pub span: Span,
    pub id: LocalDefId,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct OrPattern<'a> {
    pub patterns: Patterns<'a>,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct TyPattern<'a> {
    pub ty: &'a PathTy<'a>,
    pub ident: Option<LocalVar>,
}

#[derive(Copy, Clone, PartialEq, Debug, Serialize)]
pub struct LocalVar {
    pub ident: InternedStr,
    pub span: Span,
    pub id: LocalDefId,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct DestructurePattern<'a> {
    pub ty: &'a PathTy<'a>,
    pub exprs: DestructureExprs<'a>,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct Closure<'a> {
    pub params: AnonParams<'a>,
    pub ret_ty: &'a Ty<'a>,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct ClosureExpr<'a> {
    pub params: ClosureParams<'a>,
    pub stmt: &'a Stmt<'a>,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct AssignExpr<'a> {
    pub lhs: &'a Expr<'a>,
    pub rhs: &'a Expr<'a>,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct FieldExpr<'a> {
    pub lhs: &'a Expr<'a>,
    pub ident: Ident,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct IndexExpr<'a> {
    pub expr: &'a Expr<'a>,
    pub key: &'a Expr<'a>,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct PathExpr<'a> {
    pub segments: Segments<'a>,
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub enum Res {
    Crate(CrateId),
    ModuleSegment(CrateId, ModulePath),
    Module(ModuleId),
    ValueDef(ValueDef),
    Fn(MaybeFnDef),
    // These have to be late resolved after type information is deduced?
    Local(LocalDef),
    Primitive(Primitive),
}

#[derive(PartialEq, Debug, Clone, Copy, Serialize)]
pub enum LocalDef {
    Var(LocalVar),
    Generic(LocalDefId),
}

#[derive(PartialEq, Eq, Debug, Clone, Copy, Serialize)]
pub enum DefTy {
    GlobalLet,
    Class,
    Enum,
    EnumMember,
    Trait,
    Fn,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct Segment<'a> {
    pub res: &'a Res,
    pub generics: Option<Generics<'a>>,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct Field {
    pub name: Ident,
    pub ty: DefId,
    pub span: Span,
    pub id: LocalDefId,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct LetStmt<'a> {
    pub local_var: LocalVar,
    pub mutability: Mutability,
    pub ty: Option<&'a Ty<'a>>,
    pub initializer: Option<&'a Expr<'a>>,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct ReturnStmt<'a> {
    pub value: Option<&'a Expr<'a>>,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct WhileStmt<'a> {
    pub condition: &'a Expr<'a>,
    pub block: &'a Block<'a>,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct ForStmt<'a> {
    pub ident: LocalVar,
    pub range: &'a Expr<'a>,
    pub body: &'a Block<'a>,
}

/// TODO: Consider making if statements an expression so that they can yield a value.
/// Alternatively you can just wrap an if statement in a function to yield a value, but this is clumsy.
#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct IfStmt<'a> {
    pub condition: Expr<'a>,
    pub if_true: Block<'a>,
    pub if_false: Option<Block<'a>>,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
#[repr(transparent)]
#[serde(transparent)]
pub struct ClosureParam {
    pub ident: Ident,
}

#[derive(PartialEq, Debug, Copy, Clone, Serialize)]
pub struct GenericParam<'a> {
    pub ident: Ident,
    pub trait_bound: Option<TraitBound<'a>>,
    pub span: Span,
    pub id: LocalDefId,
}

#[derive(PartialEq, Debug, Default, Clone, Serialize)]
pub struct HirMap<'a> {
    crates: Vec<HirCrate<'a>>,
    names_to_indices: StrSet,
}

impl<'a> HirMap<'a> {
    pub fn insert(&mut self, krate: HirCrate<'a>) {
        self.names_to_indices.insert(krate.name);
        self.crates.push(krate);
    }

    pub fn krate_by_name(&self, name: &InternedStr) -> &HirCrate {
        let index = self.names_to_indices.get_index_of(name).unwrap();
        &self.crates[index]
    }
    pub fn krate(&self, def_id: &DefId) -> &HirCrate {
        &self.crates[def_id.crate_id().as_usize()]
    }
    pub fn krates(&self) -> impl Iterator<Item = &HirCrate<'a>> {
        self.crates.iter()
    }

    pub fn into_krates(self) -> impl Iterator<Item = HirCrate<'a>> + Debug {
        self.crates.into_iter()
    }

    pub fn expect_item(&'a self, id: DefId) -> &'a Item<'a> {
        let krate = self.krate(&id);

        match krate.nodes.get(&id.local_id()).unwrap() {
            Node::Item(item) => item,
            _ => unreachable!(),
        }
    }
}

#[derive(PartialEq, Debug, Clone, Serialize)]
pub struct HirCrate<'a> {
    pub name: InternedStr,
    pub id: CrateId,
    pub items: Vec<&'a Item<'a>>,
    #[cfg(not(test))]
    pub nodes: LDefMap<Node<'a>>,
    #[cfg(test)]
    pub nodes: BTreeMap<LocalDefId, Node<'a>>,
}

impl<'a> HirCrate<'a> {
    pub fn node(&'a self, id: &LocalDefId) -> &'a Node<'a> {
        self.nodes.get(id).unwrap()
    }
}
