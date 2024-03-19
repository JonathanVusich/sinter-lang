#![allow(unused)]

use std::borrow::Borrow;
use std::collections::VecDeque;
use std::sync::Arc;

use itertools::Itertools;
use nibble_vec::Nibblet;
use radix_trie::TrieKey;
use serde::{Deserialize, Serialize};

use id::{DefId, LocalDefId, ModuleId};
use interner::{InternedStr, StringInterner};
use macros::named_slice;
use span::Span;
use types::{IStrMap, StrMap};

/// This trait describes a visitor that can traverse the AST and collect information.
pub trait AstPass<T>: Default
where
    T: From<Self>,
{
    fn visit(ast: &Module) -> T {
        let mut pass = Self::default();
        for item in &ast.items {
            pass.visit_item(item);
        }
        pass.into()
    }

    fn visit_item(&mut self, node: &Item);
}

/// This enum represents the various types of nodes in the AST.
#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub enum ItemKind {
    Use(UseStmt),
    GlobalLet(GlobalLetStmt),
    Class(ClassStmt),
    Enum(EnumStmt),
    Trait(TraitStmt),
    TraitImpl(TraitImplStmt),
    Fn(FnStmt),
}

/// This struct represents the node plus diagnostic information.
#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct Item {
    pub kind: ItemKind,
    pub span: Span,
    pub id: LocalDefId,
}

impl Item {
    pub fn new(kind: ItemKind, span: Span, id: LocalDefId) -> Self {
        Self { kind, span, id }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub enum ExprKind {
    Array(ArrayExpr),
    Call(CallExpr),
    Infix(InfixExpr),
    Unary(UnaryExpr),
    True,
    False,
    None,
    Float(f64),
    Int(i64),
    UInt(u64),
    String(InternedStr),
    Match(MatchExpr),
    Closure(ClosureExpr),
    Assign(AssignExpr),
    Field(FieldExpr),
    Index(IndexExpr),
    Path(PathExpr),
    Parentheses(Parentheses),
    Break,
    Continue,
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
    pub id: LocalDefId,
}

impl Expr {
    pub fn new(kind: ExprKind, span: Span, id: LocalDefId) -> Self {
        Self { kind, span, id }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct Ty {
    pub kind: TyKind,
    pub span: Span,
    pub id: LocalDefId,
}

impl Ty {
    pub fn new(kind: TyKind, span: Span, id: LocalDefId) -> Self {
        Self { kind, span, id }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub enum TyKind {
    Array { ty: Box<Ty> },
    Path { path: PathTy },
    TraitBound { trait_bound: TraitBound },
    Closure { params: Vec<Ty>, ret_ty: Box<Ty> },
    QSelf,
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

#[derive(PartialEq, Debug, Default, Serialize, Deserialize)]
pub struct Module {
    pub id: ModuleId,
    pub path: ModulePath,
    pub namespace: ModuleNS,
    pub items: Vec<Item>,
}

impl Module {
    pub fn new(items: Vec<Item>) -> Self {
        Self {
            path: Default::default(),
            id: Default::default(),
            namespace: Default::default(),
            items,
        }
    }
}

#[derive(PartialEq, Eq, Default, Hash, Debug, Clone, Serialize, Deserialize)]
pub struct ModulePath {
    module_path: VecDeque<InternedStr>,
}

impl ModulePath {
    pub fn from_iter<T: IntoIterator<Item = InternedStr>>(module_path: T) -> Self {
        Self {
            module_path: VecDeque::from_iter(module_path),
        }
    }

    pub fn concat(&self, mut other: Self) -> Self {
        self.module_path
            .iter()
            .rev()
            .for_each(|seg| other.module_path.push_front(*seg));
        other
    }

    pub fn front(&self) -> Option<InternedStr> {
        self.module_path.front().copied()
    }

    pub fn back(&self) -> Option<InternedStr> {
        self.module_path.back().copied()
    }

    pub fn pop_front(&mut self) -> Option<InternedStr> {
        self.module_path.pop_front()
    }

    pub fn pop_back(&mut self) -> Option<InternedStr> {
        self.module_path.pop_back()
    }

    fn push_front(&mut self, value: InternedStr) {
        self.module_path.push_front(value)
    }

    fn push_back(&mut self, value: InternedStr) {
        self.module_path.push_back(value)
    }

    fn append(&mut self, deque: VecDeque<InternedStr>) {
        self.module_path.extend(deque)
    }
}

impl TrieKey for ModulePath {
    #[inline]
    fn encode(&self) -> Nibblet {
        let mut nibblet = Nibblet::new();
        for seg in self.module_path.iter().copied() {
            let bytes = seg.into_inner().to_be_bytes();
            for byte in bytes {
                nibblet.push(byte);
            }
        }
        nibblet
    }
}

impl<T> From<T> for ModulePath
where
    T: Borrow<QualifiedIdent>,
{
    fn from(value: T) -> Self {
        let qualified_ident = value.borrow();
        let inner = qualified_ident
            .idents
            .iter()
            .map(|ident| ident.ident)
            .collect();
        Self { module_path: inner }
    }
}

#[derive(PartialEq, Eq, Debug, Default, Serialize, Deserialize)]
pub struct ModuleNS {
    pub modules: IStrMap<ModuleId>,
    // We have one namespace for classes, fns and constants because they are all callable and cannot
    // be disambiguated at resolve time by their usage.
    pub values: IStrMap<ValueDef>,
}

impl ModuleNS {
    pub fn find_ident_with_module(&self, ident: &QualifiedIdent) -> Option<DefId> {
        match ident.idents.as_slice() {
            [_module, ident] => self.values.get(&ident.ident).map(ValueDef::id),
            [_module, enm, member] => {
                if let Some(ValueDef::Enum(enum_def)) = self.values.get(&enm.ident) {
                    return enum_def.members.get(&member.ident).map(|def| def.id);
                }
                None
            }
            _ => None,
        }
    }
    pub fn find_ident(&self, ident: &QualifiedIdent) -> Option<DefId> {
        match ident.idents.as_slice() {
            [ident] => self.values.get(&ident.ident).map(ValueDef::id),
            [enm, member] => {
                if let Some(ValueDef::Enum(enum_def)) = self.values.get(&enm.ident) {
                    return enum_def.members.get(&member.ident).map(|def| def.id);
                }
                None
            }
            _ => None,
        }
    }

    pub fn find_value(&self, value: InternedStr) -> Option<&ValueDef> {
        self.values.get(&value)
    }

    pub fn find_module(&self, module: InternedStr) -> Option<ModuleId> {
        self.modules.get(&module).copied()
    }
}

#[derive(PartialEq, Eq, Debug, Copy, Clone, Serialize, Deserialize)]
pub enum MaybeFnDef {
    None(InternedStr),
    Some(FnDef),
}

#[derive(PartialEq, Eq, Debug, Copy, Clone, Serialize, Deserialize)]
pub struct FnDef {
    pub id: DefId,
}

impl FnDef {
    pub fn new(id: DefId) -> Self {
        Self { id }
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Serialize, Deserialize)]
pub struct FieldDef {
    pub id: DefId,
}

impl FieldDef {
    pub fn new(id: DefId) -> Self {
        Self { id }
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Serialize, Deserialize)]
pub struct TraitFnDef {
    pub trait_id: DefId,
    pub id: DefId,
}

#[derive(PartialEq, Eq, Debug, Clone, Serialize, Deserialize)]
pub struct TraitDef {
    pub id: DefId,
    pub fns: IStrMap<FnDef>,
}

impl TraitDef {
    pub fn new(id: DefId, fns: IStrMap<FnDef>) -> Self {
        Self { id, fns }
    }
}

#[derive(PartialEq, Eq, Debug, Copy, Clone, Serialize, Deserialize)]
pub struct GlobalVarDef {
    pub id: DefId,
}

impl GlobalVarDef {
    pub fn new(id: DefId) -> Self {
        Self { id }
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Serialize, Deserialize)]
pub struct ClassDef {
    pub id: DefId,
    pub fields: IStrMap<FieldDef>,
    pub fns: IStrMap<FnDef>,
}

impl ClassDef {
    pub fn new(id: DefId, fields: StrMap<FieldDef>, fns: StrMap<FnDef>) -> Self {
        Self {
            id,
            fields: Arc::new(fields),
            fns: Arc::new(fns),
        }
    }
}

/// Cloning this type should be cheap since it uses an immutable map.
#[derive(PartialEq, Eq, Debug, Clone, Serialize, Deserialize)]
pub struct EnumDef {
    pub id: DefId,
    pub members: IStrMap<EnumMemberDef>,
    pub fns: IStrMap<FnDef>,
}

impl EnumDef {
    pub fn new(id: DefId, members: IStrMap<EnumMemberDef>, fns: IStrMap<FnDef>) -> Self {
        Self { id, members, fns }
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Serialize, Deserialize)]
pub struct EnumMemberDef {
    pub id: DefId,
    pub fns: IStrMap<FnDef>,
}

impl EnumMemberDef {
    pub fn new(id: DefId, fns: IStrMap<FnDef>) -> Self {
        Self { id, fns }
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Serialize, Deserialize)]
pub enum ValueDef {
    GlobalVar(GlobalVarDef),
    Class(ClassDef),
    Enum(),
    EnumMember(EnumMemberDef),
    Trait(TraitDef),
    Fn(FnDef),
}

impl ValueDef {
    pub fn id(&self) -> DefId {
        match self {
            ValueDef::GlobalVar(let_stmt) => let_stmt.id,
            ValueDef::Class(class_stmt) => class_stmt.id,
            ValueDef::Enum(enum_stmt) => enum_stmt.id,
            ValueDef::EnumMember(enum_member) => enum_member.id,
            ValueDef::Trait(trait_stmt) => trait_stmt.id,
            ValueDef::Fn(fn_stmt) => fn_stmt.id,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Ident {
    pub ident: InternedStr,
    pub span: Span,
}

impl Ident {
    pub fn new(ident: InternedStr, span: Span) -> Self {
        Self { ident, span }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct QualifiedIdent {
    pub ident_type: IdentType,
    pub idents: Vec<Ident>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum IdentType {
    Crate,
    LocalOrUse,
}

impl QualifiedIdent {
    pub fn new(ident_type: IdentType, idents: Vec<Ident>) -> Self {
        if idents.is_empty() {
            panic!("QualifiedIdent must have at least one identifier!")
        }
        Self { ident_type, idents }
    }

    pub fn len(&self) -> usize {
        self.idents.len()
    }

    pub fn first(&self) -> InternedStr {
        // It is safe to unwrap here since a QualifiedIdent should always have at least one element.
        self.idents.first().map(|ident| ident.ident).unwrap()
    }

    pub fn last(&self) -> InternedStr {
        // It is safe to unwrap here since a QualifiedIdent should always have at least one element.
        self.idents.last().map(|ident| ident.ident).unwrap()
    }

    pub fn is_single(&self) -> Option<InternedStr> {
        if self.idents.len() == 1 {
            Some(self.idents[0].ident)
        } else {
            None
        }
    }

    pub fn format(&self, string_interner: &StringInterner) -> String {
        self.idents
            .iter()
            .map(|ident| string_interner.resolve(ident.ident))
            .join("::")
    }
}

named_slice!(TraitBound, PathTy);
named_slice!(Generics, Ty);
named_slice!(GenericParams, GenericParam);
named_slice!(GenericCallSite, Ty);
named_slice!(Params, Param);
named_slice!(Fields, Field);
named_slice!(Args, Expr);
named_slice!(Segments, Segment);

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct PathTy {
    pub ident: QualifiedIdent,
    pub generics: Generics,
}

impl PathTy {
    pub fn new(ident: QualifiedIdent, generics: Generics) -> Self {
        Self { ident, generics }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash, Serialize, Deserialize)]
#[serde(transparent)]
pub struct UseStmt {
    pub path: QualifiedIdent,
}

impl UseStmt {
    pub fn new(path: QualifiedIdent) -> Self {
        Self { path }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct GenericParam {
    pub name: Ident,
    // Must be of type TraitBound, but need the entire Ty to store span + id.
    pub trait_bound: Option<Ty>,
    pub span: Span,
    pub id: LocalDefId,
}

impl GenericParam {
    pub fn new(ident: Ident, trait_bound: Option<Ty>, span: Span, id: LocalDefId) -> Self {
        Self {
            name: ident,
            trait_bound,
            span,
            id,
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Serialize, Deserialize)]
pub enum Mutability {
    Mutable,
    Immutable,
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct ClassStmt {
    pub name: Ident,
    pub class_type: ClassType,
    pub generic_params: GenericParams,
    pub fields: Fields,
    pub self_fns: Vec<FnSelfStmt>,
}

impl ClassStmt {
    pub fn new(
        name: Ident,
        class_type: ClassType,
        generic_params: GenericParams,
        fields: Fields,
        fn_stmts: Vec<FnSelfStmt>,
    ) -> Self {
        Self {
            name,
            class_type,
            generic_params,
            fields,
            self_fns: fn_stmts,
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug, Serialize, Deserialize)]
pub enum ClassType {
    Reference,
    Inline,
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct EnumStmt {
    pub name: Ident,
    pub generic_params: GenericParams,
    pub members: Vec<EnumMember>,
    pub self_fns: Vec<FnSelfStmt>,
}

impl EnumStmt {
    pub fn new(
        name: Ident,
        generic_params: GenericParams,
        members: Vec<EnumMember>,
        member_fns: Vec<FnSelfStmt>,
    ) -> Self {
        Self {
            name,
            generic_params,
            members,
            self_fns: member_fns,
        }
    }

    pub fn generic_params(&self) -> &GenericParams {
        &self.generic_params
    }

    pub fn members(&self) -> &[EnumMember] {
        &self.members
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct TraitStmt {
    pub name: Ident,
    pub generic_params: GenericParams,
    pub self_fns: Vec<FnSelfStmt>,
}

impl TraitStmt {
    pub fn new(ident: Ident, generic_params: GenericParams, member_fns: Vec<FnSelfStmt>) -> Self {
        Self {
            name: ident,
            generic_params,
            self_fns: member_fns,
        }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct TraitImplStmt {
    pub trait_to_impl: PathTy,
    pub target_ty: QualifiedIdent,
    pub self_fns: Vec<FnSelfStmt>,
}

impl TraitImplStmt {
    pub fn new(
        trait_to_impl: PathTy,
        target_ty: QualifiedIdent,
        member_fns: Vec<FnSelfStmt>,
    ) -> Self {
        Self {
            trait_to_impl,
            target_ty,
            self_fns: member_fns,
        }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct FnStmt {
    pub sig: FnSig,
    pub body: Option<Block>,
}

impl FnStmt {
    pub fn new(sig: FnSig, body: Option<Block>) -> Self {
        Self { sig, body }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct FnSelfStmt {
    pub sig: FnSig,
    pub body: Option<Block>,
    pub span: Span,
    pub id: LocalDefId,
}

impl FnSelfStmt {
    pub fn new(sig: FnSig, body: Option<Block>, span: Span, id: LocalDefId) -> Self {
        Self {
            sig,
            body,
            span,
            id,
        }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct ForStmt {
    pub ident: Ident,
    pub range: Box<Expr>,
    pub body: Block,
}

impl ForStmt {
    pub fn new(ident: Ident, range: Expr, body: Block) -> Self {
        Self {
            ident,
            range: Box::new(range),
            body,
        }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct ReturnStmt {
    pub value: Option<Box<Expr>>,
}

impl ReturnStmt {
    pub fn new(value: Option<Expr>) -> Self {
        Self {
            value: value.map(Box::new),
        }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct IfStmt {
    pub condition: Box<Expr>,
    pub if_true: Block,
    pub if_false: Option<Block>,
}

impl IfStmt {
    pub fn new(condition: Expr, if_true: Block, if_false: Option<Block>) -> Self {
        Self {
            condition: Box::new(condition),
            if_true,
            if_false,
        }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct EnumMember {
    pub name: InternedStr,
    pub fields: Fields,
    pub self_fns: Vec<FnSelfStmt>,
    pub span: Span,
    pub id: LocalDefId,
}

impl EnumMember {
    pub fn new(
        name: InternedStr,
        parameters: Fields,
        member_functions: Vec<FnSelfStmt>,
        span: Span,
        id: LocalDefId,
    ) -> Self {
        Self {
            name,
            fields: parameters,
            self_fns: member_functions,
            id,
            span,
        }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct FnSig {
    pub name: Ident,
    pub generic_params: GenericParams,
    pub params: Params,
    pub return_type: Option<Ty>,
}

impl FnSig {
    pub fn new(
        name: Ident,
        generic_params: GenericParams,
        parameters: Params,
        return_type: Option<Ty>,
    ) -> Self {
        Self {
            name,
            generic_params,
            params: parameters,
            return_type,
        }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct Param {
    pub name: Ident,
    pub ty: Ty,
    pub mutability: Mutability,
    pub span: Span,
    pub id: LocalDefId,
}

impl Param {
    pub fn new(ident: Ident, ty: Ty, mutability: Mutability, span: Span, id: LocalDefId) -> Self {
        Self {
            name: ident,
            ty,
            mutability,
            span,
            id,
        }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct Field {
    pub name: Ident,
    pub ty: Ty,
    pub span: Span,
    pub id: LocalDefId,
}

impl Field {
    pub fn new(ident: Ident, ty: Ty, span: Span, id: LocalDefId) -> Self {
        Self {
            name: ident,
            ty,
            span,
            id,
        }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct GlobalLetStmt {
    pub local_var: LocalVar,
    pub ty: Ty,
    pub initializer: Expr,
}

impl GlobalLetStmt {
    pub fn new(name: LocalVar, ty: Ty, initializer: Expr) -> Self {
        Self {
            local_var: name,
            ty,
            initializer,
        }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct LetStmt {
    pub local_var: LocalVar,
    pub mutability: Mutability,
    pub ty: Option<Ty>,
    pub initializer: Option<Box<Expr>>,
}

impl LetStmt {
    pub fn new(
        local_var: LocalVar,
        mutability: Mutability,
        ty: Option<Ty>,
        initializer: Option<Expr>,
    ) -> Self {
        Self {
            local_var,
            mutability,
            ty,
            initializer: initializer.map(Box::new),
        }
    }
}

#[derive(PartialEq, Debug)]
pub enum VarInitializer {
    Array(Vec<VarInitializer>),
    Statement(Stmt),
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct CallExpr {
    pub target: Box<Expr>,
    pub args: Args,
}

impl CallExpr {
    pub fn new(func: Expr, args: Args) -> Self {
        Self {
            target: Box::new(func),
            args,
        }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct InfixExpr {
    pub operator: InfixOp,
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

impl InfixExpr {
    pub fn new(lhs: Expr, rhs: Expr, operator: InfixOp) -> Self {
        Self {
            operator,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct UnaryExpr {
    pub operator: UnaryOp,
    pub expr: Box<Expr>,
}

impl UnaryExpr {
    pub fn new(operator: UnaryOp, expr: Expr) -> Self {
        Self {
            operator,
            expr: Box::new(expr),
        }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct MatchExpr {
    pub source: Box<Expr>,
    pub arms: Vec<MatchArm>,
}

impl MatchExpr {
    pub fn new(source: Expr, arms: Vec<MatchArm>) -> Self {
        Self {
            source: Box::new(source),
            arms,
        }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub body: Stmt,
    pub span: Span,
    pub id: LocalDefId,
}

impl MatchArm {
    pub fn new(pattern: Pattern, body: Stmt, span: Span, id: LocalDefId) -> Self {
        Self {
            pattern,
            body,
            span,
            id,
        }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct ClosureExpr {
    pub params: Vec<ClosureParam>,
    pub stmt: Stmt,
}

impl ClosureExpr {
    pub fn new(params: Vec<ClosureParam>, stmt: Stmt) -> Self {
        Self { params, stmt }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
#[repr(transparent)]
#[serde(transparent)]
pub struct ClosureParam {
    pub ident: Ident,
}

impl ClosureParam {
    pub fn new(ident: Ident) -> Self {
        Self { ident }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct AssignExpr {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct FieldExpr {
    pub lhs: Box<Expr>,
    pub ident: Ident,
}

impl FieldExpr {
    pub fn new(lhs: Expr, ident: Ident) -> Self {
        Self {
            lhs: Box::new(lhs),
            ident,
        }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct IndexExpr {
    pub expr: Box<Expr>,
    pub key: Box<Expr>,
}

impl IndexExpr {
    pub fn new(expr: Expr, key: Expr) -> Self {
        Self {
            expr: Box::new(expr),
            key: Box::new(key),
        }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub enum ArrayExpr {
    SizedInitializer(Box<Expr>, Box<Expr>),
    Initializer(Vec<Expr>),
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct Segment {
    pub ident: Ident,
    pub generics: Option<Generics>,
}

impl Segment {
    pub fn new(ident: Ident, generics: Option<Generics>) -> Self {
        Self { ident, generics }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct PathExpr {
    pub ident_type: IdentType,
    pub segments: Segments,
}

impl PathExpr {
    pub fn new(ident_type: IdentType, segments: Vec<Segment>) -> Self {
        Self {
            ident_type,
            segments: segments.into(),
        }
    }

    pub fn is_single(&self) -> Option<&Segment> {
        if let [first] = self.segments.as_slice() {
            Some(first)
        } else {
            None
        }
    }

    pub fn to_module_path(&self) -> QualifiedIdent {
        let mut idents = self
            .segments
            .into_iter()
            .map(|f| f.ident)
            .collect::<Vec<Ident>>();
        // Remove the last ident because it is not part of the module path.
        idents.pop();
        QualifiedIdent::new(self.ident_type, idents)
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct Parentheses {
    pub expr: Box<Expr>,
}

impl Parentheses {
    pub fn new(expr: Expr) -> Self {
        Self {
            expr: Box::new(expr),
        }
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

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub enum Pattern {
    Wildcard,
    // _
    Or(OrPattern),
    // pat | pat
    True,
    False,
    Float(f64),
    Int(i64),
    UInt(u64),
    String(InternedStr),
    None,
    // "true"
    Ty(TyPattern),
    // Logical logical => { }
    Destructure(DestructurePattern), // Logical(1, true, 100) => { }
}

#[derive(PartialEq, Debug, Serialize, Deserialize)]
pub enum Range {
    Default(i64, i64),
    // 1..3
    Full,
    // ..
    From(i64),
    // 1..
    FromInclusive(i64, i64),
    // 1..=3
    To(i64),
    // ..3
    ToInclusive(i64), // ..=3
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct OrPattern {
    pub patterns: Vec<Pattern>,
}

impl OrPattern {
    pub fn new(patterns: Vec<Pattern>) -> Self {
        Self { patterns }
    }
}

#[derive(Copy, Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct LocalVar {
    pub ident: InternedStr,
    pub span: Span,
    pub id: LocalDefId,
}

impl LocalVar {
    pub fn new(ident: InternedStr, span: Span, id: LocalDefId) -> Self {
        Self { ident, span, id }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct TyPattern {
    pub ty: PathTy,
    pub ident: Option<LocalVar>,
}

impl TyPattern {
    pub fn new(ty: PathTy, ident: Option<LocalVar>) -> Self {
        Self { ty, ident }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct DestructurePattern {
    pub ty: PathTy,
    pub exprs: Vec<DestructureExpr>,
}

impl DestructurePattern {
    pub fn new(ty: PathTy, exprs: Vec<DestructureExpr>) -> Self {
        Self { ty, exprs }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct DestructureExpr {
    pub kind: DestructureExprKind,
    pub span: Span,
    pub id: LocalDefId,
}

impl DestructureExpr {
    pub fn new(kind: DestructureExprKind, span: Span, id: LocalDefId) -> Self {
        Self { kind, span, id }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub enum DestructureExprKind {
    Pattern(DestructurePattern),
    Identifier(InternedStr),
    True,
    False,
    Float(f64),
    Int(i64),
    UInt(u64),
    String(InternedStr),
    None,
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct WhileStmt {
    pub condition: Box<Expr>,
    pub block_stmt: Block,
}

impl WhileStmt {
    pub fn new(condition: Expr, block_stmt: Block) -> Self {
        Self {
            condition: Box::new(condition),
            block_stmt,
        }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub span: Span,
    pub id: LocalDefId,
}

impl Block {
    pub fn new(stmts: Vec<Stmt>, span: Span, id: LocalDefId) -> Self {
        Self { stmts, span, id }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct Expression {
    pub expr: Box<Expr>,
    pub implicit_return: bool,
}

impl Expression {
    pub fn new(expr: Expr, implicit_return: bool) -> Self {
        Self {
            expr: Box::new(expr),
            implicit_return,
        }
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
            }
            InfixOp::LeftShift | InfixOp::RightShift | InfixOp::TripleRightShift => (17, 18),
            InfixOp::Add | InfixOp::Subtract => (19, 20),
            InfixOp::Multiply | InfixOp::Divide | InfixOp::Modulo => (21, 22),
            _ => panic!(),
        }
    }

    pub fn token_len(self) -> usize {
        match self {
            InfixOp::LeftShift => 2,
            InfixOp::RightShift => 2,
            InfixOp::TripleRightShift => 3,
            _ => 1,
        }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
    pub id: LocalDefId,
}

impl Stmt {
    pub fn new(kind: StmtKind, span: Span, id: LocalDefId) -> Self {
        Self { kind, span, id }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize, Deserialize)]
pub enum StmtKind {
    Let(LetStmt),
    For(ForStmt),
    If(IfStmt),
    Return(ReturnStmt),
    While(WhileStmt),
    Block(Block),
    Expression(Expression),
}

mod tests {}
