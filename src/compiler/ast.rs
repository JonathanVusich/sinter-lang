use string_interner::symbol::SymbolUsize;
use crate::compiler::types::types::Type;
use crate::traits::traits::Trait;

type Identifier = SymbolUsize;

pub struct Module<'ctx> {
    type_declarations: Vec<TypeDecl<'ctx>>,
    function_declarations: Vec<FunctionDecl<'ctx>>,
}

pub enum TypeDecl<'ctx> {
    Enum(EnumDecl),
    InlineClass(InlineClassDecl<'ctx>),
    Class(ClassDecl<'ctx>),
    Trait(TraitDecl)
}

pub struct InlineClassDecl<'ctx> {
    name: Identifier,
    generic_types: Vec<GenericTypeDecl>,
    members: Vec<MemberDecl<'ctx>>,
    member_functions: Vec<MemberFunctionDecl<'ctx>>,
}

pub struct ClassDecl<'ctx> {
    name: Identifier,
    generic_types: Vec<GenericTypeDecl>,
    members: Vec<MemberDecl<'ctx>>,
    member_functions: Vec<MemberFunctionDecl<'ctx>>,
}

pub struct EnumDecl {

}

pub struct TraitDecl {

}

pub struct FunctionDecl<'ctx> {
    ident: Identifier,
    generic_types: Vec<GenericTypeDecl>,
    parameters: Vec<ParameterDecl<'ctx>>,

}

pub struct GenericTypeDecl {
    ident: Identifier,
    trait_bound: Option<TraitBound>,
}

pub struct TraitBound {
    trait_refs: Vec<TraitRef>,
}

pub struct TraitRef {
    ident: Identifier,
}




pub struct MemberDecl<'ctx> {
    ident: Identifier,
    type_ref: Type<'ctx>,
}

pub struct MemberFunctionDecl<'ctx> {
    ident: Identifier,
    generic_types: Vec<GenericTypeDecl>,
    parameters: Vec<ParameterDecl<'ctx>>,
    return_type: Type<'ctx>,
}

pub struct ParameterDecl<'ctx> {
    ident: &'static str,
    type_ref: Type<'ctx>,
}
