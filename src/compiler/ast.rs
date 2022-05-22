use string_interner::symbol::SymbolUsize;
use crate::compiler::types::types::{Identifier, Type};
use crate::traits::traits::Trait;

pub struct Module {
    type_declarations: Vec<TypeDecl>,
    function_declarations: Vec<FunctionDecl>,
}

pub enum TypeDecl {
    Enum(EnumDecl),
    InlineClass(InlineClassDecl),
    Class(ClassDecl),
    Trait(TraitDecl)
}

pub struct InlineClassDecl {
    name: Identifier,
    generic_types: Vec<GenericTypeDecl>,
    members: Vec<MemberDecl>,
    member_functions: Vec<MemberFunctionDecl>,
}

pub struct ClassDecl {
    name: Identifier,
    generic_types: Vec<GenericTypeDecl>,
    members: Vec<MemberDecl>,
    member_functions: Vec<MemberFunctionDecl>,
}

pub struct EnumDecl {
    ident: Identifier,
    members: Vec<EnumMemberDecl>,
}

pub struct TraitDecl {
    ident: Identifier,
    functions: Vec<MemberFunctionDecl>,
}

pub struct FunctionDecl {
    ident: Identifier,
    generic_types: Vec<GenericTypeDecl>,
    parameters: Vec<ParameterDecl>,
    return_type: Type,
}

pub struct GenericTypeDecl {
    ident: Identifier,
    type_ref: Type,
}

pub struct MemberDecl {
    ident: Identifier,
    type_ref: Type,
}

pub struct EnumMemberDecl {
    ident: Identifier,
    parameters: Vec<ParameterDecl>,
    member_functions: Vec<MemberFunctionDecl>,
}

pub struct MemberFunctionDecl {
    ident: Identifier,
    generic_types: Vec<GenericTypeDecl>,
    parameters: Vec<ParameterDecl>,
    return_type: Type,
}

pub struct ParameterDecl {
    ident: Identifier,
    ty: Type,
}
