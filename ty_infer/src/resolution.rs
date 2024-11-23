use std::any::Any;
use std::cell::RefCell;
use std::collections::HashMap;

use bumpalo::Bump;

use ast::Ident;
use hir::{HirMap, Item, ItemKind, Node, Primitive};
use id::{CrateId, DefId};
use interner::Interner;
use typed_hir::{
    ClassDef, ClosureDef, EnumDef, Field, Fields, FloatTy, FnDef, FnDefs, GenericParam,
    GenericParams, Generics, IntTy, MemberDef, MemberDefs, Param, Params, Trait, TraitBound,
    TraitDef, Ty, TyKind, UintTy,
};

use crate::DefType;

#[derive(Debug)]
pub struct TyResolver<'hir> {
    ty_cache: TyDefCache<'hir>,
    hir_map: &'hir HirMap<'hir>,
    hir_allocator: &'hir Bump,
}

impl<'hir> TyResolver<'hir> {
    pub fn new(hir_map: &'hir HirMap<'hir>, hir_allocator: &'hir Bump) -> Self {
        let ty_cache = TyDefCache::new(hir_allocator);
        Self {
            ty_cache,
            hir_map,
            hir_allocator,
        }
    }

    pub fn intern(&self, ty_kind: TyKind<'hir>) -> Ty<'hir> {
        self.ty_cache.intern(ty_kind)
    }

    pub fn maybe_resolve_ty(&self, existing_ty: Option<&hir::Ty<'hir>>) -> Option<Ty<'hir>> {
        existing_ty.map(|ty| self.resolve_ty(ty))
    }

    /// This function will generate a type from a definition and the associated generics
    /// These types will be non-recursive meaning they can be built incrementally while traversing
    /// the HIR and inferring types without getting stuck in infinite loops.
    pub fn type_of(&self, def_id: DefId, generics: hir::Generics<'hir>) -> Ty<'hir> {
        let def_type = self.resolve_def(def_id);
        let generics = self.resolve_generics(generics);

        match def_type {
            DefType::Class(class_def) => {
                let ty_kind = TyKind::Class(class_def, generics);
                self.intern(ty_kind)
            }
            DefType::Enum(enum_def) => {
                let ty_kind = TyKind::Enum(enum_def, generics);
                self.intern(ty_kind)
            }
            DefType::Member(member_def) => {
                let ty_kind = TyKind::Member(member_def, generics);
                self.intern(ty_kind)
            }
            DefType::Trait(trait_def) => {
                let ty_kind = TyKind::Trait(trait_def, generics);
                self.intern(ty_kind)
            }
            DefType::Fn(fn_def) => {
                let ty_kind = TyKind::Fn(fn_def, generics);
                self.intern(ty_kind)
            }
            DefType::GenericParam(generic_param) => todo!(),
        }
    }

    pub fn resolve_def(&self, def_id: DefId) -> &'hir DefType<'hir> {
        self.ty_cache
            .resolve_def(def_id, || self.resolve_def_type(def_id))
    }

    pub fn resolve_ty(&self, existing_ty: &hir::Ty<'hir>) -> Ty<'hir> {
        dbg!(existing_ty);
        let ty_kind = match existing_ty.kind {
            hir::TyKind::Array(array_ty) => TyKind::Array(self.resolve_ty(array_ty)),
            hir::TyKind::Path(path) => self.resolve_path_ty(path),
            hir::TyKind::GenericParam(generic_param) => {
                let generic_param = self.resolve_generic_param(generic_param);
                TyKind::GenericParam(generic_param)
            }
            hir::TyKind::TraitBound(trait_bound) => {
                let trait_bound = self.resolve_trait_bound(trait_bound);
                TyKind::TraitBound(trait_bound)
            }
            hir::TyKind::Closure(closure) => {
                let params = self.hir_allocator.alloc_slice_fill_iter(
                    closure.params.iter().map(|param| self.resolve_ty(param)),
                );
                let return_type = self.resolve_ty(closure.ret_ty);
                let closure_def = self.alloc(ClosureDef {
                    params,
                    return_type,
                });
                TyKind::Closure(closure_def)
            }
            hir::TyKind::Primitive(primitive) => match primitive {
                Primitive::U8 => TyKind::Uint(UintTy::U8),
                Primitive::U16 => TyKind::Uint(UintTy::U16),
                Primitive::U32 => TyKind::Uint(UintTy::U32),
                Primitive::U64 => TyKind::Uint(UintTy::U64),
                Primitive::I8 => TyKind::Int(IntTy::I8),
                Primitive::I16 => TyKind::Int(IntTy::I16),
                Primitive::I32 => TyKind::Int(IntTy::I32),
                Primitive::I64 => TyKind::Int(IntTy::I64),
                Primitive::F32 => TyKind::Float(FloatTy::F32),
                Primitive::F64 => TyKind::Float(FloatTy::F64),
                Primitive::Str => TyKind::Str,
                Primitive::Boolean => TyKind::Boolean,
                Primitive::None => TyKind::None,
            },
        };
        self.intern(ty_kind)
    }

    pub fn resolve_generic_params(&self, params: hir::GenericParams<'hir>) -> GenericParams<'hir> {
        self.hir_allocator
            .alloc_slice_fill_iter(params.iter().map(|param| self.resolve_generic_param(param)))
    }

    pub fn resolve_generics(&self, params: hir::Generics<'hir>) -> Generics<'hir> {
        self.hir_allocator
            .alloc_slice_fill_iter(params.iter().map(|param| self.resolve_ty(param)))
    }

    fn resolve_path_ty(&self, path: &hir::PathTy<'hir>) -> TyKind<'hir> {
        let def_type = self
            .ty_cache
            .resolve_def(path.definition, || self.resolve_def_type(path.definition));
        let generics = self
            .hir_allocator
            .alloc_slice_fill_iter(path.generics.iter().map(|generic| self.resolve_ty(generic)));
        match def_type {
            DefType::Class(class_def) => TyKind::Class(class_def, generics),
            DefType::Enum(enum_def) => TyKind::Enum(enum_def, generics),
            DefType::Member(member_def) => TyKind::Member(member_def, generics),
            DefType::Trait(trait_def) => {
                let trait_bound = self.hir_allocator.alloc_slice_fill_iter([self.alloc(Trait {
                    trait_def,
                    generics,
                })]);
                TyKind::TraitBound(trait_bound)
            }
            DefType::Fn(fn_def) => TyKind::Fn(fn_def, generics),
            DefType::GenericParam(generic_param) => TyKind::GenericParam(generic_param),
        }
    }

    fn resolve_def_type(&self, id: DefId) -> &'hir DefType<'hir> {
        let krate = self.hir_map.krate(&id);
        let node = krate.node(&id.local_id());
        let crate_id = krate.id;
        let def_type = match node {
            Node::Item(Item {
                kind: ItemKind::Class(class_def),
                ..
            }) => DefType::Class(self.resolve_class_def(class_def, crate_id)),
            Node::Item(Item {
                kind: ItemKind::Enum(enum_def),
                ..
            }) => DefType::Enum(self.resolve_enum_def(enum_def, crate_id)),
            Node::Item(Item {
                kind: ItemKind::Member(member_def),
                ..
            }) => DefType::Member(self.resolve_enum_member(member_def, crate_id)),
            Node::Item(Item {
                kind: ItemKind::Fn(fn_def),
                ..
            }) => DefType::Fn(self.resolve_fn_def(fn_def, crate_id)),
            Node::Item(Item {
                kind: ItemKind::Trait(trait_def),
                ..
            }) => DefType::Trait(self.resolve_trait_def(trait_def, crate_id)),
            Node::Ty(hir::Ty {
                kind: hir::TyKind::GenericParam(param),
                ..
            }) => DefType::GenericParam(self.resolve_generic_param(param)),
            node => {
                dbg!(node);
                panic!("Unsupported node type!");
            }
        };
        self.alloc(def_type)
    }

    fn maybe_resolve_trait_bound(
        &self,
        trait_bound: &Option<hir::TraitBound>,
    ) -> Option<TraitBound<'hir>> {
        if let Some(trait_bound) = trait_bound {
            return Some(self.resolve_trait_bound(trait_bound));
        }
        None
    }

    fn resolve_trait_bound(&self, trait_bound: hir::TraitBound) -> TraitBound<'hir> {
        todo!()
    }

    fn resolve_class_def(
        &self,
        class_def: &hir::ClassDef<'hir>,
        crate_id: CrateId,
    ) -> ClassDef<'hir> {
        let fields = self.resolve_fields(class_def.fields, crate_id);
        let fns = self.resolve_fn_defs(class_def.fn_defs, crate_id);
        ClassDef {
            name: class_def.name,
            fields,
            fns,
        }
    }

    fn resolve_enum_def(&self, enum_def: &hir::EnumDef<'hir>, crate_id: CrateId) -> EnumDef<'hir> {
        let name = enum_def.name;
        let generic_params = self.resolve_generic_params(enum_def.generic_params);
        let members = self.resolve_members(enum_def.members, crate_id);
        let fn_defs = self.resolve_fn_defs(enum_def.fn_defs, crate_id);
        EnumDef {
            name,
            generic_params,
            members,
            fn_defs,
        }
    }

    fn resolve_members(
        &self,
        members: hir::MemberDefs<'hir>,
        crate_id: CrateId,
    ) -> MemberDefs<'hir> {
        self.hir_allocator.alloc_slice_fill_iter(
            members
                .iter()
                .map(|member| self.resolve_enum_member(member, crate_id)),
        )
    }

    fn resolve_enum_member(
        &self,
        enum_member: &hir::MemberDef<'hir>,
        crate_id: CrateId,
    ) -> MemberDef<'hir> {
        let name = enum_member.name;
        let fields = self.resolve_fields(enum_member.fields, crate_id);
        let fn_defs = self.resolve_fn_defs(enum_member.fn_defs, crate_id);

        MemberDef {
            name,
            fields,
            fn_defs,
        }
    }

    fn resolve_trait_def(
        &self,
        trait_def: &hir::TraitDef<'hir>,
        crate_id: CrateId,
    ) -> TraitDef<'hir> {
        let name = trait_def.name;
        let fn_defs = self.resolve_fn_defs(trait_def.fn_defs, crate_id);
        TraitDef { name, fn_defs }
    }

    fn resolve_fn_defs(&self, fn_defs: hir::FnDefs<'hir>, crate_id: CrateId) -> FnDefs<'hir> {
        self.hir_allocator.alloc_slice_fill_iter(
            fn_defs
                .iter()
                .map(|fn_def| self.resolve_fn_def(fn_def, crate_id)),
        )
    }

    fn resolve_fn_def(&self, fn_def: &hir::FnDef<'hir>, crate_id: CrateId) -> FnDef<'hir> {
        let name = fn_def.sig.name;
        let params = self.resolve_params(fn_def.sig.params, crate_id);
        let ret_ty = fn_def.sig.ret_ty.map(|ty| ty.id.to_def_id(crate_id));

        FnDef {
            name,
            ret_ty,
            params,
        }
    }

    fn resolve_fields(&self, fields: hir::Fields<'hir>, crate_id: CrateId) -> Fields<'hir> {
        self.hir_allocator
            .alloc_slice_fill_iter(fields.iter().map(|field| {
                let name = field.name;
                let id = field.id.to_def_id(crate_id);
                let ty = self.resolve_ty(field.ty);
                Field { name, ty }
            }))
    }

    pub fn resolve_params(&self, params: hir::Params<'hir>, crate_id: CrateId) -> Params<'hir> {
        self.hir_allocator
            .alloc_slice_fill_iter(params.iter().map(|param| {
                let name = param.local_var.ident;
                let span = param.local_var.span;
                let id = param.id.to_def_id(crate_id);
                let ty = self.resolve_ty(param.ty);

                Param {
                    name: Ident::new(name, span),
                    ty,
                    id,
                }
            }))
    }

    fn resolve_generic_param(&self, param: &hir::GenericParam<'hir>) -> &'hir GenericParam<'hir> {
        let trait_bound = self.maybe_resolve_trait_bound(&param.trait_bound);
        self.alloc(GenericParam {
            ident: param.ident,
            trait_bound,
        })
    }

    fn alloc<T>(&self, val: T) -> &'hir T {
        self.hir_allocator.alloc(val)
    }
}
#[derive(Debug)]
struct TyDefCache<'a> {
    ty_allocator: &'a Bump,
    defs: RefCell<HashMap<DefId, &'a DefType<'a>>>,
    interner: Interner<'a, TyKind<'a>>,
}

impl<'a> TyDefCache<'a> {
    pub fn new(ty_allocator: &'a Bump) -> Self {
        Self {
            ty_allocator,
            defs: Default::default(),
            interner: Default::default(),
        }
    }

    fn resolve_def<F: FnOnce() -> &'a DefType<'a>>(
        &self,
        def_id: DefId,
        resolver: F,
    ) -> &'a DefType<'a> {
        if let Some(occupied) = self.defs.borrow().get(&def_id) {
            return *occupied;
        }
        let resolved_value = resolver();
        self.defs.borrow_mut().insert(def_id, resolved_value);
        resolved_value
    }

    pub fn intern(&self, kind: TyKind<'a>) -> Ty<'a> {
        let kind: &'a TyKind<'a> = self
            .interner
            .intern(kind, |val| self.ty_allocator.alloc(val));
        Ty { kind }
    }
}
