use std::cell::RefCell;
use std::collections::HashMap;

use bumpalo::Bump;

use ast::Ident;
use hir::{HirMap, Item, ItemKind, Node, Primitive};
use id::DefId;
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

    pub fn resolve_def(&self, def_id: DefId) -> &'hir DefType<'hir> {
        self.ty_cache
            .resolve_def(def_id, || self.resolve_path(def_id))
    }

    pub fn resolve_ty(&self, existing_ty: &hir::Ty<'hir>) -> Ty<'hir> {
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

    fn resolve_path_ty(&self, path: &hir::PathTy<'hir>) -> TyKind<'hir> {
        let trait_ty = self
            .ty_cache
            .resolve_def(path.definition, || self.resolve_path(path.definition));
        let generics = self
            .hir_allocator
            .alloc_slice_fill_iter(path.generics.iter().map(|generic| self.resolve_ty(generic)));
        match trait_ty {
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

    fn resolve_path(&self, id: DefId) -> &'hir DefType<'hir> {
        let node = self.hir_map.krate(&id).node(&id.local_id());
        let def_type = match node {
            Node::Item(Item {
                kind: ItemKind::Class(class_def),
                ..
            }) => DefType::Class(self.resolve_class_def(class_def)),
            Node::Item(Item {
                kind: ItemKind::Enum(enum_def),
                ..
            }) => DefType::Enum(self.resolve_enum_def(enum_def)),
            Node::Item(Item {
                kind: ItemKind::Member(member_def),
                ..
            }) => DefType::Member(self.resolve_enum_member(member_def)),
            Node::Item(Item {
                kind: ItemKind::Fn(fn_def),
                ..
            }) => DefType::Fn(self.resolve_fn_def(fn_def)),
            Node::Item(Item {
                kind: ItemKind::Trait(trait_def),
                ..
            }) => DefType::Trait(self.resolve_trait_def(trait_def)),
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

    fn resolve_class_def(&self, class_def: &hir::ClassDef<'hir>) -> &'hir ClassDef<'hir> {
        let generic_params = self.resolve_generic_params(class_def.generic_params);
        let fields = self.resolve_fields(class_def.fields);
        let fns = self.resolve_fn_defs(class_def.fn_defs);
        let class_def = self.alloc(ClassDef {
            name: class_def.name,
            class_type: class_def.class_type,
            generic_params,
            fields,
            fns,
        });
        class_def
    }

    fn resolve_enum_def(&self, enum_def: &hir::EnumDef<'hir>) -> &'hir EnumDef<'hir> {
        let name = enum_def.name;
        let generic_params = self.resolve_generic_params(enum_def.generic_params);
        let members = self.resolve_members(enum_def.members);
        let fn_defs = self.resolve_fn_defs(enum_def.fn_defs);
        let enum_def = self.alloc(EnumDef {
            name,
            generic_params,
            members,
            fn_defs,
        });
        enum_def
    }

    fn resolve_members(&self, members: hir::MemberDefs<'hir>) -> MemberDefs<'hir> {
        self.hir_allocator.alloc_slice_fill_iter(
            members
                .iter()
                .map(|member| self.resolve_enum_member(member)),
        )
    }

    fn resolve_enum_member(&self, enum_member: &hir::MemberDef<'hir>) -> &'hir MemberDef<'hir> {
        let name = enum_member.name;
        let fields = self.resolve_fields(enum_member.fields);
        let fn_defs = self.resolve_fn_defs(enum_member.fn_defs);

        let member_def = self.alloc(MemberDef {
            name,
            fields,
            fn_defs,
        });
        member_def
    }

    fn resolve_trait_def(&self, trait_def: &hir::TraitDef<'hir>) -> &'hir TraitDef<'hir> {
        let name = trait_def.name;
        let generic_params = self.resolve_generic_params(trait_def.generic_params);
        let fn_defs = self.resolve_fn_defs(trait_def.fn_defs);
        let trait_def = self.alloc(TraitDef {
            name,
            generic_params,
            fn_defs,
        });
        trait_def
    }

    fn resolve_fn_defs(&self, fn_defs: hir::FnDefs<'hir>) -> FnDefs<'hir> {
        self.hir_allocator
            .alloc_slice_fill_iter(fn_defs.iter().map(|fn_def| self.resolve_fn_def(fn_def)))
    }

    fn resolve_fn_def(&self, fn_def: &hir::FnDef<'hir>) -> &'hir FnDef<'hir> {
        let name = fn_def.sig.name;
        let generic_params = self.resolve_generic_params(fn_def.sig.generic_params);
        let params = self.resolve_params(fn_def.sig.params);
        let return_type = fn_def
            .sig
            .ret_ty
            .map(|ty| self.resolve_ty(ty))
            .unwrap_or_else(|| self.intern(TyKind::None));

        let fn_def = self.alloc(FnDef {
            name,
            generic_params,
            params,
            return_type,
        });
        fn_def
    }

    fn resolve_fields(&self, fields: hir::Fields<'hir>) -> Fields<'hir> {
        self.hir_allocator
            .alloc_slice_fill_iter(fields.iter().map(|field| {
                let ident = field.ident;
                let ty = self.resolve_ty(field.ty);
                Field { ident, ty }
            }))
    }

    pub fn resolve_params(&self, params: hir::Params<'hir>) -> Params<'hir> {
        self.hir_allocator
            .alloc_slice_fill_iter(params.iter().map(|param| {
                let ty = self.resolve_ty(param.ty);
                Param {
                    ident: Ident::new(param.local_var.ident, param.local_var.span),
                    ty,
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
