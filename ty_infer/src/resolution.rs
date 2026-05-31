use std::cell::RefCell;
use std::collections::HashMap;

use bumpalo::Bump;

use ast::Ident;
use hir::{HirMap, Item, ItemKind, Node, Primitive};
use id::{CrateId, DefId};
use interner::Interner;
use typed_hir::{
    ClassDef, EnumDef, Field, Fields, FloatTy, FnDef, FnDefs, GenericDef, GenericParam,
    GenericParams, Generics, IntTy, MemberDef, MemberDefs, Param, Params, TraitBound, TraitDef, Ty,
    TyKind, UintTy,
};

#[derive(Debug)]
pub struct TyResolver<'hir> {
    tys: PrimitiveTypes<'hir>,
    ty_cache: TyDefCache<'hir>,
    hir_map: &'hir HirMap<'hir>,
    hir_allocator: &'hir Bump,
}

#[derive(Debug)]
struct PrimitiveTypes<'hir> {
    u8: Ty<'hir>,
    u16: Ty<'hir>,
    u32: Ty<'hir>,
    u64: Ty<'hir>,
    i8: Ty<'hir>,
    i16: Ty<'hir>,
    i32: Ty<'hir>,
    i64: Ty<'hir>,
    f32: Ty<'hir>,
    f64: Ty<'hir>,
    str: Ty<'hir>,
    none: Ty<'hir>,
    bool: Ty<'hir>,
}

impl<'hir> PrimitiveTypes<'hir> {
    pub fn convert(&self, primitive: Primitive) -> Ty<'hir> {
        match primitive {
            Primitive::U8 => self.u8,
            Primitive::U16 => self.u16,
            Primitive::U32 => self.u32,
            Primitive::U64 => self.u64,
            Primitive::I8 => self.i8,
            Primitive::I16 => self.i16,
            Primitive::I32 => self.i32,
            Primitive::I64 => self.i64,
            Primitive::F32 => self.f32,
            Primitive::F64 => self.f64,
            Primitive::Str => self.str,
            Primitive::Boolean => self.bool,
            Primitive::None => self.none,
        }
    }
}

/// This is needed because we need to have a way to construct types from their definition either
/// with or without associated generic types.
#[derive(Copy, Clone, Debug)]
pub enum Binder<'hir> {
    Array(&'hir Binder<'hir>),
    Class(&'hir ClassDef<'hir>),
    Enum(&'hir EnumDef<'hir>),
    EnumMember(&'hir MemberDef<'hir>),
    Trait(&'hir TraitDef<'hir>),
    GenericParam(&'hir GenericParam<'hir>),
    Fn(&'hir FnDef<'hir>),
    BareDef(Ty<'hir>),
}

// TODO: Currently allocating way more than necessary because we are allocating for types that may be interned.
// Need to look into this more and see if there is a more performant way to construct types only once.
impl<'hir> TyResolver<'hir> {
    pub fn new(hir_map: &'hir HirMap<'hir>, hir_allocator: &'hir Bump) -> Self {
        let ty_cache = TyDefCache::new(hir_allocator);
        let tys = PrimitiveTypes {
            u8: ty_cache.intern(TyKind::Uint(UintTy::U8)),
            u16: ty_cache.intern(TyKind::Uint(UintTy::U16)),
            u32: ty_cache.intern(TyKind::Uint(UintTy::U32)),
            u64: ty_cache.intern(TyKind::Uint(UintTy::U64)),
            i8: ty_cache.intern(TyKind::Int(IntTy::I8)),
            i16: ty_cache.intern(TyKind::Int(IntTy::I16)),
            i32: ty_cache.intern(TyKind::Int(IntTy::I32)),
            i64: ty_cache.intern(TyKind::Int(IntTy::I64)),
            f32: ty_cache.intern(TyKind::Float(FloatTy::F32)),
            f64: ty_cache.intern(TyKind::Float(FloatTy::F64)),
            str: ty_cache.intern(TyKind::Str),
            none: ty_cache.intern(TyKind::None),
            bool: ty_cache.intern(TyKind::Boolean),
        };
        Self {
            ty_cache,
            tys,
            hir_map,
            hir_allocator,
        }
    }

    pub fn intern(&self, ty_kind: TyKind<'hir>) -> Ty<'hir> {
        self.ty_cache.intern(ty_kind)
    }

    /// This function will generate a bindable type from a definition
    /// These types will be non-recursive meaning they can be built incrementally while traversing
    /// the HIR and inferring types without getting stuck in infinite loops.
    pub fn type_of(&self, def_id: DefId) -> Binder<'hir> {
        self.ty_cache
            .resolve_binder(def_id, || self.resolve_binder_from_id(def_id))
    }

    fn instantiate(&self, binder: &Binder<'hir>) -> Ty<'hir> {
        match binder {
            Binder::Array(inner) => self.intern(TyKind::Array(self.instantiate(inner))),
            Binder::Class(class_def) => self.intern(TyKind::Class(
                class_def,
                self.instantiate_generics(class_def),
            )),
            Binder::Enum(_) => {}
            Binder::EnumMember(_) => {}
            Binder::Trait(_) => {}
            Binder::GenericParam(_) => {}
            Binder::Fn(_) => {}
            Binder::BareDef(_) => {}
        }
    }

    pub fn resolve_generic_params(
        &self,
        generic_params: hir::GenericParams<'hir>,
        crate_id: CrateId,
    ) -> GenericParams<'hir> {
        self.hir_allocator.alloc_slice_fill_iter(
            generic_params
                .iter()
                .map(|generic| self.resolve_generic_param(*generic, crate_id)),
        )
    }

    pub fn resolve_generics(&self, generics: hir::Generics<'hir>) -> &'hir [Binder<'hir>] {
        self.hir_allocator
            .alloc_slice_fill_iter(generics.iter().map(|generic| self.type_of(*generic)))
    }

    fn resolve_binder_from_id(&self, id: DefId) -> Binder<'hir> {
        let krate = self.hir_map.krate(&id);
        let node = krate.node(&id.local_id());
        let crate_id = krate.id;
        match node {
            Node::Item(item) => self.resolve_binder_from_item(item, crate_id),
            Node::Ty(ty) => self.resolve_binder_from_ty(ty, crate_id),
            Node::GenericParam(param) => {
                let generic_param = self.resolve_generic_param(param, crate_id);
                Binder::GenericParam(&generic_param)
            }
            node => {
                dbg!(node);
                panic!("Unsupported node type!");
            }
        }
    }

    fn resolve_binder_from_ty(&self, ty: &'hir hir::Ty<'hir>, crate_id: CrateId) -> Binder<'hir> {
        match ty.kind {
            hir::TyKind::Array(array) => Binder::Array(&self.type_of(array)),
            hir::TyKind::Path(path) => self.type_of(path.definition),
            hir::TyKind::Primitive(primitive) => Binder::BareDef(self.tys.convert(primitive)),
            ty => {
                dbg!(ty);
                panic!("Unsupported ty type!")
            }
        }
    }

    fn resolve_binder_from_item(&self, item: &'hir Item<'hir>, crate_id: CrateId) -> Binder<'hir> {
        match item.kind {
            ItemKind::Class(class_def) => {
                let def = self.resolve_class_def(class_def, crate_id);
                Binder::Class(self.alloc(def))
            }
            ItemKind::Enum(enum_def) => {
                let def = self.resolve_enum_def(enum_def, crate_id);
                Binder::Enum(self.alloc(def))
            }
            ItemKind::Member(member_def) => {
                let def = self.resolve_enum_member(member_def, crate_id);
                Binder::EnumMember(self.alloc(def))
            }
            ItemKind::Fn(fn_def) => {
                let def = self.resolve_fn_def(fn_def, crate_id);
                Binder::Fn(self.alloc(def))
            }
            ItemKind::Trait(trait_def) => {
                let def = self.resolve_trait_def(trait_def, crate_id);
                Binder::Trait(self.alloc(def))
            }
            item => {
                dbg!(item);
                panic!("Unsupported item type!")
            }
        }
    }

    fn to_direct_binder<'a, F: 'a, B: FnOnce(F) -> TyKind<'hir>>(
        &self,
        ty_kind: F,
        builder: B,
    ) -> Binder<'hir> {
        let ty_kind = builder(ty_kind);
        Binder::BareDef(self.intern(ty_kind))
    }

    fn maybe_resolve_trait_bound(&self, trait_bound: &Option<hir::TraitBound>) -> Option<Ty<'hir>> {
        if let Some(trait_bound) = trait_bound {
            return Some(self.resolve_trait_bound(trait_bound));
        }
        None
    }

    fn resolve_trait_bound(&self, trait_bound: hir::TraitBound) -> Ty<'hir> {
        todo!()
    }

    fn resolve_class_def(
        &self,
        class_def: &hir::ClassDef<'hir>,
        crate_id: CrateId,
    ) -> ClassDef<'hir> {
        let generic_params = self.resolve_generic_params(class_def.generic_params, crate_id);
        let fields = self.resolve_fields(class_def.fields, crate_id);
        let fns = self.resolve_fn_defs(class_def.fn_defs, crate_id);
        ClassDef {
            name: class_def.name,
            generic_params,
            fields,
            fns,
        }
    }

    fn resolve_enum_def(&self, enum_def: &hir::EnumDef<'hir>, crate_id: CrateId) -> EnumDef<'hir> {
        let name = enum_def.name;
        let generic_params = self.resolve_generic_params(enum_def.generic_params, crate_id);
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
        let generic_params = self.resolve_generic_params(enum_member.generic_params, crate_id);
        let fields = self.resolve_fields(enum_member.fields, crate_id);
        let fn_defs = self.resolve_fn_defs(enum_member.fn_defs, crate_id);

        MemberDef {
            name,
            generic_params,
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
        let generic_params = self.resolve_generic_params(trait_def.generic_params, crate_id);
        let fn_defs = self.resolve_fn_defs(trait_def.fn_defs, crate_id);
        TraitDef {
            name,
            generic_params,
            fn_defs,
        }
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
        let generic_params = self.resolve_generic_params(fn_def.sig.generic_params, crate_id);
        let params = self.resolve_params(fn_def.sig.params, crate_id);
        let ret_ty = fn_def.sig.ret_ty;

        FnDef {
            name,
            ret_ty,
            generic_params,
            params,
        }
    }

    fn resolve_fields(&self, fields: hir::Fields<'hir>, crate_id: CrateId) -> Fields<'hir> {
        self.hir_allocator
            .alloc_slice_fill_iter(fields.iter().map(|field| {
                let name = field.name;
                let ty = field.ty;
                // Constructing the actual type is deferred
                Field { name, ty }
            }))
    }

    pub fn resolve_params(&self, params: hir::Params<'hir>, crate_id: CrateId) -> Params<'hir> {
        self.hir_allocator
            .alloc_slice_fill_iter(params.iter().map(|param| {
                let name = param.local_var.ident;
                let ty = param.ty;
                let span = param.local_var.span;
                let id = param.id.to_def_id(crate_id);

                Param {
                    name: Ident::new(name, span),
                    ty,
                    id,
                }
            }))
    }

    fn resolve_generic_param(
        &self,
        param: &hir::GenericParam<'hir>,
        crate_id: CrateId,
    ) -> GenericParam<'hir> {
        let trait_bound = self.maybe_resolve_trait_bound(&param.trait_bound);
        GenericParam {
            ident: param.ident,
            trait_bound,
            id: param.id.to_def_id(crate_id),
        }
    }

    fn alloc<T>(&self, val: T) -> &'hir T {
        self.hir_allocator.alloc(val)
    }
}
#[derive(Debug)]
struct TyDefCache<'a> {
    ty_allocator: &'a Bump,
    defs: RefCell<HashMap<DefId, Binder<'a>>>,
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

    fn resolve_binder<F: FnOnce() -> Binder<'a>>(&self, def_id: DefId, resolver: F) -> Binder<'a> {
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
