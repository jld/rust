// Copyright 2013 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

/*!
 * # Representation of Algebraic Data Types
 *
 * This module determines how to represent enums, structs, and tuples
 * based on their monomorphized types; it is responsible both for
 * choosing a representation and translating basic operations on
 * values of those types.
 *
 * Note that the interface treats everything as a general case of an
 * enum, so structs/tuples/etc. have one pseudo-variant with
 * discriminant 0; i.e., as if they were a univariant enum.
 *
 * Having everything in one place will enable improvements to data
 * structure representation; possibilities include:
 *
 * - User-specified alignment (e.g., cacheline-aligning parts of
 *   concurrently accessed data structures); LLVM can't represent this
 *   directly, so we'd have to insert padding fields in any structure
 *   that might contain one and adjust GEP indices accordingly.  See
 *   issue #4578.
 *
 * - Rendering `Option<&T>` as a possibly-null `*T` instead of using
 *   an extra word (and likewise for `@T` and `~T`).  Can and probably
 *   should also apply to any enum with one empty case and one case
 *   starting with a non-null pointer (e.g., `Result<(), ~str>`).
 *
 * - Using smaller integer types for discriminants.
 *
 * - Store nested enums' discriminants in the same word.  Rather, if
 *   some variants start with enums, and those enums representations
 *   have unused alignment padding between discriminant and body, the
 *   outer enum's discriminant can be stored there and those variants
 *   can start at offset 0.  Kind of fancy, and might need work to
 *   make copies of the inner enum type cooperate, but it could help
 *   with `Option` or `Result` wrapped around another enum.
 *
 * - Tagged pointers would be neat, but given that any type can be
 *   used unboxed and any field can have pointers (including mutable)
 *   taken to it, implementing them for Rust seems difficult.
 */

use core::container::Map;
use core::libc::c_ulonglong;
use core::option::{Option, Some, None};
use core::{u8,u16,u32,u64,i8,i16,i32};
use core::vec;

use lib::llvm::{ValueRef, TypeRef, True, False};
use middle::trans::_match;
use middle::trans::build::*;
use middle::trans::common::*;
use middle::trans::machine;
use middle::trans::type_of;
use middle::ty;
use middle::ty::Disr;
use syntax::ast;
use util::ppaux::ty_to_str;


/// Representations.
pub enum Repr {
    /// C-like enums; basically an int.
    CEnum(IntType, Disr, Disr), // discriminant range
    /**
     * Single-case variants, and structs/tuples/records.
     *
     * Structs with destructors need a dynamic destroyedness flag to
     * avoid running the destructor too many times; this is included
     * in the `Struct` if present.
     */
    Univariant(Struct, bool),
    /**
     * General-case enums: for each case there is a struct, and they
     * all start with a field for the discriminant.
     */
    General(IntType, ~[Struct])
}

/// How to represent a discriminant
struct IntType {
    signed: bool,
    bits: IntSize
}
enum IntSize {
    I8, I16, I32, I64
}

impl IntType {
    fn lltype(self) -> TypeRef {
        match self.bits {
            I8 => T_i8(),
            I16 => T_i16(),
            I32 => T_i32(),
            I64 => T_i64()
        }
    }

    fn llconst(self, i: i64) -> ValueRef {
        C_integral(self.lltype(), i as u64, if self.signed { True } else { False })
    }

    fn const_to_int(self, c: ValueRef) -> i64 {
        if self.signed {
            const_to_int(c) as i64
        } else {
            const_to_uint(c) as i64
        }
    }

    fn ty(self, tcx: ty::ctxt) -> ty::t {
        if self.signed {
            match self.bits {
                I8 => ty::mk_i8(tcx),
                I16 => ty::mk_i16(tcx),
                I32 => ty::mk_i32(tcx),
                I64 => ty::mk_i64(tcx)
            }
        } else {
            match self.bits {
                I8 => ty::mk_u8(tcx),
                I16 => ty::mk_u16(tcx),
                I32 => ty::mk_u32(tcx),
                I64 => ty::mk_u64(tcx)
            }
        }
    }

    fn mask(self) -> u64 {
        match self.bits {
            I8 => u8::max_value as u64,
            I16 => u16::max_value as u64,
            I32 => u32::max_value as u64,
            I64 => u64::max_value
        }
    }
}



/// For structs, and struct-like parts of anything fancier.
struct Struct {
    size: u64,
    align: u64,
    fields: ~[ty::t]
}

/**
 * Convenience for `represent_type`.  There should probably be more or
 * these, for places in trans where the `ty::t` isn't directly
 * available.
 */
pub fn represent_node(bcx: block, node: ast::node_id) -> @Repr {
    represent_type(bcx.ccx(), node_id_type(bcx, node))
}

/// Decides how to represent a given type.
pub fn represent_type(cx: @CrateContext, t: ty::t) -> @Repr {
    debug!("Representing: %s", ty_to_str(cx.tcx, t));
    match cx.adt_reprs.find(&t) {
        Some(repr) => return *repr,
        None => { }
    }
    let repr = @match ty::get(t).sty {
        ty::ty_tup(ref elems) => {
            Univariant(mk_struct(cx, *elems), false)
        }
        ty::ty_struct(def_id, ref substs) => {
            let fields = ty::lookup_struct_fields(cx.tcx, def_id);
            let ftys = do fields.map |field| {
                ty::lookup_field_type(cx.tcx, def_id, field.id, substs)
            };
            let dtor = ty::ty_dtor(cx.tcx, def_id).is_present();
            let ftys =
                if dtor { ftys + [ty::mk_bool(cx.tcx)] } else { ftys };
            Univariant(mk_struct(cx, ftys), dtor)
        }
        ty::ty_enum(def_id, ref substs) => {
            struct Case { discr: Disr, tys: ~[ty::t] };

            let cases = do ty::enum_variants(cx.tcx, def_id).map |vi| {
                let arg_tys = do vi.args.map |&raw_ty| {
                    ty::subst(cx.tcx, substs, raw_ty)
                };
                Case { discr: vi.disr_val, tys: arg_tys }
            };
            if cases.len() == 0 {
                // Uninhabitable; represent as unit
                Univariant(mk_struct(cx, ~[]), false)
            } else if cases.all(|c| c.tys.len() == 0) {
                // All bodies empty -> intlike
                let discrs = cases.map(|c| c.discr);
                mk_cenum(discrs.min(), discrs.max())
            } else if cases.len() == 1 {
                // Equivalent to a struct/tuple/newtype.
                fail_unless!(cases[0].discr == 0);
                Univariant(mk_struct(cx, cases[0].tys), false)
            } else {
                // The general case.  Since there's at least one
                // non-empty body, explicit discriminants should have
                // been rejected by a checker before this point.
                if !cases.alli(|i,c| c.discr == (i as Disr)) {
                    cx.sess.bug(fmt!("non-C-like enum %s with specified \
                                      discriminants",
                                     ty::item_path_str(cx.tcx, def_id)))
                }
                let discr_it = choose_uint(cases.len() as u64);
                let discr = ~[discr_it.ty(cx.tcx)];
                General(discr_it, cases.map(|c| mk_struct(cx, discr + c.tys)))
            }
        }
        _ => cx.sess.bug(~"adt::represent_type called on non-ADT type")
    };
    cx.adt_reprs.insert(t, repr);
    return repr;
}

fn mk_cenum(min: i64, max: i64) -> Repr {
    CEnum(choose_int(min, max), min, max)
}

fn mk_struct(cx: @CrateContext, tys: &[ty::t]) -> Struct {
    let lltys = tys.map(|&ty| type_of::sizing_type_of(cx, ty));
    let llty_rec = T_struct(lltys);
    Struct {
        size: machine::llsize_of_alloc(cx, llty_rec) /*bad*/as u64,
        align: machine::llalign_of_min(cx, llty_rec) /*bad*/as u64,
        fields: vec::from_slice(tys)
    }
}

fn choose_uint(max: u64) -> IntType {
    IntType {
        signed: false,
        bits: if max <= u8::max_value as u64 { I8 }
        else if max <= u16::max_value as u64 { I16 }
        else if max <= u32::max_value as u64 { I32 }
        else { I64 }
    }
}

fn choose_int(min: i64, max: i64) -> IntType {
    if min >= 0 {
        choose_uint(max as u64)
    } else {
        IntType {
            signed: true,
            bits: if min >= i8::min_value as i64 && max <= i8::max_value as i64 { I8 }
            else if min >= i16::min_value as i64 && max <= i16::max_value as i64 { I16 }
            else if min >= i32::min_value as i64 && max <= i32::max_value as i64 { I32 }
            else { I64 }
        }
    }
}


/**
 * Returns the fields of a struct for the given representation.
 * All nominal types are LLVM structs, in order to be able to use
 * forward-declared opaque types to prevent circularity in `type_of`.
 */
pub fn fields_of(cx: @CrateContext, r: &Repr) -> ~[TypeRef] {
    generic_fields_of(cx, r, false)
}
/// Like `fields_of`, but for `type_of::sizing_type_of` (q.v.).
pub fn sizing_fields_of(cx: @CrateContext, r: &Repr) -> ~[TypeRef] {
    generic_fields_of(cx, r, true)
}
fn generic_fields_of(cx: @CrateContext, r: &Repr, sizing: bool)
    -> ~[TypeRef] {
    match *r {
        CEnum(it, _, _) => ~[it.lltype()],
        Univariant(ref st, _dtor) => struct_llfields(cx, st, sizing),
        General(_it, ref sts) => {
            // To get "the" type of a general enum, we pick the case
            // with the largest alignment (so it will always align
            // correctly in containing structures) and pad it out.
            fail_unless!(sts.len() >= 1);
            let mut most_aligned = None;
            let mut largest_align = 0;
            let mut largest_size = 0;
            for sts.each |st| {
                if largest_size < st.size {
                    largest_size = st.size;
                }
                if largest_align < st.align {
                    // Clang breaks ties by size; it is unclear if
                    // that accomplishes anything important.
                    largest_align = st.align;
                    most_aligned = Some(st);
                }
            }
            let most_aligned = most_aligned.get();
            let padding = largest_size - most_aligned.size;

            struct_llfields(cx, most_aligned, sizing)
                + [T_array(T_i8(), padding /*bad*/as uint)]
        }
    }
}

fn struct_llfields(cx: @CrateContext, st: &Struct, sizing: bool)
    -> ~[TypeRef] {
    if sizing {
        st.fields.map(|&ty| type_of::sizing_type_of(cx, ty))
    } else {
        st.fields.map(|&ty| type_of::type_of(cx, ty))
    }
}

/**
 * Obtain a representation of the discriminant sufficient to translate
 * destructuring; this may or may not involve the actual discriminant.
 *
 * This should ideally be less tightly tied to `_match`.
 */
pub fn trans_switch(bcx: block, r: &Repr, scrutinee: ValueRef)
    -> (_match::branch_kind, Option<ValueRef>) {
    match *r {
        CEnum(*) | General(*) => {
            (_match::switch, Some(trans_get_discr(bcx, r, scrutinee)))
        }
        Univariant(*) => {
            (_match::single, None)
        }
    }
}

/// Obtain the actual discriminant of a value.
pub fn trans_get_discr(bcx: block, r: &Repr, scrutinee: ValueRef)
    -> ValueRef {
    match *r {
        CEnum(it, min, max) => load_discr(bcx, scrutinee, it, min, max),
        Univariant(*) => C_u8(0),
        General(it, ref cases) => load_discr(bcx, scrutinee, it, 0,
                                             (cases.len() - 1) as Disr)
    }
}

/// Helper for cases where the discriminant is simply loaded.
fn load_discr(bcx: block, scrutinee: ValueRef, it: IntType, min: Disr, max: Disr)
    -> ValueRef {
    let ptr = GEPi(bcx, scrutinee, [0, 0]);
    let mask = it.mask() as Disr;
    if ((max + 1) & mask) == (min & mask) {
        // i.e., if the range is everything.  The lo==hi case would be
        // rejected by the LLVM verifier (it would mean either an
        // empty set, which is impossible, or the entire range of the
        // type, which is pointless).
        Load(bcx, ptr)
    } else {
        // llvm::ConstantRange can deal with ranges that wrap around,
        // so an overflow on (max + 1) is fine.
        debug!("LRA: %? %? %?", min, max, it);
        LoadRangeAssert(bcx, ptr, min as c_ulonglong,
                        (max + 1) as c_ulonglong,
                        if it.signed { True } else { False })
    }
}

/**
 * Yield information about how to dispatch a case of the
 * discriminant-like value returned by `trans_switch`.
 *
 * This should ideally be less tightly tied to `_match`.
 */
pub fn trans_case(bcx: block, r: &Repr, discr: Disr) -> _match::opt_result {
    match *r {
        CEnum(it, _, _) => {
            _match::single_result(rslt(bcx, it.llconst(discr)))
        }
        Univariant(*)=> {
            bcx.ccx().sess.bug(~"no cases for univariants or structs")
        }
        General(it, _) => {
            _match::single_result(rslt(bcx, it.llconst(discr)))
        }
    }
}

/**
 * Begin initializing a new value of the given case of the given
 * representation.  The fields, if any, should then be initialized via
 * `trans_field_ptr`.
 */
pub fn trans_start_init(bcx: block, r: &Repr, val: ValueRef, discr: Disr) {
    match *r {
        CEnum(it, min, max) => {
            fail_unless!(min <= discr && discr <= max);
            Store(bcx, it.llconst(discr), GEPi(bcx, val, [0, 0]))
        }
        Univariant(ref st, true) => {
            fail_unless!(discr == 0);
            Store(bcx, C_bool(true),
                  GEPi(bcx, val, [0, st.fields.len() - 1]))
        }
        Univariant(*) => {
            fail_unless!(discr == 0);
        }
        General(it, _) => {
            Store(bcx, it.llconst(discr), GEPi(bcx, val, [0, 0]))
        }
    }
}

/**
 * The number of fields in a given case; for use when obtaining this
 * information from the type or definition is less convenient.
 */
pub fn num_args(r: &Repr, discr: Disr) -> uint {
    match *r {
        CEnum(*) => 0,
        Univariant(ref st, dtor) => {
            fail_unless!(discr == 0);
            st.fields.len() - (if dtor { 1 } else { 0 })
        }
        General(_it, ref cases) => cases[discr as uint].fields.len() - 1
    }
}

/// Access a field, at a point when the value's case is known.
pub fn trans_field_ptr(bcx: block, r: &Repr, val: ValueRef, discr: Disr,
                       ix: uint) -> ValueRef {
    // Note: if this ever needs to generate conditionals (e.g., if we
    // decide to do some kind of cdr-coding-like non-unique repr
    // someday), it will need to return a possibly-new bcx as well.
    match *r {
        CEnum(*) => {
            bcx.ccx().sess.bug(~"element access in C-like enum")
        }
        Univariant(ref st, _dtor) => {
            fail_unless!(discr == 0);
            struct_field_ptr(bcx, st, val, ix, false)
        }
        General(_it, ref cases) => {
            struct_field_ptr(bcx, &cases[discr as uint], val, ix + 1, true)
        }
    }
}

fn struct_field_ptr(bcx: block, st: &Struct, val: ValueRef, ix: uint,
              needs_cast: bool) -> ValueRef {
    let ccx = bcx.ccx();

    let val = if needs_cast {
        let real_llty = T_struct(st.fields.map(
            |&ty| type_of::type_of(ccx, ty)));
        PointerCast(bcx, val, T_ptr(real_llty))
    } else {
        val
    };

    GEPi(bcx, val, [0, ix])
}

/// Access the struct drop flag, if present.
pub fn trans_drop_flag_ptr(bcx: block, r: &Repr, val: ValueRef) -> ValueRef {
    match *r {
        Univariant(ref st, true) => GEPi(bcx, val, [0, st.fields.len() - 1]),
        _ => bcx.ccx().sess.bug(~"tried to get drop flag of non-droppable \
                                  type")
    }
}

/**
 * Construct a constant value, suitable for initializing a
 * GlobalVariable, given a case and constant values for its fields.
 * Note that this may have a different LLVM type (and different
 * alignment!) from the representation's `type_of`, so it needs a
 * pointer cast before use.
 *
 * The LLVM type system does not directly support unions, and only
 * pointers can be bitcast, so a constant (and, by extension, the
 * GlobalVariable initialized by it) will have a type that can vary
 * depending on which case of an enum it is.
 *
 * To understand the alignment situation, consider `enum E { V64(u64),
 * V32(u32, u32) }` on win32.  The type has 8-byte alignment to
 * accommodate the u64, but `V32(x, y)` would have LLVM type `{i32,
 * i32, i32}`, which is 4-byte aligned.
 *
 * Currently the returned value has the same size as the type, but
 * this could be changed in the future to avoid allocating unnecessary
 * space after values of shorter-than-maximum cases.
 */
pub fn trans_const(ccx: @CrateContext, r: &Repr, discr: Disr,
                   vals: &[ValueRef]) -> ValueRef {
    match *r {
        CEnum(it, min, max) => {
            fail_unless!(vals.len() == 0);
            fail_unless!(min <= discr && discr <= max);
            it.llconst(discr)
        }
        Univariant(ref st, _dro) => {
            fail_unless!(discr == 0);
            C_struct(build_const_struct(ccx, st, vals))
        }
        General(it, ref cases) => {
            let case = &cases[discr as uint];
            let max_sz = cases.map(|s| s.size).max();
            let contents = build_const_struct(ccx, case,
                                              ~[it.llconst(discr)] + vals);
            C_struct(contents + [padding(max_sz - case.size)])
        }
    }
}

/**
 * Building structs is a little complicated, because we might need to
 * insert padding if a field's value is less aligned than its type.
 *
 * Continuing the example from `trans_const`, a value of type `(u32,
 * E)` should have the `E` at offset 8, but if that field's
 * initializer is 4-byte aligned then simply translating the tuple as
 * a two-element struct will locate it at offset 4, and accesses to it
 * will read the wrong memory.
 */
fn build_const_struct(ccx: @CrateContext, st: &Struct, vals: &[ValueRef])
    -> ~[ValueRef] {
    fail_unless!(vals.len() == st.fields.len());

    let mut offset = 0;
    let mut cfields = ~[];
    for st.fields.eachi |i, &ty| {
        let llty = type_of::sizing_type_of(ccx, ty);
        let type_align = machine::llalign_of_min(ccx, llty)
            /*bad*/as u64;
        let val_align = machine::llalign_of_min(ccx, val_ty(vals[i]))
            /*bad*/as u64;
        let target_offset = roundup(offset, type_align);
        offset = roundup(offset, val_align);
        if (offset != target_offset) {
            cfields.push(padding(target_offset - offset));
            offset = target_offset;
        }
        fail_unless!(!is_undef(vals[i]));
        // If that assert fails, could change it to wrap in a struct?
        // (See `const_struct_field` for why real fields must not be undef.)
        cfields.push(vals[i]);
    }

    return cfields;
}

fn padding(size: u64) -> ValueRef {
    C_undef(T_array(T_i8(), size /*bad*/as uint))
}

// XXX this utility routine should be somewhere more general
#[always_inline]
fn roundup(x: u64, a: u64) -> u64 { ((x + (a - 1)) / a) * a }

/// Get the discriminant of a constant value.  (Not currently used.)
pub fn const_get_discrim(ccx: @CrateContext, r: &Repr, val: ValueRef)
    -> Disr {
    match *r {
        CEnum(it, _, _) => it.const_to_int(val),
        Univariant(*) => 0,
        General(it, _) => it.const_to_int(const_get_elt(ccx, val, [0])),
    }
}

/**
 * Extract a field of a constant value, as appropriate for its
 * representation.
 *
 * (Not to be confused with `common::const_get_elt`, which operates on
 * raw LLVM-level structs and arrays.)
 */
pub fn const_get_field(ccx: @CrateContext, r: &Repr, val: ValueRef,
                       _discr: Disr, ix: uint) -> ValueRef {
    match *r {
        CEnum(*) => ccx.sess.bug(~"element access in C-like enum const"),
        Univariant(*) => const_struct_field(ccx, val, ix),
        General(*) => const_struct_field(ccx, val, ix + 1)
    }
}

/// Extract field of struct-like const, skipping our alignment padding.
fn const_struct_field(ccx: @CrateContext, val: ValueRef, ix: uint)
    -> ValueRef {
    // Get the ix-th non-undef element of the struct.
    let mut real_ix = 0; // actual position in the struct
    let mut ix = ix; // logical index relative to real_ix
    let mut field;
    loop {
        loop {
            field = const_get_elt(ccx, val, [real_ix]);
            if !is_undef(field) {
                break;
            }
            real_ix = real_ix + 1;
        }
        if ix == 0 {
            return field;
        }
        ix = ix - 1;
        real_ix = real_ix + 1;
    }
}

/// Is it safe to bitcast a value to the one field of its one variant?
pub fn is_newtypeish(r: &Repr) -> bool {
    match *r {
        Univariant(ref st, false) => st.fields.len() == 1,
        _ => false
    }
}
