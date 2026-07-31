package main

import (
    _goml_fmt "fmt"
    _goml_slices "slices"
    _goml_utf8 "unicode/utf8"
)

func _goml_runtime_core_char_to_string(x rune) string {
    if !_goml_utf8.ValidRune(x) {
        panic("invalid char")
    }
    return string(x)
}

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type _goml_vec_EnvEntry struct {
    items []EnvEntry
}

func vec_new__Vec_8EnvEntry() *_goml_vec_EnvEntry {
    return &_goml_vec_EnvEntry{
        items: nil,
    }
}

func vec_with_capacity__Vec_8EnvEntry(capacity int) *_goml_vec_EnvEntry {
    return &_goml_vec_EnvEntry{
        items: _goml_slices.Grow([]EnvEntry{}, int(capacity)),
    }
}

func vec_push__Vec_8EnvEntry(vec *_goml_vec_EnvEntry, elem EnvEntry) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_8EnvEntry(vec *_goml_vec_EnvEntry, index int) EnvEntry {
    return vec.items[index]
}

func vec_len__Vec_8EnvEntry(vec *_goml_vec_EnvEntry) int {
    return int(len(vec.items))
}

type _goml_vec_SubstEntry struct {
    items []SubstEntry
}

func vec_new__Vec_10SubstEntry() *_goml_vec_SubstEntry {
    return &_goml_vec_SubstEntry{
        items: nil,
    }
}

func vec_with_capacity__Vec_10SubstEntry(capacity int) *_goml_vec_SubstEntry {
    return &_goml_vec_SubstEntry{
        items: _goml_slices.Grow([]SubstEntry{}, int(capacity)),
    }
}

func vec_push__Vec_10SubstEntry(vec *_goml_vec_SubstEntry, elem SubstEntry) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_10SubstEntry(vec *_goml_vec_SubstEntry, index int) SubstEntry {
    return vec.items[index]
}

func vec_len__Vec_10SubstEntry(vec *_goml_vec_SubstEntry) int {
    return int(len(vec.items))
}

type ref_int32_x struct {
    value int32
}

func ref__Ref_5int32(value int32) *ref_int32_x {
    return &ref_int32_x{
        value: value,
    }
}

func ref_get__Ref_5int32(reference *ref_int32_x) int32 {
    return reference.value
}

func ref_set__Ref_5int32(reference *ref_int32_x, value int32) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_Tv_x struct {
    value Tv
}

func ref__Ref_2Tv(value Tv) *ref_Tv_x {
    return &ref_Tv_x{
        value: value,
    }
}

func ref_get__Ref_2Tv(reference *ref_Tv_x) Tv {
    return reference.value
}

func ref_set__Ref_2Tv(reference *ref_Tv_x, value Tv) struct{} {
    reference.value = value
    return struct{}{}
}

func ptr_eq__Ref_2Tv(a *ref_Tv_x, b *ref_Tv_x) bool {
    return a == b
}

type ref_int_x struct {
    value int
}

func ref__Ref_3int(value int) *ref_int_x {
    return &ref_int_x{
        value: value,
    }
}

func ref_get__Ref_3int(reference *ref_int_x) int {
    return reference.value
}

func ref_set__Ref_3int(reference *ref_int_x, value int) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_Option__Typ_x struct {
    value Option__Typ
}

func ref__Ref_11Option__Typ(value Option__Typ) *ref_Option__Typ_x {
    return &ref_Option__Typ_x{
        value: value,
    }
}

func ref_get__Ref_11Option__Typ(reference *ref_Option__Typ_x) Option__Typ {
    return reference.value
}

func ref_set__Ref_11Option__Typ(reference *ref_Option__Typ_x, value Option__Typ) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_bool_x struct {
    value bool
}

func ref__Ref_4bool(value bool) *ref_bool_x {
    return &ref_bool_x{
        value: value,
    }
}

func ref_get__Ref_4bool(reference *ref_bool_x) bool {
    return reference.value
}

func ref_set__Ref_4bool(reference *ref_bool_x, value bool) struct{} {
    reference.value = value
    return struct{}{}
}

type Tuple2_3Typ_3Typ struct {
    _0 Typ
    _1 Typ
}

type Tuple2_3Typ_16Vec_10SubstEntry struct {
    _0 Typ
    _1 *_goml_vec_SubstEntry
}

type CheckerState struct {
    gensym_counter *ref_int32_x
    current_level *ref_int32_x
}

type EnvEntry struct {
    name string
    ty Typ
}

type SubstEntry struct {
    name string
    ty Typ
}

type Exp interface {
    isExp()
}

type Var struct {
    _0 string
}

func (_ Var) isExp() {}

type App struct {
    _0 Exp
    _1 Exp
}

func (_ App) isExp() {}

type Lam struct {
    _0 string
    _1 Exp
}

func (_ Lam) isExp() {}

type Let struct {
    _0 string
    _1 Exp
    _2 Exp
}

func (_ Let) isExp() {}

type Typ interface {
    isTyp()
}

type TVar struct {
    _0 *ref_Tv_x
}

func (_ TVar) isTyp() {}

type QVar struct {
    _0 string
}

func (_ QVar) isTyp() {}

type TArrow struct {
    _0 Typ
    _1 Typ
}

func (_ TArrow) isTyp() {}

type Tv interface {
    isTv()
}

type Unbound struct {
    _0 string
    _1 int32
}

func (_ Unbound) isTv() {}

type Link struct {
    _0 Typ
}

func (_ Link) isTv() {}

type Option__Typ interface {
    isOption__Typ()
}

type None struct {}

func (_ None) isOption__Typ() {}

type Some struct {
    _0 Typ
}

func (_ Some) isOption__Typ() {}

type Result__unit__string interface {
    isResult__unit__string()
}

type Result__unit__string_Ok struct {
    _0 struct{}
}

func (_ Result__unit__string_Ok) isResult__unit__string() {}

type Result__unit__string_Err struct {
    _0 string
}

func (_ Result__unit__string_Err) isResult__unit__string() {}

type Result__Typ__string interface {
    isResult__Typ__string()
}

type Result__Typ__string_Ok struct {
    _0 Typ
}

func (_ Result__Typ__string_Ok) isResult__Typ__string() {}

type Result__Typ__string_Err struct {
    _0 string
}

func (_ Result__Typ__string_Err) isResult__Typ__string() {}

func state_new() CheckerState {
    var retv349 CheckerState
    var t350 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var t351 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(1)
    var t352 CheckerState = CheckerState{
        gensym_counter: t350,
        current_level: t351,
    }
    retv349 = t352
    return retv349
}

func reset_gensym(st__0 CheckerState) struct{} {
    var t354 *ref_int32_x = st__0.gensym_counter
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t354, 0)
    return struct{}{}
}

func reset_level(st__1 CheckerState) struct{} {
    var t356 *ref_int32_x = st__1.current_level
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t356, 1)
    return struct{}{}
}

func reset_type_variables(st__2 CheckerState) struct{} {
    reset_gensym(st__2)
    reset_level(st__2)
    return struct{}{}
}

func enter_level(st__3 CheckerState) struct{} {
    var t360 *ref_int32_x = st__3.current_level
    var l__4 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t360)
    var t361 *ref_int32_x = st__3.current_level
    var t362 int32 = l__4 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t361, t362)
    return struct{}{}
}

func leave_level(st__5 CheckerState) struct{} {
    var t364 *ref_int32_x = st__5.current_level
    var l__6 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t364)
    var t365 *ref_int32_x = st__5.current_level
    var t366 int32 = l__6 - 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t365, t366)
    return struct{}{}
}

func min_i32(a__7 int32, b__8 int32) int32 {
    var retv368 int32
    var t371 bool = a__7 < b__8
    var jp370 int32
    if t371 {
        jp370 = a__7
    } else {
        jp370 = b__8
    }
    retv368 = jp370
    return retv368
}

func nth_letter(n__9 int32) rune {
    var retv373 rune
    var jp375 rune
    switch n__9 {
    case 0:
        jp375 = 97
    case 1:
        jp375 = 98
    case 2:
        jp375 = 99
    case 3:
        jp375 = 100
    case 4:
        jp375 = 101
    case 5:
        jp375 = 102
    case 6:
        jp375 = 103
    case 7:
        jp375 = 104
    case 8:
        jp375 = 105
    case 9:
        jp375 = 106
    case 10:
        jp375 = 107
    case 11:
        jp375 = 108
    case 12:
        jp375 = 109
    case 13:
        jp375 = 110
    case 14:
        jp375 = 111
    case 15:
        jp375 = 112
    case 16:
        jp375 = 113
    case 17:
        jp375 = 114
    case 18:
        jp375 = 115
    case 19:
        jp375 = 116
    case 20:
        jp375 = 117
    case 21:
        jp375 = 118
    case 22:
        jp375 = 119
    case 23:
        jp375 = 120
    case 24:
        jp375 = 121
    case 25:
        jp375 = 122
    default:
        jp375 = 97
    }
    retv373 = jp375
    return retv373
}

func gensym(st__10 CheckerState) string {
    var retv377 string
    var t378 *ref_int32_x = st__10.gensym_counter
    var n__11 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t378)
    var t379 *ref_int32_x = st__10.gensym_counter
    var t380 int32 = n__11 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t379, t380)
    var t383 bool = n__11 < 26
    var jp382 string
    if t383 {
        var t384 rune = nth_letter(n__11)
        var t385 string = _goml_m_inherent_i_char_i_char_i_to__string(t384)
        jp382 = t385
    } else {
        var t386 string = _goml_m_inherent_i_int32_i_int32_i_to__string(n__11)
        var t387 string = "t" + t386
        jp382 = t387
    }
    retv377 = jp382
    return retv377
}

func newvar(st__12 CheckerState) Typ {
    var retv389 Typ
    var name__13 string = gensym(st__12)
    var t390 *ref_int32_x = st__12.current_level
    var level__14 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t390)
    var t391 Tv = Unbound{
        _0: name__13,
        _1: level__14,
    }
    var t392 *ref_Tv_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Tv(t391)
    var t393 Typ = TVar{
        _0: t392,
    }
    retv389 = t393
    return retv389
}

func typ_is_arrow(ty__15 Typ) bool {
    var retv395 bool
    var jp397 bool
    switch ty__15.(type) {
    case TVar:
        var x158 *ref_Tv_x = ty__15.(TVar)._0
        var tvref__16 *ref_Tv_x = x158
        var mtmp162 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvref__16)
        var jp399 bool
        switch mtmp162.(type) {
        case Link:
            var x165 Typ = mtmp162.(Link)._0
            var inner__17 Typ = x165
            var t400 bool = typ_is_arrow(inner__17)
            jp399 = t400
        default:
            jp399 = false
        }
        jp397 = jp399
    case TArrow:
        jp397 = true
    default:
        jp397 = false
    }
    retv395 = jp397
    return retv395
}

func typ_to_string(ty__18 Typ) string {
    var retv402 string
    var jp404 string
    switch ty__18.(type) {
    case TVar:
        var x166 *ref_Tv_x = ty__18.(TVar)._0
        var tvref__20 *ref_Tv_x = x166
        var mtmp170 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvref__20)
        var jp406 string
        switch mtmp170.(type) {
        case Unbound:
            var x171 string = mtmp170.(Unbound)._0
            var name__21 string = x171
            var t407 string = "'" + name__21
            jp406 = t407
        case Link:
            var x173 Typ = mtmp170.(Link)._0
            var inner__22 Typ = x173
            var t408 string = typ_to_string(inner__22)
            jp406 = t408
        default:
            panic("non-exhaustive match")
        }
        jp404 = jp406
    case QVar:
        var x167 string = ty__18.(QVar)._0
        var name__19 string = x167
        var t409 string = "'" + name__19
        jp404 = t409
    case TArrow:
        var x168 Typ = ty__18.(TArrow)._0
        var x169 Typ = ty__18.(TArrow)._1
        var t2__24 Typ = x169
        var t1__23 Typ = x168
        var t414 bool = typ_is_arrow(t1__23)
        var jp411 string
        if t414 {
            var t415 string = typ_to_string(t1__23)
            var t416 string = "(" + t415
            var t417 string = t416 + ")"
            jp411 = t417
        } else {
            var t418 string = typ_to_string(t1__23)
            jp411 = t418
        }
        var s1__25 string = jp411
        var s2__26 string = typ_to_string(t2__24)
        var t412 string = s1__25 + " -> "
        var t413 string = t412 + s2__26
        jp404 = t413
    default:
        panic("non-exhaustive match")
    }
    retv402 = jp404
    return retv402
}

func env_empty() *_goml_vec_EnvEntry {
    var retv420 *_goml_vec_EnvEntry
    var env__27 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__EnvEntry()
    retv420 = env__27
    return retv420
}

func env_lookup(env__28 *_goml_vec_EnvEntry, name__29 string) Option__Typ {
    var retv422 Option__Typ
    var t423 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__EnvEntry(env__28)
    var t424 int = t423 - 1
    var i__30 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(t424)
    var found__31 *ref_Option__Typ_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_Typ_r_(None{})
    var done__32 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    Loop_loop427:
    for {
        var t440 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(done__32)
        var t441 bool = !t440
        var jp429 bool
        if t441 {
            var t442 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__30)
            var t443 bool = t442 >= 0
            jp429 = t443
        } else {
            jp429 = false
        }
        if jp429 {
            var t430 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__30)
            var entry__33 EnvEntry = vec_get__Vec_8EnvEntry(env__28, t430)
            var t432 string = entry__33.name
            var t433 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(t432, name__29)
            if t433 {
                var t434 Typ = entry__33.ty
                var t435 Option__Typ = Some{
                    _0: t434,
                }
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Option_l_Typ_r_(found__31, t435)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(done__32, true)
            } else {
                var t437 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__30)
                var t438 int = t437 - 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__30, t438)
            }
            continue
        } else {
            break Loop_loop427
        }
    }
    var t426 Option__Typ = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Option_l_Typ_r_(found__31)
    retv422 = t426
    return retv422
}

func subst_lookup(subst__34 *_goml_vec_SubstEntry, name__35 string) Option__Typ {
    var retv445 Option__Typ
    var t446 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SubstEntry(subst__34)
    var t447 int = t446 - 1
    var i__36 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(t447)
    var found__37 *ref_Option__Typ_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_Typ_r_(None{})
    var done__38 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    Loop_loop450:
    for {
        var t463 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(done__38)
        var t464 bool = !t463
        var jp452 bool
        if t464 {
            var t465 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__36)
            var t466 bool = t465 >= 0
            jp452 = t466
        } else {
            jp452 = false
        }
        if jp452 {
            var t453 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__36)
            var entry__39 SubstEntry = vec_get__Vec_10SubstEntry(subst__34, t453)
            var t455 string = entry__39.name
            var t456 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(t455, name__35)
            if t456 {
                var t457 Typ = entry__39.ty
                var t458 Option__Typ = Some{
                    _0: t457,
                }
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Option_l_Typ_r_(found__37, t458)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(done__38, true)
            } else {
                var t460 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__36)
                var t461 int = t460 - 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__36, t461)
            }
            continue
        } else {
            break Loop_loop450
        }
    }
    var t449 Option__Typ = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Option_l_Typ_r_(found__37)
    retv445 = t449
    return retv445
}

func occurs(st__40 CheckerState, tvr__41 *ref_Tv_x, ty__42 Typ) Result__unit__string {
    var retv468 Result__unit__string
    var jp470 Result__unit__string
    switch ty__42.(type) {
    case TVar:
        var x178 *ref_Tv_x = ty__42.(TVar)._0
        var tvr2__43 *ref_Tv_x = x178
        var t473 bool = ptr_eq__Ref_2Tv(tvr__41, tvr2__43)
        var jp472 Result__unit__string
        if t473 {
            var t474 Result__unit__string = Result__unit__string_Err{
                _0: "occurs check",
            }
            jp472 = t474
        } else {
            var mtmp182 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvr2__43)
            var jp476 Result__unit__string
            switch mtmp182.(type) {
            case Unbound:
                var x183 string = mtmp182.(Unbound)._0
                var x184 int32 = mtmp182.(Unbound)._1
                var l2__45 int32 = x184
                var name__44 string = x183
                var mtmp186 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvr__41)
                var jp478 int32
                switch mtmp186.(type) {
                case Unbound:
                    var x188 int32 = mtmp186.(Unbound)._1
                    var l__46 int32 = x188
                    var t481 int32 = min_i32(l__46, l2__45)
                    jp478 = t481
                default:
                    jp478 = l2__45
                }
                var min_level__47 int32 = jp478
                var t479 Tv = Unbound{
                    _0: name__44,
                    _1: min_level__47,
                }
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(tvr2__43, t479)
                var t480 Result__unit__string = Result__unit__string_Ok{
                    _0: struct{}{},
                }
                jp476 = t480
            case Link:
                var x185 Typ = mtmp182.(Link)._0
                var inner__48 Typ = x185
                var t482 Result__unit__string = occurs(st__40, tvr__41, inner__48)
                jp476 = t482
            default:
                panic("non-exhaustive match")
            }
            jp472 = jp476
        }
        jp470 = jp472
    case TArrow:
        var x180 Typ = ty__42.(TArrow)._0
        var x181 Typ = ty__42.(TArrow)._1
        var t2__50 Typ = x181
        var t1__49 Typ = x180
        var mtmp191 Result__unit__string = occurs(st__40, tvr__41, t1__49)
        var jp484 Result__unit__string
        switch mtmp191.(type) {
        case Result__unit__string_Ok:
            var t485 Result__unit__string = occurs(st__40, tvr__41, t2__50)
            jp484 = t485
        case Result__unit__string_Err:
            var x193 string = mtmp191.(Result__unit__string_Err)._0
            var e__51 string = x193
            var t486 Result__unit__string = Result__unit__string_Err{
                _0: e__51,
            }
            jp484 = t486
        default:
            panic("non-exhaustive match")
        }
        jp470 = jp484
    default:
        var t487 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp470 = t487
    }
    retv468 = jp470
    return retv468
}

func unify(st__52 CheckerState, t1__53 Typ, t2__54 Typ) Result__unit__string {
    var retv489 Result__unit__string
    var mtmp194 Tuple2_3Typ_3Typ = Tuple2_3Typ_3Typ{
        _0: t1__53,
        _1: t2__54,
    }
    var x195 Typ = mtmp194._0
    var x196 Typ = mtmp194._1
    var jp491 Result__unit__string
    switch x196.(type) {
    case TVar:
        var x197 *ref_Tv_x = x196.(TVar)._0
        var jp493 Result__unit__string
        switch x195.(type) {
        case TVar:
            var x201 *ref_Tv_x = x195.(TVar)._0
            var r1__55 *ref_Tv_x = x201
            var r2__56 *ref_Tv_x = x197
            var t496 bool = ptr_eq__Ref_2Tv(r1__55, r2__56)
            var jp495 Result__unit__string
            if t496 {
                var t497 Result__unit__string = Result__unit__string_Ok{
                    _0: struct{}{},
                }
                jp495 = t497
            } else {
                var mtmp205 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r1__55)
                var jp499 Result__unit__string
                switch mtmp205.(type) {
                case Unbound:
                    var mtmp209 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r2__56)
                    var jp501 Result__unit__string
                    switch mtmp209.(type) {
                    case Unbound:
                        var t502 Typ = TVar{
                            _0: r2__56,
                        }
                        var mtmp213 Result__unit__string = occurs(st__52, r1__55, t502)
                        var jp504 Result__unit__string
                        switch mtmp213.(type) {
                        case Result__unit__string_Ok:
                            var t505 Typ = TVar{
                                _0: r2__56,
                            }
                            var t506 Tv = Link{
                                _0: t505,
                            }
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(r1__55, t506)
                            var t507 Result__unit__string = Result__unit__string_Ok{
                                _0: struct{}{},
                            }
                            jp504 = t507
                        case Result__unit__string_Err:
                            var x215 string = mtmp213.(Result__unit__string_Err)._0
                            var e__59 string = x215
                            var t508 Result__unit__string = Result__unit__string_Err{
                                _0: e__59,
                            }
                            jp504 = t508
                        default:
                            panic("non-exhaustive match")
                        }
                        jp501 = jp504
                    case Link:
                        var x212 Typ = mtmp209.(Link)._0
                        var inner__58 Typ = x212
                        var t509 Typ = TVar{
                            _0: r1__55,
                        }
                        var t510 Result__unit__string = unify(st__52, t509, inner__58)
                        jp501 = t510
                    default:
                        panic("non-exhaustive match")
                    }
                    jp499 = jp501
                case Link:
                    var x208 Typ = mtmp205.(Link)._0
                    var inner__57 Typ = x208
                    var t511 Typ = TVar{
                        _0: r2__56,
                    }
                    var t512 Result__unit__string = unify(st__52, inner__57, t511)
                    jp499 = t512
                default:
                    panic("non-exhaustive match")
                }
                jp495 = jp499
            }
            jp493 = jp495
        default:
            var r2__65 *ref_Tv_x = x197
            var other__64 Typ = x195
            var mtmp217 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r2__65)
            var jp514 Result__unit__string
            switch mtmp217.(type) {
            case Unbound:
                var mtmp221 Result__unit__string = occurs(st__52, r2__65, other__64)
                var jp516 Result__unit__string
                switch mtmp221.(type) {
                case Result__unit__string_Ok:
                    var t517 Tv = Link{
                        _0: other__64,
                    }
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(r2__65, t517)
                    var t518 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    jp516 = t518
                case Result__unit__string_Err:
                    var x223 string = mtmp221.(Result__unit__string_Err)._0
                    var e__67 string = x223
                    var t519 Result__unit__string = Result__unit__string_Err{
                        _0: e__67,
                    }
                    jp516 = t519
                default:
                    panic("non-exhaustive match")
                }
                jp514 = jp516
            case Link:
                var x220 Typ = mtmp217.(Link)._0
                var inner__66 Typ = x220
                var t520 Result__unit__string = unify(st__52, other__64, inner__66)
                jp514 = t520
            default:
                panic("non-exhaustive match")
            }
            jp493 = jp514
        }
        jp491 = jp493
    case TArrow:
        var x199 Typ = x196.(TArrow)._0
        var x200 Typ = x196.(TArrow)._1
        var jp522 Result__unit__string
        switch x195.(type) {
        case TVar:
            var x225 *ref_Tv_x = x195.(TVar)._0
            var r1__60 *ref_Tv_x = x225
            var other__61 Typ = x196
            var mtmp229 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r1__60)
            var jp524 Result__unit__string
            switch mtmp229.(type) {
            case Unbound:
                var mtmp233 Result__unit__string = occurs(st__52, r1__60, other__61)
                var jp526 Result__unit__string
                switch mtmp233.(type) {
                case Result__unit__string_Ok:
                    var t527 Tv = Link{
                        _0: other__61,
                    }
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(r1__60, t527)
                    var t528 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    jp526 = t528
                case Result__unit__string_Err:
                    var x235 string = mtmp233.(Result__unit__string_Err)._0
                    var e__63 string = x235
                    var t529 Result__unit__string = Result__unit__string_Err{
                        _0: e__63,
                    }
                    jp526 = t529
                default:
                    panic("non-exhaustive match")
                }
                jp524 = jp526
            case Link:
                var x232 Typ = mtmp229.(Link)._0
                var inner__62 Typ = x232
                var t530 Result__unit__string = unify(st__52, inner__62, other__61)
                jp524 = t530
            default:
                panic("non-exhaustive match")
            }
            jp522 = jp524
        case TArrow:
            var x227 Typ = x195.(TArrow)._0
            var x228 Typ = x195.(TArrow)._1
            var a2__69 Typ = x228
            var a1__68 Typ = x227
            var b2__71 Typ = x200
            var b1__70 Typ = x199
            var mtmp237 Result__unit__string = unify(st__52, a1__68, b1__70)
            var jp532 Result__unit__string
            switch mtmp237.(type) {
            case Result__unit__string_Ok:
                var t533 Result__unit__string = unify(st__52, a2__69, b2__71)
                jp532 = t533
            case Result__unit__string_Err:
                var x239 string = mtmp237.(Result__unit__string_Err)._0
                var e__72 string = x239
                var t534 Result__unit__string = Result__unit__string_Err{
                    _0: e__72,
                }
                jp532 = t534
            default:
                panic("non-exhaustive match")
            }
            jp522 = jp532
        default:
            var t535 Result__unit__string = Result__unit__string_Err{
                _0: "unify error",
            }
            jp522 = t535
        }
        jp491 = jp522
    default:
        var jp537 Result__unit__string
        switch x195.(type) {
        case TVar:
            var x240 *ref_Tv_x = x195.(TVar)._0
            var r1__60 *ref_Tv_x = x240
            var other__61 Typ = x196
            var mtmp244 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r1__60)
            var jp539 Result__unit__string
            switch mtmp244.(type) {
            case Unbound:
                var mtmp248 Result__unit__string = occurs(st__52, r1__60, other__61)
                var jp541 Result__unit__string
                switch mtmp248.(type) {
                case Result__unit__string_Ok:
                    var t542 Tv = Link{
                        _0: other__61,
                    }
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(r1__60, t542)
                    var t543 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    jp541 = t543
                case Result__unit__string_Err:
                    var x250 string = mtmp248.(Result__unit__string_Err)._0
                    var e__63 string = x250
                    var t544 Result__unit__string = Result__unit__string_Err{
                        _0: e__63,
                    }
                    jp541 = t544
                default:
                    panic("non-exhaustive match")
                }
                jp539 = jp541
            case Link:
                var x247 Typ = mtmp244.(Link)._0
                var inner__62 Typ = x247
                var t545 Result__unit__string = unify(st__52, inner__62, other__61)
                jp539 = t545
            default:
                panic("non-exhaustive match")
            }
            jp537 = jp539
        default:
            var t546 Result__unit__string = Result__unit__string_Err{
                _0: "unify error",
            }
            jp537 = t546
        }
        jp491 = jp537
    }
    retv489 = jp491
    return retv489
}

func gen(st__73 CheckerState, ty__74 Typ) Typ {
    var retv548 Typ
    var jp550 Typ
    switch ty__74.(type) {
    case TVar:
        var x252 *ref_Tv_x = ty__74.(TVar)._0
        var tvref__75 *ref_Tv_x = x252
        var mtmp256 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvref__75)
        var jp552 Typ
        switch mtmp256.(type) {
        case Unbound:
            var x257 string = mtmp256.(Unbound)._0
            var x258 int32 = mtmp256.(Unbound)._1
            var l__77 int32 = x258
            var name__76 string = x257
            var t553 *ref_int32_x = st__73.current_level
            var cur__78 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t553)
            var t556 bool = l__77 > cur__78
            var jp555 Typ
            if t556 {
                var t557 Typ = QVar{
                    _0: name__76,
                }
                jp555 = t557
            } else {
                var t558 Typ = TVar{
                    _0: tvref__75,
                }
                jp555 = t558
            }
            jp552 = jp555
        case Link:
            var x259 Typ = mtmp256.(Link)._0
            var inner__79 Typ = x259
            var t559 Typ = gen(st__73, inner__79)
            jp552 = t559
        default:
            panic("non-exhaustive match")
        }
        jp550 = jp552
    case TArrow:
        var x254 Typ = ty__74.(TArrow)._0
        var x255 Typ = ty__74.(TArrow)._1
        var t2__81 Typ = x255
        var t1__80 Typ = x254
        var t560 Typ = gen(st__73, t1__80)
        var t561 Typ = gen(st__73, t2__81)
        var t562 Typ = TArrow{
            _0: t560,
            _1: t561,
        }
        jp550 = t562
    default:
        var other__82 Typ = ty__74
        jp550 = other__82
    }
    retv548 = jp550
    return retv548
}

func inst_loop(st__83 CheckerState, subst__84 *_goml_vec_SubstEntry, ty__85 Typ) Tuple2_3Typ_16Vec_10SubstEntry {
    var retv564 Tuple2_3Typ_16Vec_10SubstEntry
    var jp566 Tuple2_3Typ_16Vec_10SubstEntry
    switch ty__85.(type) {
    case TVar:
        var x260 *ref_Tv_x = ty__85.(TVar)._0
        var tvref__90 *ref_Tv_x = x260
        var mtmp264 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvref__90)
        var jp568 Tuple2_3Typ_16Vec_10SubstEntry
        switch mtmp264.(type) {
        case Link:
            var x267 Typ = mtmp264.(Link)._0
            var inner__91 Typ = x267
            var t569 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, subst__84, inner__91)
            jp568 = t569
        default:
            var t570 Typ = TVar{
                _0: tvref__90,
            }
            var t571 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: t570,
                _1: subst__84,
            }
            jp568 = t571
        }
        jp566 = jp568
    case QVar:
        var x261 string = ty__85.(QVar)._0
        var name__86 string = x261
        var mtmp268 Option__Typ = subst_lookup(subst__84, name__86)
        var jp573 Tuple2_3Typ_16Vec_10SubstEntry
        switch mtmp268.(type) {
        case None:
            var tv__88 Typ = newvar(st__83)
            var t574 SubstEntry = SubstEntry{
                name: name__86,
                ty: tv__88,
            }
            var new_subst__89 *_goml_vec_SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SubstEntry(subst__84, t574)
            var t575 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: tv__88,
                _1: new_subst__89,
            }
            jp573 = t575
        case Some:
            var x269 Typ = mtmp268.(Some)._0
            var t__87 Typ = x269
            var t576 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: t__87,
                _1: subst__84,
            }
            jp573 = t576
        default:
            panic("non-exhaustive match")
        }
        jp566 = jp573
    case TArrow:
        var x262 Typ = ty__85.(TArrow)._0
        var x263 Typ = ty__85.(TArrow)._1
        var t2__93 Typ = x263
        var t1__92 Typ = x262
        var mtmp270 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, subst__84, t1__92)
        var x271 Typ = mtmp270._0
        var x272 *_goml_vec_SubstEntry = mtmp270._1
        var subst1__95 *_goml_vec_SubstEntry = x272
        var ty1__94 Typ = x271
        var mtmp273 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, subst1__95, t2__93)
        var x274 Typ = mtmp273._0
        var x275 *_goml_vec_SubstEntry = mtmp273._1
        var subst2__97 *_goml_vec_SubstEntry = x275
        var ty2__96 Typ = x274
        var t577 Typ = TArrow{
            _0: ty1__94,
            _1: ty2__96,
        }
        var t578 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
            _0: t577,
            _1: subst2__97,
        }
        jp566 = t578
    default:
        panic("non-exhaustive match")
    }
    retv564 = jp566
    return retv564
}

func inst(st__98 CheckerState, ty__99 Typ) Typ {
    var retv580 Typ
    var subst0__100 *_goml_vec_SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SubstEntry()
    var mtmp276 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__98, subst0__100, ty__99)
    var x277 Typ = mtmp276._0
    var t__101 Typ = x277
    retv580 = t__101
    return retv580
}

func typeof(st__102 CheckerState, env__103 *_goml_vec_EnvEntry, e__104 Exp) Result__Typ__string {
    var retv582 Result__Typ__string
    var jp584 Result__Typ__string
    switch e__104.(type) {
    case Var:
        var x279 string = e__104.(Var)._0
        var x__105 string = x279
        var mtmp287 Option__Typ = env_lookup(env__103, x__105)
        var jp586 Result__Typ__string
        switch mtmp287.(type) {
        case None:
            var t587 Result__Typ__string = Result__Typ__string_Err{
                _0: "unbound var",
            }
            jp586 = t587
        case Some:
            var x288 Typ = mtmp287.(Some)._0
            var ty__106 Typ = x288
            var t588 Typ = inst(st__102, ty__106)
            var t589 Result__Typ__string = Result__Typ__string_Ok{
                _0: t588,
            }
            jp586 = t589
        default:
            panic("non-exhaustive match")
        }
        jp584 = jp586
    case App:
        var x280 Exp = e__104.(App)._0
        var x281 Exp = e__104.(App)._1
        var e2__114 Exp = x281
        var e1__113 Exp = x280
        var mtmp289 Result__Typ__string = typeof(st__102, env__103, e1__113)
        var jp591 Result__Typ__string
        switch mtmp289.(type) {
        case Result__Typ__string_Ok:
            var x290 Typ = mtmp289.(Result__Typ__string_Ok)._0
            var ty_fun__116 Typ = x290
            var mtmp292 Result__Typ__string = typeof(st__102, env__103, e2__114)
            var jp593 Result__Typ__string
            switch mtmp292.(type) {
            case Result__Typ__string_Ok:
                var x293 Typ = mtmp292.(Result__Typ__string_Ok)._0
                var ty_arg__118 Typ = x293
                var ty_res__119 Typ = newvar(st__102)
                var arrow__120 Typ = TArrow{
                    _0: ty_arg__118,
                    _1: ty_res__119,
                }
                var mtmp295 Result__unit__string = unify(st__102, ty_fun__116, arrow__120)
                var jp595 Result__Typ__string
                switch mtmp295.(type) {
                case Result__unit__string_Ok:
                    var t596 Result__Typ__string = Result__Typ__string_Ok{
                        _0: ty_res__119,
                    }
                    jp595 = t596
                case Result__unit__string_Err:
                    var x297 string = mtmp295.(Result__unit__string_Err)._0
                    var e__121 string = x297
                    var t597 Result__Typ__string = Result__Typ__string_Err{
                        _0: e__121,
                    }
                    jp595 = t597
                default:
                    panic("non-exhaustive match")
                }
                jp593 = jp595
            case Result__Typ__string_Err:
                var x294 string = mtmp292.(Result__Typ__string_Err)._0
                var e__117 string = x294
                var t598 Result__Typ__string = Result__Typ__string_Err{
                    _0: e__117,
                }
                jp593 = t598
            default:
                panic("non-exhaustive match")
            }
            jp591 = jp593
        case Result__Typ__string_Err:
            var x291 string = mtmp289.(Result__Typ__string_Err)._0
            var e__115 string = x291
            var t599 Result__Typ__string = Result__Typ__string_Err{
                _0: e__115,
            }
            jp591 = t599
        default:
            panic("non-exhaustive match")
        }
        jp584 = jp591
    case Lam:
        var x282 string = e__104.(Lam)._0
        var x283 Exp = e__104.(Lam)._1
        var body__108 Exp = x283
        var x__107 string = x282
        var ty_x__109 Typ = newvar(st__102)
        var t600 EnvEntry = EnvEntry{
            name: x__107,
            ty: ty_x__109,
        }
        var env2__110 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__EnvEntry(env__103, t600)
        var mtmp298 Result__Typ__string = typeof(st__102, env2__110, body__108)
        var jp602 Result__Typ__string
        switch mtmp298.(type) {
        case Result__Typ__string_Ok:
            var x299 Typ = mtmp298.(Result__Typ__string_Ok)._0
            var ty_e__111 Typ = x299
            var t603 Typ = TArrow{
                _0: ty_x__109,
                _1: ty_e__111,
            }
            var t604 Result__Typ__string = Result__Typ__string_Ok{
                _0: t603,
            }
            jp602 = t604
        case Result__Typ__string_Err:
            var x300 string = mtmp298.(Result__Typ__string_Err)._0
            var e__112 string = x300
            var t605 Result__Typ__string = Result__Typ__string_Err{
                _0: e__112,
            }
            jp602 = t605
        default:
            panic("non-exhaustive match")
        }
        jp584 = jp602
    case Let:
        var x284 string = e__104.(Let)._0
        var x285 Exp = e__104.(Let)._1
        var x286 Exp = e__104.(Let)._2
        var e2__124 Exp = x286
        var e1__123 Exp = x285
        var x__122 string = x284
        enter_level(st__102)
        var ty_e__125 Result__Typ__string = typeof(st__102, env__103, e1__123)
        leave_level(st__102)
        var jp607 Result__Typ__string
        switch ty_e__125.(type) {
        case Result__Typ__string_Ok:
            var x303 Typ = ty_e__125.(Result__Typ__string_Ok)._0
            var ty1__127 Typ = x303
            var t608 Typ = gen(st__102, ty1__127)
            var t609 EnvEntry = EnvEntry{
                name: x__122,
                ty: t608,
            }
            var env2__128 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__EnvEntry(env__103, t609)
            var t610 Result__Typ__string = typeof(st__102, env2__128, e2__124)
            jp607 = t610
        case Result__Typ__string_Err:
            var x304 string = ty_e__125.(Result__Typ__string_Err)._0
            var e__126 string = x304
            var t611 Result__Typ__string = Result__Typ__string_Err{
                _0: e__126,
            }
            jp607 = t611
        default:
            panic("non-exhaustive match")
        }
        jp584 = jp607
    default:
        panic("non-exhaustive match")
    }
    retv582 = jp584
    return retv582
}

func exp_var(name__129 string) Exp {
    var retv613 Exp
    var t614 Exp = Var{
        _0: name__129,
    }
    retv613 = t614
    return retv613
}

func exp_lam(name__130 string, body__131 Exp) Exp {
    var retv616 Exp
    var t617 Exp = Lam{
        _0: name__130,
        _1: body__131,
    }
    retv616 = t617
    return retv616
}

func exp_app(a__132 Exp, b__133 Exp) Exp {
    var retv619 Exp
    var t620 Exp = App{
        _0: a__132,
        _1: b__133,
    }
    retv619 = t620
    return retv619
}

func exp_let(name__134 string, a__135 Exp, b__136 Exp) Exp {
    var retv622 Exp
    var t623 Exp = Let{
        _0: name__134,
        _1: a__135,
        _2: b__136,
    }
    retv622 = t623
    return retv622
}

func show_result(label__137 string, res__138 Result__Typ__string) struct{} {
    switch res__138.(type) {
    case Result__Typ__string_Ok:
        var x305 Typ = res__138.(Result__Typ__string_Ok)._0
        var ty__139 Typ = x305
        var t626 string = label__137 + ": "
        var t627 string = typ_to_string(ty__139)
        var t628 string = t626 + t627
        println__T_string(t628)
    case Result__Typ__string_Err:
        var x306 string = res__138.(Result__Typ__string_Err)._0
        var e__140 string = x306
        var t630 string = label__137 + ": "
        var t631 string = t630 + e__140
        println__T_string(t631)
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func main0() struct{} {
    var st__141 CheckerState = state_new()
    var t634 Exp = exp_var("x")
    var id__142 Exp = exp_lam("x", t634)
    var t635 Exp = exp_var("x")
    var t636 Exp = exp_var("y")
    var t637 Exp = exp_app(t635, t636)
    var t638 Exp = exp_lam("y", t637)
    var c1__143 Exp = exp_lam("x", t638)
    reset_type_variables(st__141)
    var t639 *_goml_vec_EnvEntry = env_empty()
    var t640 Result__Typ__string = typeof(st__141, t639, id__142)
    show_result("id", t640)
    reset_type_variables(st__141)
    var t641 *_goml_vec_EnvEntry = env_empty()
    var t642 Result__Typ__string = typeof(st__141, t641, c1__143)
    show_result("c1", t642)
    reset_type_variables(st__141)
    var t643 *_goml_vec_EnvEntry = env_empty()
    var t644 Exp = exp_var("x")
    var t645 Exp = exp_let("x", c1__143, t644)
    var t646 Result__Typ__string = typeof(st__141, t643, t645)
    show_result("let_x_c1_x", t646)
    reset_type_variables(st__141)
    var t647 *_goml_vec_EnvEntry = env_empty()
    var t648 Exp = exp_var("z")
    var t649 Exp = exp_lam("z", t648)
    var t650 Exp = exp_var("y")
    var t651 Exp = exp_let("y", t649, t650)
    var t652 Result__Typ__string = typeof(st__141, t647, t651)
    show_result("let_y_id_y", t652)
    reset_type_variables(st__141)
    var t653 *_goml_vec_EnvEntry = env_empty()
    var t654 Exp = exp_var("z")
    var t655 Exp = exp_lam("z", t654)
    var t656 Exp = exp_var("y")
    var t657 Exp = exp_let("y", t655, t656)
    var t658 Exp = exp_lam("x", t657)
    var t659 Result__Typ__string = typeof(st__141, t653, t658)
    show_result("lam_x_let_y_id_y", t659)
    reset_type_variables(st__141)
    var t660 *_goml_vec_EnvEntry = env_empty()
    var t661 Exp = exp_var("z")
    var t662 Exp = exp_lam("z", t661)
    var t663 Exp = exp_var("y")
    var t664 Exp = exp_var("x")
    var t665 Exp = exp_app(t663, t664)
    var t666 Exp = exp_let("y", t662, t665)
    var t667 Exp = exp_lam("x", t666)
    var t668 Result__Typ__string = typeof(st__141, t660, t667)
    show_result("lam_x_let_y_id_yx", t668)
    reset_type_variables(st__141)
    var t669 *_goml_vec_EnvEntry = env_empty()
    var t670 Exp = exp_var("x")
    var t671 Exp = exp_var("x")
    var t672 Exp = exp_app(t670, t671)
    var t673 Exp = exp_lam("x", t672)
    var t674 Result__Typ__string = typeof(st__141, t669, t673)
    show_result("self_apply", t674)
    reset_type_variables(st__141)
    var t675 *_goml_vec_EnvEntry = env_empty()
    var t676 Exp = exp_var("x")
    var t677 Exp = exp_var("x")
    var t678 Exp = exp_let("x", t676, t677)
    var t679 Result__Typ__string = typeof(st__141, t675, t678)
    show_result("unbound_var", t679)
    reset_type_variables(st__141)
    var t680 *_goml_vec_EnvEntry = env_empty()
    var t681 Exp = exp_var("y")
    var t682 Exp = exp_var("y")
    var t683 Exp = exp_var("z")
    var t684 Exp = exp_app(t682, t683)
    var t685 Exp = exp_lam("z", t684)
    var t686 Exp = exp_app(t681, t685)
    var t687 Exp = exp_lam("y", t686)
    var t688 Result__Typ__string = typeof(st__141, t680, t687)
    show_result("max_heiber", t688)
    reset_type_variables(st__141)
    var t689 *_goml_vec_EnvEntry = env_empty()
    var t690 Exp = exp_var("k")
    var t691 Exp = exp_var("k")
    var t692 Exp = exp_var("x")
    var t693 Exp = exp_app(t691, t692)
    var t694 Exp = exp_var("y")
    var t695 Exp = exp_app(t693, t694)
    var t696 Exp = exp_app(t690, t695)
    var t697 Exp = exp_var("k")
    var t698 Exp = exp_var("y")
    var t699 Exp = exp_app(t697, t698)
    var t700 Exp = exp_var("x")
    var t701 Exp = exp_app(t699, t700)
    var t702 Exp = exp_app(t696, t701)
    var t703 Exp = exp_lam("k", t702)
    var t704 Exp = exp_lam("y", t703)
    var t705 Exp = exp_lam("x", t704)
    var t706 Result__Typ__string = typeof(st__141, t689, t705)
    show_result("kirang", t706)
    reset_type_variables(st__141)
    var t707 *_goml_vec_EnvEntry = env_empty()
    var t708 Exp = exp_var("id")
    var t709 Exp = exp_var("id")
    var t710 Exp = exp_app(t708, t709)
    var t711 Exp = exp_let("id", id__142, t710)
    var t712 Result__Typ__string = typeof(st__141, t707, t711)
    show_result("let_id_idid", t712)
    reset_type_variables(st__141)
    var t713 *_goml_vec_EnvEntry = env_empty()
    var t714 Exp = exp_var("x")
    var t715 Exp = exp_app(t714, id__142)
    var t716 Exp = exp_var("z")
    var t717 Exp = exp_let("z", t715, t716)
    var t718 Exp = exp_var("y")
    var t719 Exp = exp_let("y", t717, t718)
    var t720 Exp = exp_let("x", c1__143, t719)
    var t721 Result__Typ__string = typeof(st__141, t713, t720)
    show_result("nested_lets", t721)
    reset_type_variables(st__141)
    var t722 *_goml_vec_EnvEntry = env_empty()
    var t723 Exp = exp_var("x")
    var t724 Exp = exp_var("y")
    var t725 Exp = exp_app(t723, t724)
    var t726 Exp = exp_var("y")
    var t727 Exp = exp_var("x")
    var t728 Exp = exp_app(t726, t727)
    var t729 Exp = exp_lam("x", t728)
    var t730 Exp = exp_let("x", t725, t729)
    var t731 Exp = exp_lam("y", t730)
    var t732 Exp = exp_lam("x", t731)
    var t733 Result__Typ__string = typeof(st__141, t722, t732)
    show_result("fun_x_fun_y_let_x_xy_fun_x_yx", t733)
    reset_type_variables(st__141)
    var t734 *_goml_vec_EnvEntry = env_empty()
    var t735 Exp = exp_var("x")
    var t736 Exp = exp_var("y")
    var t737 Exp = exp_let("y", t735, t736)
    var t738 Exp = exp_lam("x", t737)
    var t739 Result__Typ__string = typeof(st__141, t734, t738)
    show_result("sound_gen_1", t739)
    reset_type_variables(st__141)
    var t740 *_goml_vec_EnvEntry = env_empty()
    var t741 Exp = exp_var("x")
    var t742 Exp = exp_lam("z", t741)
    var t743 Exp = exp_var("y")
    var t744 Exp = exp_let("y", t742, t743)
    var t745 Exp = exp_lam("x", t744)
    var t746 Result__Typ__string = typeof(st__141, t740, t745)
    show_result("sound_gen_2", t746)
    reset_type_variables(st__141)
    var t747 *_goml_vec_EnvEntry = env_empty()
    var t748 Exp = exp_var("x")
    var t749 Exp = exp_var("z")
    var t750 Exp = exp_app(t748, t749)
    var t751 Exp = exp_lam("z", t750)
    var t752 Exp = exp_var("y")
    var t753 Exp = exp_let("y", t751, t752)
    var t754 Exp = exp_lam("x", t753)
    var t755 Result__Typ__string = typeof(st__141, t747, t754)
    show_result("sound_gen_3", t755)
    reset_type_variables(st__141)
    var t756 *_goml_vec_EnvEntry = env_empty()
    var t757 Exp = exp_var("x")
    var t758 Exp = exp_var("y")
    var t759 Exp = exp_app(t757, t758)
    var t760 Exp = exp_var("x")
    var t761 Exp = exp_var("y")
    var t762 Exp = exp_app(t760, t761)
    var t763 Exp = exp_let("x", t759, t762)
    var t764 Exp = exp_lam("y", t763)
    var t765 Exp = exp_lam("x", t764)
    var t766 Result__Typ__string = typeof(st__141, t756, t765)
    show_result("double_apply", t766)
    reset_type_variables(st__141)
    var t767 *_goml_vec_EnvEntry = env_empty()
    var t768 Exp = exp_var("x")
    var t769 Exp = exp_var("y")
    var t770 Exp = exp_var("y")
    var t771 Exp = exp_app(t769, t770)
    var t772 Exp = exp_let("y", t768, t771)
    var t773 Exp = exp_lam("x", t772)
    var t774 Result__Typ__string = typeof(st__141, t767, t773)
    show_result("sound_gen_occurs", t774)
    reset_gensym(st__141)
    var t775 *_goml_vec_EnvEntry = env_empty()
    var t776 Exp = exp_var("x")
    var t777 Exp = exp_app(t776, id__142)
    var t778 Exp = exp_var("z")
    var t779 Exp = exp_let("z", t777, t778)
    var t780 Exp = exp_var("y")
    var t781 Exp = exp_let("y", t779, t780)
    var t782 Exp = exp_lam("x", t781)
    var t783 Result__Typ__string = typeof(st__141, t775, t782)
    show_result("fun_x_let_y_let_z_x_id_z_y", t783)
    println__T_string("")
    println__T_string("All Done")
    println__T_string("")
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__207 int32) *ref_int32_x {
    var retv785 *ref_int32_x
    var t786 *ref_int32_x = ref__Ref_5int32(value__207)
    retv785 = t786
    return retv785
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__209 *ref_int32_x, value__210 int32) struct{} {
    ref_set__Ref_5int32(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__208 *ref_int32_x) int32 {
    var retv790 int32
    var t791 int32 = ref_get__Ref_5int32(self__208)
    retv790 = t791
    return retv790
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__7 rune) string {
    var retv793 string
    var t794 string = _goml_runtime_core_char_to_string(self__7)
    retv793 = t794
    return retv793
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv796 string
    var t797 string = _goml_runtime_core_int32_to_string(self__6)
    retv796 = t797
    return retv796
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Tv(value__207 Tv) *ref_Tv_x {
    var retv799 *ref_Tv_x
    var t800 *ref_Tv_x = ref__Ref_2Tv(value__207)
    retv799 = t800
    return retv799
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(self__208 *ref_Tv_x) Tv {
    var retv802 Tv
    var t803 Tv = ref_get__Ref_2Tv(self__208)
    retv802 = t803
    return retv802
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__EnvEntry() *_goml_vec_EnvEntry {
    var retv805 *_goml_vec_EnvEntry
    var t806 *_goml_vec_EnvEntry = vec_new__Vec_8EnvEntry()
    retv805 = t806
    return retv805
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__EnvEntry(self__137 *_goml_vec_EnvEntry) int {
    var retv808 int
    var t809 int = vec_len__Vec_8EnvEntry(self__137)
    retv808 = t809
    return retv808
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv811 *ref_int_x
    var t812 *ref_int_x = ref__Ref_3int(value__207)
    retv811 = t812
    return retv811
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_Typ_r_(value__207 Option__Typ) *ref_Option__Typ_x {
    var retv814 *ref_Option__Typ_x
    var t815 *ref_Option__Typ_x = ref__Ref_11Option__Typ(value__207)
    retv814 = t815
    return retv814
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__207 bool) *ref_bool_x {
    var retv817 *ref_bool_x
    var t818 *ref_bool_x = ref__Ref_4bool(value__207)
    retv817 = t818
    return retv817
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__208 *ref_bool_x) bool {
    var retv820 bool
    var t821 bool = ref_get__Ref_4bool(self__208)
    retv820 = t821
    return retv820
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv823 int
    var t824 int = ref_get__Ref_3int(self__208)
    retv823 = t824
    return retv823
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__55 string, other__56 string) bool {
    var retv826 bool
    var t827 bool = self__55 == other__56
    retv826 = t827
    return retv826
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Option_l_Typ_r_(self__209 *ref_Option__Typ_x, value__210 Option__Typ) struct{} {
    ref_set__Ref_11Option__Typ(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(self__209 *ref_bool_x, value__210 bool) struct{} {
    ref_set__Ref_4bool(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Option_l_Typ_r_(self__208 *ref_Option__Typ_x) Option__Typ {
    var retv835 Option__Typ
    var t836 Option__Typ = ref_get__Ref_11Option__Typ(self__208)
    retv835 = t836
    return retv835
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SubstEntry(self__137 *_goml_vec_SubstEntry) int {
    var retv838 int
    var t839 int = vec_len__Vec_10SubstEntry(self__137)
    retv838 = t839
    return retv838
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(self__209 *ref_Tv_x, value__210 Tv) struct{} {
    ref_set__Ref_2Tv(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SubstEntry(self__128 *_goml_vec_SubstEntry, elem__129 SubstEntry) *_goml_vec_SubstEntry {
    var retv843 *_goml_vec_SubstEntry
    var t844 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SubstEntry(self__128)
    var t845 int = t844 + 1
    var result__130 *_goml_vec_SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__SubstEntry(t845)
    var index__131 int = 0
    Loop_loop847:
    for {
        var t848 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SubstEntry(self__128)
        var t849 bool = index__131 < t848
        if t849 {
            var t850 SubstEntry = vec_get__Vec_10SubstEntry(self__128, index__131)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SubstEntry(result__130, t850)
            var compound_old38 int = index__131
            var compound_value39 int = 1
            var t851 int = compound_old38 + compound_value39
            index__131 = t851
            continue
        } else {
            break Loop_loop847
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SubstEntry(result__130, elem__129)
    retv843 = result__130
    return retv843
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SubstEntry() *_goml_vec_SubstEntry {
    var retv854 *_goml_vec_SubstEntry
    var t855 *_goml_vec_SubstEntry = vec_new__Vec_10SubstEntry()
    retv854 = t855
    return retv854
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__EnvEntry(self__128 *_goml_vec_EnvEntry, elem__129 EnvEntry) *_goml_vec_EnvEntry {
    var retv857 *_goml_vec_EnvEntry
    var t858 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__EnvEntry(self__128)
    var t859 int = t858 + 1
    var result__130 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__EnvEntry(t859)
    var index__131 int = 0
    Loop_loop861:
    for {
        var t862 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__EnvEntry(self__128)
        var t863 bool = index__131 < t862
        if t863 {
            var t864 EnvEntry = vec_get__Vec_8EnvEntry(self__128, index__131)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__EnvEntry(result__130, t864)
            var compound_old38 int = index__131
            var compound_value39 int = 1
            var t865 int = compound_old38 + compound_value39
            index__131 = t865
            continue
        } else {
            break Loop_loop861
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__EnvEntry(result__130, elem__129)
    retv857 = result__130
    return retv857
}

func println__T_string(value__1 string) struct{} {
    var t868 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t868)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__SubstEntry(capacity__125 int) *_goml_vec_SubstEntry {
    var retv871 *_goml_vec_SubstEntry
    var t872 *_goml_vec_SubstEntry = vec_with_capacity__Vec_10SubstEntry(capacity__125)
    retv871 = t872
    return retv871
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SubstEntry(self__126 *_goml_vec_SubstEntry, elem__127 SubstEntry) struct{} {
    vec_push__Vec_10SubstEntry(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__EnvEntry(capacity__125 int) *_goml_vec_EnvEntry {
    var retv876 *_goml_vec_EnvEntry
    var t877 *_goml_vec_EnvEntry = vec_with_capacity__Vec_8EnvEntry(capacity__125)
    retv876 = t877
    return retv876
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__EnvEntry(self__126 *_goml_vec_EnvEntry, elem__127 EnvEntry) struct{} {
    vec_push__Vec_8EnvEntry(self__126, elem__127)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv881 string
    retv881 = self__38
    return retv881
}

func main() {
    main0()
}
