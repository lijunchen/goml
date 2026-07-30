package main

import (
    _goml_fmt "fmt"
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
    var retv305 CheckerState
    var t306 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var t307 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(1)
    var t308 CheckerState = CheckerState{
        gensym_counter: t306,
        current_level: t307,
    }
    retv305 = t308
    return retv305
}

func reset_gensym(st__0 CheckerState) struct{} {
    var t310 *ref_int32_x = st__0.gensym_counter
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t310, 0)
    return struct{}{}
}

func reset_level(st__1 CheckerState) struct{} {
    var t312 *ref_int32_x = st__1.current_level
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t312, 1)
    return struct{}{}
}

func reset_type_variables(st__2 CheckerState) struct{} {
    reset_gensym(st__2)
    reset_level(st__2)
    return struct{}{}
}

func enter_level(st__3 CheckerState) struct{} {
    var t316 *ref_int32_x = st__3.current_level
    var l__4 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t316)
    var t317 *ref_int32_x = st__3.current_level
    var t318 int32 = l__4 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t317, t318)
    return struct{}{}
}

func leave_level(st__5 CheckerState) struct{} {
    var t320 *ref_int32_x = st__5.current_level
    var l__6 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t320)
    var t321 *ref_int32_x = st__5.current_level
    var t322 int32 = l__6 - 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t321, t322)
    return struct{}{}
}

func min_i32(a__7 int32, b__8 int32) int32 {
    var retv324 int32
    var t327 bool = a__7 < b__8
    var jp326 int32
    if t327 {
        jp326 = a__7
    } else {
        jp326 = b__8
    }
    retv324 = jp326
    return retv324
}

func nth_letter(n__9 int32) rune {
    var retv329 rune
    var jp331 rune
    switch n__9 {
    case 0:
        jp331 = 97
    case 1:
        jp331 = 98
    case 2:
        jp331 = 99
    case 3:
        jp331 = 100
    case 4:
        jp331 = 101
    case 5:
        jp331 = 102
    case 6:
        jp331 = 103
    case 7:
        jp331 = 104
    case 8:
        jp331 = 105
    case 9:
        jp331 = 106
    case 10:
        jp331 = 107
    case 11:
        jp331 = 108
    case 12:
        jp331 = 109
    case 13:
        jp331 = 110
    case 14:
        jp331 = 111
    case 15:
        jp331 = 112
    case 16:
        jp331 = 113
    case 17:
        jp331 = 114
    case 18:
        jp331 = 115
    case 19:
        jp331 = 116
    case 20:
        jp331 = 117
    case 21:
        jp331 = 118
    case 22:
        jp331 = 119
    case 23:
        jp331 = 120
    case 24:
        jp331 = 121
    case 25:
        jp331 = 122
    default:
        jp331 = 97
    }
    retv329 = jp331
    return retv329
}

func gensym(st__10 CheckerState) string {
    var retv333 string
    var t334 *ref_int32_x = st__10.gensym_counter
    var n__11 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t334)
    var t335 *ref_int32_x = st__10.gensym_counter
    var t336 int32 = n__11 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t335, t336)
    var t339 bool = n__11 < 26
    var jp338 string
    if t339 {
        var t340 rune = nth_letter(n__11)
        var t341 string = _goml_m_inherent_i_char_i_char_i_to__string(t340)
        jp338 = t341
    } else {
        var t342 string = _goml_m_inherent_i_int32_i_int32_i_to__string(n__11)
        var t343 string = "t" + t342
        jp338 = t343
    }
    retv333 = jp338
    return retv333
}

func newvar(st__12 CheckerState) Typ {
    var retv345 Typ
    var name__13 string = gensym(st__12)
    var t346 *ref_int32_x = st__12.current_level
    var level__14 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t346)
    var t347 Tv = Unbound{
        _0: name__13,
        _1: level__14,
    }
    var t348 *ref_Tv_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Tv(t347)
    var t349 Typ = TVar{
        _0: t348,
    }
    retv345 = t349
    return retv345
}

func typ_is_arrow(ty__15 Typ) bool {
    var retv351 bool
    var jp353 bool
    switch ty__15.(type) {
    case TVar:
        var x114 *ref_Tv_x = ty__15.(TVar)._0
        var tvref__16 *ref_Tv_x = x114
        var mtmp118 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvref__16)
        var jp355 bool
        switch mtmp118.(type) {
        case Link:
            var x121 Typ = mtmp118.(Link)._0
            var inner__17 Typ = x121
            var t356 bool = typ_is_arrow(inner__17)
            jp355 = t356
        default:
            jp355 = false
        }
        jp353 = jp355
    case TArrow:
        jp353 = true
    default:
        jp353 = false
    }
    retv351 = jp353
    return retv351
}

func typ_to_string(ty__18 Typ) string {
    var retv358 string
    var jp360 string
    switch ty__18.(type) {
    case TVar:
        var x122 *ref_Tv_x = ty__18.(TVar)._0
        var tvref__20 *ref_Tv_x = x122
        var mtmp126 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvref__20)
        var jp362 string
        switch mtmp126.(type) {
        case Unbound:
            var x127 string = mtmp126.(Unbound)._0
            var name__21 string = x127
            var t363 string = "'" + name__21
            jp362 = t363
        case Link:
            var x129 Typ = mtmp126.(Link)._0
            var inner__22 Typ = x129
            var t364 string = typ_to_string(inner__22)
            jp362 = t364
        default:
            panic("non-exhaustive match")
        }
        jp360 = jp362
    case QVar:
        var x123 string = ty__18.(QVar)._0
        var name__19 string = x123
        var t365 string = "'" + name__19
        jp360 = t365
    case TArrow:
        var x124 Typ = ty__18.(TArrow)._0
        var x125 Typ = ty__18.(TArrow)._1
        var t2__24 Typ = x125
        var t1__23 Typ = x124
        var t370 bool = typ_is_arrow(t1__23)
        var jp367 string
        if t370 {
            var t371 string = typ_to_string(t1__23)
            var t372 string = "(" + t371
            var t373 string = t372 + ")"
            jp367 = t373
        } else {
            var t374 string = typ_to_string(t1__23)
            jp367 = t374
        }
        var s1__25 string = jp367
        var s2__26 string = typ_to_string(t2__24)
        var t368 string = s1__25 + " -> "
        var t369 string = t368 + s2__26
        jp360 = t369
    default:
        panic("non-exhaustive match")
    }
    retv358 = jp360
    return retv358
}

func env_empty() *_goml_vec_EnvEntry {
    var retv376 *_goml_vec_EnvEntry
    var env__27 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__EnvEntry()
    retv376 = env__27
    return retv376
}

func env_lookup(env__28 *_goml_vec_EnvEntry, name__29 string) Option__Typ {
    var retv378 Option__Typ
    var t379 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__EnvEntry(env__28)
    var t380 int = t379 - 1
    var i__30 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(t380)
    var found__31 *ref_Option__Typ_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_Typ_r_(None{})
    var done__32 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    Loop_loop383:
    for {
        var t396 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(done__32)
        var t397 bool = !t396
        var jp385 bool
        if t397 {
            var t398 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__30)
            var t399 bool = t398 >= 0
            jp385 = t399
        } else {
            jp385 = false
        }
        if jp385 {
            var t386 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__30)
            var entry__33 EnvEntry = vec_get__Vec_8EnvEntry(env__28, t386)
            var t388 string = entry__33.name
            var t389 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(t388, name__29)
            if t389 {
                var t390 Typ = entry__33.ty
                var t391 Option__Typ = Some{
                    _0: t390,
                }
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Option_l_Typ_r_(found__31, t391)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(done__32, true)
            } else {
                var t393 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__30)
                var t394 int = t393 - 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__30, t394)
            }
            continue
        } else {
            break Loop_loop383
        }
    }
    var t382 Option__Typ = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Option_l_Typ_r_(found__31)
    retv378 = t382
    return retv378
}

func subst_lookup(subst__34 *_goml_vec_SubstEntry, name__35 string) Option__Typ {
    var retv401 Option__Typ
    var t402 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SubstEntry(subst__34)
    var t403 int = t402 - 1
    var i__36 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(t403)
    var found__37 *ref_Option__Typ_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_Typ_r_(None{})
    var done__38 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    Loop_loop406:
    for {
        var t419 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(done__38)
        var t420 bool = !t419
        var jp408 bool
        if t420 {
            var t421 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__36)
            var t422 bool = t421 >= 0
            jp408 = t422
        } else {
            jp408 = false
        }
        if jp408 {
            var t409 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__36)
            var entry__39 SubstEntry = vec_get__Vec_10SubstEntry(subst__34, t409)
            var t411 string = entry__39.name
            var t412 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(t411, name__35)
            if t412 {
                var t413 Typ = entry__39.ty
                var t414 Option__Typ = Some{
                    _0: t413,
                }
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Option_l_Typ_r_(found__37, t414)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(done__38, true)
            } else {
                var t416 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__36)
                var t417 int = t416 - 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__36, t417)
            }
            continue
        } else {
            break Loop_loop406
        }
    }
    var t405 Option__Typ = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Option_l_Typ_r_(found__37)
    retv401 = t405
    return retv401
}

func occurs(st__40 CheckerState, tvr__41 *ref_Tv_x, ty__42 Typ) Result__unit__string {
    var retv424 Result__unit__string
    var jp426 Result__unit__string
    switch ty__42.(type) {
    case TVar:
        var x134 *ref_Tv_x = ty__42.(TVar)._0
        var tvr2__43 *ref_Tv_x = x134
        var t429 bool = ptr_eq__Ref_2Tv(tvr__41, tvr2__43)
        var jp428 Result__unit__string
        if t429 {
            var t430 Result__unit__string = Result__unit__string_Err{
                _0: "occurs check",
            }
            jp428 = t430
        } else {
            var mtmp138 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvr2__43)
            var jp432 Result__unit__string
            switch mtmp138.(type) {
            case Unbound:
                var x139 string = mtmp138.(Unbound)._0
                var x140 int32 = mtmp138.(Unbound)._1
                var l2__45 int32 = x140
                var name__44 string = x139
                var mtmp142 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvr__41)
                var jp434 int32
                switch mtmp142.(type) {
                case Unbound:
                    var x144 int32 = mtmp142.(Unbound)._1
                    var l__46 int32 = x144
                    var t437 int32 = min_i32(l__46, l2__45)
                    jp434 = t437
                default:
                    jp434 = l2__45
                }
                var min_level__47 int32 = jp434
                var t435 Tv = Unbound{
                    _0: name__44,
                    _1: min_level__47,
                }
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(tvr2__43, t435)
                var t436 Result__unit__string = Result__unit__string_Ok{
                    _0: struct{}{},
                }
                jp432 = t436
            case Link:
                var x141 Typ = mtmp138.(Link)._0
                var inner__48 Typ = x141
                var t438 Result__unit__string = occurs(st__40, tvr__41, inner__48)
                jp432 = t438
            default:
                panic("non-exhaustive match")
            }
            jp428 = jp432
        }
        jp426 = jp428
    case TArrow:
        var x136 Typ = ty__42.(TArrow)._0
        var x137 Typ = ty__42.(TArrow)._1
        var t2__50 Typ = x137
        var t1__49 Typ = x136
        var mtmp147 Result__unit__string = occurs(st__40, tvr__41, t1__49)
        var jp440 Result__unit__string
        switch mtmp147.(type) {
        case Result__unit__string_Ok:
            var t441 Result__unit__string = occurs(st__40, tvr__41, t2__50)
            jp440 = t441
        case Result__unit__string_Err:
            var x149 string = mtmp147.(Result__unit__string_Err)._0
            var e__51 string = x149
            var t442 Result__unit__string = Result__unit__string_Err{
                _0: e__51,
            }
            jp440 = t442
        default:
            panic("non-exhaustive match")
        }
        jp426 = jp440
    default:
        var t443 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp426 = t443
    }
    retv424 = jp426
    return retv424
}

func unify(st__52 CheckerState, t1__53 Typ, t2__54 Typ) Result__unit__string {
    var retv445 Result__unit__string
    var mtmp150 Tuple2_3Typ_3Typ = Tuple2_3Typ_3Typ{
        _0: t1__53,
        _1: t2__54,
    }
    var x151 Typ = mtmp150._0
    var x152 Typ = mtmp150._1
    var jp447 Result__unit__string
    switch x152.(type) {
    case TVar:
        var x153 *ref_Tv_x = x152.(TVar)._0
        var jp449 Result__unit__string
        switch x151.(type) {
        case TVar:
            var x157 *ref_Tv_x = x151.(TVar)._0
            var r1__55 *ref_Tv_x = x157
            var r2__56 *ref_Tv_x = x153
            var t452 bool = ptr_eq__Ref_2Tv(r1__55, r2__56)
            var jp451 Result__unit__string
            if t452 {
                var t453 Result__unit__string = Result__unit__string_Ok{
                    _0: struct{}{},
                }
                jp451 = t453
            } else {
                var mtmp161 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r1__55)
                var jp455 Result__unit__string
                switch mtmp161.(type) {
                case Unbound:
                    var mtmp165 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r2__56)
                    var jp457 Result__unit__string
                    switch mtmp165.(type) {
                    case Unbound:
                        var t458 Typ = TVar{
                            _0: r2__56,
                        }
                        var mtmp169 Result__unit__string = occurs(st__52, r1__55, t458)
                        var jp460 Result__unit__string
                        switch mtmp169.(type) {
                        case Result__unit__string_Ok:
                            var t461 Typ = TVar{
                                _0: r2__56,
                            }
                            var t462 Tv = Link{
                                _0: t461,
                            }
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(r1__55, t462)
                            var t463 Result__unit__string = Result__unit__string_Ok{
                                _0: struct{}{},
                            }
                            jp460 = t463
                        case Result__unit__string_Err:
                            var x171 string = mtmp169.(Result__unit__string_Err)._0
                            var e__59 string = x171
                            var t464 Result__unit__string = Result__unit__string_Err{
                                _0: e__59,
                            }
                            jp460 = t464
                        default:
                            panic("non-exhaustive match")
                        }
                        jp457 = jp460
                    case Link:
                        var x168 Typ = mtmp165.(Link)._0
                        var inner__58 Typ = x168
                        var t465 Typ = TVar{
                            _0: r1__55,
                        }
                        var t466 Result__unit__string = unify(st__52, t465, inner__58)
                        jp457 = t466
                    default:
                        panic("non-exhaustive match")
                    }
                    jp455 = jp457
                case Link:
                    var x164 Typ = mtmp161.(Link)._0
                    var inner__57 Typ = x164
                    var t467 Typ = TVar{
                        _0: r2__56,
                    }
                    var t468 Result__unit__string = unify(st__52, inner__57, t467)
                    jp455 = t468
                default:
                    panic("non-exhaustive match")
                }
                jp451 = jp455
            }
            jp449 = jp451
        default:
            var r2__65 *ref_Tv_x = x153
            var other__64 Typ = x151
            var mtmp173 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r2__65)
            var jp470 Result__unit__string
            switch mtmp173.(type) {
            case Unbound:
                var mtmp177 Result__unit__string = occurs(st__52, r2__65, other__64)
                var jp472 Result__unit__string
                switch mtmp177.(type) {
                case Result__unit__string_Ok:
                    var t473 Tv = Link{
                        _0: other__64,
                    }
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(r2__65, t473)
                    var t474 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    jp472 = t474
                case Result__unit__string_Err:
                    var x179 string = mtmp177.(Result__unit__string_Err)._0
                    var e__67 string = x179
                    var t475 Result__unit__string = Result__unit__string_Err{
                        _0: e__67,
                    }
                    jp472 = t475
                default:
                    panic("non-exhaustive match")
                }
                jp470 = jp472
            case Link:
                var x176 Typ = mtmp173.(Link)._0
                var inner__66 Typ = x176
                var t476 Result__unit__string = unify(st__52, other__64, inner__66)
                jp470 = t476
            default:
                panic("non-exhaustive match")
            }
            jp449 = jp470
        }
        jp447 = jp449
    case TArrow:
        var x155 Typ = x152.(TArrow)._0
        var x156 Typ = x152.(TArrow)._1
        var jp478 Result__unit__string
        switch x151.(type) {
        case TVar:
            var x181 *ref_Tv_x = x151.(TVar)._0
            var r1__60 *ref_Tv_x = x181
            var other__61 Typ = x152
            var mtmp185 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r1__60)
            var jp480 Result__unit__string
            switch mtmp185.(type) {
            case Unbound:
                var mtmp189 Result__unit__string = occurs(st__52, r1__60, other__61)
                var jp482 Result__unit__string
                switch mtmp189.(type) {
                case Result__unit__string_Ok:
                    var t483 Tv = Link{
                        _0: other__61,
                    }
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(r1__60, t483)
                    var t484 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    jp482 = t484
                case Result__unit__string_Err:
                    var x191 string = mtmp189.(Result__unit__string_Err)._0
                    var e__63 string = x191
                    var t485 Result__unit__string = Result__unit__string_Err{
                        _0: e__63,
                    }
                    jp482 = t485
                default:
                    panic("non-exhaustive match")
                }
                jp480 = jp482
            case Link:
                var x188 Typ = mtmp185.(Link)._0
                var inner__62 Typ = x188
                var t486 Result__unit__string = unify(st__52, inner__62, other__61)
                jp480 = t486
            default:
                panic("non-exhaustive match")
            }
            jp478 = jp480
        case TArrow:
            var x183 Typ = x151.(TArrow)._0
            var x184 Typ = x151.(TArrow)._1
            var a2__69 Typ = x184
            var a1__68 Typ = x183
            var b2__71 Typ = x156
            var b1__70 Typ = x155
            var mtmp193 Result__unit__string = unify(st__52, a1__68, b1__70)
            var jp488 Result__unit__string
            switch mtmp193.(type) {
            case Result__unit__string_Ok:
                var t489 Result__unit__string = unify(st__52, a2__69, b2__71)
                jp488 = t489
            case Result__unit__string_Err:
                var x195 string = mtmp193.(Result__unit__string_Err)._0
                var e__72 string = x195
                var t490 Result__unit__string = Result__unit__string_Err{
                    _0: e__72,
                }
                jp488 = t490
            default:
                panic("non-exhaustive match")
            }
            jp478 = jp488
        default:
            var t491 Result__unit__string = Result__unit__string_Err{
                _0: "unify error",
            }
            jp478 = t491
        }
        jp447 = jp478
    default:
        var jp493 Result__unit__string
        switch x151.(type) {
        case TVar:
            var x196 *ref_Tv_x = x151.(TVar)._0
            var r1__60 *ref_Tv_x = x196
            var other__61 Typ = x152
            var mtmp200 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r1__60)
            var jp495 Result__unit__string
            switch mtmp200.(type) {
            case Unbound:
                var mtmp204 Result__unit__string = occurs(st__52, r1__60, other__61)
                var jp497 Result__unit__string
                switch mtmp204.(type) {
                case Result__unit__string_Ok:
                    var t498 Tv = Link{
                        _0: other__61,
                    }
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(r1__60, t498)
                    var t499 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    jp497 = t499
                case Result__unit__string_Err:
                    var x206 string = mtmp204.(Result__unit__string_Err)._0
                    var e__63 string = x206
                    var t500 Result__unit__string = Result__unit__string_Err{
                        _0: e__63,
                    }
                    jp497 = t500
                default:
                    panic("non-exhaustive match")
                }
                jp495 = jp497
            case Link:
                var x203 Typ = mtmp200.(Link)._0
                var inner__62 Typ = x203
                var t501 Result__unit__string = unify(st__52, inner__62, other__61)
                jp495 = t501
            default:
                panic("non-exhaustive match")
            }
            jp493 = jp495
        default:
            var t502 Result__unit__string = Result__unit__string_Err{
                _0: "unify error",
            }
            jp493 = t502
        }
        jp447 = jp493
    }
    retv445 = jp447
    return retv445
}

func gen(st__73 CheckerState, ty__74 Typ) Typ {
    var retv504 Typ
    var jp506 Typ
    switch ty__74.(type) {
    case TVar:
        var x208 *ref_Tv_x = ty__74.(TVar)._0
        var tvref__75 *ref_Tv_x = x208
        var mtmp212 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvref__75)
        var jp508 Typ
        switch mtmp212.(type) {
        case Unbound:
            var x213 string = mtmp212.(Unbound)._0
            var x214 int32 = mtmp212.(Unbound)._1
            var l__77 int32 = x214
            var name__76 string = x213
            var t509 *ref_int32_x = st__73.current_level
            var cur__78 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t509)
            var t512 bool = l__77 > cur__78
            var jp511 Typ
            if t512 {
                var t513 Typ = QVar{
                    _0: name__76,
                }
                jp511 = t513
            } else {
                var t514 Typ = TVar{
                    _0: tvref__75,
                }
                jp511 = t514
            }
            jp508 = jp511
        case Link:
            var x215 Typ = mtmp212.(Link)._0
            var inner__79 Typ = x215
            var t515 Typ = gen(st__73, inner__79)
            jp508 = t515
        default:
            panic("non-exhaustive match")
        }
        jp506 = jp508
    case TArrow:
        var x210 Typ = ty__74.(TArrow)._0
        var x211 Typ = ty__74.(TArrow)._1
        var t2__81 Typ = x211
        var t1__80 Typ = x210
        var t516 Typ = gen(st__73, t1__80)
        var t517 Typ = gen(st__73, t2__81)
        var t518 Typ = TArrow{
            _0: t516,
            _1: t517,
        }
        jp506 = t518
    default:
        var other__82 Typ = ty__74
        jp506 = other__82
    }
    retv504 = jp506
    return retv504
}

func inst_loop(st__83 CheckerState, subst__84 *_goml_vec_SubstEntry, ty__85 Typ) Tuple2_3Typ_16Vec_10SubstEntry {
    var retv520 Tuple2_3Typ_16Vec_10SubstEntry
    var jp522 Tuple2_3Typ_16Vec_10SubstEntry
    switch ty__85.(type) {
    case TVar:
        var x216 *ref_Tv_x = ty__85.(TVar)._0
        var tvref__90 *ref_Tv_x = x216
        var mtmp220 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvref__90)
        var jp524 Tuple2_3Typ_16Vec_10SubstEntry
        switch mtmp220.(type) {
        case Link:
            var x223 Typ = mtmp220.(Link)._0
            var inner__91 Typ = x223
            var t525 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, subst__84, inner__91)
            jp524 = t525
        default:
            var t526 Typ = TVar{
                _0: tvref__90,
            }
            var t527 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: t526,
                _1: subst__84,
            }
            jp524 = t527
        }
        jp522 = jp524
    case QVar:
        var x217 string = ty__85.(QVar)._0
        var name__86 string = x217
        var mtmp224 Option__Typ = subst_lookup(subst__84, name__86)
        var jp529 Tuple2_3Typ_16Vec_10SubstEntry
        switch mtmp224.(type) {
        case None:
            var tv__88 Typ = newvar(st__83)
            var t530 SubstEntry = SubstEntry{
                name: name__86,
                ty: tv__88,
            }
            var new_subst__89 *_goml_vec_SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SubstEntry(subst__84, t530)
            var t531 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: tv__88,
                _1: new_subst__89,
            }
            jp529 = t531
        case Some:
            var x225 Typ = mtmp224.(Some)._0
            var t__87 Typ = x225
            var t532 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: t__87,
                _1: subst__84,
            }
            jp529 = t532
        default:
            panic("non-exhaustive match")
        }
        jp522 = jp529
    case TArrow:
        var x218 Typ = ty__85.(TArrow)._0
        var x219 Typ = ty__85.(TArrow)._1
        var t2__93 Typ = x219
        var t1__92 Typ = x218
        var mtmp226 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, subst__84, t1__92)
        var x227 Typ = mtmp226._0
        var x228 *_goml_vec_SubstEntry = mtmp226._1
        var subst1__95 *_goml_vec_SubstEntry = x228
        var ty1__94 Typ = x227
        var mtmp229 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, subst1__95, t2__93)
        var x230 Typ = mtmp229._0
        var x231 *_goml_vec_SubstEntry = mtmp229._1
        var subst2__97 *_goml_vec_SubstEntry = x231
        var ty2__96 Typ = x230
        var t533 Typ = TArrow{
            _0: ty1__94,
            _1: ty2__96,
        }
        var t534 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
            _0: t533,
            _1: subst2__97,
        }
        jp522 = t534
    default:
        panic("non-exhaustive match")
    }
    retv520 = jp522
    return retv520
}

func inst(st__98 CheckerState, ty__99 Typ) Typ {
    var retv536 Typ
    var subst0__100 *_goml_vec_SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SubstEntry()
    var mtmp232 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__98, subst0__100, ty__99)
    var x233 Typ = mtmp232._0
    var t__101 Typ = x233
    retv536 = t__101
    return retv536
}

func typeof(st__102 CheckerState, env__103 *_goml_vec_EnvEntry, e__104 Exp) Result__Typ__string {
    var retv538 Result__Typ__string
    var jp540 Result__Typ__string
    switch e__104.(type) {
    case Var:
        var x235 string = e__104.(Var)._0
        var x__105 string = x235
        var mtmp243 Option__Typ = env_lookup(env__103, x__105)
        var jp542 Result__Typ__string
        switch mtmp243.(type) {
        case None:
            var t543 Result__Typ__string = Result__Typ__string_Err{
                _0: "unbound var",
            }
            jp542 = t543
        case Some:
            var x244 Typ = mtmp243.(Some)._0
            var ty__106 Typ = x244
            var t544 Typ = inst(st__102, ty__106)
            var t545 Result__Typ__string = Result__Typ__string_Ok{
                _0: t544,
            }
            jp542 = t545
        default:
            panic("non-exhaustive match")
        }
        jp540 = jp542
    case App:
        var x236 Exp = e__104.(App)._0
        var x237 Exp = e__104.(App)._1
        var e2__114 Exp = x237
        var e1__113 Exp = x236
        var mtmp245 Result__Typ__string = typeof(st__102, env__103, e1__113)
        var jp547 Result__Typ__string
        switch mtmp245.(type) {
        case Result__Typ__string_Ok:
            var x246 Typ = mtmp245.(Result__Typ__string_Ok)._0
            var ty_fun__116 Typ = x246
            var mtmp248 Result__Typ__string = typeof(st__102, env__103, e2__114)
            var jp549 Result__Typ__string
            switch mtmp248.(type) {
            case Result__Typ__string_Ok:
                var x249 Typ = mtmp248.(Result__Typ__string_Ok)._0
                var ty_arg__118 Typ = x249
                var ty_res__119 Typ = newvar(st__102)
                var arrow__120 Typ = TArrow{
                    _0: ty_arg__118,
                    _1: ty_res__119,
                }
                var mtmp251 Result__unit__string = unify(st__102, ty_fun__116, arrow__120)
                var jp551 Result__Typ__string
                switch mtmp251.(type) {
                case Result__unit__string_Ok:
                    var t552 Result__Typ__string = Result__Typ__string_Ok{
                        _0: ty_res__119,
                    }
                    jp551 = t552
                case Result__unit__string_Err:
                    var x253 string = mtmp251.(Result__unit__string_Err)._0
                    var e__121 string = x253
                    var t553 Result__Typ__string = Result__Typ__string_Err{
                        _0: e__121,
                    }
                    jp551 = t553
                default:
                    panic("non-exhaustive match")
                }
                jp549 = jp551
            case Result__Typ__string_Err:
                var x250 string = mtmp248.(Result__Typ__string_Err)._0
                var e__117 string = x250
                var t554 Result__Typ__string = Result__Typ__string_Err{
                    _0: e__117,
                }
                jp549 = t554
            default:
                panic("non-exhaustive match")
            }
            jp547 = jp549
        case Result__Typ__string_Err:
            var x247 string = mtmp245.(Result__Typ__string_Err)._0
            var e__115 string = x247
            var t555 Result__Typ__string = Result__Typ__string_Err{
                _0: e__115,
            }
            jp547 = t555
        default:
            panic("non-exhaustive match")
        }
        jp540 = jp547
    case Lam:
        var x238 string = e__104.(Lam)._0
        var x239 Exp = e__104.(Lam)._1
        var body__108 Exp = x239
        var x__107 string = x238
        var ty_x__109 Typ = newvar(st__102)
        var t556 EnvEntry = EnvEntry{
            name: x__107,
            ty: ty_x__109,
        }
        var env2__110 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__EnvEntry(env__103, t556)
        var mtmp254 Result__Typ__string = typeof(st__102, env2__110, body__108)
        var jp558 Result__Typ__string
        switch mtmp254.(type) {
        case Result__Typ__string_Ok:
            var x255 Typ = mtmp254.(Result__Typ__string_Ok)._0
            var ty_e__111 Typ = x255
            var t559 Typ = TArrow{
                _0: ty_x__109,
                _1: ty_e__111,
            }
            var t560 Result__Typ__string = Result__Typ__string_Ok{
                _0: t559,
            }
            jp558 = t560
        case Result__Typ__string_Err:
            var x256 string = mtmp254.(Result__Typ__string_Err)._0
            var e__112 string = x256
            var t561 Result__Typ__string = Result__Typ__string_Err{
                _0: e__112,
            }
            jp558 = t561
        default:
            panic("non-exhaustive match")
        }
        jp540 = jp558
    case Let:
        var x240 string = e__104.(Let)._0
        var x241 Exp = e__104.(Let)._1
        var x242 Exp = e__104.(Let)._2
        var e2__124 Exp = x242
        var e1__123 Exp = x241
        var x__122 string = x240
        enter_level(st__102)
        var ty_e__125 Result__Typ__string = typeof(st__102, env__103, e1__123)
        leave_level(st__102)
        var jp563 Result__Typ__string
        switch ty_e__125.(type) {
        case Result__Typ__string_Ok:
            var x259 Typ = ty_e__125.(Result__Typ__string_Ok)._0
            var ty1__127 Typ = x259
            var t564 Typ = gen(st__102, ty1__127)
            var t565 EnvEntry = EnvEntry{
                name: x__122,
                ty: t564,
            }
            var env2__128 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__EnvEntry(env__103, t565)
            var t566 Result__Typ__string = typeof(st__102, env2__128, e2__124)
            jp563 = t566
        case Result__Typ__string_Err:
            var x260 string = ty_e__125.(Result__Typ__string_Err)._0
            var e__126 string = x260
            var t567 Result__Typ__string = Result__Typ__string_Err{
                _0: e__126,
            }
            jp563 = t567
        default:
            panic("non-exhaustive match")
        }
        jp540 = jp563
    default:
        panic("non-exhaustive match")
    }
    retv538 = jp540
    return retv538
}

func exp_var(name__129 string) Exp {
    var retv569 Exp
    var t570 Exp = Var{
        _0: name__129,
    }
    retv569 = t570
    return retv569
}

func exp_lam(name__130 string, body__131 Exp) Exp {
    var retv572 Exp
    var t573 Exp = Lam{
        _0: name__130,
        _1: body__131,
    }
    retv572 = t573
    return retv572
}

func exp_app(a__132 Exp, b__133 Exp) Exp {
    var retv575 Exp
    var t576 Exp = App{
        _0: a__132,
        _1: b__133,
    }
    retv575 = t576
    return retv575
}

func exp_let(name__134 string, a__135 Exp, b__136 Exp) Exp {
    var retv578 Exp
    var t579 Exp = Let{
        _0: name__134,
        _1: a__135,
        _2: b__136,
    }
    retv578 = t579
    return retv578
}

func show_result(label__137 string, res__138 Result__Typ__string) struct{} {
    switch res__138.(type) {
    case Result__Typ__string_Ok:
        var x261 Typ = res__138.(Result__Typ__string_Ok)._0
        var ty__139 Typ = x261
        var t582 string = label__137 + ": "
        var t583 string = typ_to_string(ty__139)
        var t584 string = t582 + t583
        println__T_string(t584)
    case Result__Typ__string_Err:
        var x262 string = res__138.(Result__Typ__string_Err)._0
        var e__140 string = x262
        var t586 string = label__137 + ": "
        var t587 string = t586 + e__140
        println__T_string(t587)
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func main0() struct{} {
    var st__141 CheckerState = state_new()
    var t590 Exp = exp_var("x")
    var id__142 Exp = exp_lam("x", t590)
    var t591 Exp = exp_var("x")
    var t592 Exp = exp_var("y")
    var t593 Exp = exp_app(t591, t592)
    var t594 Exp = exp_lam("y", t593)
    var c1__143 Exp = exp_lam("x", t594)
    reset_type_variables(st__141)
    var t595 *_goml_vec_EnvEntry = env_empty()
    var t596 Result__Typ__string = typeof(st__141, t595, id__142)
    show_result("id", t596)
    reset_type_variables(st__141)
    var t597 *_goml_vec_EnvEntry = env_empty()
    var t598 Result__Typ__string = typeof(st__141, t597, c1__143)
    show_result("c1", t598)
    reset_type_variables(st__141)
    var t599 *_goml_vec_EnvEntry = env_empty()
    var t600 Exp = exp_var("x")
    var t601 Exp = exp_let("x", c1__143, t600)
    var t602 Result__Typ__string = typeof(st__141, t599, t601)
    show_result("let_x_c1_x", t602)
    reset_type_variables(st__141)
    var t603 *_goml_vec_EnvEntry = env_empty()
    var t604 Exp = exp_var("z")
    var t605 Exp = exp_lam("z", t604)
    var t606 Exp = exp_var("y")
    var t607 Exp = exp_let("y", t605, t606)
    var t608 Result__Typ__string = typeof(st__141, t603, t607)
    show_result("let_y_id_y", t608)
    reset_type_variables(st__141)
    var t609 *_goml_vec_EnvEntry = env_empty()
    var t610 Exp = exp_var("z")
    var t611 Exp = exp_lam("z", t610)
    var t612 Exp = exp_var("y")
    var t613 Exp = exp_let("y", t611, t612)
    var t614 Exp = exp_lam("x", t613)
    var t615 Result__Typ__string = typeof(st__141, t609, t614)
    show_result("lam_x_let_y_id_y", t615)
    reset_type_variables(st__141)
    var t616 *_goml_vec_EnvEntry = env_empty()
    var t617 Exp = exp_var("z")
    var t618 Exp = exp_lam("z", t617)
    var t619 Exp = exp_var("y")
    var t620 Exp = exp_var("x")
    var t621 Exp = exp_app(t619, t620)
    var t622 Exp = exp_let("y", t618, t621)
    var t623 Exp = exp_lam("x", t622)
    var t624 Result__Typ__string = typeof(st__141, t616, t623)
    show_result("lam_x_let_y_id_yx", t624)
    reset_type_variables(st__141)
    var t625 *_goml_vec_EnvEntry = env_empty()
    var t626 Exp = exp_var("x")
    var t627 Exp = exp_var("x")
    var t628 Exp = exp_app(t626, t627)
    var t629 Exp = exp_lam("x", t628)
    var t630 Result__Typ__string = typeof(st__141, t625, t629)
    show_result("self_apply", t630)
    reset_type_variables(st__141)
    var t631 *_goml_vec_EnvEntry = env_empty()
    var t632 Exp = exp_var("x")
    var t633 Exp = exp_var("x")
    var t634 Exp = exp_let("x", t632, t633)
    var t635 Result__Typ__string = typeof(st__141, t631, t634)
    show_result("unbound_var", t635)
    reset_type_variables(st__141)
    var t636 *_goml_vec_EnvEntry = env_empty()
    var t637 Exp = exp_var("y")
    var t638 Exp = exp_var("y")
    var t639 Exp = exp_var("z")
    var t640 Exp = exp_app(t638, t639)
    var t641 Exp = exp_lam("z", t640)
    var t642 Exp = exp_app(t637, t641)
    var t643 Exp = exp_lam("y", t642)
    var t644 Result__Typ__string = typeof(st__141, t636, t643)
    show_result("max_heiber", t644)
    reset_type_variables(st__141)
    var t645 *_goml_vec_EnvEntry = env_empty()
    var t646 Exp = exp_var("k")
    var t647 Exp = exp_var("k")
    var t648 Exp = exp_var("x")
    var t649 Exp = exp_app(t647, t648)
    var t650 Exp = exp_var("y")
    var t651 Exp = exp_app(t649, t650)
    var t652 Exp = exp_app(t646, t651)
    var t653 Exp = exp_var("k")
    var t654 Exp = exp_var("y")
    var t655 Exp = exp_app(t653, t654)
    var t656 Exp = exp_var("x")
    var t657 Exp = exp_app(t655, t656)
    var t658 Exp = exp_app(t652, t657)
    var t659 Exp = exp_lam("k", t658)
    var t660 Exp = exp_lam("y", t659)
    var t661 Exp = exp_lam("x", t660)
    var t662 Result__Typ__string = typeof(st__141, t645, t661)
    show_result("kirang", t662)
    reset_type_variables(st__141)
    var t663 *_goml_vec_EnvEntry = env_empty()
    var t664 Exp = exp_var("id")
    var t665 Exp = exp_var("id")
    var t666 Exp = exp_app(t664, t665)
    var t667 Exp = exp_let("id", id__142, t666)
    var t668 Result__Typ__string = typeof(st__141, t663, t667)
    show_result("let_id_idid", t668)
    reset_type_variables(st__141)
    var t669 *_goml_vec_EnvEntry = env_empty()
    var t670 Exp = exp_var("x")
    var t671 Exp = exp_app(t670, id__142)
    var t672 Exp = exp_var("z")
    var t673 Exp = exp_let("z", t671, t672)
    var t674 Exp = exp_var("y")
    var t675 Exp = exp_let("y", t673, t674)
    var t676 Exp = exp_let("x", c1__143, t675)
    var t677 Result__Typ__string = typeof(st__141, t669, t676)
    show_result("nested_lets", t677)
    reset_type_variables(st__141)
    var t678 *_goml_vec_EnvEntry = env_empty()
    var t679 Exp = exp_var("x")
    var t680 Exp = exp_var("y")
    var t681 Exp = exp_app(t679, t680)
    var t682 Exp = exp_var("y")
    var t683 Exp = exp_var("x")
    var t684 Exp = exp_app(t682, t683)
    var t685 Exp = exp_lam("x", t684)
    var t686 Exp = exp_let("x", t681, t685)
    var t687 Exp = exp_lam("y", t686)
    var t688 Exp = exp_lam("x", t687)
    var t689 Result__Typ__string = typeof(st__141, t678, t688)
    show_result("fun_x_fun_y_let_x_xy_fun_x_yx", t689)
    reset_type_variables(st__141)
    var t690 *_goml_vec_EnvEntry = env_empty()
    var t691 Exp = exp_var("x")
    var t692 Exp = exp_var("y")
    var t693 Exp = exp_let("y", t691, t692)
    var t694 Exp = exp_lam("x", t693)
    var t695 Result__Typ__string = typeof(st__141, t690, t694)
    show_result("sound_gen_1", t695)
    reset_type_variables(st__141)
    var t696 *_goml_vec_EnvEntry = env_empty()
    var t697 Exp = exp_var("x")
    var t698 Exp = exp_lam("z", t697)
    var t699 Exp = exp_var("y")
    var t700 Exp = exp_let("y", t698, t699)
    var t701 Exp = exp_lam("x", t700)
    var t702 Result__Typ__string = typeof(st__141, t696, t701)
    show_result("sound_gen_2", t702)
    reset_type_variables(st__141)
    var t703 *_goml_vec_EnvEntry = env_empty()
    var t704 Exp = exp_var("x")
    var t705 Exp = exp_var("z")
    var t706 Exp = exp_app(t704, t705)
    var t707 Exp = exp_lam("z", t706)
    var t708 Exp = exp_var("y")
    var t709 Exp = exp_let("y", t707, t708)
    var t710 Exp = exp_lam("x", t709)
    var t711 Result__Typ__string = typeof(st__141, t703, t710)
    show_result("sound_gen_3", t711)
    reset_type_variables(st__141)
    var t712 *_goml_vec_EnvEntry = env_empty()
    var t713 Exp = exp_var("x")
    var t714 Exp = exp_var("y")
    var t715 Exp = exp_app(t713, t714)
    var t716 Exp = exp_var("x")
    var t717 Exp = exp_var("y")
    var t718 Exp = exp_app(t716, t717)
    var t719 Exp = exp_let("x", t715, t718)
    var t720 Exp = exp_lam("y", t719)
    var t721 Exp = exp_lam("x", t720)
    var t722 Result__Typ__string = typeof(st__141, t712, t721)
    show_result("double_apply", t722)
    reset_type_variables(st__141)
    var t723 *_goml_vec_EnvEntry = env_empty()
    var t724 Exp = exp_var("x")
    var t725 Exp = exp_var("y")
    var t726 Exp = exp_var("y")
    var t727 Exp = exp_app(t725, t726)
    var t728 Exp = exp_let("y", t724, t727)
    var t729 Exp = exp_lam("x", t728)
    var t730 Result__Typ__string = typeof(st__141, t723, t729)
    show_result("sound_gen_occurs", t730)
    reset_gensym(st__141)
    var t731 *_goml_vec_EnvEntry = env_empty()
    var t732 Exp = exp_var("x")
    var t733 Exp = exp_app(t732, id__142)
    var t734 Exp = exp_var("z")
    var t735 Exp = exp_let("z", t733, t734)
    var t736 Exp = exp_var("y")
    var t737 Exp = exp_let("y", t735, t736)
    var t738 Exp = exp_lam("x", t737)
    var t739 Result__Typ__string = typeof(st__141, t731, t738)
    show_result("fun_x_let_y_let_z_x_id_z_y", t739)
    println__T_string("")
    println__T_string("All Done")
    println__T_string("")
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__207 int32) *ref_int32_x {
    var retv741 *ref_int32_x
    var t742 *ref_int32_x = ref__Ref_5int32(value__207)
    retv741 = t742
    return retv741
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__209 *ref_int32_x, value__210 int32) struct{} {
    ref_set__Ref_5int32(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__208 *ref_int32_x) int32 {
    var retv746 int32
    var t747 int32 = ref_get__Ref_5int32(self__208)
    retv746 = t747
    return retv746
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__7 rune) string {
    var retv749 string
    var t750 string = _goml_runtime_core_char_to_string(self__7)
    retv749 = t750
    return retv749
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv752 string
    var t753 string = _goml_runtime_core_int32_to_string(self__6)
    retv752 = t753
    return retv752
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Tv(value__207 Tv) *ref_Tv_x {
    var retv755 *ref_Tv_x
    var t756 *ref_Tv_x = ref__Ref_2Tv(value__207)
    retv755 = t756
    return retv755
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(self__208 *ref_Tv_x) Tv {
    var retv758 Tv
    var t759 Tv = ref_get__Ref_2Tv(self__208)
    retv758 = t759
    return retv758
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__EnvEntry() *_goml_vec_EnvEntry {
    var retv761 *_goml_vec_EnvEntry
    var t762 *_goml_vec_EnvEntry = vec_new__Vec_8EnvEntry()
    retv761 = t762
    return retv761
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__EnvEntry(self__137 *_goml_vec_EnvEntry) int {
    var retv764 int
    var t765 int = vec_len__Vec_8EnvEntry(self__137)
    retv764 = t765
    return retv764
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv767 *ref_int_x
    var t768 *ref_int_x = ref__Ref_3int(value__207)
    retv767 = t768
    return retv767
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_Typ_r_(value__207 Option__Typ) *ref_Option__Typ_x {
    var retv770 *ref_Option__Typ_x
    var t771 *ref_Option__Typ_x = ref__Ref_11Option__Typ(value__207)
    retv770 = t771
    return retv770
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__207 bool) *ref_bool_x {
    var retv773 *ref_bool_x
    var t774 *ref_bool_x = ref__Ref_4bool(value__207)
    retv773 = t774
    return retv773
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__208 *ref_bool_x) bool {
    var retv776 bool
    var t777 bool = ref_get__Ref_4bool(self__208)
    retv776 = t777
    return retv776
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv779 int
    var t780 int = ref_get__Ref_3int(self__208)
    retv779 = t780
    return retv779
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__55 string, other__56 string) bool {
    var retv782 bool
    var t783 bool = self__55 == other__56
    retv782 = t783
    return retv782
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
    var retv791 Option__Typ
    var t792 Option__Typ = ref_get__Ref_11Option__Typ(self__208)
    retv791 = t792
    return retv791
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SubstEntry(self__137 *_goml_vec_SubstEntry) int {
    var retv794 int
    var t795 int = vec_len__Vec_10SubstEntry(self__137)
    retv794 = t795
    return retv794
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(self__209 *ref_Tv_x, value__210 Tv) struct{} {
    ref_set__Ref_2Tv(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SubstEntry(self__128 *_goml_vec_SubstEntry, elem__129 SubstEntry) *_goml_vec_SubstEntry {
    var retv799 *_goml_vec_SubstEntry
    var result__130 *_goml_vec_SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SubstEntry()
    var index__131 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop801:
    for {
        var t802 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
        var t803 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SubstEntry(self__128)
        var t804 bool = t802 < t803
        if t804 {
            var t805 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
            var t806 SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__SubstEntry(self__128, t805)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SubstEntry(result__130, t806)
            var t807 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
            var t808 int = t807 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__131, t808)
            continue
        } else {
            break Loop_loop801
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SubstEntry(result__130, elem__129)
    retv799 = result__130
    return retv799
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SubstEntry() *_goml_vec_SubstEntry {
    var retv810 *_goml_vec_SubstEntry
    var t811 *_goml_vec_SubstEntry = vec_new__Vec_10SubstEntry()
    retv810 = t811
    return retv810
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__EnvEntry(self__128 *_goml_vec_EnvEntry, elem__129 EnvEntry) *_goml_vec_EnvEntry {
    var retv813 *_goml_vec_EnvEntry
    var result__130 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__EnvEntry()
    var index__131 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop815:
    for {
        var t816 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
        var t817 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__EnvEntry(self__128)
        var t818 bool = t816 < t817
        if t818 {
            var t819 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
            var t820 EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__EnvEntry(self__128, t819)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__EnvEntry(result__130, t820)
            var t821 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
            var t822 int = t821 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__131, t822)
            continue
        } else {
            break Loop_loop815
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__EnvEntry(result__130, elem__129)
    retv813 = result__130
    return retv813
}

func println__T_string(value__1 string) struct{} {
    var t824 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t824)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SubstEntry(self__126 *_goml_vec_SubstEntry, elem__127 SubstEntry) struct{} {
    vec_push__Vec_10SubstEntry(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__SubstEntry(self__132 *_goml_vec_SubstEntry, index__133 int) SubstEntry {
    var retv829 SubstEntry
    var t830 SubstEntry = vec_get__Vec_10SubstEntry(self__132, index__133)
    retv829 = t830
    return retv829
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__EnvEntry(self__126 *_goml_vec_EnvEntry, elem__127 EnvEntry) struct{} {
    vec_push__Vec_8EnvEntry(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__EnvEntry(self__132 *_goml_vec_EnvEntry, index__133 int) EnvEntry {
    var retv834 EnvEntry
    var t835 EnvEntry = vec_get__Vec_8EnvEntry(self__132, index__133)
    retv834 = t835
    return retv834
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv837 string
    retv837 = self__38
    return retv837
}

func main() {
    main0()
}
