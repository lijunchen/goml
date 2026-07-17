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

func vec_get__Vec_8EnvEntry(vec *_goml_vec_EnvEntry, index int32) EnvEntry {
    return vec.items[index]
}

func vec_len__Vec_8EnvEntry(vec *_goml_vec_EnvEntry) int32 {
    return int32(len(vec.items))
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

func vec_get__Vec_10SubstEntry(vec *_goml_vec_SubstEntry, index int32) SubstEntry {
    return vec.items[index]
}

func vec_len__Vec_10SubstEntry(vec *_goml_vec_SubstEntry) int32 {
    return int32(len(vec.items))
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
    var retv266 CheckerState
    var t267 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var t268 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(1)
    var t269 CheckerState = CheckerState{
        gensym_counter: t267,
        current_level: t268,
    }
    retv266 = t269
    return retv266
}

func reset_gensym(st__0 CheckerState) struct{} {
    var t271 *ref_int32_x = st__0.gensym_counter
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t271, 0)
    return struct{}{}
}

func reset_level(st__1 CheckerState) struct{} {
    var t273 *ref_int32_x = st__1.current_level
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t273, 1)
    return struct{}{}
}

func reset_type_variables(st__2 CheckerState) struct{} {
    reset_gensym(st__2)
    reset_level(st__2)
    return struct{}{}
}

func enter_level(st__3 CheckerState) struct{} {
    var t277 *ref_int32_x = st__3.current_level
    var l__4 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t277)
    var t278 *ref_int32_x = st__3.current_level
    var t279 int32 = l__4 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t278, t279)
    return struct{}{}
}

func leave_level(st__5 CheckerState) struct{} {
    var t281 *ref_int32_x = st__5.current_level
    var l__6 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t281)
    var t282 *ref_int32_x = st__5.current_level
    var t283 int32 = l__6 - 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t282, t283)
    return struct{}{}
}

func min_i32(a__7 int32, b__8 int32) int32 {
    var retv285 int32
    var t288 bool = a__7 < b__8
    var jp287 int32
    if t288 {
        jp287 = a__7
    } else {
        jp287 = b__8
    }
    retv285 = jp287
    return retv285
}

func nth_letter(n__9 int32) rune {
    var retv290 rune
    var jp292 rune
    switch n__9 {
    case 0:
        jp292 = 97
    case 1:
        jp292 = 98
    case 2:
        jp292 = 99
    case 3:
        jp292 = 100
    case 4:
        jp292 = 101
    case 5:
        jp292 = 102
    case 6:
        jp292 = 103
    case 7:
        jp292 = 104
    case 8:
        jp292 = 105
    case 9:
        jp292 = 106
    case 10:
        jp292 = 107
    case 11:
        jp292 = 108
    case 12:
        jp292 = 109
    case 13:
        jp292 = 110
    case 14:
        jp292 = 111
    case 15:
        jp292 = 112
    case 16:
        jp292 = 113
    case 17:
        jp292 = 114
    case 18:
        jp292 = 115
    case 19:
        jp292 = 116
    case 20:
        jp292 = 117
    case 21:
        jp292 = 118
    case 22:
        jp292 = 119
    case 23:
        jp292 = 120
    case 24:
        jp292 = 121
    case 25:
        jp292 = 122
    default:
        jp292 = 97
    }
    retv290 = jp292
    return retv290
}

func gensym(st__10 CheckerState) string {
    var retv294 string
    var t295 *ref_int32_x = st__10.gensym_counter
    var n__11 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t295)
    var t296 *ref_int32_x = st__10.gensym_counter
    var t297 int32 = n__11 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t296, t297)
    var t300 bool = n__11 < 26
    var jp299 string
    if t300 {
        var t301 rune = nth_letter(n__11)
        var t302 string = _goml_m_inherent_i_char_i_char_i_to__string(t301)
        jp299 = t302
    } else {
        var t303 string = _goml_m_inherent_i_int32_i_int32_i_to__string(n__11)
        var t304 string = "t" + t303
        jp299 = t304
    }
    retv294 = jp299
    return retv294
}

func newvar(st__12 CheckerState) Typ {
    var retv306 Typ
    var name__13 string = gensym(st__12)
    var t307 *ref_int32_x = st__12.current_level
    var level__14 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t307)
    var t308 Tv = Unbound{
        _0: name__13,
        _1: level__14,
    }
    var t309 *ref_Tv_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Tv(t308)
    var t310 Typ = TVar{
        _0: t309,
    }
    retv306 = t310
    return retv306
}

func typ_is_arrow(ty__15 Typ) bool {
    var retv312 bool
    var jp314 bool
    switch ty__15.(type) {
    case TVar:
        var x67 *ref_Tv_x = ty__15.(TVar)._0
        var tvref__16 *ref_Tv_x = x67
        var mtmp71 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvref__16)
        var jp316 bool
        switch mtmp71.(type) {
        case Unbound:
            jp316 = false
        case Link:
            var x74 Typ = mtmp71.(Link)._0
            var inner__17 Typ = x74
            var t317 bool = typ_is_arrow(inner__17)
            jp316 = t317
        default:
            panic("non-exhaustive match")
        }
        jp314 = jp316
    case QVar:
        jp314 = false
    case TArrow:
        jp314 = true
    default:
        panic("non-exhaustive match")
    }
    retv312 = jp314
    return retv312
}

func typ_to_string(ty__18 Typ) string {
    var retv319 string
    var jp321 string
    switch ty__18.(type) {
    case TVar:
        var x75 *ref_Tv_x = ty__18.(TVar)._0
        var tvref__20 *ref_Tv_x = x75
        var mtmp79 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvref__20)
        var jp323 string
        switch mtmp79.(type) {
        case Unbound:
            var x80 string = mtmp79.(Unbound)._0
            var name__21 string = x80
            var t324 string = "'" + name__21
            jp323 = t324
        case Link:
            var x82 Typ = mtmp79.(Link)._0
            var inner__22 Typ = x82
            var t325 string = typ_to_string(inner__22)
            jp323 = t325
        default:
            panic("non-exhaustive match")
        }
        jp321 = jp323
    case QVar:
        var x76 string = ty__18.(QVar)._0
        var name__19 string = x76
        var t326 string = "'" + name__19
        jp321 = t326
    case TArrow:
        var x77 Typ = ty__18.(TArrow)._0
        var x78 Typ = ty__18.(TArrow)._1
        var t2__24 Typ = x78
        var t1__23 Typ = x77
        var t331 bool = typ_is_arrow(t1__23)
        var jp328 string
        if t331 {
            var t332 string = typ_to_string(t1__23)
            var t333 string = "(" + t332
            var t334 string = t333 + ")"
            jp328 = t334
        } else {
            var t335 string = typ_to_string(t1__23)
            jp328 = t335
        }
        var s1__25 string = jp328
        var s2__26 string = typ_to_string(t2__24)
        var t329 string = s1__25 + " -> "
        var t330 string = t329 + s2__26
        jp321 = t330
    default:
        panic("non-exhaustive match")
    }
    retv319 = jp321
    return retv319
}

func env_empty() *_goml_vec_EnvEntry {
    var retv337 *_goml_vec_EnvEntry
    var env__27 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__EnvEntry()
    retv337 = env__27
    return retv337
}

func env_lookup(env__28 *_goml_vec_EnvEntry, name__29 string) Option__Typ {
    var retv339 Option__Typ
    var t340 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__EnvEntry(env__28)
    var t341 int32 = t340 - 1
    var i__30 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(t341)
    var found__31 *ref_Option__Typ_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_Typ_r_(None{})
    var done__32 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    Loop_loop344:
    for {
        var t357 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(done__32)
        var t358 bool = !t357
        var jp346 bool
        if t358 {
            var t359 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__30)
            var t360 bool = t359 >= 0
            jp346 = t360
        } else {
            jp346 = false
        }
        if jp346 {
            var t347 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__30)
            var entry__33 EnvEntry = vec_get__Vec_8EnvEntry(env__28, t347)
            var t349 string = entry__33.name
            var t350 bool = t349 == name__29
            if t350 {
                var t351 Typ = entry__33.ty
                var t352 Option__Typ = Some{
                    _0: t351,
                }
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Option_l_Typ_r_(found__31, t352)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(done__32, true)
            } else {
                var t354 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__30)
                var t355 int32 = t354 - 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__30, t355)
            }
            continue
        } else {
            break Loop_loop344
        }
    }
    var t343 Option__Typ = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Option_l_Typ_r_(found__31)
    retv339 = t343
    return retv339
}

func subst_lookup(subst__34 *_goml_vec_SubstEntry, name__35 string) Option__Typ {
    var retv362 Option__Typ
    var t363 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SubstEntry(subst__34)
    var t364 int32 = t363 - 1
    var i__36 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(t364)
    var found__37 *ref_Option__Typ_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_Typ_r_(None{})
    var done__38 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    Loop_loop367:
    for {
        var t380 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(done__38)
        var t381 bool = !t380
        var jp369 bool
        if t381 {
            var t382 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__36)
            var t383 bool = t382 >= 0
            jp369 = t383
        } else {
            jp369 = false
        }
        if jp369 {
            var t370 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__36)
            var entry__39 SubstEntry = vec_get__Vec_10SubstEntry(subst__34, t370)
            var t372 string = entry__39.name
            var t373 bool = t372 == name__35
            if t373 {
                var t374 Typ = entry__39.ty
                var t375 Option__Typ = Some{
                    _0: t374,
                }
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Option_l_Typ_r_(found__37, t375)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(done__38, true)
            } else {
                var t377 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__36)
                var t378 int32 = t377 - 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__36, t378)
            }
            continue
        } else {
            break Loop_loop367
        }
    }
    var t366 Option__Typ = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Option_l_Typ_r_(found__37)
    retv362 = t366
    return retv362
}

func occurs(st__40 CheckerState, tvr__41 *ref_Tv_x, ty__42 Typ) Result__unit__string {
    var retv385 Result__unit__string
    var jp387 Result__unit__string
    switch ty__42.(type) {
    case TVar:
        var x87 *ref_Tv_x = ty__42.(TVar)._0
        var tvr2__43 *ref_Tv_x = x87
        var t390 bool = ptr_eq__Ref_2Tv(tvr__41, tvr2__43)
        var jp389 Result__unit__string
        if t390 {
            var t391 Result__unit__string = Result__unit__string_Err{
                _0: "occurs check",
            }
            jp389 = t391
        } else {
            var mtmp91 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvr2__43)
            var jp393 Result__unit__string
            switch mtmp91.(type) {
            case Unbound:
                var x92 string = mtmp91.(Unbound)._0
                var x93 int32 = mtmp91.(Unbound)._1
                var l2__45 int32 = x93
                var name__44 string = x92
                var mtmp95 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvr__41)
                var jp395 int32
                switch mtmp95.(type) {
                case Unbound:
                    var x97 int32 = mtmp95.(Unbound)._1
                    var l__46 int32 = x97
                    var t398 int32 = min_i32(l__46, l2__45)
                    jp395 = t398
                case Link:
                    jp395 = l2__45
                default:
                    panic("non-exhaustive match")
                }
                var min_level__47 int32 = jp395
                var t396 Tv = Unbound{
                    _0: name__44,
                    _1: min_level__47,
                }
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(tvr2__43, t396)
                var t397 Result__unit__string = Result__unit__string_Ok{
                    _0: struct{}{},
                }
                jp393 = t397
            case Link:
                var x94 Typ = mtmp91.(Link)._0
                var inner__48 Typ = x94
                var t399 Result__unit__string = occurs(st__40, tvr__41, inner__48)
                jp393 = t399
            default:
                panic("non-exhaustive match")
            }
            jp389 = jp393
        }
        jp387 = jp389
    case QVar:
        var t400 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp387 = t400
    case TArrow:
        var x89 Typ = ty__42.(TArrow)._0
        var x90 Typ = ty__42.(TArrow)._1
        var t2__50 Typ = x90
        var t1__49 Typ = x89
        var mtmp100 Result__unit__string = occurs(st__40, tvr__41, t1__49)
        var jp402 Result__unit__string
        switch mtmp100.(type) {
        case Result__unit__string_Ok:
            var t403 Result__unit__string = occurs(st__40, tvr__41, t2__50)
            jp402 = t403
        case Result__unit__string_Err:
            var x102 string = mtmp100.(Result__unit__string_Err)._0
            var e__51 string = x102
            var t404 Result__unit__string = Result__unit__string_Err{
                _0: e__51,
            }
            jp402 = t404
        default:
            panic("non-exhaustive match")
        }
        jp387 = jp402
    default:
        panic("non-exhaustive match")
    }
    retv385 = jp387
    return retv385
}

func unify(st__52 CheckerState, t1__53 Typ, t2__54 Typ) Result__unit__string {
    var retv406 Result__unit__string
    var mtmp103 Tuple2_3Typ_3Typ = Tuple2_3Typ_3Typ{
        _0: t1__53,
        _1: t2__54,
    }
    var x104 Typ = mtmp103._0
    var x105 Typ = mtmp103._1
    var jp408 Result__unit__string
    switch x105.(type) {
    case TVar:
        var x106 *ref_Tv_x = x105.(TVar)._0
        var jp410 Result__unit__string
        switch x104.(type) {
        case TVar:
            var x110 *ref_Tv_x = x104.(TVar)._0
            var r1__55 *ref_Tv_x = x110
            var r2__56 *ref_Tv_x = x106
            var t413 bool = ptr_eq__Ref_2Tv(r1__55, r2__56)
            var jp412 Result__unit__string
            if t413 {
                var t414 Result__unit__string = Result__unit__string_Ok{
                    _0: struct{}{},
                }
                jp412 = t414
            } else {
                var mtmp114 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r1__55)
                var jp416 Result__unit__string
                switch mtmp114.(type) {
                case Unbound:
                    var mtmp118 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r2__56)
                    var jp418 Result__unit__string
                    switch mtmp118.(type) {
                    case Unbound:
                        var t419 Typ = TVar{
                            _0: r2__56,
                        }
                        var mtmp122 Result__unit__string = occurs(st__52, r1__55, t419)
                        var jp421 Result__unit__string
                        switch mtmp122.(type) {
                        case Result__unit__string_Ok:
                            var t422 Typ = TVar{
                                _0: r2__56,
                            }
                            var t423 Tv = Link{
                                _0: t422,
                            }
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(r1__55, t423)
                            var t424 Result__unit__string = Result__unit__string_Ok{
                                _0: struct{}{},
                            }
                            jp421 = t424
                        case Result__unit__string_Err:
                            var x124 string = mtmp122.(Result__unit__string_Err)._0
                            var e__59 string = x124
                            var t425 Result__unit__string = Result__unit__string_Err{
                                _0: e__59,
                            }
                            jp421 = t425
                        default:
                            panic("non-exhaustive match")
                        }
                        jp418 = jp421
                    case Link:
                        var x121 Typ = mtmp118.(Link)._0
                        var inner__58 Typ = x121
                        var t426 Typ = TVar{
                            _0: r1__55,
                        }
                        var t427 Result__unit__string = unify(st__52, t426, inner__58)
                        jp418 = t427
                    default:
                        panic("non-exhaustive match")
                    }
                    jp416 = jp418
                case Link:
                    var x117 Typ = mtmp114.(Link)._0
                    var inner__57 Typ = x117
                    var t428 Typ = TVar{
                        _0: r2__56,
                    }
                    var t429 Result__unit__string = unify(st__52, inner__57, t428)
                    jp416 = t429
                default:
                    panic("non-exhaustive match")
                }
                jp412 = jp416
            }
            jp410 = jp412
        case QVar:
            var r2__65 *ref_Tv_x = x106
            var other__64 Typ = x104
            var mtmp126 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r2__65)
            var jp431 Result__unit__string
            switch mtmp126.(type) {
            case Unbound:
                var mtmp130 Result__unit__string = occurs(st__52, r2__65, other__64)
                var jp433 Result__unit__string
                switch mtmp130.(type) {
                case Result__unit__string_Ok:
                    var t434 Tv = Link{
                        _0: other__64,
                    }
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(r2__65, t434)
                    var t435 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    jp433 = t435
                case Result__unit__string_Err:
                    var x132 string = mtmp130.(Result__unit__string_Err)._0
                    var e__67 string = x132
                    var t436 Result__unit__string = Result__unit__string_Err{
                        _0: e__67,
                    }
                    jp433 = t436
                default:
                    panic("non-exhaustive match")
                }
                jp431 = jp433
            case Link:
                var x129 Typ = mtmp126.(Link)._0
                var inner__66 Typ = x129
                var t437 Result__unit__string = unify(st__52, other__64, inner__66)
                jp431 = t437
            default:
                panic("non-exhaustive match")
            }
            jp410 = jp431
        case TArrow:
            var r2__65 *ref_Tv_x = x106
            var other__64 Typ = x104
            var mtmp134 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r2__65)
            var jp439 Result__unit__string
            switch mtmp134.(type) {
            case Unbound:
                var mtmp138 Result__unit__string = occurs(st__52, r2__65, other__64)
                var jp441 Result__unit__string
                switch mtmp138.(type) {
                case Result__unit__string_Ok:
                    var t442 Tv = Link{
                        _0: other__64,
                    }
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(r2__65, t442)
                    var t443 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    jp441 = t443
                case Result__unit__string_Err:
                    var x140 string = mtmp138.(Result__unit__string_Err)._0
                    var e__67 string = x140
                    var t444 Result__unit__string = Result__unit__string_Err{
                        _0: e__67,
                    }
                    jp441 = t444
                default:
                    panic("non-exhaustive match")
                }
                jp439 = jp441
            case Link:
                var x137 Typ = mtmp134.(Link)._0
                var inner__66 Typ = x137
                var t445 Result__unit__string = unify(st__52, other__64, inner__66)
                jp439 = t445
            default:
                panic("non-exhaustive match")
            }
            jp410 = jp439
        default:
            panic("non-exhaustive match")
        }
        jp408 = jp410
    case QVar:
        var jp447 Result__unit__string
        switch x104.(type) {
        case TVar:
            var x142 *ref_Tv_x = x104.(TVar)._0
            var r1__60 *ref_Tv_x = x142
            var other__61 Typ = x105
            var mtmp146 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r1__60)
            var jp449 Result__unit__string
            switch mtmp146.(type) {
            case Unbound:
                var mtmp150 Result__unit__string = occurs(st__52, r1__60, other__61)
                var jp451 Result__unit__string
                switch mtmp150.(type) {
                case Result__unit__string_Ok:
                    var t452 Tv = Link{
                        _0: other__61,
                    }
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(r1__60, t452)
                    var t453 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    jp451 = t453
                case Result__unit__string_Err:
                    var x152 string = mtmp150.(Result__unit__string_Err)._0
                    var e__63 string = x152
                    var t454 Result__unit__string = Result__unit__string_Err{
                        _0: e__63,
                    }
                    jp451 = t454
                default:
                    panic("non-exhaustive match")
                }
                jp449 = jp451
            case Link:
                var x149 Typ = mtmp146.(Link)._0
                var inner__62 Typ = x149
                var t455 Result__unit__string = unify(st__52, inner__62, other__61)
                jp449 = t455
            default:
                panic("non-exhaustive match")
            }
            jp447 = jp449
        case QVar:
            var t456 Result__unit__string = Result__unit__string_Err{
                _0: "unify error",
            }
            jp447 = t456
        case TArrow:
            var t457 Result__unit__string = Result__unit__string_Err{
                _0: "unify error",
            }
            jp447 = t457
        default:
            panic("non-exhaustive match")
        }
        jp408 = jp447
    case TArrow:
        var x108 Typ = x105.(TArrow)._0
        var x109 Typ = x105.(TArrow)._1
        var jp459 Result__unit__string
        switch x104.(type) {
        case TVar:
            var x154 *ref_Tv_x = x104.(TVar)._0
            var r1__60 *ref_Tv_x = x154
            var other__61 Typ = x105
            var mtmp158 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r1__60)
            var jp461 Result__unit__string
            switch mtmp158.(type) {
            case Unbound:
                var mtmp162 Result__unit__string = occurs(st__52, r1__60, other__61)
                var jp463 Result__unit__string
                switch mtmp162.(type) {
                case Result__unit__string_Ok:
                    var t464 Tv = Link{
                        _0: other__61,
                    }
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(r1__60, t464)
                    var t465 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    jp463 = t465
                case Result__unit__string_Err:
                    var x164 string = mtmp162.(Result__unit__string_Err)._0
                    var e__63 string = x164
                    var t466 Result__unit__string = Result__unit__string_Err{
                        _0: e__63,
                    }
                    jp463 = t466
                default:
                    panic("non-exhaustive match")
                }
                jp461 = jp463
            case Link:
                var x161 Typ = mtmp158.(Link)._0
                var inner__62 Typ = x161
                var t467 Result__unit__string = unify(st__52, inner__62, other__61)
                jp461 = t467
            default:
                panic("non-exhaustive match")
            }
            jp459 = jp461
        case QVar:
            var t468 Result__unit__string = Result__unit__string_Err{
                _0: "unify error",
            }
            jp459 = t468
        case TArrow:
            var x156 Typ = x104.(TArrow)._0
            var x157 Typ = x104.(TArrow)._1
            var a2__69 Typ = x157
            var a1__68 Typ = x156
            var b2__71 Typ = x109
            var b1__70 Typ = x108
            var mtmp166 Result__unit__string = unify(st__52, a1__68, b1__70)
            var jp470 Result__unit__string
            switch mtmp166.(type) {
            case Result__unit__string_Ok:
                var t471 Result__unit__string = unify(st__52, a2__69, b2__71)
                jp470 = t471
            case Result__unit__string_Err:
                var x168 string = mtmp166.(Result__unit__string_Err)._0
                var e__72 string = x168
                var t472 Result__unit__string = Result__unit__string_Err{
                    _0: e__72,
                }
                jp470 = t472
            default:
                panic("non-exhaustive match")
            }
            jp459 = jp470
        default:
            panic("non-exhaustive match")
        }
        jp408 = jp459
    default:
        panic("non-exhaustive match")
    }
    retv406 = jp408
    return retv406
}

func gen(st__73 CheckerState, ty__74 Typ) Typ {
    var retv474 Typ
    var jp476 Typ
    switch ty__74.(type) {
    case TVar:
        var x169 *ref_Tv_x = ty__74.(TVar)._0
        var tvref__75 *ref_Tv_x = x169
        var mtmp173 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvref__75)
        var jp478 Typ
        switch mtmp173.(type) {
        case Unbound:
            var x174 string = mtmp173.(Unbound)._0
            var x175 int32 = mtmp173.(Unbound)._1
            var l__77 int32 = x175
            var name__76 string = x174
            var t479 *ref_int32_x = st__73.current_level
            var cur__78 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t479)
            var t482 bool = l__77 > cur__78
            var jp481 Typ
            if t482 {
                var t483 Typ = QVar{
                    _0: name__76,
                }
                jp481 = t483
            } else {
                var t484 Typ = TVar{
                    _0: tvref__75,
                }
                jp481 = t484
            }
            jp478 = jp481
        case Link:
            var x176 Typ = mtmp173.(Link)._0
            var inner__79 Typ = x176
            var t485 Typ = gen(st__73, inner__79)
            jp478 = t485
        default:
            panic("non-exhaustive match")
        }
        jp476 = jp478
    case QVar:
        var other__82 Typ = ty__74
        jp476 = other__82
    case TArrow:
        var x171 Typ = ty__74.(TArrow)._0
        var x172 Typ = ty__74.(TArrow)._1
        var t2__81 Typ = x172
        var t1__80 Typ = x171
        var t486 Typ = gen(st__73, t1__80)
        var t487 Typ = gen(st__73, t2__81)
        var t488 Typ = TArrow{
            _0: t486,
            _1: t487,
        }
        jp476 = t488
    default:
        panic("non-exhaustive match")
    }
    retv474 = jp476
    return retv474
}

func inst_loop(st__83 CheckerState, subst__84 *_goml_vec_SubstEntry, ty__85 Typ) Tuple2_3Typ_16Vec_10SubstEntry {
    var retv490 Tuple2_3Typ_16Vec_10SubstEntry
    var jp492 Tuple2_3Typ_16Vec_10SubstEntry
    switch ty__85.(type) {
    case TVar:
        var x177 *ref_Tv_x = ty__85.(TVar)._0
        var tvref__90 *ref_Tv_x = x177
        var mtmp181 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvref__90)
        var jp494 Tuple2_3Typ_16Vec_10SubstEntry
        switch mtmp181.(type) {
        case Unbound:
            var t495 Typ = TVar{
                _0: tvref__90,
            }
            var t496 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: t495,
                _1: subst__84,
            }
            jp494 = t496
        case Link:
            var x184 Typ = mtmp181.(Link)._0
            var inner__91 Typ = x184
            var t497 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, subst__84, inner__91)
            jp494 = t497
        default:
            panic("non-exhaustive match")
        }
        jp492 = jp494
    case QVar:
        var x178 string = ty__85.(QVar)._0
        var name__86 string = x178
        var mtmp185 Option__Typ = subst_lookup(subst__84, name__86)
        var jp499 Tuple2_3Typ_16Vec_10SubstEntry
        switch mtmp185.(type) {
        case None:
            var tv__88 Typ = newvar(st__83)
            var t500 SubstEntry = SubstEntry{
                name: name__86,
                ty: tv__88,
            }
            var new_subst__89 *_goml_vec_SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SubstEntry(subst__84, t500)
            var t501 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: tv__88,
                _1: new_subst__89,
            }
            jp499 = t501
        case Some:
            var x186 Typ = mtmp185.(Some)._0
            var t__87 Typ = x186
            var t502 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: t__87,
                _1: subst__84,
            }
            jp499 = t502
        default:
            panic("non-exhaustive match")
        }
        jp492 = jp499
    case TArrow:
        var x179 Typ = ty__85.(TArrow)._0
        var x180 Typ = ty__85.(TArrow)._1
        var t2__93 Typ = x180
        var t1__92 Typ = x179
        var mtmp187 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, subst__84, t1__92)
        var x188 Typ = mtmp187._0
        var x189 *_goml_vec_SubstEntry = mtmp187._1
        var subst1__95 *_goml_vec_SubstEntry = x189
        var ty1__94 Typ = x188
        var mtmp190 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, subst1__95, t2__93)
        var x191 Typ = mtmp190._0
        var x192 *_goml_vec_SubstEntry = mtmp190._1
        var subst2__97 *_goml_vec_SubstEntry = x192
        var ty2__96 Typ = x191
        var t503 Typ = TArrow{
            _0: ty1__94,
            _1: ty2__96,
        }
        var t504 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
            _0: t503,
            _1: subst2__97,
        }
        jp492 = t504
    default:
        panic("non-exhaustive match")
    }
    retv490 = jp492
    return retv490
}

func inst(st__98 CheckerState, ty__99 Typ) Typ {
    var retv506 Typ
    var subst0__100 *_goml_vec_SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SubstEntry()
    var mtmp193 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__98, subst0__100, ty__99)
    var x194 Typ = mtmp193._0
    var t__101 Typ = x194
    retv506 = t__101
    return retv506
}

func typeof(st__102 CheckerState, env__103 *_goml_vec_EnvEntry, e__104 Exp) Result__Typ__string {
    var retv508 Result__Typ__string
    var jp510 Result__Typ__string
    switch e__104.(type) {
    case Var:
        var x196 string = e__104.(Var)._0
        var x__105 string = x196
        var mtmp204 Option__Typ = env_lookup(env__103, x__105)
        var jp512 Result__Typ__string
        switch mtmp204.(type) {
        case None:
            var t513 Result__Typ__string = Result__Typ__string_Err{
                _0: "unbound var",
            }
            jp512 = t513
        case Some:
            var x205 Typ = mtmp204.(Some)._0
            var ty__106 Typ = x205
            var t514 Typ = inst(st__102, ty__106)
            var t515 Result__Typ__string = Result__Typ__string_Ok{
                _0: t514,
            }
            jp512 = t515
        default:
            panic("non-exhaustive match")
        }
        jp510 = jp512
    case App:
        var x197 Exp = e__104.(App)._0
        var x198 Exp = e__104.(App)._1
        var e2__114 Exp = x198
        var e1__113 Exp = x197
        var mtmp206 Result__Typ__string = typeof(st__102, env__103, e1__113)
        var jp517 Result__Typ__string
        switch mtmp206.(type) {
        case Result__Typ__string_Ok:
            var x207 Typ = mtmp206.(Result__Typ__string_Ok)._0
            var ty_fun__116 Typ = x207
            var mtmp209 Result__Typ__string = typeof(st__102, env__103, e2__114)
            var jp519 Result__Typ__string
            switch mtmp209.(type) {
            case Result__Typ__string_Ok:
                var x210 Typ = mtmp209.(Result__Typ__string_Ok)._0
                var ty_arg__118 Typ = x210
                var ty_res__119 Typ = newvar(st__102)
                var arrow__120 Typ = TArrow{
                    _0: ty_arg__118,
                    _1: ty_res__119,
                }
                var mtmp212 Result__unit__string = unify(st__102, ty_fun__116, arrow__120)
                var jp521 Result__Typ__string
                switch mtmp212.(type) {
                case Result__unit__string_Ok:
                    var t522 Result__Typ__string = Result__Typ__string_Ok{
                        _0: ty_res__119,
                    }
                    jp521 = t522
                case Result__unit__string_Err:
                    var x214 string = mtmp212.(Result__unit__string_Err)._0
                    var e__121 string = x214
                    var t523 Result__Typ__string = Result__Typ__string_Err{
                        _0: e__121,
                    }
                    jp521 = t523
                default:
                    panic("non-exhaustive match")
                }
                jp519 = jp521
            case Result__Typ__string_Err:
                var x211 string = mtmp209.(Result__Typ__string_Err)._0
                var e__117 string = x211
                var t524 Result__Typ__string = Result__Typ__string_Err{
                    _0: e__117,
                }
                jp519 = t524
            default:
                panic("non-exhaustive match")
            }
            jp517 = jp519
        case Result__Typ__string_Err:
            var x208 string = mtmp206.(Result__Typ__string_Err)._0
            var e__115 string = x208
            var t525 Result__Typ__string = Result__Typ__string_Err{
                _0: e__115,
            }
            jp517 = t525
        default:
            panic("non-exhaustive match")
        }
        jp510 = jp517
    case Lam:
        var x199 string = e__104.(Lam)._0
        var x200 Exp = e__104.(Lam)._1
        var body__108 Exp = x200
        var x__107 string = x199
        var ty_x__109 Typ = newvar(st__102)
        var t526 EnvEntry = EnvEntry{
            name: x__107,
            ty: ty_x__109,
        }
        var env2__110 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__EnvEntry(env__103, t526)
        var mtmp215 Result__Typ__string = typeof(st__102, env2__110, body__108)
        var jp528 Result__Typ__string
        switch mtmp215.(type) {
        case Result__Typ__string_Ok:
            var x216 Typ = mtmp215.(Result__Typ__string_Ok)._0
            var ty_e__111 Typ = x216
            var t529 Typ = TArrow{
                _0: ty_x__109,
                _1: ty_e__111,
            }
            var t530 Result__Typ__string = Result__Typ__string_Ok{
                _0: t529,
            }
            jp528 = t530
        case Result__Typ__string_Err:
            var x217 string = mtmp215.(Result__Typ__string_Err)._0
            var e__112 string = x217
            var t531 Result__Typ__string = Result__Typ__string_Err{
                _0: e__112,
            }
            jp528 = t531
        default:
            panic("non-exhaustive match")
        }
        jp510 = jp528
    case Let:
        var x201 string = e__104.(Let)._0
        var x202 Exp = e__104.(Let)._1
        var x203 Exp = e__104.(Let)._2
        var e2__124 Exp = x203
        var e1__123 Exp = x202
        var x__122 string = x201
        enter_level(st__102)
        var ty_e__125 Result__Typ__string = typeof(st__102, env__103, e1__123)
        leave_level(st__102)
        var jp533 Result__Typ__string
        switch ty_e__125.(type) {
        case Result__Typ__string_Ok:
            var x220 Typ = ty_e__125.(Result__Typ__string_Ok)._0
            var ty1__127 Typ = x220
            var t534 Typ = gen(st__102, ty1__127)
            var t535 EnvEntry = EnvEntry{
                name: x__122,
                ty: t534,
            }
            var env2__128 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__EnvEntry(env__103, t535)
            var t536 Result__Typ__string = typeof(st__102, env2__128, e2__124)
            jp533 = t536
        case Result__Typ__string_Err:
            var x221 string = ty_e__125.(Result__Typ__string_Err)._0
            var e__126 string = x221
            var t537 Result__Typ__string = Result__Typ__string_Err{
                _0: e__126,
            }
            jp533 = t537
        default:
            panic("non-exhaustive match")
        }
        jp510 = jp533
    default:
        panic("non-exhaustive match")
    }
    retv508 = jp510
    return retv508
}

func exp_var(name__129 string) Exp {
    var retv539 Exp
    var t540 Exp = Var{
        _0: name__129,
    }
    retv539 = t540
    return retv539
}

func exp_lam(name__130 string, body__131 Exp) Exp {
    var retv542 Exp
    var t543 Exp = Lam{
        _0: name__130,
        _1: body__131,
    }
    retv542 = t543
    return retv542
}

func exp_app(a__132 Exp, b__133 Exp) Exp {
    var retv545 Exp
    var t546 Exp = App{
        _0: a__132,
        _1: b__133,
    }
    retv545 = t546
    return retv545
}

func exp_let(name__134 string, a__135 Exp, b__136 Exp) Exp {
    var retv548 Exp
    var t549 Exp = Let{
        _0: name__134,
        _1: a__135,
        _2: b__136,
    }
    retv548 = t549
    return retv548
}

func show_result(label__137 string, res__138 Result__Typ__string) struct{} {
    switch res__138.(type) {
    case Result__Typ__string_Ok:
        var x222 Typ = res__138.(Result__Typ__string_Ok)._0
        var ty__139 Typ = x222
        var t552 string = label__137 + ": "
        var t553 string = typ_to_string(ty__139)
        var t554 string = t552 + t553
        println__T_string(t554)
    case Result__Typ__string_Err:
        var x223 string = res__138.(Result__Typ__string_Err)._0
        var e__140 string = x223
        var t556 string = label__137 + ": "
        var t557 string = t556 + e__140
        println__T_string(t557)
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func main0() struct{} {
    var st__141 CheckerState = state_new()
    var t560 Exp = exp_var("x")
    var id__142 Exp = exp_lam("x", t560)
    var t561 Exp = exp_var("x")
    var t562 Exp = exp_var("y")
    var t563 Exp = exp_app(t561, t562)
    var t564 Exp = exp_lam("y", t563)
    var c1__143 Exp = exp_lam("x", t564)
    reset_type_variables(st__141)
    var t565 *_goml_vec_EnvEntry = env_empty()
    var t566 Result__Typ__string = typeof(st__141, t565, id__142)
    show_result("id", t566)
    reset_type_variables(st__141)
    var t567 *_goml_vec_EnvEntry = env_empty()
    var t568 Result__Typ__string = typeof(st__141, t567, c1__143)
    show_result("c1", t568)
    reset_type_variables(st__141)
    var t569 *_goml_vec_EnvEntry = env_empty()
    var t570 Exp = exp_var("x")
    var t571 Exp = exp_let("x", c1__143, t570)
    var t572 Result__Typ__string = typeof(st__141, t569, t571)
    show_result("let_x_c1_x", t572)
    reset_type_variables(st__141)
    var t573 *_goml_vec_EnvEntry = env_empty()
    var t574 Exp = exp_var("z")
    var t575 Exp = exp_lam("z", t574)
    var t576 Exp = exp_var("y")
    var t577 Exp = exp_let("y", t575, t576)
    var t578 Result__Typ__string = typeof(st__141, t573, t577)
    show_result("let_y_id_y", t578)
    reset_type_variables(st__141)
    var t579 *_goml_vec_EnvEntry = env_empty()
    var t580 Exp = exp_var("z")
    var t581 Exp = exp_lam("z", t580)
    var t582 Exp = exp_var("y")
    var t583 Exp = exp_let("y", t581, t582)
    var t584 Exp = exp_lam("x", t583)
    var t585 Result__Typ__string = typeof(st__141, t579, t584)
    show_result("lam_x_let_y_id_y", t585)
    reset_type_variables(st__141)
    var t586 *_goml_vec_EnvEntry = env_empty()
    var t587 Exp = exp_var("z")
    var t588 Exp = exp_lam("z", t587)
    var t589 Exp = exp_var("y")
    var t590 Exp = exp_var("x")
    var t591 Exp = exp_app(t589, t590)
    var t592 Exp = exp_let("y", t588, t591)
    var t593 Exp = exp_lam("x", t592)
    var t594 Result__Typ__string = typeof(st__141, t586, t593)
    show_result("lam_x_let_y_id_yx", t594)
    reset_type_variables(st__141)
    var t595 *_goml_vec_EnvEntry = env_empty()
    var t596 Exp = exp_var("x")
    var t597 Exp = exp_var("x")
    var t598 Exp = exp_app(t596, t597)
    var t599 Exp = exp_lam("x", t598)
    var t600 Result__Typ__string = typeof(st__141, t595, t599)
    show_result("self_apply", t600)
    reset_type_variables(st__141)
    var t601 *_goml_vec_EnvEntry = env_empty()
    var t602 Exp = exp_var("x")
    var t603 Exp = exp_var("x")
    var t604 Exp = exp_let("x", t602, t603)
    var t605 Result__Typ__string = typeof(st__141, t601, t604)
    show_result("unbound_var", t605)
    reset_type_variables(st__141)
    var t606 *_goml_vec_EnvEntry = env_empty()
    var t607 Exp = exp_var("y")
    var t608 Exp = exp_var("y")
    var t609 Exp = exp_var("z")
    var t610 Exp = exp_app(t608, t609)
    var t611 Exp = exp_lam("z", t610)
    var t612 Exp = exp_app(t607, t611)
    var t613 Exp = exp_lam("y", t612)
    var t614 Result__Typ__string = typeof(st__141, t606, t613)
    show_result("max_heiber", t614)
    reset_type_variables(st__141)
    var t615 *_goml_vec_EnvEntry = env_empty()
    var t616 Exp = exp_var("k")
    var t617 Exp = exp_var("k")
    var t618 Exp = exp_var("x")
    var t619 Exp = exp_app(t617, t618)
    var t620 Exp = exp_var("y")
    var t621 Exp = exp_app(t619, t620)
    var t622 Exp = exp_app(t616, t621)
    var t623 Exp = exp_var("k")
    var t624 Exp = exp_var("y")
    var t625 Exp = exp_app(t623, t624)
    var t626 Exp = exp_var("x")
    var t627 Exp = exp_app(t625, t626)
    var t628 Exp = exp_app(t622, t627)
    var t629 Exp = exp_lam("k", t628)
    var t630 Exp = exp_lam("y", t629)
    var t631 Exp = exp_lam("x", t630)
    var t632 Result__Typ__string = typeof(st__141, t615, t631)
    show_result("kirang", t632)
    reset_type_variables(st__141)
    var t633 *_goml_vec_EnvEntry = env_empty()
    var t634 Exp = exp_var("id")
    var t635 Exp = exp_var("id")
    var t636 Exp = exp_app(t634, t635)
    var t637 Exp = exp_let("id", id__142, t636)
    var t638 Result__Typ__string = typeof(st__141, t633, t637)
    show_result("let_id_idid", t638)
    reset_type_variables(st__141)
    var t639 *_goml_vec_EnvEntry = env_empty()
    var t640 Exp = exp_var("x")
    var t641 Exp = exp_app(t640, id__142)
    var t642 Exp = exp_var("z")
    var t643 Exp = exp_let("z", t641, t642)
    var t644 Exp = exp_var("y")
    var t645 Exp = exp_let("y", t643, t644)
    var t646 Exp = exp_let("x", c1__143, t645)
    var t647 Result__Typ__string = typeof(st__141, t639, t646)
    show_result("nested_lets", t647)
    reset_type_variables(st__141)
    var t648 *_goml_vec_EnvEntry = env_empty()
    var t649 Exp = exp_var("x")
    var t650 Exp = exp_var("y")
    var t651 Exp = exp_app(t649, t650)
    var t652 Exp = exp_var("y")
    var t653 Exp = exp_var("x")
    var t654 Exp = exp_app(t652, t653)
    var t655 Exp = exp_lam("x", t654)
    var t656 Exp = exp_let("x", t651, t655)
    var t657 Exp = exp_lam("y", t656)
    var t658 Exp = exp_lam("x", t657)
    var t659 Result__Typ__string = typeof(st__141, t648, t658)
    show_result("fun_x_fun_y_let_x_xy_fun_x_yx", t659)
    reset_type_variables(st__141)
    var t660 *_goml_vec_EnvEntry = env_empty()
    var t661 Exp = exp_var("x")
    var t662 Exp = exp_var("y")
    var t663 Exp = exp_let("y", t661, t662)
    var t664 Exp = exp_lam("x", t663)
    var t665 Result__Typ__string = typeof(st__141, t660, t664)
    show_result("sound_gen_1", t665)
    reset_type_variables(st__141)
    var t666 *_goml_vec_EnvEntry = env_empty()
    var t667 Exp = exp_var("x")
    var t668 Exp = exp_lam("z", t667)
    var t669 Exp = exp_var("y")
    var t670 Exp = exp_let("y", t668, t669)
    var t671 Exp = exp_lam("x", t670)
    var t672 Result__Typ__string = typeof(st__141, t666, t671)
    show_result("sound_gen_2", t672)
    reset_type_variables(st__141)
    var t673 *_goml_vec_EnvEntry = env_empty()
    var t674 Exp = exp_var("x")
    var t675 Exp = exp_var("z")
    var t676 Exp = exp_app(t674, t675)
    var t677 Exp = exp_lam("z", t676)
    var t678 Exp = exp_var("y")
    var t679 Exp = exp_let("y", t677, t678)
    var t680 Exp = exp_lam("x", t679)
    var t681 Result__Typ__string = typeof(st__141, t673, t680)
    show_result("sound_gen_3", t681)
    reset_type_variables(st__141)
    var t682 *_goml_vec_EnvEntry = env_empty()
    var t683 Exp = exp_var("x")
    var t684 Exp = exp_var("y")
    var t685 Exp = exp_app(t683, t684)
    var t686 Exp = exp_var("x")
    var t687 Exp = exp_var("y")
    var t688 Exp = exp_app(t686, t687)
    var t689 Exp = exp_let("x", t685, t688)
    var t690 Exp = exp_lam("y", t689)
    var t691 Exp = exp_lam("x", t690)
    var t692 Result__Typ__string = typeof(st__141, t682, t691)
    show_result("double_apply", t692)
    reset_type_variables(st__141)
    var t693 *_goml_vec_EnvEntry = env_empty()
    var t694 Exp = exp_var("x")
    var t695 Exp = exp_var("y")
    var t696 Exp = exp_var("y")
    var t697 Exp = exp_app(t695, t696)
    var t698 Exp = exp_let("y", t694, t697)
    var t699 Exp = exp_lam("x", t698)
    var t700 Result__Typ__string = typeof(st__141, t693, t699)
    show_result("sound_gen_occurs", t700)
    reset_gensym(st__141)
    var t701 *_goml_vec_EnvEntry = env_empty()
    var t702 Exp = exp_var("x")
    var t703 Exp = exp_app(t702, id__142)
    var t704 Exp = exp_var("z")
    var t705 Exp = exp_let("z", t703, t704)
    var t706 Exp = exp_var("y")
    var t707 Exp = exp_let("y", t705, t706)
    var t708 Exp = exp_lam("x", t707)
    var t709 Result__Typ__string = typeof(st__141, t701, t708)
    show_result("fun_x_let_y_let_z_x_id_z_y", t709)
    println__T_string("")
    println__T_string("All Done")
    println__T_string("")
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__204 int32) *ref_int32_x {
    var retv711 *ref_int32_x
    var t712 *ref_int32_x = ref__Ref_5int32(value__204)
    retv711 = t712
    return retv711
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__206 *ref_int32_x, value__207 int32) struct{} {
    ref_set__Ref_5int32(self__206, value__207)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__205 *ref_int32_x) int32 {
    var retv716 int32
    var t717 int32 = ref_get__Ref_5int32(self__205)
    retv716 = t717
    return retv716
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__6 rune) string {
    var retv719 string
    var t720 string = _goml_runtime_core_char_to_string(self__6)
    retv719 = t720
    return retv719
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv722 string
    var t723 string = _goml_runtime_core_int32_to_string(self__5)
    retv722 = t723
    return retv722
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Tv(value__204 Tv) *ref_Tv_x {
    var retv725 *ref_Tv_x
    var t726 *ref_Tv_x = ref__Ref_2Tv(value__204)
    retv725 = t726
    return retv725
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(self__205 *ref_Tv_x) Tv {
    var retv728 Tv
    var t729 Tv = ref_get__Ref_2Tv(self__205)
    retv728 = t729
    return retv728
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__EnvEntry() *_goml_vec_EnvEntry {
    var retv731 *_goml_vec_EnvEntry
    var t732 *_goml_vec_EnvEntry = vec_new__Vec_8EnvEntry()
    retv731 = t732
    return retv731
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__EnvEntry(self__134 *_goml_vec_EnvEntry) int32 {
    var retv734 int32
    var t735 int32 = vec_len__Vec_8EnvEntry(self__134)
    retv734 = t735
    return retv734
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_Typ_r_(value__204 Option__Typ) *ref_Option__Typ_x {
    var retv737 *ref_Option__Typ_x
    var t738 *ref_Option__Typ_x = ref__Ref_11Option__Typ(value__204)
    retv737 = t738
    return retv737
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__204 bool) *ref_bool_x {
    var retv740 *ref_bool_x
    var t741 *ref_bool_x = ref__Ref_4bool(value__204)
    retv740 = t741
    return retv740
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__205 *ref_bool_x) bool {
    var retv743 bool
    var t744 bool = ref_get__Ref_4bool(self__205)
    retv743 = t744
    return retv743
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Option_l_Typ_r_(self__206 *ref_Option__Typ_x, value__207 Option__Typ) struct{} {
    ref_set__Ref_11Option__Typ(self__206, value__207)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(self__206 *ref_bool_x, value__207 bool) struct{} {
    ref_set__Ref_4bool(self__206, value__207)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Option_l_Typ_r_(self__205 *ref_Option__Typ_x) Option__Typ {
    var retv750 Option__Typ
    var t751 Option__Typ = ref_get__Ref_11Option__Typ(self__205)
    retv750 = t751
    return retv750
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SubstEntry(self__134 *_goml_vec_SubstEntry) int32 {
    var retv753 int32
    var t754 int32 = vec_len__Vec_10SubstEntry(self__134)
    retv753 = t754
    return retv753
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(self__206 *ref_Tv_x, value__207 Tv) struct{} {
    ref_set__Ref_2Tv(self__206, value__207)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SubstEntry(self__125 *_goml_vec_SubstEntry, elem__126 SubstEntry) *_goml_vec_SubstEntry {
    var retv758 *_goml_vec_SubstEntry
    var result__127 *_goml_vec_SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SubstEntry()
    var index__128 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop760:
    for {
        var t761 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
        var t762 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SubstEntry(self__125)
        var t763 bool = t761 < t762
        if t763 {
            var t764 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
            var t765 SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__SubstEntry(self__125, t764)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SubstEntry(result__127, t765)
            var t766 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
            var t767 int32 = t766 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__128, t767)
            continue
        } else {
            break Loop_loop760
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SubstEntry(result__127, elem__126)
    retv758 = result__127
    return retv758
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SubstEntry() *_goml_vec_SubstEntry {
    var retv769 *_goml_vec_SubstEntry
    var t770 *_goml_vec_SubstEntry = vec_new__Vec_10SubstEntry()
    retv769 = t770
    return retv769
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__EnvEntry(self__125 *_goml_vec_EnvEntry, elem__126 EnvEntry) *_goml_vec_EnvEntry {
    var retv772 *_goml_vec_EnvEntry
    var result__127 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__EnvEntry()
    var index__128 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop774:
    for {
        var t775 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
        var t776 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__EnvEntry(self__125)
        var t777 bool = t775 < t776
        if t777 {
            var t778 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
            var t779 EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__EnvEntry(self__125, t778)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__EnvEntry(result__127, t779)
            var t780 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
            var t781 int32 = t780 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__128, t781)
            continue
        } else {
            break Loop_loop774
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__EnvEntry(result__127, elem__126)
    retv772 = result__127
    return retv772
}

func println__T_string(value__1 string) struct{} {
    var t783 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t783)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SubstEntry(self__123 *_goml_vec_SubstEntry, elem__124 SubstEntry) struct{} {
    vec_push__Vec_10SubstEntry(self__123, elem__124)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__SubstEntry(self__129 *_goml_vec_SubstEntry, index__130 int32) SubstEntry {
    var retv788 SubstEntry
    var t789 SubstEntry = vec_get__Vec_10SubstEntry(self__129, index__130)
    retv788 = t789
    return retv788
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__EnvEntry(self__123 *_goml_vec_EnvEntry, elem__124 EnvEntry) struct{} {
    vec_push__Vec_8EnvEntry(self__123, elem__124)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__EnvEntry(self__129 *_goml_vec_EnvEntry, index__130 int32) EnvEntry {
    var retv793 EnvEntry
    var t794 EnvEntry = vec_get__Vec_8EnvEntry(self__129, index__130)
    retv793 = t794
    return retv793
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv796 string
    retv796 = self__37
    return retv796
}

func main() {
    main0()
}
