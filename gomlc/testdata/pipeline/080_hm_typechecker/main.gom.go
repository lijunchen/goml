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
    var retv265 CheckerState
    var t266 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var t267 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(1)
    var t268 CheckerState = CheckerState{
        gensym_counter: t266,
        current_level: t267,
    }
    retv265 = t268
    return retv265
}

func reset_gensym(st__0 CheckerState) struct{} {
    var t270 *ref_int32_x = st__0.gensym_counter
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t270, 0)
    return struct{}{}
}

func reset_level(st__1 CheckerState) struct{} {
    var t272 *ref_int32_x = st__1.current_level
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t272, 1)
    return struct{}{}
}

func reset_type_variables(st__2 CheckerState) struct{} {
    reset_gensym(st__2)
    reset_level(st__2)
    return struct{}{}
}

func enter_level(st__3 CheckerState) struct{} {
    var t276 *ref_int32_x = st__3.current_level
    var l__4 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t276)
    var t277 *ref_int32_x = st__3.current_level
    var t278 int32 = l__4 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t277, t278)
    return struct{}{}
}

func leave_level(st__5 CheckerState) struct{} {
    var t280 *ref_int32_x = st__5.current_level
    var l__6 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t280)
    var t281 *ref_int32_x = st__5.current_level
    var t282 int32 = l__6 - 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t281, t282)
    return struct{}{}
}

func min_i32(a__7 int32, b__8 int32) int32 {
    var retv284 int32
    var t287 bool = a__7 < b__8
    var jp286 int32
    if t287 {
        jp286 = a__7
    } else {
        jp286 = b__8
    }
    retv284 = jp286
    return retv284
}

func nth_letter(n__9 int32) rune {
    var retv289 rune
    var jp291 rune
    switch n__9 {
    case 0:
        jp291 = 97
    case 1:
        jp291 = 98
    case 2:
        jp291 = 99
    case 3:
        jp291 = 100
    case 4:
        jp291 = 101
    case 5:
        jp291 = 102
    case 6:
        jp291 = 103
    case 7:
        jp291 = 104
    case 8:
        jp291 = 105
    case 9:
        jp291 = 106
    case 10:
        jp291 = 107
    case 11:
        jp291 = 108
    case 12:
        jp291 = 109
    case 13:
        jp291 = 110
    case 14:
        jp291 = 111
    case 15:
        jp291 = 112
    case 16:
        jp291 = 113
    case 17:
        jp291 = 114
    case 18:
        jp291 = 115
    case 19:
        jp291 = 116
    case 20:
        jp291 = 117
    case 21:
        jp291 = 118
    case 22:
        jp291 = 119
    case 23:
        jp291 = 120
    case 24:
        jp291 = 121
    case 25:
        jp291 = 122
    default:
        jp291 = 97
    }
    retv289 = jp291
    return retv289
}

func gensym(st__10 CheckerState) string {
    var retv293 string
    var t294 *ref_int32_x = st__10.gensym_counter
    var n__11 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t294)
    var t295 *ref_int32_x = st__10.gensym_counter
    var t296 int32 = n__11 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t295, t296)
    var t299 bool = n__11 < 26
    var jp298 string
    if t299 {
        var t300 rune = nth_letter(n__11)
        var t301 string = _goml_m_inherent_i_char_i_char_i_to__string(t300)
        jp298 = t301
    } else {
        var t302 string = _goml_m_inherent_i_int32_i_int32_i_to__string(n__11)
        var t303 string = "t" + t302
        jp298 = t303
    }
    retv293 = jp298
    return retv293
}

func newvar(st__12 CheckerState) Typ {
    var retv305 Typ
    var name__13 string = gensym(st__12)
    var t306 *ref_int32_x = st__12.current_level
    var level__14 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t306)
    var t307 Tv = Unbound{
        _0: name__13,
        _1: level__14,
    }
    var t308 *ref_Tv_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Tv(t307)
    var t309 Typ = TVar{
        _0: t308,
    }
    retv305 = t309
    return retv305
}

func typ_is_arrow(ty__15 Typ) bool {
    var retv311 bool
    var jp313 bool
    switch ty__15.(type) {
    case TVar:
        var x74 *ref_Tv_x = ty__15.(TVar)._0
        var tvref__16 *ref_Tv_x = x74
        var mtmp78 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvref__16)
        var jp315 bool
        switch mtmp78.(type) {
        case Link:
            var x81 Typ = mtmp78.(Link)._0
            var inner__17 Typ = x81
            var t316 bool = typ_is_arrow(inner__17)
            jp315 = t316
        default:
            jp315 = false
        }
        jp313 = jp315
    case TArrow:
        jp313 = true
    default:
        jp313 = false
    }
    retv311 = jp313
    return retv311
}

func typ_to_string(ty__18 Typ) string {
    var retv318 string
    var jp320 string
    switch ty__18.(type) {
    case TVar:
        var x82 *ref_Tv_x = ty__18.(TVar)._0
        var tvref__20 *ref_Tv_x = x82
        var mtmp86 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvref__20)
        var jp322 string
        switch mtmp86.(type) {
        case Unbound:
            var x87 string = mtmp86.(Unbound)._0
            var name__21 string = x87
            var t323 string = "'" + name__21
            jp322 = t323
        case Link:
            var x89 Typ = mtmp86.(Link)._0
            var inner__22 Typ = x89
            var t324 string = typ_to_string(inner__22)
            jp322 = t324
        default:
            panic("non-exhaustive match")
        }
        jp320 = jp322
    case QVar:
        var x83 string = ty__18.(QVar)._0
        var name__19 string = x83
        var t325 string = "'" + name__19
        jp320 = t325
    case TArrow:
        var x84 Typ = ty__18.(TArrow)._0
        var x85 Typ = ty__18.(TArrow)._1
        var t2__24 Typ = x85
        var t1__23 Typ = x84
        var t330 bool = typ_is_arrow(t1__23)
        var jp327 string
        if t330 {
            var t331 string = typ_to_string(t1__23)
            var t332 string = "(" + t331
            var t333 string = t332 + ")"
            jp327 = t333
        } else {
            var t334 string = typ_to_string(t1__23)
            jp327 = t334
        }
        var s1__25 string = jp327
        var s2__26 string = typ_to_string(t2__24)
        var t328 string = s1__25 + " -> "
        var t329 string = t328 + s2__26
        jp320 = t329
    default:
        panic("non-exhaustive match")
    }
    retv318 = jp320
    return retv318
}

func env_empty() *_goml_vec_EnvEntry {
    var retv336 *_goml_vec_EnvEntry
    var env__27 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__EnvEntry()
    retv336 = env__27
    return retv336
}

func env_lookup(env__28 *_goml_vec_EnvEntry, name__29 string) Option__Typ {
    var retv338 Option__Typ
    var t339 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__EnvEntry(env__28)
    var t340 int = t339 - 1
    var i__30 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(t340)
    var found__31 *ref_Option__Typ_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_Typ_r_(None{})
    var done__32 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    Loop_loop343:
    for {
        var t356 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(done__32)
        var t357 bool = !t356
        var jp345 bool
        if t357 {
            var t358 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__30)
            var t359 bool = t358 >= 0
            jp345 = t359
        } else {
            jp345 = false
        }
        if jp345 {
            var t346 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__30)
            var entry__33 EnvEntry = vec_get__Vec_8EnvEntry(env__28, t346)
            var t348 string = entry__33.name
            var t349 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(t348, name__29)
            if t349 {
                var t350 Typ = entry__33.ty
                var t351 Option__Typ = Some{
                    _0: t350,
                }
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Option_l_Typ_r_(found__31, t351)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(done__32, true)
            } else {
                var t353 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__30)
                var t354 int = t353 - 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__30, t354)
            }
            continue
        } else {
            break Loop_loop343
        }
    }
    var t342 Option__Typ = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Option_l_Typ_r_(found__31)
    retv338 = t342
    return retv338
}

func subst_lookup(subst__34 *_goml_vec_SubstEntry, name__35 string) Option__Typ {
    var retv361 Option__Typ
    var t362 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SubstEntry(subst__34)
    var t363 int = t362 - 1
    var i__36 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(t363)
    var found__37 *ref_Option__Typ_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_Typ_r_(None{})
    var done__38 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    Loop_loop366:
    for {
        var t379 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(done__38)
        var t380 bool = !t379
        var jp368 bool
        if t380 {
            var t381 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__36)
            var t382 bool = t381 >= 0
            jp368 = t382
        } else {
            jp368 = false
        }
        if jp368 {
            var t369 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__36)
            var entry__39 SubstEntry = vec_get__Vec_10SubstEntry(subst__34, t369)
            var t371 string = entry__39.name
            var t372 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(t371, name__35)
            if t372 {
                var t373 Typ = entry__39.ty
                var t374 Option__Typ = Some{
                    _0: t373,
                }
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Option_l_Typ_r_(found__37, t374)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(done__38, true)
            } else {
                var t376 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__36)
                var t377 int = t376 - 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__36, t377)
            }
            continue
        } else {
            break Loop_loop366
        }
    }
    var t365 Option__Typ = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Option_l_Typ_r_(found__37)
    retv361 = t365
    return retv361
}

func occurs(st__40 CheckerState, tvr__41 *ref_Tv_x, ty__42 Typ) Result__unit__string {
    var retv384 Result__unit__string
    var jp386 Result__unit__string
    switch ty__42.(type) {
    case TVar:
        var x94 *ref_Tv_x = ty__42.(TVar)._0
        var tvr2__43 *ref_Tv_x = x94
        var t389 bool = ptr_eq__Ref_2Tv(tvr__41, tvr2__43)
        var jp388 Result__unit__string
        if t389 {
            var t390 Result__unit__string = Result__unit__string_Err{
                _0: "occurs check",
            }
            jp388 = t390
        } else {
            var mtmp98 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvr2__43)
            var jp392 Result__unit__string
            switch mtmp98.(type) {
            case Unbound:
                var x99 string = mtmp98.(Unbound)._0
                var x100 int32 = mtmp98.(Unbound)._1
                var l2__45 int32 = x100
                var name__44 string = x99
                var mtmp102 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvr__41)
                var jp394 int32
                switch mtmp102.(type) {
                case Unbound:
                    var x104 int32 = mtmp102.(Unbound)._1
                    var l__46 int32 = x104
                    var t397 int32 = min_i32(l__46, l2__45)
                    jp394 = t397
                default:
                    jp394 = l2__45
                }
                var min_level__47 int32 = jp394
                var t395 Tv = Unbound{
                    _0: name__44,
                    _1: min_level__47,
                }
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(tvr2__43, t395)
                var t396 Result__unit__string = Result__unit__string_Ok{
                    _0: struct{}{},
                }
                jp392 = t396
            case Link:
                var x101 Typ = mtmp98.(Link)._0
                var inner__48 Typ = x101
                var t398 Result__unit__string = occurs(st__40, tvr__41, inner__48)
                jp392 = t398
            default:
                panic("non-exhaustive match")
            }
            jp388 = jp392
        }
        jp386 = jp388
    case TArrow:
        var x96 Typ = ty__42.(TArrow)._0
        var x97 Typ = ty__42.(TArrow)._1
        var t2__50 Typ = x97
        var t1__49 Typ = x96
        var mtmp107 Result__unit__string = occurs(st__40, tvr__41, t1__49)
        var jp400 Result__unit__string
        switch mtmp107.(type) {
        case Result__unit__string_Ok:
            var t401 Result__unit__string = occurs(st__40, tvr__41, t2__50)
            jp400 = t401
        case Result__unit__string_Err:
            var x109 string = mtmp107.(Result__unit__string_Err)._0
            var e__51 string = x109
            var t402 Result__unit__string = Result__unit__string_Err{
                _0: e__51,
            }
            jp400 = t402
        default:
            panic("non-exhaustive match")
        }
        jp386 = jp400
    default:
        var t403 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp386 = t403
    }
    retv384 = jp386
    return retv384
}

func unify(st__52 CheckerState, t1__53 Typ, t2__54 Typ) Result__unit__string {
    var retv405 Result__unit__string
    var mtmp110 Tuple2_3Typ_3Typ = Tuple2_3Typ_3Typ{
        _0: t1__53,
        _1: t2__54,
    }
    var x111 Typ = mtmp110._0
    var x112 Typ = mtmp110._1
    var jp407 Result__unit__string
    switch x112.(type) {
    case TVar:
        var x113 *ref_Tv_x = x112.(TVar)._0
        var jp409 Result__unit__string
        switch x111.(type) {
        case TVar:
            var x117 *ref_Tv_x = x111.(TVar)._0
            var r1__55 *ref_Tv_x = x117
            var r2__56 *ref_Tv_x = x113
            var t412 bool = ptr_eq__Ref_2Tv(r1__55, r2__56)
            var jp411 Result__unit__string
            if t412 {
                var t413 Result__unit__string = Result__unit__string_Ok{
                    _0: struct{}{},
                }
                jp411 = t413
            } else {
                var mtmp121 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r1__55)
                var jp415 Result__unit__string
                switch mtmp121.(type) {
                case Unbound:
                    var mtmp125 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r2__56)
                    var jp417 Result__unit__string
                    switch mtmp125.(type) {
                    case Unbound:
                        var t418 Typ = TVar{
                            _0: r2__56,
                        }
                        var mtmp129 Result__unit__string = occurs(st__52, r1__55, t418)
                        var jp420 Result__unit__string
                        switch mtmp129.(type) {
                        case Result__unit__string_Ok:
                            var t421 Typ = TVar{
                                _0: r2__56,
                            }
                            var t422 Tv = Link{
                                _0: t421,
                            }
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(r1__55, t422)
                            var t423 Result__unit__string = Result__unit__string_Ok{
                                _0: struct{}{},
                            }
                            jp420 = t423
                        case Result__unit__string_Err:
                            var x131 string = mtmp129.(Result__unit__string_Err)._0
                            var e__59 string = x131
                            var t424 Result__unit__string = Result__unit__string_Err{
                                _0: e__59,
                            }
                            jp420 = t424
                        default:
                            panic("non-exhaustive match")
                        }
                        jp417 = jp420
                    case Link:
                        var x128 Typ = mtmp125.(Link)._0
                        var inner__58 Typ = x128
                        var t425 Typ = TVar{
                            _0: r1__55,
                        }
                        var t426 Result__unit__string = unify(st__52, t425, inner__58)
                        jp417 = t426
                    default:
                        panic("non-exhaustive match")
                    }
                    jp415 = jp417
                case Link:
                    var x124 Typ = mtmp121.(Link)._0
                    var inner__57 Typ = x124
                    var t427 Typ = TVar{
                        _0: r2__56,
                    }
                    var t428 Result__unit__string = unify(st__52, inner__57, t427)
                    jp415 = t428
                default:
                    panic("non-exhaustive match")
                }
                jp411 = jp415
            }
            jp409 = jp411
        default:
            var r2__65 *ref_Tv_x = x113
            var other__64 Typ = x111
            var mtmp133 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r2__65)
            var jp430 Result__unit__string
            switch mtmp133.(type) {
            case Unbound:
                var mtmp137 Result__unit__string = occurs(st__52, r2__65, other__64)
                var jp432 Result__unit__string
                switch mtmp137.(type) {
                case Result__unit__string_Ok:
                    var t433 Tv = Link{
                        _0: other__64,
                    }
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(r2__65, t433)
                    var t434 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    jp432 = t434
                case Result__unit__string_Err:
                    var x139 string = mtmp137.(Result__unit__string_Err)._0
                    var e__67 string = x139
                    var t435 Result__unit__string = Result__unit__string_Err{
                        _0: e__67,
                    }
                    jp432 = t435
                default:
                    panic("non-exhaustive match")
                }
                jp430 = jp432
            case Link:
                var x136 Typ = mtmp133.(Link)._0
                var inner__66 Typ = x136
                var t436 Result__unit__string = unify(st__52, other__64, inner__66)
                jp430 = t436
            default:
                panic("non-exhaustive match")
            }
            jp409 = jp430
        }
        jp407 = jp409
    case TArrow:
        var x115 Typ = x112.(TArrow)._0
        var x116 Typ = x112.(TArrow)._1
        var jp438 Result__unit__string
        switch x111.(type) {
        case TVar:
            var x141 *ref_Tv_x = x111.(TVar)._0
            var r1__60 *ref_Tv_x = x141
            var other__61 Typ = x112
            var mtmp145 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r1__60)
            var jp440 Result__unit__string
            switch mtmp145.(type) {
            case Unbound:
                var mtmp149 Result__unit__string = occurs(st__52, r1__60, other__61)
                var jp442 Result__unit__string
                switch mtmp149.(type) {
                case Result__unit__string_Ok:
                    var t443 Tv = Link{
                        _0: other__61,
                    }
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(r1__60, t443)
                    var t444 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    jp442 = t444
                case Result__unit__string_Err:
                    var x151 string = mtmp149.(Result__unit__string_Err)._0
                    var e__63 string = x151
                    var t445 Result__unit__string = Result__unit__string_Err{
                        _0: e__63,
                    }
                    jp442 = t445
                default:
                    panic("non-exhaustive match")
                }
                jp440 = jp442
            case Link:
                var x148 Typ = mtmp145.(Link)._0
                var inner__62 Typ = x148
                var t446 Result__unit__string = unify(st__52, inner__62, other__61)
                jp440 = t446
            default:
                panic("non-exhaustive match")
            }
            jp438 = jp440
        case TArrow:
            var x143 Typ = x111.(TArrow)._0
            var x144 Typ = x111.(TArrow)._1
            var a2__69 Typ = x144
            var a1__68 Typ = x143
            var b2__71 Typ = x116
            var b1__70 Typ = x115
            var mtmp153 Result__unit__string = unify(st__52, a1__68, b1__70)
            var jp448 Result__unit__string
            switch mtmp153.(type) {
            case Result__unit__string_Ok:
                var t449 Result__unit__string = unify(st__52, a2__69, b2__71)
                jp448 = t449
            case Result__unit__string_Err:
                var x155 string = mtmp153.(Result__unit__string_Err)._0
                var e__72 string = x155
                var t450 Result__unit__string = Result__unit__string_Err{
                    _0: e__72,
                }
                jp448 = t450
            default:
                panic("non-exhaustive match")
            }
            jp438 = jp448
        default:
            var t451 Result__unit__string = Result__unit__string_Err{
                _0: "unify error",
            }
            jp438 = t451
        }
        jp407 = jp438
    default:
        var jp453 Result__unit__string
        switch x111.(type) {
        case TVar:
            var x156 *ref_Tv_x = x111.(TVar)._0
            var r1__60 *ref_Tv_x = x156
            var other__61 Typ = x112
            var mtmp160 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r1__60)
            var jp455 Result__unit__string
            switch mtmp160.(type) {
            case Unbound:
                var mtmp164 Result__unit__string = occurs(st__52, r1__60, other__61)
                var jp457 Result__unit__string
                switch mtmp164.(type) {
                case Result__unit__string_Ok:
                    var t458 Tv = Link{
                        _0: other__61,
                    }
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(r1__60, t458)
                    var t459 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    jp457 = t459
                case Result__unit__string_Err:
                    var x166 string = mtmp164.(Result__unit__string_Err)._0
                    var e__63 string = x166
                    var t460 Result__unit__string = Result__unit__string_Err{
                        _0: e__63,
                    }
                    jp457 = t460
                default:
                    panic("non-exhaustive match")
                }
                jp455 = jp457
            case Link:
                var x163 Typ = mtmp160.(Link)._0
                var inner__62 Typ = x163
                var t461 Result__unit__string = unify(st__52, inner__62, other__61)
                jp455 = t461
            default:
                panic("non-exhaustive match")
            }
            jp453 = jp455
        default:
            var t462 Result__unit__string = Result__unit__string_Err{
                _0: "unify error",
            }
            jp453 = t462
        }
        jp407 = jp453
    }
    retv405 = jp407
    return retv405
}

func gen(st__73 CheckerState, ty__74 Typ) Typ {
    var retv464 Typ
    var jp466 Typ
    switch ty__74.(type) {
    case TVar:
        var x168 *ref_Tv_x = ty__74.(TVar)._0
        var tvref__75 *ref_Tv_x = x168
        var mtmp172 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvref__75)
        var jp468 Typ
        switch mtmp172.(type) {
        case Unbound:
            var x173 string = mtmp172.(Unbound)._0
            var x174 int32 = mtmp172.(Unbound)._1
            var l__77 int32 = x174
            var name__76 string = x173
            var t469 *ref_int32_x = st__73.current_level
            var cur__78 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t469)
            var t472 bool = l__77 > cur__78
            var jp471 Typ
            if t472 {
                var t473 Typ = QVar{
                    _0: name__76,
                }
                jp471 = t473
            } else {
                var t474 Typ = TVar{
                    _0: tvref__75,
                }
                jp471 = t474
            }
            jp468 = jp471
        case Link:
            var x175 Typ = mtmp172.(Link)._0
            var inner__79 Typ = x175
            var t475 Typ = gen(st__73, inner__79)
            jp468 = t475
        default:
            panic("non-exhaustive match")
        }
        jp466 = jp468
    case TArrow:
        var x170 Typ = ty__74.(TArrow)._0
        var x171 Typ = ty__74.(TArrow)._1
        var t2__81 Typ = x171
        var t1__80 Typ = x170
        var t476 Typ = gen(st__73, t1__80)
        var t477 Typ = gen(st__73, t2__81)
        var t478 Typ = TArrow{
            _0: t476,
            _1: t477,
        }
        jp466 = t478
    default:
        var other__82 Typ = ty__74
        jp466 = other__82
    }
    retv464 = jp466
    return retv464
}

func inst_loop(st__83 CheckerState, subst__84 *_goml_vec_SubstEntry, ty__85 Typ) Tuple2_3Typ_16Vec_10SubstEntry {
    var retv480 Tuple2_3Typ_16Vec_10SubstEntry
    var jp482 Tuple2_3Typ_16Vec_10SubstEntry
    switch ty__85.(type) {
    case TVar:
        var x176 *ref_Tv_x = ty__85.(TVar)._0
        var tvref__90 *ref_Tv_x = x176
        var mtmp180 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvref__90)
        var jp484 Tuple2_3Typ_16Vec_10SubstEntry
        switch mtmp180.(type) {
        case Link:
            var x183 Typ = mtmp180.(Link)._0
            var inner__91 Typ = x183
            var t485 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, subst__84, inner__91)
            jp484 = t485
        default:
            var t486 Typ = TVar{
                _0: tvref__90,
            }
            var t487 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: t486,
                _1: subst__84,
            }
            jp484 = t487
        }
        jp482 = jp484
    case QVar:
        var x177 string = ty__85.(QVar)._0
        var name__86 string = x177
        var mtmp184 Option__Typ = subst_lookup(subst__84, name__86)
        var jp489 Tuple2_3Typ_16Vec_10SubstEntry
        switch mtmp184.(type) {
        case None:
            var tv__88 Typ = newvar(st__83)
            var t490 SubstEntry = SubstEntry{
                name: name__86,
                ty: tv__88,
            }
            var new_subst__89 *_goml_vec_SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SubstEntry(subst__84, t490)
            var t491 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: tv__88,
                _1: new_subst__89,
            }
            jp489 = t491
        case Some:
            var x185 Typ = mtmp184.(Some)._0
            var t__87 Typ = x185
            var t492 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: t__87,
                _1: subst__84,
            }
            jp489 = t492
        default:
            panic("non-exhaustive match")
        }
        jp482 = jp489
    case TArrow:
        var x178 Typ = ty__85.(TArrow)._0
        var x179 Typ = ty__85.(TArrow)._1
        var t2__93 Typ = x179
        var t1__92 Typ = x178
        var mtmp186 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, subst__84, t1__92)
        var x187 Typ = mtmp186._0
        var x188 *_goml_vec_SubstEntry = mtmp186._1
        var subst1__95 *_goml_vec_SubstEntry = x188
        var ty1__94 Typ = x187
        var mtmp189 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, subst1__95, t2__93)
        var x190 Typ = mtmp189._0
        var x191 *_goml_vec_SubstEntry = mtmp189._1
        var subst2__97 *_goml_vec_SubstEntry = x191
        var ty2__96 Typ = x190
        var t493 Typ = TArrow{
            _0: ty1__94,
            _1: ty2__96,
        }
        var t494 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
            _0: t493,
            _1: subst2__97,
        }
        jp482 = t494
    default:
        panic("non-exhaustive match")
    }
    retv480 = jp482
    return retv480
}

func inst(st__98 CheckerState, ty__99 Typ) Typ {
    var retv496 Typ
    var subst0__100 *_goml_vec_SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SubstEntry()
    var mtmp192 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__98, subst0__100, ty__99)
    var x193 Typ = mtmp192._0
    var t__101 Typ = x193
    retv496 = t__101
    return retv496
}

func typeof(st__102 CheckerState, env__103 *_goml_vec_EnvEntry, e__104 Exp) Result__Typ__string {
    var retv498 Result__Typ__string
    var jp500 Result__Typ__string
    switch e__104.(type) {
    case Var:
        var x195 string = e__104.(Var)._0
        var x__105 string = x195
        var mtmp203 Option__Typ = env_lookup(env__103, x__105)
        var jp502 Result__Typ__string
        switch mtmp203.(type) {
        case None:
            var t503 Result__Typ__string = Result__Typ__string_Err{
                _0: "unbound var",
            }
            jp502 = t503
        case Some:
            var x204 Typ = mtmp203.(Some)._0
            var ty__106 Typ = x204
            var t504 Typ = inst(st__102, ty__106)
            var t505 Result__Typ__string = Result__Typ__string_Ok{
                _0: t504,
            }
            jp502 = t505
        default:
            panic("non-exhaustive match")
        }
        jp500 = jp502
    case App:
        var x196 Exp = e__104.(App)._0
        var x197 Exp = e__104.(App)._1
        var e2__114 Exp = x197
        var e1__113 Exp = x196
        var mtmp205 Result__Typ__string = typeof(st__102, env__103, e1__113)
        var jp507 Result__Typ__string
        switch mtmp205.(type) {
        case Result__Typ__string_Ok:
            var x206 Typ = mtmp205.(Result__Typ__string_Ok)._0
            var ty_fun__116 Typ = x206
            var mtmp208 Result__Typ__string = typeof(st__102, env__103, e2__114)
            var jp509 Result__Typ__string
            switch mtmp208.(type) {
            case Result__Typ__string_Ok:
                var x209 Typ = mtmp208.(Result__Typ__string_Ok)._0
                var ty_arg__118 Typ = x209
                var ty_res__119 Typ = newvar(st__102)
                var arrow__120 Typ = TArrow{
                    _0: ty_arg__118,
                    _1: ty_res__119,
                }
                var mtmp211 Result__unit__string = unify(st__102, ty_fun__116, arrow__120)
                var jp511 Result__Typ__string
                switch mtmp211.(type) {
                case Result__unit__string_Ok:
                    var t512 Result__Typ__string = Result__Typ__string_Ok{
                        _0: ty_res__119,
                    }
                    jp511 = t512
                case Result__unit__string_Err:
                    var x213 string = mtmp211.(Result__unit__string_Err)._0
                    var e__121 string = x213
                    var t513 Result__Typ__string = Result__Typ__string_Err{
                        _0: e__121,
                    }
                    jp511 = t513
                default:
                    panic("non-exhaustive match")
                }
                jp509 = jp511
            case Result__Typ__string_Err:
                var x210 string = mtmp208.(Result__Typ__string_Err)._0
                var e__117 string = x210
                var t514 Result__Typ__string = Result__Typ__string_Err{
                    _0: e__117,
                }
                jp509 = t514
            default:
                panic("non-exhaustive match")
            }
            jp507 = jp509
        case Result__Typ__string_Err:
            var x207 string = mtmp205.(Result__Typ__string_Err)._0
            var e__115 string = x207
            var t515 Result__Typ__string = Result__Typ__string_Err{
                _0: e__115,
            }
            jp507 = t515
        default:
            panic("non-exhaustive match")
        }
        jp500 = jp507
    case Lam:
        var x198 string = e__104.(Lam)._0
        var x199 Exp = e__104.(Lam)._1
        var body__108 Exp = x199
        var x__107 string = x198
        var ty_x__109 Typ = newvar(st__102)
        var t516 EnvEntry = EnvEntry{
            name: x__107,
            ty: ty_x__109,
        }
        var env2__110 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__EnvEntry(env__103, t516)
        var mtmp214 Result__Typ__string = typeof(st__102, env2__110, body__108)
        var jp518 Result__Typ__string
        switch mtmp214.(type) {
        case Result__Typ__string_Ok:
            var x215 Typ = mtmp214.(Result__Typ__string_Ok)._0
            var ty_e__111 Typ = x215
            var t519 Typ = TArrow{
                _0: ty_x__109,
                _1: ty_e__111,
            }
            var t520 Result__Typ__string = Result__Typ__string_Ok{
                _0: t519,
            }
            jp518 = t520
        case Result__Typ__string_Err:
            var x216 string = mtmp214.(Result__Typ__string_Err)._0
            var e__112 string = x216
            var t521 Result__Typ__string = Result__Typ__string_Err{
                _0: e__112,
            }
            jp518 = t521
        default:
            panic("non-exhaustive match")
        }
        jp500 = jp518
    case Let:
        var x200 string = e__104.(Let)._0
        var x201 Exp = e__104.(Let)._1
        var x202 Exp = e__104.(Let)._2
        var e2__124 Exp = x202
        var e1__123 Exp = x201
        var x__122 string = x200
        enter_level(st__102)
        var ty_e__125 Result__Typ__string = typeof(st__102, env__103, e1__123)
        leave_level(st__102)
        var jp523 Result__Typ__string
        switch ty_e__125.(type) {
        case Result__Typ__string_Ok:
            var x219 Typ = ty_e__125.(Result__Typ__string_Ok)._0
            var ty1__127 Typ = x219
            var t524 Typ = gen(st__102, ty1__127)
            var t525 EnvEntry = EnvEntry{
                name: x__122,
                ty: t524,
            }
            var env2__128 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__EnvEntry(env__103, t525)
            var t526 Result__Typ__string = typeof(st__102, env2__128, e2__124)
            jp523 = t526
        case Result__Typ__string_Err:
            var x220 string = ty_e__125.(Result__Typ__string_Err)._0
            var e__126 string = x220
            var t527 Result__Typ__string = Result__Typ__string_Err{
                _0: e__126,
            }
            jp523 = t527
        default:
            panic("non-exhaustive match")
        }
        jp500 = jp523
    default:
        panic("non-exhaustive match")
    }
    retv498 = jp500
    return retv498
}

func exp_var(name__129 string) Exp {
    var retv529 Exp
    var t530 Exp = Var{
        _0: name__129,
    }
    retv529 = t530
    return retv529
}

func exp_lam(name__130 string, body__131 Exp) Exp {
    var retv532 Exp
    var t533 Exp = Lam{
        _0: name__130,
        _1: body__131,
    }
    retv532 = t533
    return retv532
}

func exp_app(a__132 Exp, b__133 Exp) Exp {
    var retv535 Exp
    var t536 Exp = App{
        _0: a__132,
        _1: b__133,
    }
    retv535 = t536
    return retv535
}

func exp_let(name__134 string, a__135 Exp, b__136 Exp) Exp {
    var retv538 Exp
    var t539 Exp = Let{
        _0: name__134,
        _1: a__135,
        _2: b__136,
    }
    retv538 = t539
    return retv538
}

func show_result(label__137 string, res__138 Result__Typ__string) struct{} {
    switch res__138.(type) {
    case Result__Typ__string_Ok:
        var x221 Typ = res__138.(Result__Typ__string_Ok)._0
        var ty__139 Typ = x221
        var t542 string = label__137 + ": "
        var t543 string = typ_to_string(ty__139)
        var t544 string = t542 + t543
        println__T_string(t544)
    case Result__Typ__string_Err:
        var x222 string = res__138.(Result__Typ__string_Err)._0
        var e__140 string = x222
        var t546 string = label__137 + ": "
        var t547 string = t546 + e__140
        println__T_string(t547)
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func main0() struct{} {
    var st__141 CheckerState = state_new()
    var t550 Exp = exp_var("x")
    var id__142 Exp = exp_lam("x", t550)
    var t551 Exp = exp_var("x")
    var t552 Exp = exp_var("y")
    var t553 Exp = exp_app(t551, t552)
    var t554 Exp = exp_lam("y", t553)
    var c1__143 Exp = exp_lam("x", t554)
    reset_type_variables(st__141)
    var t555 *_goml_vec_EnvEntry = env_empty()
    var t556 Result__Typ__string = typeof(st__141, t555, id__142)
    show_result("id", t556)
    reset_type_variables(st__141)
    var t557 *_goml_vec_EnvEntry = env_empty()
    var t558 Result__Typ__string = typeof(st__141, t557, c1__143)
    show_result("c1", t558)
    reset_type_variables(st__141)
    var t559 *_goml_vec_EnvEntry = env_empty()
    var t560 Exp = exp_var("x")
    var t561 Exp = exp_let("x", c1__143, t560)
    var t562 Result__Typ__string = typeof(st__141, t559, t561)
    show_result("let_x_c1_x", t562)
    reset_type_variables(st__141)
    var t563 *_goml_vec_EnvEntry = env_empty()
    var t564 Exp = exp_var("z")
    var t565 Exp = exp_lam("z", t564)
    var t566 Exp = exp_var("y")
    var t567 Exp = exp_let("y", t565, t566)
    var t568 Result__Typ__string = typeof(st__141, t563, t567)
    show_result("let_y_id_y", t568)
    reset_type_variables(st__141)
    var t569 *_goml_vec_EnvEntry = env_empty()
    var t570 Exp = exp_var("z")
    var t571 Exp = exp_lam("z", t570)
    var t572 Exp = exp_var("y")
    var t573 Exp = exp_let("y", t571, t572)
    var t574 Exp = exp_lam("x", t573)
    var t575 Result__Typ__string = typeof(st__141, t569, t574)
    show_result("lam_x_let_y_id_y", t575)
    reset_type_variables(st__141)
    var t576 *_goml_vec_EnvEntry = env_empty()
    var t577 Exp = exp_var("z")
    var t578 Exp = exp_lam("z", t577)
    var t579 Exp = exp_var("y")
    var t580 Exp = exp_var("x")
    var t581 Exp = exp_app(t579, t580)
    var t582 Exp = exp_let("y", t578, t581)
    var t583 Exp = exp_lam("x", t582)
    var t584 Result__Typ__string = typeof(st__141, t576, t583)
    show_result("lam_x_let_y_id_yx", t584)
    reset_type_variables(st__141)
    var t585 *_goml_vec_EnvEntry = env_empty()
    var t586 Exp = exp_var("x")
    var t587 Exp = exp_var("x")
    var t588 Exp = exp_app(t586, t587)
    var t589 Exp = exp_lam("x", t588)
    var t590 Result__Typ__string = typeof(st__141, t585, t589)
    show_result("self_apply", t590)
    reset_type_variables(st__141)
    var t591 *_goml_vec_EnvEntry = env_empty()
    var t592 Exp = exp_var("x")
    var t593 Exp = exp_var("x")
    var t594 Exp = exp_let("x", t592, t593)
    var t595 Result__Typ__string = typeof(st__141, t591, t594)
    show_result("unbound_var", t595)
    reset_type_variables(st__141)
    var t596 *_goml_vec_EnvEntry = env_empty()
    var t597 Exp = exp_var("y")
    var t598 Exp = exp_var("y")
    var t599 Exp = exp_var("z")
    var t600 Exp = exp_app(t598, t599)
    var t601 Exp = exp_lam("z", t600)
    var t602 Exp = exp_app(t597, t601)
    var t603 Exp = exp_lam("y", t602)
    var t604 Result__Typ__string = typeof(st__141, t596, t603)
    show_result("max_heiber", t604)
    reset_type_variables(st__141)
    var t605 *_goml_vec_EnvEntry = env_empty()
    var t606 Exp = exp_var("k")
    var t607 Exp = exp_var("k")
    var t608 Exp = exp_var("x")
    var t609 Exp = exp_app(t607, t608)
    var t610 Exp = exp_var("y")
    var t611 Exp = exp_app(t609, t610)
    var t612 Exp = exp_app(t606, t611)
    var t613 Exp = exp_var("k")
    var t614 Exp = exp_var("y")
    var t615 Exp = exp_app(t613, t614)
    var t616 Exp = exp_var("x")
    var t617 Exp = exp_app(t615, t616)
    var t618 Exp = exp_app(t612, t617)
    var t619 Exp = exp_lam("k", t618)
    var t620 Exp = exp_lam("y", t619)
    var t621 Exp = exp_lam("x", t620)
    var t622 Result__Typ__string = typeof(st__141, t605, t621)
    show_result("kirang", t622)
    reset_type_variables(st__141)
    var t623 *_goml_vec_EnvEntry = env_empty()
    var t624 Exp = exp_var("id")
    var t625 Exp = exp_var("id")
    var t626 Exp = exp_app(t624, t625)
    var t627 Exp = exp_let("id", id__142, t626)
    var t628 Result__Typ__string = typeof(st__141, t623, t627)
    show_result("let_id_idid", t628)
    reset_type_variables(st__141)
    var t629 *_goml_vec_EnvEntry = env_empty()
    var t630 Exp = exp_var("x")
    var t631 Exp = exp_app(t630, id__142)
    var t632 Exp = exp_var("z")
    var t633 Exp = exp_let("z", t631, t632)
    var t634 Exp = exp_var("y")
    var t635 Exp = exp_let("y", t633, t634)
    var t636 Exp = exp_let("x", c1__143, t635)
    var t637 Result__Typ__string = typeof(st__141, t629, t636)
    show_result("nested_lets", t637)
    reset_type_variables(st__141)
    var t638 *_goml_vec_EnvEntry = env_empty()
    var t639 Exp = exp_var("x")
    var t640 Exp = exp_var("y")
    var t641 Exp = exp_app(t639, t640)
    var t642 Exp = exp_var("y")
    var t643 Exp = exp_var("x")
    var t644 Exp = exp_app(t642, t643)
    var t645 Exp = exp_lam("x", t644)
    var t646 Exp = exp_let("x", t641, t645)
    var t647 Exp = exp_lam("y", t646)
    var t648 Exp = exp_lam("x", t647)
    var t649 Result__Typ__string = typeof(st__141, t638, t648)
    show_result("fun_x_fun_y_let_x_xy_fun_x_yx", t649)
    reset_type_variables(st__141)
    var t650 *_goml_vec_EnvEntry = env_empty()
    var t651 Exp = exp_var("x")
    var t652 Exp = exp_var("y")
    var t653 Exp = exp_let("y", t651, t652)
    var t654 Exp = exp_lam("x", t653)
    var t655 Result__Typ__string = typeof(st__141, t650, t654)
    show_result("sound_gen_1", t655)
    reset_type_variables(st__141)
    var t656 *_goml_vec_EnvEntry = env_empty()
    var t657 Exp = exp_var("x")
    var t658 Exp = exp_lam("z", t657)
    var t659 Exp = exp_var("y")
    var t660 Exp = exp_let("y", t658, t659)
    var t661 Exp = exp_lam("x", t660)
    var t662 Result__Typ__string = typeof(st__141, t656, t661)
    show_result("sound_gen_2", t662)
    reset_type_variables(st__141)
    var t663 *_goml_vec_EnvEntry = env_empty()
    var t664 Exp = exp_var("x")
    var t665 Exp = exp_var("z")
    var t666 Exp = exp_app(t664, t665)
    var t667 Exp = exp_lam("z", t666)
    var t668 Exp = exp_var("y")
    var t669 Exp = exp_let("y", t667, t668)
    var t670 Exp = exp_lam("x", t669)
    var t671 Result__Typ__string = typeof(st__141, t663, t670)
    show_result("sound_gen_3", t671)
    reset_type_variables(st__141)
    var t672 *_goml_vec_EnvEntry = env_empty()
    var t673 Exp = exp_var("x")
    var t674 Exp = exp_var("y")
    var t675 Exp = exp_app(t673, t674)
    var t676 Exp = exp_var("x")
    var t677 Exp = exp_var("y")
    var t678 Exp = exp_app(t676, t677)
    var t679 Exp = exp_let("x", t675, t678)
    var t680 Exp = exp_lam("y", t679)
    var t681 Exp = exp_lam("x", t680)
    var t682 Result__Typ__string = typeof(st__141, t672, t681)
    show_result("double_apply", t682)
    reset_type_variables(st__141)
    var t683 *_goml_vec_EnvEntry = env_empty()
    var t684 Exp = exp_var("x")
    var t685 Exp = exp_var("y")
    var t686 Exp = exp_var("y")
    var t687 Exp = exp_app(t685, t686)
    var t688 Exp = exp_let("y", t684, t687)
    var t689 Exp = exp_lam("x", t688)
    var t690 Result__Typ__string = typeof(st__141, t683, t689)
    show_result("sound_gen_occurs", t690)
    reset_gensym(st__141)
    var t691 *_goml_vec_EnvEntry = env_empty()
    var t692 Exp = exp_var("x")
    var t693 Exp = exp_app(t692, id__142)
    var t694 Exp = exp_var("z")
    var t695 Exp = exp_let("z", t693, t694)
    var t696 Exp = exp_var("y")
    var t697 Exp = exp_let("y", t695, t696)
    var t698 Exp = exp_lam("x", t697)
    var t699 Result__Typ__string = typeof(st__141, t691, t698)
    show_result("fun_x_let_y_let_z_x_id_z_y", t699)
    println__T_string("")
    println__T_string("All Done")
    println__T_string("")
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__207 int32) *ref_int32_x {
    var retv701 *ref_int32_x
    var t702 *ref_int32_x = ref__Ref_5int32(value__207)
    retv701 = t702
    return retv701
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__209 *ref_int32_x, value__210 int32) struct{} {
    ref_set__Ref_5int32(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__208 *ref_int32_x) int32 {
    var retv706 int32
    var t707 int32 = ref_get__Ref_5int32(self__208)
    retv706 = t707
    return retv706
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__7 rune) string {
    var retv709 string
    var t710 string = _goml_runtime_core_char_to_string(self__7)
    retv709 = t710
    return retv709
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv712 string
    var t713 string = _goml_runtime_core_int32_to_string(self__6)
    retv712 = t713
    return retv712
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Tv(value__207 Tv) *ref_Tv_x {
    var retv715 *ref_Tv_x
    var t716 *ref_Tv_x = ref__Ref_2Tv(value__207)
    retv715 = t716
    return retv715
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(self__208 *ref_Tv_x) Tv {
    var retv718 Tv
    var t719 Tv = ref_get__Ref_2Tv(self__208)
    retv718 = t719
    return retv718
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__EnvEntry() *_goml_vec_EnvEntry {
    var retv721 *_goml_vec_EnvEntry
    var t722 *_goml_vec_EnvEntry = vec_new__Vec_8EnvEntry()
    retv721 = t722
    return retv721
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__EnvEntry(self__137 *_goml_vec_EnvEntry) int {
    var retv724 int
    var t725 int = vec_len__Vec_8EnvEntry(self__137)
    retv724 = t725
    return retv724
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv727 *ref_int_x
    var t728 *ref_int_x = ref__Ref_3int(value__207)
    retv727 = t728
    return retv727
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_Typ_r_(value__207 Option__Typ) *ref_Option__Typ_x {
    var retv730 *ref_Option__Typ_x
    var t731 *ref_Option__Typ_x = ref__Ref_11Option__Typ(value__207)
    retv730 = t731
    return retv730
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__207 bool) *ref_bool_x {
    var retv733 *ref_bool_x
    var t734 *ref_bool_x = ref__Ref_4bool(value__207)
    retv733 = t734
    return retv733
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__208 *ref_bool_x) bool {
    var retv736 bool
    var t737 bool = ref_get__Ref_4bool(self__208)
    retv736 = t737
    return retv736
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv739 int
    var t740 int = ref_get__Ref_3int(self__208)
    retv739 = t740
    return retv739
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__55 string, other__56 string) bool {
    var retv742 bool
    var t743 bool = self__55 == other__56
    retv742 = t743
    return retv742
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
    var retv751 Option__Typ
    var t752 Option__Typ = ref_get__Ref_11Option__Typ(self__208)
    retv751 = t752
    return retv751
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SubstEntry(self__137 *_goml_vec_SubstEntry) int {
    var retv754 int
    var t755 int = vec_len__Vec_10SubstEntry(self__137)
    retv754 = t755
    return retv754
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(self__209 *ref_Tv_x, value__210 Tv) struct{} {
    ref_set__Ref_2Tv(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SubstEntry(self__128 *_goml_vec_SubstEntry, elem__129 SubstEntry) *_goml_vec_SubstEntry {
    var retv759 *_goml_vec_SubstEntry
    var result__130 *_goml_vec_SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SubstEntry()
    var index__131 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop761:
    for {
        var t762 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
        var t763 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SubstEntry(self__128)
        var t764 bool = t762 < t763
        if t764 {
            var t765 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
            var t766 SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__SubstEntry(self__128, t765)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SubstEntry(result__130, t766)
            var t767 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
            var t768 int = t767 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__131, t768)
            continue
        } else {
            break Loop_loop761
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SubstEntry(result__130, elem__129)
    retv759 = result__130
    return retv759
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SubstEntry() *_goml_vec_SubstEntry {
    var retv770 *_goml_vec_SubstEntry
    var t771 *_goml_vec_SubstEntry = vec_new__Vec_10SubstEntry()
    retv770 = t771
    return retv770
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__EnvEntry(self__128 *_goml_vec_EnvEntry, elem__129 EnvEntry) *_goml_vec_EnvEntry {
    var retv773 *_goml_vec_EnvEntry
    var result__130 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__EnvEntry()
    var index__131 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop775:
    for {
        var t776 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
        var t777 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__EnvEntry(self__128)
        var t778 bool = t776 < t777
        if t778 {
            var t779 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
            var t780 EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__EnvEntry(self__128, t779)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__EnvEntry(result__130, t780)
            var t781 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
            var t782 int = t781 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__131, t782)
            continue
        } else {
            break Loop_loop775
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__EnvEntry(result__130, elem__129)
    retv773 = result__130
    return retv773
}

func println__T_string(value__1 string) struct{} {
    var t784 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t784)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SubstEntry(self__126 *_goml_vec_SubstEntry, elem__127 SubstEntry) struct{} {
    vec_push__Vec_10SubstEntry(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__SubstEntry(self__132 *_goml_vec_SubstEntry, index__133 int) SubstEntry {
    var retv789 SubstEntry
    var t790 SubstEntry = vec_get__Vec_10SubstEntry(self__132, index__133)
    retv789 = t790
    return retv789
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__EnvEntry(self__126 *_goml_vec_EnvEntry, elem__127 EnvEntry) struct{} {
    vec_push__Vec_8EnvEntry(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__EnvEntry(self__132 *_goml_vec_EnvEntry, index__133 int) EnvEntry {
    var retv794 EnvEntry
    var t795 EnvEntry = vec_get__Vec_8EnvEntry(self__132, index__133)
    retv794 = t795
    return retv794
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv797 string
    retv797 = self__38
    return retv797
}

func main() {
    main0()
}
