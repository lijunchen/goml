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
    var retv258 CheckerState
    var t259 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var t260 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(1)
    var t261 CheckerState = CheckerState{
        gensym_counter: t259,
        current_level: t260,
    }
    retv258 = t261
    return retv258
}

func reset_gensym(st__0 CheckerState) struct{} {
    var t263 *ref_int32_x = st__0.gensym_counter
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t263, 0)
    return struct{}{}
}

func reset_level(st__1 CheckerState) struct{} {
    var t265 *ref_int32_x = st__1.current_level
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t265, 1)
    return struct{}{}
}

func reset_type_variables(st__2 CheckerState) struct{} {
    reset_gensym(st__2)
    reset_level(st__2)
    return struct{}{}
}

func enter_level(st__3 CheckerState) struct{} {
    var t269 *ref_int32_x = st__3.current_level
    var l__4 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t269)
    var t270 *ref_int32_x = st__3.current_level
    var t271 int32 = l__4 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t270, t271)
    return struct{}{}
}

func leave_level(st__5 CheckerState) struct{} {
    var t273 *ref_int32_x = st__5.current_level
    var l__6 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t273)
    var t274 *ref_int32_x = st__5.current_level
    var t275 int32 = l__6 - 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t274, t275)
    return struct{}{}
}

func min_i32(a__7 int32, b__8 int32) int32 {
    var retv277 int32
    var t280 bool = a__7 < b__8
    var jp279 int32
    if t280 {
        jp279 = a__7
    } else {
        jp279 = b__8
    }
    retv277 = jp279
    return retv277
}

func nth_letter(n__9 int32) rune {
    var retv282 rune
    var jp284 rune
    switch n__9 {
    case 0:
        jp284 = 97
    case 1:
        jp284 = 98
    case 2:
        jp284 = 99
    case 3:
        jp284 = 100
    case 4:
        jp284 = 101
    case 5:
        jp284 = 102
    case 6:
        jp284 = 103
    case 7:
        jp284 = 104
    case 8:
        jp284 = 105
    case 9:
        jp284 = 106
    case 10:
        jp284 = 107
    case 11:
        jp284 = 108
    case 12:
        jp284 = 109
    case 13:
        jp284 = 110
    case 14:
        jp284 = 111
    case 15:
        jp284 = 112
    case 16:
        jp284 = 113
    case 17:
        jp284 = 114
    case 18:
        jp284 = 115
    case 19:
        jp284 = 116
    case 20:
        jp284 = 117
    case 21:
        jp284 = 118
    case 22:
        jp284 = 119
    case 23:
        jp284 = 120
    case 24:
        jp284 = 121
    case 25:
        jp284 = 122
    default:
        jp284 = 97
    }
    retv282 = jp284
    return retv282
}

func gensym(st__10 CheckerState) string {
    var retv286 string
    var t287 *ref_int32_x = st__10.gensym_counter
    var n__11 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t287)
    var t288 *ref_int32_x = st__10.gensym_counter
    var t289 int32 = n__11 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t288, t289)
    var t292 bool = n__11 < 26
    var jp291 string
    if t292 {
        var t293 rune = nth_letter(n__11)
        var t294 string = _goml_m_inherent_i_char_i_char_i_to__string(t293)
        jp291 = t294
    } else {
        var t295 string = _goml_m_inherent_i_int32_i_int32_i_to__string(n__11)
        var t296 string = "t" + t295
        jp291 = t296
    }
    retv286 = jp291
    return retv286
}

func newvar(st__12 CheckerState) Typ {
    var retv298 Typ
    var name__13 string = gensym(st__12)
    var t299 *ref_int32_x = st__12.current_level
    var level__14 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t299)
    var t300 Tv = Unbound{
        _0: name__13,
        _1: level__14,
    }
    var t301 *ref_Tv_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Tv(t300)
    var t302 Typ = TVar{
        _0: t301,
    }
    retv298 = t302
    return retv298
}

func typ_is_arrow(ty__15 Typ) bool {
    var retv304 bool
    var jp306 bool
    switch ty__15.(type) {
    case TVar:
        var x67 *ref_Tv_x = ty__15.(TVar)._0
        var tvref__16 *ref_Tv_x = x67
        var mtmp71 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvref__16)
        var jp308 bool
        switch mtmp71.(type) {
        case Link:
            var x74 Typ = mtmp71.(Link)._0
            var inner__17 Typ = x74
            var t309 bool = typ_is_arrow(inner__17)
            jp308 = t309
        default:
            jp308 = false
        }
        jp306 = jp308
    case TArrow:
        jp306 = true
    default:
        jp306 = false
    }
    retv304 = jp306
    return retv304
}

func typ_to_string(ty__18 Typ) string {
    var retv311 string
    var jp313 string
    switch ty__18.(type) {
    case TVar:
        var x75 *ref_Tv_x = ty__18.(TVar)._0
        var tvref__20 *ref_Tv_x = x75
        var mtmp79 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvref__20)
        var jp315 string
        switch mtmp79.(type) {
        case Unbound:
            var x80 string = mtmp79.(Unbound)._0
            var name__21 string = x80
            var t316 string = "'" + name__21
            jp315 = t316
        case Link:
            var x82 Typ = mtmp79.(Link)._0
            var inner__22 Typ = x82
            var t317 string = typ_to_string(inner__22)
            jp315 = t317
        default:
            panic("non-exhaustive match")
        }
        jp313 = jp315
    case QVar:
        var x76 string = ty__18.(QVar)._0
        var name__19 string = x76
        var t318 string = "'" + name__19
        jp313 = t318
    case TArrow:
        var x77 Typ = ty__18.(TArrow)._0
        var x78 Typ = ty__18.(TArrow)._1
        var t2__24 Typ = x78
        var t1__23 Typ = x77
        var t323 bool = typ_is_arrow(t1__23)
        var jp320 string
        if t323 {
            var t324 string = typ_to_string(t1__23)
            var t325 string = "(" + t324
            var t326 string = t325 + ")"
            jp320 = t326
        } else {
            var t327 string = typ_to_string(t1__23)
            jp320 = t327
        }
        var s1__25 string = jp320
        var s2__26 string = typ_to_string(t2__24)
        var t321 string = s1__25 + " -> "
        var t322 string = t321 + s2__26
        jp313 = t322
    default:
        panic("non-exhaustive match")
    }
    retv311 = jp313
    return retv311
}

func env_empty() *_goml_vec_EnvEntry {
    var retv329 *_goml_vec_EnvEntry
    var env__27 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__EnvEntry()
    retv329 = env__27
    return retv329
}

func env_lookup(env__28 *_goml_vec_EnvEntry, name__29 string) Option__Typ {
    var retv331 Option__Typ
    var t332 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__EnvEntry(env__28)
    var t333 int32 = t332 - 1
    var i__30 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(t333)
    var found__31 *ref_Option__Typ_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_Typ_r_(None{})
    var done__32 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    Loop_loop336:
    for {
        var t349 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(done__32)
        var t350 bool = !t349
        var jp338 bool
        if t350 {
            var t351 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__30)
            var t352 bool = t351 >= 0
            jp338 = t352
        } else {
            jp338 = false
        }
        if jp338 {
            var t339 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__30)
            var entry__33 EnvEntry = vec_get__Vec_8EnvEntry(env__28, t339)
            var t341 string = entry__33.name
            var t342 bool = t341 == name__29
            if t342 {
                var t343 Typ = entry__33.ty
                var t344 Option__Typ = Some{
                    _0: t343,
                }
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Option_l_Typ_r_(found__31, t344)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(done__32, true)
            } else {
                var t346 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__30)
                var t347 int32 = t346 - 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__30, t347)
            }
            continue
        } else {
            break Loop_loop336
        }
    }
    var t335 Option__Typ = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Option_l_Typ_r_(found__31)
    retv331 = t335
    return retv331
}

func subst_lookup(subst__34 *_goml_vec_SubstEntry, name__35 string) Option__Typ {
    var retv354 Option__Typ
    var t355 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SubstEntry(subst__34)
    var t356 int32 = t355 - 1
    var i__36 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(t356)
    var found__37 *ref_Option__Typ_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_Typ_r_(None{})
    var done__38 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    Loop_loop359:
    for {
        var t372 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(done__38)
        var t373 bool = !t372
        var jp361 bool
        if t373 {
            var t374 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__36)
            var t375 bool = t374 >= 0
            jp361 = t375
        } else {
            jp361 = false
        }
        if jp361 {
            var t362 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__36)
            var entry__39 SubstEntry = vec_get__Vec_10SubstEntry(subst__34, t362)
            var t364 string = entry__39.name
            var t365 bool = t364 == name__35
            if t365 {
                var t366 Typ = entry__39.ty
                var t367 Option__Typ = Some{
                    _0: t366,
                }
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Option_l_Typ_r_(found__37, t367)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(done__38, true)
            } else {
                var t369 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__36)
                var t370 int32 = t369 - 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__36, t370)
            }
            continue
        } else {
            break Loop_loop359
        }
    }
    var t358 Option__Typ = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Option_l_Typ_r_(found__37)
    retv354 = t358
    return retv354
}

func occurs(st__40 CheckerState, tvr__41 *ref_Tv_x, ty__42 Typ) Result__unit__string {
    var retv377 Result__unit__string
    var jp379 Result__unit__string
    switch ty__42.(type) {
    case TVar:
        var x87 *ref_Tv_x = ty__42.(TVar)._0
        var tvr2__43 *ref_Tv_x = x87
        var t382 bool = ptr_eq__Ref_2Tv(tvr__41, tvr2__43)
        var jp381 Result__unit__string
        if t382 {
            var t383 Result__unit__string = Result__unit__string_Err{
                _0: "occurs check",
            }
            jp381 = t383
        } else {
            var mtmp91 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvr2__43)
            var jp385 Result__unit__string
            switch mtmp91.(type) {
            case Unbound:
                var x92 string = mtmp91.(Unbound)._0
                var x93 int32 = mtmp91.(Unbound)._1
                var l2__45 int32 = x93
                var name__44 string = x92
                var mtmp95 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvr__41)
                var jp387 int32
                switch mtmp95.(type) {
                case Unbound:
                    var x97 int32 = mtmp95.(Unbound)._1
                    var l__46 int32 = x97
                    var t390 int32 = min_i32(l__46, l2__45)
                    jp387 = t390
                default:
                    jp387 = l2__45
                }
                var min_level__47 int32 = jp387
                var t388 Tv = Unbound{
                    _0: name__44,
                    _1: min_level__47,
                }
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(tvr2__43, t388)
                var t389 Result__unit__string = Result__unit__string_Ok{
                    _0: struct{}{},
                }
                jp385 = t389
            case Link:
                var x94 Typ = mtmp91.(Link)._0
                var inner__48 Typ = x94
                var t391 Result__unit__string = occurs(st__40, tvr__41, inner__48)
                jp385 = t391
            default:
                panic("non-exhaustive match")
            }
            jp381 = jp385
        }
        jp379 = jp381
    case TArrow:
        var x89 Typ = ty__42.(TArrow)._0
        var x90 Typ = ty__42.(TArrow)._1
        var t2__50 Typ = x90
        var t1__49 Typ = x89
        var mtmp100 Result__unit__string = occurs(st__40, tvr__41, t1__49)
        var jp393 Result__unit__string
        switch mtmp100.(type) {
        case Result__unit__string_Ok:
            var t394 Result__unit__string = occurs(st__40, tvr__41, t2__50)
            jp393 = t394
        case Result__unit__string_Err:
            var x102 string = mtmp100.(Result__unit__string_Err)._0
            var e__51 string = x102
            var t395 Result__unit__string = Result__unit__string_Err{
                _0: e__51,
            }
            jp393 = t395
        default:
            panic("non-exhaustive match")
        }
        jp379 = jp393
    default:
        var t396 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp379 = t396
    }
    retv377 = jp379
    return retv377
}

func unify(st__52 CheckerState, t1__53 Typ, t2__54 Typ) Result__unit__string {
    var retv398 Result__unit__string
    var mtmp103 Tuple2_3Typ_3Typ = Tuple2_3Typ_3Typ{
        _0: t1__53,
        _1: t2__54,
    }
    var x104 Typ = mtmp103._0
    var x105 Typ = mtmp103._1
    var jp400 Result__unit__string
    switch x105.(type) {
    case TVar:
        var x106 *ref_Tv_x = x105.(TVar)._0
        var jp402 Result__unit__string
        switch x104.(type) {
        case TVar:
            var x110 *ref_Tv_x = x104.(TVar)._0
            var r1__55 *ref_Tv_x = x110
            var r2__56 *ref_Tv_x = x106
            var t405 bool = ptr_eq__Ref_2Tv(r1__55, r2__56)
            var jp404 Result__unit__string
            if t405 {
                var t406 Result__unit__string = Result__unit__string_Ok{
                    _0: struct{}{},
                }
                jp404 = t406
            } else {
                var mtmp114 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r1__55)
                var jp408 Result__unit__string
                switch mtmp114.(type) {
                case Unbound:
                    var mtmp118 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r2__56)
                    var jp410 Result__unit__string
                    switch mtmp118.(type) {
                    case Unbound:
                        var t411 Typ = TVar{
                            _0: r2__56,
                        }
                        var mtmp122 Result__unit__string = occurs(st__52, r1__55, t411)
                        var jp413 Result__unit__string
                        switch mtmp122.(type) {
                        case Result__unit__string_Ok:
                            var t414 Typ = TVar{
                                _0: r2__56,
                            }
                            var t415 Tv = Link{
                                _0: t414,
                            }
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(r1__55, t415)
                            var t416 Result__unit__string = Result__unit__string_Ok{
                                _0: struct{}{},
                            }
                            jp413 = t416
                        case Result__unit__string_Err:
                            var x124 string = mtmp122.(Result__unit__string_Err)._0
                            var e__59 string = x124
                            var t417 Result__unit__string = Result__unit__string_Err{
                                _0: e__59,
                            }
                            jp413 = t417
                        default:
                            panic("non-exhaustive match")
                        }
                        jp410 = jp413
                    case Link:
                        var x121 Typ = mtmp118.(Link)._0
                        var inner__58 Typ = x121
                        var t418 Typ = TVar{
                            _0: r1__55,
                        }
                        var t419 Result__unit__string = unify(st__52, t418, inner__58)
                        jp410 = t419
                    default:
                        panic("non-exhaustive match")
                    }
                    jp408 = jp410
                case Link:
                    var x117 Typ = mtmp114.(Link)._0
                    var inner__57 Typ = x117
                    var t420 Typ = TVar{
                        _0: r2__56,
                    }
                    var t421 Result__unit__string = unify(st__52, inner__57, t420)
                    jp408 = t421
                default:
                    panic("non-exhaustive match")
                }
                jp404 = jp408
            }
            jp402 = jp404
        default:
            var r2__65 *ref_Tv_x = x106
            var other__64 Typ = x104
            var mtmp126 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r2__65)
            var jp423 Result__unit__string
            switch mtmp126.(type) {
            case Unbound:
                var mtmp130 Result__unit__string = occurs(st__52, r2__65, other__64)
                var jp425 Result__unit__string
                switch mtmp130.(type) {
                case Result__unit__string_Ok:
                    var t426 Tv = Link{
                        _0: other__64,
                    }
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(r2__65, t426)
                    var t427 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    jp425 = t427
                case Result__unit__string_Err:
                    var x132 string = mtmp130.(Result__unit__string_Err)._0
                    var e__67 string = x132
                    var t428 Result__unit__string = Result__unit__string_Err{
                        _0: e__67,
                    }
                    jp425 = t428
                default:
                    panic("non-exhaustive match")
                }
                jp423 = jp425
            case Link:
                var x129 Typ = mtmp126.(Link)._0
                var inner__66 Typ = x129
                var t429 Result__unit__string = unify(st__52, other__64, inner__66)
                jp423 = t429
            default:
                panic("non-exhaustive match")
            }
            jp402 = jp423
        }
        jp400 = jp402
    case TArrow:
        var x108 Typ = x105.(TArrow)._0
        var x109 Typ = x105.(TArrow)._1
        var jp431 Result__unit__string
        switch x104.(type) {
        case TVar:
            var x134 *ref_Tv_x = x104.(TVar)._0
            var r1__60 *ref_Tv_x = x134
            var other__61 Typ = x105
            var mtmp138 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r1__60)
            var jp433 Result__unit__string
            switch mtmp138.(type) {
            case Unbound:
                var mtmp142 Result__unit__string = occurs(st__52, r1__60, other__61)
                var jp435 Result__unit__string
                switch mtmp142.(type) {
                case Result__unit__string_Ok:
                    var t436 Tv = Link{
                        _0: other__61,
                    }
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(r1__60, t436)
                    var t437 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    jp435 = t437
                case Result__unit__string_Err:
                    var x144 string = mtmp142.(Result__unit__string_Err)._0
                    var e__63 string = x144
                    var t438 Result__unit__string = Result__unit__string_Err{
                        _0: e__63,
                    }
                    jp435 = t438
                default:
                    panic("non-exhaustive match")
                }
                jp433 = jp435
            case Link:
                var x141 Typ = mtmp138.(Link)._0
                var inner__62 Typ = x141
                var t439 Result__unit__string = unify(st__52, inner__62, other__61)
                jp433 = t439
            default:
                panic("non-exhaustive match")
            }
            jp431 = jp433
        case TArrow:
            var x136 Typ = x104.(TArrow)._0
            var x137 Typ = x104.(TArrow)._1
            var a2__69 Typ = x137
            var a1__68 Typ = x136
            var b2__71 Typ = x109
            var b1__70 Typ = x108
            var mtmp146 Result__unit__string = unify(st__52, a1__68, b1__70)
            var jp441 Result__unit__string
            switch mtmp146.(type) {
            case Result__unit__string_Ok:
                var t442 Result__unit__string = unify(st__52, a2__69, b2__71)
                jp441 = t442
            case Result__unit__string_Err:
                var x148 string = mtmp146.(Result__unit__string_Err)._0
                var e__72 string = x148
                var t443 Result__unit__string = Result__unit__string_Err{
                    _0: e__72,
                }
                jp441 = t443
            default:
                panic("non-exhaustive match")
            }
            jp431 = jp441
        default:
            var t444 Result__unit__string = Result__unit__string_Err{
                _0: "unify error",
            }
            jp431 = t444
        }
        jp400 = jp431
    default:
        var jp446 Result__unit__string
        switch x104.(type) {
        case TVar:
            var x149 *ref_Tv_x = x104.(TVar)._0
            var r1__60 *ref_Tv_x = x149
            var other__61 Typ = x105
            var mtmp153 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r1__60)
            var jp448 Result__unit__string
            switch mtmp153.(type) {
            case Unbound:
                var mtmp157 Result__unit__string = occurs(st__52, r1__60, other__61)
                var jp450 Result__unit__string
                switch mtmp157.(type) {
                case Result__unit__string_Ok:
                    var t451 Tv = Link{
                        _0: other__61,
                    }
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(r1__60, t451)
                    var t452 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    jp450 = t452
                case Result__unit__string_Err:
                    var x159 string = mtmp157.(Result__unit__string_Err)._0
                    var e__63 string = x159
                    var t453 Result__unit__string = Result__unit__string_Err{
                        _0: e__63,
                    }
                    jp450 = t453
                default:
                    panic("non-exhaustive match")
                }
                jp448 = jp450
            case Link:
                var x156 Typ = mtmp153.(Link)._0
                var inner__62 Typ = x156
                var t454 Result__unit__string = unify(st__52, inner__62, other__61)
                jp448 = t454
            default:
                panic("non-exhaustive match")
            }
            jp446 = jp448
        default:
            var t455 Result__unit__string = Result__unit__string_Err{
                _0: "unify error",
            }
            jp446 = t455
        }
        jp400 = jp446
    }
    retv398 = jp400
    return retv398
}

func gen(st__73 CheckerState, ty__74 Typ) Typ {
    var retv457 Typ
    var jp459 Typ
    switch ty__74.(type) {
    case TVar:
        var x161 *ref_Tv_x = ty__74.(TVar)._0
        var tvref__75 *ref_Tv_x = x161
        var mtmp165 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvref__75)
        var jp461 Typ
        switch mtmp165.(type) {
        case Unbound:
            var x166 string = mtmp165.(Unbound)._0
            var x167 int32 = mtmp165.(Unbound)._1
            var l__77 int32 = x167
            var name__76 string = x166
            var t462 *ref_int32_x = st__73.current_level
            var cur__78 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t462)
            var t465 bool = l__77 > cur__78
            var jp464 Typ
            if t465 {
                var t466 Typ = QVar{
                    _0: name__76,
                }
                jp464 = t466
            } else {
                var t467 Typ = TVar{
                    _0: tvref__75,
                }
                jp464 = t467
            }
            jp461 = jp464
        case Link:
            var x168 Typ = mtmp165.(Link)._0
            var inner__79 Typ = x168
            var t468 Typ = gen(st__73, inner__79)
            jp461 = t468
        default:
            panic("non-exhaustive match")
        }
        jp459 = jp461
    case TArrow:
        var x163 Typ = ty__74.(TArrow)._0
        var x164 Typ = ty__74.(TArrow)._1
        var t2__81 Typ = x164
        var t1__80 Typ = x163
        var t469 Typ = gen(st__73, t1__80)
        var t470 Typ = gen(st__73, t2__81)
        var t471 Typ = TArrow{
            _0: t469,
            _1: t470,
        }
        jp459 = t471
    default:
        var other__82 Typ = ty__74
        jp459 = other__82
    }
    retv457 = jp459
    return retv457
}

func inst_loop(st__83 CheckerState, subst__84 *_goml_vec_SubstEntry, ty__85 Typ) Tuple2_3Typ_16Vec_10SubstEntry {
    var retv473 Tuple2_3Typ_16Vec_10SubstEntry
    var jp475 Tuple2_3Typ_16Vec_10SubstEntry
    switch ty__85.(type) {
    case TVar:
        var x169 *ref_Tv_x = ty__85.(TVar)._0
        var tvref__90 *ref_Tv_x = x169
        var mtmp173 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvref__90)
        var jp477 Tuple2_3Typ_16Vec_10SubstEntry
        switch mtmp173.(type) {
        case Link:
            var x176 Typ = mtmp173.(Link)._0
            var inner__91 Typ = x176
            var t478 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, subst__84, inner__91)
            jp477 = t478
        default:
            var t479 Typ = TVar{
                _0: tvref__90,
            }
            var t480 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: t479,
                _1: subst__84,
            }
            jp477 = t480
        }
        jp475 = jp477
    case QVar:
        var x170 string = ty__85.(QVar)._0
        var name__86 string = x170
        var mtmp177 Option__Typ = subst_lookup(subst__84, name__86)
        var jp482 Tuple2_3Typ_16Vec_10SubstEntry
        switch mtmp177.(type) {
        case None:
            var tv__88 Typ = newvar(st__83)
            var t483 SubstEntry = SubstEntry{
                name: name__86,
                ty: tv__88,
            }
            var new_subst__89 *_goml_vec_SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SubstEntry(subst__84, t483)
            var t484 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: tv__88,
                _1: new_subst__89,
            }
            jp482 = t484
        case Some:
            var x178 Typ = mtmp177.(Some)._0
            var t__87 Typ = x178
            var t485 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: t__87,
                _1: subst__84,
            }
            jp482 = t485
        default:
            panic("non-exhaustive match")
        }
        jp475 = jp482
    case TArrow:
        var x171 Typ = ty__85.(TArrow)._0
        var x172 Typ = ty__85.(TArrow)._1
        var t2__93 Typ = x172
        var t1__92 Typ = x171
        var mtmp179 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, subst__84, t1__92)
        var x180 Typ = mtmp179._0
        var x181 *_goml_vec_SubstEntry = mtmp179._1
        var subst1__95 *_goml_vec_SubstEntry = x181
        var ty1__94 Typ = x180
        var mtmp182 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, subst1__95, t2__93)
        var x183 Typ = mtmp182._0
        var x184 *_goml_vec_SubstEntry = mtmp182._1
        var subst2__97 *_goml_vec_SubstEntry = x184
        var ty2__96 Typ = x183
        var t486 Typ = TArrow{
            _0: ty1__94,
            _1: ty2__96,
        }
        var t487 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
            _0: t486,
            _1: subst2__97,
        }
        jp475 = t487
    default:
        panic("non-exhaustive match")
    }
    retv473 = jp475
    return retv473
}

func inst(st__98 CheckerState, ty__99 Typ) Typ {
    var retv489 Typ
    var subst0__100 *_goml_vec_SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SubstEntry()
    var mtmp185 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__98, subst0__100, ty__99)
    var x186 Typ = mtmp185._0
    var t__101 Typ = x186
    retv489 = t__101
    return retv489
}

func typeof(st__102 CheckerState, env__103 *_goml_vec_EnvEntry, e__104 Exp) Result__Typ__string {
    var retv491 Result__Typ__string
    var jp493 Result__Typ__string
    switch e__104.(type) {
    case Var:
        var x188 string = e__104.(Var)._0
        var x__105 string = x188
        var mtmp196 Option__Typ = env_lookup(env__103, x__105)
        var jp495 Result__Typ__string
        switch mtmp196.(type) {
        case None:
            var t496 Result__Typ__string = Result__Typ__string_Err{
                _0: "unbound var",
            }
            jp495 = t496
        case Some:
            var x197 Typ = mtmp196.(Some)._0
            var ty__106 Typ = x197
            var t497 Typ = inst(st__102, ty__106)
            var t498 Result__Typ__string = Result__Typ__string_Ok{
                _0: t497,
            }
            jp495 = t498
        default:
            panic("non-exhaustive match")
        }
        jp493 = jp495
    case App:
        var x189 Exp = e__104.(App)._0
        var x190 Exp = e__104.(App)._1
        var e2__114 Exp = x190
        var e1__113 Exp = x189
        var mtmp198 Result__Typ__string = typeof(st__102, env__103, e1__113)
        var jp500 Result__Typ__string
        switch mtmp198.(type) {
        case Result__Typ__string_Ok:
            var x199 Typ = mtmp198.(Result__Typ__string_Ok)._0
            var ty_fun__116 Typ = x199
            var mtmp201 Result__Typ__string = typeof(st__102, env__103, e2__114)
            var jp502 Result__Typ__string
            switch mtmp201.(type) {
            case Result__Typ__string_Ok:
                var x202 Typ = mtmp201.(Result__Typ__string_Ok)._0
                var ty_arg__118 Typ = x202
                var ty_res__119 Typ = newvar(st__102)
                var arrow__120 Typ = TArrow{
                    _0: ty_arg__118,
                    _1: ty_res__119,
                }
                var mtmp204 Result__unit__string = unify(st__102, ty_fun__116, arrow__120)
                var jp504 Result__Typ__string
                switch mtmp204.(type) {
                case Result__unit__string_Ok:
                    var t505 Result__Typ__string = Result__Typ__string_Ok{
                        _0: ty_res__119,
                    }
                    jp504 = t505
                case Result__unit__string_Err:
                    var x206 string = mtmp204.(Result__unit__string_Err)._0
                    var e__121 string = x206
                    var t506 Result__Typ__string = Result__Typ__string_Err{
                        _0: e__121,
                    }
                    jp504 = t506
                default:
                    panic("non-exhaustive match")
                }
                jp502 = jp504
            case Result__Typ__string_Err:
                var x203 string = mtmp201.(Result__Typ__string_Err)._0
                var e__117 string = x203
                var t507 Result__Typ__string = Result__Typ__string_Err{
                    _0: e__117,
                }
                jp502 = t507
            default:
                panic("non-exhaustive match")
            }
            jp500 = jp502
        case Result__Typ__string_Err:
            var x200 string = mtmp198.(Result__Typ__string_Err)._0
            var e__115 string = x200
            var t508 Result__Typ__string = Result__Typ__string_Err{
                _0: e__115,
            }
            jp500 = t508
        default:
            panic("non-exhaustive match")
        }
        jp493 = jp500
    case Lam:
        var x191 string = e__104.(Lam)._0
        var x192 Exp = e__104.(Lam)._1
        var body__108 Exp = x192
        var x__107 string = x191
        var ty_x__109 Typ = newvar(st__102)
        var t509 EnvEntry = EnvEntry{
            name: x__107,
            ty: ty_x__109,
        }
        var env2__110 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__EnvEntry(env__103, t509)
        var mtmp207 Result__Typ__string = typeof(st__102, env2__110, body__108)
        var jp511 Result__Typ__string
        switch mtmp207.(type) {
        case Result__Typ__string_Ok:
            var x208 Typ = mtmp207.(Result__Typ__string_Ok)._0
            var ty_e__111 Typ = x208
            var t512 Typ = TArrow{
                _0: ty_x__109,
                _1: ty_e__111,
            }
            var t513 Result__Typ__string = Result__Typ__string_Ok{
                _0: t512,
            }
            jp511 = t513
        case Result__Typ__string_Err:
            var x209 string = mtmp207.(Result__Typ__string_Err)._0
            var e__112 string = x209
            var t514 Result__Typ__string = Result__Typ__string_Err{
                _0: e__112,
            }
            jp511 = t514
        default:
            panic("non-exhaustive match")
        }
        jp493 = jp511
    case Let:
        var x193 string = e__104.(Let)._0
        var x194 Exp = e__104.(Let)._1
        var x195 Exp = e__104.(Let)._2
        var e2__124 Exp = x195
        var e1__123 Exp = x194
        var x__122 string = x193
        enter_level(st__102)
        var ty_e__125 Result__Typ__string = typeof(st__102, env__103, e1__123)
        leave_level(st__102)
        var jp516 Result__Typ__string
        switch ty_e__125.(type) {
        case Result__Typ__string_Ok:
            var x212 Typ = ty_e__125.(Result__Typ__string_Ok)._0
            var ty1__127 Typ = x212
            var t517 Typ = gen(st__102, ty1__127)
            var t518 EnvEntry = EnvEntry{
                name: x__122,
                ty: t517,
            }
            var env2__128 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__EnvEntry(env__103, t518)
            var t519 Result__Typ__string = typeof(st__102, env2__128, e2__124)
            jp516 = t519
        case Result__Typ__string_Err:
            var x213 string = ty_e__125.(Result__Typ__string_Err)._0
            var e__126 string = x213
            var t520 Result__Typ__string = Result__Typ__string_Err{
                _0: e__126,
            }
            jp516 = t520
        default:
            panic("non-exhaustive match")
        }
        jp493 = jp516
    default:
        panic("non-exhaustive match")
    }
    retv491 = jp493
    return retv491
}

func exp_var(name__129 string) Exp {
    var retv522 Exp
    var t523 Exp = Var{
        _0: name__129,
    }
    retv522 = t523
    return retv522
}

func exp_lam(name__130 string, body__131 Exp) Exp {
    var retv525 Exp
    var t526 Exp = Lam{
        _0: name__130,
        _1: body__131,
    }
    retv525 = t526
    return retv525
}

func exp_app(a__132 Exp, b__133 Exp) Exp {
    var retv528 Exp
    var t529 Exp = App{
        _0: a__132,
        _1: b__133,
    }
    retv528 = t529
    return retv528
}

func exp_let(name__134 string, a__135 Exp, b__136 Exp) Exp {
    var retv531 Exp
    var t532 Exp = Let{
        _0: name__134,
        _1: a__135,
        _2: b__136,
    }
    retv531 = t532
    return retv531
}

func show_result(label__137 string, res__138 Result__Typ__string) struct{} {
    switch res__138.(type) {
    case Result__Typ__string_Ok:
        var x214 Typ = res__138.(Result__Typ__string_Ok)._0
        var ty__139 Typ = x214
        var t535 string = label__137 + ": "
        var t536 string = typ_to_string(ty__139)
        var t537 string = t535 + t536
        println__T_string(t537)
    case Result__Typ__string_Err:
        var x215 string = res__138.(Result__Typ__string_Err)._0
        var e__140 string = x215
        var t539 string = label__137 + ": "
        var t540 string = t539 + e__140
        println__T_string(t540)
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func main0() struct{} {
    var st__141 CheckerState = state_new()
    var t543 Exp = exp_var("x")
    var id__142 Exp = exp_lam("x", t543)
    var t544 Exp = exp_var("x")
    var t545 Exp = exp_var("y")
    var t546 Exp = exp_app(t544, t545)
    var t547 Exp = exp_lam("y", t546)
    var c1__143 Exp = exp_lam("x", t547)
    reset_type_variables(st__141)
    var t548 *_goml_vec_EnvEntry = env_empty()
    var t549 Result__Typ__string = typeof(st__141, t548, id__142)
    show_result("id", t549)
    reset_type_variables(st__141)
    var t550 *_goml_vec_EnvEntry = env_empty()
    var t551 Result__Typ__string = typeof(st__141, t550, c1__143)
    show_result("c1", t551)
    reset_type_variables(st__141)
    var t552 *_goml_vec_EnvEntry = env_empty()
    var t553 Exp = exp_var("x")
    var t554 Exp = exp_let("x", c1__143, t553)
    var t555 Result__Typ__string = typeof(st__141, t552, t554)
    show_result("let_x_c1_x", t555)
    reset_type_variables(st__141)
    var t556 *_goml_vec_EnvEntry = env_empty()
    var t557 Exp = exp_var("z")
    var t558 Exp = exp_lam("z", t557)
    var t559 Exp = exp_var("y")
    var t560 Exp = exp_let("y", t558, t559)
    var t561 Result__Typ__string = typeof(st__141, t556, t560)
    show_result("let_y_id_y", t561)
    reset_type_variables(st__141)
    var t562 *_goml_vec_EnvEntry = env_empty()
    var t563 Exp = exp_var("z")
    var t564 Exp = exp_lam("z", t563)
    var t565 Exp = exp_var("y")
    var t566 Exp = exp_let("y", t564, t565)
    var t567 Exp = exp_lam("x", t566)
    var t568 Result__Typ__string = typeof(st__141, t562, t567)
    show_result("lam_x_let_y_id_y", t568)
    reset_type_variables(st__141)
    var t569 *_goml_vec_EnvEntry = env_empty()
    var t570 Exp = exp_var("z")
    var t571 Exp = exp_lam("z", t570)
    var t572 Exp = exp_var("y")
    var t573 Exp = exp_var("x")
    var t574 Exp = exp_app(t572, t573)
    var t575 Exp = exp_let("y", t571, t574)
    var t576 Exp = exp_lam("x", t575)
    var t577 Result__Typ__string = typeof(st__141, t569, t576)
    show_result("lam_x_let_y_id_yx", t577)
    reset_type_variables(st__141)
    var t578 *_goml_vec_EnvEntry = env_empty()
    var t579 Exp = exp_var("x")
    var t580 Exp = exp_var("x")
    var t581 Exp = exp_app(t579, t580)
    var t582 Exp = exp_lam("x", t581)
    var t583 Result__Typ__string = typeof(st__141, t578, t582)
    show_result("self_apply", t583)
    reset_type_variables(st__141)
    var t584 *_goml_vec_EnvEntry = env_empty()
    var t585 Exp = exp_var("x")
    var t586 Exp = exp_var("x")
    var t587 Exp = exp_let("x", t585, t586)
    var t588 Result__Typ__string = typeof(st__141, t584, t587)
    show_result("unbound_var", t588)
    reset_type_variables(st__141)
    var t589 *_goml_vec_EnvEntry = env_empty()
    var t590 Exp = exp_var("y")
    var t591 Exp = exp_var("y")
    var t592 Exp = exp_var("z")
    var t593 Exp = exp_app(t591, t592)
    var t594 Exp = exp_lam("z", t593)
    var t595 Exp = exp_app(t590, t594)
    var t596 Exp = exp_lam("y", t595)
    var t597 Result__Typ__string = typeof(st__141, t589, t596)
    show_result("max_heiber", t597)
    reset_type_variables(st__141)
    var t598 *_goml_vec_EnvEntry = env_empty()
    var t599 Exp = exp_var("k")
    var t600 Exp = exp_var("k")
    var t601 Exp = exp_var("x")
    var t602 Exp = exp_app(t600, t601)
    var t603 Exp = exp_var("y")
    var t604 Exp = exp_app(t602, t603)
    var t605 Exp = exp_app(t599, t604)
    var t606 Exp = exp_var("k")
    var t607 Exp = exp_var("y")
    var t608 Exp = exp_app(t606, t607)
    var t609 Exp = exp_var("x")
    var t610 Exp = exp_app(t608, t609)
    var t611 Exp = exp_app(t605, t610)
    var t612 Exp = exp_lam("k", t611)
    var t613 Exp = exp_lam("y", t612)
    var t614 Exp = exp_lam("x", t613)
    var t615 Result__Typ__string = typeof(st__141, t598, t614)
    show_result("kirang", t615)
    reset_type_variables(st__141)
    var t616 *_goml_vec_EnvEntry = env_empty()
    var t617 Exp = exp_var("id")
    var t618 Exp = exp_var("id")
    var t619 Exp = exp_app(t617, t618)
    var t620 Exp = exp_let("id", id__142, t619)
    var t621 Result__Typ__string = typeof(st__141, t616, t620)
    show_result("let_id_idid", t621)
    reset_type_variables(st__141)
    var t622 *_goml_vec_EnvEntry = env_empty()
    var t623 Exp = exp_var("x")
    var t624 Exp = exp_app(t623, id__142)
    var t625 Exp = exp_var("z")
    var t626 Exp = exp_let("z", t624, t625)
    var t627 Exp = exp_var("y")
    var t628 Exp = exp_let("y", t626, t627)
    var t629 Exp = exp_let("x", c1__143, t628)
    var t630 Result__Typ__string = typeof(st__141, t622, t629)
    show_result("nested_lets", t630)
    reset_type_variables(st__141)
    var t631 *_goml_vec_EnvEntry = env_empty()
    var t632 Exp = exp_var("x")
    var t633 Exp = exp_var("y")
    var t634 Exp = exp_app(t632, t633)
    var t635 Exp = exp_var("y")
    var t636 Exp = exp_var("x")
    var t637 Exp = exp_app(t635, t636)
    var t638 Exp = exp_lam("x", t637)
    var t639 Exp = exp_let("x", t634, t638)
    var t640 Exp = exp_lam("y", t639)
    var t641 Exp = exp_lam("x", t640)
    var t642 Result__Typ__string = typeof(st__141, t631, t641)
    show_result("fun_x_fun_y_let_x_xy_fun_x_yx", t642)
    reset_type_variables(st__141)
    var t643 *_goml_vec_EnvEntry = env_empty()
    var t644 Exp = exp_var("x")
    var t645 Exp = exp_var("y")
    var t646 Exp = exp_let("y", t644, t645)
    var t647 Exp = exp_lam("x", t646)
    var t648 Result__Typ__string = typeof(st__141, t643, t647)
    show_result("sound_gen_1", t648)
    reset_type_variables(st__141)
    var t649 *_goml_vec_EnvEntry = env_empty()
    var t650 Exp = exp_var("x")
    var t651 Exp = exp_lam("z", t650)
    var t652 Exp = exp_var("y")
    var t653 Exp = exp_let("y", t651, t652)
    var t654 Exp = exp_lam("x", t653)
    var t655 Result__Typ__string = typeof(st__141, t649, t654)
    show_result("sound_gen_2", t655)
    reset_type_variables(st__141)
    var t656 *_goml_vec_EnvEntry = env_empty()
    var t657 Exp = exp_var("x")
    var t658 Exp = exp_var("z")
    var t659 Exp = exp_app(t657, t658)
    var t660 Exp = exp_lam("z", t659)
    var t661 Exp = exp_var("y")
    var t662 Exp = exp_let("y", t660, t661)
    var t663 Exp = exp_lam("x", t662)
    var t664 Result__Typ__string = typeof(st__141, t656, t663)
    show_result("sound_gen_3", t664)
    reset_type_variables(st__141)
    var t665 *_goml_vec_EnvEntry = env_empty()
    var t666 Exp = exp_var("x")
    var t667 Exp = exp_var("y")
    var t668 Exp = exp_app(t666, t667)
    var t669 Exp = exp_var("x")
    var t670 Exp = exp_var("y")
    var t671 Exp = exp_app(t669, t670)
    var t672 Exp = exp_let("x", t668, t671)
    var t673 Exp = exp_lam("y", t672)
    var t674 Exp = exp_lam("x", t673)
    var t675 Result__Typ__string = typeof(st__141, t665, t674)
    show_result("double_apply", t675)
    reset_type_variables(st__141)
    var t676 *_goml_vec_EnvEntry = env_empty()
    var t677 Exp = exp_var("x")
    var t678 Exp = exp_var("y")
    var t679 Exp = exp_var("y")
    var t680 Exp = exp_app(t678, t679)
    var t681 Exp = exp_let("y", t677, t680)
    var t682 Exp = exp_lam("x", t681)
    var t683 Result__Typ__string = typeof(st__141, t676, t682)
    show_result("sound_gen_occurs", t683)
    reset_gensym(st__141)
    var t684 *_goml_vec_EnvEntry = env_empty()
    var t685 Exp = exp_var("x")
    var t686 Exp = exp_app(t685, id__142)
    var t687 Exp = exp_var("z")
    var t688 Exp = exp_let("z", t686, t687)
    var t689 Exp = exp_var("y")
    var t690 Exp = exp_let("y", t688, t689)
    var t691 Exp = exp_lam("x", t690)
    var t692 Result__Typ__string = typeof(st__141, t684, t691)
    show_result("fun_x_let_y_let_z_x_id_z_y", t692)
    println__T_string("")
    println__T_string("All Done")
    println__T_string("")
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__204 int32) *ref_int32_x {
    var retv694 *ref_int32_x
    var t695 *ref_int32_x = ref__Ref_5int32(value__204)
    retv694 = t695
    return retv694
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__206 *ref_int32_x, value__207 int32) struct{} {
    ref_set__Ref_5int32(self__206, value__207)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__205 *ref_int32_x) int32 {
    var retv699 int32
    var t700 int32 = ref_get__Ref_5int32(self__205)
    retv699 = t700
    return retv699
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__6 rune) string {
    var retv702 string
    var t703 string = _goml_runtime_core_char_to_string(self__6)
    retv702 = t703
    return retv702
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv705 string
    var t706 string = _goml_runtime_core_int32_to_string(self__5)
    retv705 = t706
    return retv705
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Tv(value__204 Tv) *ref_Tv_x {
    var retv708 *ref_Tv_x
    var t709 *ref_Tv_x = ref__Ref_2Tv(value__204)
    retv708 = t709
    return retv708
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(self__205 *ref_Tv_x) Tv {
    var retv711 Tv
    var t712 Tv = ref_get__Ref_2Tv(self__205)
    retv711 = t712
    return retv711
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__EnvEntry() *_goml_vec_EnvEntry {
    var retv714 *_goml_vec_EnvEntry
    var t715 *_goml_vec_EnvEntry = vec_new__Vec_8EnvEntry()
    retv714 = t715
    return retv714
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__EnvEntry(self__134 *_goml_vec_EnvEntry) int32 {
    var retv717 int32
    var t718 int32 = vec_len__Vec_8EnvEntry(self__134)
    retv717 = t718
    return retv717
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_Typ_r_(value__204 Option__Typ) *ref_Option__Typ_x {
    var retv720 *ref_Option__Typ_x
    var t721 *ref_Option__Typ_x = ref__Ref_11Option__Typ(value__204)
    retv720 = t721
    return retv720
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__204 bool) *ref_bool_x {
    var retv723 *ref_bool_x
    var t724 *ref_bool_x = ref__Ref_4bool(value__204)
    retv723 = t724
    return retv723
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__205 *ref_bool_x) bool {
    var retv726 bool
    var t727 bool = ref_get__Ref_4bool(self__205)
    retv726 = t727
    return retv726
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
    var retv733 Option__Typ
    var t734 Option__Typ = ref_get__Ref_11Option__Typ(self__205)
    retv733 = t734
    return retv733
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SubstEntry(self__134 *_goml_vec_SubstEntry) int32 {
    var retv736 int32
    var t737 int32 = vec_len__Vec_10SubstEntry(self__134)
    retv736 = t737
    return retv736
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(self__206 *ref_Tv_x, value__207 Tv) struct{} {
    ref_set__Ref_2Tv(self__206, value__207)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SubstEntry(self__125 *_goml_vec_SubstEntry, elem__126 SubstEntry) *_goml_vec_SubstEntry {
    var retv741 *_goml_vec_SubstEntry
    var result__127 *_goml_vec_SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SubstEntry()
    var index__128 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop743:
    for {
        var t744 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
        var t745 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SubstEntry(self__125)
        var t746 bool = t744 < t745
        if t746 {
            var t747 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
            var t748 SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__SubstEntry(self__125, t747)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SubstEntry(result__127, t748)
            var t749 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
            var t750 int32 = t749 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__128, t750)
            continue
        } else {
            break Loop_loop743
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SubstEntry(result__127, elem__126)
    retv741 = result__127
    return retv741
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SubstEntry() *_goml_vec_SubstEntry {
    var retv752 *_goml_vec_SubstEntry
    var t753 *_goml_vec_SubstEntry = vec_new__Vec_10SubstEntry()
    retv752 = t753
    return retv752
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__EnvEntry(self__125 *_goml_vec_EnvEntry, elem__126 EnvEntry) *_goml_vec_EnvEntry {
    var retv755 *_goml_vec_EnvEntry
    var result__127 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__EnvEntry()
    var index__128 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop757:
    for {
        var t758 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
        var t759 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__EnvEntry(self__125)
        var t760 bool = t758 < t759
        if t760 {
            var t761 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
            var t762 EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__EnvEntry(self__125, t761)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__EnvEntry(result__127, t762)
            var t763 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
            var t764 int32 = t763 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__128, t764)
            continue
        } else {
            break Loop_loop757
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__EnvEntry(result__127, elem__126)
    retv755 = result__127
    return retv755
}

func println__T_string(value__1 string) struct{} {
    var t766 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t766)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SubstEntry(self__123 *_goml_vec_SubstEntry, elem__124 SubstEntry) struct{} {
    vec_push__Vec_10SubstEntry(self__123, elem__124)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__SubstEntry(self__129 *_goml_vec_SubstEntry, index__130 int32) SubstEntry {
    var retv771 SubstEntry
    var t772 SubstEntry = vec_get__Vec_10SubstEntry(self__129, index__130)
    retv771 = t772
    return retv771
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__EnvEntry(self__123 *_goml_vec_EnvEntry, elem__124 EnvEntry) struct{} {
    vec_push__Vec_8EnvEntry(self__123, elem__124)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__EnvEntry(self__129 *_goml_vec_EnvEntry, index__130 int32) EnvEntry {
    var retv776 EnvEntry
    var t777 EnvEntry = vec_get__Vec_8EnvEntry(self__129, index__130)
    retv776 = t777
    return retv776
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv779 string
    retv779 = self__37
    return retv779
}

func main() {
    main0()
}
