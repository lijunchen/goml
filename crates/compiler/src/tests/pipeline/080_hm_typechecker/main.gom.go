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
    var retv263 CheckerState
    var t264 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var t265 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(1)
    var t266 CheckerState = CheckerState{
        gensym_counter: t264,
        current_level: t265,
    }
    retv263 = t266
    return retv263
}

func reset_gensym(st__0 CheckerState) struct{} {
    var t268 *ref_int32_x = st__0.gensym_counter
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t268, 0)
    return struct{}{}
}

func reset_level(st__1 CheckerState) struct{} {
    var t270 *ref_int32_x = st__1.current_level
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t270, 1)
    return struct{}{}
}

func reset_type_variables(st__2 CheckerState) struct{} {
    reset_gensym(st__2)
    reset_level(st__2)
    return struct{}{}
}

func enter_level(st__3 CheckerState) struct{} {
    var t274 *ref_int32_x = st__3.current_level
    var l__4 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t274)
    var t275 *ref_int32_x = st__3.current_level
    var t276 int32 = l__4 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t275, t276)
    return struct{}{}
}

func leave_level(st__5 CheckerState) struct{} {
    var t278 *ref_int32_x = st__5.current_level
    var l__6 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t278)
    var t279 *ref_int32_x = st__5.current_level
    var t280 int32 = l__6 - 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t279, t280)
    return struct{}{}
}

func min_i32(a__7 int32, b__8 int32) int32 {
    var retv282 int32
    var t285 bool = a__7 < b__8
    var jp284 int32
    if t285 {
        jp284 = a__7
    } else {
        jp284 = b__8
    }
    retv282 = jp284
    return retv282
}

func nth_letter(n__9 int32) rune {
    var retv287 rune
    var jp289 rune
    switch n__9 {
    case 0:
        jp289 = 97
    case 1:
        jp289 = 98
    case 2:
        jp289 = 99
    case 3:
        jp289 = 100
    case 4:
        jp289 = 101
    case 5:
        jp289 = 102
    case 6:
        jp289 = 103
    case 7:
        jp289 = 104
    case 8:
        jp289 = 105
    case 9:
        jp289 = 106
    case 10:
        jp289 = 107
    case 11:
        jp289 = 108
    case 12:
        jp289 = 109
    case 13:
        jp289 = 110
    case 14:
        jp289 = 111
    case 15:
        jp289 = 112
    case 16:
        jp289 = 113
    case 17:
        jp289 = 114
    case 18:
        jp289 = 115
    case 19:
        jp289 = 116
    case 20:
        jp289 = 117
    case 21:
        jp289 = 118
    case 22:
        jp289 = 119
    case 23:
        jp289 = 120
    case 24:
        jp289 = 121
    case 25:
        jp289 = 122
    default:
        jp289 = 97
    }
    retv287 = jp289
    return retv287
}

func gensym(st__10 CheckerState) string {
    var retv291 string
    var t292 *ref_int32_x = st__10.gensym_counter
    var n__11 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t292)
    var t293 *ref_int32_x = st__10.gensym_counter
    var t294 int32 = n__11 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t293, t294)
    var t297 bool = n__11 < 26
    var jp296 string
    if t297 {
        var t298 rune = nth_letter(n__11)
        var t299 string = _goml_m_inherent_i_char_i_char_i_to__string(t298)
        jp296 = t299
    } else {
        var t300 string = _goml_m_inherent_i_int32_i_int32_i_to__string(n__11)
        var t301 string = "t" + t300
        jp296 = t301
    }
    retv291 = jp296
    return retv291
}

func newvar(st__12 CheckerState) Typ {
    var retv303 Typ
    var name__13 string = gensym(st__12)
    var t304 *ref_int32_x = st__12.current_level
    var level__14 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t304)
    var t305 Tv = Unbound{
        _0: name__13,
        _1: level__14,
    }
    var t306 *ref_Tv_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Tv(t305)
    var t307 Typ = TVar{
        _0: t306,
    }
    retv303 = t307
    return retv303
}

func typ_is_arrow(ty__15 Typ) bool {
    var retv309 bool
    var jp311 bool
    switch ty__15.(type) {
    case TVar:
        var x64 *ref_Tv_x = ty__15.(TVar)._0
        var tvref__16 *ref_Tv_x = x64
        var mtmp68 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvref__16)
        var jp313 bool
        switch mtmp68.(type) {
        case Unbound:
            jp313 = false
        case Link:
            var x71 Typ = mtmp68.(Link)._0
            var inner__17 Typ = x71
            var t314 bool = typ_is_arrow(inner__17)
            jp313 = t314
        default:
            panic("non-exhaustive match")
        }
        jp311 = jp313
    case QVar:
        jp311 = false
    case TArrow:
        jp311 = true
    default:
        panic("non-exhaustive match")
    }
    retv309 = jp311
    return retv309
}

func typ_to_string(ty__18 Typ) string {
    var retv316 string
    var jp318 string
    switch ty__18.(type) {
    case TVar:
        var x72 *ref_Tv_x = ty__18.(TVar)._0
        var tvref__20 *ref_Tv_x = x72
        var mtmp76 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvref__20)
        var jp320 string
        switch mtmp76.(type) {
        case Unbound:
            var x77 string = mtmp76.(Unbound)._0
            var name__21 string = x77
            var t321 string = "'" + name__21
            jp320 = t321
        case Link:
            var x79 Typ = mtmp76.(Link)._0
            var inner__22 Typ = x79
            var t322 string = typ_to_string(inner__22)
            jp320 = t322
        default:
            panic("non-exhaustive match")
        }
        jp318 = jp320
    case QVar:
        var x73 string = ty__18.(QVar)._0
        var name__19 string = x73
        var t323 string = "'" + name__19
        jp318 = t323
    case TArrow:
        var x74 Typ = ty__18.(TArrow)._0
        var x75 Typ = ty__18.(TArrow)._1
        var t2__24 Typ = x75
        var t1__23 Typ = x74
        var t328 bool = typ_is_arrow(t1__23)
        var jp325 string
        if t328 {
            var t329 string = typ_to_string(t1__23)
            var t330 string = "(" + t329
            var t331 string = t330 + ")"
            jp325 = t331
        } else {
            var t332 string = typ_to_string(t1__23)
            jp325 = t332
        }
        var s1__25 string = jp325
        var s2__26 string = typ_to_string(t2__24)
        var t326 string = s1__25 + " -> "
        var t327 string = t326 + s2__26
        jp318 = t327
    default:
        panic("non-exhaustive match")
    }
    retv316 = jp318
    return retv316
}

func env_empty() *_goml_vec_EnvEntry {
    var retv334 *_goml_vec_EnvEntry
    var env__27 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__EnvEntry()
    retv334 = env__27
    return retv334
}

func env_lookup(env__28 *_goml_vec_EnvEntry, name__29 string) Option__Typ {
    var retv336 Option__Typ
    var t337 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__EnvEntry(env__28)
    var t338 int32 = t337 - 1
    var i__30 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(t338)
    var found__31 *ref_Option__Typ_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_Typ_r_(None{})
    var done__32 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    Loop_loop341:
    for {
        var t354 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(done__32)
        var t355 bool = !t354
        var jp343 bool
        if t355 {
            var t356 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__30)
            var t357 bool = t356 >= 0
            jp343 = t357
        } else {
            jp343 = false
        }
        if jp343 {
            var t344 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__30)
            var entry__33 EnvEntry = vec_get__Vec_8EnvEntry(env__28, t344)
            var t346 string = entry__33.name
            var t347 bool = t346 == name__29
            if t347 {
                var t348 Typ = entry__33.ty
                var t349 Option__Typ = Some{
                    _0: t348,
                }
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Option_l_Typ_r_(found__31, t349)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(done__32, true)
            } else {
                var t351 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__30)
                var t352 int32 = t351 - 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__30, t352)
            }
            continue
        } else {
            break Loop_loop341
        }
    }
    var t340 Option__Typ = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Option_l_Typ_r_(found__31)
    retv336 = t340
    return retv336
}

func subst_lookup(subst__34 *_goml_vec_SubstEntry, name__35 string) Option__Typ {
    var retv359 Option__Typ
    var t360 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SubstEntry(subst__34)
    var t361 int32 = t360 - 1
    var i__36 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(t361)
    var found__37 *ref_Option__Typ_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_Typ_r_(None{})
    var done__38 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    Loop_loop364:
    for {
        var t377 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(done__38)
        var t378 bool = !t377
        var jp366 bool
        if t378 {
            var t379 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__36)
            var t380 bool = t379 >= 0
            jp366 = t380
        } else {
            jp366 = false
        }
        if jp366 {
            var t367 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__36)
            var entry__39 SubstEntry = vec_get__Vec_10SubstEntry(subst__34, t367)
            var t369 string = entry__39.name
            var t370 bool = t369 == name__35
            if t370 {
                var t371 Typ = entry__39.ty
                var t372 Option__Typ = Some{
                    _0: t371,
                }
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Option_l_Typ_r_(found__37, t372)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(done__38, true)
            } else {
                var t374 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__36)
                var t375 int32 = t374 - 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__36, t375)
            }
            continue
        } else {
            break Loop_loop364
        }
    }
    var t363 Option__Typ = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Option_l_Typ_r_(found__37)
    retv359 = t363
    return retv359
}

func occurs(st__40 CheckerState, tvr__41 *ref_Tv_x, ty__42 Typ) Result__unit__string {
    var retv382 Result__unit__string
    var jp384 Result__unit__string
    switch ty__42.(type) {
    case TVar:
        var x84 *ref_Tv_x = ty__42.(TVar)._0
        var tvr2__43 *ref_Tv_x = x84
        var t387 bool = ptr_eq__Ref_2Tv(tvr__41, tvr2__43)
        var jp386 Result__unit__string
        if t387 {
            var t388 Result__unit__string = Result__unit__string_Err{
                _0: "occurs check",
            }
            jp386 = t388
        } else {
            var mtmp88 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvr2__43)
            var jp390 Result__unit__string
            switch mtmp88.(type) {
            case Unbound:
                var x89 string = mtmp88.(Unbound)._0
                var x90 int32 = mtmp88.(Unbound)._1
                var l2__45 int32 = x90
                var name__44 string = x89
                var mtmp92 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvr__41)
                var jp392 int32
                switch mtmp92.(type) {
                case Unbound:
                    var x94 int32 = mtmp92.(Unbound)._1
                    var l__46 int32 = x94
                    var t395 int32 = min_i32(l__46, l2__45)
                    jp392 = t395
                case Link:
                    jp392 = l2__45
                default:
                    panic("non-exhaustive match")
                }
                var min_level__47 int32 = jp392
                var t393 Tv = Unbound{
                    _0: name__44,
                    _1: min_level__47,
                }
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(tvr2__43, t393)
                var t394 Result__unit__string = Result__unit__string_Ok{
                    _0: struct{}{},
                }
                jp390 = t394
            case Link:
                var x91 Typ = mtmp88.(Link)._0
                var inner__48 Typ = x91
                var t396 Result__unit__string = occurs(st__40, tvr__41, inner__48)
                jp390 = t396
            default:
                panic("non-exhaustive match")
            }
            jp386 = jp390
        }
        jp384 = jp386
    case QVar:
        var t397 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp384 = t397
    case TArrow:
        var x86 Typ = ty__42.(TArrow)._0
        var x87 Typ = ty__42.(TArrow)._1
        var t2__50 Typ = x87
        var t1__49 Typ = x86
        var mtmp97 Result__unit__string = occurs(st__40, tvr__41, t1__49)
        var jp399 Result__unit__string
        switch mtmp97.(type) {
        case Result__unit__string_Ok:
            var t400 Result__unit__string = occurs(st__40, tvr__41, t2__50)
            jp399 = t400
        case Result__unit__string_Err:
            var x99 string = mtmp97.(Result__unit__string_Err)._0
            var e__51 string = x99
            var t401 Result__unit__string = Result__unit__string_Err{
                _0: e__51,
            }
            jp399 = t401
        default:
            panic("non-exhaustive match")
        }
        jp384 = jp399
    default:
        panic("non-exhaustive match")
    }
    retv382 = jp384
    return retv382
}

func unify(st__52 CheckerState, t1__53 Typ, t2__54 Typ) Result__unit__string {
    var retv403 Result__unit__string
    var mtmp100 Tuple2_3Typ_3Typ = Tuple2_3Typ_3Typ{
        _0: t1__53,
        _1: t2__54,
    }
    var x101 Typ = mtmp100._0
    var x102 Typ = mtmp100._1
    var jp405 Result__unit__string
    switch x102.(type) {
    case TVar:
        var x103 *ref_Tv_x = x102.(TVar)._0
        var jp407 Result__unit__string
        switch x101.(type) {
        case TVar:
            var x107 *ref_Tv_x = x101.(TVar)._0
            var r1__55 *ref_Tv_x = x107
            var r2__56 *ref_Tv_x = x103
            var t410 bool = ptr_eq__Ref_2Tv(r1__55, r2__56)
            var jp409 Result__unit__string
            if t410 {
                var t411 Result__unit__string = Result__unit__string_Ok{
                    _0: struct{}{},
                }
                jp409 = t411
            } else {
                var mtmp111 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r1__55)
                var jp413 Result__unit__string
                switch mtmp111.(type) {
                case Unbound:
                    var mtmp115 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r2__56)
                    var jp415 Result__unit__string
                    switch mtmp115.(type) {
                    case Unbound:
                        var t416 Typ = TVar{
                            _0: r2__56,
                        }
                        var mtmp119 Result__unit__string = occurs(st__52, r1__55, t416)
                        var jp418 Result__unit__string
                        switch mtmp119.(type) {
                        case Result__unit__string_Ok:
                            var t419 Typ = TVar{
                                _0: r2__56,
                            }
                            var t420 Tv = Link{
                                _0: t419,
                            }
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(r1__55, t420)
                            var t421 Result__unit__string = Result__unit__string_Ok{
                                _0: struct{}{},
                            }
                            jp418 = t421
                        case Result__unit__string_Err:
                            var x121 string = mtmp119.(Result__unit__string_Err)._0
                            var e__59 string = x121
                            var t422 Result__unit__string = Result__unit__string_Err{
                                _0: e__59,
                            }
                            jp418 = t422
                        default:
                            panic("non-exhaustive match")
                        }
                        jp415 = jp418
                    case Link:
                        var x118 Typ = mtmp115.(Link)._0
                        var inner__58 Typ = x118
                        var t423 Typ = TVar{
                            _0: r1__55,
                        }
                        var t424 Result__unit__string = unify(st__52, t423, inner__58)
                        jp415 = t424
                    default:
                        panic("non-exhaustive match")
                    }
                    jp413 = jp415
                case Link:
                    var x114 Typ = mtmp111.(Link)._0
                    var inner__57 Typ = x114
                    var t425 Typ = TVar{
                        _0: r2__56,
                    }
                    var t426 Result__unit__string = unify(st__52, inner__57, t425)
                    jp413 = t426
                default:
                    panic("non-exhaustive match")
                }
                jp409 = jp413
            }
            jp407 = jp409
        case QVar:
            var r2__65 *ref_Tv_x = x103
            var other__64 Typ = x101
            var mtmp123 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r2__65)
            var jp428 Result__unit__string
            switch mtmp123.(type) {
            case Unbound:
                var mtmp127 Result__unit__string = occurs(st__52, r2__65, other__64)
                var jp430 Result__unit__string
                switch mtmp127.(type) {
                case Result__unit__string_Ok:
                    var t431 Tv = Link{
                        _0: other__64,
                    }
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(r2__65, t431)
                    var t432 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    jp430 = t432
                case Result__unit__string_Err:
                    var x129 string = mtmp127.(Result__unit__string_Err)._0
                    var e__67 string = x129
                    var t433 Result__unit__string = Result__unit__string_Err{
                        _0: e__67,
                    }
                    jp430 = t433
                default:
                    panic("non-exhaustive match")
                }
                jp428 = jp430
            case Link:
                var x126 Typ = mtmp123.(Link)._0
                var inner__66 Typ = x126
                var t434 Result__unit__string = unify(st__52, other__64, inner__66)
                jp428 = t434
            default:
                panic("non-exhaustive match")
            }
            jp407 = jp428
        case TArrow:
            var r2__65 *ref_Tv_x = x103
            var other__64 Typ = x101
            var mtmp131 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r2__65)
            var jp436 Result__unit__string
            switch mtmp131.(type) {
            case Unbound:
                var mtmp135 Result__unit__string = occurs(st__52, r2__65, other__64)
                var jp438 Result__unit__string
                switch mtmp135.(type) {
                case Result__unit__string_Ok:
                    var t439 Tv = Link{
                        _0: other__64,
                    }
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(r2__65, t439)
                    var t440 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    jp438 = t440
                case Result__unit__string_Err:
                    var x137 string = mtmp135.(Result__unit__string_Err)._0
                    var e__67 string = x137
                    var t441 Result__unit__string = Result__unit__string_Err{
                        _0: e__67,
                    }
                    jp438 = t441
                default:
                    panic("non-exhaustive match")
                }
                jp436 = jp438
            case Link:
                var x134 Typ = mtmp131.(Link)._0
                var inner__66 Typ = x134
                var t442 Result__unit__string = unify(st__52, other__64, inner__66)
                jp436 = t442
            default:
                panic("non-exhaustive match")
            }
            jp407 = jp436
        default:
            panic("non-exhaustive match")
        }
        jp405 = jp407
    case QVar:
        var jp444 Result__unit__string
        switch x101.(type) {
        case TVar:
            var x139 *ref_Tv_x = x101.(TVar)._0
            var r1__60 *ref_Tv_x = x139
            var other__61 Typ = x102
            var mtmp143 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r1__60)
            var jp446 Result__unit__string
            switch mtmp143.(type) {
            case Unbound:
                var mtmp147 Result__unit__string = occurs(st__52, r1__60, other__61)
                var jp448 Result__unit__string
                switch mtmp147.(type) {
                case Result__unit__string_Ok:
                    var t449 Tv = Link{
                        _0: other__61,
                    }
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(r1__60, t449)
                    var t450 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    jp448 = t450
                case Result__unit__string_Err:
                    var x149 string = mtmp147.(Result__unit__string_Err)._0
                    var e__63 string = x149
                    var t451 Result__unit__string = Result__unit__string_Err{
                        _0: e__63,
                    }
                    jp448 = t451
                default:
                    panic("non-exhaustive match")
                }
                jp446 = jp448
            case Link:
                var x146 Typ = mtmp143.(Link)._0
                var inner__62 Typ = x146
                var t452 Result__unit__string = unify(st__52, inner__62, other__61)
                jp446 = t452
            default:
                panic("non-exhaustive match")
            }
            jp444 = jp446
        case QVar:
            var t453 Result__unit__string = Result__unit__string_Err{
                _0: "unify error",
            }
            jp444 = t453
        case TArrow:
            var t454 Result__unit__string = Result__unit__string_Err{
                _0: "unify error",
            }
            jp444 = t454
        default:
            panic("non-exhaustive match")
        }
        jp405 = jp444
    case TArrow:
        var x105 Typ = x102.(TArrow)._0
        var x106 Typ = x102.(TArrow)._1
        var jp456 Result__unit__string
        switch x101.(type) {
        case TVar:
            var x151 *ref_Tv_x = x101.(TVar)._0
            var r1__60 *ref_Tv_x = x151
            var other__61 Typ = x102
            var mtmp155 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r1__60)
            var jp458 Result__unit__string
            switch mtmp155.(type) {
            case Unbound:
                var mtmp159 Result__unit__string = occurs(st__52, r1__60, other__61)
                var jp460 Result__unit__string
                switch mtmp159.(type) {
                case Result__unit__string_Ok:
                    var t461 Tv = Link{
                        _0: other__61,
                    }
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(r1__60, t461)
                    var t462 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    jp460 = t462
                case Result__unit__string_Err:
                    var x161 string = mtmp159.(Result__unit__string_Err)._0
                    var e__63 string = x161
                    var t463 Result__unit__string = Result__unit__string_Err{
                        _0: e__63,
                    }
                    jp460 = t463
                default:
                    panic("non-exhaustive match")
                }
                jp458 = jp460
            case Link:
                var x158 Typ = mtmp155.(Link)._0
                var inner__62 Typ = x158
                var t464 Result__unit__string = unify(st__52, inner__62, other__61)
                jp458 = t464
            default:
                panic("non-exhaustive match")
            }
            jp456 = jp458
        case QVar:
            var t465 Result__unit__string = Result__unit__string_Err{
                _0: "unify error",
            }
            jp456 = t465
        case TArrow:
            var x153 Typ = x101.(TArrow)._0
            var x154 Typ = x101.(TArrow)._1
            var a2__69 Typ = x154
            var a1__68 Typ = x153
            var b2__71 Typ = x106
            var b1__70 Typ = x105
            var mtmp163 Result__unit__string = unify(st__52, a1__68, b1__70)
            var jp467 Result__unit__string
            switch mtmp163.(type) {
            case Result__unit__string_Ok:
                var t468 Result__unit__string = unify(st__52, a2__69, b2__71)
                jp467 = t468
            case Result__unit__string_Err:
                var x165 string = mtmp163.(Result__unit__string_Err)._0
                var e__72 string = x165
                var t469 Result__unit__string = Result__unit__string_Err{
                    _0: e__72,
                }
                jp467 = t469
            default:
                panic("non-exhaustive match")
            }
            jp456 = jp467
        default:
            panic("non-exhaustive match")
        }
        jp405 = jp456
    default:
        panic("non-exhaustive match")
    }
    retv403 = jp405
    return retv403
}

func gen(st__73 CheckerState, ty__74 Typ) Typ {
    var retv471 Typ
    var jp473 Typ
    switch ty__74.(type) {
    case TVar:
        var x166 *ref_Tv_x = ty__74.(TVar)._0
        var tvref__75 *ref_Tv_x = x166
        var mtmp170 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvref__75)
        var jp475 Typ
        switch mtmp170.(type) {
        case Unbound:
            var x171 string = mtmp170.(Unbound)._0
            var x172 int32 = mtmp170.(Unbound)._1
            var l__77 int32 = x172
            var name__76 string = x171
            var t476 *ref_int32_x = st__73.current_level
            var cur__78 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t476)
            var t479 bool = l__77 > cur__78
            var jp478 Typ
            if t479 {
                var t480 Typ = QVar{
                    _0: name__76,
                }
                jp478 = t480
            } else {
                var t481 Typ = TVar{
                    _0: tvref__75,
                }
                jp478 = t481
            }
            jp475 = jp478
        case Link:
            var x173 Typ = mtmp170.(Link)._0
            var inner__79 Typ = x173
            var t482 Typ = gen(st__73, inner__79)
            jp475 = t482
        default:
            panic("non-exhaustive match")
        }
        jp473 = jp475
    case QVar:
        var other__82 Typ = ty__74
        jp473 = other__82
    case TArrow:
        var x168 Typ = ty__74.(TArrow)._0
        var x169 Typ = ty__74.(TArrow)._1
        var t2__81 Typ = x169
        var t1__80 Typ = x168
        var t483 Typ = gen(st__73, t1__80)
        var t484 Typ = gen(st__73, t2__81)
        var t485 Typ = TArrow{
            _0: t483,
            _1: t484,
        }
        jp473 = t485
    default:
        panic("non-exhaustive match")
    }
    retv471 = jp473
    return retv471
}

func inst_loop(st__83 CheckerState, subst__84 *_goml_vec_SubstEntry, ty__85 Typ) Tuple2_3Typ_16Vec_10SubstEntry {
    var retv487 Tuple2_3Typ_16Vec_10SubstEntry
    var jp489 Tuple2_3Typ_16Vec_10SubstEntry
    switch ty__85.(type) {
    case TVar:
        var x174 *ref_Tv_x = ty__85.(TVar)._0
        var tvref__90 *ref_Tv_x = x174
        var mtmp178 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvref__90)
        var jp491 Tuple2_3Typ_16Vec_10SubstEntry
        switch mtmp178.(type) {
        case Unbound:
            var t492 Typ = TVar{
                _0: tvref__90,
            }
            var t493 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: t492,
                _1: subst__84,
            }
            jp491 = t493
        case Link:
            var x181 Typ = mtmp178.(Link)._0
            var inner__91 Typ = x181
            var t494 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, subst__84, inner__91)
            jp491 = t494
        default:
            panic("non-exhaustive match")
        }
        jp489 = jp491
    case QVar:
        var x175 string = ty__85.(QVar)._0
        var name__86 string = x175
        var mtmp182 Option__Typ = subst_lookup(subst__84, name__86)
        var jp496 Tuple2_3Typ_16Vec_10SubstEntry
        switch mtmp182.(type) {
        case None:
            var tv__88 Typ = newvar(st__83)
            var t497 SubstEntry = SubstEntry{
                name: name__86,
                ty: tv__88,
            }
            var new_subst__89 *_goml_vec_SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SubstEntry(subst__84, t497)
            var t498 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: tv__88,
                _1: new_subst__89,
            }
            jp496 = t498
        case Some:
            var x183 Typ = mtmp182.(Some)._0
            var t__87 Typ = x183
            var t499 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: t__87,
                _1: subst__84,
            }
            jp496 = t499
        default:
            panic("non-exhaustive match")
        }
        jp489 = jp496
    case TArrow:
        var x176 Typ = ty__85.(TArrow)._0
        var x177 Typ = ty__85.(TArrow)._1
        var t2__93 Typ = x177
        var t1__92 Typ = x176
        var mtmp184 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, subst__84, t1__92)
        var x185 Typ = mtmp184._0
        var x186 *_goml_vec_SubstEntry = mtmp184._1
        var subst1__95 *_goml_vec_SubstEntry = x186
        var ty1__94 Typ = x185
        var mtmp187 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, subst1__95, t2__93)
        var x188 Typ = mtmp187._0
        var x189 *_goml_vec_SubstEntry = mtmp187._1
        var subst2__97 *_goml_vec_SubstEntry = x189
        var ty2__96 Typ = x188
        var t500 Typ = TArrow{
            _0: ty1__94,
            _1: ty2__96,
        }
        var t501 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
            _0: t500,
            _1: subst2__97,
        }
        jp489 = t501
    default:
        panic("non-exhaustive match")
    }
    retv487 = jp489
    return retv487
}

func inst(st__98 CheckerState, ty__99 Typ) Typ {
    var retv503 Typ
    var subst0__100 *_goml_vec_SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SubstEntry()
    var mtmp190 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__98, subst0__100, ty__99)
    var x191 Typ = mtmp190._0
    var t__101 Typ = x191
    retv503 = t__101
    return retv503
}

func typeof(st__102 CheckerState, env__103 *_goml_vec_EnvEntry, e__104 Exp) Result__Typ__string {
    var retv505 Result__Typ__string
    var jp507 Result__Typ__string
    switch e__104.(type) {
    case Var:
        var x193 string = e__104.(Var)._0
        var x__105 string = x193
        var mtmp201 Option__Typ = env_lookup(env__103, x__105)
        var jp509 Result__Typ__string
        switch mtmp201.(type) {
        case None:
            var t510 Result__Typ__string = Result__Typ__string_Err{
                _0: "unbound var",
            }
            jp509 = t510
        case Some:
            var x202 Typ = mtmp201.(Some)._0
            var ty__106 Typ = x202
            var t511 Typ = inst(st__102, ty__106)
            var t512 Result__Typ__string = Result__Typ__string_Ok{
                _0: t511,
            }
            jp509 = t512
        default:
            panic("non-exhaustive match")
        }
        jp507 = jp509
    case App:
        var x194 Exp = e__104.(App)._0
        var x195 Exp = e__104.(App)._1
        var e2__114 Exp = x195
        var e1__113 Exp = x194
        var mtmp203 Result__Typ__string = typeof(st__102, env__103, e1__113)
        var jp514 Result__Typ__string
        switch mtmp203.(type) {
        case Result__Typ__string_Ok:
            var x204 Typ = mtmp203.(Result__Typ__string_Ok)._0
            var ty_fun__116 Typ = x204
            var mtmp206 Result__Typ__string = typeof(st__102, env__103, e2__114)
            var jp516 Result__Typ__string
            switch mtmp206.(type) {
            case Result__Typ__string_Ok:
                var x207 Typ = mtmp206.(Result__Typ__string_Ok)._0
                var ty_arg__118 Typ = x207
                var ty_res__119 Typ = newvar(st__102)
                var arrow__120 Typ = TArrow{
                    _0: ty_arg__118,
                    _1: ty_res__119,
                }
                var mtmp209 Result__unit__string = unify(st__102, ty_fun__116, arrow__120)
                var jp518 Result__Typ__string
                switch mtmp209.(type) {
                case Result__unit__string_Ok:
                    var t519 Result__Typ__string = Result__Typ__string_Ok{
                        _0: ty_res__119,
                    }
                    jp518 = t519
                case Result__unit__string_Err:
                    var x211 string = mtmp209.(Result__unit__string_Err)._0
                    var e__121 string = x211
                    var t520 Result__Typ__string = Result__Typ__string_Err{
                        _0: e__121,
                    }
                    jp518 = t520
                default:
                    panic("non-exhaustive match")
                }
                jp516 = jp518
            case Result__Typ__string_Err:
                var x208 string = mtmp206.(Result__Typ__string_Err)._0
                var e__117 string = x208
                var t521 Result__Typ__string = Result__Typ__string_Err{
                    _0: e__117,
                }
                jp516 = t521
            default:
                panic("non-exhaustive match")
            }
            jp514 = jp516
        case Result__Typ__string_Err:
            var x205 string = mtmp203.(Result__Typ__string_Err)._0
            var e__115 string = x205
            var t522 Result__Typ__string = Result__Typ__string_Err{
                _0: e__115,
            }
            jp514 = t522
        default:
            panic("non-exhaustive match")
        }
        jp507 = jp514
    case Lam:
        var x196 string = e__104.(Lam)._0
        var x197 Exp = e__104.(Lam)._1
        var body__108 Exp = x197
        var x__107 string = x196
        var ty_x__109 Typ = newvar(st__102)
        var t523 EnvEntry = EnvEntry{
            name: x__107,
            ty: ty_x__109,
        }
        var env2__110 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__EnvEntry(env__103, t523)
        var mtmp212 Result__Typ__string = typeof(st__102, env2__110, body__108)
        var jp525 Result__Typ__string
        switch mtmp212.(type) {
        case Result__Typ__string_Ok:
            var x213 Typ = mtmp212.(Result__Typ__string_Ok)._0
            var ty_e__111 Typ = x213
            var t526 Typ = TArrow{
                _0: ty_x__109,
                _1: ty_e__111,
            }
            var t527 Result__Typ__string = Result__Typ__string_Ok{
                _0: t526,
            }
            jp525 = t527
        case Result__Typ__string_Err:
            var x214 string = mtmp212.(Result__Typ__string_Err)._0
            var e__112 string = x214
            var t528 Result__Typ__string = Result__Typ__string_Err{
                _0: e__112,
            }
            jp525 = t528
        default:
            panic("non-exhaustive match")
        }
        jp507 = jp525
    case Let:
        var x198 string = e__104.(Let)._0
        var x199 Exp = e__104.(Let)._1
        var x200 Exp = e__104.(Let)._2
        var e2__124 Exp = x200
        var e1__123 Exp = x199
        var x__122 string = x198
        enter_level(st__102)
        var ty_e__125 Result__Typ__string = typeof(st__102, env__103, e1__123)
        leave_level(st__102)
        var jp530 Result__Typ__string
        switch ty_e__125.(type) {
        case Result__Typ__string_Ok:
            var x217 Typ = ty_e__125.(Result__Typ__string_Ok)._0
            var ty1__127 Typ = x217
            var t531 Typ = gen(st__102, ty1__127)
            var t532 EnvEntry = EnvEntry{
                name: x__122,
                ty: t531,
            }
            var env2__128 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__EnvEntry(env__103, t532)
            var t533 Result__Typ__string = typeof(st__102, env2__128, e2__124)
            jp530 = t533
        case Result__Typ__string_Err:
            var x218 string = ty_e__125.(Result__Typ__string_Err)._0
            var e__126 string = x218
            var t534 Result__Typ__string = Result__Typ__string_Err{
                _0: e__126,
            }
            jp530 = t534
        default:
            panic("non-exhaustive match")
        }
        jp507 = jp530
    default:
        panic("non-exhaustive match")
    }
    retv505 = jp507
    return retv505
}

func exp_var(name__129 string) Exp {
    var retv536 Exp
    var t537 Exp = Var{
        _0: name__129,
    }
    retv536 = t537
    return retv536
}

func exp_lam(name__130 string, body__131 Exp) Exp {
    var retv539 Exp
    var t540 Exp = Lam{
        _0: name__130,
        _1: body__131,
    }
    retv539 = t540
    return retv539
}

func exp_app(a__132 Exp, b__133 Exp) Exp {
    var retv542 Exp
    var t543 Exp = App{
        _0: a__132,
        _1: b__133,
    }
    retv542 = t543
    return retv542
}

func exp_let(name__134 string, a__135 Exp, b__136 Exp) Exp {
    var retv545 Exp
    var t546 Exp = Let{
        _0: name__134,
        _1: a__135,
        _2: b__136,
    }
    retv545 = t546
    return retv545
}

func show_result(label__137 string, res__138 Result__Typ__string) struct{} {
    switch res__138.(type) {
    case Result__Typ__string_Ok:
        var x219 Typ = res__138.(Result__Typ__string_Ok)._0
        var ty__139 Typ = x219
        var t549 string = label__137 + ": "
        var t550 string = typ_to_string(ty__139)
        var t551 string = t549 + t550
        println__T_string(t551)
    case Result__Typ__string_Err:
        var x220 string = res__138.(Result__Typ__string_Err)._0
        var e__140 string = x220
        var t553 string = label__137 + ": "
        var t554 string = t553 + e__140
        println__T_string(t554)
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func main0() struct{} {
    var st__141 CheckerState = state_new()
    var t557 Exp = exp_var("x")
    var id__142 Exp = exp_lam("x", t557)
    var t558 Exp = exp_var("x")
    var t559 Exp = exp_var("y")
    var t560 Exp = exp_app(t558, t559)
    var t561 Exp = exp_lam("y", t560)
    var c1__143 Exp = exp_lam("x", t561)
    reset_type_variables(st__141)
    var t562 *_goml_vec_EnvEntry = env_empty()
    var t563 Result__Typ__string = typeof(st__141, t562, id__142)
    show_result("id", t563)
    reset_type_variables(st__141)
    var t564 *_goml_vec_EnvEntry = env_empty()
    var t565 Result__Typ__string = typeof(st__141, t564, c1__143)
    show_result("c1", t565)
    reset_type_variables(st__141)
    var t566 *_goml_vec_EnvEntry = env_empty()
    var t567 Exp = exp_var("x")
    var t568 Exp = exp_let("x", c1__143, t567)
    var t569 Result__Typ__string = typeof(st__141, t566, t568)
    show_result("let_x_c1_x", t569)
    reset_type_variables(st__141)
    var t570 *_goml_vec_EnvEntry = env_empty()
    var t571 Exp = exp_var("z")
    var t572 Exp = exp_lam("z", t571)
    var t573 Exp = exp_var("y")
    var t574 Exp = exp_let("y", t572, t573)
    var t575 Result__Typ__string = typeof(st__141, t570, t574)
    show_result("let_y_id_y", t575)
    reset_type_variables(st__141)
    var t576 *_goml_vec_EnvEntry = env_empty()
    var t577 Exp = exp_var("z")
    var t578 Exp = exp_lam("z", t577)
    var t579 Exp = exp_var("y")
    var t580 Exp = exp_let("y", t578, t579)
    var t581 Exp = exp_lam("x", t580)
    var t582 Result__Typ__string = typeof(st__141, t576, t581)
    show_result("lam_x_let_y_id_y", t582)
    reset_type_variables(st__141)
    var t583 *_goml_vec_EnvEntry = env_empty()
    var t584 Exp = exp_var("z")
    var t585 Exp = exp_lam("z", t584)
    var t586 Exp = exp_var("y")
    var t587 Exp = exp_var("x")
    var t588 Exp = exp_app(t586, t587)
    var t589 Exp = exp_let("y", t585, t588)
    var t590 Exp = exp_lam("x", t589)
    var t591 Result__Typ__string = typeof(st__141, t583, t590)
    show_result("lam_x_let_y_id_yx", t591)
    reset_type_variables(st__141)
    var t592 *_goml_vec_EnvEntry = env_empty()
    var t593 Exp = exp_var("x")
    var t594 Exp = exp_var("x")
    var t595 Exp = exp_app(t593, t594)
    var t596 Exp = exp_lam("x", t595)
    var t597 Result__Typ__string = typeof(st__141, t592, t596)
    show_result("self_apply", t597)
    reset_type_variables(st__141)
    var t598 *_goml_vec_EnvEntry = env_empty()
    var t599 Exp = exp_var("x")
    var t600 Exp = exp_var("x")
    var t601 Exp = exp_let("x", t599, t600)
    var t602 Result__Typ__string = typeof(st__141, t598, t601)
    show_result("unbound_var", t602)
    reset_type_variables(st__141)
    var t603 *_goml_vec_EnvEntry = env_empty()
    var t604 Exp = exp_var("y")
    var t605 Exp = exp_var("y")
    var t606 Exp = exp_var("z")
    var t607 Exp = exp_app(t605, t606)
    var t608 Exp = exp_lam("z", t607)
    var t609 Exp = exp_app(t604, t608)
    var t610 Exp = exp_lam("y", t609)
    var t611 Result__Typ__string = typeof(st__141, t603, t610)
    show_result("max_heiber", t611)
    reset_type_variables(st__141)
    var t612 *_goml_vec_EnvEntry = env_empty()
    var t613 Exp = exp_var("k")
    var t614 Exp = exp_var("k")
    var t615 Exp = exp_var("x")
    var t616 Exp = exp_app(t614, t615)
    var t617 Exp = exp_var("y")
    var t618 Exp = exp_app(t616, t617)
    var t619 Exp = exp_app(t613, t618)
    var t620 Exp = exp_var("k")
    var t621 Exp = exp_var("y")
    var t622 Exp = exp_app(t620, t621)
    var t623 Exp = exp_var("x")
    var t624 Exp = exp_app(t622, t623)
    var t625 Exp = exp_app(t619, t624)
    var t626 Exp = exp_lam("k", t625)
    var t627 Exp = exp_lam("y", t626)
    var t628 Exp = exp_lam("x", t627)
    var t629 Result__Typ__string = typeof(st__141, t612, t628)
    show_result("kirang", t629)
    reset_type_variables(st__141)
    var t630 *_goml_vec_EnvEntry = env_empty()
    var t631 Exp = exp_var("id")
    var t632 Exp = exp_var("id")
    var t633 Exp = exp_app(t631, t632)
    var t634 Exp = exp_let("id", id__142, t633)
    var t635 Result__Typ__string = typeof(st__141, t630, t634)
    show_result("let_id_idid", t635)
    reset_type_variables(st__141)
    var t636 *_goml_vec_EnvEntry = env_empty()
    var t637 Exp = exp_var("x")
    var t638 Exp = exp_app(t637, id__142)
    var t639 Exp = exp_var("z")
    var t640 Exp = exp_let("z", t638, t639)
    var t641 Exp = exp_var("y")
    var t642 Exp = exp_let("y", t640, t641)
    var t643 Exp = exp_let("x", c1__143, t642)
    var t644 Result__Typ__string = typeof(st__141, t636, t643)
    show_result("nested_lets", t644)
    reset_type_variables(st__141)
    var t645 *_goml_vec_EnvEntry = env_empty()
    var t646 Exp = exp_var("x")
    var t647 Exp = exp_var("y")
    var t648 Exp = exp_app(t646, t647)
    var t649 Exp = exp_var("y")
    var t650 Exp = exp_var("x")
    var t651 Exp = exp_app(t649, t650)
    var t652 Exp = exp_lam("x", t651)
    var t653 Exp = exp_let("x", t648, t652)
    var t654 Exp = exp_lam("y", t653)
    var t655 Exp = exp_lam("x", t654)
    var t656 Result__Typ__string = typeof(st__141, t645, t655)
    show_result("fun_x_fun_y_let_x_xy_fun_x_yx", t656)
    reset_type_variables(st__141)
    var t657 *_goml_vec_EnvEntry = env_empty()
    var t658 Exp = exp_var("x")
    var t659 Exp = exp_var("y")
    var t660 Exp = exp_let("y", t658, t659)
    var t661 Exp = exp_lam("x", t660)
    var t662 Result__Typ__string = typeof(st__141, t657, t661)
    show_result("sound_gen_1", t662)
    reset_type_variables(st__141)
    var t663 *_goml_vec_EnvEntry = env_empty()
    var t664 Exp = exp_var("x")
    var t665 Exp = exp_lam("z", t664)
    var t666 Exp = exp_var("y")
    var t667 Exp = exp_let("y", t665, t666)
    var t668 Exp = exp_lam("x", t667)
    var t669 Result__Typ__string = typeof(st__141, t663, t668)
    show_result("sound_gen_2", t669)
    reset_type_variables(st__141)
    var t670 *_goml_vec_EnvEntry = env_empty()
    var t671 Exp = exp_var("x")
    var t672 Exp = exp_var("z")
    var t673 Exp = exp_app(t671, t672)
    var t674 Exp = exp_lam("z", t673)
    var t675 Exp = exp_var("y")
    var t676 Exp = exp_let("y", t674, t675)
    var t677 Exp = exp_lam("x", t676)
    var t678 Result__Typ__string = typeof(st__141, t670, t677)
    show_result("sound_gen_3", t678)
    reset_type_variables(st__141)
    var t679 *_goml_vec_EnvEntry = env_empty()
    var t680 Exp = exp_var("x")
    var t681 Exp = exp_var("y")
    var t682 Exp = exp_app(t680, t681)
    var t683 Exp = exp_var("x")
    var t684 Exp = exp_var("y")
    var t685 Exp = exp_app(t683, t684)
    var t686 Exp = exp_let("x", t682, t685)
    var t687 Exp = exp_lam("y", t686)
    var t688 Exp = exp_lam("x", t687)
    var t689 Result__Typ__string = typeof(st__141, t679, t688)
    show_result("double_apply", t689)
    reset_type_variables(st__141)
    var t690 *_goml_vec_EnvEntry = env_empty()
    var t691 Exp = exp_var("x")
    var t692 Exp = exp_var("y")
    var t693 Exp = exp_var("y")
    var t694 Exp = exp_app(t692, t693)
    var t695 Exp = exp_let("y", t691, t694)
    var t696 Exp = exp_lam("x", t695)
    var t697 Result__Typ__string = typeof(st__141, t690, t696)
    show_result("sound_gen_occurs", t697)
    reset_gensym(st__141)
    var t698 *_goml_vec_EnvEntry = env_empty()
    var t699 Exp = exp_var("x")
    var t700 Exp = exp_app(t699, id__142)
    var t701 Exp = exp_var("z")
    var t702 Exp = exp_let("z", t700, t701)
    var t703 Exp = exp_var("y")
    var t704 Exp = exp_let("y", t702, t703)
    var t705 Exp = exp_lam("x", t704)
    var t706 Result__Typ__string = typeof(st__141, t698, t705)
    show_result("fun_x_let_y_let_z_x_id_z_y", t706)
    println__T_string("")
    println__T_string("All Done")
    println__T_string("")
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__201 int32) *ref_int32_x {
    var retv708 *ref_int32_x
    var t709 *ref_int32_x = ref__Ref_5int32(value__201)
    retv708 = t709
    return retv708
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__203 *ref_int32_x, value__204 int32) struct{} {
    ref_set__Ref_5int32(self__203, value__204)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__202 *ref_int32_x) int32 {
    var retv713 int32
    var t714 int32 = ref_get__Ref_5int32(self__202)
    retv713 = t714
    return retv713
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__3 rune) string {
    var retv716 string
    var t717 string = _goml_runtime_core_char_to_string(self__3)
    retv716 = t717
    return retv716
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv719 string
    var t720 string = _goml_runtime_core_int32_to_string(self__2)
    retv719 = t720
    return retv719
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Tv(value__201 Tv) *ref_Tv_x {
    var retv722 *ref_Tv_x
    var t723 *ref_Tv_x = ref__Ref_2Tv(value__201)
    retv722 = t723
    return retv722
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(self__202 *ref_Tv_x) Tv {
    var retv725 Tv
    var t726 Tv = ref_get__Ref_2Tv(self__202)
    retv725 = t726
    return retv725
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__EnvEntry() *_goml_vec_EnvEntry {
    var retv728 *_goml_vec_EnvEntry
    var t729 *_goml_vec_EnvEntry = vec_new__Vec_8EnvEntry()
    retv728 = t729
    return retv728
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__EnvEntry(self__131 *_goml_vec_EnvEntry) int32 {
    var retv731 int32
    var t732 int32 = vec_len__Vec_8EnvEntry(self__131)
    retv731 = t732
    return retv731
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_Typ_r_(value__201 Option__Typ) *ref_Option__Typ_x {
    var retv734 *ref_Option__Typ_x
    var t735 *ref_Option__Typ_x = ref__Ref_11Option__Typ(value__201)
    retv734 = t735
    return retv734
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__201 bool) *ref_bool_x {
    var retv737 *ref_bool_x
    var t738 *ref_bool_x = ref__Ref_4bool(value__201)
    retv737 = t738
    return retv737
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__202 *ref_bool_x) bool {
    var retv740 bool
    var t741 bool = ref_get__Ref_4bool(self__202)
    retv740 = t741
    return retv740
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Option_l_Typ_r_(self__203 *ref_Option__Typ_x, value__204 Option__Typ) struct{} {
    ref_set__Ref_11Option__Typ(self__203, value__204)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(self__203 *ref_bool_x, value__204 bool) struct{} {
    ref_set__Ref_4bool(self__203, value__204)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Option_l_Typ_r_(self__202 *ref_Option__Typ_x) Option__Typ {
    var retv747 Option__Typ
    var t748 Option__Typ = ref_get__Ref_11Option__Typ(self__202)
    retv747 = t748
    return retv747
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SubstEntry(self__131 *_goml_vec_SubstEntry) int32 {
    var retv750 int32
    var t751 int32 = vec_len__Vec_10SubstEntry(self__131)
    retv750 = t751
    return retv750
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(self__203 *ref_Tv_x, value__204 Tv) struct{} {
    ref_set__Ref_2Tv(self__203, value__204)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SubstEntry(self__122 *_goml_vec_SubstEntry, elem__123 SubstEntry) *_goml_vec_SubstEntry {
    var retv755 *_goml_vec_SubstEntry
    var result__124 *_goml_vec_SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SubstEntry()
    var index__125 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop757:
    for {
        var t758 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__125)
        var t759 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SubstEntry(self__122)
        var t760 bool = t758 < t759
        if t760 {
            var t761 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__125)
            var t762 SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__SubstEntry(self__122, t761)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SubstEntry(result__124, t762)
            var t763 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__125)
            var t764 int32 = t763 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__125, t764)
            continue
        } else {
            break Loop_loop757
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SubstEntry(result__124, elem__123)
    retv755 = result__124
    return retv755
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SubstEntry() *_goml_vec_SubstEntry {
    var retv766 *_goml_vec_SubstEntry
    var t767 *_goml_vec_SubstEntry = vec_new__Vec_10SubstEntry()
    retv766 = t767
    return retv766
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__EnvEntry(self__122 *_goml_vec_EnvEntry, elem__123 EnvEntry) *_goml_vec_EnvEntry {
    var retv769 *_goml_vec_EnvEntry
    var result__124 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__EnvEntry()
    var index__125 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop771:
    for {
        var t772 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__125)
        var t773 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__EnvEntry(self__122)
        var t774 bool = t772 < t773
        if t774 {
            var t775 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__125)
            var t776 EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__EnvEntry(self__122, t775)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__EnvEntry(result__124, t776)
            var t777 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__125)
            var t778 int32 = t777 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__125, t778)
            continue
        } else {
            break Loop_loop771
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__EnvEntry(result__124, elem__123)
    retv769 = result__124
    return retv769
}

func println__T_string(value__1 string) struct{} {
    var t780 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t780)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SubstEntry(self__120 *_goml_vec_SubstEntry, elem__121 SubstEntry) struct{} {
    vec_push__Vec_10SubstEntry(self__120, elem__121)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__SubstEntry(self__126 *_goml_vec_SubstEntry, index__127 int32) SubstEntry {
    var retv785 SubstEntry
    var t786 SubstEntry = vec_get__Vec_10SubstEntry(self__126, index__127)
    retv785 = t786
    return retv785
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__EnvEntry(self__120 *_goml_vec_EnvEntry, elem__121 EnvEntry) struct{} {
    vec_push__Vec_8EnvEntry(self__120, elem__121)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__EnvEntry(self__126 *_goml_vec_EnvEntry, index__127 int32) EnvEntry {
    var retv790 EnvEntry
    var t791 EnvEntry = vec_get__Vec_8EnvEntry(self__126, index__127)
    retv790 = t791
    return retv790
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv793 string
    retv793 = self__34
    return retv793
}

func main() {
    main0()
}
