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
    var retv261 CheckerState
    var t262 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var t263 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(1)
    var t264 CheckerState = CheckerState{
        gensym_counter: t262,
        current_level: t263,
    }
    retv261 = t264
    return retv261
}

func reset_gensym(st__0 CheckerState) struct{} {
    var t266 *ref_int32_x = st__0.gensym_counter
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t266, 0)
    return struct{}{}
}

func reset_level(st__1 CheckerState) struct{} {
    var t268 *ref_int32_x = st__1.current_level
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t268, 1)
    return struct{}{}
}

func reset_type_variables(st__2 CheckerState) struct{} {
    reset_gensym(st__2)
    reset_level(st__2)
    return struct{}{}
}

func enter_level(st__3 CheckerState) struct{} {
    var t272 *ref_int32_x = st__3.current_level
    var l__4 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t272)
    var t273 *ref_int32_x = st__3.current_level
    var t274 int32 = l__4 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t273, t274)
    return struct{}{}
}

func leave_level(st__5 CheckerState) struct{} {
    var t276 *ref_int32_x = st__5.current_level
    var l__6 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t276)
    var t277 *ref_int32_x = st__5.current_level
    var t278 int32 = l__6 - 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t277, t278)
    return struct{}{}
}

func min_i32(a__7 int32, b__8 int32) int32 {
    var retv280 int32
    var t283 bool = a__7 < b__8
    var jp282 int32
    if t283 {
        jp282 = a__7
    } else {
        jp282 = b__8
    }
    retv280 = jp282
    return retv280
}

func nth_letter(n__9 int32) rune {
    var retv285 rune
    var jp287 rune
    switch n__9 {
    case 0:
        jp287 = 97
    case 1:
        jp287 = 98
    case 2:
        jp287 = 99
    case 3:
        jp287 = 100
    case 4:
        jp287 = 101
    case 5:
        jp287 = 102
    case 6:
        jp287 = 103
    case 7:
        jp287 = 104
    case 8:
        jp287 = 105
    case 9:
        jp287 = 106
    case 10:
        jp287 = 107
    case 11:
        jp287 = 108
    case 12:
        jp287 = 109
    case 13:
        jp287 = 110
    case 14:
        jp287 = 111
    case 15:
        jp287 = 112
    case 16:
        jp287 = 113
    case 17:
        jp287 = 114
    case 18:
        jp287 = 115
    case 19:
        jp287 = 116
    case 20:
        jp287 = 117
    case 21:
        jp287 = 118
    case 22:
        jp287 = 119
    case 23:
        jp287 = 120
    case 24:
        jp287 = 121
    case 25:
        jp287 = 122
    default:
        jp287 = 97
    }
    retv285 = jp287
    return retv285
}

func gensym(st__10 CheckerState) string {
    var retv289 string
    var t290 *ref_int32_x = st__10.gensym_counter
    var n__11 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t290)
    var t291 *ref_int32_x = st__10.gensym_counter
    var t292 int32 = n__11 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t291, t292)
    var t295 bool = n__11 < 26
    var jp294 string
    if t295 {
        var t296 rune = nth_letter(n__11)
        var t297 string = _goml_m_inherent_i_char_i_char_i_to__string(t296)
        jp294 = t297
    } else {
        var t298 string = _goml_m_inherent_i_int32_i_int32_i_to__string(n__11)
        var t299 string = "t" + t298
        jp294 = t299
    }
    retv289 = jp294
    return retv289
}

func newvar(st__12 CheckerState) Typ {
    var retv301 Typ
    var name__13 string = gensym(st__12)
    var t302 *ref_int32_x = st__12.current_level
    var level__14 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t302)
    var t303 Tv = Unbound{
        _0: name__13,
        _1: level__14,
    }
    var t304 *ref_Tv_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Tv(t303)
    var t305 Typ = TVar{
        _0: t304,
    }
    retv301 = t305
    return retv301
}

func typ_is_arrow(ty__15 Typ) bool {
    var retv307 bool
    var jp309 bool
    switch ty__15.(type) {
    case TVar:
        var x70 *ref_Tv_x = ty__15.(TVar)._0
        var tvref__16 *ref_Tv_x = x70
        var mtmp74 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvref__16)
        var jp311 bool
        switch mtmp74.(type) {
        case Link:
            var x77 Typ = mtmp74.(Link)._0
            var inner__17 Typ = x77
            var t312 bool = typ_is_arrow(inner__17)
            jp311 = t312
        default:
            jp311 = false
        }
        jp309 = jp311
    case TArrow:
        jp309 = true
    default:
        jp309 = false
    }
    retv307 = jp309
    return retv307
}

func typ_to_string(ty__18 Typ) string {
    var retv314 string
    var jp316 string
    switch ty__18.(type) {
    case TVar:
        var x78 *ref_Tv_x = ty__18.(TVar)._0
        var tvref__20 *ref_Tv_x = x78
        var mtmp82 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvref__20)
        var jp318 string
        switch mtmp82.(type) {
        case Unbound:
            var x83 string = mtmp82.(Unbound)._0
            var name__21 string = x83
            var t319 string = "'" + name__21
            jp318 = t319
        case Link:
            var x85 Typ = mtmp82.(Link)._0
            var inner__22 Typ = x85
            var t320 string = typ_to_string(inner__22)
            jp318 = t320
        default:
            panic("non-exhaustive match")
        }
        jp316 = jp318
    case QVar:
        var x79 string = ty__18.(QVar)._0
        var name__19 string = x79
        var t321 string = "'" + name__19
        jp316 = t321
    case TArrow:
        var x80 Typ = ty__18.(TArrow)._0
        var x81 Typ = ty__18.(TArrow)._1
        var t2__24 Typ = x81
        var t1__23 Typ = x80
        var t326 bool = typ_is_arrow(t1__23)
        var jp323 string
        if t326 {
            var t327 string = typ_to_string(t1__23)
            var t328 string = "(" + t327
            var t329 string = t328 + ")"
            jp323 = t329
        } else {
            var t330 string = typ_to_string(t1__23)
            jp323 = t330
        }
        var s1__25 string = jp323
        var s2__26 string = typ_to_string(t2__24)
        var t324 string = s1__25 + " -> "
        var t325 string = t324 + s2__26
        jp316 = t325
    default:
        panic("non-exhaustive match")
    }
    retv314 = jp316
    return retv314
}

func env_empty() *_goml_vec_EnvEntry {
    var retv332 *_goml_vec_EnvEntry
    var env__27 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__EnvEntry()
    retv332 = env__27
    return retv332
}

func env_lookup(env__28 *_goml_vec_EnvEntry, name__29 string) Option__Typ {
    var retv334 Option__Typ
    var t335 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__EnvEntry(env__28)
    var t336 int = t335 - 1
    var i__30 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(t336)
    var found__31 *ref_Option__Typ_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_Typ_r_(None{})
    var done__32 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    Loop_loop339:
    for {
        var t352 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(done__32)
        var t353 bool = !t352
        var jp341 bool
        if t353 {
            var t354 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__30)
            var t355 bool = t354 >= 0
            jp341 = t355
        } else {
            jp341 = false
        }
        if jp341 {
            var t342 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__30)
            var entry__33 EnvEntry = vec_get__Vec_8EnvEntry(env__28, t342)
            var t344 string = entry__33.name
            var t345 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(t344, name__29)
            if t345 {
                var t346 Typ = entry__33.ty
                var t347 Option__Typ = Some{
                    _0: t346,
                }
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Option_l_Typ_r_(found__31, t347)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(done__32, true)
            } else {
                var t349 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__30)
                var t350 int = t349 - 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__30, t350)
            }
            continue
        } else {
            break Loop_loop339
        }
    }
    var t338 Option__Typ = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Option_l_Typ_r_(found__31)
    retv334 = t338
    return retv334
}

func subst_lookup(subst__34 *_goml_vec_SubstEntry, name__35 string) Option__Typ {
    var retv357 Option__Typ
    var t358 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SubstEntry(subst__34)
    var t359 int = t358 - 1
    var i__36 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(t359)
    var found__37 *ref_Option__Typ_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_Typ_r_(None{})
    var done__38 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    Loop_loop362:
    for {
        var t375 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(done__38)
        var t376 bool = !t375
        var jp364 bool
        if t376 {
            var t377 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__36)
            var t378 bool = t377 >= 0
            jp364 = t378
        } else {
            jp364 = false
        }
        if jp364 {
            var t365 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__36)
            var entry__39 SubstEntry = vec_get__Vec_10SubstEntry(subst__34, t365)
            var t367 string = entry__39.name
            var t368 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(t367, name__35)
            if t368 {
                var t369 Typ = entry__39.ty
                var t370 Option__Typ = Some{
                    _0: t369,
                }
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Option_l_Typ_r_(found__37, t370)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(done__38, true)
            } else {
                var t372 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__36)
                var t373 int = t372 - 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__36, t373)
            }
            continue
        } else {
            break Loop_loop362
        }
    }
    var t361 Option__Typ = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Option_l_Typ_r_(found__37)
    retv357 = t361
    return retv357
}

func occurs(st__40 CheckerState, tvr__41 *ref_Tv_x, ty__42 Typ) Result__unit__string {
    var retv380 Result__unit__string
    var jp382 Result__unit__string
    switch ty__42.(type) {
    case TVar:
        var x90 *ref_Tv_x = ty__42.(TVar)._0
        var tvr2__43 *ref_Tv_x = x90
        var t385 bool = ptr_eq__Ref_2Tv(tvr__41, tvr2__43)
        var jp384 Result__unit__string
        if t385 {
            var t386 Result__unit__string = Result__unit__string_Err{
                _0: "occurs check",
            }
            jp384 = t386
        } else {
            var mtmp94 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvr2__43)
            var jp388 Result__unit__string
            switch mtmp94.(type) {
            case Unbound:
                var x95 string = mtmp94.(Unbound)._0
                var x96 int32 = mtmp94.(Unbound)._1
                var l2__45 int32 = x96
                var name__44 string = x95
                var mtmp98 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvr__41)
                var jp390 int32
                switch mtmp98.(type) {
                case Unbound:
                    var x100 int32 = mtmp98.(Unbound)._1
                    var l__46 int32 = x100
                    var t393 int32 = min_i32(l__46, l2__45)
                    jp390 = t393
                default:
                    jp390 = l2__45
                }
                var min_level__47 int32 = jp390
                var t391 Tv = Unbound{
                    _0: name__44,
                    _1: min_level__47,
                }
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(tvr2__43, t391)
                var t392 Result__unit__string = Result__unit__string_Ok{
                    _0: struct{}{},
                }
                jp388 = t392
            case Link:
                var x97 Typ = mtmp94.(Link)._0
                var inner__48 Typ = x97
                var t394 Result__unit__string = occurs(st__40, tvr__41, inner__48)
                jp388 = t394
            default:
                panic("non-exhaustive match")
            }
            jp384 = jp388
        }
        jp382 = jp384
    case TArrow:
        var x92 Typ = ty__42.(TArrow)._0
        var x93 Typ = ty__42.(TArrow)._1
        var t2__50 Typ = x93
        var t1__49 Typ = x92
        var mtmp103 Result__unit__string = occurs(st__40, tvr__41, t1__49)
        var jp396 Result__unit__string
        switch mtmp103.(type) {
        case Result__unit__string_Ok:
            var t397 Result__unit__string = occurs(st__40, tvr__41, t2__50)
            jp396 = t397
        case Result__unit__string_Err:
            var x105 string = mtmp103.(Result__unit__string_Err)._0
            var e__51 string = x105
            var t398 Result__unit__string = Result__unit__string_Err{
                _0: e__51,
            }
            jp396 = t398
        default:
            panic("non-exhaustive match")
        }
        jp382 = jp396
    default:
        var t399 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp382 = t399
    }
    retv380 = jp382
    return retv380
}

func unify(st__52 CheckerState, t1__53 Typ, t2__54 Typ) Result__unit__string {
    var retv401 Result__unit__string
    var mtmp106 Tuple2_3Typ_3Typ = Tuple2_3Typ_3Typ{
        _0: t1__53,
        _1: t2__54,
    }
    var x107 Typ = mtmp106._0
    var x108 Typ = mtmp106._1
    var jp403 Result__unit__string
    switch x108.(type) {
    case TVar:
        var x109 *ref_Tv_x = x108.(TVar)._0
        var jp405 Result__unit__string
        switch x107.(type) {
        case TVar:
            var x113 *ref_Tv_x = x107.(TVar)._0
            var r1__55 *ref_Tv_x = x113
            var r2__56 *ref_Tv_x = x109
            var t408 bool = ptr_eq__Ref_2Tv(r1__55, r2__56)
            var jp407 Result__unit__string
            if t408 {
                var t409 Result__unit__string = Result__unit__string_Ok{
                    _0: struct{}{},
                }
                jp407 = t409
            } else {
                var mtmp117 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r1__55)
                var jp411 Result__unit__string
                switch mtmp117.(type) {
                case Unbound:
                    var mtmp121 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r2__56)
                    var jp413 Result__unit__string
                    switch mtmp121.(type) {
                    case Unbound:
                        var t414 Typ = TVar{
                            _0: r2__56,
                        }
                        var mtmp125 Result__unit__string = occurs(st__52, r1__55, t414)
                        var jp416 Result__unit__string
                        switch mtmp125.(type) {
                        case Result__unit__string_Ok:
                            var t417 Typ = TVar{
                                _0: r2__56,
                            }
                            var t418 Tv = Link{
                                _0: t417,
                            }
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(r1__55, t418)
                            var t419 Result__unit__string = Result__unit__string_Ok{
                                _0: struct{}{},
                            }
                            jp416 = t419
                        case Result__unit__string_Err:
                            var x127 string = mtmp125.(Result__unit__string_Err)._0
                            var e__59 string = x127
                            var t420 Result__unit__string = Result__unit__string_Err{
                                _0: e__59,
                            }
                            jp416 = t420
                        default:
                            panic("non-exhaustive match")
                        }
                        jp413 = jp416
                    case Link:
                        var x124 Typ = mtmp121.(Link)._0
                        var inner__58 Typ = x124
                        var t421 Typ = TVar{
                            _0: r1__55,
                        }
                        var t422 Result__unit__string = unify(st__52, t421, inner__58)
                        jp413 = t422
                    default:
                        panic("non-exhaustive match")
                    }
                    jp411 = jp413
                case Link:
                    var x120 Typ = mtmp117.(Link)._0
                    var inner__57 Typ = x120
                    var t423 Typ = TVar{
                        _0: r2__56,
                    }
                    var t424 Result__unit__string = unify(st__52, inner__57, t423)
                    jp411 = t424
                default:
                    panic("non-exhaustive match")
                }
                jp407 = jp411
            }
            jp405 = jp407
        default:
            var r2__65 *ref_Tv_x = x109
            var other__64 Typ = x107
            var mtmp129 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r2__65)
            var jp426 Result__unit__string
            switch mtmp129.(type) {
            case Unbound:
                var mtmp133 Result__unit__string = occurs(st__52, r2__65, other__64)
                var jp428 Result__unit__string
                switch mtmp133.(type) {
                case Result__unit__string_Ok:
                    var t429 Tv = Link{
                        _0: other__64,
                    }
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(r2__65, t429)
                    var t430 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    jp428 = t430
                case Result__unit__string_Err:
                    var x135 string = mtmp133.(Result__unit__string_Err)._0
                    var e__67 string = x135
                    var t431 Result__unit__string = Result__unit__string_Err{
                        _0: e__67,
                    }
                    jp428 = t431
                default:
                    panic("non-exhaustive match")
                }
                jp426 = jp428
            case Link:
                var x132 Typ = mtmp129.(Link)._0
                var inner__66 Typ = x132
                var t432 Result__unit__string = unify(st__52, other__64, inner__66)
                jp426 = t432
            default:
                panic("non-exhaustive match")
            }
            jp405 = jp426
        }
        jp403 = jp405
    case TArrow:
        var x111 Typ = x108.(TArrow)._0
        var x112 Typ = x108.(TArrow)._1
        var jp434 Result__unit__string
        switch x107.(type) {
        case TVar:
            var x137 *ref_Tv_x = x107.(TVar)._0
            var r1__60 *ref_Tv_x = x137
            var other__61 Typ = x108
            var mtmp141 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r1__60)
            var jp436 Result__unit__string
            switch mtmp141.(type) {
            case Unbound:
                var mtmp145 Result__unit__string = occurs(st__52, r1__60, other__61)
                var jp438 Result__unit__string
                switch mtmp145.(type) {
                case Result__unit__string_Ok:
                    var t439 Tv = Link{
                        _0: other__61,
                    }
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(r1__60, t439)
                    var t440 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    jp438 = t440
                case Result__unit__string_Err:
                    var x147 string = mtmp145.(Result__unit__string_Err)._0
                    var e__63 string = x147
                    var t441 Result__unit__string = Result__unit__string_Err{
                        _0: e__63,
                    }
                    jp438 = t441
                default:
                    panic("non-exhaustive match")
                }
                jp436 = jp438
            case Link:
                var x144 Typ = mtmp141.(Link)._0
                var inner__62 Typ = x144
                var t442 Result__unit__string = unify(st__52, inner__62, other__61)
                jp436 = t442
            default:
                panic("non-exhaustive match")
            }
            jp434 = jp436
        case TArrow:
            var x139 Typ = x107.(TArrow)._0
            var x140 Typ = x107.(TArrow)._1
            var a2__69 Typ = x140
            var a1__68 Typ = x139
            var b2__71 Typ = x112
            var b1__70 Typ = x111
            var mtmp149 Result__unit__string = unify(st__52, a1__68, b1__70)
            var jp444 Result__unit__string
            switch mtmp149.(type) {
            case Result__unit__string_Ok:
                var t445 Result__unit__string = unify(st__52, a2__69, b2__71)
                jp444 = t445
            case Result__unit__string_Err:
                var x151 string = mtmp149.(Result__unit__string_Err)._0
                var e__72 string = x151
                var t446 Result__unit__string = Result__unit__string_Err{
                    _0: e__72,
                }
                jp444 = t446
            default:
                panic("non-exhaustive match")
            }
            jp434 = jp444
        default:
            var t447 Result__unit__string = Result__unit__string_Err{
                _0: "unify error",
            }
            jp434 = t447
        }
        jp403 = jp434
    default:
        var jp449 Result__unit__string
        switch x107.(type) {
        case TVar:
            var x152 *ref_Tv_x = x107.(TVar)._0
            var r1__60 *ref_Tv_x = x152
            var other__61 Typ = x108
            var mtmp156 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r1__60)
            var jp451 Result__unit__string
            switch mtmp156.(type) {
            case Unbound:
                var mtmp160 Result__unit__string = occurs(st__52, r1__60, other__61)
                var jp453 Result__unit__string
                switch mtmp160.(type) {
                case Result__unit__string_Ok:
                    var t454 Tv = Link{
                        _0: other__61,
                    }
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(r1__60, t454)
                    var t455 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    jp453 = t455
                case Result__unit__string_Err:
                    var x162 string = mtmp160.(Result__unit__string_Err)._0
                    var e__63 string = x162
                    var t456 Result__unit__string = Result__unit__string_Err{
                        _0: e__63,
                    }
                    jp453 = t456
                default:
                    panic("non-exhaustive match")
                }
                jp451 = jp453
            case Link:
                var x159 Typ = mtmp156.(Link)._0
                var inner__62 Typ = x159
                var t457 Result__unit__string = unify(st__52, inner__62, other__61)
                jp451 = t457
            default:
                panic("non-exhaustive match")
            }
            jp449 = jp451
        default:
            var t458 Result__unit__string = Result__unit__string_Err{
                _0: "unify error",
            }
            jp449 = t458
        }
        jp403 = jp449
    }
    retv401 = jp403
    return retv401
}

func gen(st__73 CheckerState, ty__74 Typ) Typ {
    var retv460 Typ
    var jp462 Typ
    switch ty__74.(type) {
    case TVar:
        var x164 *ref_Tv_x = ty__74.(TVar)._0
        var tvref__75 *ref_Tv_x = x164
        var mtmp168 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvref__75)
        var jp464 Typ
        switch mtmp168.(type) {
        case Unbound:
            var x169 string = mtmp168.(Unbound)._0
            var x170 int32 = mtmp168.(Unbound)._1
            var l__77 int32 = x170
            var name__76 string = x169
            var t465 *ref_int32_x = st__73.current_level
            var cur__78 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t465)
            var t468 bool = l__77 > cur__78
            var jp467 Typ
            if t468 {
                var t469 Typ = QVar{
                    _0: name__76,
                }
                jp467 = t469
            } else {
                var t470 Typ = TVar{
                    _0: tvref__75,
                }
                jp467 = t470
            }
            jp464 = jp467
        case Link:
            var x171 Typ = mtmp168.(Link)._0
            var inner__79 Typ = x171
            var t471 Typ = gen(st__73, inner__79)
            jp464 = t471
        default:
            panic("non-exhaustive match")
        }
        jp462 = jp464
    case TArrow:
        var x166 Typ = ty__74.(TArrow)._0
        var x167 Typ = ty__74.(TArrow)._1
        var t2__81 Typ = x167
        var t1__80 Typ = x166
        var t472 Typ = gen(st__73, t1__80)
        var t473 Typ = gen(st__73, t2__81)
        var t474 Typ = TArrow{
            _0: t472,
            _1: t473,
        }
        jp462 = t474
    default:
        var other__82 Typ = ty__74
        jp462 = other__82
    }
    retv460 = jp462
    return retv460
}

func inst_loop(st__83 CheckerState, subst__84 *_goml_vec_SubstEntry, ty__85 Typ) Tuple2_3Typ_16Vec_10SubstEntry {
    var retv476 Tuple2_3Typ_16Vec_10SubstEntry
    var jp478 Tuple2_3Typ_16Vec_10SubstEntry
    switch ty__85.(type) {
    case TVar:
        var x172 *ref_Tv_x = ty__85.(TVar)._0
        var tvref__90 *ref_Tv_x = x172
        var mtmp176 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvref__90)
        var jp480 Tuple2_3Typ_16Vec_10SubstEntry
        switch mtmp176.(type) {
        case Link:
            var x179 Typ = mtmp176.(Link)._0
            var inner__91 Typ = x179
            var t481 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, subst__84, inner__91)
            jp480 = t481
        default:
            var t482 Typ = TVar{
                _0: tvref__90,
            }
            var t483 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: t482,
                _1: subst__84,
            }
            jp480 = t483
        }
        jp478 = jp480
    case QVar:
        var x173 string = ty__85.(QVar)._0
        var name__86 string = x173
        var mtmp180 Option__Typ = subst_lookup(subst__84, name__86)
        var jp485 Tuple2_3Typ_16Vec_10SubstEntry
        switch mtmp180.(type) {
        case None:
            var tv__88 Typ = newvar(st__83)
            var t486 SubstEntry = SubstEntry{
                name: name__86,
                ty: tv__88,
            }
            var new_subst__89 *_goml_vec_SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SubstEntry(subst__84, t486)
            var t487 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: tv__88,
                _1: new_subst__89,
            }
            jp485 = t487
        case Some:
            var x181 Typ = mtmp180.(Some)._0
            var t__87 Typ = x181
            var t488 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: t__87,
                _1: subst__84,
            }
            jp485 = t488
        default:
            panic("non-exhaustive match")
        }
        jp478 = jp485
    case TArrow:
        var x174 Typ = ty__85.(TArrow)._0
        var x175 Typ = ty__85.(TArrow)._1
        var t2__93 Typ = x175
        var t1__92 Typ = x174
        var mtmp182 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, subst__84, t1__92)
        var x183 Typ = mtmp182._0
        var x184 *_goml_vec_SubstEntry = mtmp182._1
        var subst1__95 *_goml_vec_SubstEntry = x184
        var ty1__94 Typ = x183
        var mtmp185 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, subst1__95, t2__93)
        var x186 Typ = mtmp185._0
        var x187 *_goml_vec_SubstEntry = mtmp185._1
        var subst2__97 *_goml_vec_SubstEntry = x187
        var ty2__96 Typ = x186
        var t489 Typ = TArrow{
            _0: ty1__94,
            _1: ty2__96,
        }
        var t490 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
            _0: t489,
            _1: subst2__97,
        }
        jp478 = t490
    default:
        panic("non-exhaustive match")
    }
    retv476 = jp478
    return retv476
}

func inst(st__98 CheckerState, ty__99 Typ) Typ {
    var retv492 Typ
    var subst0__100 *_goml_vec_SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SubstEntry()
    var mtmp188 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__98, subst0__100, ty__99)
    var x189 Typ = mtmp188._0
    var t__101 Typ = x189
    retv492 = t__101
    return retv492
}

func typeof(st__102 CheckerState, env__103 *_goml_vec_EnvEntry, e__104 Exp) Result__Typ__string {
    var retv494 Result__Typ__string
    var jp496 Result__Typ__string
    switch e__104.(type) {
    case Var:
        var x191 string = e__104.(Var)._0
        var x__105 string = x191
        var mtmp199 Option__Typ = env_lookup(env__103, x__105)
        var jp498 Result__Typ__string
        switch mtmp199.(type) {
        case None:
            var t499 Result__Typ__string = Result__Typ__string_Err{
                _0: "unbound var",
            }
            jp498 = t499
        case Some:
            var x200 Typ = mtmp199.(Some)._0
            var ty__106 Typ = x200
            var t500 Typ = inst(st__102, ty__106)
            var t501 Result__Typ__string = Result__Typ__string_Ok{
                _0: t500,
            }
            jp498 = t501
        default:
            panic("non-exhaustive match")
        }
        jp496 = jp498
    case App:
        var x192 Exp = e__104.(App)._0
        var x193 Exp = e__104.(App)._1
        var e2__114 Exp = x193
        var e1__113 Exp = x192
        var mtmp201 Result__Typ__string = typeof(st__102, env__103, e1__113)
        var jp503 Result__Typ__string
        switch mtmp201.(type) {
        case Result__Typ__string_Ok:
            var x202 Typ = mtmp201.(Result__Typ__string_Ok)._0
            var ty_fun__116 Typ = x202
            var mtmp204 Result__Typ__string = typeof(st__102, env__103, e2__114)
            var jp505 Result__Typ__string
            switch mtmp204.(type) {
            case Result__Typ__string_Ok:
                var x205 Typ = mtmp204.(Result__Typ__string_Ok)._0
                var ty_arg__118 Typ = x205
                var ty_res__119 Typ = newvar(st__102)
                var arrow__120 Typ = TArrow{
                    _0: ty_arg__118,
                    _1: ty_res__119,
                }
                var mtmp207 Result__unit__string = unify(st__102, ty_fun__116, arrow__120)
                var jp507 Result__Typ__string
                switch mtmp207.(type) {
                case Result__unit__string_Ok:
                    var t508 Result__Typ__string = Result__Typ__string_Ok{
                        _0: ty_res__119,
                    }
                    jp507 = t508
                case Result__unit__string_Err:
                    var x209 string = mtmp207.(Result__unit__string_Err)._0
                    var e__121 string = x209
                    var t509 Result__Typ__string = Result__Typ__string_Err{
                        _0: e__121,
                    }
                    jp507 = t509
                default:
                    panic("non-exhaustive match")
                }
                jp505 = jp507
            case Result__Typ__string_Err:
                var x206 string = mtmp204.(Result__Typ__string_Err)._0
                var e__117 string = x206
                var t510 Result__Typ__string = Result__Typ__string_Err{
                    _0: e__117,
                }
                jp505 = t510
            default:
                panic("non-exhaustive match")
            }
            jp503 = jp505
        case Result__Typ__string_Err:
            var x203 string = mtmp201.(Result__Typ__string_Err)._0
            var e__115 string = x203
            var t511 Result__Typ__string = Result__Typ__string_Err{
                _0: e__115,
            }
            jp503 = t511
        default:
            panic("non-exhaustive match")
        }
        jp496 = jp503
    case Lam:
        var x194 string = e__104.(Lam)._0
        var x195 Exp = e__104.(Lam)._1
        var body__108 Exp = x195
        var x__107 string = x194
        var ty_x__109 Typ = newvar(st__102)
        var t512 EnvEntry = EnvEntry{
            name: x__107,
            ty: ty_x__109,
        }
        var env2__110 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__EnvEntry(env__103, t512)
        var mtmp210 Result__Typ__string = typeof(st__102, env2__110, body__108)
        var jp514 Result__Typ__string
        switch mtmp210.(type) {
        case Result__Typ__string_Ok:
            var x211 Typ = mtmp210.(Result__Typ__string_Ok)._0
            var ty_e__111 Typ = x211
            var t515 Typ = TArrow{
                _0: ty_x__109,
                _1: ty_e__111,
            }
            var t516 Result__Typ__string = Result__Typ__string_Ok{
                _0: t515,
            }
            jp514 = t516
        case Result__Typ__string_Err:
            var x212 string = mtmp210.(Result__Typ__string_Err)._0
            var e__112 string = x212
            var t517 Result__Typ__string = Result__Typ__string_Err{
                _0: e__112,
            }
            jp514 = t517
        default:
            panic("non-exhaustive match")
        }
        jp496 = jp514
    case Let:
        var x196 string = e__104.(Let)._0
        var x197 Exp = e__104.(Let)._1
        var x198 Exp = e__104.(Let)._2
        var e2__124 Exp = x198
        var e1__123 Exp = x197
        var x__122 string = x196
        enter_level(st__102)
        var ty_e__125 Result__Typ__string = typeof(st__102, env__103, e1__123)
        leave_level(st__102)
        var jp519 Result__Typ__string
        switch ty_e__125.(type) {
        case Result__Typ__string_Ok:
            var x215 Typ = ty_e__125.(Result__Typ__string_Ok)._0
            var ty1__127 Typ = x215
            var t520 Typ = gen(st__102, ty1__127)
            var t521 EnvEntry = EnvEntry{
                name: x__122,
                ty: t520,
            }
            var env2__128 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__EnvEntry(env__103, t521)
            var t522 Result__Typ__string = typeof(st__102, env2__128, e2__124)
            jp519 = t522
        case Result__Typ__string_Err:
            var x216 string = ty_e__125.(Result__Typ__string_Err)._0
            var e__126 string = x216
            var t523 Result__Typ__string = Result__Typ__string_Err{
                _0: e__126,
            }
            jp519 = t523
        default:
            panic("non-exhaustive match")
        }
        jp496 = jp519
    default:
        panic("non-exhaustive match")
    }
    retv494 = jp496
    return retv494
}

func exp_var(name__129 string) Exp {
    var retv525 Exp
    var t526 Exp = Var{
        _0: name__129,
    }
    retv525 = t526
    return retv525
}

func exp_lam(name__130 string, body__131 Exp) Exp {
    var retv528 Exp
    var t529 Exp = Lam{
        _0: name__130,
        _1: body__131,
    }
    retv528 = t529
    return retv528
}

func exp_app(a__132 Exp, b__133 Exp) Exp {
    var retv531 Exp
    var t532 Exp = App{
        _0: a__132,
        _1: b__133,
    }
    retv531 = t532
    return retv531
}

func exp_let(name__134 string, a__135 Exp, b__136 Exp) Exp {
    var retv534 Exp
    var t535 Exp = Let{
        _0: name__134,
        _1: a__135,
        _2: b__136,
    }
    retv534 = t535
    return retv534
}

func show_result(label__137 string, res__138 Result__Typ__string) struct{} {
    switch res__138.(type) {
    case Result__Typ__string_Ok:
        var x217 Typ = res__138.(Result__Typ__string_Ok)._0
        var ty__139 Typ = x217
        var t538 string = label__137 + ": "
        var t539 string = typ_to_string(ty__139)
        var t540 string = t538 + t539
        println__T_string(t540)
    case Result__Typ__string_Err:
        var x218 string = res__138.(Result__Typ__string_Err)._0
        var e__140 string = x218
        var t542 string = label__137 + ": "
        var t543 string = t542 + e__140
        println__T_string(t543)
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func main0() struct{} {
    var st__141 CheckerState = state_new()
    var t546 Exp = exp_var("x")
    var id__142 Exp = exp_lam("x", t546)
    var t547 Exp = exp_var("x")
    var t548 Exp = exp_var("y")
    var t549 Exp = exp_app(t547, t548)
    var t550 Exp = exp_lam("y", t549)
    var c1__143 Exp = exp_lam("x", t550)
    reset_type_variables(st__141)
    var t551 *_goml_vec_EnvEntry = env_empty()
    var t552 Result__Typ__string = typeof(st__141, t551, id__142)
    show_result("id", t552)
    reset_type_variables(st__141)
    var t553 *_goml_vec_EnvEntry = env_empty()
    var t554 Result__Typ__string = typeof(st__141, t553, c1__143)
    show_result("c1", t554)
    reset_type_variables(st__141)
    var t555 *_goml_vec_EnvEntry = env_empty()
    var t556 Exp = exp_var("x")
    var t557 Exp = exp_let("x", c1__143, t556)
    var t558 Result__Typ__string = typeof(st__141, t555, t557)
    show_result("let_x_c1_x", t558)
    reset_type_variables(st__141)
    var t559 *_goml_vec_EnvEntry = env_empty()
    var t560 Exp = exp_var("z")
    var t561 Exp = exp_lam("z", t560)
    var t562 Exp = exp_var("y")
    var t563 Exp = exp_let("y", t561, t562)
    var t564 Result__Typ__string = typeof(st__141, t559, t563)
    show_result("let_y_id_y", t564)
    reset_type_variables(st__141)
    var t565 *_goml_vec_EnvEntry = env_empty()
    var t566 Exp = exp_var("z")
    var t567 Exp = exp_lam("z", t566)
    var t568 Exp = exp_var("y")
    var t569 Exp = exp_let("y", t567, t568)
    var t570 Exp = exp_lam("x", t569)
    var t571 Result__Typ__string = typeof(st__141, t565, t570)
    show_result("lam_x_let_y_id_y", t571)
    reset_type_variables(st__141)
    var t572 *_goml_vec_EnvEntry = env_empty()
    var t573 Exp = exp_var("z")
    var t574 Exp = exp_lam("z", t573)
    var t575 Exp = exp_var("y")
    var t576 Exp = exp_var("x")
    var t577 Exp = exp_app(t575, t576)
    var t578 Exp = exp_let("y", t574, t577)
    var t579 Exp = exp_lam("x", t578)
    var t580 Result__Typ__string = typeof(st__141, t572, t579)
    show_result("lam_x_let_y_id_yx", t580)
    reset_type_variables(st__141)
    var t581 *_goml_vec_EnvEntry = env_empty()
    var t582 Exp = exp_var("x")
    var t583 Exp = exp_var("x")
    var t584 Exp = exp_app(t582, t583)
    var t585 Exp = exp_lam("x", t584)
    var t586 Result__Typ__string = typeof(st__141, t581, t585)
    show_result("self_apply", t586)
    reset_type_variables(st__141)
    var t587 *_goml_vec_EnvEntry = env_empty()
    var t588 Exp = exp_var("x")
    var t589 Exp = exp_var("x")
    var t590 Exp = exp_let("x", t588, t589)
    var t591 Result__Typ__string = typeof(st__141, t587, t590)
    show_result("unbound_var", t591)
    reset_type_variables(st__141)
    var t592 *_goml_vec_EnvEntry = env_empty()
    var t593 Exp = exp_var("y")
    var t594 Exp = exp_var("y")
    var t595 Exp = exp_var("z")
    var t596 Exp = exp_app(t594, t595)
    var t597 Exp = exp_lam("z", t596)
    var t598 Exp = exp_app(t593, t597)
    var t599 Exp = exp_lam("y", t598)
    var t600 Result__Typ__string = typeof(st__141, t592, t599)
    show_result("max_heiber", t600)
    reset_type_variables(st__141)
    var t601 *_goml_vec_EnvEntry = env_empty()
    var t602 Exp = exp_var("k")
    var t603 Exp = exp_var("k")
    var t604 Exp = exp_var("x")
    var t605 Exp = exp_app(t603, t604)
    var t606 Exp = exp_var("y")
    var t607 Exp = exp_app(t605, t606)
    var t608 Exp = exp_app(t602, t607)
    var t609 Exp = exp_var("k")
    var t610 Exp = exp_var("y")
    var t611 Exp = exp_app(t609, t610)
    var t612 Exp = exp_var("x")
    var t613 Exp = exp_app(t611, t612)
    var t614 Exp = exp_app(t608, t613)
    var t615 Exp = exp_lam("k", t614)
    var t616 Exp = exp_lam("y", t615)
    var t617 Exp = exp_lam("x", t616)
    var t618 Result__Typ__string = typeof(st__141, t601, t617)
    show_result("kirang", t618)
    reset_type_variables(st__141)
    var t619 *_goml_vec_EnvEntry = env_empty()
    var t620 Exp = exp_var("id")
    var t621 Exp = exp_var("id")
    var t622 Exp = exp_app(t620, t621)
    var t623 Exp = exp_let("id", id__142, t622)
    var t624 Result__Typ__string = typeof(st__141, t619, t623)
    show_result("let_id_idid", t624)
    reset_type_variables(st__141)
    var t625 *_goml_vec_EnvEntry = env_empty()
    var t626 Exp = exp_var("x")
    var t627 Exp = exp_app(t626, id__142)
    var t628 Exp = exp_var("z")
    var t629 Exp = exp_let("z", t627, t628)
    var t630 Exp = exp_var("y")
    var t631 Exp = exp_let("y", t629, t630)
    var t632 Exp = exp_let("x", c1__143, t631)
    var t633 Result__Typ__string = typeof(st__141, t625, t632)
    show_result("nested_lets", t633)
    reset_type_variables(st__141)
    var t634 *_goml_vec_EnvEntry = env_empty()
    var t635 Exp = exp_var("x")
    var t636 Exp = exp_var("y")
    var t637 Exp = exp_app(t635, t636)
    var t638 Exp = exp_var("y")
    var t639 Exp = exp_var("x")
    var t640 Exp = exp_app(t638, t639)
    var t641 Exp = exp_lam("x", t640)
    var t642 Exp = exp_let("x", t637, t641)
    var t643 Exp = exp_lam("y", t642)
    var t644 Exp = exp_lam("x", t643)
    var t645 Result__Typ__string = typeof(st__141, t634, t644)
    show_result("fun_x_fun_y_let_x_xy_fun_x_yx", t645)
    reset_type_variables(st__141)
    var t646 *_goml_vec_EnvEntry = env_empty()
    var t647 Exp = exp_var("x")
    var t648 Exp = exp_var("y")
    var t649 Exp = exp_let("y", t647, t648)
    var t650 Exp = exp_lam("x", t649)
    var t651 Result__Typ__string = typeof(st__141, t646, t650)
    show_result("sound_gen_1", t651)
    reset_type_variables(st__141)
    var t652 *_goml_vec_EnvEntry = env_empty()
    var t653 Exp = exp_var("x")
    var t654 Exp = exp_lam("z", t653)
    var t655 Exp = exp_var("y")
    var t656 Exp = exp_let("y", t654, t655)
    var t657 Exp = exp_lam("x", t656)
    var t658 Result__Typ__string = typeof(st__141, t652, t657)
    show_result("sound_gen_2", t658)
    reset_type_variables(st__141)
    var t659 *_goml_vec_EnvEntry = env_empty()
    var t660 Exp = exp_var("x")
    var t661 Exp = exp_var("z")
    var t662 Exp = exp_app(t660, t661)
    var t663 Exp = exp_lam("z", t662)
    var t664 Exp = exp_var("y")
    var t665 Exp = exp_let("y", t663, t664)
    var t666 Exp = exp_lam("x", t665)
    var t667 Result__Typ__string = typeof(st__141, t659, t666)
    show_result("sound_gen_3", t667)
    reset_type_variables(st__141)
    var t668 *_goml_vec_EnvEntry = env_empty()
    var t669 Exp = exp_var("x")
    var t670 Exp = exp_var("y")
    var t671 Exp = exp_app(t669, t670)
    var t672 Exp = exp_var("x")
    var t673 Exp = exp_var("y")
    var t674 Exp = exp_app(t672, t673)
    var t675 Exp = exp_let("x", t671, t674)
    var t676 Exp = exp_lam("y", t675)
    var t677 Exp = exp_lam("x", t676)
    var t678 Result__Typ__string = typeof(st__141, t668, t677)
    show_result("double_apply", t678)
    reset_type_variables(st__141)
    var t679 *_goml_vec_EnvEntry = env_empty()
    var t680 Exp = exp_var("x")
    var t681 Exp = exp_var("y")
    var t682 Exp = exp_var("y")
    var t683 Exp = exp_app(t681, t682)
    var t684 Exp = exp_let("y", t680, t683)
    var t685 Exp = exp_lam("x", t684)
    var t686 Result__Typ__string = typeof(st__141, t679, t685)
    show_result("sound_gen_occurs", t686)
    reset_gensym(st__141)
    var t687 *_goml_vec_EnvEntry = env_empty()
    var t688 Exp = exp_var("x")
    var t689 Exp = exp_app(t688, id__142)
    var t690 Exp = exp_var("z")
    var t691 Exp = exp_let("z", t689, t690)
    var t692 Exp = exp_var("y")
    var t693 Exp = exp_let("y", t691, t692)
    var t694 Exp = exp_lam("x", t693)
    var t695 Result__Typ__string = typeof(st__141, t687, t694)
    show_result("fun_x_let_y_let_z_x_id_z_y", t695)
    println__T_string("")
    println__T_string("All Done")
    println__T_string("")
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__209 int32) *ref_int32_x {
    var retv697 *ref_int32_x
    var t698 *ref_int32_x = ref__Ref_5int32(value__209)
    retv697 = t698
    return retv697
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__211 *ref_int32_x, value__212 int32) struct{} {
    ref_set__Ref_5int32(self__211, value__212)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__210 *ref_int32_x) int32 {
    var retv702 int32
    var t703 int32 = ref_get__Ref_5int32(self__210)
    retv702 = t703
    return retv702
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__7 rune) string {
    var retv705 string
    var t706 string = _goml_runtime_core_char_to_string(self__7)
    retv705 = t706
    return retv705
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv708 string
    var t709 string = _goml_runtime_core_int32_to_string(self__6)
    retv708 = t709
    return retv708
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Tv(value__209 Tv) *ref_Tv_x {
    var retv711 *ref_Tv_x
    var t712 *ref_Tv_x = ref__Ref_2Tv(value__209)
    retv711 = t712
    return retv711
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(self__210 *ref_Tv_x) Tv {
    var retv714 Tv
    var t715 Tv = ref_get__Ref_2Tv(self__210)
    retv714 = t715
    return retv714
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__EnvEntry() *_goml_vec_EnvEntry {
    var retv717 *_goml_vec_EnvEntry
    var t718 *_goml_vec_EnvEntry = vec_new__Vec_8EnvEntry()
    retv717 = t718
    return retv717
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__EnvEntry(self__139 *_goml_vec_EnvEntry) int {
    var retv720 int
    var t721 int = vec_len__Vec_8EnvEntry(self__139)
    retv720 = t721
    return retv720
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__209 int) *ref_int_x {
    var retv723 *ref_int_x
    var t724 *ref_int_x = ref__Ref_3int(value__209)
    retv723 = t724
    return retv723
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_Typ_r_(value__209 Option__Typ) *ref_Option__Typ_x {
    var retv726 *ref_Option__Typ_x
    var t727 *ref_Option__Typ_x = ref__Ref_11Option__Typ(value__209)
    retv726 = t727
    return retv726
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__209 bool) *ref_bool_x {
    var retv729 *ref_bool_x
    var t730 *ref_bool_x = ref__Ref_4bool(value__209)
    retv729 = t730
    return retv729
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__210 *ref_bool_x) bool {
    var retv732 bool
    var t733 bool = ref_get__Ref_4bool(self__210)
    retv732 = t733
    return retv732
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__210 *ref_int_x) int {
    var retv735 int
    var t736 int = ref_get__Ref_3int(self__210)
    retv735 = t736
    return retv735
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__55 string, other__56 string) bool {
    var retv738 bool
    var t739 bool = self__55 == other__56
    retv738 = t739
    return retv738
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Option_l_Typ_r_(self__211 *ref_Option__Typ_x, value__212 Option__Typ) struct{} {
    ref_set__Ref_11Option__Typ(self__211, value__212)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(self__211 *ref_bool_x, value__212 bool) struct{} {
    ref_set__Ref_4bool(self__211, value__212)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__211 *ref_int_x, value__212 int) struct{} {
    ref_set__Ref_3int(self__211, value__212)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Option_l_Typ_r_(self__210 *ref_Option__Typ_x) Option__Typ {
    var retv747 Option__Typ
    var t748 Option__Typ = ref_get__Ref_11Option__Typ(self__210)
    retv747 = t748
    return retv747
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SubstEntry(self__139 *_goml_vec_SubstEntry) int {
    var retv750 int
    var t751 int = vec_len__Vec_10SubstEntry(self__139)
    retv750 = t751
    return retv750
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(self__211 *ref_Tv_x, value__212 Tv) struct{} {
    ref_set__Ref_2Tv(self__211, value__212)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SubstEntry(self__130 *_goml_vec_SubstEntry, elem__131 SubstEntry) *_goml_vec_SubstEntry {
    var retv755 *_goml_vec_SubstEntry
    var result__132 *_goml_vec_SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SubstEntry()
    var index__133 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop757:
    for {
        var t758 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__133)
        var t759 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SubstEntry(self__130)
        var t760 bool = t758 < t759
        if t760 {
            var t761 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__133)
            var t762 SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__SubstEntry(self__130, t761)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SubstEntry(result__132, t762)
            var t763 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__133)
            var t764 int = t763 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__133, t764)
            continue
        } else {
            break Loop_loop757
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SubstEntry(result__132, elem__131)
    retv755 = result__132
    return retv755
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SubstEntry() *_goml_vec_SubstEntry {
    var retv766 *_goml_vec_SubstEntry
    var t767 *_goml_vec_SubstEntry = vec_new__Vec_10SubstEntry()
    retv766 = t767
    return retv766
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__EnvEntry(self__130 *_goml_vec_EnvEntry, elem__131 EnvEntry) *_goml_vec_EnvEntry {
    var retv769 *_goml_vec_EnvEntry
    var result__132 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__EnvEntry()
    var index__133 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop771:
    for {
        var t772 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__133)
        var t773 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__EnvEntry(self__130)
        var t774 bool = t772 < t773
        if t774 {
            var t775 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__133)
            var t776 EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__EnvEntry(self__130, t775)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__EnvEntry(result__132, t776)
            var t777 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__133)
            var t778 int = t777 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__133, t778)
            continue
        } else {
            break Loop_loop771
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__EnvEntry(result__132, elem__131)
    retv769 = result__132
    return retv769
}

func println__T_string(value__1 string) struct{} {
    var t780 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t780)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SubstEntry(self__128 *_goml_vec_SubstEntry, elem__129 SubstEntry) struct{} {
    vec_push__Vec_10SubstEntry(self__128, elem__129)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__SubstEntry(self__134 *_goml_vec_SubstEntry, index__135 int) SubstEntry {
    var retv785 SubstEntry
    var t786 SubstEntry = vec_get__Vec_10SubstEntry(self__134, index__135)
    retv785 = t786
    return retv785
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__EnvEntry(self__128 *_goml_vec_EnvEntry, elem__129 EnvEntry) struct{} {
    vec_push__Vec_8EnvEntry(self__128, elem__129)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__EnvEntry(self__134 *_goml_vec_EnvEntry, index__135 int) EnvEntry {
    var retv790 EnvEntry
    var t791 EnvEntry = vec_get__Vec_8EnvEntry(self__134, index__135)
    retv790 = t791
    return retv790
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv793 string
    retv793 = self__38
    return retv793
}

func main() {
    main0()
}
