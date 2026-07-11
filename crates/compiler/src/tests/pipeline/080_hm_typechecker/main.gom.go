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
    var retv227 CheckerState
    var t228 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var t229 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(1)
    var t230 CheckerState = CheckerState{
        gensym_counter: t228,
        current_level: t229,
    }
    retv227 = t230
    return retv227
}

func reset_gensym(st__0 CheckerState) struct{} {
    var t232 *ref_int32_x = st__0.gensym_counter
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t232, 0)
    return struct{}{}
}

func reset_level(st__1 CheckerState) struct{} {
    var t234 *ref_int32_x = st__1.current_level
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t234, 1)
    return struct{}{}
}

func reset_type_variables(st__2 CheckerState) struct{} {
    reset_gensym(st__2)
    reset_level(st__2)
    return struct{}{}
}

func enter_level(st__3 CheckerState) struct{} {
    var t238 *ref_int32_x = st__3.current_level
    var l__4 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t238)
    var t239 *ref_int32_x = st__3.current_level
    var t240 int32 = l__4 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t239, t240)
    return struct{}{}
}

func leave_level(st__5 CheckerState) struct{} {
    var t242 *ref_int32_x = st__5.current_level
    var l__6 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t242)
    var t243 *ref_int32_x = st__5.current_level
    var t244 int32 = l__6 - 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t243, t244)
    return struct{}{}
}

func min_i32(a__7 int32, b__8 int32) int32 {
    var retv246 int32
    var t249 bool = a__7 < b__8
    var jp248 int32
    if t249 {
        jp248 = a__7
    } else {
        jp248 = b__8
    }
    retv246 = jp248
    return retv246
}

func nth_letter(n__9 int32) rune {
    var retv251 rune
    var jp253 rune
    switch n__9 {
    case 0:
        jp253 = 97
    case 1:
        jp253 = 98
    case 2:
        jp253 = 99
    case 3:
        jp253 = 100
    case 4:
        jp253 = 101
    case 5:
        jp253 = 102
    case 6:
        jp253 = 103
    case 7:
        jp253 = 104
    case 8:
        jp253 = 105
    case 9:
        jp253 = 106
    case 10:
        jp253 = 107
    case 11:
        jp253 = 108
    case 12:
        jp253 = 109
    case 13:
        jp253 = 110
    case 14:
        jp253 = 111
    case 15:
        jp253 = 112
    case 16:
        jp253 = 113
    case 17:
        jp253 = 114
    case 18:
        jp253 = 115
    case 19:
        jp253 = 116
    case 20:
        jp253 = 117
    case 21:
        jp253 = 118
    case 22:
        jp253 = 119
    case 23:
        jp253 = 120
    case 24:
        jp253 = 121
    case 25:
        jp253 = 122
    default:
        jp253 = 97
    }
    retv251 = jp253
    return retv251
}

func gensym(st__10 CheckerState) string {
    var retv255 string
    var t256 *ref_int32_x = st__10.gensym_counter
    var n__11 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t256)
    var t257 *ref_int32_x = st__10.gensym_counter
    var t258 int32 = n__11 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t257, t258)
    var t261 bool = n__11 < 26
    var jp260 string
    if t261 {
        var t262 rune = nth_letter(n__11)
        var t263 string = _goml_m_inherent_i_char_i_char_i_to__string(t262)
        jp260 = t263
    } else {
        var t264 string = _goml_m_inherent_i_int32_i_int32_i_to__string(n__11)
        var t265 string = "t" + t264
        jp260 = t265
    }
    retv255 = jp260
    return retv255
}

func newvar(st__12 CheckerState) Typ {
    var retv267 Typ
    var name__13 string = gensym(st__12)
    var t268 *ref_int32_x = st__12.current_level
    var level__14 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t268)
    var t269 Tv = Unbound{
        _0: name__13,
        _1: level__14,
    }
    var t270 *ref_Tv_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Tv(t269)
    var t271 Typ = TVar{
        _0: t270,
    }
    retv267 = t271
    return retv267
}

func typ_is_arrow(ty__15 Typ) bool {
    var retv273 bool
    var jp275 bool
    switch ty__15.(type) {
    case TVar:
        var x28 *ref_Tv_x = ty__15.(TVar)._0
        var tvref__16 *ref_Tv_x = x28
        var mtmp32 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvref__16)
        var jp277 bool
        switch mtmp32.(type) {
        case Unbound:
            jp277 = false
        case Link:
            var x35 Typ = mtmp32.(Link)._0
            var inner__17 Typ = x35
            var t278 bool = typ_is_arrow(inner__17)
            jp277 = t278
        default:
            panic("non-exhaustive match")
        }
        jp275 = jp277
    case QVar:
        jp275 = false
    case TArrow:
        jp275 = true
    default:
        panic("non-exhaustive match")
    }
    retv273 = jp275
    return retv273
}

func typ_to_string(ty__18 Typ) string {
    var retv280 string
    var jp282 string
    switch ty__18.(type) {
    case TVar:
        var x36 *ref_Tv_x = ty__18.(TVar)._0
        var tvref__20 *ref_Tv_x = x36
        var mtmp40 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvref__20)
        var jp284 string
        switch mtmp40.(type) {
        case Unbound:
            var x41 string = mtmp40.(Unbound)._0
            var name__21 string = x41
            var t285 string = "'" + name__21
            jp284 = t285
        case Link:
            var x43 Typ = mtmp40.(Link)._0
            var inner__22 Typ = x43
            var t286 string = typ_to_string(inner__22)
            jp284 = t286
        default:
            panic("non-exhaustive match")
        }
        jp282 = jp284
    case QVar:
        var x37 string = ty__18.(QVar)._0
        var name__19 string = x37
        var t287 string = "'" + name__19
        jp282 = t287
    case TArrow:
        var x38 Typ = ty__18.(TArrow)._0
        var x39 Typ = ty__18.(TArrow)._1
        var t2__24 Typ = x39
        var t1__23 Typ = x38
        var t292 bool = typ_is_arrow(t1__23)
        var jp289 string
        if t292 {
            var t293 string = typ_to_string(t1__23)
            var t294 string = "(" + t293
            var t295 string = t294 + ")"
            jp289 = t295
        } else {
            var t296 string = typ_to_string(t1__23)
            jp289 = t296
        }
        var s1__25 string = jp289
        var s2__26 string = typ_to_string(t2__24)
        var t290 string = s1__25 + " -> "
        var t291 string = t290 + s2__26
        jp282 = t291
    default:
        panic("non-exhaustive match")
    }
    retv280 = jp282
    return retv280
}

func env_empty() *_goml_vec_EnvEntry {
    var retv298 *_goml_vec_EnvEntry
    var env__27 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__EnvEntry()
    retv298 = env__27
    return retv298
}

func env_lookup(env__28 *_goml_vec_EnvEntry, name__29 string) Option__Typ {
    var retv300 Option__Typ
    var t301 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__EnvEntry(env__28)
    var t302 int32 = t301 - 1
    var i__30 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(t302)
    var found__31 *ref_Option__Typ_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_Typ_r_(None{})
    var done__32 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    Loop_loop305:
    for {
        var t318 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(done__32)
        var t319 bool = !t318
        var jp307 bool
        if t319 {
            var t320 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__30)
            var t321 bool = t320 >= 0
            jp307 = t321
        } else {
            jp307 = false
        }
        if jp307 {
            var t308 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__30)
            var entry__33 EnvEntry = vec_get__Vec_8EnvEntry(env__28, t308)
            var t310 string = entry__33.name
            var t311 bool = t310 == name__29
            if t311 {
                var t312 Typ = entry__33.ty
                var t313 Option__Typ = Some{
                    _0: t312,
                }
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Option_l_Typ_r_(found__31, t313)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(done__32, true)
            } else {
                var t315 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__30)
                var t316 int32 = t315 - 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__30, t316)
            }
            continue
        } else {
            break Loop_loop305
        }
    }
    var t304 Option__Typ = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Option_l_Typ_r_(found__31)
    retv300 = t304
    return retv300
}

func subst_lookup(subst__34 *_goml_vec_SubstEntry, name__35 string) Option__Typ {
    var retv323 Option__Typ
    var t324 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SubstEntry(subst__34)
    var t325 int32 = t324 - 1
    var i__36 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(t325)
    var found__37 *ref_Option__Typ_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_Typ_r_(None{})
    var done__38 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    Loop_loop328:
    for {
        var t341 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(done__38)
        var t342 bool = !t341
        var jp330 bool
        if t342 {
            var t343 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__36)
            var t344 bool = t343 >= 0
            jp330 = t344
        } else {
            jp330 = false
        }
        if jp330 {
            var t331 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__36)
            var entry__39 SubstEntry = vec_get__Vec_10SubstEntry(subst__34, t331)
            var t333 string = entry__39.name
            var t334 bool = t333 == name__35
            if t334 {
                var t335 Typ = entry__39.ty
                var t336 Option__Typ = Some{
                    _0: t335,
                }
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Option_l_Typ_r_(found__37, t336)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(done__38, true)
            } else {
                var t338 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__36)
                var t339 int32 = t338 - 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__36, t339)
            }
            continue
        } else {
            break Loop_loop328
        }
    }
    var t327 Option__Typ = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Option_l_Typ_r_(found__37)
    retv323 = t327
    return retv323
}

func occurs(st__40 CheckerState, tvr__41 *ref_Tv_x, ty__42 Typ) Result__unit__string {
    var retv346 Result__unit__string
    var jp348 Result__unit__string
    switch ty__42.(type) {
    case TVar:
        var x48 *ref_Tv_x = ty__42.(TVar)._0
        var tvr2__43 *ref_Tv_x = x48
        var t351 bool = ptr_eq__Ref_2Tv(tvr__41, tvr2__43)
        var jp350 Result__unit__string
        if t351 {
            var t352 Result__unit__string = Result__unit__string_Err{
                _0: "occurs check",
            }
            jp350 = t352
        } else {
            var mtmp52 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvr2__43)
            var jp354 Result__unit__string
            switch mtmp52.(type) {
            case Unbound:
                var x53 string = mtmp52.(Unbound)._0
                var x54 int32 = mtmp52.(Unbound)._1
                var l2__45 int32 = x54
                var name__44 string = x53
                var mtmp56 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvr__41)
                var jp356 int32
                switch mtmp56.(type) {
                case Unbound:
                    var x58 int32 = mtmp56.(Unbound)._1
                    var l__46 int32 = x58
                    var t359 int32 = min_i32(l__46, l2__45)
                    jp356 = t359
                case Link:
                    jp356 = l2__45
                default:
                    panic("non-exhaustive match")
                }
                var min_level__47 int32 = jp356
                var t357 Tv = Unbound{
                    _0: name__44,
                    _1: min_level__47,
                }
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(tvr2__43, t357)
                var t358 Result__unit__string = Result__unit__string_Ok{
                    _0: struct{}{},
                }
                jp354 = t358
            case Link:
                var x55 Typ = mtmp52.(Link)._0
                var inner__48 Typ = x55
                var t360 Result__unit__string = occurs(st__40, tvr__41, inner__48)
                jp354 = t360
            default:
                panic("non-exhaustive match")
            }
            jp350 = jp354
        }
        jp348 = jp350
    case QVar:
        var t361 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp348 = t361
    case TArrow:
        var x50 Typ = ty__42.(TArrow)._0
        var x51 Typ = ty__42.(TArrow)._1
        var t2__50 Typ = x51
        var t1__49 Typ = x50
        var mtmp61 Result__unit__string = occurs(st__40, tvr__41, t1__49)
        var jp363 Result__unit__string
        switch mtmp61.(type) {
        case Result__unit__string_Ok:
            var t364 Result__unit__string = occurs(st__40, tvr__41, t2__50)
            jp363 = t364
        case Result__unit__string_Err:
            var x63 string = mtmp61.(Result__unit__string_Err)._0
            var e__51 string = x63
            var t365 Result__unit__string = Result__unit__string_Err{
                _0: e__51,
            }
            jp363 = t365
        default:
            panic("non-exhaustive match")
        }
        jp348 = jp363
    default:
        panic("non-exhaustive match")
    }
    retv346 = jp348
    return retv346
}

func unify(st__52 CheckerState, t1__53 Typ, t2__54 Typ) Result__unit__string {
    var retv367 Result__unit__string
    var mtmp64 Tuple2_3Typ_3Typ = Tuple2_3Typ_3Typ{
        _0: t1__53,
        _1: t2__54,
    }
    var x65 Typ = mtmp64._0
    var x66 Typ = mtmp64._1
    var jp369 Result__unit__string
    switch x66.(type) {
    case TVar:
        var x67 *ref_Tv_x = x66.(TVar)._0
        var jp371 Result__unit__string
        switch x65.(type) {
        case TVar:
            var x71 *ref_Tv_x = x65.(TVar)._0
            var r1__55 *ref_Tv_x = x71
            var r2__56 *ref_Tv_x = x67
            var t374 bool = ptr_eq__Ref_2Tv(r1__55, r2__56)
            var jp373 Result__unit__string
            if t374 {
                var t375 Result__unit__string = Result__unit__string_Ok{
                    _0: struct{}{},
                }
                jp373 = t375
            } else {
                var mtmp75 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r1__55)
                var jp377 Result__unit__string
                switch mtmp75.(type) {
                case Unbound:
                    var mtmp79 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r2__56)
                    var jp379 Result__unit__string
                    switch mtmp79.(type) {
                    case Unbound:
                        var t380 Typ = TVar{
                            _0: r2__56,
                        }
                        var mtmp83 Result__unit__string = occurs(st__52, r1__55, t380)
                        var jp382 Result__unit__string
                        switch mtmp83.(type) {
                        case Result__unit__string_Ok:
                            var t383 Typ = TVar{
                                _0: r2__56,
                            }
                            var t384 Tv = Link{
                                _0: t383,
                            }
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(r1__55, t384)
                            var t385 Result__unit__string = Result__unit__string_Ok{
                                _0: struct{}{},
                            }
                            jp382 = t385
                        case Result__unit__string_Err:
                            var x85 string = mtmp83.(Result__unit__string_Err)._0
                            var e__59 string = x85
                            var t386 Result__unit__string = Result__unit__string_Err{
                                _0: e__59,
                            }
                            jp382 = t386
                        default:
                            panic("non-exhaustive match")
                        }
                        jp379 = jp382
                    case Link:
                        var x82 Typ = mtmp79.(Link)._0
                        var inner__58 Typ = x82
                        var t387 Typ = TVar{
                            _0: r1__55,
                        }
                        var t388 Result__unit__string = unify(st__52, t387, inner__58)
                        jp379 = t388
                    default:
                        panic("non-exhaustive match")
                    }
                    jp377 = jp379
                case Link:
                    var x78 Typ = mtmp75.(Link)._0
                    var inner__57 Typ = x78
                    var t389 Typ = TVar{
                        _0: r2__56,
                    }
                    var t390 Result__unit__string = unify(st__52, inner__57, t389)
                    jp377 = t390
                default:
                    panic("non-exhaustive match")
                }
                jp373 = jp377
            }
            jp371 = jp373
        case QVar:
            var r2__65 *ref_Tv_x = x67
            var other__64 Typ = x65
            var mtmp87 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r2__65)
            var jp392 Result__unit__string
            switch mtmp87.(type) {
            case Unbound:
                var mtmp91 Result__unit__string = occurs(st__52, r2__65, other__64)
                var jp394 Result__unit__string
                switch mtmp91.(type) {
                case Result__unit__string_Ok:
                    var t395 Tv = Link{
                        _0: other__64,
                    }
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(r2__65, t395)
                    var t396 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    jp394 = t396
                case Result__unit__string_Err:
                    var x93 string = mtmp91.(Result__unit__string_Err)._0
                    var e__67 string = x93
                    var t397 Result__unit__string = Result__unit__string_Err{
                        _0: e__67,
                    }
                    jp394 = t397
                default:
                    panic("non-exhaustive match")
                }
                jp392 = jp394
            case Link:
                var x90 Typ = mtmp87.(Link)._0
                var inner__66 Typ = x90
                var t398 Result__unit__string = unify(st__52, other__64, inner__66)
                jp392 = t398
            default:
                panic("non-exhaustive match")
            }
            jp371 = jp392
        case TArrow:
            var r2__65 *ref_Tv_x = x67
            var other__64 Typ = x65
            var mtmp95 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r2__65)
            var jp400 Result__unit__string
            switch mtmp95.(type) {
            case Unbound:
                var mtmp99 Result__unit__string = occurs(st__52, r2__65, other__64)
                var jp402 Result__unit__string
                switch mtmp99.(type) {
                case Result__unit__string_Ok:
                    var t403 Tv = Link{
                        _0: other__64,
                    }
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(r2__65, t403)
                    var t404 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    jp402 = t404
                case Result__unit__string_Err:
                    var x101 string = mtmp99.(Result__unit__string_Err)._0
                    var e__67 string = x101
                    var t405 Result__unit__string = Result__unit__string_Err{
                        _0: e__67,
                    }
                    jp402 = t405
                default:
                    panic("non-exhaustive match")
                }
                jp400 = jp402
            case Link:
                var x98 Typ = mtmp95.(Link)._0
                var inner__66 Typ = x98
                var t406 Result__unit__string = unify(st__52, other__64, inner__66)
                jp400 = t406
            default:
                panic("non-exhaustive match")
            }
            jp371 = jp400
        default:
            panic("non-exhaustive match")
        }
        jp369 = jp371
    case QVar:
        var jp408 Result__unit__string
        switch x65.(type) {
        case TVar:
            var x103 *ref_Tv_x = x65.(TVar)._0
            var r1__60 *ref_Tv_x = x103
            var other__61 Typ = x66
            var mtmp107 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r1__60)
            var jp410 Result__unit__string
            switch mtmp107.(type) {
            case Unbound:
                var mtmp111 Result__unit__string = occurs(st__52, r1__60, other__61)
                var jp412 Result__unit__string
                switch mtmp111.(type) {
                case Result__unit__string_Ok:
                    var t413 Tv = Link{
                        _0: other__61,
                    }
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(r1__60, t413)
                    var t414 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    jp412 = t414
                case Result__unit__string_Err:
                    var x113 string = mtmp111.(Result__unit__string_Err)._0
                    var e__63 string = x113
                    var t415 Result__unit__string = Result__unit__string_Err{
                        _0: e__63,
                    }
                    jp412 = t415
                default:
                    panic("non-exhaustive match")
                }
                jp410 = jp412
            case Link:
                var x110 Typ = mtmp107.(Link)._0
                var inner__62 Typ = x110
                var t416 Result__unit__string = unify(st__52, inner__62, other__61)
                jp410 = t416
            default:
                panic("non-exhaustive match")
            }
            jp408 = jp410
        case QVar:
            var t417 Result__unit__string = Result__unit__string_Err{
                _0: "unify error",
            }
            jp408 = t417
        case TArrow:
            var t418 Result__unit__string = Result__unit__string_Err{
                _0: "unify error",
            }
            jp408 = t418
        default:
            panic("non-exhaustive match")
        }
        jp369 = jp408
    case TArrow:
        var x69 Typ = x66.(TArrow)._0
        var x70 Typ = x66.(TArrow)._1
        var jp420 Result__unit__string
        switch x65.(type) {
        case TVar:
            var x115 *ref_Tv_x = x65.(TVar)._0
            var r1__60 *ref_Tv_x = x115
            var other__61 Typ = x66
            var mtmp119 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r1__60)
            var jp422 Result__unit__string
            switch mtmp119.(type) {
            case Unbound:
                var mtmp123 Result__unit__string = occurs(st__52, r1__60, other__61)
                var jp424 Result__unit__string
                switch mtmp123.(type) {
                case Result__unit__string_Ok:
                    var t425 Tv = Link{
                        _0: other__61,
                    }
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(r1__60, t425)
                    var t426 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    jp424 = t426
                case Result__unit__string_Err:
                    var x125 string = mtmp123.(Result__unit__string_Err)._0
                    var e__63 string = x125
                    var t427 Result__unit__string = Result__unit__string_Err{
                        _0: e__63,
                    }
                    jp424 = t427
                default:
                    panic("non-exhaustive match")
                }
                jp422 = jp424
            case Link:
                var x122 Typ = mtmp119.(Link)._0
                var inner__62 Typ = x122
                var t428 Result__unit__string = unify(st__52, inner__62, other__61)
                jp422 = t428
            default:
                panic("non-exhaustive match")
            }
            jp420 = jp422
        case QVar:
            var t429 Result__unit__string = Result__unit__string_Err{
                _0: "unify error",
            }
            jp420 = t429
        case TArrow:
            var x117 Typ = x65.(TArrow)._0
            var x118 Typ = x65.(TArrow)._1
            var a2__69 Typ = x118
            var a1__68 Typ = x117
            var b2__71 Typ = x70
            var b1__70 Typ = x69
            var mtmp127 Result__unit__string = unify(st__52, a1__68, b1__70)
            var jp431 Result__unit__string
            switch mtmp127.(type) {
            case Result__unit__string_Ok:
                var t432 Result__unit__string = unify(st__52, a2__69, b2__71)
                jp431 = t432
            case Result__unit__string_Err:
                var x129 string = mtmp127.(Result__unit__string_Err)._0
                var e__72 string = x129
                var t433 Result__unit__string = Result__unit__string_Err{
                    _0: e__72,
                }
                jp431 = t433
            default:
                panic("non-exhaustive match")
            }
            jp420 = jp431
        default:
            panic("non-exhaustive match")
        }
        jp369 = jp420
    default:
        panic("non-exhaustive match")
    }
    retv367 = jp369
    return retv367
}

func gen(st__73 CheckerState, ty__74 Typ) Typ {
    var retv435 Typ
    var jp437 Typ
    switch ty__74.(type) {
    case TVar:
        var x130 *ref_Tv_x = ty__74.(TVar)._0
        var tvref__75 *ref_Tv_x = x130
        var mtmp134 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvref__75)
        var jp439 Typ
        switch mtmp134.(type) {
        case Unbound:
            var x135 string = mtmp134.(Unbound)._0
            var x136 int32 = mtmp134.(Unbound)._1
            var l__77 int32 = x136
            var name__76 string = x135
            var t440 *ref_int32_x = st__73.current_level
            var cur__78 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t440)
            var t443 bool = l__77 > cur__78
            var jp442 Typ
            if t443 {
                var t444 Typ = QVar{
                    _0: name__76,
                }
                jp442 = t444
            } else {
                var t445 Typ = TVar{
                    _0: tvref__75,
                }
                jp442 = t445
            }
            jp439 = jp442
        case Link:
            var x137 Typ = mtmp134.(Link)._0
            var inner__79 Typ = x137
            var t446 Typ = gen(st__73, inner__79)
            jp439 = t446
        default:
            panic("non-exhaustive match")
        }
        jp437 = jp439
    case QVar:
        var other__82 Typ = ty__74
        jp437 = other__82
    case TArrow:
        var x132 Typ = ty__74.(TArrow)._0
        var x133 Typ = ty__74.(TArrow)._1
        var t2__81 Typ = x133
        var t1__80 Typ = x132
        var t447 Typ = gen(st__73, t1__80)
        var t448 Typ = gen(st__73, t2__81)
        var t449 Typ = TArrow{
            _0: t447,
            _1: t448,
        }
        jp437 = t449
    default:
        panic("non-exhaustive match")
    }
    retv435 = jp437
    return retv435
}

func inst_loop(st__83 CheckerState, subst__84 *_goml_vec_SubstEntry, ty__85 Typ) Tuple2_3Typ_16Vec_10SubstEntry {
    var retv451 Tuple2_3Typ_16Vec_10SubstEntry
    var jp453 Tuple2_3Typ_16Vec_10SubstEntry
    switch ty__85.(type) {
    case TVar:
        var x138 *ref_Tv_x = ty__85.(TVar)._0
        var tvref__90 *ref_Tv_x = x138
        var mtmp142 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvref__90)
        var jp455 Tuple2_3Typ_16Vec_10SubstEntry
        switch mtmp142.(type) {
        case Unbound:
            var t456 Typ = TVar{
                _0: tvref__90,
            }
            var t457 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: t456,
                _1: subst__84,
            }
            jp455 = t457
        case Link:
            var x145 Typ = mtmp142.(Link)._0
            var inner__91 Typ = x145
            var t458 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, subst__84, inner__91)
            jp455 = t458
        default:
            panic("non-exhaustive match")
        }
        jp453 = jp455
    case QVar:
        var x139 string = ty__85.(QVar)._0
        var name__86 string = x139
        var mtmp146 Option__Typ = subst_lookup(subst__84, name__86)
        var jp460 Tuple2_3Typ_16Vec_10SubstEntry
        switch mtmp146.(type) {
        case None:
            var tv__88 Typ = newvar(st__83)
            var t461 SubstEntry = SubstEntry{
                name: name__86,
                ty: tv__88,
            }
            var new_subst__89 *_goml_vec_SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SubstEntry(subst__84, t461)
            var t462 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: tv__88,
                _1: new_subst__89,
            }
            jp460 = t462
        case Some:
            var x147 Typ = mtmp146.(Some)._0
            var t__87 Typ = x147
            var t463 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: t__87,
                _1: subst__84,
            }
            jp460 = t463
        default:
            panic("non-exhaustive match")
        }
        jp453 = jp460
    case TArrow:
        var x140 Typ = ty__85.(TArrow)._0
        var x141 Typ = ty__85.(TArrow)._1
        var t2__93 Typ = x141
        var t1__92 Typ = x140
        var mtmp148 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, subst__84, t1__92)
        var x149 Typ = mtmp148._0
        var x150 *_goml_vec_SubstEntry = mtmp148._1
        var subst1__95 *_goml_vec_SubstEntry = x150
        var ty1__94 Typ = x149
        var mtmp151 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, subst1__95, t2__93)
        var x152 Typ = mtmp151._0
        var x153 *_goml_vec_SubstEntry = mtmp151._1
        var subst2__97 *_goml_vec_SubstEntry = x153
        var ty2__96 Typ = x152
        var t464 Typ = TArrow{
            _0: ty1__94,
            _1: ty2__96,
        }
        var t465 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
            _0: t464,
            _1: subst2__97,
        }
        jp453 = t465
    default:
        panic("non-exhaustive match")
    }
    retv451 = jp453
    return retv451
}

func inst(st__98 CheckerState, ty__99 Typ) Typ {
    var retv467 Typ
    var subst0__100 *_goml_vec_SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SubstEntry()
    var mtmp154 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__98, subst0__100, ty__99)
    var x155 Typ = mtmp154._0
    var t__101 Typ = x155
    retv467 = t__101
    return retv467
}

func typeof(st__102 CheckerState, env__103 *_goml_vec_EnvEntry, e__104 Exp) Result__Typ__string {
    var retv469 Result__Typ__string
    var jp471 Result__Typ__string
    switch e__104.(type) {
    case Var:
        var x157 string = e__104.(Var)._0
        var x__105 string = x157
        var mtmp165 Option__Typ = env_lookup(env__103, x__105)
        var jp473 Result__Typ__string
        switch mtmp165.(type) {
        case None:
            var t474 Result__Typ__string = Result__Typ__string_Err{
                _0: "unbound var",
            }
            jp473 = t474
        case Some:
            var x166 Typ = mtmp165.(Some)._0
            var ty__106 Typ = x166
            var t475 Typ = inst(st__102, ty__106)
            var t476 Result__Typ__string = Result__Typ__string_Ok{
                _0: t475,
            }
            jp473 = t476
        default:
            panic("non-exhaustive match")
        }
        jp471 = jp473
    case App:
        var x158 Exp = e__104.(App)._0
        var x159 Exp = e__104.(App)._1
        var e2__114 Exp = x159
        var e1__113 Exp = x158
        var mtmp167 Result__Typ__string = typeof(st__102, env__103, e1__113)
        var jp478 Result__Typ__string
        switch mtmp167.(type) {
        case Result__Typ__string_Ok:
            var x168 Typ = mtmp167.(Result__Typ__string_Ok)._0
            var ty_fun__116 Typ = x168
            var mtmp170 Result__Typ__string = typeof(st__102, env__103, e2__114)
            var jp480 Result__Typ__string
            switch mtmp170.(type) {
            case Result__Typ__string_Ok:
                var x171 Typ = mtmp170.(Result__Typ__string_Ok)._0
                var ty_arg__118 Typ = x171
                var ty_res__119 Typ = newvar(st__102)
                var arrow__120 Typ = TArrow{
                    _0: ty_arg__118,
                    _1: ty_res__119,
                }
                var mtmp173 Result__unit__string = unify(st__102, ty_fun__116, arrow__120)
                var jp482 Result__Typ__string
                switch mtmp173.(type) {
                case Result__unit__string_Ok:
                    var t483 Result__Typ__string = Result__Typ__string_Ok{
                        _0: ty_res__119,
                    }
                    jp482 = t483
                case Result__unit__string_Err:
                    var x175 string = mtmp173.(Result__unit__string_Err)._0
                    var e__121 string = x175
                    var t484 Result__Typ__string = Result__Typ__string_Err{
                        _0: e__121,
                    }
                    jp482 = t484
                default:
                    panic("non-exhaustive match")
                }
                jp480 = jp482
            case Result__Typ__string_Err:
                var x172 string = mtmp170.(Result__Typ__string_Err)._0
                var e__117 string = x172
                var t485 Result__Typ__string = Result__Typ__string_Err{
                    _0: e__117,
                }
                jp480 = t485
            default:
                panic("non-exhaustive match")
            }
            jp478 = jp480
        case Result__Typ__string_Err:
            var x169 string = mtmp167.(Result__Typ__string_Err)._0
            var e__115 string = x169
            var t486 Result__Typ__string = Result__Typ__string_Err{
                _0: e__115,
            }
            jp478 = t486
        default:
            panic("non-exhaustive match")
        }
        jp471 = jp478
    case Lam:
        var x160 string = e__104.(Lam)._0
        var x161 Exp = e__104.(Lam)._1
        var body__108 Exp = x161
        var x__107 string = x160
        var ty_x__109 Typ = newvar(st__102)
        var t487 EnvEntry = EnvEntry{
            name: x__107,
            ty: ty_x__109,
        }
        var env2__110 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__EnvEntry(env__103, t487)
        var mtmp176 Result__Typ__string = typeof(st__102, env2__110, body__108)
        var jp489 Result__Typ__string
        switch mtmp176.(type) {
        case Result__Typ__string_Ok:
            var x177 Typ = mtmp176.(Result__Typ__string_Ok)._0
            var ty_e__111 Typ = x177
            var t490 Typ = TArrow{
                _0: ty_x__109,
                _1: ty_e__111,
            }
            var t491 Result__Typ__string = Result__Typ__string_Ok{
                _0: t490,
            }
            jp489 = t491
        case Result__Typ__string_Err:
            var x178 string = mtmp176.(Result__Typ__string_Err)._0
            var e__112 string = x178
            var t492 Result__Typ__string = Result__Typ__string_Err{
                _0: e__112,
            }
            jp489 = t492
        default:
            panic("non-exhaustive match")
        }
        jp471 = jp489
    case Let:
        var x162 string = e__104.(Let)._0
        var x163 Exp = e__104.(Let)._1
        var x164 Exp = e__104.(Let)._2
        var e2__124 Exp = x164
        var e1__123 Exp = x163
        var x__122 string = x162
        enter_level(st__102)
        var ty_e__125 Result__Typ__string = typeof(st__102, env__103, e1__123)
        leave_level(st__102)
        var jp494 Result__Typ__string
        switch ty_e__125.(type) {
        case Result__Typ__string_Ok:
            var x181 Typ = ty_e__125.(Result__Typ__string_Ok)._0
            var ty1__127 Typ = x181
            var t495 Typ = gen(st__102, ty1__127)
            var t496 EnvEntry = EnvEntry{
                name: x__122,
                ty: t495,
            }
            var env2__128 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__EnvEntry(env__103, t496)
            var t497 Result__Typ__string = typeof(st__102, env2__128, e2__124)
            jp494 = t497
        case Result__Typ__string_Err:
            var x182 string = ty_e__125.(Result__Typ__string_Err)._0
            var e__126 string = x182
            var t498 Result__Typ__string = Result__Typ__string_Err{
                _0: e__126,
            }
            jp494 = t498
        default:
            panic("non-exhaustive match")
        }
        jp471 = jp494
    default:
        panic("non-exhaustive match")
    }
    retv469 = jp471
    return retv469
}

func exp_var(name__129 string) Exp {
    var retv500 Exp
    var t501 Exp = Var{
        _0: name__129,
    }
    retv500 = t501
    return retv500
}

func exp_lam(name__130 string, body__131 Exp) Exp {
    var retv503 Exp
    var t504 Exp = Lam{
        _0: name__130,
        _1: body__131,
    }
    retv503 = t504
    return retv503
}

func exp_app(a__132 Exp, b__133 Exp) Exp {
    var retv506 Exp
    var t507 Exp = App{
        _0: a__132,
        _1: b__133,
    }
    retv506 = t507
    return retv506
}

func exp_let(name__134 string, a__135 Exp, b__136 Exp) Exp {
    var retv509 Exp
    var t510 Exp = Let{
        _0: name__134,
        _1: a__135,
        _2: b__136,
    }
    retv509 = t510
    return retv509
}

func show_result(label__137 string, res__138 Result__Typ__string) struct{} {
    switch res__138.(type) {
    case Result__Typ__string_Ok:
        var x183 Typ = res__138.(Result__Typ__string_Ok)._0
        var ty__139 Typ = x183
        var t513 string = label__137 + ": "
        var t514 string = typ_to_string(ty__139)
        var t515 string = t513 + t514
        println__T_string(t515)
    case Result__Typ__string_Err:
        var x184 string = res__138.(Result__Typ__string_Err)._0
        var e__140 string = x184
        var t517 string = label__137 + ": "
        var t518 string = t517 + e__140
        println__T_string(t518)
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func main0() struct{} {
    var st__141 CheckerState = state_new()
    var t521 Exp = exp_var("x")
    var id__142 Exp = exp_lam("x", t521)
    var t522 Exp = exp_var("x")
    var t523 Exp = exp_var("y")
    var t524 Exp = exp_app(t522, t523)
    var t525 Exp = exp_lam("y", t524)
    var c1__143 Exp = exp_lam("x", t525)
    reset_type_variables(st__141)
    var t526 *_goml_vec_EnvEntry = env_empty()
    var t527 Result__Typ__string = typeof(st__141, t526, id__142)
    show_result("id", t527)
    reset_type_variables(st__141)
    var t528 *_goml_vec_EnvEntry = env_empty()
    var t529 Result__Typ__string = typeof(st__141, t528, c1__143)
    show_result("c1", t529)
    reset_type_variables(st__141)
    var t530 *_goml_vec_EnvEntry = env_empty()
    var t531 Exp = exp_var("x")
    var t532 Exp = exp_let("x", c1__143, t531)
    var t533 Result__Typ__string = typeof(st__141, t530, t532)
    show_result("let_x_c1_x", t533)
    reset_type_variables(st__141)
    var t534 *_goml_vec_EnvEntry = env_empty()
    var t535 Exp = exp_var("z")
    var t536 Exp = exp_lam("z", t535)
    var t537 Exp = exp_var("y")
    var t538 Exp = exp_let("y", t536, t537)
    var t539 Result__Typ__string = typeof(st__141, t534, t538)
    show_result("let_y_id_y", t539)
    reset_type_variables(st__141)
    var t540 *_goml_vec_EnvEntry = env_empty()
    var t541 Exp = exp_var("z")
    var t542 Exp = exp_lam("z", t541)
    var t543 Exp = exp_var("y")
    var t544 Exp = exp_let("y", t542, t543)
    var t545 Exp = exp_lam("x", t544)
    var t546 Result__Typ__string = typeof(st__141, t540, t545)
    show_result("lam_x_let_y_id_y", t546)
    reset_type_variables(st__141)
    var t547 *_goml_vec_EnvEntry = env_empty()
    var t548 Exp = exp_var("z")
    var t549 Exp = exp_lam("z", t548)
    var t550 Exp = exp_var("y")
    var t551 Exp = exp_var("x")
    var t552 Exp = exp_app(t550, t551)
    var t553 Exp = exp_let("y", t549, t552)
    var t554 Exp = exp_lam("x", t553)
    var t555 Result__Typ__string = typeof(st__141, t547, t554)
    show_result("lam_x_let_y_id_yx", t555)
    reset_type_variables(st__141)
    var t556 *_goml_vec_EnvEntry = env_empty()
    var t557 Exp = exp_var("x")
    var t558 Exp = exp_var("x")
    var t559 Exp = exp_app(t557, t558)
    var t560 Exp = exp_lam("x", t559)
    var t561 Result__Typ__string = typeof(st__141, t556, t560)
    show_result("self_apply", t561)
    reset_type_variables(st__141)
    var t562 *_goml_vec_EnvEntry = env_empty()
    var t563 Exp = exp_var("x")
    var t564 Exp = exp_var("x")
    var t565 Exp = exp_let("x", t563, t564)
    var t566 Result__Typ__string = typeof(st__141, t562, t565)
    show_result("unbound_var", t566)
    reset_type_variables(st__141)
    var t567 *_goml_vec_EnvEntry = env_empty()
    var t568 Exp = exp_var("y")
    var t569 Exp = exp_var("y")
    var t570 Exp = exp_var("z")
    var t571 Exp = exp_app(t569, t570)
    var t572 Exp = exp_lam("z", t571)
    var t573 Exp = exp_app(t568, t572)
    var t574 Exp = exp_lam("y", t573)
    var t575 Result__Typ__string = typeof(st__141, t567, t574)
    show_result("max_heiber", t575)
    reset_type_variables(st__141)
    var t576 *_goml_vec_EnvEntry = env_empty()
    var t577 Exp = exp_var("k")
    var t578 Exp = exp_var("k")
    var t579 Exp = exp_var("x")
    var t580 Exp = exp_app(t578, t579)
    var t581 Exp = exp_var("y")
    var t582 Exp = exp_app(t580, t581)
    var t583 Exp = exp_app(t577, t582)
    var t584 Exp = exp_var("k")
    var t585 Exp = exp_var("y")
    var t586 Exp = exp_app(t584, t585)
    var t587 Exp = exp_var("x")
    var t588 Exp = exp_app(t586, t587)
    var t589 Exp = exp_app(t583, t588)
    var t590 Exp = exp_lam("k", t589)
    var t591 Exp = exp_lam("y", t590)
    var t592 Exp = exp_lam("x", t591)
    var t593 Result__Typ__string = typeof(st__141, t576, t592)
    show_result("kirang", t593)
    reset_type_variables(st__141)
    var t594 *_goml_vec_EnvEntry = env_empty()
    var t595 Exp = exp_var("id")
    var t596 Exp = exp_var("id")
    var t597 Exp = exp_app(t595, t596)
    var t598 Exp = exp_let("id", id__142, t597)
    var t599 Result__Typ__string = typeof(st__141, t594, t598)
    show_result("let_id_idid", t599)
    reset_type_variables(st__141)
    var t600 *_goml_vec_EnvEntry = env_empty()
    var t601 Exp = exp_var("x")
    var t602 Exp = exp_app(t601, id__142)
    var t603 Exp = exp_var("z")
    var t604 Exp = exp_let("z", t602, t603)
    var t605 Exp = exp_var("y")
    var t606 Exp = exp_let("y", t604, t605)
    var t607 Exp = exp_let("x", c1__143, t606)
    var t608 Result__Typ__string = typeof(st__141, t600, t607)
    show_result("nested_lets", t608)
    reset_type_variables(st__141)
    var t609 *_goml_vec_EnvEntry = env_empty()
    var t610 Exp = exp_var("x")
    var t611 Exp = exp_var("y")
    var t612 Exp = exp_app(t610, t611)
    var t613 Exp = exp_var("y")
    var t614 Exp = exp_var("x")
    var t615 Exp = exp_app(t613, t614)
    var t616 Exp = exp_lam("x", t615)
    var t617 Exp = exp_let("x", t612, t616)
    var t618 Exp = exp_lam("y", t617)
    var t619 Exp = exp_lam("x", t618)
    var t620 Result__Typ__string = typeof(st__141, t609, t619)
    show_result("fun_x_fun_y_let_x_xy_fun_x_yx", t620)
    reset_type_variables(st__141)
    var t621 *_goml_vec_EnvEntry = env_empty()
    var t622 Exp = exp_var("x")
    var t623 Exp = exp_var("y")
    var t624 Exp = exp_let("y", t622, t623)
    var t625 Exp = exp_lam("x", t624)
    var t626 Result__Typ__string = typeof(st__141, t621, t625)
    show_result("sound_gen_1", t626)
    reset_type_variables(st__141)
    var t627 *_goml_vec_EnvEntry = env_empty()
    var t628 Exp = exp_var("x")
    var t629 Exp = exp_lam("z", t628)
    var t630 Exp = exp_var("y")
    var t631 Exp = exp_let("y", t629, t630)
    var t632 Exp = exp_lam("x", t631)
    var t633 Result__Typ__string = typeof(st__141, t627, t632)
    show_result("sound_gen_2", t633)
    reset_type_variables(st__141)
    var t634 *_goml_vec_EnvEntry = env_empty()
    var t635 Exp = exp_var("x")
    var t636 Exp = exp_var("z")
    var t637 Exp = exp_app(t635, t636)
    var t638 Exp = exp_lam("z", t637)
    var t639 Exp = exp_var("y")
    var t640 Exp = exp_let("y", t638, t639)
    var t641 Exp = exp_lam("x", t640)
    var t642 Result__Typ__string = typeof(st__141, t634, t641)
    show_result("sound_gen_3", t642)
    reset_type_variables(st__141)
    var t643 *_goml_vec_EnvEntry = env_empty()
    var t644 Exp = exp_var("x")
    var t645 Exp = exp_var("y")
    var t646 Exp = exp_app(t644, t645)
    var t647 Exp = exp_var("x")
    var t648 Exp = exp_var("y")
    var t649 Exp = exp_app(t647, t648)
    var t650 Exp = exp_let("x", t646, t649)
    var t651 Exp = exp_lam("y", t650)
    var t652 Exp = exp_lam("x", t651)
    var t653 Result__Typ__string = typeof(st__141, t643, t652)
    show_result("double_apply", t653)
    reset_type_variables(st__141)
    var t654 *_goml_vec_EnvEntry = env_empty()
    var t655 Exp = exp_var("x")
    var t656 Exp = exp_var("y")
    var t657 Exp = exp_var("y")
    var t658 Exp = exp_app(t656, t657)
    var t659 Exp = exp_let("y", t655, t658)
    var t660 Exp = exp_lam("x", t659)
    var t661 Result__Typ__string = typeof(st__141, t654, t660)
    show_result("sound_gen_occurs", t661)
    reset_gensym(st__141)
    var t662 *_goml_vec_EnvEntry = env_empty()
    var t663 Exp = exp_var("x")
    var t664 Exp = exp_app(t663, id__142)
    var t665 Exp = exp_var("z")
    var t666 Exp = exp_let("z", t664, t665)
    var t667 Exp = exp_var("y")
    var t668 Exp = exp_let("y", t666, t667)
    var t669 Exp = exp_lam("x", t668)
    var t670 Result__Typ__string = typeof(st__141, t662, t669)
    show_result("fun_x_let_y_let_z_x_id_z_y", t670)
    println__T_string("")
    println__T_string("All Done")
    println__T_string("")
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__137 int32) *ref_int32_x {
    var retv672 *ref_int32_x
    var t673 *ref_int32_x = ref__Ref_5int32(value__137)
    retv672 = t673
    return retv672
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__139 *ref_int32_x, value__140 int32) struct{} {
    ref_set__Ref_5int32(self__139, value__140)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__138 *ref_int32_x) int32 {
    var retv677 int32
    var t678 int32 = ref_get__Ref_5int32(self__138)
    retv677 = t678
    return retv677
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__3 rune) string {
    var retv680 string
    var t681 string = _goml_runtime_core_char_to_string(self__3)
    retv680 = t681
    return retv680
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv683 string
    var t684 string = _goml_runtime_core_int32_to_string(self__2)
    retv683 = t684
    return retv683
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Tv(value__137 Tv) *ref_Tv_x {
    var retv686 *ref_Tv_x
    var t687 *ref_Tv_x = ref__Ref_2Tv(value__137)
    retv686 = t687
    return retv686
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(self__138 *ref_Tv_x) Tv {
    var retv689 Tv
    var t690 Tv = ref_get__Ref_2Tv(self__138)
    retv689 = t690
    return retv689
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__EnvEntry() *_goml_vec_EnvEntry {
    var retv692 *_goml_vec_EnvEntry
    var t693 *_goml_vec_EnvEntry = vec_new__Vec_8EnvEntry()
    retv692 = t693
    return retv692
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__EnvEntry(self__107 *_goml_vec_EnvEntry) int32 {
    var retv695 int32
    var t696 int32 = vec_len__Vec_8EnvEntry(self__107)
    retv695 = t696
    return retv695
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_Typ_r_(value__137 Option__Typ) *ref_Option__Typ_x {
    var retv698 *ref_Option__Typ_x
    var t699 *ref_Option__Typ_x = ref__Ref_11Option__Typ(value__137)
    retv698 = t699
    return retv698
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__137 bool) *ref_bool_x {
    var retv701 *ref_bool_x
    var t702 *ref_bool_x = ref__Ref_4bool(value__137)
    retv701 = t702
    return retv701
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__138 *ref_bool_x) bool {
    var retv704 bool
    var t705 bool = ref_get__Ref_4bool(self__138)
    retv704 = t705
    return retv704
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Option_l_Typ_r_(self__139 *ref_Option__Typ_x, value__140 Option__Typ) struct{} {
    ref_set__Ref_11Option__Typ(self__139, value__140)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(self__139 *ref_bool_x, value__140 bool) struct{} {
    ref_set__Ref_4bool(self__139, value__140)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Option_l_Typ_r_(self__138 *ref_Option__Typ_x) Option__Typ {
    var retv711 Option__Typ
    var t712 Option__Typ = ref_get__Ref_11Option__Typ(self__138)
    retv711 = t712
    return retv711
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SubstEntry(self__107 *_goml_vec_SubstEntry) int32 {
    var retv714 int32
    var t715 int32 = vec_len__Vec_10SubstEntry(self__107)
    retv714 = t715
    return retv714
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(self__139 *ref_Tv_x, value__140 Tv) struct{} {
    ref_set__Ref_2Tv(self__139, value__140)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SubstEntry(self__98 *_goml_vec_SubstEntry, elem__99 SubstEntry) *_goml_vec_SubstEntry {
    var retv719 *_goml_vec_SubstEntry
    var result__100 *_goml_vec_SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SubstEntry()
    var index__101 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop721:
    for {
        var t722 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__101)
        var t723 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SubstEntry(self__98)
        var t724 bool = t722 < t723
        if t724 {
            var t725 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__101)
            var t726 SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__SubstEntry(self__98, t725)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SubstEntry(result__100, t726)
            var t727 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__101)
            var t728 int32 = t727 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__101, t728)
            continue
        } else {
            break Loop_loop721
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SubstEntry(result__100, elem__99)
    retv719 = result__100
    return retv719
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SubstEntry() *_goml_vec_SubstEntry {
    var retv730 *_goml_vec_SubstEntry
    var t731 *_goml_vec_SubstEntry = vec_new__Vec_10SubstEntry()
    retv730 = t731
    return retv730
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__EnvEntry(self__98 *_goml_vec_EnvEntry, elem__99 EnvEntry) *_goml_vec_EnvEntry {
    var retv733 *_goml_vec_EnvEntry
    var result__100 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__EnvEntry()
    var index__101 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop735:
    for {
        var t736 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__101)
        var t737 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__EnvEntry(self__98)
        var t738 bool = t736 < t737
        if t738 {
            var t739 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__101)
            var t740 EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__EnvEntry(self__98, t739)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__EnvEntry(result__100, t740)
            var t741 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__101)
            var t742 int32 = t741 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__101, t742)
            continue
        } else {
            break Loop_loop735
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__EnvEntry(result__100, elem__99)
    retv733 = result__100
    return retv733
}

func println__T_string(value__1 string) struct{} {
    var t744 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t744)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SubstEntry(self__96 *_goml_vec_SubstEntry, elem__97 SubstEntry) struct{} {
    vec_push__Vec_10SubstEntry(self__96, elem__97)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__SubstEntry(self__102 *_goml_vec_SubstEntry, index__103 int32) SubstEntry {
    var retv749 SubstEntry
    var t750 SubstEntry = vec_get__Vec_10SubstEntry(self__102, index__103)
    retv749 = t750
    return retv749
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__EnvEntry(self__96 *_goml_vec_EnvEntry, elem__97 EnvEntry) struct{} {
    vec_push__Vec_8EnvEntry(self__96, elem__97)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__EnvEntry(self__102 *_goml_vec_EnvEntry, index__103 int32) EnvEntry {
    var retv754 EnvEntry
    var t755 EnvEntry = vec_get__Vec_8EnvEntry(self__102, index__103)
    retv754 = t755
    return retv754
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv757 string
    retv757 = self__9
    return retv757
}

func main() {
    main0()
}
