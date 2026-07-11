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
    var retv212 CheckerState
    var t213 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var t214 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(1)
    var t215 CheckerState = CheckerState{
        gensym_counter: t213,
        current_level: t214,
    }
    retv212 = t215
    return retv212
}

func reset_gensym(st__0 CheckerState) struct{} {
    var t217 *ref_int32_x = st__0.gensym_counter
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t217, 0)
    return struct{}{}
}

func reset_level(st__1 CheckerState) struct{} {
    var t219 *ref_int32_x = st__1.current_level
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t219, 1)
    return struct{}{}
}

func reset_type_variables(st__2 CheckerState) struct{} {
    reset_gensym(st__2)
    reset_level(st__2)
    return struct{}{}
}

func enter_level(st__3 CheckerState) struct{} {
    var t223 *ref_int32_x = st__3.current_level
    var l__4 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t223)
    var t224 *ref_int32_x = st__3.current_level
    var t225 int32 = l__4 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t224, t225)
    return struct{}{}
}

func leave_level(st__5 CheckerState) struct{} {
    var t227 *ref_int32_x = st__5.current_level
    var l__6 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t227)
    var t228 *ref_int32_x = st__5.current_level
    var t229 int32 = l__6 - 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t228, t229)
    return struct{}{}
}

func min_i32(a__7 int32, b__8 int32) int32 {
    var retv231 int32
    var t234 bool = a__7 < b__8
    var jp233 int32
    if t234 {
        jp233 = a__7
    } else {
        jp233 = b__8
    }
    retv231 = jp233
    return retv231
}

func nth_letter(n__9 int32) rune {
    var retv236 rune
    var jp238 rune
    switch n__9 {
    case 0:
        jp238 = 97
    case 1:
        jp238 = 98
    case 2:
        jp238 = 99
    case 3:
        jp238 = 100
    case 4:
        jp238 = 101
    case 5:
        jp238 = 102
    case 6:
        jp238 = 103
    case 7:
        jp238 = 104
    case 8:
        jp238 = 105
    case 9:
        jp238 = 106
    case 10:
        jp238 = 107
    case 11:
        jp238 = 108
    case 12:
        jp238 = 109
    case 13:
        jp238 = 110
    case 14:
        jp238 = 111
    case 15:
        jp238 = 112
    case 16:
        jp238 = 113
    case 17:
        jp238 = 114
    case 18:
        jp238 = 115
    case 19:
        jp238 = 116
    case 20:
        jp238 = 117
    case 21:
        jp238 = 118
    case 22:
        jp238 = 119
    case 23:
        jp238 = 120
    case 24:
        jp238 = 121
    case 25:
        jp238 = 122
    default:
        jp238 = 97
    }
    retv236 = jp238
    return retv236
}

func gensym(st__10 CheckerState) string {
    var retv240 string
    var t241 *ref_int32_x = st__10.gensym_counter
    var n__11 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t241)
    var t242 *ref_int32_x = st__10.gensym_counter
    var t243 int32 = n__11 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t242, t243)
    var t246 bool = n__11 < 26
    var jp245 string
    if t246 {
        var t247 rune = nth_letter(n__11)
        var t248 string = _goml_m_inherent_i_char_i_char_i_to__string(t247)
        jp245 = t248
    } else {
        var t249 string = _goml_m_inherent_i_int32_i_int32_i_to__string(n__11)
        var t250 string = "t" + t249
        jp245 = t250
    }
    retv240 = jp245
    return retv240
}

func newvar(st__12 CheckerState) Typ {
    var retv252 Typ
    var name__13 string = gensym(st__12)
    var t253 *ref_int32_x = st__12.current_level
    var level__14 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t253)
    var t254 Tv = Unbound{
        _0: name__13,
        _1: level__14,
    }
    var t255 *ref_Tv_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Tv(t254)
    var t256 Typ = TVar{
        _0: t255,
    }
    retv252 = t256
    return retv252
}

func typ_is_arrow(ty__15 Typ) bool {
    var retv258 bool
    var jp260 bool
    switch ty__15.(type) {
    case TVar:
        var x13 *ref_Tv_x = ty__15.(TVar)._0
        var tvref__16 *ref_Tv_x = x13
        var mtmp17 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvref__16)
        var jp262 bool
        switch mtmp17.(type) {
        case Unbound:
            jp262 = false
        case Link:
            var x20 Typ = mtmp17.(Link)._0
            var inner__17 Typ = x20
            var t263 bool = typ_is_arrow(inner__17)
            jp262 = t263
        default:
            panic("non-exhaustive match")
        }
        jp260 = jp262
    case QVar:
        jp260 = false
    case TArrow:
        jp260 = true
    default:
        panic("non-exhaustive match")
    }
    retv258 = jp260
    return retv258
}

func typ_to_string(ty__18 Typ) string {
    var retv265 string
    var jp267 string
    switch ty__18.(type) {
    case TVar:
        var x21 *ref_Tv_x = ty__18.(TVar)._0
        var tvref__20 *ref_Tv_x = x21
        var mtmp25 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvref__20)
        var jp269 string
        switch mtmp25.(type) {
        case Unbound:
            var x26 string = mtmp25.(Unbound)._0
            var name__21 string = x26
            var t270 string = "'" + name__21
            jp269 = t270
        case Link:
            var x28 Typ = mtmp25.(Link)._0
            var inner__22 Typ = x28
            var t271 string = typ_to_string(inner__22)
            jp269 = t271
        default:
            panic("non-exhaustive match")
        }
        jp267 = jp269
    case QVar:
        var x22 string = ty__18.(QVar)._0
        var name__19 string = x22
        var t272 string = "'" + name__19
        jp267 = t272
    case TArrow:
        var x23 Typ = ty__18.(TArrow)._0
        var x24 Typ = ty__18.(TArrow)._1
        var t2__24 Typ = x24
        var t1__23 Typ = x23
        var t277 bool = typ_is_arrow(t1__23)
        var jp274 string
        if t277 {
            var t278 string = typ_to_string(t1__23)
            var t279 string = "(" + t278
            var t280 string = t279 + ")"
            jp274 = t280
        } else {
            var t281 string = typ_to_string(t1__23)
            jp274 = t281
        }
        var s1__25 string = jp274
        var s2__26 string = typ_to_string(t2__24)
        var t275 string = s1__25 + " -> "
        var t276 string = t275 + s2__26
        jp267 = t276
    default:
        panic("non-exhaustive match")
    }
    retv265 = jp267
    return retv265
}

func env_empty() *_goml_vec_EnvEntry {
    var retv283 *_goml_vec_EnvEntry
    var env__27 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__EnvEntry()
    retv283 = env__27
    return retv283
}

func env_lookup(env__28 *_goml_vec_EnvEntry, name__29 string) Option__Typ {
    var retv285 Option__Typ
    var t286 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__EnvEntry(env__28)
    var t287 int32 = t286 - 1
    var i__30 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(t287)
    var found__31 *ref_Option__Typ_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_Typ_r_(None{})
    var done__32 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    Loop_loop290:
    for {
        var t303 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(done__32)
        var t304 bool = !t303
        var jp292 bool
        if t304 {
            var t305 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__30)
            var t306 bool = t305 >= 0
            jp292 = t306
        } else {
            jp292 = false
        }
        if jp292 {
            var t293 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__30)
            var entry__33 EnvEntry = vec_get__Vec_8EnvEntry(env__28, t293)
            var t295 string = entry__33.name
            var t296 bool = t295 == name__29
            if t296 {
                var t297 Typ = entry__33.ty
                var t298 Option__Typ = Some{
                    _0: t297,
                }
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Option_l_Typ_r_(found__31, t298)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(done__32, true)
            } else {
                var t300 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__30)
                var t301 int32 = t300 - 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__30, t301)
            }
            continue
        } else {
            break Loop_loop290
        }
    }
    var t289 Option__Typ = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Option_l_Typ_r_(found__31)
    retv285 = t289
    return retv285
}

func subst_lookup(subst__34 *_goml_vec_SubstEntry, name__35 string) Option__Typ {
    var retv308 Option__Typ
    var t309 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SubstEntry(subst__34)
    var t310 int32 = t309 - 1
    var i__36 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(t310)
    var found__37 *ref_Option__Typ_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_Typ_r_(None{})
    var done__38 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    Loop_loop313:
    for {
        var t326 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(done__38)
        var t327 bool = !t326
        var jp315 bool
        if t327 {
            var t328 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__36)
            var t329 bool = t328 >= 0
            jp315 = t329
        } else {
            jp315 = false
        }
        if jp315 {
            var t316 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__36)
            var entry__39 SubstEntry = vec_get__Vec_10SubstEntry(subst__34, t316)
            var t318 string = entry__39.name
            var t319 bool = t318 == name__35
            if t319 {
                var t320 Typ = entry__39.ty
                var t321 Option__Typ = Some{
                    _0: t320,
                }
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Option_l_Typ_r_(found__37, t321)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(done__38, true)
            } else {
                var t323 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__36)
                var t324 int32 = t323 - 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__36, t324)
            }
            continue
        } else {
            break Loop_loop313
        }
    }
    var t312 Option__Typ = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Option_l_Typ_r_(found__37)
    retv308 = t312
    return retv308
}

func occurs(st__40 CheckerState, tvr__41 *ref_Tv_x, ty__42 Typ) Result__unit__string {
    var retv331 Result__unit__string
    var jp333 Result__unit__string
    switch ty__42.(type) {
    case TVar:
        var x33 *ref_Tv_x = ty__42.(TVar)._0
        var tvr2__43 *ref_Tv_x = x33
        var t336 bool = ptr_eq__Ref_2Tv(tvr__41, tvr2__43)
        var jp335 Result__unit__string
        if t336 {
            var t337 Result__unit__string = Result__unit__string_Err{
                _0: "occurs check",
            }
            jp335 = t337
        } else {
            var mtmp37 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvr2__43)
            var jp339 Result__unit__string
            switch mtmp37.(type) {
            case Unbound:
                var x38 string = mtmp37.(Unbound)._0
                var x39 int32 = mtmp37.(Unbound)._1
                var l2__45 int32 = x39
                var name__44 string = x38
                var mtmp41 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvr__41)
                var jp341 int32
                switch mtmp41.(type) {
                case Unbound:
                    var x43 int32 = mtmp41.(Unbound)._1
                    var l__46 int32 = x43
                    var t344 int32 = min_i32(l__46, l2__45)
                    jp341 = t344
                case Link:
                    jp341 = l2__45
                default:
                    panic("non-exhaustive match")
                }
                var min_level__47 int32 = jp341
                var t342 Tv = Unbound{
                    _0: name__44,
                    _1: min_level__47,
                }
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(tvr2__43, t342)
                var t343 Result__unit__string = Result__unit__string_Ok{
                    _0: struct{}{},
                }
                jp339 = t343
            case Link:
                var x40 Typ = mtmp37.(Link)._0
                var inner__48 Typ = x40
                var t345 Result__unit__string = occurs(st__40, tvr__41, inner__48)
                jp339 = t345
            default:
                panic("non-exhaustive match")
            }
            jp335 = jp339
        }
        jp333 = jp335
    case QVar:
        var t346 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp333 = t346
    case TArrow:
        var x35 Typ = ty__42.(TArrow)._0
        var x36 Typ = ty__42.(TArrow)._1
        var t2__50 Typ = x36
        var t1__49 Typ = x35
        var mtmp46 Result__unit__string = occurs(st__40, tvr__41, t1__49)
        var jp348 Result__unit__string
        switch mtmp46.(type) {
        case Result__unit__string_Ok:
            var t349 Result__unit__string = occurs(st__40, tvr__41, t2__50)
            jp348 = t349
        case Result__unit__string_Err:
            var x48 string = mtmp46.(Result__unit__string_Err)._0
            var e__51 string = x48
            var t350 Result__unit__string = Result__unit__string_Err{
                _0: e__51,
            }
            jp348 = t350
        default:
            panic("non-exhaustive match")
        }
        jp333 = jp348
    default:
        panic("non-exhaustive match")
    }
    retv331 = jp333
    return retv331
}

func unify(st__52 CheckerState, t1__53 Typ, t2__54 Typ) Result__unit__string {
    var retv352 Result__unit__string
    var mtmp49 Tuple2_3Typ_3Typ = Tuple2_3Typ_3Typ{
        _0: t1__53,
        _1: t2__54,
    }
    var x50 Typ = mtmp49._0
    var x51 Typ = mtmp49._1
    var jp354 Result__unit__string
    switch x51.(type) {
    case TVar:
        var x52 *ref_Tv_x = x51.(TVar)._0
        var jp356 Result__unit__string
        switch x50.(type) {
        case TVar:
            var x56 *ref_Tv_x = x50.(TVar)._0
            var r1__55 *ref_Tv_x = x56
            var r2__56 *ref_Tv_x = x52
            var t359 bool = ptr_eq__Ref_2Tv(r1__55, r2__56)
            var jp358 Result__unit__string
            if t359 {
                var t360 Result__unit__string = Result__unit__string_Ok{
                    _0: struct{}{},
                }
                jp358 = t360
            } else {
                var mtmp60 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r1__55)
                var jp362 Result__unit__string
                switch mtmp60.(type) {
                case Unbound:
                    var mtmp64 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r2__56)
                    var jp364 Result__unit__string
                    switch mtmp64.(type) {
                    case Unbound:
                        var t365 Typ = TVar{
                            _0: r2__56,
                        }
                        var mtmp68 Result__unit__string = occurs(st__52, r1__55, t365)
                        var jp367 Result__unit__string
                        switch mtmp68.(type) {
                        case Result__unit__string_Ok:
                            var t368 Typ = TVar{
                                _0: r2__56,
                            }
                            var t369 Tv = Link{
                                _0: t368,
                            }
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(r1__55, t369)
                            var t370 Result__unit__string = Result__unit__string_Ok{
                                _0: struct{}{},
                            }
                            jp367 = t370
                        case Result__unit__string_Err:
                            var x70 string = mtmp68.(Result__unit__string_Err)._0
                            var e__59 string = x70
                            var t371 Result__unit__string = Result__unit__string_Err{
                                _0: e__59,
                            }
                            jp367 = t371
                        default:
                            panic("non-exhaustive match")
                        }
                        jp364 = jp367
                    case Link:
                        var x67 Typ = mtmp64.(Link)._0
                        var inner__58 Typ = x67
                        var t372 Typ = TVar{
                            _0: r1__55,
                        }
                        var t373 Result__unit__string = unify(st__52, t372, inner__58)
                        jp364 = t373
                    default:
                        panic("non-exhaustive match")
                    }
                    jp362 = jp364
                case Link:
                    var x63 Typ = mtmp60.(Link)._0
                    var inner__57 Typ = x63
                    var t374 Typ = TVar{
                        _0: r2__56,
                    }
                    var t375 Result__unit__string = unify(st__52, inner__57, t374)
                    jp362 = t375
                default:
                    panic("non-exhaustive match")
                }
                jp358 = jp362
            }
            jp356 = jp358
        case QVar:
            var r2__65 *ref_Tv_x = x52
            var other__64 Typ = x50
            var mtmp72 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r2__65)
            var jp377 Result__unit__string
            switch mtmp72.(type) {
            case Unbound:
                var mtmp76 Result__unit__string = occurs(st__52, r2__65, other__64)
                var jp379 Result__unit__string
                switch mtmp76.(type) {
                case Result__unit__string_Ok:
                    var t380 Tv = Link{
                        _0: other__64,
                    }
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(r2__65, t380)
                    var t381 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    jp379 = t381
                case Result__unit__string_Err:
                    var x78 string = mtmp76.(Result__unit__string_Err)._0
                    var e__67 string = x78
                    var t382 Result__unit__string = Result__unit__string_Err{
                        _0: e__67,
                    }
                    jp379 = t382
                default:
                    panic("non-exhaustive match")
                }
                jp377 = jp379
            case Link:
                var x75 Typ = mtmp72.(Link)._0
                var inner__66 Typ = x75
                var t383 Result__unit__string = unify(st__52, other__64, inner__66)
                jp377 = t383
            default:
                panic("non-exhaustive match")
            }
            jp356 = jp377
        case TArrow:
            var r2__65 *ref_Tv_x = x52
            var other__64 Typ = x50
            var mtmp80 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r2__65)
            var jp385 Result__unit__string
            switch mtmp80.(type) {
            case Unbound:
                var mtmp84 Result__unit__string = occurs(st__52, r2__65, other__64)
                var jp387 Result__unit__string
                switch mtmp84.(type) {
                case Result__unit__string_Ok:
                    var t388 Tv = Link{
                        _0: other__64,
                    }
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(r2__65, t388)
                    var t389 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    jp387 = t389
                case Result__unit__string_Err:
                    var x86 string = mtmp84.(Result__unit__string_Err)._0
                    var e__67 string = x86
                    var t390 Result__unit__string = Result__unit__string_Err{
                        _0: e__67,
                    }
                    jp387 = t390
                default:
                    panic("non-exhaustive match")
                }
                jp385 = jp387
            case Link:
                var x83 Typ = mtmp80.(Link)._0
                var inner__66 Typ = x83
                var t391 Result__unit__string = unify(st__52, other__64, inner__66)
                jp385 = t391
            default:
                panic("non-exhaustive match")
            }
            jp356 = jp385
        default:
            panic("non-exhaustive match")
        }
        jp354 = jp356
    case QVar:
        var jp393 Result__unit__string
        switch x50.(type) {
        case TVar:
            var x88 *ref_Tv_x = x50.(TVar)._0
            var r1__60 *ref_Tv_x = x88
            var other__61 Typ = x51
            var mtmp92 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r1__60)
            var jp395 Result__unit__string
            switch mtmp92.(type) {
            case Unbound:
                var mtmp96 Result__unit__string = occurs(st__52, r1__60, other__61)
                var jp397 Result__unit__string
                switch mtmp96.(type) {
                case Result__unit__string_Ok:
                    var t398 Tv = Link{
                        _0: other__61,
                    }
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(r1__60, t398)
                    var t399 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    jp397 = t399
                case Result__unit__string_Err:
                    var x98 string = mtmp96.(Result__unit__string_Err)._0
                    var e__63 string = x98
                    var t400 Result__unit__string = Result__unit__string_Err{
                        _0: e__63,
                    }
                    jp397 = t400
                default:
                    panic("non-exhaustive match")
                }
                jp395 = jp397
            case Link:
                var x95 Typ = mtmp92.(Link)._0
                var inner__62 Typ = x95
                var t401 Result__unit__string = unify(st__52, inner__62, other__61)
                jp395 = t401
            default:
                panic("non-exhaustive match")
            }
            jp393 = jp395
        case QVar:
            var t402 Result__unit__string = Result__unit__string_Err{
                _0: "unify error",
            }
            jp393 = t402
        case TArrow:
            var t403 Result__unit__string = Result__unit__string_Err{
                _0: "unify error",
            }
            jp393 = t403
        default:
            panic("non-exhaustive match")
        }
        jp354 = jp393
    case TArrow:
        var x54 Typ = x51.(TArrow)._0
        var x55 Typ = x51.(TArrow)._1
        var jp405 Result__unit__string
        switch x50.(type) {
        case TVar:
            var x100 *ref_Tv_x = x50.(TVar)._0
            var r1__60 *ref_Tv_x = x100
            var other__61 Typ = x51
            var mtmp104 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r1__60)
            var jp407 Result__unit__string
            switch mtmp104.(type) {
            case Unbound:
                var mtmp108 Result__unit__string = occurs(st__52, r1__60, other__61)
                var jp409 Result__unit__string
                switch mtmp108.(type) {
                case Result__unit__string_Ok:
                    var t410 Tv = Link{
                        _0: other__61,
                    }
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(r1__60, t410)
                    var t411 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    jp409 = t411
                case Result__unit__string_Err:
                    var x110 string = mtmp108.(Result__unit__string_Err)._0
                    var e__63 string = x110
                    var t412 Result__unit__string = Result__unit__string_Err{
                        _0: e__63,
                    }
                    jp409 = t412
                default:
                    panic("non-exhaustive match")
                }
                jp407 = jp409
            case Link:
                var x107 Typ = mtmp104.(Link)._0
                var inner__62 Typ = x107
                var t413 Result__unit__string = unify(st__52, inner__62, other__61)
                jp407 = t413
            default:
                panic("non-exhaustive match")
            }
            jp405 = jp407
        case QVar:
            var t414 Result__unit__string = Result__unit__string_Err{
                _0: "unify error",
            }
            jp405 = t414
        case TArrow:
            var x102 Typ = x50.(TArrow)._0
            var x103 Typ = x50.(TArrow)._1
            var a2__69 Typ = x103
            var a1__68 Typ = x102
            var b2__71 Typ = x55
            var b1__70 Typ = x54
            var mtmp112 Result__unit__string = unify(st__52, a1__68, b1__70)
            var jp416 Result__unit__string
            switch mtmp112.(type) {
            case Result__unit__string_Ok:
                var t417 Result__unit__string = unify(st__52, a2__69, b2__71)
                jp416 = t417
            case Result__unit__string_Err:
                var x114 string = mtmp112.(Result__unit__string_Err)._0
                var e__72 string = x114
                var t418 Result__unit__string = Result__unit__string_Err{
                    _0: e__72,
                }
                jp416 = t418
            default:
                panic("non-exhaustive match")
            }
            jp405 = jp416
        default:
            panic("non-exhaustive match")
        }
        jp354 = jp405
    default:
        panic("non-exhaustive match")
    }
    retv352 = jp354
    return retv352
}

func gen(st__73 CheckerState, ty__74 Typ) Typ {
    var retv420 Typ
    var jp422 Typ
    switch ty__74.(type) {
    case TVar:
        var x115 *ref_Tv_x = ty__74.(TVar)._0
        var tvref__75 *ref_Tv_x = x115
        var mtmp119 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvref__75)
        var jp424 Typ
        switch mtmp119.(type) {
        case Unbound:
            var x120 string = mtmp119.(Unbound)._0
            var x121 int32 = mtmp119.(Unbound)._1
            var l__77 int32 = x121
            var name__76 string = x120
            var t425 *ref_int32_x = st__73.current_level
            var cur__78 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t425)
            var t428 bool = l__77 > cur__78
            var jp427 Typ
            if t428 {
                var t429 Typ = QVar{
                    _0: name__76,
                }
                jp427 = t429
            } else {
                var t430 Typ = TVar{
                    _0: tvref__75,
                }
                jp427 = t430
            }
            jp424 = jp427
        case Link:
            var x122 Typ = mtmp119.(Link)._0
            var inner__79 Typ = x122
            var t431 Typ = gen(st__73, inner__79)
            jp424 = t431
        default:
            panic("non-exhaustive match")
        }
        jp422 = jp424
    case QVar:
        var other__82 Typ = ty__74
        jp422 = other__82
    case TArrow:
        var x117 Typ = ty__74.(TArrow)._0
        var x118 Typ = ty__74.(TArrow)._1
        var t2__81 Typ = x118
        var t1__80 Typ = x117
        var t432 Typ = gen(st__73, t1__80)
        var t433 Typ = gen(st__73, t2__81)
        var t434 Typ = TArrow{
            _0: t432,
            _1: t433,
        }
        jp422 = t434
    default:
        panic("non-exhaustive match")
    }
    retv420 = jp422
    return retv420
}

func inst_loop(st__83 CheckerState, subst__84 *_goml_vec_SubstEntry, ty__85 Typ) Tuple2_3Typ_16Vec_10SubstEntry {
    var retv436 Tuple2_3Typ_16Vec_10SubstEntry
    var jp438 Tuple2_3Typ_16Vec_10SubstEntry
    switch ty__85.(type) {
    case TVar:
        var x123 *ref_Tv_x = ty__85.(TVar)._0
        var tvref__90 *ref_Tv_x = x123
        var mtmp127 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvref__90)
        var jp440 Tuple2_3Typ_16Vec_10SubstEntry
        switch mtmp127.(type) {
        case Unbound:
            var t441 Typ = TVar{
                _0: tvref__90,
            }
            var t442 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: t441,
                _1: subst__84,
            }
            jp440 = t442
        case Link:
            var x130 Typ = mtmp127.(Link)._0
            var inner__91 Typ = x130
            var t443 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, subst__84, inner__91)
            jp440 = t443
        default:
            panic("non-exhaustive match")
        }
        jp438 = jp440
    case QVar:
        var x124 string = ty__85.(QVar)._0
        var name__86 string = x124
        var mtmp131 Option__Typ = subst_lookup(subst__84, name__86)
        var jp445 Tuple2_3Typ_16Vec_10SubstEntry
        switch mtmp131.(type) {
        case None:
            var tv__88 Typ = newvar(st__83)
            var t446 SubstEntry = SubstEntry{
                name: name__86,
                ty: tv__88,
            }
            var new_subst__89 *_goml_vec_SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SubstEntry(subst__84, t446)
            var t447 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: tv__88,
                _1: new_subst__89,
            }
            jp445 = t447
        case Some:
            var x132 Typ = mtmp131.(Some)._0
            var t__87 Typ = x132
            var t448 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: t__87,
                _1: subst__84,
            }
            jp445 = t448
        default:
            panic("non-exhaustive match")
        }
        jp438 = jp445
    case TArrow:
        var x125 Typ = ty__85.(TArrow)._0
        var x126 Typ = ty__85.(TArrow)._1
        var t2__93 Typ = x126
        var t1__92 Typ = x125
        var mtmp133 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, subst__84, t1__92)
        var x134 Typ = mtmp133._0
        var x135 *_goml_vec_SubstEntry = mtmp133._1
        var subst1__95 *_goml_vec_SubstEntry = x135
        var ty1__94 Typ = x134
        var mtmp136 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, subst1__95, t2__93)
        var x137 Typ = mtmp136._0
        var x138 *_goml_vec_SubstEntry = mtmp136._1
        var subst2__97 *_goml_vec_SubstEntry = x138
        var ty2__96 Typ = x137
        var t449 Typ = TArrow{
            _0: ty1__94,
            _1: ty2__96,
        }
        var t450 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
            _0: t449,
            _1: subst2__97,
        }
        jp438 = t450
    default:
        panic("non-exhaustive match")
    }
    retv436 = jp438
    return retv436
}

func inst(st__98 CheckerState, ty__99 Typ) Typ {
    var retv452 Typ
    var subst0__100 *_goml_vec_SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SubstEntry()
    var mtmp139 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__98, subst0__100, ty__99)
    var x140 Typ = mtmp139._0
    var t__101 Typ = x140
    retv452 = t__101
    return retv452
}

func typeof(st__102 CheckerState, env__103 *_goml_vec_EnvEntry, e__104 Exp) Result__Typ__string {
    var retv454 Result__Typ__string
    var jp456 Result__Typ__string
    switch e__104.(type) {
    case Var:
        var x142 string = e__104.(Var)._0
        var x__105 string = x142
        var mtmp150 Option__Typ = env_lookup(env__103, x__105)
        var jp458 Result__Typ__string
        switch mtmp150.(type) {
        case None:
            var t459 Result__Typ__string = Result__Typ__string_Err{
                _0: "unbound var",
            }
            jp458 = t459
        case Some:
            var x151 Typ = mtmp150.(Some)._0
            var ty__106 Typ = x151
            var t460 Typ = inst(st__102, ty__106)
            var t461 Result__Typ__string = Result__Typ__string_Ok{
                _0: t460,
            }
            jp458 = t461
        default:
            panic("non-exhaustive match")
        }
        jp456 = jp458
    case App:
        var x143 Exp = e__104.(App)._0
        var x144 Exp = e__104.(App)._1
        var e2__114 Exp = x144
        var e1__113 Exp = x143
        var mtmp152 Result__Typ__string = typeof(st__102, env__103, e1__113)
        var jp463 Result__Typ__string
        switch mtmp152.(type) {
        case Result__Typ__string_Ok:
            var x153 Typ = mtmp152.(Result__Typ__string_Ok)._0
            var ty_fun__116 Typ = x153
            var mtmp155 Result__Typ__string = typeof(st__102, env__103, e2__114)
            var jp465 Result__Typ__string
            switch mtmp155.(type) {
            case Result__Typ__string_Ok:
                var x156 Typ = mtmp155.(Result__Typ__string_Ok)._0
                var ty_arg__118 Typ = x156
                var ty_res__119 Typ = newvar(st__102)
                var arrow__120 Typ = TArrow{
                    _0: ty_arg__118,
                    _1: ty_res__119,
                }
                var mtmp158 Result__unit__string = unify(st__102, ty_fun__116, arrow__120)
                var jp467 Result__Typ__string
                switch mtmp158.(type) {
                case Result__unit__string_Ok:
                    var t468 Result__Typ__string = Result__Typ__string_Ok{
                        _0: ty_res__119,
                    }
                    jp467 = t468
                case Result__unit__string_Err:
                    var x160 string = mtmp158.(Result__unit__string_Err)._0
                    var e__121 string = x160
                    var t469 Result__Typ__string = Result__Typ__string_Err{
                        _0: e__121,
                    }
                    jp467 = t469
                default:
                    panic("non-exhaustive match")
                }
                jp465 = jp467
            case Result__Typ__string_Err:
                var x157 string = mtmp155.(Result__Typ__string_Err)._0
                var e__117 string = x157
                var t470 Result__Typ__string = Result__Typ__string_Err{
                    _0: e__117,
                }
                jp465 = t470
            default:
                panic("non-exhaustive match")
            }
            jp463 = jp465
        case Result__Typ__string_Err:
            var x154 string = mtmp152.(Result__Typ__string_Err)._0
            var e__115 string = x154
            var t471 Result__Typ__string = Result__Typ__string_Err{
                _0: e__115,
            }
            jp463 = t471
        default:
            panic("non-exhaustive match")
        }
        jp456 = jp463
    case Lam:
        var x145 string = e__104.(Lam)._0
        var x146 Exp = e__104.(Lam)._1
        var body__108 Exp = x146
        var x__107 string = x145
        var ty_x__109 Typ = newvar(st__102)
        var t472 EnvEntry = EnvEntry{
            name: x__107,
            ty: ty_x__109,
        }
        var env2__110 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__EnvEntry(env__103, t472)
        var mtmp161 Result__Typ__string = typeof(st__102, env2__110, body__108)
        var jp474 Result__Typ__string
        switch mtmp161.(type) {
        case Result__Typ__string_Ok:
            var x162 Typ = mtmp161.(Result__Typ__string_Ok)._0
            var ty_e__111 Typ = x162
            var t475 Typ = TArrow{
                _0: ty_x__109,
                _1: ty_e__111,
            }
            var t476 Result__Typ__string = Result__Typ__string_Ok{
                _0: t475,
            }
            jp474 = t476
        case Result__Typ__string_Err:
            var x163 string = mtmp161.(Result__Typ__string_Err)._0
            var e__112 string = x163
            var t477 Result__Typ__string = Result__Typ__string_Err{
                _0: e__112,
            }
            jp474 = t477
        default:
            panic("non-exhaustive match")
        }
        jp456 = jp474
    case Let:
        var x147 string = e__104.(Let)._0
        var x148 Exp = e__104.(Let)._1
        var x149 Exp = e__104.(Let)._2
        var e2__124 Exp = x149
        var e1__123 Exp = x148
        var x__122 string = x147
        enter_level(st__102)
        var ty_e__125 Result__Typ__string = typeof(st__102, env__103, e1__123)
        leave_level(st__102)
        var jp479 Result__Typ__string
        switch ty_e__125.(type) {
        case Result__Typ__string_Ok:
            var x166 Typ = ty_e__125.(Result__Typ__string_Ok)._0
            var ty1__127 Typ = x166
            var t480 Typ = gen(st__102, ty1__127)
            var t481 EnvEntry = EnvEntry{
                name: x__122,
                ty: t480,
            }
            var env2__128 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__EnvEntry(env__103, t481)
            var t482 Result__Typ__string = typeof(st__102, env2__128, e2__124)
            jp479 = t482
        case Result__Typ__string_Err:
            var x167 string = ty_e__125.(Result__Typ__string_Err)._0
            var e__126 string = x167
            var t483 Result__Typ__string = Result__Typ__string_Err{
                _0: e__126,
            }
            jp479 = t483
        default:
            panic("non-exhaustive match")
        }
        jp456 = jp479
    default:
        panic("non-exhaustive match")
    }
    retv454 = jp456
    return retv454
}

func exp_var(name__129 string) Exp {
    var retv485 Exp
    var t486 Exp = Var{
        _0: name__129,
    }
    retv485 = t486
    return retv485
}

func exp_lam(name__130 string, body__131 Exp) Exp {
    var retv488 Exp
    var t489 Exp = Lam{
        _0: name__130,
        _1: body__131,
    }
    retv488 = t489
    return retv488
}

func exp_app(a__132 Exp, b__133 Exp) Exp {
    var retv491 Exp
    var t492 Exp = App{
        _0: a__132,
        _1: b__133,
    }
    retv491 = t492
    return retv491
}

func exp_let(name__134 string, a__135 Exp, b__136 Exp) Exp {
    var retv494 Exp
    var t495 Exp = Let{
        _0: name__134,
        _1: a__135,
        _2: b__136,
    }
    retv494 = t495
    return retv494
}

func show_result(label__137 string, res__138 Result__Typ__string) struct{} {
    switch res__138.(type) {
    case Result__Typ__string_Ok:
        var x168 Typ = res__138.(Result__Typ__string_Ok)._0
        var ty__139 Typ = x168
        var t498 string = label__137 + ": "
        var t499 string = typ_to_string(ty__139)
        var t500 string = t498 + t499
        println__T_string(t500)
    case Result__Typ__string_Err:
        var x169 string = res__138.(Result__Typ__string_Err)._0
        var e__140 string = x169
        var t502 string = label__137 + ": "
        var t503 string = t502 + e__140
        println__T_string(t503)
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func main0() struct{} {
    var st__141 CheckerState = state_new()
    var t506 Exp = exp_var("x")
    var id__142 Exp = exp_lam("x", t506)
    var t507 Exp = exp_var("x")
    var t508 Exp = exp_var("y")
    var t509 Exp = exp_app(t507, t508)
    var t510 Exp = exp_lam("y", t509)
    var c1__143 Exp = exp_lam("x", t510)
    reset_type_variables(st__141)
    var t511 *_goml_vec_EnvEntry = env_empty()
    var t512 Result__Typ__string = typeof(st__141, t511, id__142)
    show_result("id", t512)
    reset_type_variables(st__141)
    var t513 *_goml_vec_EnvEntry = env_empty()
    var t514 Result__Typ__string = typeof(st__141, t513, c1__143)
    show_result("c1", t514)
    reset_type_variables(st__141)
    var t515 *_goml_vec_EnvEntry = env_empty()
    var t516 Exp = exp_var("x")
    var t517 Exp = exp_let("x", c1__143, t516)
    var t518 Result__Typ__string = typeof(st__141, t515, t517)
    show_result("let_x_c1_x", t518)
    reset_type_variables(st__141)
    var t519 *_goml_vec_EnvEntry = env_empty()
    var t520 Exp = exp_var("z")
    var t521 Exp = exp_lam("z", t520)
    var t522 Exp = exp_var("y")
    var t523 Exp = exp_let("y", t521, t522)
    var t524 Result__Typ__string = typeof(st__141, t519, t523)
    show_result("let_y_id_y", t524)
    reset_type_variables(st__141)
    var t525 *_goml_vec_EnvEntry = env_empty()
    var t526 Exp = exp_var("z")
    var t527 Exp = exp_lam("z", t526)
    var t528 Exp = exp_var("y")
    var t529 Exp = exp_let("y", t527, t528)
    var t530 Exp = exp_lam("x", t529)
    var t531 Result__Typ__string = typeof(st__141, t525, t530)
    show_result("lam_x_let_y_id_y", t531)
    reset_type_variables(st__141)
    var t532 *_goml_vec_EnvEntry = env_empty()
    var t533 Exp = exp_var("z")
    var t534 Exp = exp_lam("z", t533)
    var t535 Exp = exp_var("y")
    var t536 Exp = exp_var("x")
    var t537 Exp = exp_app(t535, t536)
    var t538 Exp = exp_let("y", t534, t537)
    var t539 Exp = exp_lam("x", t538)
    var t540 Result__Typ__string = typeof(st__141, t532, t539)
    show_result("lam_x_let_y_id_yx", t540)
    reset_type_variables(st__141)
    var t541 *_goml_vec_EnvEntry = env_empty()
    var t542 Exp = exp_var("x")
    var t543 Exp = exp_var("x")
    var t544 Exp = exp_app(t542, t543)
    var t545 Exp = exp_lam("x", t544)
    var t546 Result__Typ__string = typeof(st__141, t541, t545)
    show_result("self_apply", t546)
    reset_type_variables(st__141)
    var t547 *_goml_vec_EnvEntry = env_empty()
    var t548 Exp = exp_var("x")
    var t549 Exp = exp_var("x")
    var t550 Exp = exp_let("x", t548, t549)
    var t551 Result__Typ__string = typeof(st__141, t547, t550)
    show_result("unbound_var", t551)
    reset_type_variables(st__141)
    var t552 *_goml_vec_EnvEntry = env_empty()
    var t553 Exp = exp_var("y")
    var t554 Exp = exp_var("y")
    var t555 Exp = exp_var("z")
    var t556 Exp = exp_app(t554, t555)
    var t557 Exp = exp_lam("z", t556)
    var t558 Exp = exp_app(t553, t557)
    var t559 Exp = exp_lam("y", t558)
    var t560 Result__Typ__string = typeof(st__141, t552, t559)
    show_result("max_heiber", t560)
    reset_type_variables(st__141)
    var t561 *_goml_vec_EnvEntry = env_empty()
    var t562 Exp = exp_var("k")
    var t563 Exp = exp_var("k")
    var t564 Exp = exp_var("x")
    var t565 Exp = exp_app(t563, t564)
    var t566 Exp = exp_var("y")
    var t567 Exp = exp_app(t565, t566)
    var t568 Exp = exp_app(t562, t567)
    var t569 Exp = exp_var("k")
    var t570 Exp = exp_var("y")
    var t571 Exp = exp_app(t569, t570)
    var t572 Exp = exp_var("x")
    var t573 Exp = exp_app(t571, t572)
    var t574 Exp = exp_app(t568, t573)
    var t575 Exp = exp_lam("k", t574)
    var t576 Exp = exp_lam("y", t575)
    var t577 Exp = exp_lam("x", t576)
    var t578 Result__Typ__string = typeof(st__141, t561, t577)
    show_result("kirang", t578)
    reset_type_variables(st__141)
    var t579 *_goml_vec_EnvEntry = env_empty()
    var t580 Exp = exp_var("id")
    var t581 Exp = exp_var("id")
    var t582 Exp = exp_app(t580, t581)
    var t583 Exp = exp_let("id", id__142, t582)
    var t584 Result__Typ__string = typeof(st__141, t579, t583)
    show_result("let_id_idid", t584)
    reset_type_variables(st__141)
    var t585 *_goml_vec_EnvEntry = env_empty()
    var t586 Exp = exp_var("x")
    var t587 Exp = exp_app(t586, id__142)
    var t588 Exp = exp_var("z")
    var t589 Exp = exp_let("z", t587, t588)
    var t590 Exp = exp_var("y")
    var t591 Exp = exp_let("y", t589, t590)
    var t592 Exp = exp_let("x", c1__143, t591)
    var t593 Result__Typ__string = typeof(st__141, t585, t592)
    show_result("nested_lets", t593)
    reset_type_variables(st__141)
    var t594 *_goml_vec_EnvEntry = env_empty()
    var t595 Exp = exp_var("x")
    var t596 Exp = exp_var("y")
    var t597 Exp = exp_app(t595, t596)
    var t598 Exp = exp_var("y")
    var t599 Exp = exp_var("x")
    var t600 Exp = exp_app(t598, t599)
    var t601 Exp = exp_lam("x", t600)
    var t602 Exp = exp_let("x", t597, t601)
    var t603 Exp = exp_lam("y", t602)
    var t604 Exp = exp_lam("x", t603)
    var t605 Result__Typ__string = typeof(st__141, t594, t604)
    show_result("fun_x_fun_y_let_x_xy_fun_x_yx", t605)
    reset_type_variables(st__141)
    var t606 *_goml_vec_EnvEntry = env_empty()
    var t607 Exp = exp_var("x")
    var t608 Exp = exp_var("y")
    var t609 Exp = exp_let("y", t607, t608)
    var t610 Exp = exp_lam("x", t609)
    var t611 Result__Typ__string = typeof(st__141, t606, t610)
    show_result("sound_gen_1", t611)
    reset_type_variables(st__141)
    var t612 *_goml_vec_EnvEntry = env_empty()
    var t613 Exp = exp_var("x")
    var t614 Exp = exp_lam("z", t613)
    var t615 Exp = exp_var("y")
    var t616 Exp = exp_let("y", t614, t615)
    var t617 Exp = exp_lam("x", t616)
    var t618 Result__Typ__string = typeof(st__141, t612, t617)
    show_result("sound_gen_2", t618)
    reset_type_variables(st__141)
    var t619 *_goml_vec_EnvEntry = env_empty()
    var t620 Exp = exp_var("x")
    var t621 Exp = exp_var("z")
    var t622 Exp = exp_app(t620, t621)
    var t623 Exp = exp_lam("z", t622)
    var t624 Exp = exp_var("y")
    var t625 Exp = exp_let("y", t623, t624)
    var t626 Exp = exp_lam("x", t625)
    var t627 Result__Typ__string = typeof(st__141, t619, t626)
    show_result("sound_gen_3", t627)
    reset_type_variables(st__141)
    var t628 *_goml_vec_EnvEntry = env_empty()
    var t629 Exp = exp_var("x")
    var t630 Exp = exp_var("y")
    var t631 Exp = exp_app(t629, t630)
    var t632 Exp = exp_var("x")
    var t633 Exp = exp_var("y")
    var t634 Exp = exp_app(t632, t633)
    var t635 Exp = exp_let("x", t631, t634)
    var t636 Exp = exp_lam("y", t635)
    var t637 Exp = exp_lam("x", t636)
    var t638 Result__Typ__string = typeof(st__141, t628, t637)
    show_result("double_apply", t638)
    reset_type_variables(st__141)
    var t639 *_goml_vec_EnvEntry = env_empty()
    var t640 Exp = exp_var("x")
    var t641 Exp = exp_var("y")
    var t642 Exp = exp_var("y")
    var t643 Exp = exp_app(t641, t642)
    var t644 Exp = exp_let("y", t640, t643)
    var t645 Exp = exp_lam("x", t644)
    var t646 Result__Typ__string = typeof(st__141, t639, t645)
    show_result("sound_gen_occurs", t646)
    reset_gensym(st__141)
    var t647 *_goml_vec_EnvEntry = env_empty()
    var t648 Exp = exp_var("x")
    var t649 Exp = exp_app(t648, id__142)
    var t650 Exp = exp_var("z")
    var t651 Exp = exp_let("z", t649, t650)
    var t652 Exp = exp_var("y")
    var t653 Exp = exp_let("y", t651, t652)
    var t654 Exp = exp_lam("x", t653)
    var t655 Result__Typ__string = typeof(st__141, t647, t654)
    show_result("fun_x_let_y_let_z_x_id_z_y", t655)
    println__T_string("")
    println__T_string("All Done")
    println__T_string("")
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__114 int32) *ref_int32_x {
    var retv657 *ref_int32_x
    var t658 *ref_int32_x = ref__Ref_5int32(value__114)
    retv657 = t658
    return retv657
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__116 *ref_int32_x, value__117 int32) struct{} {
    ref_set__Ref_5int32(self__116, value__117)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__115 *ref_int32_x) int32 {
    var retv662 int32
    var t663 int32 = ref_get__Ref_5int32(self__115)
    retv662 = t663
    return retv662
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__3 rune) string {
    var retv665 string
    var t666 string = _goml_runtime_core_char_to_string(self__3)
    retv665 = t666
    return retv665
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv668 string
    var t669 string = _goml_runtime_core_int32_to_string(self__2)
    retv668 = t669
    return retv668
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Tv(value__114 Tv) *ref_Tv_x {
    var retv671 *ref_Tv_x
    var t672 *ref_Tv_x = ref__Ref_2Tv(value__114)
    retv671 = t672
    return retv671
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(self__115 *ref_Tv_x) Tv {
    var retv674 Tv
    var t675 Tv = ref_get__Ref_2Tv(self__115)
    retv674 = t675
    return retv674
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__EnvEntry() *_goml_vec_EnvEntry {
    var retv677 *_goml_vec_EnvEntry
    var t678 *_goml_vec_EnvEntry = vec_new__Vec_8EnvEntry()
    retv677 = t678
    return retv677
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__EnvEntry(self__84 *_goml_vec_EnvEntry) int32 {
    var retv680 int32
    var t681 int32 = vec_len__Vec_8EnvEntry(self__84)
    retv680 = t681
    return retv680
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_Typ_r_(value__114 Option__Typ) *ref_Option__Typ_x {
    var retv683 *ref_Option__Typ_x
    var t684 *ref_Option__Typ_x = ref__Ref_11Option__Typ(value__114)
    retv683 = t684
    return retv683
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__114 bool) *ref_bool_x {
    var retv686 *ref_bool_x
    var t687 *ref_bool_x = ref__Ref_4bool(value__114)
    retv686 = t687
    return retv686
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__115 *ref_bool_x) bool {
    var retv689 bool
    var t690 bool = ref_get__Ref_4bool(self__115)
    retv689 = t690
    return retv689
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Option_l_Typ_r_(self__116 *ref_Option__Typ_x, value__117 Option__Typ) struct{} {
    ref_set__Ref_11Option__Typ(self__116, value__117)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(self__116 *ref_bool_x, value__117 bool) struct{} {
    ref_set__Ref_4bool(self__116, value__117)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Option_l_Typ_r_(self__115 *ref_Option__Typ_x) Option__Typ {
    var retv696 Option__Typ
    var t697 Option__Typ = ref_get__Ref_11Option__Typ(self__115)
    retv696 = t697
    return retv696
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SubstEntry(self__84 *_goml_vec_SubstEntry) int32 {
    var retv699 int32
    var t700 int32 = vec_len__Vec_10SubstEntry(self__84)
    retv699 = t700
    return retv699
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(self__116 *ref_Tv_x, value__117 Tv) struct{} {
    ref_set__Ref_2Tv(self__116, value__117)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SubstEntry(self__75 *_goml_vec_SubstEntry, elem__76 SubstEntry) *_goml_vec_SubstEntry {
    var retv704 *_goml_vec_SubstEntry
    var result__77 *_goml_vec_SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SubstEntry()
    var index__78 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop706:
    for {
        var t707 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__78)
        var t708 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SubstEntry(self__75)
        var t709 bool = t707 < t708
        if t709 {
            var t710 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__78)
            var t711 SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__SubstEntry(self__75, t710)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SubstEntry(result__77, t711)
            var t712 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__78)
            var t713 int32 = t712 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__78, t713)
            continue
        } else {
            break Loop_loop706
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SubstEntry(result__77, elem__76)
    retv704 = result__77
    return retv704
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SubstEntry() *_goml_vec_SubstEntry {
    var retv715 *_goml_vec_SubstEntry
    var t716 *_goml_vec_SubstEntry = vec_new__Vec_10SubstEntry()
    retv715 = t716
    return retv715
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__EnvEntry(self__75 *_goml_vec_EnvEntry, elem__76 EnvEntry) *_goml_vec_EnvEntry {
    var retv718 *_goml_vec_EnvEntry
    var result__77 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__EnvEntry()
    var index__78 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop720:
    for {
        var t721 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__78)
        var t722 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__EnvEntry(self__75)
        var t723 bool = t721 < t722
        if t723 {
            var t724 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__78)
            var t725 EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__EnvEntry(self__75, t724)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__EnvEntry(result__77, t725)
            var t726 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__78)
            var t727 int32 = t726 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__78, t727)
            continue
        } else {
            break Loop_loop720
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__EnvEntry(result__77, elem__76)
    retv718 = result__77
    return retv718
}

func println__T_string(value__1 string) struct{} {
    var t729 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t729)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SubstEntry(self__73 *_goml_vec_SubstEntry, elem__74 SubstEntry) struct{} {
    vec_push__Vec_10SubstEntry(self__73, elem__74)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__SubstEntry(self__79 *_goml_vec_SubstEntry, index__80 int32) SubstEntry {
    var retv734 SubstEntry
    var t735 SubstEntry = vec_get__Vec_10SubstEntry(self__79, index__80)
    retv734 = t735
    return retv734
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__EnvEntry(self__73 *_goml_vec_EnvEntry, elem__74 EnvEntry) struct{} {
    vec_push__Vec_8EnvEntry(self__73, elem__74)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__EnvEntry(self__79 *_goml_vec_EnvEntry, index__80 int32) EnvEntry {
    var retv739 EnvEntry
    var t740 EnvEntry = vec_get__Vec_8EnvEntry(self__79, index__80)
    retv739 = t740
    return retv739
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv742 string
    retv742 = self__9
    return retv742
}

func main() {
    main0()
}
