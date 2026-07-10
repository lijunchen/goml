package main

import (
    _goml_fmt "fmt"
    _goml_utf8 "unicode/utf8"
)

func char_to_string(x rune) string {
    if !_goml_utf8.ValidRune(x) {
        panic("invalid char")
    }
    return string(x)
}

func int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
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
    _1 []SubstEntry
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
    var retv205 CheckerState
    var t206 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var t207 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(1)
    var t208 CheckerState = CheckerState{
        gensym_counter: t206,
        current_level: t207,
    }
    retv205 = t208
    return retv205
}

func reset_gensym(st__0 CheckerState) struct{} {
    var t210 *ref_int32_x = st__0.gensym_counter
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t210, 0)
    return struct{}{}
}

func reset_level(st__1 CheckerState) struct{} {
    var t212 *ref_int32_x = st__1.current_level
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t212, 1)
    return struct{}{}
}

func reset_type_variables(st__2 CheckerState) struct{} {
    reset_gensym(st__2)
    reset_level(st__2)
    return struct{}{}
}

func enter_level(st__3 CheckerState) struct{} {
    var t216 *ref_int32_x = st__3.current_level
    var l__4 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t216)
    var t217 *ref_int32_x = st__3.current_level
    var t218 int32 = l__4 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t217, t218)
    return struct{}{}
}

func leave_level(st__5 CheckerState) struct{} {
    var t220 *ref_int32_x = st__5.current_level
    var l__6 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t220)
    var t221 *ref_int32_x = st__5.current_level
    var t222 int32 = l__6 - 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t221, t222)
    return struct{}{}
}

func min_i32(a__7 int32, b__8 int32) int32 {
    var retv224 int32
    var t227 bool = a__7 < b__8
    var jp226 int32
    if t227 {
        jp226 = a__7
    } else {
        jp226 = b__8
    }
    retv224 = jp226
    return retv224
}

func nth_letter(n__9 int32) rune {
    var retv229 rune
    var jp231 rune
    switch n__9 {
    case 0:
        jp231 = 97
    case 1:
        jp231 = 98
    case 2:
        jp231 = 99
    case 3:
        jp231 = 100
    case 4:
        jp231 = 101
    case 5:
        jp231 = 102
    case 6:
        jp231 = 103
    case 7:
        jp231 = 104
    case 8:
        jp231 = 105
    case 9:
        jp231 = 106
    case 10:
        jp231 = 107
    case 11:
        jp231 = 108
    case 12:
        jp231 = 109
    case 13:
        jp231 = 110
    case 14:
        jp231 = 111
    case 15:
        jp231 = 112
    case 16:
        jp231 = 113
    case 17:
        jp231 = 114
    case 18:
        jp231 = 115
    case 19:
        jp231 = 116
    case 20:
        jp231 = 117
    case 21:
        jp231 = 118
    case 22:
        jp231 = 119
    case 23:
        jp231 = 120
    case 24:
        jp231 = 121
    case 25:
        jp231 = 122
    default:
        jp231 = 97
    }
    retv229 = jp231
    return retv229
}

func gensym(st__10 CheckerState) string {
    var retv233 string
    var t234 *ref_int32_x = st__10.gensym_counter
    var n__11 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t234)
    var t235 *ref_int32_x = st__10.gensym_counter
    var t236 int32 = n__11 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t235, t236)
    var t239 bool = n__11 < 26
    var jp238 string
    if t239 {
        var t240 rune = nth_letter(n__11)
        var t241 string = char_to_string(t240)
        jp238 = t241
    } else {
        var t242 string = int32_to_string(n__11)
        var t243 string = "t" + t242
        jp238 = t243
    }
    retv233 = jp238
    return retv233
}

func newvar(st__12 CheckerState) Typ {
    var retv245 Typ
    var name__13 string = gensym(st__12)
    var t246 *ref_int32_x = st__12.current_level
    var level__14 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t246)
    var t247 Tv = Unbound{
        _0: name__13,
        _1: level__14,
    }
    var t248 *ref_Tv_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Tv(t247)
    var t249 Typ = TVar{
        _0: t248,
    }
    retv245 = t249
    return retv245
}

func typ_is_arrow(ty__15 Typ) bool {
    var retv251 bool
    var jp253 bool
    switch ty__15.(type) {
    case TVar:
        var x6 *ref_Tv_x = ty__15.(TVar)._0
        var tvref__16 *ref_Tv_x = x6
        var mtmp10 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvref__16)
        var jp255 bool
        switch mtmp10.(type) {
        case Unbound:
            jp255 = false
        case Link:
            var x13 Typ = mtmp10.(Link)._0
            var inner__17 Typ = x13
            var t256 bool = typ_is_arrow(inner__17)
            jp255 = t256
        default:
            panic("non-exhaustive match")
        }
        jp253 = jp255
    case QVar:
        jp253 = false
    case TArrow:
        jp253 = true
    default:
        panic("non-exhaustive match")
    }
    retv251 = jp253
    return retv251
}

func typ_to_string(ty__18 Typ) string {
    var retv258 string
    var jp260 string
    switch ty__18.(type) {
    case TVar:
        var x14 *ref_Tv_x = ty__18.(TVar)._0
        var tvref__20 *ref_Tv_x = x14
        var mtmp18 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvref__20)
        var jp262 string
        switch mtmp18.(type) {
        case Unbound:
            var x19 string = mtmp18.(Unbound)._0
            var name__21 string = x19
            var t263 string = "'" + name__21
            jp262 = t263
        case Link:
            var x21 Typ = mtmp18.(Link)._0
            var inner__22 Typ = x21
            var t264 string = typ_to_string(inner__22)
            jp262 = t264
        default:
            panic("non-exhaustive match")
        }
        jp260 = jp262
    case QVar:
        var x15 string = ty__18.(QVar)._0
        var name__19 string = x15
        var t265 string = "'" + name__19
        jp260 = t265
    case TArrow:
        var x16 Typ = ty__18.(TArrow)._0
        var x17 Typ = ty__18.(TArrow)._1
        var t2__24 Typ = x17
        var t1__23 Typ = x16
        var t270 bool = typ_is_arrow(t1__23)
        var jp267 string
        if t270 {
            var t271 string = typ_to_string(t1__23)
            var t272 string = "(" + t271
            var t273 string = t272 + ")"
            jp267 = t273
        } else {
            var t274 string = typ_to_string(t1__23)
            jp267 = t274
        }
        var s1__25 string = jp267
        var s2__26 string = typ_to_string(t2__24)
        var t268 string = s1__25 + " -> "
        var t269 string = t268 + s2__26
        jp260 = t269
    default:
        panic("non-exhaustive match")
    }
    retv258 = jp260
    return retv258
}

func env_empty() []EnvEntry {
    var retv276 []EnvEntry
    var env__27 []EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__EnvEntry()
    retv276 = env__27
    return retv276
}

func env_lookup(env__28 []EnvEntry, name__29 string) Option__Typ {
    var retv278 Option__Typ
    var t279 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__EnvEntry(env__28)
    var t280 int32 = t279 - 1
    var i__30 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(t280)
    var found__31 *ref_Option__Typ_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_Typ_r_(None{})
    var done__32 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    Loop_loop283:
    for {
        var t296 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(done__32)
        var t297 bool = !t296
        var jp285 bool
        if t297 {
            var t298 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__30)
            var t299 bool = t298 >= 0
            jp285 = t299
        } else {
            jp285 = false
        }
        if jp285 {
            var t286 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__30)
            var entry__33 EnvEntry = env__28[t286]
            var t288 string = entry__33.name
            var t289 bool = t288 == name__29
            if t289 {
                var t290 Typ = entry__33.ty
                var t291 Option__Typ = Some{
                    _0: t290,
                }
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Option_l_Typ_r_(found__31, t291)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(done__32, true)
            } else {
                var t293 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__30)
                var t294 int32 = t293 - 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__30, t294)
            }
            continue
        } else {
            break Loop_loop283
        }
    }
    var t282 Option__Typ = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Option_l_Typ_r_(found__31)
    retv278 = t282
    return retv278
}

func subst_lookup(subst__34 []SubstEntry, name__35 string) Option__Typ {
    var retv301 Option__Typ
    var t302 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SubstEntry(subst__34)
    var t303 int32 = t302 - 1
    var i__36 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(t303)
    var found__37 *ref_Option__Typ_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_Typ_r_(None{})
    var done__38 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    Loop_loop306:
    for {
        var t319 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(done__38)
        var t320 bool = !t319
        var jp308 bool
        if t320 {
            var t321 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__36)
            var t322 bool = t321 >= 0
            jp308 = t322
        } else {
            jp308 = false
        }
        if jp308 {
            var t309 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__36)
            var entry__39 SubstEntry = subst__34[t309]
            var t311 string = entry__39.name
            var t312 bool = t311 == name__35
            if t312 {
                var t313 Typ = entry__39.ty
                var t314 Option__Typ = Some{
                    _0: t313,
                }
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Option_l_Typ_r_(found__37, t314)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(done__38, true)
            } else {
                var t316 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__36)
                var t317 int32 = t316 - 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__36, t317)
            }
            continue
        } else {
            break Loop_loop306
        }
    }
    var t305 Option__Typ = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Option_l_Typ_r_(found__37)
    retv301 = t305
    return retv301
}

func occurs(st__40 CheckerState, tvr__41 *ref_Tv_x, ty__42 Typ) Result__unit__string {
    var retv324 Result__unit__string
    var jp326 Result__unit__string
    switch ty__42.(type) {
    case TVar:
        var x26 *ref_Tv_x = ty__42.(TVar)._0
        var tvr2__43 *ref_Tv_x = x26
        var t329 bool = ptr_eq__Ref_2Tv(tvr__41, tvr2__43)
        var jp328 Result__unit__string
        if t329 {
            var t330 Result__unit__string = Result__unit__string_Err{
                _0: "occurs check",
            }
            jp328 = t330
        } else {
            var mtmp30 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvr2__43)
            var jp332 Result__unit__string
            switch mtmp30.(type) {
            case Unbound:
                var x31 string = mtmp30.(Unbound)._0
                var x32 int32 = mtmp30.(Unbound)._1
                var l2__45 int32 = x32
                var name__44 string = x31
                var mtmp34 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvr__41)
                var jp334 int32
                switch mtmp34.(type) {
                case Unbound:
                    var x36 int32 = mtmp34.(Unbound)._1
                    var l__46 int32 = x36
                    var t337 int32 = min_i32(l__46, l2__45)
                    jp334 = t337
                case Link:
                    jp334 = l2__45
                default:
                    panic("non-exhaustive match")
                }
                var min_level__47 int32 = jp334
                var t335 Tv = Unbound{
                    _0: name__44,
                    _1: min_level__47,
                }
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(tvr2__43, t335)
                var t336 Result__unit__string = Result__unit__string_Ok{
                    _0: struct{}{},
                }
                jp332 = t336
            case Link:
                var x33 Typ = mtmp30.(Link)._0
                var inner__48 Typ = x33
                var t338 Result__unit__string = occurs(st__40, tvr__41, inner__48)
                jp332 = t338
            default:
                panic("non-exhaustive match")
            }
            jp328 = jp332
        }
        jp326 = jp328
    case QVar:
        var t339 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp326 = t339
    case TArrow:
        var x28 Typ = ty__42.(TArrow)._0
        var x29 Typ = ty__42.(TArrow)._1
        var t2__50 Typ = x29
        var t1__49 Typ = x28
        var mtmp39 Result__unit__string = occurs(st__40, tvr__41, t1__49)
        var jp341 Result__unit__string
        switch mtmp39.(type) {
        case Result__unit__string_Ok:
            var t342 Result__unit__string = occurs(st__40, tvr__41, t2__50)
            jp341 = t342
        case Result__unit__string_Err:
            var x41 string = mtmp39.(Result__unit__string_Err)._0
            var e__51 string = x41
            var t343 Result__unit__string = Result__unit__string_Err{
                _0: e__51,
            }
            jp341 = t343
        default:
            panic("non-exhaustive match")
        }
        jp326 = jp341
    default:
        panic("non-exhaustive match")
    }
    retv324 = jp326
    return retv324
}

func unify(st__52 CheckerState, t1__53 Typ, t2__54 Typ) Result__unit__string {
    var retv345 Result__unit__string
    var mtmp42 Tuple2_3Typ_3Typ = Tuple2_3Typ_3Typ{
        _0: t1__53,
        _1: t2__54,
    }
    var x43 Typ = mtmp42._0
    var x44 Typ = mtmp42._1
    var jp347 Result__unit__string
    switch x44.(type) {
    case TVar:
        var x45 *ref_Tv_x = x44.(TVar)._0
        var jp349 Result__unit__string
        switch x43.(type) {
        case TVar:
            var x49 *ref_Tv_x = x43.(TVar)._0
            var r1__55 *ref_Tv_x = x49
            var r2__56 *ref_Tv_x = x45
            var t352 bool = ptr_eq__Ref_2Tv(r1__55, r2__56)
            var jp351 Result__unit__string
            if t352 {
                var t353 Result__unit__string = Result__unit__string_Ok{
                    _0: struct{}{},
                }
                jp351 = t353
            } else {
                var mtmp53 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r1__55)
                var jp355 Result__unit__string
                switch mtmp53.(type) {
                case Unbound:
                    var mtmp57 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r2__56)
                    var jp357 Result__unit__string
                    switch mtmp57.(type) {
                    case Unbound:
                        var t358 Typ = TVar{
                            _0: r2__56,
                        }
                        var mtmp61 Result__unit__string = occurs(st__52, r1__55, t358)
                        var jp360 Result__unit__string
                        switch mtmp61.(type) {
                        case Result__unit__string_Ok:
                            var t361 Typ = TVar{
                                _0: r2__56,
                            }
                            var t362 Tv = Link{
                                _0: t361,
                            }
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(r1__55, t362)
                            var t363 Result__unit__string = Result__unit__string_Ok{
                                _0: struct{}{},
                            }
                            jp360 = t363
                        case Result__unit__string_Err:
                            var x63 string = mtmp61.(Result__unit__string_Err)._0
                            var e__59 string = x63
                            var t364 Result__unit__string = Result__unit__string_Err{
                                _0: e__59,
                            }
                            jp360 = t364
                        default:
                            panic("non-exhaustive match")
                        }
                        jp357 = jp360
                    case Link:
                        var x60 Typ = mtmp57.(Link)._0
                        var inner__58 Typ = x60
                        var t365 Typ = TVar{
                            _0: r1__55,
                        }
                        var t366 Result__unit__string = unify(st__52, t365, inner__58)
                        jp357 = t366
                    default:
                        panic("non-exhaustive match")
                    }
                    jp355 = jp357
                case Link:
                    var x56 Typ = mtmp53.(Link)._0
                    var inner__57 Typ = x56
                    var t367 Typ = TVar{
                        _0: r2__56,
                    }
                    var t368 Result__unit__string = unify(st__52, inner__57, t367)
                    jp355 = t368
                default:
                    panic("non-exhaustive match")
                }
                jp351 = jp355
            }
            jp349 = jp351
        case QVar:
            var r2__65 *ref_Tv_x = x45
            var other__64 Typ = x43
            var mtmp65 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r2__65)
            var jp370 Result__unit__string
            switch mtmp65.(type) {
            case Unbound:
                var mtmp69 Result__unit__string = occurs(st__52, r2__65, other__64)
                var jp372 Result__unit__string
                switch mtmp69.(type) {
                case Result__unit__string_Ok:
                    var t373 Tv = Link{
                        _0: other__64,
                    }
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(r2__65, t373)
                    var t374 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    jp372 = t374
                case Result__unit__string_Err:
                    var x71 string = mtmp69.(Result__unit__string_Err)._0
                    var e__67 string = x71
                    var t375 Result__unit__string = Result__unit__string_Err{
                        _0: e__67,
                    }
                    jp372 = t375
                default:
                    panic("non-exhaustive match")
                }
                jp370 = jp372
            case Link:
                var x68 Typ = mtmp65.(Link)._0
                var inner__66 Typ = x68
                var t376 Result__unit__string = unify(st__52, other__64, inner__66)
                jp370 = t376
            default:
                panic("non-exhaustive match")
            }
            jp349 = jp370
        case TArrow:
            var r2__65 *ref_Tv_x = x45
            var other__64 Typ = x43
            var mtmp73 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r2__65)
            var jp378 Result__unit__string
            switch mtmp73.(type) {
            case Unbound:
                var mtmp77 Result__unit__string = occurs(st__52, r2__65, other__64)
                var jp380 Result__unit__string
                switch mtmp77.(type) {
                case Result__unit__string_Ok:
                    var t381 Tv = Link{
                        _0: other__64,
                    }
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(r2__65, t381)
                    var t382 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    jp380 = t382
                case Result__unit__string_Err:
                    var x79 string = mtmp77.(Result__unit__string_Err)._0
                    var e__67 string = x79
                    var t383 Result__unit__string = Result__unit__string_Err{
                        _0: e__67,
                    }
                    jp380 = t383
                default:
                    panic("non-exhaustive match")
                }
                jp378 = jp380
            case Link:
                var x76 Typ = mtmp73.(Link)._0
                var inner__66 Typ = x76
                var t384 Result__unit__string = unify(st__52, other__64, inner__66)
                jp378 = t384
            default:
                panic("non-exhaustive match")
            }
            jp349 = jp378
        default:
            panic("non-exhaustive match")
        }
        jp347 = jp349
    case QVar:
        var jp386 Result__unit__string
        switch x43.(type) {
        case TVar:
            var x81 *ref_Tv_x = x43.(TVar)._0
            var r1__60 *ref_Tv_x = x81
            var other__61 Typ = x44
            var mtmp85 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r1__60)
            var jp388 Result__unit__string
            switch mtmp85.(type) {
            case Unbound:
                var mtmp89 Result__unit__string = occurs(st__52, r1__60, other__61)
                var jp390 Result__unit__string
                switch mtmp89.(type) {
                case Result__unit__string_Ok:
                    var t391 Tv = Link{
                        _0: other__61,
                    }
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(r1__60, t391)
                    var t392 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    jp390 = t392
                case Result__unit__string_Err:
                    var x91 string = mtmp89.(Result__unit__string_Err)._0
                    var e__63 string = x91
                    var t393 Result__unit__string = Result__unit__string_Err{
                        _0: e__63,
                    }
                    jp390 = t393
                default:
                    panic("non-exhaustive match")
                }
                jp388 = jp390
            case Link:
                var x88 Typ = mtmp85.(Link)._0
                var inner__62 Typ = x88
                var t394 Result__unit__string = unify(st__52, inner__62, other__61)
                jp388 = t394
            default:
                panic("non-exhaustive match")
            }
            jp386 = jp388
        case QVar:
            var t395 Result__unit__string = Result__unit__string_Err{
                _0: "unify error",
            }
            jp386 = t395
        case TArrow:
            var t396 Result__unit__string = Result__unit__string_Err{
                _0: "unify error",
            }
            jp386 = t396
        default:
            panic("non-exhaustive match")
        }
        jp347 = jp386
    case TArrow:
        var x47 Typ = x44.(TArrow)._0
        var x48 Typ = x44.(TArrow)._1
        var jp398 Result__unit__string
        switch x43.(type) {
        case TVar:
            var x93 *ref_Tv_x = x43.(TVar)._0
            var r1__60 *ref_Tv_x = x93
            var other__61 Typ = x44
            var mtmp97 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(r1__60)
            var jp400 Result__unit__string
            switch mtmp97.(type) {
            case Unbound:
                var mtmp101 Result__unit__string = occurs(st__52, r1__60, other__61)
                var jp402 Result__unit__string
                switch mtmp101.(type) {
                case Result__unit__string_Ok:
                    var t403 Tv = Link{
                        _0: other__61,
                    }
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(r1__60, t403)
                    var t404 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    jp402 = t404
                case Result__unit__string_Err:
                    var x103 string = mtmp101.(Result__unit__string_Err)._0
                    var e__63 string = x103
                    var t405 Result__unit__string = Result__unit__string_Err{
                        _0: e__63,
                    }
                    jp402 = t405
                default:
                    panic("non-exhaustive match")
                }
                jp400 = jp402
            case Link:
                var x100 Typ = mtmp97.(Link)._0
                var inner__62 Typ = x100
                var t406 Result__unit__string = unify(st__52, inner__62, other__61)
                jp400 = t406
            default:
                panic("non-exhaustive match")
            }
            jp398 = jp400
        case QVar:
            var t407 Result__unit__string = Result__unit__string_Err{
                _0: "unify error",
            }
            jp398 = t407
        case TArrow:
            var x95 Typ = x43.(TArrow)._0
            var x96 Typ = x43.(TArrow)._1
            var a2__69 Typ = x96
            var a1__68 Typ = x95
            var b2__71 Typ = x48
            var b1__70 Typ = x47
            var mtmp105 Result__unit__string = unify(st__52, a1__68, b1__70)
            var jp409 Result__unit__string
            switch mtmp105.(type) {
            case Result__unit__string_Ok:
                var t410 Result__unit__string = unify(st__52, a2__69, b2__71)
                jp409 = t410
            case Result__unit__string_Err:
                var x107 string = mtmp105.(Result__unit__string_Err)._0
                var e__72 string = x107
                var t411 Result__unit__string = Result__unit__string_Err{
                    _0: e__72,
                }
                jp409 = t411
            default:
                panic("non-exhaustive match")
            }
            jp398 = jp409
        default:
            panic("non-exhaustive match")
        }
        jp347 = jp398
    default:
        panic("non-exhaustive match")
    }
    retv345 = jp347
    return retv345
}

func gen(st__73 CheckerState, ty__74 Typ) Typ {
    var retv413 Typ
    var jp415 Typ
    switch ty__74.(type) {
    case TVar:
        var x108 *ref_Tv_x = ty__74.(TVar)._0
        var tvref__75 *ref_Tv_x = x108
        var mtmp112 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvref__75)
        var jp417 Typ
        switch mtmp112.(type) {
        case Unbound:
            var x113 string = mtmp112.(Unbound)._0
            var x114 int32 = mtmp112.(Unbound)._1
            var l__77 int32 = x114
            var name__76 string = x113
            var t418 *ref_int32_x = st__73.current_level
            var cur__78 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t418)
            var t421 bool = l__77 > cur__78
            var jp420 Typ
            if t421 {
                var t422 Typ = QVar{
                    _0: name__76,
                }
                jp420 = t422
            } else {
                var t423 Typ = TVar{
                    _0: tvref__75,
                }
                jp420 = t423
            }
            jp417 = jp420
        case Link:
            var x115 Typ = mtmp112.(Link)._0
            var inner__79 Typ = x115
            var t424 Typ = gen(st__73, inner__79)
            jp417 = t424
        default:
            panic("non-exhaustive match")
        }
        jp415 = jp417
    case QVar:
        var other__82 Typ = ty__74
        jp415 = other__82
    case TArrow:
        var x110 Typ = ty__74.(TArrow)._0
        var x111 Typ = ty__74.(TArrow)._1
        var t2__81 Typ = x111
        var t1__80 Typ = x110
        var t425 Typ = gen(st__73, t1__80)
        var t426 Typ = gen(st__73, t2__81)
        var t427 Typ = TArrow{
            _0: t425,
            _1: t426,
        }
        jp415 = t427
    default:
        panic("non-exhaustive match")
    }
    retv413 = jp415
    return retv413
}

func inst_loop(st__83 CheckerState, subst__84 []SubstEntry, ty__85 Typ) Tuple2_3Typ_16Vec_10SubstEntry {
    var retv429 Tuple2_3Typ_16Vec_10SubstEntry
    var jp431 Tuple2_3Typ_16Vec_10SubstEntry
    switch ty__85.(type) {
    case TVar:
        var x116 *ref_Tv_x = ty__85.(TVar)._0
        var tvref__90 *ref_Tv_x = x116
        var mtmp120 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvref__90)
        var jp433 Tuple2_3Typ_16Vec_10SubstEntry
        switch mtmp120.(type) {
        case Unbound:
            var t434 Typ = TVar{
                _0: tvref__90,
            }
            var t435 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: t434,
                _1: subst__84,
            }
            jp433 = t435
        case Link:
            var x123 Typ = mtmp120.(Link)._0
            var inner__91 Typ = x123
            var t436 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, subst__84, inner__91)
            jp433 = t436
        default:
            panic("non-exhaustive match")
        }
        jp431 = jp433
    case QVar:
        var x117 string = ty__85.(QVar)._0
        var name__86 string = x117
        var mtmp124 Option__Typ = subst_lookup(subst__84, name__86)
        var jp438 Tuple2_3Typ_16Vec_10SubstEntry
        switch mtmp124.(type) {
        case None:
            var tv__88 Typ = newvar(st__83)
            var t439 SubstEntry = SubstEntry{
                name: name__86,
                ty: tv__88,
            }
            var new_subst__89 []SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SubstEntry(subst__84, t439)
            var t440 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: tv__88,
                _1: new_subst__89,
            }
            jp438 = t440
        case Some:
            var x125 Typ = mtmp124.(Some)._0
            var t__87 Typ = x125
            var t441 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: t__87,
                _1: subst__84,
            }
            jp438 = t441
        default:
            panic("non-exhaustive match")
        }
        jp431 = jp438
    case TArrow:
        var x118 Typ = ty__85.(TArrow)._0
        var x119 Typ = ty__85.(TArrow)._1
        var t2__93 Typ = x119
        var t1__92 Typ = x118
        var mtmp126 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, subst__84, t1__92)
        var x127 Typ = mtmp126._0
        var x128 []SubstEntry = mtmp126._1
        var subst1__95 []SubstEntry = x128
        var ty1__94 Typ = x127
        var mtmp129 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, subst1__95, t2__93)
        var x130 Typ = mtmp129._0
        var x131 []SubstEntry = mtmp129._1
        var subst2__97 []SubstEntry = x131
        var ty2__96 Typ = x130
        var t442 Typ = TArrow{
            _0: ty1__94,
            _1: ty2__96,
        }
        var t443 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
            _0: t442,
            _1: subst2__97,
        }
        jp431 = t443
    default:
        panic("non-exhaustive match")
    }
    retv429 = jp431
    return retv429
}

func inst(st__98 CheckerState, ty__99 Typ) Typ {
    var retv445 Typ
    var subst0__100 []SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SubstEntry()
    var mtmp132 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__98, subst0__100, ty__99)
    var x133 Typ = mtmp132._0
    var t__101 Typ = x133
    retv445 = t__101
    return retv445
}

func typeof(st__102 CheckerState, env__103 []EnvEntry, e__104 Exp) Result__Typ__string {
    var retv447 Result__Typ__string
    var jp449 Result__Typ__string
    switch e__104.(type) {
    case Var:
        var x135 string = e__104.(Var)._0
        var x__105 string = x135
        var mtmp143 Option__Typ = env_lookup(env__103, x__105)
        var jp451 Result__Typ__string
        switch mtmp143.(type) {
        case None:
            var t452 Result__Typ__string = Result__Typ__string_Err{
                _0: "unbound var",
            }
            jp451 = t452
        case Some:
            var x144 Typ = mtmp143.(Some)._0
            var ty__106 Typ = x144
            var t453 Typ = inst(st__102, ty__106)
            var t454 Result__Typ__string = Result__Typ__string_Ok{
                _0: t453,
            }
            jp451 = t454
        default:
            panic("non-exhaustive match")
        }
        jp449 = jp451
    case App:
        var x136 Exp = e__104.(App)._0
        var x137 Exp = e__104.(App)._1
        var e2__114 Exp = x137
        var e1__113 Exp = x136
        var mtmp145 Result__Typ__string = typeof(st__102, env__103, e1__113)
        var jp456 Result__Typ__string
        switch mtmp145.(type) {
        case Result__Typ__string_Ok:
            var x146 Typ = mtmp145.(Result__Typ__string_Ok)._0
            var ty_fun__116 Typ = x146
            var mtmp148 Result__Typ__string = typeof(st__102, env__103, e2__114)
            var jp458 Result__Typ__string
            switch mtmp148.(type) {
            case Result__Typ__string_Ok:
                var x149 Typ = mtmp148.(Result__Typ__string_Ok)._0
                var ty_arg__118 Typ = x149
                var ty_res__119 Typ = newvar(st__102)
                var arrow__120 Typ = TArrow{
                    _0: ty_arg__118,
                    _1: ty_res__119,
                }
                var mtmp151 Result__unit__string = unify(st__102, ty_fun__116, arrow__120)
                var jp460 Result__Typ__string
                switch mtmp151.(type) {
                case Result__unit__string_Ok:
                    var t461 Result__Typ__string = Result__Typ__string_Ok{
                        _0: ty_res__119,
                    }
                    jp460 = t461
                case Result__unit__string_Err:
                    var x153 string = mtmp151.(Result__unit__string_Err)._0
                    var e__121 string = x153
                    var t462 Result__Typ__string = Result__Typ__string_Err{
                        _0: e__121,
                    }
                    jp460 = t462
                default:
                    panic("non-exhaustive match")
                }
                jp458 = jp460
            case Result__Typ__string_Err:
                var x150 string = mtmp148.(Result__Typ__string_Err)._0
                var e__117 string = x150
                var t463 Result__Typ__string = Result__Typ__string_Err{
                    _0: e__117,
                }
                jp458 = t463
            default:
                panic("non-exhaustive match")
            }
            jp456 = jp458
        case Result__Typ__string_Err:
            var x147 string = mtmp145.(Result__Typ__string_Err)._0
            var e__115 string = x147
            var t464 Result__Typ__string = Result__Typ__string_Err{
                _0: e__115,
            }
            jp456 = t464
        default:
            panic("non-exhaustive match")
        }
        jp449 = jp456
    case Lam:
        var x138 string = e__104.(Lam)._0
        var x139 Exp = e__104.(Lam)._1
        var body__108 Exp = x139
        var x__107 string = x138
        var ty_x__109 Typ = newvar(st__102)
        var t465 EnvEntry = EnvEntry{
            name: x__107,
            ty: ty_x__109,
        }
        var env2__110 []EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__EnvEntry(env__103, t465)
        var mtmp154 Result__Typ__string = typeof(st__102, env2__110, body__108)
        var jp467 Result__Typ__string
        switch mtmp154.(type) {
        case Result__Typ__string_Ok:
            var x155 Typ = mtmp154.(Result__Typ__string_Ok)._0
            var ty_e__111 Typ = x155
            var t468 Typ = TArrow{
                _0: ty_x__109,
                _1: ty_e__111,
            }
            var t469 Result__Typ__string = Result__Typ__string_Ok{
                _0: t468,
            }
            jp467 = t469
        case Result__Typ__string_Err:
            var x156 string = mtmp154.(Result__Typ__string_Err)._0
            var e__112 string = x156
            var t470 Result__Typ__string = Result__Typ__string_Err{
                _0: e__112,
            }
            jp467 = t470
        default:
            panic("non-exhaustive match")
        }
        jp449 = jp467
    case Let:
        var x140 string = e__104.(Let)._0
        var x141 Exp = e__104.(Let)._1
        var x142 Exp = e__104.(Let)._2
        var e2__124 Exp = x142
        var e1__123 Exp = x141
        var x__122 string = x140
        enter_level(st__102)
        var ty_e__125 Result__Typ__string = typeof(st__102, env__103, e1__123)
        leave_level(st__102)
        var jp472 Result__Typ__string
        switch ty_e__125.(type) {
        case Result__Typ__string_Ok:
            var x159 Typ = ty_e__125.(Result__Typ__string_Ok)._0
            var ty1__127 Typ = x159
            var t473 Typ = gen(st__102, ty1__127)
            var t474 EnvEntry = EnvEntry{
                name: x__122,
                ty: t473,
            }
            var env2__128 []EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__EnvEntry(env__103, t474)
            var t475 Result__Typ__string = typeof(st__102, env2__128, e2__124)
            jp472 = t475
        case Result__Typ__string_Err:
            var x160 string = ty_e__125.(Result__Typ__string_Err)._0
            var e__126 string = x160
            var t476 Result__Typ__string = Result__Typ__string_Err{
                _0: e__126,
            }
            jp472 = t476
        default:
            panic("non-exhaustive match")
        }
        jp449 = jp472
    default:
        panic("non-exhaustive match")
    }
    retv447 = jp449
    return retv447
}

func exp_var(name__129 string) Exp {
    var retv478 Exp
    var t479 Exp = Var{
        _0: name__129,
    }
    retv478 = t479
    return retv478
}

func exp_lam(name__130 string, body__131 Exp) Exp {
    var retv481 Exp
    var t482 Exp = Lam{
        _0: name__130,
        _1: body__131,
    }
    retv481 = t482
    return retv481
}

func exp_app(a__132 Exp, b__133 Exp) Exp {
    var retv484 Exp
    var t485 Exp = App{
        _0: a__132,
        _1: b__133,
    }
    retv484 = t485
    return retv484
}

func exp_let(name__134 string, a__135 Exp, b__136 Exp) Exp {
    var retv487 Exp
    var t488 Exp = Let{
        _0: name__134,
        _1: a__135,
        _2: b__136,
    }
    retv487 = t488
    return retv487
}

func show_result(label__137 string, res__138 Result__Typ__string) struct{} {
    switch res__138.(type) {
    case Result__Typ__string_Ok:
        var x161 Typ = res__138.(Result__Typ__string_Ok)._0
        var ty__139 Typ = x161
        var t491 string = label__137 + ": "
        var t492 string = typ_to_string(ty__139)
        var t493 string = t491 + t492
        println__T_string(t493)
    case Result__Typ__string_Err:
        var x162 string = res__138.(Result__Typ__string_Err)._0
        var e__140 string = x162
        var t495 string = label__137 + ": "
        var t496 string = t495 + e__140
        println__T_string(t496)
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func main0() struct{} {
    var st__141 CheckerState = state_new()
    var t499 Exp = exp_var("x")
    var id__142 Exp = exp_lam("x", t499)
    var t500 Exp = exp_var("x")
    var t501 Exp = exp_var("y")
    var t502 Exp = exp_app(t500, t501)
    var t503 Exp = exp_lam("y", t502)
    var c1__143 Exp = exp_lam("x", t503)
    reset_type_variables(st__141)
    var t504 []EnvEntry = env_empty()
    var t505 Result__Typ__string = typeof(st__141, t504, id__142)
    show_result("id", t505)
    reset_type_variables(st__141)
    var t506 []EnvEntry = env_empty()
    var t507 Result__Typ__string = typeof(st__141, t506, c1__143)
    show_result("c1", t507)
    reset_type_variables(st__141)
    var t508 []EnvEntry = env_empty()
    var t509 Exp = exp_var("x")
    var t510 Exp = exp_let("x", c1__143, t509)
    var t511 Result__Typ__string = typeof(st__141, t508, t510)
    show_result("let_x_c1_x", t511)
    reset_type_variables(st__141)
    var t512 []EnvEntry = env_empty()
    var t513 Exp = exp_var("z")
    var t514 Exp = exp_lam("z", t513)
    var t515 Exp = exp_var("y")
    var t516 Exp = exp_let("y", t514, t515)
    var t517 Result__Typ__string = typeof(st__141, t512, t516)
    show_result("let_y_id_y", t517)
    reset_type_variables(st__141)
    var t518 []EnvEntry = env_empty()
    var t519 Exp = exp_var("z")
    var t520 Exp = exp_lam("z", t519)
    var t521 Exp = exp_var("y")
    var t522 Exp = exp_let("y", t520, t521)
    var t523 Exp = exp_lam("x", t522)
    var t524 Result__Typ__string = typeof(st__141, t518, t523)
    show_result("lam_x_let_y_id_y", t524)
    reset_type_variables(st__141)
    var t525 []EnvEntry = env_empty()
    var t526 Exp = exp_var("z")
    var t527 Exp = exp_lam("z", t526)
    var t528 Exp = exp_var("y")
    var t529 Exp = exp_var("x")
    var t530 Exp = exp_app(t528, t529)
    var t531 Exp = exp_let("y", t527, t530)
    var t532 Exp = exp_lam("x", t531)
    var t533 Result__Typ__string = typeof(st__141, t525, t532)
    show_result("lam_x_let_y_id_yx", t533)
    reset_type_variables(st__141)
    var t534 []EnvEntry = env_empty()
    var t535 Exp = exp_var("x")
    var t536 Exp = exp_var("x")
    var t537 Exp = exp_app(t535, t536)
    var t538 Exp = exp_lam("x", t537)
    var t539 Result__Typ__string = typeof(st__141, t534, t538)
    show_result("self_apply", t539)
    reset_type_variables(st__141)
    var t540 []EnvEntry = env_empty()
    var t541 Exp = exp_var("x")
    var t542 Exp = exp_var("x")
    var t543 Exp = exp_let("x", t541, t542)
    var t544 Result__Typ__string = typeof(st__141, t540, t543)
    show_result("unbound_var", t544)
    reset_type_variables(st__141)
    var t545 []EnvEntry = env_empty()
    var t546 Exp = exp_var("y")
    var t547 Exp = exp_var("y")
    var t548 Exp = exp_var("z")
    var t549 Exp = exp_app(t547, t548)
    var t550 Exp = exp_lam("z", t549)
    var t551 Exp = exp_app(t546, t550)
    var t552 Exp = exp_lam("y", t551)
    var t553 Result__Typ__string = typeof(st__141, t545, t552)
    show_result("max_heiber", t553)
    reset_type_variables(st__141)
    var t554 []EnvEntry = env_empty()
    var t555 Exp = exp_var("k")
    var t556 Exp = exp_var("k")
    var t557 Exp = exp_var("x")
    var t558 Exp = exp_app(t556, t557)
    var t559 Exp = exp_var("y")
    var t560 Exp = exp_app(t558, t559)
    var t561 Exp = exp_app(t555, t560)
    var t562 Exp = exp_var("k")
    var t563 Exp = exp_var("y")
    var t564 Exp = exp_app(t562, t563)
    var t565 Exp = exp_var("x")
    var t566 Exp = exp_app(t564, t565)
    var t567 Exp = exp_app(t561, t566)
    var t568 Exp = exp_lam("k", t567)
    var t569 Exp = exp_lam("y", t568)
    var t570 Exp = exp_lam("x", t569)
    var t571 Result__Typ__string = typeof(st__141, t554, t570)
    show_result("kirang", t571)
    reset_type_variables(st__141)
    var t572 []EnvEntry = env_empty()
    var t573 Exp = exp_var("id")
    var t574 Exp = exp_var("id")
    var t575 Exp = exp_app(t573, t574)
    var t576 Exp = exp_let("id", id__142, t575)
    var t577 Result__Typ__string = typeof(st__141, t572, t576)
    show_result("let_id_idid", t577)
    reset_type_variables(st__141)
    var t578 []EnvEntry = env_empty()
    var t579 Exp = exp_var("x")
    var t580 Exp = exp_app(t579, id__142)
    var t581 Exp = exp_var("z")
    var t582 Exp = exp_let("z", t580, t581)
    var t583 Exp = exp_var("y")
    var t584 Exp = exp_let("y", t582, t583)
    var t585 Exp = exp_let("x", c1__143, t584)
    var t586 Result__Typ__string = typeof(st__141, t578, t585)
    show_result("nested_lets", t586)
    reset_type_variables(st__141)
    var t587 []EnvEntry = env_empty()
    var t588 Exp = exp_var("x")
    var t589 Exp = exp_var("y")
    var t590 Exp = exp_app(t588, t589)
    var t591 Exp = exp_var("y")
    var t592 Exp = exp_var("x")
    var t593 Exp = exp_app(t591, t592)
    var t594 Exp = exp_lam("x", t593)
    var t595 Exp = exp_let("x", t590, t594)
    var t596 Exp = exp_lam("y", t595)
    var t597 Exp = exp_lam("x", t596)
    var t598 Result__Typ__string = typeof(st__141, t587, t597)
    show_result("fun_x_fun_y_let_x_xy_fun_x_yx", t598)
    reset_type_variables(st__141)
    var t599 []EnvEntry = env_empty()
    var t600 Exp = exp_var("x")
    var t601 Exp = exp_var("y")
    var t602 Exp = exp_let("y", t600, t601)
    var t603 Exp = exp_lam("x", t602)
    var t604 Result__Typ__string = typeof(st__141, t599, t603)
    show_result("sound_gen_1", t604)
    reset_type_variables(st__141)
    var t605 []EnvEntry = env_empty()
    var t606 Exp = exp_var("x")
    var t607 Exp = exp_lam("z", t606)
    var t608 Exp = exp_var("y")
    var t609 Exp = exp_let("y", t607, t608)
    var t610 Exp = exp_lam("x", t609)
    var t611 Result__Typ__string = typeof(st__141, t605, t610)
    show_result("sound_gen_2", t611)
    reset_type_variables(st__141)
    var t612 []EnvEntry = env_empty()
    var t613 Exp = exp_var("x")
    var t614 Exp = exp_var("z")
    var t615 Exp = exp_app(t613, t614)
    var t616 Exp = exp_lam("z", t615)
    var t617 Exp = exp_var("y")
    var t618 Exp = exp_let("y", t616, t617)
    var t619 Exp = exp_lam("x", t618)
    var t620 Result__Typ__string = typeof(st__141, t612, t619)
    show_result("sound_gen_3", t620)
    reset_type_variables(st__141)
    var t621 []EnvEntry = env_empty()
    var t622 Exp = exp_var("x")
    var t623 Exp = exp_var("y")
    var t624 Exp = exp_app(t622, t623)
    var t625 Exp = exp_var("x")
    var t626 Exp = exp_var("y")
    var t627 Exp = exp_app(t625, t626)
    var t628 Exp = exp_let("x", t624, t627)
    var t629 Exp = exp_lam("y", t628)
    var t630 Exp = exp_lam("x", t629)
    var t631 Result__Typ__string = typeof(st__141, t621, t630)
    show_result("double_apply", t631)
    reset_type_variables(st__141)
    var t632 []EnvEntry = env_empty()
    var t633 Exp = exp_var("x")
    var t634 Exp = exp_var("y")
    var t635 Exp = exp_var("y")
    var t636 Exp = exp_app(t634, t635)
    var t637 Exp = exp_let("y", t633, t636)
    var t638 Exp = exp_lam("x", t637)
    var t639 Result__Typ__string = typeof(st__141, t632, t638)
    show_result("sound_gen_occurs", t639)
    reset_gensym(st__141)
    var t640 []EnvEntry = env_empty()
    var t641 Exp = exp_var("x")
    var t642 Exp = exp_app(t641, id__142)
    var t643 Exp = exp_var("z")
    var t644 Exp = exp_let("z", t642, t643)
    var t645 Exp = exp_var("y")
    var t646 Exp = exp_let("y", t644, t645)
    var t647 Exp = exp_lam("x", t646)
    var t648 Result__Typ__string = typeof(st__141, t640, t647)
    show_result("fun_x_let_y_let_z_x_id_z_y", t648)
    println__T_string("")
    println__T_string("All Done")
    println__T_string("")
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__93 int32) *ref_int32_x {
    var retv650 *ref_int32_x
    var t651 *ref_int32_x = ref__Ref_5int32(value__93)
    retv650 = t651
    return retv650
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__95 *ref_int32_x, value__96 int32) struct{} {
    ref_set__Ref_5int32(self__95, value__96)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__94 *ref_int32_x) int32 {
    var retv655 int32
    var t656 int32 = ref_get__Ref_5int32(self__94)
    retv655 = t656
    return retv655
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Tv(value__93 Tv) *ref_Tv_x {
    var retv658 *ref_Tv_x
    var t659 *ref_Tv_x = ref__Ref_2Tv(value__93)
    retv658 = t659
    return retv658
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(self__94 *ref_Tv_x) Tv {
    var retv661 Tv
    var t662 Tv = ref_get__Ref_2Tv(self__94)
    retv661 = t662
    return retv661
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__EnvEntry() []EnvEntry {
    var retv664 []EnvEntry
    var t665 []EnvEntry = nil
    retv664 = t665
    return retv664
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__EnvEntry(self__73 []EnvEntry) int32 {
    var retv667 int32
    var t668 int32 = int32(len(self__73))
    retv667 = t668
    return retv667
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_Typ_r_(value__93 Option__Typ) *ref_Option__Typ_x {
    var retv670 *ref_Option__Typ_x
    var t671 *ref_Option__Typ_x = ref__Ref_11Option__Typ(value__93)
    retv670 = t671
    return retv670
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__93 bool) *ref_bool_x {
    var retv673 *ref_bool_x
    var t674 *ref_bool_x = ref__Ref_4bool(value__93)
    retv673 = t674
    return retv673
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__94 *ref_bool_x) bool {
    var retv676 bool
    var t677 bool = ref_get__Ref_4bool(self__94)
    retv676 = t677
    return retv676
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Option_l_Typ_r_(self__95 *ref_Option__Typ_x, value__96 Option__Typ) struct{} {
    ref_set__Ref_11Option__Typ(self__95, value__96)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(self__95 *ref_bool_x, value__96 bool) struct{} {
    ref_set__Ref_4bool(self__95, value__96)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Option_l_Typ_r_(self__94 *ref_Option__Typ_x) Option__Typ {
    var retv683 Option__Typ
    var t684 Option__Typ = ref_get__Ref_11Option__Typ(self__94)
    retv683 = t684
    return retv683
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SubstEntry(self__73 []SubstEntry) int32 {
    var retv686 int32
    var t687 int32 = int32(len(self__73))
    retv686 = t687
    return retv686
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(self__95 *ref_Tv_x, value__96 Tv) struct{} {
    ref_set__Ref_2Tv(self__95, value__96)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SubstEntry(self__66 []SubstEntry, elem__67 SubstEntry) []SubstEntry {
    var retv691 []SubstEntry
    var t692 []SubstEntry = append(self__66, elem__67)
    retv691 = t692
    return retv691
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SubstEntry() []SubstEntry {
    var retv694 []SubstEntry
    var t695 []SubstEntry = nil
    retv694 = t695
    return retv694
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__EnvEntry(self__66 []EnvEntry, elem__67 EnvEntry) []EnvEntry {
    var retv697 []EnvEntry
    var t698 []EnvEntry = append(self__66, elem__67)
    retv697 = t698
    return retv697
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
