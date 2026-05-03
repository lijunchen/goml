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
    var t206 *ref_int32_x = ref__Ref_5int32(0)
    var t207 *ref_int32_x = ref__Ref_5int32(1)
    var t208 CheckerState = CheckerState{
        gensym_counter: t206,
        current_level: t207,
    }
    retv205 = t208
    return retv205
}

func reset_gensym(st__0 CheckerState) struct{} {
    var t210 *ref_int32_x = st__0.gensym_counter
    ref_set__Ref_5int32(t210, 0)
    return struct{}{}
}

func reset_level(st__1 CheckerState) struct{} {
    var t212 *ref_int32_x = st__1.current_level
    ref_set__Ref_5int32(t212, 1)
    return struct{}{}
}

func reset_type_variables(st__2 CheckerState) struct{} {
    reset_gensym(st__2)
    reset_level(st__2)
    return struct{}{}
}

func enter_level(st__3 CheckerState) struct{} {
    var t216 *ref_int32_x = st__3.current_level
    var l__4 int32 = ref_get__Ref_5int32(t216)
    var t217 *ref_int32_x = st__3.current_level
    var t218 int32 = l__4 + 1
    ref_set__Ref_5int32(t217, t218)
    return struct{}{}
}

func leave_level(st__5 CheckerState) struct{} {
    var t220 *ref_int32_x = st__5.current_level
    var l__6 int32 = ref_get__Ref_5int32(t220)
    var t221 *ref_int32_x = st__5.current_level
    var t222 int32 = l__6 - 1
    ref_set__Ref_5int32(t221, t222)
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
    var n__11 int32 = ref_get__Ref_5int32(t234)
    var t235 *ref_int32_x = st__10.gensym_counter
    var t236 int32 = n__11 + 1
    ref_set__Ref_5int32(t235, t236)
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
    var level__14 int32 = ref_get__Ref_5int32(t246)
    var t247 Tv = Unbound{
        _0: name__13,
        _1: level__14,
    }
    var t248 *ref_Tv_x = ref__Ref_2Tv(t247)
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
        var mtmp10 Tv = ref_get__Ref_2Tv(tvref__16)
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
        var mtmp18 Tv = ref_get__Ref_2Tv(tvref__20)
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
    var env__27 []EnvEntry = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_EnvEntry()
    retv276 = env__27
    return retv276
}

func env_lookup(env__28 []EnvEntry, name__29 string) Option__Typ {
    var retv278 Option__Typ
    var t279 int32 = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_len_x5f__x5f_T_x5f_EnvEntry(env__28)
    var t280 int32 = t279 - 1
    var i__30 *ref_int32_x = ref__Ref_5int32(t280)
    var found__31 *ref_Option__Typ_x = ref__Ref_11Option__Typ(None{})
    var done__32 *ref_bool_x = ref__Ref_4bool(false)
    Loop_loop283:
    for {
        var t284 bool = ref_get__Ref_4bool(done__32)
        var t285 bool = !t284
        var t286 int32 = ref_get__Ref_5int32(i__30)
        var t287 bool = t286 >= 0
        var t288 bool = t285 && t287
        if t288 {
            var t289 int32 = ref_get__Ref_5int32(i__30)
            var entry__33 EnvEntry = env__28[t289]
            var t291 string = entry__33.name
            var t292 bool = t291 == name__29
            if t292 {
                var t293 Typ = entry__33.ty
                var t294 Option__Typ = Some{
                    _0: t293,
                }
                ref_set__Ref_11Option__Typ(found__31, t294)
                ref_set__Ref_4bool(done__32, true)
            } else {
                var t296 int32 = ref_get__Ref_5int32(i__30)
                var t297 int32 = t296 - 1
                ref_set__Ref_5int32(i__30, t297)
            }
            continue
        } else {
            break Loop_loop283
        }
    }
    var t282 Option__Typ = ref_get__Ref_11Option__Typ(found__31)
    retv278 = t282
    return retv278
}

func subst_lookup(subst__34 []SubstEntry, name__35 string) Option__Typ {
    var retv300 Option__Typ
    var t301 int32 = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_len_x5f__x5f_T_x5f_SubstEntry(subst__34)
    var t302 int32 = t301 - 1
    var i__36 *ref_int32_x = ref__Ref_5int32(t302)
    var found__37 *ref_Option__Typ_x = ref__Ref_11Option__Typ(None{})
    var done__38 *ref_bool_x = ref__Ref_4bool(false)
    Loop_loop305:
    for {
        var t306 bool = ref_get__Ref_4bool(done__38)
        var t307 bool = !t306
        var t308 int32 = ref_get__Ref_5int32(i__36)
        var t309 bool = t308 >= 0
        var t310 bool = t307 && t309
        if t310 {
            var t311 int32 = ref_get__Ref_5int32(i__36)
            var entry__39 SubstEntry = subst__34[t311]
            var t313 string = entry__39.name
            var t314 bool = t313 == name__35
            if t314 {
                var t315 Typ = entry__39.ty
                var t316 Option__Typ = Some{
                    _0: t315,
                }
                ref_set__Ref_11Option__Typ(found__37, t316)
                ref_set__Ref_4bool(done__38, true)
            } else {
                var t318 int32 = ref_get__Ref_5int32(i__36)
                var t319 int32 = t318 - 1
                ref_set__Ref_5int32(i__36, t319)
            }
            continue
        } else {
            break Loop_loop305
        }
    }
    var t304 Option__Typ = ref_get__Ref_11Option__Typ(found__37)
    retv300 = t304
    return retv300
}

func occurs(st__40 CheckerState, tvr__41 *ref_Tv_x, ty__42 Typ) Result__unit__string {
    var retv322 Result__unit__string
    var jp324 Result__unit__string
    switch ty__42.(type) {
    case TVar:
        var x26 *ref_Tv_x = ty__42.(TVar)._0
        var tvr2__43 *ref_Tv_x = x26
        var t327 bool = ptr_eq__Ref_2Tv(tvr__41, tvr2__43)
        var jp326 Result__unit__string
        if t327 {
            var t328 Result__unit__string = Result__unit__string_Err{
                _0: "occurs check",
            }
            jp326 = t328
        } else {
            var mtmp30 Tv = ref_get__Ref_2Tv(tvr2__43)
            var jp330 Result__unit__string
            switch mtmp30.(type) {
            case Unbound:
                var x31 string = mtmp30.(Unbound)._0
                var x32 int32 = mtmp30.(Unbound)._1
                var l2__45 int32 = x32
                var name__44 string = x31
                var mtmp34 Tv = ref_get__Ref_2Tv(tvr__41)
                var jp332 int32
                switch mtmp34.(type) {
                case Unbound:
                    var x36 int32 = mtmp34.(Unbound)._1
                    var l__46 int32 = x36
                    var t335 int32 = min_i32(l__46, l2__45)
                    jp332 = t335
                case Link:
                    jp332 = l2__45
                default:
                    panic("non-exhaustive match")
                }
                var min_level__47 int32 = jp332
                var t333 Tv = Unbound{
                    _0: name__44,
                    _1: min_level__47,
                }
                ref_set__Ref_2Tv(tvr2__43, t333)
                var t334 Result__unit__string = Result__unit__string_Ok{
                    _0: struct{}{},
                }
                jp330 = t334
            case Link:
                var x33 Typ = mtmp30.(Link)._0
                var inner__48 Typ = x33
                var t336 Result__unit__string = occurs(st__40, tvr__41, inner__48)
                jp330 = t336
            default:
                panic("non-exhaustive match")
            }
            jp326 = jp330
        }
        jp324 = jp326
    case QVar:
        var t337 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp324 = t337
    case TArrow:
        var x28 Typ = ty__42.(TArrow)._0
        var x29 Typ = ty__42.(TArrow)._1
        var t2__50 Typ = x29
        var t1__49 Typ = x28
        var mtmp39 Result__unit__string = occurs(st__40, tvr__41, t1__49)
        var jp339 Result__unit__string
        switch mtmp39.(type) {
        case Result__unit__string_Ok:
            var t340 Result__unit__string = occurs(st__40, tvr__41, t2__50)
            jp339 = t340
        case Result__unit__string_Err:
            var x41 string = mtmp39.(Result__unit__string_Err)._0
            var e__51 string = x41
            var t341 Result__unit__string = Result__unit__string_Err{
                _0: e__51,
            }
            jp339 = t341
        default:
            panic("non-exhaustive match")
        }
        jp324 = jp339
    default:
        panic("non-exhaustive match")
    }
    retv322 = jp324
    return retv322
}

func unify(st__52 CheckerState, t1__53 Typ, t2__54 Typ) Result__unit__string {
    var retv343 Result__unit__string
    var mtmp42 Tuple2_3Typ_3Typ = Tuple2_3Typ_3Typ{
        _0: t1__53,
        _1: t2__54,
    }
    var x43 Typ = mtmp42._0
    var x44 Typ = mtmp42._1
    var jp345 Result__unit__string
    switch x44.(type) {
    case TVar:
        var x45 *ref_Tv_x = x44.(TVar)._0
        var jp347 Result__unit__string
        switch x43.(type) {
        case TVar:
            var x49 *ref_Tv_x = x43.(TVar)._0
            var r1__55 *ref_Tv_x = x49
            var r2__56 *ref_Tv_x = x45
            var t350 bool = ptr_eq__Ref_2Tv(r1__55, r2__56)
            var jp349 Result__unit__string
            if t350 {
                var t351 Result__unit__string = Result__unit__string_Ok{
                    _0: struct{}{},
                }
                jp349 = t351
            } else {
                var mtmp53 Tv = ref_get__Ref_2Tv(r1__55)
                var jp353 Result__unit__string
                switch mtmp53.(type) {
                case Unbound:
                    var mtmp57 Tv = ref_get__Ref_2Tv(r2__56)
                    var jp355 Result__unit__string
                    switch mtmp57.(type) {
                    case Unbound:
                        var t356 Typ = TVar{
                            _0: r2__56,
                        }
                        var mtmp61 Result__unit__string = occurs(st__52, r1__55, t356)
                        var jp358 Result__unit__string
                        switch mtmp61.(type) {
                        case Result__unit__string_Ok:
                            var t359 Typ = TVar{
                                _0: r2__56,
                            }
                            var t360 Tv = Link{
                                _0: t359,
                            }
                            ref_set__Ref_2Tv(r1__55, t360)
                            var t361 Result__unit__string = Result__unit__string_Ok{
                                _0: struct{}{},
                            }
                            jp358 = t361
                        case Result__unit__string_Err:
                            var x63 string = mtmp61.(Result__unit__string_Err)._0
                            var e__59 string = x63
                            var t362 Result__unit__string = Result__unit__string_Err{
                                _0: e__59,
                            }
                            jp358 = t362
                        default:
                            panic("non-exhaustive match")
                        }
                        jp355 = jp358
                    case Link:
                        var x60 Typ = mtmp57.(Link)._0
                        var inner__58 Typ = x60
                        var t363 Typ = TVar{
                            _0: r1__55,
                        }
                        var t364 Result__unit__string = unify(st__52, t363, inner__58)
                        jp355 = t364
                    default:
                        panic("non-exhaustive match")
                    }
                    jp353 = jp355
                case Link:
                    var x56 Typ = mtmp53.(Link)._0
                    var inner__57 Typ = x56
                    var t365 Typ = TVar{
                        _0: r2__56,
                    }
                    var t366 Result__unit__string = unify(st__52, inner__57, t365)
                    jp353 = t366
                default:
                    panic("non-exhaustive match")
                }
                jp349 = jp353
            }
            jp347 = jp349
        case QVar:
            var r2__65 *ref_Tv_x = x45
            var other__64 Typ = x43
            var mtmp65 Tv = ref_get__Ref_2Tv(r2__65)
            var jp368 Result__unit__string
            switch mtmp65.(type) {
            case Unbound:
                var mtmp69 Result__unit__string = occurs(st__52, r2__65, other__64)
                var jp370 Result__unit__string
                switch mtmp69.(type) {
                case Result__unit__string_Ok:
                    var t371 Tv = Link{
                        _0: other__64,
                    }
                    ref_set__Ref_2Tv(r2__65, t371)
                    var t372 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    jp370 = t372
                case Result__unit__string_Err:
                    var x71 string = mtmp69.(Result__unit__string_Err)._0
                    var e__67 string = x71
                    var t373 Result__unit__string = Result__unit__string_Err{
                        _0: e__67,
                    }
                    jp370 = t373
                default:
                    panic("non-exhaustive match")
                }
                jp368 = jp370
            case Link:
                var x68 Typ = mtmp65.(Link)._0
                var inner__66 Typ = x68
                var t374 Result__unit__string = unify(st__52, other__64, inner__66)
                jp368 = t374
            default:
                panic("non-exhaustive match")
            }
            jp347 = jp368
        case TArrow:
            var r2__65 *ref_Tv_x = x45
            var other__64 Typ = x43
            var mtmp73 Tv = ref_get__Ref_2Tv(r2__65)
            var jp376 Result__unit__string
            switch mtmp73.(type) {
            case Unbound:
                var mtmp77 Result__unit__string = occurs(st__52, r2__65, other__64)
                var jp378 Result__unit__string
                switch mtmp77.(type) {
                case Result__unit__string_Ok:
                    var t379 Tv = Link{
                        _0: other__64,
                    }
                    ref_set__Ref_2Tv(r2__65, t379)
                    var t380 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    jp378 = t380
                case Result__unit__string_Err:
                    var x79 string = mtmp77.(Result__unit__string_Err)._0
                    var e__67 string = x79
                    var t381 Result__unit__string = Result__unit__string_Err{
                        _0: e__67,
                    }
                    jp378 = t381
                default:
                    panic("non-exhaustive match")
                }
                jp376 = jp378
            case Link:
                var x76 Typ = mtmp73.(Link)._0
                var inner__66 Typ = x76
                var t382 Result__unit__string = unify(st__52, other__64, inner__66)
                jp376 = t382
            default:
                panic("non-exhaustive match")
            }
            jp347 = jp376
        default:
            panic("non-exhaustive match")
        }
        jp345 = jp347
    case QVar:
        var jp384 Result__unit__string
        switch x43.(type) {
        case TVar:
            var x81 *ref_Tv_x = x43.(TVar)._0
            var r1__60 *ref_Tv_x = x81
            var other__61 Typ = x44
            var mtmp85 Tv = ref_get__Ref_2Tv(r1__60)
            var jp386 Result__unit__string
            switch mtmp85.(type) {
            case Unbound:
                var mtmp89 Result__unit__string = occurs(st__52, r1__60, other__61)
                var jp388 Result__unit__string
                switch mtmp89.(type) {
                case Result__unit__string_Ok:
                    var t389 Tv = Link{
                        _0: other__61,
                    }
                    ref_set__Ref_2Tv(r1__60, t389)
                    var t390 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    jp388 = t390
                case Result__unit__string_Err:
                    var x91 string = mtmp89.(Result__unit__string_Err)._0
                    var e__63 string = x91
                    var t391 Result__unit__string = Result__unit__string_Err{
                        _0: e__63,
                    }
                    jp388 = t391
                default:
                    panic("non-exhaustive match")
                }
                jp386 = jp388
            case Link:
                var x88 Typ = mtmp85.(Link)._0
                var inner__62 Typ = x88
                var t392 Result__unit__string = unify(st__52, inner__62, other__61)
                jp386 = t392
            default:
                panic("non-exhaustive match")
            }
            jp384 = jp386
        case QVar:
            var t393 Result__unit__string = Result__unit__string_Err{
                _0: "unify error",
            }
            jp384 = t393
        case TArrow:
            var t394 Result__unit__string = Result__unit__string_Err{
                _0: "unify error",
            }
            jp384 = t394
        default:
            panic("non-exhaustive match")
        }
        jp345 = jp384
    case TArrow:
        var x47 Typ = x44.(TArrow)._0
        var x48 Typ = x44.(TArrow)._1
        var jp396 Result__unit__string
        switch x43.(type) {
        case TVar:
            var x93 *ref_Tv_x = x43.(TVar)._0
            var r1__60 *ref_Tv_x = x93
            var other__61 Typ = x44
            var mtmp97 Tv = ref_get__Ref_2Tv(r1__60)
            var jp398 Result__unit__string
            switch mtmp97.(type) {
            case Unbound:
                var mtmp101 Result__unit__string = occurs(st__52, r1__60, other__61)
                var jp400 Result__unit__string
                switch mtmp101.(type) {
                case Result__unit__string_Ok:
                    var t401 Tv = Link{
                        _0: other__61,
                    }
                    ref_set__Ref_2Tv(r1__60, t401)
                    var t402 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    jp400 = t402
                case Result__unit__string_Err:
                    var x103 string = mtmp101.(Result__unit__string_Err)._0
                    var e__63 string = x103
                    var t403 Result__unit__string = Result__unit__string_Err{
                        _0: e__63,
                    }
                    jp400 = t403
                default:
                    panic("non-exhaustive match")
                }
                jp398 = jp400
            case Link:
                var x100 Typ = mtmp97.(Link)._0
                var inner__62 Typ = x100
                var t404 Result__unit__string = unify(st__52, inner__62, other__61)
                jp398 = t404
            default:
                panic("non-exhaustive match")
            }
            jp396 = jp398
        case QVar:
            var t405 Result__unit__string = Result__unit__string_Err{
                _0: "unify error",
            }
            jp396 = t405
        case TArrow:
            var x95 Typ = x43.(TArrow)._0
            var x96 Typ = x43.(TArrow)._1
            var a2__69 Typ = x96
            var a1__68 Typ = x95
            var b2__71 Typ = x48
            var b1__70 Typ = x47
            var mtmp105 Result__unit__string = unify(st__52, a1__68, b1__70)
            var jp407 Result__unit__string
            switch mtmp105.(type) {
            case Result__unit__string_Ok:
                var t408 Result__unit__string = unify(st__52, a2__69, b2__71)
                jp407 = t408
            case Result__unit__string_Err:
                var x107 string = mtmp105.(Result__unit__string_Err)._0
                var e__72 string = x107
                var t409 Result__unit__string = Result__unit__string_Err{
                    _0: e__72,
                }
                jp407 = t409
            default:
                panic("non-exhaustive match")
            }
            jp396 = jp407
        default:
            panic("non-exhaustive match")
        }
        jp345 = jp396
    default:
        panic("non-exhaustive match")
    }
    retv343 = jp345
    return retv343
}

func gen(st__73 CheckerState, ty__74 Typ) Typ {
    var retv411 Typ
    var jp413 Typ
    switch ty__74.(type) {
    case TVar:
        var x108 *ref_Tv_x = ty__74.(TVar)._0
        var tvref__75 *ref_Tv_x = x108
        var mtmp112 Tv = ref_get__Ref_2Tv(tvref__75)
        var jp415 Typ
        switch mtmp112.(type) {
        case Unbound:
            var x113 string = mtmp112.(Unbound)._0
            var x114 int32 = mtmp112.(Unbound)._1
            var l__77 int32 = x114
            var name__76 string = x113
            var t416 *ref_int32_x = st__73.current_level
            var cur__78 int32 = ref_get__Ref_5int32(t416)
            var t419 bool = l__77 > cur__78
            var jp418 Typ
            if t419 {
                var t420 Typ = QVar{
                    _0: name__76,
                }
                jp418 = t420
            } else {
                var t421 Typ = TVar{
                    _0: tvref__75,
                }
                jp418 = t421
            }
            jp415 = jp418
        case Link:
            var x115 Typ = mtmp112.(Link)._0
            var inner__79 Typ = x115
            var t422 Typ = gen(st__73, inner__79)
            jp415 = t422
        default:
            panic("non-exhaustive match")
        }
        jp413 = jp415
    case QVar:
        var other__82 Typ = ty__74
        jp413 = other__82
    case TArrow:
        var x110 Typ = ty__74.(TArrow)._0
        var x111 Typ = ty__74.(TArrow)._1
        var t2__81 Typ = x111
        var t1__80 Typ = x110
        var t423 Typ = gen(st__73, t1__80)
        var t424 Typ = gen(st__73, t2__81)
        var t425 Typ = TArrow{
            _0: t423,
            _1: t424,
        }
        jp413 = t425
    default:
        panic("non-exhaustive match")
    }
    retv411 = jp413
    return retv411
}

func inst_loop(st__83 CheckerState, subst__84 []SubstEntry, ty__85 Typ) Tuple2_3Typ_16Vec_10SubstEntry {
    var retv427 Tuple2_3Typ_16Vec_10SubstEntry
    var jp429 Tuple2_3Typ_16Vec_10SubstEntry
    switch ty__85.(type) {
    case TVar:
        var x116 *ref_Tv_x = ty__85.(TVar)._0
        var tvref__90 *ref_Tv_x = x116
        var mtmp120 Tv = ref_get__Ref_2Tv(tvref__90)
        var jp431 Tuple2_3Typ_16Vec_10SubstEntry
        switch mtmp120.(type) {
        case Unbound:
            var t432 Typ = TVar{
                _0: tvref__90,
            }
            var t433 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: t432,
                _1: subst__84,
            }
            jp431 = t433
        case Link:
            var x123 Typ = mtmp120.(Link)._0
            var inner__91 Typ = x123
            var t434 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, subst__84, inner__91)
            jp431 = t434
        default:
            panic("non-exhaustive match")
        }
        jp429 = jp431
    case QVar:
        var x117 string = ty__85.(QVar)._0
        var name__86 string = x117
        var mtmp124 Option__Typ = subst_lookup(subst__84, name__86)
        var jp436 Tuple2_3Typ_16Vec_10SubstEntry
        switch mtmp124.(type) {
        case None:
            var tv__88 Typ = newvar(st__83)
            var t437 SubstEntry = SubstEntry{
                name: name__86,
                ty: tv__88,
            }
            var new_subst__89 []SubstEntry = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_push_x5f__x5f_T_x5f_SubstEntry(subst__84, t437)
            var t438 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: tv__88,
                _1: new_subst__89,
            }
            jp436 = t438
        case Some:
            var x125 Typ = mtmp124.(Some)._0
            var t__87 Typ = x125
            var t439 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: t__87,
                _1: subst__84,
            }
            jp436 = t439
        default:
            panic("non-exhaustive match")
        }
        jp429 = jp436
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
        var t440 Typ = TArrow{
            _0: ty1__94,
            _1: ty2__96,
        }
        var t441 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
            _0: t440,
            _1: subst2__97,
        }
        jp429 = t441
    default:
        panic("non-exhaustive match")
    }
    retv427 = jp429
    return retv427
}

func inst(st__98 CheckerState, ty__99 Typ) Typ {
    var retv443 Typ
    var subst0__100 []SubstEntry = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_SubstEntry()
    var mtmp132 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__98, subst0__100, ty__99)
    var x133 Typ = mtmp132._0
    var t__101 Typ = x133
    retv443 = t__101
    return retv443
}

func typeof(st__102 CheckerState, env__103 []EnvEntry, e__104 Exp) Result__Typ__string {
    var retv445 Result__Typ__string
    var jp447 Result__Typ__string
    switch e__104.(type) {
    case Var:
        var x135 string = e__104.(Var)._0
        var x__105 string = x135
        var mtmp143 Option__Typ = env_lookup(env__103, x__105)
        var jp449 Result__Typ__string
        switch mtmp143.(type) {
        case None:
            var t450 Result__Typ__string = Result__Typ__string_Err{
                _0: "unbound var",
            }
            jp449 = t450
        case Some:
            var x144 Typ = mtmp143.(Some)._0
            var ty__106 Typ = x144
            var t451 Typ = inst(st__102, ty__106)
            var t452 Result__Typ__string = Result__Typ__string_Ok{
                _0: t451,
            }
            jp449 = t452
        default:
            panic("non-exhaustive match")
        }
        jp447 = jp449
    case App:
        var x136 Exp = e__104.(App)._0
        var x137 Exp = e__104.(App)._1
        var e2__114 Exp = x137
        var e1__113 Exp = x136
        var mtmp145 Result__Typ__string = typeof(st__102, env__103, e1__113)
        var jp454 Result__Typ__string
        switch mtmp145.(type) {
        case Result__Typ__string_Ok:
            var x146 Typ = mtmp145.(Result__Typ__string_Ok)._0
            var ty_fun__116 Typ = x146
            var mtmp148 Result__Typ__string = typeof(st__102, env__103, e2__114)
            var jp456 Result__Typ__string
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
                var jp458 Result__Typ__string
                switch mtmp151.(type) {
                case Result__unit__string_Ok:
                    var t459 Result__Typ__string = Result__Typ__string_Ok{
                        _0: ty_res__119,
                    }
                    jp458 = t459
                case Result__unit__string_Err:
                    var x153 string = mtmp151.(Result__unit__string_Err)._0
                    var e__121 string = x153
                    var t460 Result__Typ__string = Result__Typ__string_Err{
                        _0: e__121,
                    }
                    jp458 = t460
                default:
                    panic("non-exhaustive match")
                }
                jp456 = jp458
            case Result__Typ__string_Err:
                var x150 string = mtmp148.(Result__Typ__string_Err)._0
                var e__117 string = x150
                var t461 Result__Typ__string = Result__Typ__string_Err{
                    _0: e__117,
                }
                jp456 = t461
            default:
                panic("non-exhaustive match")
            }
            jp454 = jp456
        case Result__Typ__string_Err:
            var x147 string = mtmp145.(Result__Typ__string_Err)._0
            var e__115 string = x147
            var t462 Result__Typ__string = Result__Typ__string_Err{
                _0: e__115,
            }
            jp454 = t462
        default:
            panic("non-exhaustive match")
        }
        jp447 = jp454
    case Lam:
        var x138 string = e__104.(Lam)._0
        var x139 Exp = e__104.(Lam)._1
        var body__108 Exp = x139
        var x__107 string = x138
        var ty_x__109 Typ = newvar(st__102)
        var t463 EnvEntry = EnvEntry{
            name: x__107,
            ty: ty_x__109,
        }
        var env2__110 []EnvEntry = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_push_x5f__x5f_T_x5f_EnvEntry(env__103, t463)
        var mtmp154 Result__Typ__string = typeof(st__102, env2__110, body__108)
        var jp465 Result__Typ__string
        switch mtmp154.(type) {
        case Result__Typ__string_Ok:
            var x155 Typ = mtmp154.(Result__Typ__string_Ok)._0
            var ty_e__111 Typ = x155
            var t466 Typ = TArrow{
                _0: ty_x__109,
                _1: ty_e__111,
            }
            var t467 Result__Typ__string = Result__Typ__string_Ok{
                _0: t466,
            }
            jp465 = t467
        case Result__Typ__string_Err:
            var x156 string = mtmp154.(Result__Typ__string_Err)._0
            var e__112 string = x156
            var t468 Result__Typ__string = Result__Typ__string_Err{
                _0: e__112,
            }
            jp465 = t468
        default:
            panic("non-exhaustive match")
        }
        jp447 = jp465
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
        var jp470 Result__Typ__string
        switch ty_e__125.(type) {
        case Result__Typ__string_Ok:
            var x159 Typ = ty_e__125.(Result__Typ__string_Ok)._0
            var ty1__127 Typ = x159
            var t471 Typ = gen(st__102, ty1__127)
            var t472 EnvEntry = EnvEntry{
                name: x__122,
                ty: t471,
            }
            var env2__128 []EnvEntry = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_push_x5f__x5f_T_x5f_EnvEntry(env__103, t472)
            var t473 Result__Typ__string = typeof(st__102, env2__128, e2__124)
            jp470 = t473
        case Result__Typ__string_Err:
            var x160 string = ty_e__125.(Result__Typ__string_Err)._0
            var e__126 string = x160
            var t474 Result__Typ__string = Result__Typ__string_Err{
                _0: e__126,
            }
            jp470 = t474
        default:
            panic("non-exhaustive match")
        }
        jp447 = jp470
    default:
        panic("non-exhaustive match")
    }
    retv445 = jp447
    return retv445
}

func exp_var(name__129 string) Exp {
    var retv476 Exp
    var t477 Exp = Var{
        _0: name__129,
    }
    retv476 = t477
    return retv476
}

func exp_lam(name__130 string, body__131 Exp) Exp {
    var retv479 Exp
    var t480 Exp = Lam{
        _0: name__130,
        _1: body__131,
    }
    retv479 = t480
    return retv479
}

func exp_app(a__132 Exp, b__133 Exp) Exp {
    var retv482 Exp
    var t483 Exp = App{
        _0: a__132,
        _1: b__133,
    }
    retv482 = t483
    return retv482
}

func exp_let(name__134 string, a__135 Exp, b__136 Exp) Exp {
    var retv485 Exp
    var t486 Exp = Let{
        _0: name__134,
        _1: a__135,
        _2: b__136,
    }
    retv485 = t486
    return retv485
}

func show_result(label__137 string, res__138 Result__Typ__string) struct{} {
    switch res__138.(type) {
    case Result__Typ__string_Ok:
        var x161 Typ = res__138.(Result__Typ__string_Ok)._0
        var ty__139 Typ = x161
        var t489 string = label__137 + ": "
        var t490 string = typ_to_string(ty__139)
        var t491 string = t489 + t490
        println__T_string(t491)
    case Result__Typ__string_Err:
        var x162 string = res__138.(Result__Typ__string_Err)._0
        var e__140 string = x162
        var t493 string = label__137 + ": "
        var t494 string = t493 + e__140
        println__T_string(t494)
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func main0() struct{} {
    var st__141 CheckerState = state_new()
    var t497 Exp = exp_var("x")
    var id__142 Exp = exp_lam("x", t497)
    var t498 Exp = exp_var("x")
    var t499 Exp = exp_var("y")
    var t500 Exp = exp_app(t498, t499)
    var t501 Exp = exp_lam("y", t500)
    var c1__143 Exp = exp_lam("x", t501)
    reset_type_variables(st__141)
    var t502 []EnvEntry = env_empty()
    var t503 Result__Typ__string = typeof(st__141, t502, id__142)
    show_result("id", t503)
    reset_type_variables(st__141)
    var t504 []EnvEntry = env_empty()
    var t505 Result__Typ__string = typeof(st__141, t504, c1__143)
    show_result("c1", t505)
    reset_type_variables(st__141)
    var t506 []EnvEntry = env_empty()
    var t507 Exp = exp_var("x")
    var t508 Exp = exp_let("x", c1__143, t507)
    var t509 Result__Typ__string = typeof(st__141, t506, t508)
    show_result("let_x_c1_x", t509)
    reset_type_variables(st__141)
    var t510 []EnvEntry = env_empty()
    var t511 Exp = exp_var("z")
    var t512 Exp = exp_lam("z", t511)
    var t513 Exp = exp_var("y")
    var t514 Exp = exp_let("y", t512, t513)
    var t515 Result__Typ__string = typeof(st__141, t510, t514)
    show_result("let_y_id_y", t515)
    reset_type_variables(st__141)
    var t516 []EnvEntry = env_empty()
    var t517 Exp = exp_var("z")
    var t518 Exp = exp_lam("z", t517)
    var t519 Exp = exp_var("y")
    var t520 Exp = exp_let("y", t518, t519)
    var t521 Exp = exp_lam("x", t520)
    var t522 Result__Typ__string = typeof(st__141, t516, t521)
    show_result("lam_x_let_y_id_y", t522)
    reset_type_variables(st__141)
    var t523 []EnvEntry = env_empty()
    var t524 Exp = exp_var("z")
    var t525 Exp = exp_lam("z", t524)
    var t526 Exp = exp_var("y")
    var t527 Exp = exp_var("x")
    var t528 Exp = exp_app(t526, t527)
    var t529 Exp = exp_let("y", t525, t528)
    var t530 Exp = exp_lam("x", t529)
    var t531 Result__Typ__string = typeof(st__141, t523, t530)
    show_result("lam_x_let_y_id_yx", t531)
    reset_type_variables(st__141)
    var t532 []EnvEntry = env_empty()
    var t533 Exp = exp_var("x")
    var t534 Exp = exp_var("x")
    var t535 Exp = exp_app(t533, t534)
    var t536 Exp = exp_lam("x", t535)
    var t537 Result__Typ__string = typeof(st__141, t532, t536)
    show_result("self_apply", t537)
    reset_type_variables(st__141)
    var t538 []EnvEntry = env_empty()
    var t539 Exp = exp_var("x")
    var t540 Exp = exp_var("x")
    var t541 Exp = exp_let("x", t539, t540)
    var t542 Result__Typ__string = typeof(st__141, t538, t541)
    show_result("unbound_var", t542)
    reset_type_variables(st__141)
    var t543 []EnvEntry = env_empty()
    var t544 Exp = exp_var("y")
    var t545 Exp = exp_var("y")
    var t546 Exp = exp_var("z")
    var t547 Exp = exp_app(t545, t546)
    var t548 Exp = exp_lam("z", t547)
    var t549 Exp = exp_app(t544, t548)
    var t550 Exp = exp_lam("y", t549)
    var t551 Result__Typ__string = typeof(st__141, t543, t550)
    show_result("max_heiber", t551)
    reset_type_variables(st__141)
    var t552 []EnvEntry = env_empty()
    var t553 Exp = exp_var("k")
    var t554 Exp = exp_var("k")
    var t555 Exp = exp_var("x")
    var t556 Exp = exp_app(t554, t555)
    var t557 Exp = exp_var("y")
    var t558 Exp = exp_app(t556, t557)
    var t559 Exp = exp_app(t553, t558)
    var t560 Exp = exp_var("k")
    var t561 Exp = exp_var("y")
    var t562 Exp = exp_app(t560, t561)
    var t563 Exp = exp_var("x")
    var t564 Exp = exp_app(t562, t563)
    var t565 Exp = exp_app(t559, t564)
    var t566 Exp = exp_lam("k", t565)
    var t567 Exp = exp_lam("y", t566)
    var t568 Exp = exp_lam("x", t567)
    var t569 Result__Typ__string = typeof(st__141, t552, t568)
    show_result("kirang", t569)
    reset_type_variables(st__141)
    var t570 []EnvEntry = env_empty()
    var t571 Exp = exp_var("id")
    var t572 Exp = exp_var("id")
    var t573 Exp = exp_app(t571, t572)
    var t574 Exp = exp_let("id", id__142, t573)
    var t575 Result__Typ__string = typeof(st__141, t570, t574)
    show_result("let_id_idid", t575)
    reset_type_variables(st__141)
    var t576 []EnvEntry = env_empty()
    var t577 Exp = exp_var("x")
    var t578 Exp = exp_app(t577, id__142)
    var t579 Exp = exp_var("z")
    var t580 Exp = exp_let("z", t578, t579)
    var t581 Exp = exp_var("y")
    var t582 Exp = exp_let("y", t580, t581)
    var t583 Exp = exp_let("x", c1__143, t582)
    var t584 Result__Typ__string = typeof(st__141, t576, t583)
    show_result("nested_lets", t584)
    reset_type_variables(st__141)
    var t585 []EnvEntry = env_empty()
    var t586 Exp = exp_var("x")
    var t587 Exp = exp_var("y")
    var t588 Exp = exp_app(t586, t587)
    var t589 Exp = exp_var("y")
    var t590 Exp = exp_var("x")
    var t591 Exp = exp_app(t589, t590)
    var t592 Exp = exp_lam("x", t591)
    var t593 Exp = exp_let("x", t588, t592)
    var t594 Exp = exp_lam("y", t593)
    var t595 Exp = exp_lam("x", t594)
    var t596 Result__Typ__string = typeof(st__141, t585, t595)
    show_result("fun_x_fun_y_let_x_xy_fun_x_yx", t596)
    reset_type_variables(st__141)
    var t597 []EnvEntry = env_empty()
    var t598 Exp = exp_var("x")
    var t599 Exp = exp_var("y")
    var t600 Exp = exp_let("y", t598, t599)
    var t601 Exp = exp_lam("x", t600)
    var t602 Result__Typ__string = typeof(st__141, t597, t601)
    show_result("sound_gen_1", t602)
    reset_type_variables(st__141)
    var t603 []EnvEntry = env_empty()
    var t604 Exp = exp_var("x")
    var t605 Exp = exp_lam("z", t604)
    var t606 Exp = exp_var("y")
    var t607 Exp = exp_let("y", t605, t606)
    var t608 Exp = exp_lam("x", t607)
    var t609 Result__Typ__string = typeof(st__141, t603, t608)
    show_result("sound_gen_2", t609)
    reset_type_variables(st__141)
    var t610 []EnvEntry = env_empty()
    var t611 Exp = exp_var("x")
    var t612 Exp = exp_var("z")
    var t613 Exp = exp_app(t611, t612)
    var t614 Exp = exp_lam("z", t613)
    var t615 Exp = exp_var("y")
    var t616 Exp = exp_let("y", t614, t615)
    var t617 Exp = exp_lam("x", t616)
    var t618 Result__Typ__string = typeof(st__141, t610, t617)
    show_result("sound_gen_3", t618)
    reset_type_variables(st__141)
    var t619 []EnvEntry = env_empty()
    var t620 Exp = exp_var("x")
    var t621 Exp = exp_var("y")
    var t622 Exp = exp_app(t620, t621)
    var t623 Exp = exp_var("x")
    var t624 Exp = exp_var("y")
    var t625 Exp = exp_app(t623, t624)
    var t626 Exp = exp_let("x", t622, t625)
    var t627 Exp = exp_lam("y", t626)
    var t628 Exp = exp_lam("x", t627)
    var t629 Result__Typ__string = typeof(st__141, t619, t628)
    show_result("double_apply", t629)
    reset_type_variables(st__141)
    var t630 []EnvEntry = env_empty()
    var t631 Exp = exp_var("x")
    var t632 Exp = exp_var("y")
    var t633 Exp = exp_var("y")
    var t634 Exp = exp_app(t632, t633)
    var t635 Exp = exp_let("y", t631, t634)
    var t636 Exp = exp_lam("x", t635)
    var t637 Result__Typ__string = typeof(st__141, t630, t636)
    show_result("sound_gen_occurs", t637)
    reset_gensym(st__141)
    var t638 []EnvEntry = env_empty()
    var t639 Exp = exp_var("x")
    var t640 Exp = exp_app(t639, id__142)
    var t641 Exp = exp_var("z")
    var t642 Exp = exp_let("z", t640, t641)
    var t643 Exp = exp_var("y")
    var t644 Exp = exp_let("y", t642, t643)
    var t645 Exp = exp_lam("x", t644)
    var t646 Result__Typ__string = typeof(st__141, t638, t645)
    show_result("fun_x_let_y_let_z_x_id_z_y", t646)
    println__T_string("")
    println__T_string("All Done")
    println__T_string("")
    return struct{}{}
}

func _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_EnvEntry() []EnvEntry {
    var retv648 []EnvEntry
    var t649 []EnvEntry = nil
    retv648 = t649
    return retv648
}

func _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_len_x5f__x5f_T_x5f_EnvEntry(self__73 []EnvEntry) int32 {
    var retv651 int32
    var t652 int32 = int32(len(self__73))
    retv651 = t652
    return retv651
}

func _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_len_x5f__x5f_T_x5f_SubstEntry(self__73 []SubstEntry) int32 {
    var retv654 int32
    var t655 int32 = int32(len(self__73))
    retv654 = t655
    return retv654
}

func _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_push_x5f__x5f_T_x5f_SubstEntry(self__66 []SubstEntry, elem__67 SubstEntry) []SubstEntry {
    var retv657 []SubstEntry
    var t658 []SubstEntry = append(self__66, elem__67)
    retv657 = t658
    return retv657
}

func _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_new_x5f__x5f_T_x5f_SubstEntry() []SubstEntry {
    var retv660 []SubstEntry
    var t661 []SubstEntry = nil
    retv660 = t661
    return retv660
}

func _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_push_x5f__x5f_T_x5f_EnvEntry(self__66 []EnvEntry, elem__67 EnvEntry) []EnvEntry {
    var retv663 []EnvEntry
    var t664 []EnvEntry = append(self__66, elem__67)
    retv663 = t664
    return retv663
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
