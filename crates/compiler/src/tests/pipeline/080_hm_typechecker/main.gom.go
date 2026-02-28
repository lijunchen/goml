package main

import (
    "fmt"
    "unicode/utf8"
)

func char_to_string(x rune) string {
    if !utf8.ValidRune(x) {
        panic("invalid char")
    }
    return string(x)
}

func int32_to_string(x int32) string {
    return fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

type ref_int32_x struct {
    value int32
}

func ref__Ref_int32(value int32) *ref_int32_x {
    return &ref_int32_x{
        value: value,
    }
}

func ref_get__Ref_int32(reference *ref_int32_x) int32 {
    return reference.value
}

func ref_set__Ref_int32(reference *ref_int32_x, value int32) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_tv_x struct {
    value Tv
}

func ref__Ref_Tv(value Tv) *ref_tv_x {
    return &ref_tv_x{
        value: value,
    }
}

func ref_get__Ref_Tv(reference *ref_tv_x) Tv {
    return reference.value
}

func ref_set__Ref_Tv(reference *ref_tv_x, value Tv) struct{} {
    reference.value = value
    return struct{}{}
}

func ptr_eq__Ref_Tv(a *ref_tv_x, b *ref_tv_x) bool {
    return a == b
}

type ref_option__typ_x struct {
    value Option__Typ
}

func ref__Ref_Option__Typ(value Option__Typ) *ref_option__typ_x {
    return &ref_option__typ_x{
        value: value,
    }
}

func ref_get__Ref_Option__Typ(reference *ref_option__typ_x) Option__Typ {
    return reference.value
}

func ref_set__Ref_Option__Typ(reference *ref_option__typ_x, value Option__Typ) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_bool_x struct {
    value bool
}

func ref__Ref_bool(value bool) *ref_bool_x {
    return &ref_bool_x{
        value: value,
    }
}

func ref_get__Ref_bool(reference *ref_bool_x) bool {
    return reference.value
}

func ref_set__Ref_bool(reference *ref_bool_x, value bool) struct{} {
    reference.value = value
    return struct{}{}
}

type Tuple2_Typ_Typ struct {
    _0 Typ
    _1 Typ
}

type Tuple2_Typ_Vec_SubstEntry struct {
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
    _0 *ref_tv_x
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
    var t204 *ref_int32_x = ref__Ref_int32(0)
    var t205 *ref_int32_x = ref__Ref_int32(1)
    var t206 CheckerState = CheckerState{
        gensym_counter: t204,
        current_level: t205,
    }
    return t206
}

func reset_gensym(st__0 CheckerState) struct{} {
    var t207 *ref_int32_x = st__0.gensym_counter
    ref_set__Ref_int32(t207, 0)
    return struct{}{}
}

func reset_level(st__1 CheckerState) struct{} {
    var t208 *ref_int32_x = st__1.current_level
    ref_set__Ref_int32(t208, 1)
    return struct{}{}
}

func reset_type_variables(st__2 CheckerState) struct{} {
    reset_gensym(st__2)
    var t209 struct{} = reset_level(st__2)
    return t209
}

func enter_level(st__3 CheckerState) struct{} {
    var t210 *ref_int32_x = st__3.current_level
    var l__4 int32 = ref_get__Ref_int32(t210)
    var t211 *ref_int32_x = st__3.current_level
    var t212 int32 = l__4 + 1
    ref_set__Ref_int32(t211, t212)
    return struct{}{}
}

func leave_level(st__5 CheckerState) struct{} {
    var t213 *ref_int32_x = st__5.current_level
    var l__6 int32 = ref_get__Ref_int32(t213)
    var t214 *ref_int32_x = st__5.current_level
    var t215 int32 = l__6 - 1
    ref_set__Ref_int32(t214, t215)
    return struct{}{}
}

func min_i32(a__7 int32, b__8 int32) int32 {
    var t218 bool = a__7 < b__8
    var jp217 int32
    if t218 {
        jp217 = a__7
    } else {
        jp217 = b__8
    }
    return jp217
}

func nth_letter(n__9 int32) rune {
    var jp220 rune
    switch n__9 {
    case 0:
        jp220 = 97
    case 1:
        jp220 = 98
    case 2:
        jp220 = 99
    case 3:
        jp220 = 100
    case 4:
        jp220 = 101
    case 5:
        jp220 = 102
    case 6:
        jp220 = 103
    case 7:
        jp220 = 104
    case 8:
        jp220 = 105
    case 9:
        jp220 = 106
    case 10:
        jp220 = 107
    case 11:
        jp220 = 108
    case 12:
        jp220 = 109
    case 13:
        jp220 = 110
    case 14:
        jp220 = 111
    case 15:
        jp220 = 112
    case 16:
        jp220 = 113
    case 17:
        jp220 = 114
    case 18:
        jp220 = 115
    case 19:
        jp220 = 116
    case 20:
        jp220 = 117
    case 21:
        jp220 = 118
    case 22:
        jp220 = 119
    case 23:
        jp220 = 120
    case 24:
        jp220 = 121
    case 25:
        jp220 = 122
    default:
        jp220 = 97
    }
    return jp220
}

func gensym(st__10 CheckerState) string {
    var t221 *ref_int32_x = st__10.gensym_counter
    var n__11 int32 = ref_get__Ref_int32(t221)
    var t222 *ref_int32_x = st__10.gensym_counter
    var t223 int32 = n__11 + 1
    ref_set__Ref_int32(t222, t223)
    var t226 bool = n__11 < 26
    var jp225 string
    if t226 {
        var t227 rune = nth_letter(n__11)
        var t228 string = char_to_string(t227)
        jp225 = t228
    } else {
        var t229 string = int32_to_string(n__11)
        var t230 string = "t" + t229
        jp225 = t230
    }
    return jp225
}

func newvar(st__12 CheckerState) Typ {
    var name__13 string = gensym(st__12)
    var t231 *ref_int32_x = st__12.current_level
    var level__14 int32 = ref_get__Ref_int32(t231)
    var t232 Tv = Unbound{
        _0: name__13,
        _1: level__14,
    }
    var t233 *ref_tv_x = ref__Ref_Tv(t232)
    var t234 Typ = TVar{
        _0: t233,
    }
    return t234
}

func typ_is_arrow(ty__15 Typ) bool {
    var jp236 bool
    switch ty__15.(type) {
    case TVar:
        var x6 *ref_tv_x = ty__15.(TVar)._0
        var tvref__16 *ref_tv_x = x6
        var mtmp10 Tv = ref_get__Ref_Tv(tvref__16)
        var jp238 bool
        switch mtmp10.(type) {
        case Unbound:
            jp238 = false
        case Link:
            var x13 Typ = mtmp10.(Link)._0
            var inner__17 Typ = x13
            var t239 bool = typ_is_arrow(inner__17)
            jp238 = t239
        default:
            panic("non-exhaustive match")
        }
        jp236 = jp238
        return jp236
    case QVar:
        jp236 = false
        return jp236
    case TArrow:
        jp236 = true
        return jp236
    default:
        panic("non-exhaustive match")
    }
}

func typ_to_string(ty__18 Typ) string {
    var jp241 string
    switch ty__18.(type) {
    case TVar:
        var x14 *ref_tv_x = ty__18.(TVar)._0
        var tvref__20 *ref_tv_x = x14
        var mtmp18 Tv = ref_get__Ref_Tv(tvref__20)
        var jp243 string
        switch mtmp18.(type) {
        case Unbound:
            var x19 string = mtmp18.(Unbound)._0
            var name__21 string = x19
            var t244 string = "'" + name__21
            jp243 = t244
        case Link:
            var x21 Typ = mtmp18.(Link)._0
            var inner__22 Typ = x21
            var t245 string = typ_to_string(inner__22)
            jp243 = t245
        default:
            panic("non-exhaustive match")
        }
        jp241 = jp243
        return jp241
    case QVar:
        var x15 string = ty__18.(QVar)._0
        var name__19 string = x15
        var t246 string = "'" + name__19
        jp241 = t246
        return jp241
    case TArrow:
        var x16 Typ = ty__18.(TArrow)._0
        var x17 Typ = ty__18.(TArrow)._1
        var t2__24 Typ = x17
        var t1__23 Typ = x16
        var t251 bool = typ_is_arrow(t1__23)
        var jp248 string
        if t251 {
            var t252 string = typ_to_string(t1__23)
            var t253 string = "(" + t252
            var t254 string = t253 + ")"
            jp248 = t254
        } else {
            var t255 string = typ_to_string(t1__23)
            jp248 = t255
        }
        var s1__25 string = jp248
        var s2__26 string = typ_to_string(t2__24)
        var t249 string = s1__25 + " -> "
        var t250 string = t249 + s2__26
        jp241 = t250
        return jp241
    default:
        panic("non-exhaustive match")
    }
}

func env_empty() []EnvEntry {
    var env__27 []EnvEntry = nil
    return env__27
}

func env_lookup(env__28 []EnvEntry, name__29 string) Option__Typ {
    var t256 int32 = int32(len(env__28))
    var t257 int32 = t256 - 1
    var i__30 *ref_int32_x = ref__Ref_int32(t257)
    var found__31 *ref_option__typ_x = ref__Ref_Option__Typ(None{})
    var done__32 *ref_bool_x = ref__Ref_bool(false)
    for {
        var t261 bool = ref_get__Ref_bool(done__32)
        var t262 bool = !t261
        var t263 int32 = ref_get__Ref_int32(i__30)
        var t264 bool = t263 >= 0
        var t265 bool = t262 && t264
        if !t265 {
            break
        }
        var t266 int32 = ref_get__Ref_int32(i__30)
        var entry__33 EnvEntry = env__28[t266]
        var t268 string = entry__33.name
        var t269 bool = t268 == name__29
        if t269 {
            var t270 Typ = entry__33.ty
            var t271 Option__Typ = Some{
                _0: t270,
            }
            ref_set__Ref_Option__Typ(found__31, t271)
            ref_set__Ref_bool(done__32, true)
        } else {
            var t273 int32 = ref_get__Ref_int32(i__30)
            var t274 int32 = t273 - 1
            ref_set__Ref_int32(i__30, t274)
        }
        continue
    }
    var t259 Option__Typ = ref_get__Ref_Option__Typ(found__31)
    return t259
}

func subst_lookup(subst__34 []SubstEntry, name__35 string) Option__Typ {
    var t276 int32 = int32(len(subst__34))
    var t277 int32 = t276 - 1
    var i__36 *ref_int32_x = ref__Ref_int32(t277)
    var found__37 *ref_option__typ_x = ref__Ref_Option__Typ(None{})
    var done__38 *ref_bool_x = ref__Ref_bool(false)
    for {
        var t281 bool = ref_get__Ref_bool(done__38)
        var t282 bool = !t281
        var t283 int32 = ref_get__Ref_int32(i__36)
        var t284 bool = t283 >= 0
        var t285 bool = t282 && t284
        if !t285 {
            break
        }
        var t286 int32 = ref_get__Ref_int32(i__36)
        var entry__39 SubstEntry = subst__34[t286]
        var t288 string = entry__39.name
        var t289 bool = t288 == name__35
        if t289 {
            var t290 Typ = entry__39.ty
            var t291 Option__Typ = Some{
                _0: t290,
            }
            ref_set__Ref_Option__Typ(found__37, t291)
            ref_set__Ref_bool(done__38, true)
        } else {
            var t293 int32 = ref_get__Ref_int32(i__36)
            var t294 int32 = t293 - 1
            ref_set__Ref_int32(i__36, t294)
        }
        continue
    }
    var t279 Option__Typ = ref_get__Ref_Option__Typ(found__37)
    return t279
}

func occurs(st__40 CheckerState, tvr__41 *ref_tv_x, ty__42 Typ) Result__unit__string {
    var jp297 Result__unit__string
    switch ty__42.(type) {
    case TVar:
        var x26 *ref_tv_x = ty__42.(TVar)._0
        var tvr2__43 *ref_tv_x = x26
        var t300 bool = ptr_eq__Ref_Tv(tvr__41, tvr2__43)
        var jp299 Result__unit__string
        if t300 {
            var t301 Result__unit__string = Result__unit__string_Err{
                _0: "occurs check",
            }
            jp299 = t301
            jp297 = jp299
            return jp297
        } else {
            var mtmp30 Tv = ref_get__Ref_Tv(tvr2__43)
            var jp303 Result__unit__string
            switch mtmp30.(type) {
            case Unbound:
                var x31 string = mtmp30.(Unbound)._0
                var x32 int32 = mtmp30.(Unbound)._1
                var l2__45 int32 = x32
                var name__44 string = x31
                var mtmp34 Tv = ref_get__Ref_Tv(tvr__41)
                var jp305 int32
                switch mtmp34.(type) {
                case Unbound:
                    var x36 int32 = mtmp34.(Unbound)._1
                    var l__46 int32 = x36
                    var t308 int32 = min_i32(l__46, l2__45)
                    jp305 = t308
                case Link:
                    jp305 = l2__45
                default:
                    panic("non-exhaustive match")
                }
                var min_level__47 int32 = jp305
                var t306 Tv = Unbound{
                    _0: name__44,
                    _1: min_level__47,
                }
                ref_set__Ref_Tv(tvr2__43, t306)
                var t307 Result__unit__string = Result__unit__string_Ok{
                    _0: struct{}{},
                }
                jp303 = t307
                jp299 = jp303
                jp297 = jp299
                return jp297
            case Link:
                var x33 Typ = mtmp30.(Link)._0
                var inner__48 Typ = x33
                var t309 Result__unit__string = occurs(st__40, tvr__41, inner__48)
                jp303 = t309
                jp299 = jp303
                jp297 = jp299
                return jp297
            default:
                panic("non-exhaustive match")
            }
        }
    case QVar:
        var t310 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp297 = t310
        return jp297
    case TArrow:
        var x28 Typ = ty__42.(TArrow)._0
        var x29 Typ = ty__42.(TArrow)._1
        var t2__50 Typ = x29
        var t1__49 Typ = x28
        var mtmp39 Result__unit__string = occurs(st__40, tvr__41, t1__49)
        var jp312 Result__unit__string
        switch mtmp39.(type) {
        case Result__unit__string_Ok:
            var t313 Result__unit__string = occurs(st__40, tvr__41, t2__50)
            jp312 = t313
        case Result__unit__string_Err:
            var x41 string = mtmp39.(Result__unit__string_Err)._0
            var e__51 string = x41
            var t314 Result__unit__string = Result__unit__string_Err{
                _0: e__51,
            }
            jp312 = t314
        default:
            panic("non-exhaustive match")
        }
        jp297 = jp312
        return jp297
    default:
        panic("non-exhaustive match")
    }
}

func unify(st__52 CheckerState, t1__53 Typ, t2__54 Typ) Result__unit__string {
    var mtmp42 Tuple2_Typ_Typ = Tuple2_Typ_Typ{
        _0: t1__53,
        _1: t2__54,
    }
    var x43 Typ = mtmp42._0
    var x44 Typ = mtmp42._1
    var jp316 Result__unit__string
    switch x44.(type) {
    case TVar:
        var x45 *ref_tv_x = x44.(TVar)._0
        var jp318 Result__unit__string
        switch x43.(type) {
        case TVar:
            var x49 *ref_tv_x = x43.(TVar)._0
            var r1__55 *ref_tv_x = x49
            var r2__56 *ref_tv_x = x45
            var t321 bool = ptr_eq__Ref_Tv(r1__55, r2__56)
            var jp320 Result__unit__string
            if t321 {
                var t322 Result__unit__string = Result__unit__string_Ok{
                    _0: struct{}{},
                }
                jp320 = t322
                jp318 = jp320
                jp316 = jp318
                return jp316
            } else {
                var mtmp53 Tv = ref_get__Ref_Tv(r1__55)
                var jp324 Result__unit__string
                switch mtmp53.(type) {
                case Unbound:
                    var mtmp57 Tv = ref_get__Ref_Tv(r2__56)
                    var jp326 Result__unit__string
                    switch mtmp57.(type) {
                    case Unbound:
                        var t327 Typ = TVar{
                            _0: r2__56,
                        }
                        var mtmp61 Result__unit__string = occurs(st__52, r1__55, t327)
                        var jp329 Result__unit__string
                        switch mtmp61.(type) {
                        case Result__unit__string_Ok:
                            var t330 Typ = TVar{
                                _0: r2__56,
                            }
                            var t331 Tv = Link{
                                _0: t330,
                            }
                            ref_set__Ref_Tv(r1__55, t331)
                            var t332 Result__unit__string = Result__unit__string_Ok{
                                _0: struct{}{},
                            }
                            jp329 = t332
                        case Result__unit__string_Err:
                            var x63 string = mtmp61.(Result__unit__string_Err)._0
                            var e__59 string = x63
                            var t333 Result__unit__string = Result__unit__string_Err{
                                _0: e__59,
                            }
                            jp329 = t333
                        default:
                            panic("non-exhaustive match")
                        }
                        jp326 = jp329
                        jp324 = jp326
                        jp320 = jp324
                        jp318 = jp320
                        jp316 = jp318
                        return jp316
                    case Link:
                        var x60 Typ = mtmp57.(Link)._0
                        var inner__58 Typ = x60
                        var t334 Typ = TVar{
                            _0: r1__55,
                        }
                        var t335 Result__unit__string = unify(st__52, t334, inner__58)
                        jp326 = t335
                        jp324 = jp326
                        jp320 = jp324
                        jp318 = jp320
                        jp316 = jp318
                        return jp316
                    default:
                        panic("non-exhaustive match")
                    }
                case Link:
                    var x56 Typ = mtmp53.(Link)._0
                    var inner__57 Typ = x56
                    var t336 Typ = TVar{
                        _0: r2__56,
                    }
                    var t337 Result__unit__string = unify(st__52, inner__57, t336)
                    jp324 = t337
                    jp320 = jp324
                    jp318 = jp320
                    jp316 = jp318
                    return jp316
                default:
                    panic("non-exhaustive match")
                }
            }
        case QVar:
            var r2__65 *ref_tv_x = x45
            var other__64 Typ = x43
            var mtmp65 Tv = ref_get__Ref_Tv(r2__65)
            var jp339 Result__unit__string
            switch mtmp65.(type) {
            case Unbound:
                var mtmp69 Result__unit__string = occurs(st__52, r2__65, other__64)
                var jp341 Result__unit__string
                switch mtmp69.(type) {
                case Result__unit__string_Ok:
                    var t342 Tv = Link{
                        _0: other__64,
                    }
                    ref_set__Ref_Tv(r2__65, t342)
                    var t343 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    jp341 = t343
                case Result__unit__string_Err:
                    var x71 string = mtmp69.(Result__unit__string_Err)._0
                    var e__67 string = x71
                    var t344 Result__unit__string = Result__unit__string_Err{
                        _0: e__67,
                    }
                    jp341 = t344
                default:
                    panic("non-exhaustive match")
                }
                jp339 = jp341
                jp318 = jp339
                jp316 = jp318
                return jp316
            case Link:
                var x68 Typ = mtmp65.(Link)._0
                var inner__66 Typ = x68
                var t345 Result__unit__string = unify(st__52, other__64, inner__66)
                jp339 = t345
                jp318 = jp339
                jp316 = jp318
                return jp316
            default:
                panic("non-exhaustive match")
            }
        case TArrow:
            var r2__65 *ref_tv_x = x45
            var other__64 Typ = x43
            var mtmp73 Tv = ref_get__Ref_Tv(r2__65)
            var jp347 Result__unit__string
            switch mtmp73.(type) {
            case Unbound:
                var mtmp77 Result__unit__string = occurs(st__52, r2__65, other__64)
                var jp349 Result__unit__string
                switch mtmp77.(type) {
                case Result__unit__string_Ok:
                    var t350 Tv = Link{
                        _0: other__64,
                    }
                    ref_set__Ref_Tv(r2__65, t350)
                    var t351 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    jp349 = t351
                case Result__unit__string_Err:
                    var x79 string = mtmp77.(Result__unit__string_Err)._0
                    var e__67 string = x79
                    var t352 Result__unit__string = Result__unit__string_Err{
                        _0: e__67,
                    }
                    jp349 = t352
                default:
                    panic("non-exhaustive match")
                }
                jp347 = jp349
                jp318 = jp347
                jp316 = jp318
                return jp316
            case Link:
                var x76 Typ = mtmp73.(Link)._0
                var inner__66 Typ = x76
                var t353 Result__unit__string = unify(st__52, other__64, inner__66)
                jp347 = t353
                jp318 = jp347
                jp316 = jp318
                return jp316
            default:
                panic("non-exhaustive match")
            }
        default:
            panic("non-exhaustive match")
        }
    case QVar:
        var jp355 Result__unit__string
        switch x43.(type) {
        case TVar:
            var x81 *ref_tv_x = x43.(TVar)._0
            var r1__60 *ref_tv_x = x81
            var other__61 Typ = x44
            var mtmp85 Tv = ref_get__Ref_Tv(r1__60)
            var jp357 Result__unit__string
            switch mtmp85.(type) {
            case Unbound:
                var mtmp89 Result__unit__string = occurs(st__52, r1__60, other__61)
                var jp359 Result__unit__string
                switch mtmp89.(type) {
                case Result__unit__string_Ok:
                    var t360 Tv = Link{
                        _0: other__61,
                    }
                    ref_set__Ref_Tv(r1__60, t360)
                    var t361 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    jp359 = t361
                case Result__unit__string_Err:
                    var x91 string = mtmp89.(Result__unit__string_Err)._0
                    var e__63 string = x91
                    var t362 Result__unit__string = Result__unit__string_Err{
                        _0: e__63,
                    }
                    jp359 = t362
                default:
                    panic("non-exhaustive match")
                }
                jp357 = jp359
                jp355 = jp357
                jp316 = jp355
                return jp316
            case Link:
                var x88 Typ = mtmp85.(Link)._0
                var inner__62 Typ = x88
                var t363 Result__unit__string = unify(st__52, inner__62, other__61)
                jp357 = t363
                jp355 = jp357
                jp316 = jp355
                return jp316
            default:
                panic("non-exhaustive match")
            }
        case QVar:
            var t364 Result__unit__string = Result__unit__string_Err{
                _0: "unify error",
            }
            jp355 = t364
            jp316 = jp355
            return jp316
        case TArrow:
            var t365 Result__unit__string = Result__unit__string_Err{
                _0: "unify error",
            }
            jp355 = t365
            jp316 = jp355
            return jp316
        default:
            panic("non-exhaustive match")
        }
    case TArrow:
        var x47 Typ = x44.(TArrow)._0
        var x48 Typ = x44.(TArrow)._1
        var jp367 Result__unit__string
        switch x43.(type) {
        case TVar:
            var x93 *ref_tv_x = x43.(TVar)._0
            var r1__60 *ref_tv_x = x93
            var other__61 Typ = x44
            var mtmp97 Tv = ref_get__Ref_Tv(r1__60)
            var jp369 Result__unit__string
            switch mtmp97.(type) {
            case Unbound:
                var mtmp101 Result__unit__string = occurs(st__52, r1__60, other__61)
                var jp371 Result__unit__string
                switch mtmp101.(type) {
                case Result__unit__string_Ok:
                    var t372 Tv = Link{
                        _0: other__61,
                    }
                    ref_set__Ref_Tv(r1__60, t372)
                    var t373 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    jp371 = t373
                case Result__unit__string_Err:
                    var x103 string = mtmp101.(Result__unit__string_Err)._0
                    var e__63 string = x103
                    var t374 Result__unit__string = Result__unit__string_Err{
                        _0: e__63,
                    }
                    jp371 = t374
                default:
                    panic("non-exhaustive match")
                }
                jp369 = jp371
                jp367 = jp369
                jp316 = jp367
                return jp316
            case Link:
                var x100 Typ = mtmp97.(Link)._0
                var inner__62 Typ = x100
                var t375 Result__unit__string = unify(st__52, inner__62, other__61)
                jp369 = t375
                jp367 = jp369
                jp316 = jp367
                return jp316
            default:
                panic("non-exhaustive match")
            }
        case QVar:
            var t376 Result__unit__string = Result__unit__string_Err{
                _0: "unify error",
            }
            jp367 = t376
            jp316 = jp367
            return jp316
        case TArrow:
            var x95 Typ = x43.(TArrow)._0
            var x96 Typ = x43.(TArrow)._1
            var a2__69 Typ = x96
            var a1__68 Typ = x95
            var b2__71 Typ = x48
            var b1__70 Typ = x47
            var mtmp105 Result__unit__string = unify(st__52, a1__68, b1__70)
            var jp378 Result__unit__string
            switch mtmp105.(type) {
            case Result__unit__string_Ok:
                var t379 Result__unit__string = unify(st__52, a2__69, b2__71)
                jp378 = t379
            case Result__unit__string_Err:
                var x107 string = mtmp105.(Result__unit__string_Err)._0
                var e__72 string = x107
                var t380 Result__unit__string = Result__unit__string_Err{
                    _0: e__72,
                }
                jp378 = t380
            default:
                panic("non-exhaustive match")
            }
            jp367 = jp378
            jp316 = jp367
            return jp316
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func gen(st__73 CheckerState, ty__74 Typ) Typ {
    var jp382 Typ
    switch ty__74.(type) {
    case TVar:
        var x108 *ref_tv_x = ty__74.(TVar)._0
        var tvref__75 *ref_tv_x = x108
        var mtmp112 Tv = ref_get__Ref_Tv(tvref__75)
        var jp384 Typ
        switch mtmp112.(type) {
        case Unbound:
            var x113 string = mtmp112.(Unbound)._0
            var x114 int32 = mtmp112.(Unbound)._1
            var l__77 int32 = x114
            var name__76 string = x113
            var t385 *ref_int32_x = st__73.current_level
            var cur__78 int32 = ref_get__Ref_int32(t385)
            var t388 bool = l__77 > cur__78
            var jp387 Typ
            if t388 {
                var t389 Typ = QVar{
                    _0: name__76,
                }
                jp387 = t389
            } else {
                var t390 Typ = TVar{
                    _0: tvref__75,
                }
                jp387 = t390
            }
            jp384 = jp387
            jp382 = jp384
            return jp382
        case Link:
            var x115 Typ = mtmp112.(Link)._0
            var inner__79 Typ = x115
            var t391 Typ = gen(st__73, inner__79)
            jp384 = t391
            jp382 = jp384
            return jp382
        default:
            panic("non-exhaustive match")
        }
    case QVar:
        var other__82 Typ = ty__74
        jp382 = other__82
        return jp382
    case TArrow:
        var x110 Typ = ty__74.(TArrow)._0
        var x111 Typ = ty__74.(TArrow)._1
        var t2__81 Typ = x111
        var t1__80 Typ = x110
        var t392 Typ = gen(st__73, t1__80)
        var t393 Typ = gen(st__73, t2__81)
        var t394 Typ = TArrow{
            _0: t392,
            _1: t393,
        }
        jp382 = t394
        return jp382
    default:
        panic("non-exhaustive match")
    }
}

func inst_loop(st__83 CheckerState, subst__84 []SubstEntry, ty__85 Typ) Tuple2_Typ_Vec_SubstEntry {
    var jp396 Tuple2_Typ_Vec_SubstEntry
    switch ty__85.(type) {
    case TVar:
        var x116 *ref_tv_x = ty__85.(TVar)._0
        var tvref__90 *ref_tv_x = x116
        var mtmp120 Tv = ref_get__Ref_Tv(tvref__90)
        var jp398 Tuple2_Typ_Vec_SubstEntry
        switch mtmp120.(type) {
        case Unbound:
            var t399 Typ = TVar{
                _0: tvref__90,
            }
            var t400 Tuple2_Typ_Vec_SubstEntry = Tuple2_Typ_Vec_SubstEntry{
                _0: t399,
                _1: subst__84,
            }
            jp398 = t400
        case Link:
            var x123 Typ = mtmp120.(Link)._0
            var inner__91 Typ = x123
            var t401 Tuple2_Typ_Vec_SubstEntry = inst_loop(st__83, subst__84, inner__91)
            jp398 = t401
        default:
            panic("non-exhaustive match")
        }
        jp396 = jp398
        return jp396
    case QVar:
        var x117 string = ty__85.(QVar)._0
        var name__86 string = x117
        var mtmp124 Option__Typ = subst_lookup(subst__84, name__86)
        var jp403 Tuple2_Typ_Vec_SubstEntry
        switch mtmp124.(type) {
        case None:
            var tv__88 Typ = newvar(st__83)
            var t404 SubstEntry = SubstEntry{
                name: name__86,
                ty: tv__88,
            }
            var new_subst__89 []SubstEntry = append(subst__84, t404)
            var t405 Tuple2_Typ_Vec_SubstEntry = Tuple2_Typ_Vec_SubstEntry{
                _0: tv__88,
                _1: new_subst__89,
            }
            jp403 = t405
        case Some:
            var x125 Typ = mtmp124.(Some)._0
            var t__87 Typ = x125
            var t406 Tuple2_Typ_Vec_SubstEntry = Tuple2_Typ_Vec_SubstEntry{
                _0: t__87,
                _1: subst__84,
            }
            jp403 = t406
        default:
            panic("non-exhaustive match")
        }
        jp396 = jp403
        return jp396
    case TArrow:
        var x118 Typ = ty__85.(TArrow)._0
        var x119 Typ = ty__85.(TArrow)._1
        var t2__93 Typ = x119
        var t1__92 Typ = x118
        var mtmp126 Tuple2_Typ_Vec_SubstEntry = inst_loop(st__83, subst__84, t1__92)
        var x127 Typ = mtmp126._0
        var x128 []SubstEntry = mtmp126._1
        var subst1__95 []SubstEntry = x128
        var ty1__94 Typ = x127
        var mtmp129 Tuple2_Typ_Vec_SubstEntry = inst_loop(st__83, subst1__95, t2__93)
        var x130 Typ = mtmp129._0
        var x131 []SubstEntry = mtmp129._1
        var subst2__97 []SubstEntry = x131
        var ty2__96 Typ = x130
        var t407 Typ = TArrow{
            _0: ty1__94,
            _1: ty2__96,
        }
        var t408 Tuple2_Typ_Vec_SubstEntry = Tuple2_Typ_Vec_SubstEntry{
            _0: t407,
            _1: subst2__97,
        }
        jp396 = t408
        return jp396
    default:
        panic("non-exhaustive match")
    }
}

func inst(st__98 CheckerState, ty__99 Typ) Typ {
    var subst0__100 []SubstEntry = nil
    var mtmp132 Tuple2_Typ_Vec_SubstEntry = inst_loop(st__98, subst0__100, ty__99)
    var x133 Typ = mtmp132._0
    var t__101 Typ = x133
    return t__101
}

func typeof(st__102 CheckerState, env__103 []EnvEntry, e__104 Exp) Result__Typ__string {
    var jp410 Result__Typ__string
    switch e__104.(type) {
    case Var:
        var x135 string = e__104.(Var)._0
        var x__105 string = x135
        var mtmp143 Option__Typ = env_lookup(env__103, x__105)
        var jp412 Result__Typ__string
        switch mtmp143.(type) {
        case None:
            var t413 Result__Typ__string = Result__Typ__string_Err{
                _0: "unbound var",
            }
            jp412 = t413
        case Some:
            var x144 Typ = mtmp143.(Some)._0
            var ty__106 Typ = x144
            var t414 Typ = inst(st__102, ty__106)
            var t415 Result__Typ__string = Result__Typ__string_Ok{
                _0: t414,
            }
            jp412 = t415
        default:
            panic("non-exhaustive match")
        }
        jp410 = jp412
        return jp410
    case App:
        var x136 Exp = e__104.(App)._0
        var x137 Exp = e__104.(App)._1
        var e2__114 Exp = x137
        var e1__113 Exp = x136
        var mtmp145 Result__Typ__string = typeof(st__102, env__103, e1__113)
        var jp417 Result__Typ__string
        switch mtmp145.(type) {
        case Result__Typ__string_Ok:
            var x146 Typ = mtmp145.(Result__Typ__string_Ok)._0
            var ty_fun__116 Typ = x146
            var mtmp148 Result__Typ__string = typeof(st__102, env__103, e2__114)
            var jp419 Result__Typ__string
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
                var jp421 Result__Typ__string
                switch mtmp151.(type) {
                case Result__unit__string_Ok:
                    var t422 Result__Typ__string = Result__Typ__string_Ok{
                        _0: ty_res__119,
                    }
                    jp421 = t422
                case Result__unit__string_Err:
                    var x153 string = mtmp151.(Result__unit__string_Err)._0
                    var e__121 string = x153
                    var t423 Result__Typ__string = Result__Typ__string_Err{
                        _0: e__121,
                    }
                    jp421 = t423
                default:
                    panic("non-exhaustive match")
                }
                jp419 = jp421
                jp417 = jp419
                jp410 = jp417
                return jp410
            case Result__Typ__string_Err:
                var x150 string = mtmp148.(Result__Typ__string_Err)._0
                var e__117 string = x150
                var t424 Result__Typ__string = Result__Typ__string_Err{
                    _0: e__117,
                }
                jp419 = t424
                jp417 = jp419
                jp410 = jp417
                return jp410
            default:
                panic("non-exhaustive match")
            }
        case Result__Typ__string_Err:
            var x147 string = mtmp145.(Result__Typ__string_Err)._0
            var e__115 string = x147
            var t425 Result__Typ__string = Result__Typ__string_Err{
                _0: e__115,
            }
            jp417 = t425
            jp410 = jp417
            return jp410
        default:
            panic("non-exhaustive match")
        }
    case Lam:
        var x138 string = e__104.(Lam)._0
        var x139 Exp = e__104.(Lam)._1
        var body__108 Exp = x139
        var x__107 string = x138
        var ty_x__109 Typ = newvar(st__102)
        var t426 EnvEntry = EnvEntry{
            name: x__107,
            ty: ty_x__109,
        }
        var env2__110 []EnvEntry = append(env__103, t426)
        var mtmp154 Result__Typ__string = typeof(st__102, env2__110, body__108)
        var jp428 Result__Typ__string
        switch mtmp154.(type) {
        case Result__Typ__string_Ok:
            var x155 Typ = mtmp154.(Result__Typ__string_Ok)._0
            var ty_e__111 Typ = x155
            var t429 Typ = TArrow{
                _0: ty_x__109,
                _1: ty_e__111,
            }
            var t430 Result__Typ__string = Result__Typ__string_Ok{
                _0: t429,
            }
            jp428 = t430
        case Result__Typ__string_Err:
            var x156 string = mtmp154.(Result__Typ__string_Err)._0
            var e__112 string = x156
            var t431 Result__Typ__string = Result__Typ__string_Err{
                _0: e__112,
            }
            jp428 = t431
        default:
            panic("non-exhaustive match")
        }
        jp410 = jp428
        return jp410
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
        var jp433 Result__Typ__string
        switch ty_e__125.(type) {
        case Result__Typ__string_Ok:
            var x159 Typ = ty_e__125.(Result__Typ__string_Ok)._0
            var ty1__127 Typ = x159
            var t434 Typ = gen(st__102, ty1__127)
            var t435 EnvEntry = EnvEntry{
                name: x__122,
                ty: t434,
            }
            var env2__128 []EnvEntry = append(env__103, t435)
            var t436 Result__Typ__string = typeof(st__102, env2__128, e2__124)
            jp433 = t436
        case Result__Typ__string_Err:
            var x160 string = ty_e__125.(Result__Typ__string_Err)._0
            var e__126 string = x160
            var t437 Result__Typ__string = Result__Typ__string_Err{
                _0: e__126,
            }
            jp433 = t437
        default:
            panic("non-exhaustive match")
        }
        jp410 = jp433
        return jp410
    default:
        panic("non-exhaustive match")
    }
}

func exp_var(name__129 string) Exp {
    var t438 Exp = Var{
        _0: name__129,
    }
    return t438
}

func exp_lam(name__130 string, body__131 Exp) Exp {
    var t439 Exp = Lam{
        _0: name__130,
        _1: body__131,
    }
    return t439
}

func exp_app(a__132 Exp, b__133 Exp) Exp {
    var t440 Exp = App{
        _0: a__132,
        _1: b__133,
    }
    return t440
}

func exp_let(name__134 string, a__135 Exp, b__136 Exp) Exp {
    var t441 Exp = Let{
        _0: name__134,
        _1: a__135,
        _2: b__136,
    }
    return t441
}

func show_result(label__137 string, res__138 Result__Typ__string) struct{} {
    switch res__138.(type) {
    case Result__Typ__string_Ok:
        var x161 Typ = res__138.(Result__Typ__string_Ok)._0
        var ty__139 Typ = x161
        var t443 string = label__137 + ": "
        var t444 string = typ_to_string(ty__139)
        var t445 string = t443 + t444
        string_println(t445)
    case Result__Typ__string_Err:
        var x162 string = res__138.(Result__Typ__string_Err)._0
        var e__140 string = x162
        var t447 string = label__137 + ": "
        var t448 string = t447 + e__140
        string_println(t448)
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func main0() struct{} {
    var st__141 CheckerState = state_new()
    var t450 Exp = exp_var("x")
    var id__142 Exp = exp_lam("x", t450)
    var t451 Exp = exp_var("x")
    var t452 Exp = exp_var("y")
    var t453 Exp = exp_app(t451, t452)
    var t454 Exp = exp_lam("y", t453)
    var c1__143 Exp = exp_lam("x", t454)
    reset_type_variables(st__141)
    var t455 []EnvEntry = env_empty()
    var t456 Result__Typ__string = typeof(st__141, t455, id__142)
    show_result("id", t456)
    reset_type_variables(st__141)
    var t457 []EnvEntry = env_empty()
    var t458 Result__Typ__string = typeof(st__141, t457, c1__143)
    show_result("c1", t458)
    reset_type_variables(st__141)
    var t459 []EnvEntry = env_empty()
    var t460 Exp = exp_var("x")
    var t461 Exp = exp_let("x", c1__143, t460)
    var t462 Result__Typ__string = typeof(st__141, t459, t461)
    show_result("let_x_c1_x", t462)
    reset_type_variables(st__141)
    var t463 []EnvEntry = env_empty()
    var t464 Exp = exp_var("z")
    var t465 Exp = exp_lam("z", t464)
    var t466 Exp = exp_var("y")
    var t467 Exp = exp_let("y", t465, t466)
    var t468 Result__Typ__string = typeof(st__141, t463, t467)
    show_result("let_y_id_y", t468)
    reset_type_variables(st__141)
    var t469 []EnvEntry = env_empty()
    var t470 Exp = exp_var("z")
    var t471 Exp = exp_lam("z", t470)
    var t472 Exp = exp_var("y")
    var t473 Exp = exp_let("y", t471, t472)
    var t474 Exp = exp_lam("x", t473)
    var t475 Result__Typ__string = typeof(st__141, t469, t474)
    show_result("lam_x_let_y_id_y", t475)
    reset_type_variables(st__141)
    var t476 []EnvEntry = env_empty()
    var t477 Exp = exp_var("z")
    var t478 Exp = exp_lam("z", t477)
    var t479 Exp = exp_var("y")
    var t480 Exp = exp_var("x")
    var t481 Exp = exp_app(t479, t480)
    var t482 Exp = exp_let("y", t478, t481)
    var t483 Exp = exp_lam("x", t482)
    var t484 Result__Typ__string = typeof(st__141, t476, t483)
    show_result("lam_x_let_y_id_yx", t484)
    reset_type_variables(st__141)
    var t485 []EnvEntry = env_empty()
    var t486 Exp = exp_var("x")
    var t487 Exp = exp_var("x")
    var t488 Exp = exp_app(t486, t487)
    var t489 Exp = exp_lam("x", t488)
    var t490 Result__Typ__string = typeof(st__141, t485, t489)
    show_result("self_apply", t490)
    reset_type_variables(st__141)
    var t491 []EnvEntry = env_empty()
    var t492 Exp = exp_var("x")
    var t493 Exp = exp_var("x")
    var t494 Exp = exp_let("x", t492, t493)
    var t495 Result__Typ__string = typeof(st__141, t491, t494)
    show_result("unbound_var", t495)
    reset_type_variables(st__141)
    var t496 []EnvEntry = env_empty()
    var t497 Exp = exp_var("y")
    var t498 Exp = exp_var("y")
    var t499 Exp = exp_var("z")
    var t500 Exp = exp_app(t498, t499)
    var t501 Exp = exp_lam("z", t500)
    var t502 Exp = exp_app(t497, t501)
    var t503 Exp = exp_lam("y", t502)
    var t504 Result__Typ__string = typeof(st__141, t496, t503)
    show_result("max_heiber", t504)
    reset_type_variables(st__141)
    var t505 []EnvEntry = env_empty()
    var t506 Exp = exp_var("k")
    var t507 Exp = exp_var("k")
    var t508 Exp = exp_var("x")
    var t509 Exp = exp_app(t507, t508)
    var t510 Exp = exp_var("y")
    var t511 Exp = exp_app(t509, t510)
    var t512 Exp = exp_app(t506, t511)
    var t513 Exp = exp_var("k")
    var t514 Exp = exp_var("y")
    var t515 Exp = exp_app(t513, t514)
    var t516 Exp = exp_var("x")
    var t517 Exp = exp_app(t515, t516)
    var t518 Exp = exp_app(t512, t517)
    var t519 Exp = exp_lam("k", t518)
    var t520 Exp = exp_lam("y", t519)
    var t521 Exp = exp_lam("x", t520)
    var t522 Result__Typ__string = typeof(st__141, t505, t521)
    show_result("kirang", t522)
    reset_type_variables(st__141)
    var t523 []EnvEntry = env_empty()
    var t524 Exp = exp_var("id")
    var t525 Exp = exp_var("id")
    var t526 Exp = exp_app(t524, t525)
    var t527 Exp = exp_let("id", id__142, t526)
    var t528 Result__Typ__string = typeof(st__141, t523, t527)
    show_result("let_id_idid", t528)
    reset_type_variables(st__141)
    var t529 []EnvEntry = env_empty()
    var t530 Exp = exp_var("x")
    var t531 Exp = exp_app(t530, id__142)
    var t532 Exp = exp_var("z")
    var t533 Exp = exp_let("z", t531, t532)
    var t534 Exp = exp_var("y")
    var t535 Exp = exp_let("y", t533, t534)
    var t536 Exp = exp_let("x", c1__143, t535)
    var t537 Result__Typ__string = typeof(st__141, t529, t536)
    show_result("nested_lets", t537)
    reset_type_variables(st__141)
    var t538 []EnvEntry = env_empty()
    var t539 Exp = exp_var("x")
    var t540 Exp = exp_var("y")
    var t541 Exp = exp_app(t539, t540)
    var t542 Exp = exp_var("y")
    var t543 Exp = exp_var("x")
    var t544 Exp = exp_app(t542, t543)
    var t545 Exp = exp_lam("x", t544)
    var t546 Exp = exp_let("x", t541, t545)
    var t547 Exp = exp_lam("y", t546)
    var t548 Exp = exp_lam("x", t547)
    var t549 Result__Typ__string = typeof(st__141, t538, t548)
    show_result("fun_x_fun_y_let_x_xy_fun_x_yx", t549)
    reset_type_variables(st__141)
    var t550 []EnvEntry = env_empty()
    var t551 Exp = exp_var("x")
    var t552 Exp = exp_var("y")
    var t553 Exp = exp_let("y", t551, t552)
    var t554 Exp = exp_lam("x", t553)
    var t555 Result__Typ__string = typeof(st__141, t550, t554)
    show_result("sound_gen_1", t555)
    reset_type_variables(st__141)
    var t556 []EnvEntry = env_empty()
    var t557 Exp = exp_var("x")
    var t558 Exp = exp_lam("z", t557)
    var t559 Exp = exp_var("y")
    var t560 Exp = exp_let("y", t558, t559)
    var t561 Exp = exp_lam("x", t560)
    var t562 Result__Typ__string = typeof(st__141, t556, t561)
    show_result("sound_gen_2", t562)
    reset_type_variables(st__141)
    var t563 []EnvEntry = env_empty()
    var t564 Exp = exp_var("x")
    var t565 Exp = exp_var("z")
    var t566 Exp = exp_app(t564, t565)
    var t567 Exp = exp_lam("z", t566)
    var t568 Exp = exp_var("y")
    var t569 Exp = exp_let("y", t567, t568)
    var t570 Exp = exp_lam("x", t569)
    var t571 Result__Typ__string = typeof(st__141, t563, t570)
    show_result("sound_gen_3", t571)
    reset_type_variables(st__141)
    var t572 []EnvEntry = env_empty()
    var t573 Exp = exp_var("x")
    var t574 Exp = exp_var("y")
    var t575 Exp = exp_app(t573, t574)
    var t576 Exp = exp_var("x")
    var t577 Exp = exp_var("y")
    var t578 Exp = exp_app(t576, t577)
    var t579 Exp = exp_let("x", t575, t578)
    var t580 Exp = exp_lam("y", t579)
    var t581 Exp = exp_lam("x", t580)
    var t582 Result__Typ__string = typeof(st__141, t572, t581)
    show_result("double_apply", t582)
    reset_type_variables(st__141)
    var t583 []EnvEntry = env_empty()
    var t584 Exp = exp_var("x")
    var t585 Exp = exp_var("y")
    var t586 Exp = exp_var("y")
    var t587 Exp = exp_app(t585, t586)
    var t588 Exp = exp_let("y", t584, t587)
    var t589 Exp = exp_lam("x", t588)
    var t590 Result__Typ__string = typeof(st__141, t583, t589)
    show_result("sound_gen_occurs", t590)
    reset_gensym(st__141)
    var t591 []EnvEntry = env_empty()
    var t592 Exp = exp_var("x")
    var t593 Exp = exp_app(t592, id__142)
    var t594 Exp = exp_var("z")
    var t595 Exp = exp_let("z", t593, t594)
    var t596 Exp = exp_var("y")
    var t597 Exp = exp_let("y", t595, t596)
    var t598 Exp = exp_lam("x", t597)
    var t599 Result__Typ__string = typeof(st__141, t591, t598)
    show_result("fun_x_let_y_let_z_x_id_z_y", t599)
    string_println("")
    string_println("All Done")
    string_println("")
    return struct{}{}
}

func main() {
    main0()
}
