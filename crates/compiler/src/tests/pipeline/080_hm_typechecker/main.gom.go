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
    var t204 *ref_int32_x
    var t205 *ref_int32_x
    var t206 CheckerState
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t204 = ref__Ref_int32(0)
            t205 = ref__Ref_int32(1)
            t206 = CheckerState{
                gensym_counter: t204,
                current_level: t205,
            }
            return t206
        default:
            panic("invalid pc")
        }
    }
}

func reset_gensym(st__0 CheckerState) struct{} {
    var t207 *ref_int32_x
    var mtmp0 struct{}
    _ = mtmp0
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t207 = st__0.gensym_counter
            ref_set__Ref_int32(t207, 0)
            return struct{}{}
        default:
            panic("invalid pc")
        }
    }
}

func reset_level(st__1 CheckerState) struct{} {
    var t208 *ref_int32_x
    var mtmp1 struct{}
    _ = mtmp1
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t208 = st__1.current_level
            ref_set__Ref_int32(t208, 1)
            return struct{}{}
        default:
            panic("invalid pc")
        }
    }
}

func reset_type_variables(st__2 CheckerState) struct{} {
    var _wild2 struct{}
    var t209 struct{}
    _ = _wild2
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            reset_gensym(st__2)
            t209 = reset_level(st__2)
            return t209
        default:
            panic("invalid pc")
        }
    }
}

func enter_level(st__3 CheckerState) struct{} {
    var t210 *ref_int32_x
    var l__4 int32
    var t211 *ref_int32_x
    var t212 int32
    var mtmp3 struct{}
    _ = mtmp3
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t210 = st__3.current_level
            l__4 = ref_get__Ref_int32(t210)
            t211 = st__3.current_level
            t212 = l__4 + 1
            ref_set__Ref_int32(t211, t212)
            return struct{}{}
        default:
            panic("invalid pc")
        }
    }
}

func leave_level(st__5 CheckerState) struct{} {
    var t213 *ref_int32_x
    var l__6 int32
    var t214 *ref_int32_x
    var t215 int32
    var mtmp4 struct{}
    _ = mtmp4
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t213 = st__5.current_level
            l__6 = ref_get__Ref_int32(t213)
            t214 = st__5.current_level
            t215 = l__6 - 1
            ref_set__Ref_int32(t214, t215)
            return struct{}{}
        default:
            panic("invalid pc")
        }
    }
}

func min_i32(a__7 int32, b__8 int32) int32 {
    var t218 bool
    var jp217 int32
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t218 = a__7 < b__8
            if t218 {
                pc = 2
            } else {
                pc = 3
            }
        case 1:
            return jp217
        case 2:
            jp217 = a__7
            pc = 1
        case 3:
            jp217 = b__8
            pc = 1
        default:
            panic("invalid pc")
        }
    }
}

func nth_letter(n__9 int32) rune {
    var jp220 rune
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            switch n__9 {
            case 0:
                pc = 2
            case 1:
                pc = 3
            case 2:
                pc = 4
            case 3:
                pc = 5
            case 4:
                pc = 6
            case 5:
                pc = 7
            case 6:
                pc = 8
            case 7:
                pc = 9
            case 8:
                pc = 10
            case 9:
                pc = 11
            case 10:
                pc = 12
            case 11:
                pc = 13
            case 12:
                pc = 14
            case 13:
                pc = 15
            case 14:
                pc = 16
            case 15:
                pc = 17
            case 16:
                pc = 18
            case 17:
                pc = 19
            case 18:
                pc = 20
            case 19:
                pc = 21
            case 20:
                pc = 22
            case 21:
                pc = 23
            case 22:
                pc = 24
            case 23:
                pc = 25
            case 24:
                pc = 26
            case 25:
                pc = 27
            default:
                pc = 28
            }
        case 1:
            return jp220
        case 2:
            jp220 = 97
            pc = 1
        case 3:
            jp220 = 98
            pc = 1
        case 4:
            jp220 = 99
            pc = 1
        case 5:
            jp220 = 100
            pc = 1
        case 6:
            jp220 = 101
            pc = 1
        case 7:
            jp220 = 102
            pc = 1
        case 8:
            jp220 = 103
            pc = 1
        case 9:
            jp220 = 104
            pc = 1
        case 10:
            jp220 = 105
            pc = 1
        case 11:
            jp220 = 106
            pc = 1
        case 12:
            jp220 = 107
            pc = 1
        case 13:
            jp220 = 108
            pc = 1
        case 14:
            jp220 = 109
            pc = 1
        case 15:
            jp220 = 110
            pc = 1
        case 16:
            jp220 = 111
            pc = 1
        case 17:
            jp220 = 112
            pc = 1
        case 18:
            jp220 = 113
            pc = 1
        case 19:
            jp220 = 114
            pc = 1
        case 20:
            jp220 = 115
            pc = 1
        case 21:
            jp220 = 116
            pc = 1
        case 22:
            jp220 = 117
            pc = 1
        case 23:
            jp220 = 118
            pc = 1
        case 24:
            jp220 = 119
            pc = 1
        case 25:
            jp220 = 120
            pc = 1
        case 26:
            jp220 = 121
            pc = 1
        case 27:
            jp220 = 122
            pc = 1
        case 28:
            jp220 = 97
            pc = 1
        default:
            panic("invalid pc")
        }
    }
}

func gensym(st__10 CheckerState) string {
    var t221 *ref_int32_x
    var n__11 int32
    var t222 *ref_int32_x
    var t223 int32
    var mtmp5 struct{}
    var t226 bool
    var jp225 string
    var t227 rune
    var t228 string
    var t229 string
    var t230 string
    _ = mtmp5
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t221 = st__10.gensym_counter
            n__11 = ref_get__Ref_int32(t221)
            t222 = st__10.gensym_counter
            t223 = n__11 + 1
            ref_set__Ref_int32(t222, t223)
            t226 = n__11 < 26
            if t226 {
                pc = 2
            } else {
                pc = 3
            }
        case 1:
            return jp225
        case 2:
            t227 = nth_letter(n__11)
            t228 = char_to_string(t227)
            jp225 = t228
            pc = 1
        case 3:
            t229 = int32_to_string(n__11)
            t230 = "t" + t229
            jp225 = t230
            pc = 1
        default:
            panic("invalid pc")
        }
    }
}

func newvar(st__12 CheckerState) Typ {
    var name__13 string
    var t231 *ref_int32_x
    var level__14 int32
    var t232 Tv
    var t233 *ref_tv_x
    var t234 Typ
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            name__13 = gensym(st__12)
            t231 = st__12.current_level
            level__14 = ref_get__Ref_int32(t231)
            t232 = Unbound{
                _0: name__13,
                _1: level__14,
            }
            t233 = ref__Ref_Tv(t232)
            t234 = TVar{
                _0: t233,
            }
            return t234
        default:
            panic("invalid pc")
        }
    }
}

func typ_is_arrow(ty__15 Typ) bool {
    var jp236 bool
    var x6 *ref_tv_x
    var tvref__16 *ref_tv_x
    var mtmp10 Tv
    var jp238 bool
    var x11 string
    var x12 int32
    var x13 Typ
    var inner__17 Typ
    var t239 bool
    var x7 string
    var x8 Typ
    var x9 Typ
    _ = x11
    _ = x12
    _ = x7
    _ = x8
    _ = x9
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            switch ty__15.(type) {
            case TVar:
                pc = 2
            case QVar:
                pc = 6
            case TArrow:
                pc = 7
            default:
                panic("non-exhaustive match")
            }
        case 1:
            return jp236
        case 2:
            x6 = ty__15.(TVar)._0
            tvref__16 = x6
            mtmp10 = ref_get__Ref_Tv(tvref__16)
            switch mtmp10.(type) {
            case Unbound:
                pc = 4
            case Link:
                pc = 5
            default:
                panic("non-exhaustive match")
            }
        case 3:
            jp236 = jp238
            pc = 1
        case 4:
            jp238 = false
            pc = 3
        case 5:
            x13 = mtmp10.(Link)._0
            inner__17 = x13
            t239 = typ_is_arrow(inner__17)
            jp238 = t239
            pc = 3
        case 6:
            jp236 = false
            pc = 1
        case 7:
            jp236 = true
            pc = 1
        default:
            panic("invalid pc")
        }
    }
}

func typ_to_string(ty__18 Typ) string {
    var jp241 string
    var x14 *ref_tv_x
    var tvref__20 *ref_tv_x
    var mtmp18 Tv
    var jp243 string
    var x19 string
    var x20 int32
    var name__21 string
    var t244 string
    var x21 Typ
    var inner__22 Typ
    var t245 string
    var x15 string
    var name__19 string
    var t246 string
    var x16 Typ
    var x17 Typ
    var t2__24 Typ
    var t1__23 Typ
    var t251 bool
    var jp248 string
    var s1__25 string
    var s2__26 string
    var t249 string
    var t250 string
    var t252 string
    var t253 string
    var t254 string
    var t255 string
    _ = x20
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            switch ty__18.(type) {
            case TVar:
                pc = 2
            case QVar:
                pc = 6
            case TArrow:
                pc = 7
            default:
                panic("non-exhaustive match")
            }
        case 1:
            return jp241
        case 2:
            x14 = ty__18.(TVar)._0
            tvref__20 = x14
            mtmp18 = ref_get__Ref_Tv(tvref__20)
            switch mtmp18.(type) {
            case Unbound:
                pc = 4
            case Link:
                pc = 5
            default:
                panic("non-exhaustive match")
            }
        case 3:
            jp241 = jp243
            pc = 1
        case 4:
            x19 = mtmp18.(Unbound)._0
            name__21 = x19
            t244 = "'" + name__21
            jp243 = t244
            pc = 3
        case 5:
            x21 = mtmp18.(Link)._0
            inner__22 = x21
            t245 = typ_to_string(inner__22)
            jp243 = t245
            pc = 3
        case 6:
            x15 = ty__18.(QVar)._0
            name__19 = x15
            t246 = "'" + name__19
            jp241 = t246
            pc = 1
        case 7:
            x16 = ty__18.(TArrow)._0
            x17 = ty__18.(TArrow)._1
            t2__24 = x17
            t1__23 = x16
            t251 = typ_is_arrow(t1__23)
            if t251 {
                pc = 9
            } else {
                pc = 10
            }
        case 8:
            s1__25 = jp248
            s2__26 = typ_to_string(t2__24)
            t249 = s1__25 + " -> "
            t250 = t249 + s2__26
            jp241 = t250
            pc = 1
        case 9:
            t252 = typ_to_string(t1__23)
            t253 = "(" + t252
            t254 = t253 + ")"
            jp248 = t254
            pc = 8
        case 10:
            t255 = typ_to_string(t1__23)
            jp248 = t255
            pc = 8
        default:
            panic("invalid pc")
        }
    }
}

func env_empty() []EnvEntry {
    var env__27 []EnvEntry
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            env__27 = nil
            return env__27
        default:
            panic("invalid pc")
        }
    }
}

func env_lookup(env__28 []EnvEntry, name__29 string) Option__Typ {
    var t256 int32
    var t257 int32
    var i__30 *ref_int32_x
    var found__31 *ref_option__typ_x
    var done__32 *ref_bool_x
    var mtmp23 struct{}
    var t259 Option__Typ
    var t261 bool
    var t262 bool
    var t263 int32
    var t264 bool
    var t265 bool
    var t266 int32
    var entry__33 EnvEntry
    var t268 string
    var t269 bool
    var t270 Typ
    var t271 Option__Typ
    var mtmp22 struct{}
    var t272 struct{}
    var t273 int32
    var t274 int32
    var t275 struct{}
    _ = mtmp23
    _ = mtmp22
    _ = t272
    _ = t275
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t256 = int32(len(env__28))
            t257 = t256 - 1
            i__30 = ref__Ref_int32(t257)
            found__31 = ref__Ref_Option__Typ(None{})
            done__32 = ref__Ref_bool(false)
            pc = 2
        case 1:
            t259 = ref_get__Ref_Option__Typ(found__31)
            return t259
        case 2:
            t261 = ref_get__Ref_bool(done__32)
            t262 = !t261
            t263 = ref_get__Ref_int32(i__30)
            t264 = t263 >= 0
            t265 = t262 && t264
            if t265 {
                pc = 3
            } else {
                pc = 7
            }
        case 3:
            t266 = ref_get__Ref_int32(i__30)
            entry__33 = env__28[t266]
            t268 = entry__33.name
            t269 = t268 == name__29
            if t269 {
                pc = 5
            } else {
                pc = 6
            }
        case 4:
            pc = 2
        case 5:
            t270 = entry__33.ty
            t271 = Some{
                _0: t270,
            }
            ref_set__Ref_Option__Typ(found__31, t271)
            ref_set__Ref_bool(done__32, true)
            pc = 4
        case 6:
            t273 = ref_get__Ref_int32(i__30)
            t274 = t273 - 1
            ref_set__Ref_int32(i__30, t274)
            pc = 4
        case 7:
            pc = 1
        default:
            panic("invalid pc")
        }
    }
}

func subst_lookup(subst__34 []SubstEntry, name__35 string) Option__Typ {
    var t276 int32
    var t277 int32
    var i__36 *ref_int32_x
    var found__37 *ref_option__typ_x
    var done__38 *ref_bool_x
    var mtmp25 struct{}
    var t279 Option__Typ
    var t281 bool
    var t282 bool
    var t283 int32
    var t284 bool
    var t285 bool
    var t286 int32
    var entry__39 SubstEntry
    var t288 string
    var t289 bool
    var t290 Typ
    var t291 Option__Typ
    var mtmp24 struct{}
    var t292 struct{}
    var t293 int32
    var t294 int32
    var t295 struct{}
    _ = mtmp25
    _ = mtmp24
    _ = t292
    _ = t295
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t276 = int32(len(subst__34))
            t277 = t276 - 1
            i__36 = ref__Ref_int32(t277)
            found__37 = ref__Ref_Option__Typ(None{})
            done__38 = ref__Ref_bool(false)
            pc = 2
        case 1:
            t279 = ref_get__Ref_Option__Typ(found__37)
            return t279
        case 2:
            t281 = ref_get__Ref_bool(done__38)
            t282 = !t281
            t283 = ref_get__Ref_int32(i__36)
            t284 = t283 >= 0
            t285 = t282 && t284
            if t285 {
                pc = 3
            } else {
                pc = 7
            }
        case 3:
            t286 = ref_get__Ref_int32(i__36)
            entry__39 = subst__34[t286]
            t288 = entry__39.name
            t289 = t288 == name__35
            if t289 {
                pc = 5
            } else {
                pc = 6
            }
        case 4:
            pc = 2
        case 5:
            t290 = entry__39.ty
            t291 = Some{
                _0: t290,
            }
            ref_set__Ref_Option__Typ(found__37, t291)
            ref_set__Ref_bool(done__38, true)
            pc = 4
        case 6:
            t293 = ref_get__Ref_int32(i__36)
            t294 = t293 - 1
            ref_set__Ref_int32(i__36, t294)
            pc = 4
        case 7:
            pc = 1
        default:
            panic("invalid pc")
        }
    }
}

func occurs(st__40 CheckerState, tvr__41 *ref_tv_x, ty__42 Typ) Result__unit__string {
    var jp297 Result__unit__string
    var x26 *ref_tv_x
    var tvr2__43 *ref_tv_x
    var t300 bool
    var jp299 Result__unit__string
    var t301 Result__unit__string
    var mtmp30 Tv
    var jp303 Result__unit__string
    var x31 string
    var x32 int32
    var l2__45 int32
    var name__44 string
    var mtmp34 Tv
    var jp305 int32
    var min_level__47 int32
    var t306 Tv
    var mtmp38 struct{}
    var t307 Result__unit__string
    var x35 string
    var x36 int32
    var l__46 int32
    var t308 int32
    var x37 Typ
    var x33 Typ
    var inner__48 Typ
    var t309 Result__unit__string
    var x27 string
    var t310 Result__unit__string
    var x28 Typ
    var x29 Typ
    var t2__50 Typ
    var t1__49 Typ
    var mtmp39 Result__unit__string
    var jp312 Result__unit__string
    var x40 struct{}
    var t313 Result__unit__string
    var x41 string
    var e__51 string
    var t314 Result__unit__string
    _ = mtmp38
    _ = x35
    _ = x37
    _ = x27
    _ = x40
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            switch ty__42.(type) {
            case TVar:
                pc = 2
            case QVar:
                pc = 12
            case TArrow:
                pc = 13
            default:
                panic("non-exhaustive match")
            }
        case 1:
            return jp297
        case 2:
            x26 = ty__42.(TVar)._0
            tvr2__43 = x26
            t300 = ptr_eq__Ref_Tv(tvr__41, tvr2__43)
            if t300 {
                pc = 4
            } else {
                pc = 5
            }
        case 3:
            jp297 = jp299
            pc = 1
        case 4:
            t301 = Result__unit__string_Err{
                _0: "occurs check",
            }
            jp299 = t301
            pc = 3
        case 5:
            mtmp30 = ref_get__Ref_Tv(tvr2__43)
            switch mtmp30.(type) {
            case Unbound:
                pc = 7
            case Link:
                pc = 11
            default:
                panic("non-exhaustive match")
            }
        case 6:
            jp299 = jp303
            pc = 3
        case 7:
            x31 = mtmp30.(Unbound)._0
            x32 = mtmp30.(Unbound)._1
            l2__45 = x32
            name__44 = x31
            mtmp34 = ref_get__Ref_Tv(tvr__41)
            switch mtmp34.(type) {
            case Unbound:
                pc = 9
            case Link:
                pc = 10
            default:
                panic("non-exhaustive match")
            }
        case 8:
            min_level__47 = jp305
            t306 = Unbound{
                _0: name__44,
                _1: min_level__47,
            }
            ref_set__Ref_Tv(tvr2__43, t306)
            t307 = Result__unit__string_Ok{
                _0: struct{}{},
            }
            jp303 = t307
            pc = 6
        case 9:
            x36 = mtmp34.(Unbound)._1
            l__46 = x36
            t308 = min_i32(l__46, l2__45)
            jp305 = t308
            pc = 8
        case 10:
            jp305 = l2__45
            pc = 8
        case 11:
            x33 = mtmp30.(Link)._0
            inner__48 = x33
            t309 = occurs(st__40, tvr__41, inner__48)
            jp303 = t309
            pc = 6
        case 12:
            t310 = Result__unit__string_Ok{
                _0: struct{}{},
            }
            jp297 = t310
            pc = 1
        case 13:
            x28 = ty__42.(TArrow)._0
            x29 = ty__42.(TArrow)._1
            t2__50 = x29
            t1__49 = x28
            mtmp39 = occurs(st__40, tvr__41, t1__49)
            switch mtmp39.(type) {
            case Result__unit__string_Ok:
                pc = 15
            case Result__unit__string_Err:
                pc = 16
            default:
                panic("non-exhaustive match")
            }
        case 14:
            jp297 = jp312
            pc = 1
        case 15:
            t313 = occurs(st__40, tvr__41, t2__50)
            jp312 = t313
            pc = 14
        case 16:
            x41 = mtmp39.(Result__unit__string_Err)._0
            e__51 = x41
            t314 = Result__unit__string_Err{
                _0: e__51,
            }
            jp312 = t314
            pc = 14
        default:
            panic("invalid pc")
        }
    }
}

func unify(st__52 CheckerState, t1__53 Typ, t2__54 Typ) Result__unit__string {
    var mtmp42 Tuple2_Typ_Typ
    var x43 Typ
    var x44 Typ
    var jp316 Result__unit__string
    var x45 *ref_tv_x
    var jp318 Result__unit__string
    var x49 *ref_tv_x
    var r1__55 *ref_tv_x
    var r2__56 *ref_tv_x
    var t321 bool
    var jp320 Result__unit__string
    var t322 Result__unit__string
    var mtmp53 Tv
    var jp324 Result__unit__string
    var x54 string
    var x55 int32
    var mtmp57 Tv
    var jp326 Result__unit__string
    var x58 string
    var x59 int32
    var t327 Typ
    var mtmp61 Result__unit__string
    var jp329 Result__unit__string
    var x62 struct{}
    var t330 Typ
    var t331 Tv
    var mtmp64 struct{}
    var t332 Result__unit__string
    var x63 string
    var e__59 string
    var t333 Result__unit__string
    var x60 Typ
    var inner__58 Typ
    var t334 Typ
    var t335 Result__unit__string
    var x56 Typ
    var inner__57 Typ
    var t336 Typ
    var t337 Result__unit__string
    var x50 string
    var r2__65 *ref_tv_x
    var other__64 Typ
    var mtmp65 Tv
    var jp339 Result__unit__string
    var x66 string
    var x67 int32
    var mtmp69 Result__unit__string
    var jp341 Result__unit__string
    var x70 struct{}
    var t342 Tv
    var mtmp72 struct{}
    var t343 Result__unit__string
    var x71 string
    var e__67 string
    var t344 Result__unit__string
    var x68 Typ
    var inner__66 Typ
    var t345 Result__unit__string
    var x51 Typ
    var x52 Typ
    var mtmp73 Tv
    var jp347 Result__unit__string
    var x74 string
    var x75 int32
    var mtmp77 Result__unit__string
    var jp349 Result__unit__string
    var x78 struct{}
    var t350 Tv
    var mtmp80 struct{}
    var t351 Result__unit__string
    var x79 string
    var t352 Result__unit__string
    var x76 Typ
    var t353 Result__unit__string
    var x46 string
    var jp355 Result__unit__string
    var x81 *ref_tv_x
    var r1__60 *ref_tv_x
    var other__61 Typ
    var mtmp85 Tv
    var jp357 Result__unit__string
    var x86 string
    var x87 int32
    var mtmp89 Result__unit__string
    var jp359 Result__unit__string
    var x90 struct{}
    var t360 Tv
    var mtmp92 struct{}
    var t361 Result__unit__string
    var x91 string
    var e__63 string
    var t362 Result__unit__string
    var x88 Typ
    var inner__62 Typ
    var t363 Result__unit__string
    var x82 string
    var t364 Result__unit__string
    var x83 Typ
    var x84 Typ
    var t365 Result__unit__string
    var x47 Typ
    var x48 Typ
    var jp367 Result__unit__string
    var x93 *ref_tv_x
    var mtmp97 Tv
    var jp369 Result__unit__string
    var x98 string
    var x99 int32
    var mtmp101 Result__unit__string
    var jp371 Result__unit__string
    var x102 struct{}
    var t372 Tv
    var mtmp104 struct{}
    var t373 Result__unit__string
    var x103 string
    var t374 Result__unit__string
    var x100 Typ
    var t375 Result__unit__string
    var x94 string
    var t376 Result__unit__string
    var x95 Typ
    var x96 Typ
    var a2__69 Typ
    var a1__68 Typ
    var b2__71 Typ
    var b1__70 Typ
    var mtmp105 Result__unit__string
    var jp378 Result__unit__string
    var x106 struct{}
    var t379 Result__unit__string
    var x107 string
    var e__72 string
    var t380 Result__unit__string
    _ = x54
    _ = x55
    _ = x58
    _ = x59
    _ = x62
    _ = mtmp64
    _ = x50
    _ = x66
    _ = x67
    _ = x70
    _ = mtmp72
    _ = x51
    _ = x52
    _ = x74
    _ = x75
    _ = x78
    _ = mtmp80
    _ = x46
    _ = x86
    _ = x87
    _ = x90
    _ = mtmp92
    _ = x82
    _ = x83
    _ = x84
    _ = x98
    _ = x99
    _ = x102
    _ = mtmp104
    _ = x94
    _ = x106
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            mtmp42 = Tuple2_Typ_Typ{
                _0: t1__53,
                _1: t2__54,
            }
            x43 = mtmp42._0
            x44 = mtmp42._1
            switch x44.(type) {
            case TVar:
                pc = 2
            case QVar:
                pc = 31
            case TArrow:
                pc = 42
            default:
                panic("non-exhaustive match")
            }
        case 1:
            return jp316
        case 2:
            x45 = x44.(TVar)._0
            switch x43.(type) {
            case TVar:
                pc = 4
            case QVar:
                pc = 17
            case TArrow:
                pc = 24
            default:
                panic("non-exhaustive match")
            }
        case 3:
            jp316 = jp318
            pc = 1
        case 4:
            x49 = x43.(TVar)._0
            r1__55 = x49
            r2__56 = x45
            t321 = ptr_eq__Ref_Tv(r1__55, r2__56)
            if t321 {
                pc = 6
            } else {
                pc = 7
            }
        case 5:
            jp318 = jp320
            pc = 3
        case 6:
            t322 = Result__unit__string_Ok{
                _0: struct{}{},
            }
            jp320 = t322
            pc = 5
        case 7:
            mtmp53 = ref_get__Ref_Tv(r1__55)
            switch mtmp53.(type) {
            case Unbound:
                pc = 9
            case Link:
                pc = 16
            default:
                panic("non-exhaustive match")
            }
        case 8:
            jp320 = jp324
            pc = 5
        case 9:
            mtmp57 = ref_get__Ref_Tv(r2__56)
            switch mtmp57.(type) {
            case Unbound:
                pc = 11
            case Link:
                pc = 15
            default:
                panic("non-exhaustive match")
            }
        case 10:
            jp324 = jp326
            pc = 8
        case 11:
            t327 = TVar{
                _0: r2__56,
            }
            mtmp61 = occurs(st__52, r1__55, t327)
            switch mtmp61.(type) {
            case Result__unit__string_Ok:
                pc = 13
            case Result__unit__string_Err:
                pc = 14
            default:
                panic("non-exhaustive match")
            }
        case 12:
            jp326 = jp329
            pc = 10
        case 13:
            t330 = TVar{
                _0: r2__56,
            }
            t331 = Link{
                _0: t330,
            }
            ref_set__Ref_Tv(r1__55, t331)
            t332 = Result__unit__string_Ok{
                _0: struct{}{},
            }
            jp329 = t332
            pc = 12
        case 14:
            x63 = mtmp61.(Result__unit__string_Err)._0
            e__59 = x63
            t333 = Result__unit__string_Err{
                _0: e__59,
            }
            jp329 = t333
            pc = 12
        case 15:
            x60 = mtmp57.(Link)._0
            inner__58 = x60
            t334 = TVar{
                _0: r1__55,
            }
            t335 = unify(st__52, t334, inner__58)
            jp326 = t335
            pc = 10
        case 16:
            x56 = mtmp53.(Link)._0
            inner__57 = x56
            t336 = TVar{
                _0: r2__56,
            }
            t337 = unify(st__52, inner__57, t336)
            jp324 = t337
            pc = 8
        case 17:
            r2__65 = x45
            other__64 = x43
            mtmp65 = ref_get__Ref_Tv(r2__65)
            switch mtmp65.(type) {
            case Unbound:
                pc = 19
            case Link:
                pc = 23
            default:
                panic("non-exhaustive match")
            }
        case 18:
            jp318 = jp339
            pc = 3
        case 19:
            mtmp69 = occurs(st__52, r2__65, other__64)
            switch mtmp69.(type) {
            case Result__unit__string_Ok:
                pc = 21
            case Result__unit__string_Err:
                pc = 22
            default:
                panic("non-exhaustive match")
            }
        case 20:
            jp339 = jp341
            pc = 18
        case 21:
            t342 = Link{
                _0: other__64,
            }
            ref_set__Ref_Tv(r2__65, t342)
            t343 = Result__unit__string_Ok{
                _0: struct{}{},
            }
            jp341 = t343
            pc = 20
        case 22:
            x71 = mtmp69.(Result__unit__string_Err)._0
            e__67 = x71
            t344 = Result__unit__string_Err{
                _0: e__67,
            }
            jp341 = t344
            pc = 20
        case 23:
            x68 = mtmp65.(Link)._0
            inner__66 = x68
            t345 = unify(st__52, other__64, inner__66)
            jp339 = t345
            pc = 18
        case 24:
            r2__65 = x45
            other__64 = x43
            mtmp73 = ref_get__Ref_Tv(r2__65)
            switch mtmp73.(type) {
            case Unbound:
                pc = 26
            case Link:
                pc = 30
            default:
                panic("non-exhaustive match")
            }
        case 25:
            jp318 = jp347
            pc = 3
        case 26:
            mtmp77 = occurs(st__52, r2__65, other__64)
            switch mtmp77.(type) {
            case Result__unit__string_Ok:
                pc = 28
            case Result__unit__string_Err:
                pc = 29
            default:
                panic("non-exhaustive match")
            }
        case 27:
            jp347 = jp349
            pc = 25
        case 28:
            t350 = Link{
                _0: other__64,
            }
            ref_set__Ref_Tv(r2__65, t350)
            t351 = Result__unit__string_Ok{
                _0: struct{}{},
            }
            jp349 = t351
            pc = 27
        case 29:
            x79 = mtmp77.(Result__unit__string_Err)._0
            e__67 = x79
            t352 = Result__unit__string_Err{
                _0: e__67,
            }
            jp349 = t352
            pc = 27
        case 30:
            x76 = mtmp73.(Link)._0
            inner__66 = x76
            t353 = unify(st__52, other__64, inner__66)
            jp347 = t353
            pc = 25
        case 31:
            switch x43.(type) {
            case TVar:
                pc = 33
            case QVar:
                pc = 40
            case TArrow:
                pc = 41
            default:
                panic("non-exhaustive match")
            }
        case 32:
            jp316 = jp355
            pc = 1
        case 33:
            x81 = x43.(TVar)._0
            r1__60 = x81
            other__61 = x44
            mtmp85 = ref_get__Ref_Tv(r1__60)
            switch mtmp85.(type) {
            case Unbound:
                pc = 35
            case Link:
                pc = 39
            default:
                panic("non-exhaustive match")
            }
        case 34:
            jp355 = jp357
            pc = 32
        case 35:
            mtmp89 = occurs(st__52, r1__60, other__61)
            switch mtmp89.(type) {
            case Result__unit__string_Ok:
                pc = 37
            case Result__unit__string_Err:
                pc = 38
            default:
                panic("non-exhaustive match")
            }
        case 36:
            jp357 = jp359
            pc = 34
        case 37:
            t360 = Link{
                _0: other__61,
            }
            ref_set__Ref_Tv(r1__60, t360)
            t361 = Result__unit__string_Ok{
                _0: struct{}{},
            }
            jp359 = t361
            pc = 36
        case 38:
            x91 = mtmp89.(Result__unit__string_Err)._0
            e__63 = x91
            t362 = Result__unit__string_Err{
                _0: e__63,
            }
            jp359 = t362
            pc = 36
        case 39:
            x88 = mtmp85.(Link)._0
            inner__62 = x88
            t363 = unify(st__52, inner__62, other__61)
            jp357 = t363
            pc = 34
        case 40:
            t364 = Result__unit__string_Err{
                _0: "unify error",
            }
            jp355 = t364
            pc = 32
        case 41:
            t365 = Result__unit__string_Err{
                _0: "unify error",
            }
            jp355 = t365
            pc = 32
        case 42:
            x47 = x44.(TArrow)._0
            x48 = x44.(TArrow)._1
            switch x43.(type) {
            case TVar:
                pc = 44
            case QVar:
                pc = 51
            case TArrow:
                pc = 52
            default:
                panic("non-exhaustive match")
            }
        case 43:
            jp316 = jp367
            pc = 1
        case 44:
            x93 = x43.(TVar)._0
            r1__60 = x93
            other__61 = x44
            mtmp97 = ref_get__Ref_Tv(r1__60)
            switch mtmp97.(type) {
            case Unbound:
                pc = 46
            case Link:
                pc = 50
            default:
                panic("non-exhaustive match")
            }
        case 45:
            jp367 = jp369
            pc = 43
        case 46:
            mtmp101 = occurs(st__52, r1__60, other__61)
            switch mtmp101.(type) {
            case Result__unit__string_Ok:
                pc = 48
            case Result__unit__string_Err:
                pc = 49
            default:
                panic("non-exhaustive match")
            }
        case 47:
            jp369 = jp371
            pc = 45
        case 48:
            t372 = Link{
                _0: other__61,
            }
            ref_set__Ref_Tv(r1__60, t372)
            t373 = Result__unit__string_Ok{
                _0: struct{}{},
            }
            jp371 = t373
            pc = 47
        case 49:
            x103 = mtmp101.(Result__unit__string_Err)._0
            e__63 = x103
            t374 = Result__unit__string_Err{
                _0: e__63,
            }
            jp371 = t374
            pc = 47
        case 50:
            x100 = mtmp97.(Link)._0
            inner__62 = x100
            t375 = unify(st__52, inner__62, other__61)
            jp369 = t375
            pc = 45
        case 51:
            t376 = Result__unit__string_Err{
                _0: "unify error",
            }
            jp367 = t376
            pc = 43
        case 52:
            x95 = x43.(TArrow)._0
            x96 = x43.(TArrow)._1
            a2__69 = x96
            a1__68 = x95
            b2__71 = x48
            b1__70 = x47
            mtmp105 = unify(st__52, a1__68, b1__70)
            switch mtmp105.(type) {
            case Result__unit__string_Ok:
                pc = 54
            case Result__unit__string_Err:
                pc = 55
            default:
                panic("non-exhaustive match")
            }
        case 53:
            jp367 = jp378
            pc = 43
        case 54:
            t379 = unify(st__52, a2__69, b2__71)
            jp378 = t379
            pc = 53
        case 55:
            x107 = mtmp105.(Result__unit__string_Err)._0
            e__72 = x107
            t380 = Result__unit__string_Err{
                _0: e__72,
            }
            jp378 = t380
            pc = 53
        default:
            panic("invalid pc")
        }
    }
}

func gen(st__73 CheckerState, ty__74 Typ) Typ {
    var jp382 Typ
    var x108 *ref_tv_x
    var tvref__75 *ref_tv_x
    var mtmp112 Tv
    var jp384 Typ
    var x113 string
    var x114 int32
    var l__77 int32
    var name__76 string
    var t385 *ref_int32_x
    var cur__78 int32
    var t388 bool
    var jp387 Typ
    var t389 Typ
    var t390 Typ
    var x115 Typ
    var inner__79 Typ
    var t391 Typ
    var x109 string
    var other__82 Typ
    var x110 Typ
    var x111 Typ
    var t2__81 Typ
    var t1__80 Typ
    var t392 Typ
    var t393 Typ
    var t394 Typ
    _ = x109
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            switch ty__74.(type) {
            case TVar:
                pc = 2
            case QVar:
                pc = 9
            case TArrow:
                pc = 10
            default:
                panic("non-exhaustive match")
            }
        case 1:
            return jp382
        case 2:
            x108 = ty__74.(TVar)._0
            tvref__75 = x108
            mtmp112 = ref_get__Ref_Tv(tvref__75)
            switch mtmp112.(type) {
            case Unbound:
                pc = 4
            case Link:
                pc = 8
            default:
                panic("non-exhaustive match")
            }
        case 3:
            jp382 = jp384
            pc = 1
        case 4:
            x113 = mtmp112.(Unbound)._0
            x114 = mtmp112.(Unbound)._1
            l__77 = x114
            name__76 = x113
            t385 = st__73.current_level
            cur__78 = ref_get__Ref_int32(t385)
            t388 = l__77 > cur__78
            if t388 {
                pc = 6
            } else {
                pc = 7
            }
        case 5:
            jp384 = jp387
            pc = 3
        case 6:
            t389 = QVar{
                _0: name__76,
            }
            jp387 = t389
            pc = 5
        case 7:
            t390 = TVar{
                _0: tvref__75,
            }
            jp387 = t390
            pc = 5
        case 8:
            x115 = mtmp112.(Link)._0
            inner__79 = x115
            t391 = gen(st__73, inner__79)
            jp384 = t391
            pc = 3
        case 9:
            other__82 = ty__74
            jp382 = other__82
            pc = 1
        case 10:
            x110 = ty__74.(TArrow)._0
            x111 = ty__74.(TArrow)._1
            t2__81 = x111
            t1__80 = x110
            t392 = gen(st__73, t1__80)
            t393 = gen(st__73, t2__81)
            t394 = TArrow{
                _0: t392,
                _1: t393,
            }
            jp382 = t394
            pc = 1
        default:
            panic("invalid pc")
        }
    }
}

func inst_loop(st__83 CheckerState, subst__84 []SubstEntry, ty__85 Typ) Tuple2_Typ_Vec_SubstEntry {
    var jp396 Tuple2_Typ_Vec_SubstEntry
    var x116 *ref_tv_x
    var tvref__90 *ref_tv_x
    var mtmp120 Tv
    var jp398 Tuple2_Typ_Vec_SubstEntry
    var x121 string
    var x122 int32
    var t399 Typ
    var t400 Tuple2_Typ_Vec_SubstEntry
    var x123 Typ
    var inner__91 Typ
    var t401 Tuple2_Typ_Vec_SubstEntry
    var x117 string
    var name__86 string
    var mtmp124 Option__Typ
    var jp403 Tuple2_Typ_Vec_SubstEntry
    var tv__88 Typ
    var t404 SubstEntry
    var new_subst__89 []SubstEntry
    var t405 Tuple2_Typ_Vec_SubstEntry
    var x125 Typ
    var t__87 Typ
    var t406 Tuple2_Typ_Vec_SubstEntry
    var x118 Typ
    var x119 Typ
    var t2__93 Typ
    var t1__92 Typ
    var mtmp126 Tuple2_Typ_Vec_SubstEntry
    var x127 Typ
    var x128 []SubstEntry
    var subst1__95 []SubstEntry
    var ty1__94 Typ
    var mtmp129 Tuple2_Typ_Vec_SubstEntry
    var x130 Typ
    var x131 []SubstEntry
    var subst2__97 []SubstEntry
    var ty2__96 Typ
    var t407 Typ
    var t408 Tuple2_Typ_Vec_SubstEntry
    _ = x121
    _ = x122
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            switch ty__85.(type) {
            case TVar:
                pc = 2
            case QVar:
                pc = 6
            case TArrow:
                pc = 10
            default:
                panic("non-exhaustive match")
            }
        case 1:
            return jp396
        case 2:
            x116 = ty__85.(TVar)._0
            tvref__90 = x116
            mtmp120 = ref_get__Ref_Tv(tvref__90)
            switch mtmp120.(type) {
            case Unbound:
                pc = 4
            case Link:
                pc = 5
            default:
                panic("non-exhaustive match")
            }
        case 3:
            jp396 = jp398
            pc = 1
        case 4:
            t399 = TVar{
                _0: tvref__90,
            }
            t400 = Tuple2_Typ_Vec_SubstEntry{
                _0: t399,
                _1: subst__84,
            }
            jp398 = t400
            pc = 3
        case 5:
            x123 = mtmp120.(Link)._0
            inner__91 = x123
            t401 = inst_loop(st__83, subst__84, inner__91)
            jp398 = t401
            pc = 3
        case 6:
            x117 = ty__85.(QVar)._0
            name__86 = x117
            mtmp124 = subst_lookup(subst__84, name__86)
            switch mtmp124.(type) {
            case None:
                pc = 8
            case Some:
                pc = 9
            default:
                panic("non-exhaustive match")
            }
        case 7:
            jp396 = jp403
            pc = 1
        case 8:
            tv__88 = newvar(st__83)
            t404 = SubstEntry{
                name: name__86,
                ty: tv__88,
            }
            new_subst__89 = append(subst__84, t404)
            t405 = Tuple2_Typ_Vec_SubstEntry{
                _0: tv__88,
                _1: new_subst__89,
            }
            jp403 = t405
            pc = 7
        case 9:
            x125 = mtmp124.(Some)._0
            t__87 = x125
            t406 = Tuple2_Typ_Vec_SubstEntry{
                _0: t__87,
                _1: subst__84,
            }
            jp403 = t406
            pc = 7
        case 10:
            x118 = ty__85.(TArrow)._0
            x119 = ty__85.(TArrow)._1
            t2__93 = x119
            t1__92 = x118
            mtmp126 = inst_loop(st__83, subst__84, t1__92)
            x127 = mtmp126._0
            x128 = mtmp126._1
            subst1__95 = x128
            ty1__94 = x127
            mtmp129 = inst_loop(st__83, subst1__95, t2__93)
            x130 = mtmp129._0
            x131 = mtmp129._1
            subst2__97 = x131
            ty2__96 = x130
            t407 = TArrow{
                _0: ty1__94,
                _1: ty2__96,
            }
            t408 = Tuple2_Typ_Vec_SubstEntry{
                _0: t407,
                _1: subst2__97,
            }
            jp396 = t408
            pc = 1
        default:
            panic("invalid pc")
        }
    }
}

func inst(st__98 CheckerState, ty__99 Typ) Typ {
    var subst0__100 []SubstEntry
    var mtmp132 Tuple2_Typ_Vec_SubstEntry
    var x133 Typ
    var x134 []SubstEntry
    var t__101 Typ
    _ = x134
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            subst0__100 = nil
            mtmp132 = inst_loop(st__98, subst0__100, ty__99)
            x133 = mtmp132._0
            t__101 = x133
            return t__101
        default:
            panic("invalid pc")
        }
    }
}

func typeof(st__102 CheckerState, env__103 []EnvEntry, e__104 Exp) Result__Typ__string {
    var jp410 Result__Typ__string
    var x135 string
    var x__105 string
    var mtmp143 Option__Typ
    var jp412 Result__Typ__string
    var t413 Result__Typ__string
    var x144 Typ
    var ty__106 Typ
    var t414 Typ
    var t415 Result__Typ__string
    var x136 Exp
    var x137 Exp
    var e2__114 Exp
    var e1__113 Exp
    var mtmp145 Result__Typ__string
    var jp417 Result__Typ__string
    var x146 Typ
    var ty_fun__116 Typ
    var mtmp148 Result__Typ__string
    var jp419 Result__Typ__string
    var x149 Typ
    var ty_arg__118 Typ
    var ty_res__119 Typ
    var arrow__120 Typ
    var mtmp151 Result__unit__string
    var jp421 Result__Typ__string
    var x152 struct{}
    var t422 Result__Typ__string
    var x153 string
    var e__121 string
    var t423 Result__Typ__string
    var x150 string
    var e__117 string
    var t424 Result__Typ__string
    var x147 string
    var e__115 string
    var t425 Result__Typ__string
    var x138 string
    var x139 Exp
    var body__108 Exp
    var x__107 string
    var ty_x__109 Typ
    var t426 EnvEntry
    var env2__110 []EnvEntry
    var mtmp154 Result__Typ__string
    var jp428 Result__Typ__string
    var x155 Typ
    var ty_e__111 Typ
    var t429 Typ
    var t430 Result__Typ__string
    var x156 string
    var e__112 string
    var t431 Result__Typ__string
    var x140 string
    var x141 Exp
    var x142 Exp
    var e2__124 Exp
    var e1__123 Exp
    var x__122 string
    var _wild157 struct{}
    var ty_e__125 Result__Typ__string
    var _wild158 struct{}
    var jp433 Result__Typ__string
    var x159 Typ
    var ty1__127 Typ
    var t434 Typ
    var t435 EnvEntry
    var env2__128 []EnvEntry
    var t436 Result__Typ__string
    var x160 string
    var e__126 string
    var t437 Result__Typ__string
    _ = x152
    _ = _wild157
    _ = _wild158
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            switch e__104.(type) {
            case Var:
                pc = 2
            case App:
                pc = 6
            case Lam:
                pc = 16
            case Let:
                pc = 20
            default:
                panic("non-exhaustive match")
            }
        case 1:
            return jp410
        case 2:
            x135 = e__104.(Var)._0
            x__105 = x135
            mtmp143 = env_lookup(env__103, x__105)
            switch mtmp143.(type) {
            case None:
                pc = 4
            case Some:
                pc = 5
            default:
                panic("non-exhaustive match")
            }
        case 3:
            jp410 = jp412
            pc = 1
        case 4:
            t413 = Result__Typ__string_Err{
                _0: "unbound var",
            }
            jp412 = t413
            pc = 3
        case 5:
            x144 = mtmp143.(Some)._0
            ty__106 = x144
            t414 = inst(st__102, ty__106)
            t415 = Result__Typ__string_Ok{
                _0: t414,
            }
            jp412 = t415
            pc = 3
        case 6:
            x136 = e__104.(App)._0
            x137 = e__104.(App)._1
            e2__114 = x137
            e1__113 = x136
            mtmp145 = typeof(st__102, env__103, e1__113)
            switch mtmp145.(type) {
            case Result__Typ__string_Ok:
                pc = 8
            case Result__Typ__string_Err:
                pc = 15
            default:
                panic("non-exhaustive match")
            }
        case 7:
            jp410 = jp417
            pc = 1
        case 8:
            x146 = mtmp145.(Result__Typ__string_Ok)._0
            ty_fun__116 = x146
            mtmp148 = typeof(st__102, env__103, e2__114)
            switch mtmp148.(type) {
            case Result__Typ__string_Ok:
                pc = 10
            case Result__Typ__string_Err:
                pc = 14
            default:
                panic("non-exhaustive match")
            }
        case 9:
            jp417 = jp419
            pc = 7
        case 10:
            x149 = mtmp148.(Result__Typ__string_Ok)._0
            ty_arg__118 = x149
            ty_res__119 = newvar(st__102)
            arrow__120 = TArrow{
                _0: ty_arg__118,
                _1: ty_res__119,
            }
            mtmp151 = unify(st__102, ty_fun__116, arrow__120)
            switch mtmp151.(type) {
            case Result__unit__string_Ok:
                pc = 12
            case Result__unit__string_Err:
                pc = 13
            default:
                panic("non-exhaustive match")
            }
        case 11:
            jp419 = jp421
            pc = 9
        case 12:
            t422 = Result__Typ__string_Ok{
                _0: ty_res__119,
            }
            jp421 = t422
            pc = 11
        case 13:
            x153 = mtmp151.(Result__unit__string_Err)._0
            e__121 = x153
            t423 = Result__Typ__string_Err{
                _0: e__121,
            }
            jp421 = t423
            pc = 11
        case 14:
            x150 = mtmp148.(Result__Typ__string_Err)._0
            e__117 = x150
            t424 = Result__Typ__string_Err{
                _0: e__117,
            }
            jp419 = t424
            pc = 9
        case 15:
            x147 = mtmp145.(Result__Typ__string_Err)._0
            e__115 = x147
            t425 = Result__Typ__string_Err{
                _0: e__115,
            }
            jp417 = t425
            pc = 7
        case 16:
            x138 = e__104.(Lam)._0
            x139 = e__104.(Lam)._1
            body__108 = x139
            x__107 = x138
            ty_x__109 = newvar(st__102)
            t426 = EnvEntry{
                name: x__107,
                ty: ty_x__109,
            }
            env2__110 = append(env__103, t426)
            mtmp154 = typeof(st__102, env2__110, body__108)
            switch mtmp154.(type) {
            case Result__Typ__string_Ok:
                pc = 18
            case Result__Typ__string_Err:
                pc = 19
            default:
                panic("non-exhaustive match")
            }
        case 17:
            jp410 = jp428
            pc = 1
        case 18:
            x155 = mtmp154.(Result__Typ__string_Ok)._0
            ty_e__111 = x155
            t429 = TArrow{
                _0: ty_x__109,
                _1: ty_e__111,
            }
            t430 = Result__Typ__string_Ok{
                _0: t429,
            }
            jp428 = t430
            pc = 17
        case 19:
            x156 = mtmp154.(Result__Typ__string_Err)._0
            e__112 = x156
            t431 = Result__Typ__string_Err{
                _0: e__112,
            }
            jp428 = t431
            pc = 17
        case 20:
            x140 = e__104.(Let)._0
            x141 = e__104.(Let)._1
            x142 = e__104.(Let)._2
            e2__124 = x142
            e1__123 = x141
            x__122 = x140
            enter_level(st__102)
            ty_e__125 = typeof(st__102, env__103, e1__123)
            leave_level(st__102)
            switch ty_e__125.(type) {
            case Result__Typ__string_Ok:
                pc = 22
            case Result__Typ__string_Err:
                pc = 23
            default:
                panic("non-exhaustive match")
            }
        case 21:
            jp410 = jp433
            pc = 1
        case 22:
            x159 = ty_e__125.(Result__Typ__string_Ok)._0
            ty1__127 = x159
            t434 = gen(st__102, ty1__127)
            t435 = EnvEntry{
                name: x__122,
                ty: t434,
            }
            env2__128 = append(env__103, t435)
            t436 = typeof(st__102, env2__128, e2__124)
            jp433 = t436
            pc = 21
        case 23:
            x160 = ty_e__125.(Result__Typ__string_Err)._0
            e__126 = x160
            t437 = Result__Typ__string_Err{
                _0: e__126,
            }
            jp433 = t437
            pc = 21
        default:
            panic("invalid pc")
        }
    }
}

func exp_var(name__129 string) Exp {
    var t438 Exp
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t438 = Var{
                _0: name__129,
            }
            return t438
        default:
            panic("invalid pc")
        }
    }
}

func exp_lam(name__130 string, body__131 Exp) Exp {
    var t439 Exp
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t439 = Lam{
                _0: name__130,
                _1: body__131,
            }
            return t439
        default:
            panic("invalid pc")
        }
    }
}

func exp_app(a__132 Exp, b__133 Exp) Exp {
    var t440 Exp
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t440 = App{
                _0: a__132,
                _1: b__133,
            }
            return t440
        default:
            panic("invalid pc")
        }
    }
}

func exp_let(name__134 string, a__135 Exp, b__136 Exp) Exp {
    var t441 Exp
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t441 = Let{
                _0: name__134,
                _1: a__135,
                _2: b__136,
            }
            return t441
        default:
            panic("invalid pc")
        }
    }
}

func show_result(label__137 string, res__138 Result__Typ__string) struct{} {
    var x161 Typ
    var ty__139 Typ
    var t443 string
    var t444 string
    var t445 string
    var t446 struct{}
    var x162 string
    var e__140 string
    var t447 string
    var t448 string
    var t449 struct{}
    _ = t446
    _ = t449
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            switch res__138.(type) {
            case Result__Typ__string_Ok:
                pc = 2
            case Result__Typ__string_Err:
                pc = 3
            default:
                panic("non-exhaustive match")
            }
        case 1:
            return struct{}{}
        case 2:
            x161 = res__138.(Result__Typ__string_Ok)._0
            ty__139 = x161
            t443 = label__137 + ": "
            t444 = typ_to_string(ty__139)
            t445 = t443 + t444
            string_println(t445)
            pc = 1
        case 3:
            x162 = res__138.(Result__Typ__string_Err)._0
            e__140 = x162
            t447 = label__137 + ": "
            t448 = t447 + e__140
            string_println(t448)
            pc = 1
        default:
            panic("invalid pc")
        }
    }
}

func main0() struct{} {
    var st__141 CheckerState
    var t450 Exp
    var id__142 Exp
    var t451 Exp
    var t452 Exp
    var t453 Exp
    var t454 Exp
    var c1__143 Exp
    var _wild163 struct{}
    var t455 []EnvEntry
    var t456 Result__Typ__string
    var _wild164 struct{}
    var _wild165 struct{}
    var t457 []EnvEntry
    var t458 Result__Typ__string
    var _wild166 struct{}
    var _wild167 struct{}
    var t459 []EnvEntry
    var t460 Exp
    var t461 Exp
    var t462 Result__Typ__string
    var _wild168 struct{}
    var _wild169 struct{}
    var t463 []EnvEntry
    var t464 Exp
    var t465 Exp
    var t466 Exp
    var t467 Exp
    var t468 Result__Typ__string
    var _wild170 struct{}
    var _wild171 struct{}
    var t469 []EnvEntry
    var t470 Exp
    var t471 Exp
    var t472 Exp
    var t473 Exp
    var t474 Exp
    var t475 Result__Typ__string
    var _wild172 struct{}
    var _wild173 struct{}
    var t476 []EnvEntry
    var t477 Exp
    var t478 Exp
    var t479 Exp
    var t480 Exp
    var t481 Exp
    var t482 Exp
    var t483 Exp
    var t484 Result__Typ__string
    var _wild174 struct{}
    var _wild175 struct{}
    var t485 []EnvEntry
    var t486 Exp
    var t487 Exp
    var t488 Exp
    var t489 Exp
    var t490 Result__Typ__string
    var _wild176 struct{}
    var _wild177 struct{}
    var t491 []EnvEntry
    var t492 Exp
    var t493 Exp
    var t494 Exp
    var t495 Result__Typ__string
    var _wild178 struct{}
    var _wild179 struct{}
    var t496 []EnvEntry
    var t497 Exp
    var t498 Exp
    var t499 Exp
    var t500 Exp
    var t501 Exp
    var t502 Exp
    var t503 Exp
    var t504 Result__Typ__string
    var _wild180 struct{}
    var _wild181 struct{}
    var t505 []EnvEntry
    var t506 Exp
    var t507 Exp
    var t508 Exp
    var t509 Exp
    var t510 Exp
    var t511 Exp
    var t512 Exp
    var t513 Exp
    var t514 Exp
    var t515 Exp
    var t516 Exp
    var t517 Exp
    var t518 Exp
    var t519 Exp
    var t520 Exp
    var t521 Exp
    var t522 Result__Typ__string
    var _wild182 struct{}
    var _wild183 struct{}
    var t523 []EnvEntry
    var t524 Exp
    var t525 Exp
    var t526 Exp
    var t527 Exp
    var t528 Result__Typ__string
    var _wild184 struct{}
    var _wild185 struct{}
    var t529 []EnvEntry
    var t530 Exp
    var t531 Exp
    var t532 Exp
    var t533 Exp
    var t534 Exp
    var t535 Exp
    var t536 Exp
    var t537 Result__Typ__string
    var _wild186 struct{}
    var _wild187 struct{}
    var t538 []EnvEntry
    var t539 Exp
    var t540 Exp
    var t541 Exp
    var t542 Exp
    var t543 Exp
    var t544 Exp
    var t545 Exp
    var t546 Exp
    var t547 Exp
    var t548 Exp
    var t549 Result__Typ__string
    var _wild188 struct{}
    var _wild189 struct{}
    var t550 []EnvEntry
    var t551 Exp
    var t552 Exp
    var t553 Exp
    var t554 Exp
    var t555 Result__Typ__string
    var _wild190 struct{}
    var _wild191 struct{}
    var t556 []EnvEntry
    var t557 Exp
    var t558 Exp
    var t559 Exp
    var t560 Exp
    var t561 Exp
    var t562 Result__Typ__string
    var _wild192 struct{}
    var _wild193 struct{}
    var t563 []EnvEntry
    var t564 Exp
    var t565 Exp
    var t566 Exp
    var t567 Exp
    var t568 Exp
    var t569 Exp
    var t570 Exp
    var t571 Result__Typ__string
    var _wild194 struct{}
    var _wild195 struct{}
    var t572 []EnvEntry
    var t573 Exp
    var t574 Exp
    var t575 Exp
    var t576 Exp
    var t577 Exp
    var t578 Exp
    var t579 Exp
    var t580 Exp
    var t581 Exp
    var t582 Result__Typ__string
    var _wild196 struct{}
    var _wild197 struct{}
    var t583 []EnvEntry
    var t584 Exp
    var t585 Exp
    var t586 Exp
    var t587 Exp
    var t588 Exp
    var t589 Exp
    var t590 Result__Typ__string
    var _wild198 struct{}
    var _wild199 struct{}
    var t591 []EnvEntry
    var t592 Exp
    var t593 Exp
    var t594 Exp
    var t595 Exp
    var t596 Exp
    var t597 Exp
    var t598 Exp
    var t599 Result__Typ__string
    var _wild200 struct{}
    var _wild201 struct{}
    var _wild202 struct{}
    var _wild203 struct{}
    _ = _wild163
    _ = _wild164
    _ = _wild165
    _ = _wild166
    _ = _wild167
    _ = _wild168
    _ = _wild169
    _ = _wild170
    _ = _wild171
    _ = _wild172
    _ = _wild173
    _ = _wild174
    _ = _wild175
    _ = _wild176
    _ = _wild177
    _ = _wild178
    _ = _wild179
    _ = _wild180
    _ = _wild181
    _ = _wild182
    _ = _wild183
    _ = _wild184
    _ = _wild185
    _ = _wild186
    _ = _wild187
    _ = _wild188
    _ = _wild189
    _ = _wild190
    _ = _wild191
    _ = _wild192
    _ = _wild193
    _ = _wild194
    _ = _wild195
    _ = _wild196
    _ = _wild197
    _ = _wild198
    _ = _wild199
    _ = _wild200
    _ = _wild201
    _ = _wild202
    _ = _wild203
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            st__141 = state_new()
            t450 = exp_var("x")
            id__142 = exp_lam("x", t450)
            t451 = exp_var("x")
            t452 = exp_var("y")
            t453 = exp_app(t451, t452)
            t454 = exp_lam("y", t453)
            c1__143 = exp_lam("x", t454)
            reset_type_variables(st__141)
            t455 = env_empty()
            t456 = typeof(st__141, t455, id__142)
            show_result("id", t456)
            reset_type_variables(st__141)
            t457 = env_empty()
            t458 = typeof(st__141, t457, c1__143)
            show_result("c1", t458)
            reset_type_variables(st__141)
            t459 = env_empty()
            t460 = exp_var("x")
            t461 = exp_let("x", c1__143, t460)
            t462 = typeof(st__141, t459, t461)
            show_result("let_x_c1_x", t462)
            reset_type_variables(st__141)
            t463 = env_empty()
            t464 = exp_var("z")
            t465 = exp_lam("z", t464)
            t466 = exp_var("y")
            t467 = exp_let("y", t465, t466)
            t468 = typeof(st__141, t463, t467)
            show_result("let_y_id_y", t468)
            reset_type_variables(st__141)
            t469 = env_empty()
            t470 = exp_var("z")
            t471 = exp_lam("z", t470)
            t472 = exp_var("y")
            t473 = exp_let("y", t471, t472)
            t474 = exp_lam("x", t473)
            t475 = typeof(st__141, t469, t474)
            show_result("lam_x_let_y_id_y", t475)
            reset_type_variables(st__141)
            t476 = env_empty()
            t477 = exp_var("z")
            t478 = exp_lam("z", t477)
            t479 = exp_var("y")
            t480 = exp_var("x")
            t481 = exp_app(t479, t480)
            t482 = exp_let("y", t478, t481)
            t483 = exp_lam("x", t482)
            t484 = typeof(st__141, t476, t483)
            show_result("lam_x_let_y_id_yx", t484)
            reset_type_variables(st__141)
            t485 = env_empty()
            t486 = exp_var("x")
            t487 = exp_var("x")
            t488 = exp_app(t486, t487)
            t489 = exp_lam("x", t488)
            t490 = typeof(st__141, t485, t489)
            show_result("self_apply", t490)
            reset_type_variables(st__141)
            t491 = env_empty()
            t492 = exp_var("x")
            t493 = exp_var("x")
            t494 = exp_let("x", t492, t493)
            t495 = typeof(st__141, t491, t494)
            show_result("unbound_var", t495)
            reset_type_variables(st__141)
            t496 = env_empty()
            t497 = exp_var("y")
            t498 = exp_var("y")
            t499 = exp_var("z")
            t500 = exp_app(t498, t499)
            t501 = exp_lam("z", t500)
            t502 = exp_app(t497, t501)
            t503 = exp_lam("y", t502)
            t504 = typeof(st__141, t496, t503)
            show_result("max_heiber", t504)
            reset_type_variables(st__141)
            t505 = env_empty()
            t506 = exp_var("k")
            t507 = exp_var("k")
            t508 = exp_var("x")
            t509 = exp_app(t507, t508)
            t510 = exp_var("y")
            t511 = exp_app(t509, t510)
            t512 = exp_app(t506, t511)
            t513 = exp_var("k")
            t514 = exp_var("y")
            t515 = exp_app(t513, t514)
            t516 = exp_var("x")
            t517 = exp_app(t515, t516)
            t518 = exp_app(t512, t517)
            t519 = exp_lam("k", t518)
            t520 = exp_lam("y", t519)
            t521 = exp_lam("x", t520)
            t522 = typeof(st__141, t505, t521)
            show_result("kirang", t522)
            reset_type_variables(st__141)
            t523 = env_empty()
            t524 = exp_var("id")
            t525 = exp_var("id")
            t526 = exp_app(t524, t525)
            t527 = exp_let("id", id__142, t526)
            t528 = typeof(st__141, t523, t527)
            show_result("let_id_idid", t528)
            reset_type_variables(st__141)
            t529 = env_empty()
            t530 = exp_var("x")
            t531 = exp_app(t530, id__142)
            t532 = exp_var("z")
            t533 = exp_let("z", t531, t532)
            t534 = exp_var("y")
            t535 = exp_let("y", t533, t534)
            t536 = exp_let("x", c1__143, t535)
            t537 = typeof(st__141, t529, t536)
            show_result("nested_lets", t537)
            reset_type_variables(st__141)
            t538 = env_empty()
            t539 = exp_var("x")
            t540 = exp_var("y")
            t541 = exp_app(t539, t540)
            t542 = exp_var("y")
            t543 = exp_var("x")
            t544 = exp_app(t542, t543)
            t545 = exp_lam("x", t544)
            t546 = exp_let("x", t541, t545)
            t547 = exp_lam("y", t546)
            t548 = exp_lam("x", t547)
            t549 = typeof(st__141, t538, t548)
            show_result("fun_x_fun_y_let_x_xy_fun_x_yx", t549)
            reset_type_variables(st__141)
            t550 = env_empty()
            t551 = exp_var("x")
            t552 = exp_var("y")
            t553 = exp_let("y", t551, t552)
            t554 = exp_lam("x", t553)
            t555 = typeof(st__141, t550, t554)
            show_result("sound_gen_1", t555)
            reset_type_variables(st__141)
            t556 = env_empty()
            t557 = exp_var("x")
            t558 = exp_lam("z", t557)
            t559 = exp_var("y")
            t560 = exp_let("y", t558, t559)
            t561 = exp_lam("x", t560)
            t562 = typeof(st__141, t556, t561)
            show_result("sound_gen_2", t562)
            reset_type_variables(st__141)
            t563 = env_empty()
            t564 = exp_var("x")
            t565 = exp_var("z")
            t566 = exp_app(t564, t565)
            t567 = exp_lam("z", t566)
            t568 = exp_var("y")
            t569 = exp_let("y", t567, t568)
            t570 = exp_lam("x", t569)
            t571 = typeof(st__141, t563, t570)
            show_result("sound_gen_3", t571)
            reset_type_variables(st__141)
            t572 = env_empty()
            t573 = exp_var("x")
            t574 = exp_var("y")
            t575 = exp_app(t573, t574)
            t576 = exp_var("x")
            t577 = exp_var("y")
            t578 = exp_app(t576, t577)
            t579 = exp_let("x", t575, t578)
            t580 = exp_lam("y", t579)
            t581 = exp_lam("x", t580)
            t582 = typeof(st__141, t572, t581)
            show_result("double_apply", t582)
            reset_type_variables(st__141)
            t583 = env_empty()
            t584 = exp_var("x")
            t585 = exp_var("y")
            t586 = exp_var("y")
            t587 = exp_app(t585, t586)
            t588 = exp_let("y", t584, t587)
            t589 = exp_lam("x", t588)
            t590 = typeof(st__141, t583, t589)
            show_result("sound_gen_occurs", t590)
            reset_gensym(st__141)
            t591 = env_empty()
            t592 = exp_var("x")
            t593 = exp_app(t592, id__142)
            t594 = exp_var("z")
            t595 = exp_let("z", t593, t594)
            t596 = exp_var("y")
            t597 = exp_let("y", t595, t596)
            t598 = exp_lam("x", t597)
            t599 = typeof(st__141, t591, t598)
            show_result("fun_x_let_y_let_z_x_id_z_y", t599)
            string_println("")
            string_println("All Done")
            string_println("")
            return struct{}{}
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
