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
    var ret438 CheckerState
    var t207 *ref_int32_x = ref__Ref_int32(0)
    var t208 *ref_int32_x = ref__Ref_int32(1)
    ret438 = CheckerState{
        gensym_counter: t207,
        current_level: t208,
    }
    return ret438
}

func reset_gensym(st__0 CheckerState) struct{} {
    var ret439 struct{}
    var t209 *ref_int32_x = st__0.gensym_counter
    ref_set__Ref_int32(t209, 0)
    ret439 = struct{}{}
    return ret439
}

func reset_level(st__1 CheckerState) struct{} {
    var ret440 struct{}
    var t210 *ref_int32_x = st__1.current_level
    ref_set__Ref_int32(t210, 1)
    ret440 = struct{}{}
    return ret440
}

func reset_type_variables(st__2 CheckerState) struct{} {
    var ret441 struct{}
    reset_gensym(st__2)
    ret441 = reset_level(st__2)
    return ret441
}

func enter_level(st__3 CheckerState) struct{} {
    var ret442 struct{}
    var t211 *ref_int32_x = st__3.current_level
    var l__4 int32 = ref_get__Ref_int32(t211)
    var t212 *ref_int32_x = st__3.current_level
    var t213 int32 = l__4 + 1
    ref_set__Ref_int32(t212, t213)
    ret442 = struct{}{}
    return ret442
}

func leave_level(st__5 CheckerState) struct{} {
    var ret443 struct{}
    var t214 *ref_int32_x = st__5.current_level
    var l__6 int32 = ref_get__Ref_int32(t214)
    var t215 *ref_int32_x = st__5.current_level
    var t216 int32 = l__6 - 1
    ref_set__Ref_int32(t215, t216)
    ret443 = struct{}{}
    return ret443
}

func min_i32(a__7 int32, b__8 int32) int32 {
    var ret444 int32
    var t217 bool = a__7 < b__8
    if t217 {
        ret444 = a__7
    } else {
        ret444 = b__8
    }
    return ret444
}

func nth_letter(n__9 int32) rune {
    var ret445 rune
    switch n__9 {
    case 0:
        ret445 = 97
    case 1:
        ret445 = 98
    case 2:
        ret445 = 99
    case 3:
        ret445 = 100
    case 4:
        ret445 = 101
    case 5:
        ret445 = 102
    case 6:
        ret445 = 103
    case 7:
        ret445 = 104
    case 8:
        ret445 = 105
    case 9:
        ret445 = 106
    case 10:
        ret445 = 107
    case 11:
        ret445 = 108
    case 12:
        ret445 = 109
    case 13:
        ret445 = 110
    case 14:
        ret445 = 111
    case 15:
        ret445 = 112
    case 16:
        ret445 = 113
    case 17:
        ret445 = 114
    case 18:
        ret445 = 115
    case 19:
        ret445 = 116
    case 20:
        ret445 = 117
    case 21:
        ret445 = 118
    case 22:
        ret445 = 119
    case 23:
        ret445 = 120
    case 24:
        ret445 = 121
    case 25:
        ret445 = 122
    default:
        ret445 = 97
    }
    return ret445
}

func gensym(st__10 CheckerState) string {
    var ret446 string
    var t218 *ref_int32_x = st__10.gensym_counter
    var n__11 int32 = ref_get__Ref_int32(t218)
    var t219 *ref_int32_x = st__10.gensym_counter
    var t220 int32 = n__11 + 1
    ref_set__Ref_int32(t219, t220)
    var t221 bool = n__11 < 26
    if t221 {
        var t222 rune = nth_letter(n__11)
        ret446 = char_to_string(t222)
    } else {
        var t223 string = int32_to_string(n__11)
        ret446 = "t" + t223
    }
    return ret446
}

func newvar(st__12 CheckerState) Typ {
    var ret447 Typ
    var name__13 string = gensym(st__12)
    var t224 *ref_int32_x = st__12.current_level
    var level__14 int32 = ref_get__Ref_int32(t224)
    var t226 Tv = Unbound{
        _0: name__13,
        _1: level__14,
    }
    var t225 *ref_tv_x = ref__Ref_Tv(t226)
    ret447 = TVar{
        _0: t225,
    }
    return ret447
}

func typ_is_arrow(ty__15 Typ) bool {
    var ret448 bool
    switch ty__15 := ty__15.(type) {
    case TVar:
        var x6 *ref_tv_x = ty__15._0
        var tvref__16 *ref_tv_x = x6
        var mtmp10 Tv = ref_get__Ref_Tv(tvref__16)
        switch mtmp10 := mtmp10.(type) {
        case Unbound:
            ret448 = false
        case Link:
            var x13 Typ = mtmp10._0
            var inner__17 Typ = x13
            ret448 = typ_is_arrow(inner__17)
        }
    case QVar:
        ret448 = false
    case TArrow:
        ret448 = true
    }
    return ret448
}

func typ_to_string(ty__18 Typ) string {
    var ret449 string
    switch ty__18 := ty__18.(type) {
    case TVar:
        var x14 *ref_tv_x = ty__18._0
        var tvref__20 *ref_tv_x = x14
        var mtmp18 Tv = ref_get__Ref_Tv(tvref__20)
        switch mtmp18 := mtmp18.(type) {
        case Unbound:
            var x19 string = mtmp18._0
            var name__21 string = x19
            ret449 = "'" + name__21
        case Link:
            var x21 Typ = mtmp18._0
            var inner__22 Typ = x21
            ret449 = typ_to_string(inner__22)
        }
    case QVar:
        var x15 string = ty__18._0
        var name__19 string = x15
        ret449 = "'" + name__19
    case TArrow:
        var x16 Typ = ty__18._0
        var x17 Typ = ty__18._1
        var t2__24 Typ = x17
        var t1__23 Typ = x16
        var t227 bool = typ_is_arrow(t1__23)
        var s1__25 string
        if t227 {
            var t229 string = typ_to_string(t1__23)
            var t228 string = "(" + t229
            s1__25 = t228 + ")"
        } else {
            s1__25 = typ_to_string(t1__23)
        }
        var s2__26 string = typ_to_string(t2__24)
        var t230 string = s1__25 + " -> "
        ret449 = t230 + s2__26
    }
    return ret449
}

func env_empty() []EnvEntry {
    var ret450 []EnvEntry
    var env__27 []EnvEntry = nil
    ret450 = env__27
    return ret450
}

func env_lookup(env__28 []EnvEntry, name__29 string) Option__Typ {
    var ret451 Option__Typ
    var t232 int32 = int32(len(env__28))
    var t231 int32 = t232 - 1
    var i__30 *ref_int32_x = ref__Ref_int32(t231)
    var t233 Option__Typ = None{}
    var found__31 *ref_option__typ_x = ref__Ref_Option__Typ(t233)
    var done__32 *ref_bool_x = ref__Ref_bool(false)
    var cond452 bool
    for {
        var t235 bool = ref_get__Ref_bool(done__32)
        var t234 bool = !t235
        var t237 int32 = ref_get__Ref_int32(i__30)
        var t236 bool = t237 >= 0
        cond452 = t234 && t236
        if !cond452 {
            break
        }
        var t238 int32 = ref_get__Ref_int32(i__30)
        var entry__33 EnvEntry = env__28[t238]
        var t240 string = entry__33.name
        var t239 bool = t240 == name__29
        if t239 {
            var t242 Typ = entry__33.ty
            var t241 Option__Typ = Some{
                _0: t242,
            }
            ref_set__Ref_Option__Typ(found__31, t241)
            ref_set__Ref_bool(done__32, true)
        } else {
            var t244 int32 = ref_get__Ref_int32(i__30)
            var t243 int32 = t244 - 1
            ref_set__Ref_int32(i__30, t243)
        }
    }
    ret451 = ref_get__Ref_Option__Typ(found__31)
    return ret451
}

func subst_lookup(subst__34 []SubstEntry, name__35 string) Option__Typ {
    var ret453 Option__Typ
    var t246 int32 = int32(len(subst__34))
    var t245 int32 = t246 - 1
    var i__36 *ref_int32_x = ref__Ref_int32(t245)
    var t247 Option__Typ = None{}
    var found__37 *ref_option__typ_x = ref__Ref_Option__Typ(t247)
    var done__38 *ref_bool_x = ref__Ref_bool(false)
    var cond454 bool
    for {
        var t249 bool = ref_get__Ref_bool(done__38)
        var t248 bool = !t249
        var t251 int32 = ref_get__Ref_int32(i__36)
        var t250 bool = t251 >= 0
        cond454 = t248 && t250
        if !cond454 {
            break
        }
        var t252 int32 = ref_get__Ref_int32(i__36)
        var entry__39 SubstEntry = subst__34[t252]
        var t254 string = entry__39.name
        var t253 bool = t254 == name__35
        if t253 {
            var t256 Typ = entry__39.ty
            var t255 Option__Typ = Some{
                _0: t256,
            }
            ref_set__Ref_Option__Typ(found__37, t255)
            ref_set__Ref_bool(done__38, true)
        } else {
            var t258 int32 = ref_get__Ref_int32(i__36)
            var t257 int32 = t258 - 1
            ref_set__Ref_int32(i__36, t257)
        }
    }
    ret453 = ref_get__Ref_Option__Typ(found__37)
    return ret453
}

func occurs(st__40 CheckerState, tvr__41 *ref_tv_x, ty__42 Typ) Result__unit__string {
    var ret455 Result__unit__string
    switch ty__42 := ty__42.(type) {
    case TVar:
        var x26 *ref_tv_x = ty__42._0
        var tvr2__43 *ref_tv_x = x26
        var t259 bool = ptr_eq__Ref_Tv(tvr__41, tvr2__43)
        if t259 {
            ret455 = Result__unit__string_Err{
                _0: "occurs check",
            }
        } else {
            var mtmp30 Tv = ref_get__Ref_Tv(tvr2__43)
            switch mtmp30 := mtmp30.(type) {
            case Unbound:
                var x31 string = mtmp30._0
                var x32 int32 = mtmp30._1
                var l2__45 int32 = x32
                var name__44 string = x31
                var mtmp35 Tv = ref_get__Ref_Tv(tvr__41)
                var min_level__47 int32
                switch mtmp35 := mtmp35.(type) {
                case Unbound:
                    var x37 int32 = mtmp35._1
                    var l__46 int32 = x37
                    min_level__47 = min_i32(l__46, l2__45)
                case Link:
                    min_level__47 = l2__45
                }
                var t260 Tv = Unbound{
                    _0: name__44,
                    _1: min_level__47,
                }
                ref_set__Ref_Tv(tvr2__43, t260)
                ret455 = Result__unit__string_Ok{
                    _0: struct{}{},
                }
            case Link:
                var x33 Typ = mtmp30._0
                var inner__48 Typ = x33
                ret455 = occurs(st__40, tvr__41, inner__48)
            }
        }
    case QVar:
        ret455 = Result__unit__string_Ok{
            _0: struct{}{},
        }
    case TArrow:
        var x28 Typ = ty__42._0
        var x29 Typ = ty__42._1
        var t2__50 Typ = x29
        var t1__49 Typ = x28
        var mtmp39 Result__unit__string = occurs(st__40, tvr__41, t1__49)
        switch mtmp39 := mtmp39.(type) {
        case Result__unit__string_Ok:
            ret455 = occurs(st__40, tvr__41, t2__50)
        case Result__unit__string_Err:
            var x41 string = mtmp39._0
            var e__51 string = x41
            ret455 = Result__unit__string_Err{
                _0: e__51,
            }
        }
    }
    return ret455
}

func unify(st__52 CheckerState, t1__53 Typ, t2__54 Typ) Result__unit__string {
    var ret456 Result__unit__string
    var mtmp42 Tuple2_Typ_Typ = Tuple2_Typ_Typ{
        _0: t1__53,
        _1: t2__54,
    }
    var x43 Typ = mtmp42._0
    var x44 Typ = mtmp42._1
    switch x44 := x44.(type) {
    case TVar:
        var x45 *ref_tv_x = x44._0
        switch x43 := x43.(type) {
        case TVar:
            var x49 *ref_tv_x = x43._0
            var r1__55 *ref_tv_x = x49
            var r2__56 *ref_tv_x = x45
            var t261 bool = ptr_eq__Ref_Tv(r1__55, r2__56)
            if t261 {
                ret456 = Result__unit__string_Ok{
                    _0: struct{}{},
                }
            } else {
                var mtmp53 Tv = ref_get__Ref_Tv(r1__55)
                switch mtmp53 := mtmp53.(type) {
                case Unbound:
                    var mtmp57 Tv = ref_get__Ref_Tv(r2__56)
                    switch mtmp57 := mtmp57.(type) {
                    case Unbound:
                        var t262 Typ = TVar{
                            _0: r2__56,
                        }
                        var mtmp61 Result__unit__string = occurs(st__52, r1__55, t262)
                        switch mtmp61 := mtmp61.(type) {
                        case Result__unit__string_Ok:
                            var t264 Typ = TVar{
                                _0: r2__56,
                            }
                            var t263 Tv = Link{
                                _0: t264,
                            }
                            ref_set__Ref_Tv(r1__55, t263)
                            ret456 = Result__unit__string_Ok{
                                _0: struct{}{},
                            }
                        case Result__unit__string_Err:
                            var x63 string = mtmp61._0
                            var e__59 string = x63
                            ret456 = Result__unit__string_Err{
                                _0: e__59,
                            }
                        }
                    case Link:
                        var x60 Typ = mtmp57._0
                        var inner__58 Typ = x60
                        var t265 Typ = TVar{
                            _0: r1__55,
                        }
                        ret456 = unify(st__52, t265, inner__58)
                    }
                case Link:
                    var x56 Typ = mtmp53._0
                    var inner__57 Typ = x56
                    var t266 Typ = TVar{
                        _0: r2__56,
                    }
                    ret456 = unify(st__52, inner__57, t266)
                }
            }
        case QVar:
            var r2__65 *ref_tv_x = x45
            var other__64 Typ = x43
            var mtmp65 Tv = ref_get__Ref_Tv(r2__65)
            switch mtmp65 := mtmp65.(type) {
            case Unbound:
                var mtmp69 Result__unit__string = occurs(st__52, r2__65, other__64)
                switch mtmp69 := mtmp69.(type) {
                case Result__unit__string_Ok:
                    var t267 Tv = Link{
                        _0: other__64,
                    }
                    ref_set__Ref_Tv(r2__65, t267)
                    ret456 = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                case Result__unit__string_Err:
                    var x71 string = mtmp69._0
                    var e__67 string = x71
                    ret456 = Result__unit__string_Err{
                        _0: e__67,
                    }
                }
            case Link:
                var x68 Typ = mtmp65._0
                var inner__66 Typ = x68
                ret456 = unify(st__52, other__64, inner__66)
            }
        case TArrow:
            var r2__65 *ref_tv_x = x45
            var other__64 Typ = x43
            var mtmp73 Tv = ref_get__Ref_Tv(r2__65)
            switch mtmp73 := mtmp73.(type) {
            case Unbound:
                var mtmp77 Result__unit__string = occurs(st__52, r2__65, other__64)
                switch mtmp77 := mtmp77.(type) {
                case Result__unit__string_Ok:
                    var t268 Tv = Link{
                        _0: other__64,
                    }
                    ref_set__Ref_Tv(r2__65, t268)
                    ret456 = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                case Result__unit__string_Err:
                    var x79 string = mtmp77._0
                    var e__67 string = x79
                    ret456 = Result__unit__string_Err{
                        _0: e__67,
                    }
                }
            case Link:
                var x76 Typ = mtmp73._0
                var inner__66 Typ = x76
                ret456 = unify(st__52, other__64, inner__66)
            }
        }
    case QVar:
        switch x43 := x43.(type) {
        case TVar:
            var x81 *ref_tv_x = x43._0
            var r1__60 *ref_tv_x = x81
            var other__61 Typ = x44
            var mtmp85 Tv = ref_get__Ref_Tv(r1__60)
            switch mtmp85 := mtmp85.(type) {
            case Unbound:
                var mtmp89 Result__unit__string = occurs(st__52, r1__60, other__61)
                switch mtmp89 := mtmp89.(type) {
                case Result__unit__string_Ok:
                    var t269 Tv = Link{
                        _0: other__61,
                    }
                    ref_set__Ref_Tv(r1__60, t269)
                    ret456 = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                case Result__unit__string_Err:
                    var x91 string = mtmp89._0
                    var e__63 string = x91
                    ret456 = Result__unit__string_Err{
                        _0: e__63,
                    }
                }
            case Link:
                var x88 Typ = mtmp85._0
                var inner__62 Typ = x88
                ret456 = unify(st__52, inner__62, other__61)
            }
        case QVar:
            ret456 = Result__unit__string_Err{
                _0: "unify error",
            }
        case TArrow:
            ret456 = Result__unit__string_Err{
                _0: "unify error",
            }
        }
    case TArrow:
        var x47 Typ = x44._0
        var x48 Typ = x44._1
        switch x43 := x43.(type) {
        case TVar:
            var x93 *ref_tv_x = x43._0
            var r1__60 *ref_tv_x = x93
            var other__61 Typ = x44
            var mtmp97 Tv = ref_get__Ref_Tv(r1__60)
            switch mtmp97 := mtmp97.(type) {
            case Unbound:
                var mtmp101 Result__unit__string = occurs(st__52, r1__60, other__61)
                switch mtmp101 := mtmp101.(type) {
                case Result__unit__string_Ok:
                    var t270 Tv = Link{
                        _0: other__61,
                    }
                    ref_set__Ref_Tv(r1__60, t270)
                    ret456 = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                case Result__unit__string_Err:
                    var x103 string = mtmp101._0
                    var e__63 string = x103
                    ret456 = Result__unit__string_Err{
                        _0: e__63,
                    }
                }
            case Link:
                var x100 Typ = mtmp97._0
                var inner__62 Typ = x100
                ret456 = unify(st__52, inner__62, other__61)
            }
        case QVar:
            ret456 = Result__unit__string_Err{
                _0: "unify error",
            }
        case TArrow:
            var x95 Typ = x43._0
            var x96 Typ = x43._1
            var a2__69 Typ = x96
            var a1__68 Typ = x95
            var b2__71 Typ = x48
            var b1__70 Typ = x47
            var mtmp105 Result__unit__string = unify(st__52, a1__68, b1__70)
            switch mtmp105 := mtmp105.(type) {
            case Result__unit__string_Ok:
                ret456 = unify(st__52, a2__69, b2__71)
            case Result__unit__string_Err:
                var x107 string = mtmp105._0
                var e__72 string = x107
                ret456 = Result__unit__string_Err{
                    _0: e__72,
                }
            }
        }
    }
    return ret456
}

func gen(st__73 CheckerState, ty__74 Typ) Typ {
    var ret457 Typ
    switch ty__74 := ty__74.(type) {
    case TVar:
        var x108 *ref_tv_x = ty__74._0
        var tvref__75 *ref_tv_x = x108
        var mtmp112 Tv = ref_get__Ref_Tv(tvref__75)
        switch mtmp112 := mtmp112.(type) {
        case Unbound:
            var x113 string = mtmp112._0
            var x114 int32 = mtmp112._1
            var l__77 int32 = x114
            var name__76 string = x113
            var t271 *ref_int32_x = st__73.current_level
            var cur__78 int32 = ref_get__Ref_int32(t271)
            var t272 bool = l__77 > cur__78
            if t272 {
                ret457 = QVar{
                    _0: name__76,
                }
            } else {
                ret457 = TVar{
                    _0: tvref__75,
                }
            }
        case Link:
            var x115 Typ = mtmp112._0
            var inner__79 Typ = x115
            ret457 = gen(st__73, inner__79)
        }
    case QVar:
        var other__82 Typ = ty__74
        ret457 = other__82
    case TArrow:
        var x110 Typ = ty__74._0
        var x111 Typ = ty__74._1
        var t2__81 Typ = x111
        var t1__80 Typ = x110
        var t273 Typ = gen(st__73, t1__80)
        var t274 Typ = gen(st__73, t2__81)
        ret457 = TArrow{
            _0: t273,
            _1: t274,
        }
    }
    return ret457
}

func inst_loop(st__83 CheckerState, subst__84 []SubstEntry, ty__85 Typ) Tuple2_Typ_Vec_SubstEntry {
    var ret458 Tuple2_Typ_Vec_SubstEntry
    switch ty__85 := ty__85.(type) {
    case TVar:
        var x116 *ref_tv_x = ty__85._0
        var tvref__90 *ref_tv_x = x116
        var mtmp120 Tv = ref_get__Ref_Tv(tvref__90)
        switch mtmp120 := mtmp120.(type) {
        case Unbound:
            var t275 Typ = TVar{
                _0: tvref__90,
            }
            ret458 = Tuple2_Typ_Vec_SubstEntry{
                _0: t275,
                _1: subst__84,
            }
        case Link:
            var x123 Typ = mtmp120._0
            var inner__91 Typ = x123
            ret458 = inst_loop(st__83, subst__84, inner__91)
        }
    case QVar:
        var x117 string = ty__85._0
        var name__86 string = x117
        var mtmp124 Option__Typ = subst_lookup(subst__84, name__86)
        switch mtmp124 := mtmp124.(type) {
        case None:
            var tv__88 Typ = newvar(st__83)
            var t276 SubstEntry = SubstEntry{
                name: name__86,
                ty: tv__88,
            }
            var new_subst__89 []SubstEntry = append(subst__84, t276)
            ret458 = Tuple2_Typ_Vec_SubstEntry{
                _0: tv__88,
                _1: new_subst__89,
            }
        case Some:
            var x125 Typ = mtmp124._0
            var t__87 Typ = x125
            ret458 = Tuple2_Typ_Vec_SubstEntry{
                _0: t__87,
                _1: subst__84,
            }
        }
    case TArrow:
        var x118 Typ = ty__85._0
        var x119 Typ = ty__85._1
        var t2__93 Typ = x119
        var t1__92 Typ = x118
        var mtmp129 Tuple2_Typ_Vec_SubstEntry = inst_loop(st__83, subst__84, t1__92)
        var x130 Typ = mtmp129._0
        var x131 []SubstEntry = mtmp129._1
        var subst1__95 []SubstEntry = x131
        var ty1__94 Typ = x130
        var mtmp132 Tuple2_Typ_Vec_SubstEntry = inst_loop(st__83, subst1__95, t2__93)
        var x133 Typ = mtmp132._0
        var x134 []SubstEntry = mtmp132._1
        var subst2__97 []SubstEntry = x134
        var ty2__96 Typ = x133
        var t277 Typ = TArrow{
            _0: ty1__94,
            _1: ty2__96,
        }
        ret458 = Tuple2_Typ_Vec_SubstEntry{
            _0: t277,
            _1: subst2__97,
        }
    }
    return ret458
}

func inst(st__98 CheckerState, ty__99 Typ) Typ {
    var ret459 Typ
    var subst0__100 []SubstEntry = nil
    var mtmp135 Tuple2_Typ_Vec_SubstEntry = inst_loop(st__98, subst0__100, ty__99)
    var x136 Typ = mtmp135._0
    var t__101 Typ = x136
    ret459 = t__101
    return ret459
}

func typeof(st__102 CheckerState, env__103 []EnvEntry, e__104 Exp) Result__Typ__string {
    var ret460 Result__Typ__string
    switch e__104 := e__104.(type) {
    case Var:
        var x138 string = e__104._0
        var x__105 string = x138
        var mtmp146 Option__Typ = env_lookup(env__103, x__105)
        switch mtmp146 := mtmp146.(type) {
        case None:
            ret460 = Result__Typ__string_Err{
                _0: "unbound var",
            }
        case Some:
            var x147 Typ = mtmp146._0
            var ty__106 Typ = x147
            var t278 Typ = inst(st__102, ty__106)
            ret460 = Result__Typ__string_Ok{
                _0: t278,
            }
        }
    case App:
        var x139 Exp = e__104._0
        var x140 Exp = e__104._1
        var e2__114 Exp = x140
        var e1__113 Exp = x139
        var mtmp148 Result__Typ__string = typeof(st__102, env__103, e1__113)
        switch mtmp148 := mtmp148.(type) {
        case Result__Typ__string_Ok:
            var x149 Typ = mtmp148._0
            var ty_fun__116 Typ = x149
            var mtmp151 Result__Typ__string = typeof(st__102, env__103, e2__114)
            switch mtmp151 := mtmp151.(type) {
            case Result__Typ__string_Ok:
                var x152 Typ = mtmp151._0
                var ty_arg__118 Typ = x152
                var ty_res__119 Typ = newvar(st__102)
                var arrow__120 Typ = TArrow{
                    _0: ty_arg__118,
                    _1: ty_res__119,
                }
                var mtmp154 Result__unit__string = unify(st__102, ty_fun__116, arrow__120)
                switch mtmp154 := mtmp154.(type) {
                case Result__unit__string_Ok:
                    ret460 = Result__Typ__string_Ok{
                        _0: ty_res__119,
                    }
                case Result__unit__string_Err:
                    var x156 string = mtmp154._0
                    var e__121 string = x156
                    ret460 = Result__Typ__string_Err{
                        _0: e__121,
                    }
                }
            case Result__Typ__string_Err:
                var x153 string = mtmp151._0
                var e__117 string = x153
                ret460 = Result__Typ__string_Err{
                    _0: e__117,
                }
            }
        case Result__Typ__string_Err:
            var x150 string = mtmp148._0
            var e__115 string = x150
            ret460 = Result__Typ__string_Err{
                _0: e__115,
            }
        }
    case Lam:
        var x141 string = e__104._0
        var x142 Exp = e__104._1
        var body__108 Exp = x142
        var x__107 string = x141
        var ty_x__109 Typ = newvar(st__102)
        var t279 EnvEntry = EnvEntry{
            name: x__107,
            ty: ty_x__109,
        }
        var env2__110 []EnvEntry = append(env__103, t279)
        var mtmp157 Result__Typ__string = typeof(st__102, env2__110, body__108)
        switch mtmp157 := mtmp157.(type) {
        case Result__Typ__string_Ok:
            var x158 Typ = mtmp157._0
            var ty_e__111 Typ = x158
            var t280 Typ = TArrow{
                _0: ty_x__109,
                _1: ty_e__111,
            }
            ret460 = Result__Typ__string_Ok{
                _0: t280,
            }
        case Result__Typ__string_Err:
            var x159 string = mtmp157._0
            var e__112 string = x159
            ret460 = Result__Typ__string_Err{
                _0: e__112,
            }
        }
    case Let:
        var x143 string = e__104._0
        var x144 Exp = e__104._1
        var x145 Exp = e__104._2
        var e2__124 Exp = x145
        var e1__123 Exp = x144
        var x__122 string = x143
        enter_level(st__102)
        var ty_e__125 Result__Typ__string = typeof(st__102, env__103, e1__123)
        leave_level(st__102)
        switch ty_e__125 := ty_e__125.(type) {
        case Result__Typ__string_Ok:
            var x160 Typ = ty_e__125._0
            var ty1__127 Typ = x160
            var t282 Typ = gen(st__102, ty1__127)
            var t281 EnvEntry = EnvEntry{
                name: x__122,
                ty: t282,
            }
            var env2__128 []EnvEntry = append(env__103, t281)
            ret460 = typeof(st__102, env2__128, e2__124)
        case Result__Typ__string_Err:
            var x161 string = ty_e__125._0
            var e__126 string = x161
            ret460 = Result__Typ__string_Err{
                _0: e__126,
            }
        }
    }
    return ret460
}

func exp_var(name__129 string) Exp {
    var ret461 Exp
    ret461 = Var{
        _0: name__129,
    }
    return ret461
}

func exp_lam(name__130 string, body__131 Exp) Exp {
    var ret462 Exp
    ret462 = Lam{
        _0: name__130,
        _1: body__131,
    }
    return ret462
}

func exp_app(a__132 Exp, b__133 Exp) Exp {
    var ret463 Exp
    ret463 = App{
        _0: a__132,
        _1: b__133,
    }
    return ret463
}

func exp_let(name__134 string, a__135 Exp, b__136 Exp) Exp {
    var ret464 Exp
    ret464 = Let{
        _0: name__134,
        _1: a__135,
        _2: b__136,
    }
    return ret464
}

func show_result(label__137 string, res__138 Result__Typ__string) struct{} {
    var ret465 struct{}
    switch res__138 := res__138.(type) {
    case Result__Typ__string_Ok:
        var x164 Typ = res__138._0
        var ty__139 Typ = x164
        var t284 string = label__137 + ": "
        var t285 string = typ_to_string(ty__139)
        var t283 string = t284 + t285
        ret465 = string_println(t283)
    case Result__Typ__string_Err:
        var x165 string = res__138._0
        var e__140 string = x165
        var t287 string = label__137 + ": "
        var t286 string = t287 + e__140
        ret465 = string_println(t286)
    }
    return ret465
}

func main0() struct{} {
    var ret466 struct{}
    var st__141 CheckerState = state_new()
    var t288 Exp = exp_var("x")
    var id__142 Exp = exp_lam("x", t288)
    var t291 Exp = exp_var("x")
    var t292 Exp = exp_var("y")
    var t290 Exp = exp_app(t291, t292)
    var t289 Exp = exp_lam("y", t290)
    var c1__143 Exp = exp_lam("x", t289)
    reset_type_variables(st__141)
    var t294 []EnvEntry = env_empty()
    var t293 Result__Typ__string = typeof(st__141, t294, id__142)
    show_result("id", t293)
    reset_type_variables(st__141)
    var t296 []EnvEntry = env_empty()
    var t295 Result__Typ__string = typeof(st__141, t296, c1__143)
    show_result("c1", t295)
    reset_type_variables(st__141)
    var t298 []EnvEntry = env_empty()
    var t300 Exp = exp_var("x")
    var t299 Exp = exp_let("x", c1__143, t300)
    var t297 Result__Typ__string = typeof(st__141, t298, t299)
    show_result("let_x_c1_x", t297)
    reset_type_variables(st__141)
    var t302 []EnvEntry = env_empty()
    var t305 Exp = exp_var("z")
    var t304 Exp = exp_lam("z", t305)
    var t306 Exp = exp_var("y")
    var t303 Exp = exp_let("y", t304, t306)
    var t301 Result__Typ__string = typeof(st__141, t302, t303)
    show_result("let_y_id_y", t301)
    reset_type_variables(st__141)
    var t308 []EnvEntry = env_empty()
    var t312 Exp = exp_var("z")
    var t311 Exp = exp_lam("z", t312)
    var t313 Exp = exp_var("y")
    var t310 Exp = exp_let("y", t311, t313)
    var t309 Exp = exp_lam("x", t310)
    var t307 Result__Typ__string = typeof(st__141, t308, t309)
    show_result("lam_x_let_y_id_y", t307)
    reset_type_variables(st__141)
    var t315 []EnvEntry = env_empty()
    var t319 Exp = exp_var("z")
    var t318 Exp = exp_lam("z", t319)
    var t321 Exp = exp_var("y")
    var t322 Exp = exp_var("x")
    var t320 Exp = exp_app(t321, t322)
    var t317 Exp = exp_let("y", t318, t320)
    var t316 Exp = exp_lam("x", t317)
    var t314 Result__Typ__string = typeof(st__141, t315, t316)
    show_result("lam_x_let_y_id_yx", t314)
    reset_type_variables(st__141)
    var t324 []EnvEntry = env_empty()
    var t327 Exp = exp_var("x")
    var t328 Exp = exp_var("x")
    var t326 Exp = exp_app(t327, t328)
    var t325 Exp = exp_lam("x", t326)
    var t323 Result__Typ__string = typeof(st__141, t324, t325)
    show_result("self_apply", t323)
    reset_type_variables(st__141)
    var t330 []EnvEntry = env_empty()
    var t332 Exp = exp_var("x")
    var t333 Exp = exp_var("x")
    var t331 Exp = exp_let("x", t332, t333)
    var t329 Result__Typ__string = typeof(st__141, t330, t331)
    show_result("unbound_var", t329)
    reset_type_variables(st__141)
    var t335 []EnvEntry = env_empty()
    var t338 Exp = exp_var("y")
    var t341 Exp = exp_var("y")
    var t342 Exp = exp_var("z")
    var t340 Exp = exp_app(t341, t342)
    var t339 Exp = exp_lam("z", t340)
    var t337 Exp = exp_app(t338, t339)
    var t336 Exp = exp_lam("y", t337)
    var t334 Result__Typ__string = typeof(st__141, t335, t336)
    show_result("max_heiber", t334)
    reset_type_variables(st__141)
    var t344 []EnvEntry = env_empty()
    var t350 Exp = exp_var("k")
    var t353 Exp = exp_var("k")
    var t354 Exp = exp_var("x")
    var t352 Exp = exp_app(t353, t354)
    var t355 Exp = exp_var("y")
    var t351 Exp = exp_app(t352, t355)
    var t349 Exp = exp_app(t350, t351)
    var t358 Exp = exp_var("k")
    var t359 Exp = exp_var("y")
    var t357 Exp = exp_app(t358, t359)
    var t360 Exp = exp_var("x")
    var t356 Exp = exp_app(t357, t360)
    var t348 Exp = exp_app(t349, t356)
    var t347 Exp = exp_lam("k", t348)
    var t346 Exp = exp_lam("y", t347)
    var t345 Exp = exp_lam("x", t346)
    var t343 Result__Typ__string = typeof(st__141, t344, t345)
    show_result("kirang", t343)
    reset_type_variables(st__141)
    var t362 []EnvEntry = env_empty()
    var t365 Exp = exp_var("id")
    var t366 Exp = exp_var("id")
    var t364 Exp = exp_app(t365, t366)
    var t363 Exp = exp_let("id", id__142, t364)
    var t361 Result__Typ__string = typeof(st__141, t362, t363)
    show_result("let_id_idid", t361)
    reset_type_variables(st__141)
    var t368 []EnvEntry = env_empty()
    var t373 Exp = exp_var("x")
    var t372 Exp = exp_app(t373, id__142)
    var t374 Exp = exp_var("z")
    var t371 Exp = exp_let("z", t372, t374)
    var t375 Exp = exp_var("y")
    var t370 Exp = exp_let("y", t371, t375)
    var t369 Exp = exp_let("x", c1__143, t370)
    var t367 Result__Typ__string = typeof(st__141, t368, t369)
    show_result("nested_lets", t367)
    reset_type_variables(st__141)
    var t377 []EnvEntry = env_empty()
    var t382 Exp = exp_var("x")
    var t383 Exp = exp_var("y")
    var t381 Exp = exp_app(t382, t383)
    var t386 Exp = exp_var("y")
    var t387 Exp = exp_var("x")
    var t385 Exp = exp_app(t386, t387)
    var t384 Exp = exp_lam("x", t385)
    var t380 Exp = exp_let("x", t381, t384)
    var t379 Exp = exp_lam("y", t380)
    var t378 Exp = exp_lam("x", t379)
    var t376 Result__Typ__string = typeof(st__141, t377, t378)
    show_result("fun_x_fun_y_let_x_xy_fun_x_yx", t376)
    reset_type_variables(st__141)
    var t389 []EnvEntry = env_empty()
    var t392 Exp = exp_var("x")
    var t393 Exp = exp_var("y")
    var t391 Exp = exp_let("y", t392, t393)
    var t390 Exp = exp_lam("x", t391)
    var t388 Result__Typ__string = typeof(st__141, t389, t390)
    show_result("sound_gen_1", t388)
    reset_type_variables(st__141)
    var t395 []EnvEntry = env_empty()
    var t399 Exp = exp_var("x")
    var t398 Exp = exp_lam("z", t399)
    var t400 Exp = exp_var("y")
    var t397 Exp = exp_let("y", t398, t400)
    var t396 Exp = exp_lam("x", t397)
    var t394 Result__Typ__string = typeof(st__141, t395, t396)
    show_result("sound_gen_2", t394)
    reset_type_variables(st__141)
    var t402 []EnvEntry = env_empty()
    var t407 Exp = exp_var("x")
    var t408 Exp = exp_var("z")
    var t406 Exp = exp_app(t407, t408)
    var t405 Exp = exp_lam("z", t406)
    var t409 Exp = exp_var("y")
    var t404 Exp = exp_let("y", t405, t409)
    var t403 Exp = exp_lam("x", t404)
    var t401 Result__Typ__string = typeof(st__141, t402, t403)
    show_result("sound_gen_3", t401)
    reset_type_variables(st__141)
    var t411 []EnvEntry = env_empty()
    var t416 Exp = exp_var("x")
    var t417 Exp = exp_var("y")
    var t415 Exp = exp_app(t416, t417)
    var t419 Exp = exp_var("x")
    var t420 Exp = exp_var("y")
    var t418 Exp = exp_app(t419, t420)
    var t414 Exp = exp_let("x", t415, t418)
    var t413 Exp = exp_lam("y", t414)
    var t412 Exp = exp_lam("x", t413)
    var t410 Result__Typ__string = typeof(st__141, t411, t412)
    show_result("double_apply", t410)
    reset_type_variables(st__141)
    var t422 []EnvEntry = env_empty()
    var t425 Exp = exp_var("x")
    var t427 Exp = exp_var("y")
    var t428 Exp = exp_var("y")
    var t426 Exp = exp_app(t427, t428)
    var t424 Exp = exp_let("y", t425, t426)
    var t423 Exp = exp_lam("x", t424)
    var t421 Result__Typ__string = typeof(st__141, t422, t423)
    show_result("sound_gen_occurs", t421)
    reset_gensym(st__141)
    var t430 []EnvEntry = env_empty()
    var t435 Exp = exp_var("x")
    var t434 Exp = exp_app(t435, id__142)
    var t436 Exp = exp_var("z")
    var t433 Exp = exp_let("z", t434, t436)
    var t437 Exp = exp_var("y")
    var t432 Exp = exp_let("y", t433, t437)
    var t431 Exp = exp_lam("x", t432)
    var t429 Result__Typ__string = typeof(st__141, t430, t431)
    show_result("fun_x_let_y_let_z_x_id_z_y", t429)
    string_println("")
    string_println("All Done")
    string_println("")
    ret466 = struct{}{}
    return ret466
}

func main() {
    main0()
}
