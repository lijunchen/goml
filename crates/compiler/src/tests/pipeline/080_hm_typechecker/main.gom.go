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
    var ret435 CheckerState
    var t204 *ref_int32_x = ref__Ref_int32(0)
    var t205 *ref_int32_x = ref__Ref_int32(1)
    ret435 = CheckerState{
        gensym_counter: t204,
        current_level: t205,
    }
    return ret435
}

func reset_gensym(st__0 CheckerState) struct{} {
    var ret436 struct{}
    var t206 *ref_int32_x = st__0.gensym_counter
    ref_set__Ref_int32(t206, 0)
    ret436 = struct{}{}
    return ret436
}

func reset_level(st__1 CheckerState) struct{} {
    var ret437 struct{}
    var t207 *ref_int32_x = st__1.current_level
    ref_set__Ref_int32(t207, 1)
    ret437 = struct{}{}
    return ret437
}

func reset_type_variables(st__2 CheckerState) struct{} {
    var ret438 struct{}
    reset_gensym(st__2)
    ret438 = reset_level(st__2)
    return ret438
}

func enter_level(st__3 CheckerState) struct{} {
    var ret439 struct{}
    var t208 *ref_int32_x = st__3.current_level
    var l__4 int32 = ref_get__Ref_int32(t208)
    var t209 *ref_int32_x = st__3.current_level
    var t210 int32 = l__4 + 1
    ref_set__Ref_int32(t209, t210)
    ret439 = struct{}{}
    return ret439
}

func leave_level(st__5 CheckerState) struct{} {
    var ret440 struct{}
    var t211 *ref_int32_x = st__5.current_level
    var l__6 int32 = ref_get__Ref_int32(t211)
    var t212 *ref_int32_x = st__5.current_level
    var t213 int32 = l__6 - 1
    ref_set__Ref_int32(t212, t213)
    ret440 = struct{}{}
    return ret440
}

func min_i32(a__7 int32, b__8 int32) int32 {
    var ret441 int32
    var t214 bool = a__7 < b__8
    if t214 {
        ret441 = a__7
    } else {
        ret441 = b__8
    }
    return ret441
}

func nth_letter(n__9 int32) rune {
    var ret442 rune
    switch n__9 {
    case 0:
        ret442 = 97
    case 1:
        ret442 = 98
    case 2:
        ret442 = 99
    case 3:
        ret442 = 100
    case 4:
        ret442 = 101
    case 5:
        ret442 = 102
    case 6:
        ret442 = 103
    case 7:
        ret442 = 104
    case 8:
        ret442 = 105
    case 9:
        ret442 = 106
    case 10:
        ret442 = 107
    case 11:
        ret442 = 108
    case 12:
        ret442 = 109
    case 13:
        ret442 = 110
    case 14:
        ret442 = 111
    case 15:
        ret442 = 112
    case 16:
        ret442 = 113
    case 17:
        ret442 = 114
    case 18:
        ret442 = 115
    case 19:
        ret442 = 116
    case 20:
        ret442 = 117
    case 21:
        ret442 = 118
    case 22:
        ret442 = 119
    case 23:
        ret442 = 120
    case 24:
        ret442 = 121
    case 25:
        ret442 = 122
    default:
        ret442 = 97
    }
    return ret442
}

func gensym(st__10 CheckerState) string {
    var ret443 string
    var t215 *ref_int32_x = st__10.gensym_counter
    var n__11 int32 = ref_get__Ref_int32(t215)
    var t216 *ref_int32_x = st__10.gensym_counter
    var t217 int32 = n__11 + 1
    ref_set__Ref_int32(t216, t217)
    var t218 bool = n__11 < 26
    if t218 {
        var t219 rune = nth_letter(n__11)
        ret443 = char_to_string(t219)
    } else {
        var t220 string = int32_to_string(n__11)
        ret443 = "t" + t220
    }
    return ret443
}

func newvar(st__12 CheckerState) Typ {
    var ret444 Typ
    var name__13 string = gensym(st__12)
    var t221 *ref_int32_x = st__12.current_level
    var level__14 int32 = ref_get__Ref_int32(t221)
    var t223 Tv = Unbound{
        _0: name__13,
        _1: level__14,
    }
    var t222 *ref_tv_x = ref__Ref_Tv(t223)
    ret444 = TVar{
        _0: t222,
    }
    return ret444
}

func typ_is_arrow(ty__15 Typ) bool {
    var ret445 bool
    switch ty__15 := ty__15.(type) {
    case TVar:
        var x6 *ref_tv_x = ty__15._0
        var tvref__16 *ref_tv_x = x6
        var mtmp10 Tv = ref_get__Ref_Tv(tvref__16)
        switch mtmp10 := mtmp10.(type) {
        case Unbound:
            ret445 = false
        case Link:
            var x13 Typ = mtmp10._0
            var inner__17 Typ = x13
            ret445 = typ_is_arrow(inner__17)
        }
    case QVar:
        ret445 = false
    case TArrow:
        ret445 = true
    }
    return ret445
}

func typ_to_string(ty__18 Typ) string {
    var ret446 string
    switch ty__18 := ty__18.(type) {
    case TVar:
        var x14 *ref_tv_x = ty__18._0
        var tvref__20 *ref_tv_x = x14
        var mtmp18 Tv = ref_get__Ref_Tv(tvref__20)
        switch mtmp18 := mtmp18.(type) {
        case Unbound:
            var x19 string = mtmp18._0
            var name__21 string = x19
            ret446 = "'" + name__21
        case Link:
            var x21 Typ = mtmp18._0
            var inner__22 Typ = x21
            ret446 = typ_to_string(inner__22)
        }
    case QVar:
        var x15 string = ty__18._0
        var name__19 string = x15
        ret446 = "'" + name__19
    case TArrow:
        var x16 Typ = ty__18._0
        var x17 Typ = ty__18._1
        var t2__24 Typ = x17
        var t1__23 Typ = x16
        var t224 bool = typ_is_arrow(t1__23)
        var s1__25 string
        if t224 {
            var t226 string = typ_to_string(t1__23)
            var t225 string = "(" + t226
            s1__25 = t225 + ")"
        } else {
            s1__25 = typ_to_string(t1__23)
        }
        var s2__26 string = typ_to_string(t2__24)
        var t227 string = s1__25 + " -> "
        ret446 = t227 + s2__26
    }
    return ret446
}

func env_empty() []EnvEntry {
    var ret447 []EnvEntry
    var env__27 []EnvEntry = nil
    ret447 = env__27
    return ret447
}

func env_lookup(env__28 []EnvEntry, name__29 string) Option__Typ {
    var ret448 Option__Typ
    var t229 int32 = int32(len(env__28))
    var t228 int32 = t229 - 1
    var i__30 *ref_int32_x = ref__Ref_int32(t228)
    var t230 Option__Typ = None{}
    var found__31 *ref_option__typ_x = ref__Ref_Option__Typ(t230)
    var done__32 *ref_bool_x = ref__Ref_bool(false)
    var cond449 bool
    for {
        var t232 bool = ref_get__Ref_bool(done__32)
        var t231 bool = !t232
        var t234 int32 = ref_get__Ref_int32(i__30)
        var t233 bool = t234 >= 0
        cond449 = t231 && t233
        if !cond449 {
            break
        }
        var t235 int32 = ref_get__Ref_int32(i__30)
        var entry__33 EnvEntry = env__28[t235]
        var t237 string = entry__33.name
        var t236 bool = t237 == name__29
        if t236 {
            var t239 Typ = entry__33.ty
            var t238 Option__Typ = Some{
                _0: t239,
            }
            ref_set__Ref_Option__Typ(found__31, t238)
            ref_set__Ref_bool(done__32, true)
        } else {
            var t241 int32 = ref_get__Ref_int32(i__30)
            var t240 int32 = t241 - 1
            ref_set__Ref_int32(i__30, t240)
        }
    }
    ret448 = ref_get__Ref_Option__Typ(found__31)
    return ret448
}

func subst_lookup(subst__34 []SubstEntry, name__35 string) Option__Typ {
    var ret450 Option__Typ
    var t243 int32 = int32(len(subst__34))
    var t242 int32 = t243 - 1
    var i__36 *ref_int32_x = ref__Ref_int32(t242)
    var t244 Option__Typ = None{}
    var found__37 *ref_option__typ_x = ref__Ref_Option__Typ(t244)
    var done__38 *ref_bool_x = ref__Ref_bool(false)
    var cond451 bool
    for {
        var t246 bool = ref_get__Ref_bool(done__38)
        var t245 bool = !t246
        var t248 int32 = ref_get__Ref_int32(i__36)
        var t247 bool = t248 >= 0
        cond451 = t245 && t247
        if !cond451 {
            break
        }
        var t249 int32 = ref_get__Ref_int32(i__36)
        var entry__39 SubstEntry = subst__34[t249]
        var t251 string = entry__39.name
        var t250 bool = t251 == name__35
        if t250 {
            var t253 Typ = entry__39.ty
            var t252 Option__Typ = Some{
                _0: t253,
            }
            ref_set__Ref_Option__Typ(found__37, t252)
            ref_set__Ref_bool(done__38, true)
        } else {
            var t255 int32 = ref_get__Ref_int32(i__36)
            var t254 int32 = t255 - 1
            ref_set__Ref_int32(i__36, t254)
        }
    }
    ret450 = ref_get__Ref_Option__Typ(found__37)
    return ret450
}

func occurs(st__40 CheckerState, tvr__41 *ref_tv_x, ty__42 Typ) Result__unit__string {
    var ret452 Result__unit__string
    switch ty__42 := ty__42.(type) {
    case TVar:
        var x26 *ref_tv_x = ty__42._0
        var tvr2__43 *ref_tv_x = x26
        var t256 bool = ptr_eq__Ref_Tv(tvr__41, tvr2__43)
        if t256 {
            ret452 = Result__unit__string_Err{
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
                var mtmp34 Tv = ref_get__Ref_Tv(tvr__41)
                var min_level__47 int32
                switch mtmp34 := mtmp34.(type) {
                case Unbound:
                    var x36 int32 = mtmp34._1
                    var l__46 int32 = x36
                    min_level__47 = min_i32(l__46, l2__45)
                case Link:
                    min_level__47 = l2__45
                }
                var t257 Tv = Unbound{
                    _0: name__44,
                    _1: min_level__47,
                }
                ref_set__Ref_Tv(tvr2__43, t257)
                ret452 = Result__unit__string_Ok{
                    _0: struct{}{},
                }
            case Link:
                var x33 Typ = mtmp30._0
                var inner__48 Typ = x33
                ret452 = occurs(st__40, tvr__41, inner__48)
            }
        }
    case QVar:
        ret452 = Result__unit__string_Ok{
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
            ret452 = occurs(st__40, tvr__41, t2__50)
        case Result__unit__string_Err:
            var x41 string = mtmp39._0
            var e__51 string = x41
            ret452 = Result__unit__string_Err{
                _0: e__51,
            }
        }
    }
    return ret452
}

func unify(st__52 CheckerState, t1__53 Typ, t2__54 Typ) Result__unit__string {
    var ret453 Result__unit__string
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
            var t258 bool = ptr_eq__Ref_Tv(r1__55, r2__56)
            if t258 {
                ret453 = Result__unit__string_Ok{
                    _0: struct{}{},
                }
            } else {
                var mtmp53 Tv = ref_get__Ref_Tv(r1__55)
                switch mtmp53 := mtmp53.(type) {
                case Unbound:
                    var mtmp57 Tv = ref_get__Ref_Tv(r2__56)
                    switch mtmp57 := mtmp57.(type) {
                    case Unbound:
                        var t259 Typ = TVar{
                            _0: r2__56,
                        }
                        var mtmp61 Result__unit__string = occurs(st__52, r1__55, t259)
                        switch mtmp61 := mtmp61.(type) {
                        case Result__unit__string_Ok:
                            var t261 Typ = TVar{
                                _0: r2__56,
                            }
                            var t260 Tv = Link{
                                _0: t261,
                            }
                            ref_set__Ref_Tv(r1__55, t260)
                            ret453 = Result__unit__string_Ok{
                                _0: struct{}{},
                            }
                        case Result__unit__string_Err:
                            var x63 string = mtmp61._0
                            var e__59 string = x63
                            ret453 = Result__unit__string_Err{
                                _0: e__59,
                            }
                        }
                    case Link:
                        var x60 Typ = mtmp57._0
                        var inner__58 Typ = x60
                        var t262 Typ = TVar{
                            _0: r1__55,
                        }
                        ret453 = unify(st__52, t262, inner__58)
                    }
                case Link:
                    var x56 Typ = mtmp53._0
                    var inner__57 Typ = x56
                    var t263 Typ = TVar{
                        _0: r2__56,
                    }
                    ret453 = unify(st__52, inner__57, t263)
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
                    var t264 Tv = Link{
                        _0: other__64,
                    }
                    ref_set__Ref_Tv(r2__65, t264)
                    ret453 = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                case Result__unit__string_Err:
                    var x71 string = mtmp69._0
                    var e__67 string = x71
                    ret453 = Result__unit__string_Err{
                        _0: e__67,
                    }
                }
            case Link:
                var x68 Typ = mtmp65._0
                var inner__66 Typ = x68
                ret453 = unify(st__52, other__64, inner__66)
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
                    var t265 Tv = Link{
                        _0: other__64,
                    }
                    ref_set__Ref_Tv(r2__65, t265)
                    ret453 = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                case Result__unit__string_Err:
                    var x79 string = mtmp77._0
                    var e__67 string = x79
                    ret453 = Result__unit__string_Err{
                        _0: e__67,
                    }
                }
            case Link:
                var x76 Typ = mtmp73._0
                var inner__66 Typ = x76
                ret453 = unify(st__52, other__64, inner__66)
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
                    var t266 Tv = Link{
                        _0: other__61,
                    }
                    ref_set__Ref_Tv(r1__60, t266)
                    ret453 = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                case Result__unit__string_Err:
                    var x91 string = mtmp89._0
                    var e__63 string = x91
                    ret453 = Result__unit__string_Err{
                        _0: e__63,
                    }
                }
            case Link:
                var x88 Typ = mtmp85._0
                var inner__62 Typ = x88
                ret453 = unify(st__52, inner__62, other__61)
            }
        case QVar:
            ret453 = Result__unit__string_Err{
                _0: "unify error",
            }
        case TArrow:
            ret453 = Result__unit__string_Err{
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
                    var t267 Tv = Link{
                        _0: other__61,
                    }
                    ref_set__Ref_Tv(r1__60, t267)
                    ret453 = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                case Result__unit__string_Err:
                    var x103 string = mtmp101._0
                    var e__63 string = x103
                    ret453 = Result__unit__string_Err{
                        _0: e__63,
                    }
                }
            case Link:
                var x100 Typ = mtmp97._0
                var inner__62 Typ = x100
                ret453 = unify(st__52, inner__62, other__61)
            }
        case QVar:
            ret453 = Result__unit__string_Err{
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
                ret453 = unify(st__52, a2__69, b2__71)
            case Result__unit__string_Err:
                var x107 string = mtmp105._0
                var e__72 string = x107
                ret453 = Result__unit__string_Err{
                    _0: e__72,
                }
            }
        }
    }
    return ret453
}

func gen(st__73 CheckerState, ty__74 Typ) Typ {
    var ret454 Typ
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
            var t268 *ref_int32_x = st__73.current_level
            var cur__78 int32 = ref_get__Ref_int32(t268)
            var t269 bool = l__77 > cur__78
            if t269 {
                ret454 = QVar{
                    _0: name__76,
                }
            } else {
                ret454 = TVar{
                    _0: tvref__75,
                }
            }
        case Link:
            var x115 Typ = mtmp112._0
            var inner__79 Typ = x115
            ret454 = gen(st__73, inner__79)
        }
    case QVar:
        var other__82 Typ = ty__74
        ret454 = other__82
    case TArrow:
        var x110 Typ = ty__74._0
        var x111 Typ = ty__74._1
        var t2__81 Typ = x111
        var t1__80 Typ = x110
        var t270 Typ = gen(st__73, t1__80)
        var t271 Typ = gen(st__73, t2__81)
        ret454 = TArrow{
            _0: t270,
            _1: t271,
        }
    }
    return ret454
}

func inst_loop(st__83 CheckerState, subst__84 []SubstEntry, ty__85 Typ) Tuple2_Typ_Vec_SubstEntry {
    var ret455 Tuple2_Typ_Vec_SubstEntry
    switch ty__85 := ty__85.(type) {
    case TVar:
        var x116 *ref_tv_x = ty__85._0
        var tvref__90 *ref_tv_x = x116
        var mtmp120 Tv = ref_get__Ref_Tv(tvref__90)
        switch mtmp120 := mtmp120.(type) {
        case Unbound:
            var t272 Typ = TVar{
                _0: tvref__90,
            }
            ret455 = Tuple2_Typ_Vec_SubstEntry{
                _0: t272,
                _1: subst__84,
            }
        case Link:
            var x123 Typ = mtmp120._0
            var inner__91 Typ = x123
            ret455 = inst_loop(st__83, subst__84, inner__91)
        }
    case QVar:
        var x117 string = ty__85._0
        var name__86 string = x117
        var mtmp124 Option__Typ = subst_lookup(subst__84, name__86)
        switch mtmp124 := mtmp124.(type) {
        case None:
            var tv__88 Typ = newvar(st__83)
            var t273 SubstEntry = SubstEntry{
                name: name__86,
                ty: tv__88,
            }
            var new_subst__89 []SubstEntry = append(subst__84, t273)
            ret455 = Tuple2_Typ_Vec_SubstEntry{
                _0: tv__88,
                _1: new_subst__89,
            }
        case Some:
            var x125 Typ = mtmp124._0
            var t__87 Typ = x125
            ret455 = Tuple2_Typ_Vec_SubstEntry{
                _0: t__87,
                _1: subst__84,
            }
        }
    case TArrow:
        var x118 Typ = ty__85._0
        var x119 Typ = ty__85._1
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
        var t274 Typ = TArrow{
            _0: ty1__94,
            _1: ty2__96,
        }
        ret455 = Tuple2_Typ_Vec_SubstEntry{
            _0: t274,
            _1: subst2__97,
        }
    }
    return ret455
}

func inst(st__98 CheckerState, ty__99 Typ) Typ {
    var ret456 Typ
    var subst0__100 []SubstEntry = nil
    var mtmp132 Tuple2_Typ_Vec_SubstEntry = inst_loop(st__98, subst0__100, ty__99)
    var x133 Typ = mtmp132._0
    var t__101 Typ = x133
    ret456 = t__101
    return ret456
}

func typeof(st__102 CheckerState, env__103 []EnvEntry, e__104 Exp) Result__Typ__string {
    var ret457 Result__Typ__string
    switch e__104 := e__104.(type) {
    case Var:
        var x135 string = e__104._0
        var x__105 string = x135
        var mtmp143 Option__Typ = env_lookup(env__103, x__105)
        switch mtmp143 := mtmp143.(type) {
        case None:
            ret457 = Result__Typ__string_Err{
                _0: "unbound var",
            }
        case Some:
            var x144 Typ = mtmp143._0
            var ty__106 Typ = x144
            var t275 Typ = inst(st__102, ty__106)
            ret457 = Result__Typ__string_Ok{
                _0: t275,
            }
        }
    case App:
        var x136 Exp = e__104._0
        var x137 Exp = e__104._1
        var e2__114 Exp = x137
        var e1__113 Exp = x136
        var mtmp145 Result__Typ__string = typeof(st__102, env__103, e1__113)
        switch mtmp145 := mtmp145.(type) {
        case Result__Typ__string_Ok:
            var x146 Typ = mtmp145._0
            var ty_fun__116 Typ = x146
            var mtmp148 Result__Typ__string = typeof(st__102, env__103, e2__114)
            switch mtmp148 := mtmp148.(type) {
            case Result__Typ__string_Ok:
                var x149 Typ = mtmp148._0
                var ty_arg__118 Typ = x149
                var ty_res__119 Typ = newvar(st__102)
                var arrow__120 Typ = TArrow{
                    _0: ty_arg__118,
                    _1: ty_res__119,
                }
                var mtmp151 Result__unit__string = unify(st__102, ty_fun__116, arrow__120)
                switch mtmp151 := mtmp151.(type) {
                case Result__unit__string_Ok:
                    ret457 = Result__Typ__string_Ok{
                        _0: ty_res__119,
                    }
                case Result__unit__string_Err:
                    var x153 string = mtmp151._0
                    var e__121 string = x153
                    ret457 = Result__Typ__string_Err{
                        _0: e__121,
                    }
                }
            case Result__Typ__string_Err:
                var x150 string = mtmp148._0
                var e__117 string = x150
                ret457 = Result__Typ__string_Err{
                    _0: e__117,
                }
            }
        case Result__Typ__string_Err:
            var x147 string = mtmp145._0
            var e__115 string = x147
            ret457 = Result__Typ__string_Err{
                _0: e__115,
            }
        }
    case Lam:
        var x138 string = e__104._0
        var x139 Exp = e__104._1
        var body__108 Exp = x139
        var x__107 string = x138
        var ty_x__109 Typ = newvar(st__102)
        var t276 EnvEntry = EnvEntry{
            name: x__107,
            ty: ty_x__109,
        }
        var env2__110 []EnvEntry = append(env__103, t276)
        var mtmp154 Result__Typ__string = typeof(st__102, env2__110, body__108)
        switch mtmp154 := mtmp154.(type) {
        case Result__Typ__string_Ok:
            var x155 Typ = mtmp154._0
            var ty_e__111 Typ = x155
            var t277 Typ = TArrow{
                _0: ty_x__109,
                _1: ty_e__111,
            }
            ret457 = Result__Typ__string_Ok{
                _0: t277,
            }
        case Result__Typ__string_Err:
            var x156 string = mtmp154._0
            var e__112 string = x156
            ret457 = Result__Typ__string_Err{
                _0: e__112,
            }
        }
    case Let:
        var x140 string = e__104._0
        var x141 Exp = e__104._1
        var x142 Exp = e__104._2
        var e2__124 Exp = x142
        var e1__123 Exp = x141
        var x__122 string = x140
        enter_level(st__102)
        var ty_e__125 Result__Typ__string = typeof(st__102, env__103, e1__123)
        leave_level(st__102)
        switch ty_e__125 := ty_e__125.(type) {
        case Result__Typ__string_Ok:
            var x159 Typ = ty_e__125._0
            var ty1__127 Typ = x159
            var t279 Typ = gen(st__102, ty1__127)
            var t278 EnvEntry = EnvEntry{
                name: x__122,
                ty: t279,
            }
            var env2__128 []EnvEntry = append(env__103, t278)
            ret457 = typeof(st__102, env2__128, e2__124)
        case Result__Typ__string_Err:
            var x160 string = ty_e__125._0
            var e__126 string = x160
            ret457 = Result__Typ__string_Err{
                _0: e__126,
            }
        }
    }
    return ret457
}

func exp_var(name__129 string) Exp {
    var ret458 Exp
    ret458 = Var{
        _0: name__129,
    }
    return ret458
}

func exp_lam(name__130 string, body__131 Exp) Exp {
    var ret459 Exp
    ret459 = Lam{
        _0: name__130,
        _1: body__131,
    }
    return ret459
}

func exp_app(a__132 Exp, b__133 Exp) Exp {
    var ret460 Exp
    ret460 = App{
        _0: a__132,
        _1: b__133,
    }
    return ret460
}

func exp_let(name__134 string, a__135 Exp, b__136 Exp) Exp {
    var ret461 Exp
    ret461 = Let{
        _0: name__134,
        _1: a__135,
        _2: b__136,
    }
    return ret461
}

func show_result(label__137 string, res__138 Result__Typ__string) struct{} {
    var ret462 struct{}
    switch res__138 := res__138.(type) {
    case Result__Typ__string_Ok:
        var x161 Typ = res__138._0
        var ty__139 Typ = x161
        var t281 string = label__137 + ": "
        var t282 string = typ_to_string(ty__139)
        var t280 string = t281 + t282
        ret462 = string_println(t280)
    case Result__Typ__string_Err:
        var x162 string = res__138._0
        var e__140 string = x162
        var t284 string = label__137 + ": "
        var t283 string = t284 + e__140
        ret462 = string_println(t283)
    }
    return ret462
}

func main0() struct{} {
    var ret463 struct{}
    var st__141 CheckerState = state_new()
    var t285 Exp = exp_var("x")
    var id__142 Exp = exp_lam("x", t285)
    var t288 Exp = exp_var("x")
    var t289 Exp = exp_var("y")
    var t287 Exp = exp_app(t288, t289)
    var t286 Exp = exp_lam("y", t287)
    var c1__143 Exp = exp_lam("x", t286)
    reset_type_variables(st__141)
    var t291 []EnvEntry = env_empty()
    var t290 Result__Typ__string = typeof(st__141, t291, id__142)
    show_result("id", t290)
    reset_type_variables(st__141)
    var t293 []EnvEntry = env_empty()
    var t292 Result__Typ__string = typeof(st__141, t293, c1__143)
    show_result("c1", t292)
    reset_type_variables(st__141)
    var t295 []EnvEntry = env_empty()
    var t297 Exp = exp_var("x")
    var t296 Exp = exp_let("x", c1__143, t297)
    var t294 Result__Typ__string = typeof(st__141, t295, t296)
    show_result("let_x_c1_x", t294)
    reset_type_variables(st__141)
    var t299 []EnvEntry = env_empty()
    var t302 Exp = exp_var("z")
    var t301 Exp = exp_lam("z", t302)
    var t303 Exp = exp_var("y")
    var t300 Exp = exp_let("y", t301, t303)
    var t298 Result__Typ__string = typeof(st__141, t299, t300)
    show_result("let_y_id_y", t298)
    reset_type_variables(st__141)
    var t305 []EnvEntry = env_empty()
    var t309 Exp = exp_var("z")
    var t308 Exp = exp_lam("z", t309)
    var t310 Exp = exp_var("y")
    var t307 Exp = exp_let("y", t308, t310)
    var t306 Exp = exp_lam("x", t307)
    var t304 Result__Typ__string = typeof(st__141, t305, t306)
    show_result("lam_x_let_y_id_y", t304)
    reset_type_variables(st__141)
    var t312 []EnvEntry = env_empty()
    var t316 Exp = exp_var("z")
    var t315 Exp = exp_lam("z", t316)
    var t318 Exp = exp_var("y")
    var t319 Exp = exp_var("x")
    var t317 Exp = exp_app(t318, t319)
    var t314 Exp = exp_let("y", t315, t317)
    var t313 Exp = exp_lam("x", t314)
    var t311 Result__Typ__string = typeof(st__141, t312, t313)
    show_result("lam_x_let_y_id_yx", t311)
    reset_type_variables(st__141)
    var t321 []EnvEntry = env_empty()
    var t324 Exp = exp_var("x")
    var t325 Exp = exp_var("x")
    var t323 Exp = exp_app(t324, t325)
    var t322 Exp = exp_lam("x", t323)
    var t320 Result__Typ__string = typeof(st__141, t321, t322)
    show_result("self_apply", t320)
    reset_type_variables(st__141)
    var t327 []EnvEntry = env_empty()
    var t329 Exp = exp_var("x")
    var t330 Exp = exp_var("x")
    var t328 Exp = exp_let("x", t329, t330)
    var t326 Result__Typ__string = typeof(st__141, t327, t328)
    show_result("unbound_var", t326)
    reset_type_variables(st__141)
    var t332 []EnvEntry = env_empty()
    var t335 Exp = exp_var("y")
    var t338 Exp = exp_var("y")
    var t339 Exp = exp_var("z")
    var t337 Exp = exp_app(t338, t339)
    var t336 Exp = exp_lam("z", t337)
    var t334 Exp = exp_app(t335, t336)
    var t333 Exp = exp_lam("y", t334)
    var t331 Result__Typ__string = typeof(st__141, t332, t333)
    show_result("max_heiber", t331)
    reset_type_variables(st__141)
    var t341 []EnvEntry = env_empty()
    var t347 Exp = exp_var("k")
    var t350 Exp = exp_var("k")
    var t351 Exp = exp_var("x")
    var t349 Exp = exp_app(t350, t351)
    var t352 Exp = exp_var("y")
    var t348 Exp = exp_app(t349, t352)
    var t346 Exp = exp_app(t347, t348)
    var t355 Exp = exp_var("k")
    var t356 Exp = exp_var("y")
    var t354 Exp = exp_app(t355, t356)
    var t357 Exp = exp_var("x")
    var t353 Exp = exp_app(t354, t357)
    var t345 Exp = exp_app(t346, t353)
    var t344 Exp = exp_lam("k", t345)
    var t343 Exp = exp_lam("y", t344)
    var t342 Exp = exp_lam("x", t343)
    var t340 Result__Typ__string = typeof(st__141, t341, t342)
    show_result("kirang", t340)
    reset_type_variables(st__141)
    var t359 []EnvEntry = env_empty()
    var t362 Exp = exp_var("id")
    var t363 Exp = exp_var("id")
    var t361 Exp = exp_app(t362, t363)
    var t360 Exp = exp_let("id", id__142, t361)
    var t358 Result__Typ__string = typeof(st__141, t359, t360)
    show_result("let_id_idid", t358)
    reset_type_variables(st__141)
    var t365 []EnvEntry = env_empty()
    var t370 Exp = exp_var("x")
    var t369 Exp = exp_app(t370, id__142)
    var t371 Exp = exp_var("z")
    var t368 Exp = exp_let("z", t369, t371)
    var t372 Exp = exp_var("y")
    var t367 Exp = exp_let("y", t368, t372)
    var t366 Exp = exp_let("x", c1__143, t367)
    var t364 Result__Typ__string = typeof(st__141, t365, t366)
    show_result("nested_lets", t364)
    reset_type_variables(st__141)
    var t374 []EnvEntry = env_empty()
    var t379 Exp = exp_var("x")
    var t380 Exp = exp_var("y")
    var t378 Exp = exp_app(t379, t380)
    var t383 Exp = exp_var("y")
    var t384 Exp = exp_var("x")
    var t382 Exp = exp_app(t383, t384)
    var t381 Exp = exp_lam("x", t382)
    var t377 Exp = exp_let("x", t378, t381)
    var t376 Exp = exp_lam("y", t377)
    var t375 Exp = exp_lam("x", t376)
    var t373 Result__Typ__string = typeof(st__141, t374, t375)
    show_result("fun_x_fun_y_let_x_xy_fun_x_yx", t373)
    reset_type_variables(st__141)
    var t386 []EnvEntry = env_empty()
    var t389 Exp = exp_var("x")
    var t390 Exp = exp_var("y")
    var t388 Exp = exp_let("y", t389, t390)
    var t387 Exp = exp_lam("x", t388)
    var t385 Result__Typ__string = typeof(st__141, t386, t387)
    show_result("sound_gen_1", t385)
    reset_type_variables(st__141)
    var t392 []EnvEntry = env_empty()
    var t396 Exp = exp_var("x")
    var t395 Exp = exp_lam("z", t396)
    var t397 Exp = exp_var("y")
    var t394 Exp = exp_let("y", t395, t397)
    var t393 Exp = exp_lam("x", t394)
    var t391 Result__Typ__string = typeof(st__141, t392, t393)
    show_result("sound_gen_2", t391)
    reset_type_variables(st__141)
    var t399 []EnvEntry = env_empty()
    var t404 Exp = exp_var("x")
    var t405 Exp = exp_var("z")
    var t403 Exp = exp_app(t404, t405)
    var t402 Exp = exp_lam("z", t403)
    var t406 Exp = exp_var("y")
    var t401 Exp = exp_let("y", t402, t406)
    var t400 Exp = exp_lam("x", t401)
    var t398 Result__Typ__string = typeof(st__141, t399, t400)
    show_result("sound_gen_3", t398)
    reset_type_variables(st__141)
    var t408 []EnvEntry = env_empty()
    var t413 Exp = exp_var("x")
    var t414 Exp = exp_var("y")
    var t412 Exp = exp_app(t413, t414)
    var t416 Exp = exp_var("x")
    var t417 Exp = exp_var("y")
    var t415 Exp = exp_app(t416, t417)
    var t411 Exp = exp_let("x", t412, t415)
    var t410 Exp = exp_lam("y", t411)
    var t409 Exp = exp_lam("x", t410)
    var t407 Result__Typ__string = typeof(st__141, t408, t409)
    show_result("double_apply", t407)
    reset_type_variables(st__141)
    var t419 []EnvEntry = env_empty()
    var t422 Exp = exp_var("x")
    var t424 Exp = exp_var("y")
    var t425 Exp = exp_var("y")
    var t423 Exp = exp_app(t424, t425)
    var t421 Exp = exp_let("y", t422, t423)
    var t420 Exp = exp_lam("x", t421)
    var t418 Result__Typ__string = typeof(st__141, t419, t420)
    show_result("sound_gen_occurs", t418)
    reset_gensym(st__141)
    var t427 []EnvEntry = env_empty()
    var t432 Exp = exp_var("x")
    var t431 Exp = exp_app(t432, id__142)
    var t433 Exp = exp_var("z")
    var t430 Exp = exp_let("z", t431, t433)
    var t434 Exp = exp_var("y")
    var t429 Exp = exp_let("y", t430, t434)
    var t428 Exp = exp_lam("x", t429)
    var t426 Result__Typ__string = typeof(st__141, t427, t428)
    show_result("fun_x_let_y_let_z_x_id_z_y", t426)
    string_println("")
    string_println("All Done")
    string_println("")
    ret463 = struct{}{}
    return ret463
}

func main() {
    main0()
}
