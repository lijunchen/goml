package main

import (
    _goml_fmt "fmt"
    _goml_slices "slices"
)

func _goml_runtime_core_string_get(s string, i int) rune {
    return rune(s[i])
}

func _goml_runtime_core_char_to_string(x rune) string {
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

func vec_with_capacity__Vec_8EnvEntry(capacity int) *_goml_vec_EnvEntry {
    return &_goml_vec_EnvEntry{
        items: _goml_slices.Grow([]EnvEntry{}, int(capacity)),
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

func vec_with_capacity__Vec_10SubstEntry(capacity int) *_goml_vec_SubstEntry {
    return &_goml_vec_SubstEntry{
        items: _goml_slices.Grow([]SubstEntry{}, int(capacity)),
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
    var t380 *ref_int32_x
    var inline949 int32 = 0
    var inline950 *ref_int32_x = ref__Ref_5int32(inline949)
    t380 = inline950
    var t381 *ref_int32_x
    var inline946 int32 = 1
    var inline947 *ref_int32_x = ref__Ref_5int32(inline946)
    t381 = inline947
    var t382 CheckerState = CheckerState{
        gensym_counter: t380,
        current_level: t381,
    }
    return t382
}

func reset_type_variables(st__2 CheckerState) struct{} {
    var inline961 *ref_int32_x = st__2.gensym_counter
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline961, 0)
    var inline958 *ref_int32_x = st__2.current_level
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline958, 1)
    return struct{}{}
}

func nth_letter(n__9 int32) rune {
    switch n__9 {
    case 0:
        return 97
    case 1:
        return 98
    case 2:
        return 99
    case 3:
        return 100
    case 4:
        return 101
    case 5:
        return 102
    case 6:
        return 103
    case 7:
        return 104
    case 8:
        return 105
    case 9:
        return 106
    case 10:
        return 107
    case 11:
        return 108
    case 12:
        return 109
    case 13:
        return 110
    case 14:
        return 111
    case 15:
        return 112
    case 16:
        return 113
    case 17:
        return 114
    case 18:
        return 115
    case 19:
        return 116
    case 20:
        return 117
    case 21:
        return 118
    case 22:
        return 119
    case 23:
        return 120
    case 24:
        return 121
    case 25:
        return 122
    default:
        return 97
    }
}

func gensym(st__10 CheckerState) string {
    var t408 *ref_int32_x = st__10.gensym_counter
    var n__11 int32
    var inline978 int32 = ref_get__Ref_5int32(t408)
    n__11 = inline978
    var t409 *ref_int32_x = st__10.gensym_counter
    var t410 int32 = n__11 + 1
    ref_set__Ref_5int32(t409, t410)
    var t413 bool = n__11 < 26
    if t413 {
        var t414 rune = nth_letter(n__11)
        var inline972 string = char_to_string(t414)
        return inline972
    } else {
        var t416 string
        var inline974 string = _goml_runtime_core_int32_to_string(n__11)
        t416 = inline974
        var t417 string = "t" + t416
        return t417
    }
}

func typ_is_arrow(ty__15 Typ) bool {
    switch ty__15.(type) {
    case TVar:
        var x188 *ref_Tv_x = ty__15.(TVar)._0
        var mtmp192 Tv
        var inline995 Tv = ref_get__Ref_2Tv(x188)
        mtmp192 = inline995
        switch mtmp192.(type) {
        case Link:
            var x195 Typ = mtmp192.(Link)._0
            var t430 bool = typ_is_arrow(x195)
            return t430
        default:
            return false
        }
    case TArrow:
        return true
    default:
        return false
    }
}

func typ_to_string(ty__18 Typ) string {
    switch ty__18.(type) {
    case TVar:
        var x196 *ref_Tv_x = ty__18.(TVar)._0
        var mtmp200 Tv
        var inline997 Tv = ref_get__Ref_2Tv(x196)
        mtmp200 = inline997
        switch mtmp200.(type) {
        case Unbound:
            var x201 string = mtmp200.(Unbound)._0
            var t437 string = "'" + x201
            return t437
        case Link:
            var x203 Typ = mtmp200.(Link)._0
            var t438 string = typ_to_string(x203)
            return t438
        default:
            panic("non-exhaustive match")
        }
    case QVar:
        var x197 string = ty__18.(QVar)._0
        var t439 string = "'" + x197
        return t439
    case TArrow:
        var x198 Typ = ty__18.(TArrow)._0
        var x199 Typ = ty__18.(TArrow)._1
        var t444 bool = typ_is_arrow(x198)
        var jp441 string
        if t444 {
            var t445 string = typ_to_string(x198)
            var t446 string = "(" + t445
            var t447 string = t446 + ")"
            jp441 = t447
        } else {
            var t448 string = typ_to_string(x198)
            jp441 = t448
        }
        var s2__26 string = typ_to_string(x199)
        var t442 string = jp441 + " -> "
        var t443 string = t442 + s2__26
        return t443
    default:
        panic("non-exhaustive match")
    }
}

func env_empty() *_goml_vec_EnvEntry {
    var inline999 *_goml_vec_EnvEntry = vec_new__Vec_8EnvEntry()
    return inline999
}

func env_lookup(env__28 *_goml_vec_EnvEntry, name__29 string) Option__Typ {
    var t453 int
    var inline1025 int = vec_len__Vec_8EnvEntry(env__28)
    t453 = inline1025
    var t454 int = t453 - 1
    var i__30 *ref_int_x
    var inline1023 *ref_int_x = ref__Ref_3int(t454)
    i__30 = inline1023
    var found__31 *ref_Option__Typ_x
    var inline1021 *ref_Option__Typ_x = ref__Ref_11Option__Typ(None{})
    found__31 = inline1021
    var done__32 *ref_bool_x
    var inline1018 bool = false
    var inline1019 *ref_bool_x = ref__Ref_4bool(inline1018)
    done__32 = inline1019
    Loop_loop457:
    for {
        var t470 bool
        var inline1014 bool = ref_get__Ref_4bool(done__32)
        t470 = inline1014
        var t471 bool = !t470
        var jp459 bool
        if t471 {
            var t472 int
            var inline1001 int = ref_get__Ref_3int(i__30)
            t472 = inline1001
            var t473 bool = t472 >= 0
            jp459 = t473
        } else {
            jp459 = false
        }
        if jp459 {
            var t460 int
            var inline1012 int = ref_get__Ref_3int(i__30)
            t460 = inline1012
            var entry__33 EnvEntry = vec_get__Vec_8EnvEntry(env__28, t460)
            var t462 string = entry__33.name
            var t463 bool = t462 == name__29
            if t463 {
                var t464 Typ = entry__33.ty
                var t465 Option__Typ = Some{
                    _0: t464,
                }
                ref_set__Ref_11Option__Typ(found__31, t465)
                var inline1003 bool = true
                ref_set__Ref_4bool(done__32, inline1003)
                continue
            } else {
                var t467 int
                var inline1010 int = ref_get__Ref_3int(i__30)
                t467 = inline1010
                var t468 int = t467 - 1
                ref_set__Ref_3int(i__30, t468)
                continue
            }
        } else {
            break Loop_loop457
        }
    }
    var inline1016 Option__Typ = ref_get__Ref_11Option__Typ(found__31)
    return inline1016
}

func subst_lookup(subst__34 *_goml_vec_SubstEntry, name__35 string) Option__Typ {
    var t476 int
    var inline1051 int = vec_len__Vec_10SubstEntry(subst__34)
    t476 = inline1051
    var t477 int = t476 - 1
    var i__36 *ref_int_x
    var inline1049 *ref_int_x = ref__Ref_3int(t477)
    i__36 = inline1049
    var found__37 *ref_Option__Typ_x
    var inline1047 *ref_Option__Typ_x = ref__Ref_11Option__Typ(None{})
    found__37 = inline1047
    var done__38 *ref_bool_x
    var inline1044 bool = false
    var inline1045 *ref_bool_x = ref__Ref_4bool(inline1044)
    done__38 = inline1045
    Loop_loop480:
    for {
        var t493 bool
        var inline1040 bool = ref_get__Ref_4bool(done__38)
        t493 = inline1040
        var t494 bool = !t493
        var jp482 bool
        if t494 {
            var t495 int
            var inline1027 int = ref_get__Ref_3int(i__36)
            t495 = inline1027
            var t496 bool = t495 >= 0
            jp482 = t496
        } else {
            jp482 = false
        }
        if jp482 {
            var t483 int
            var inline1038 int = ref_get__Ref_3int(i__36)
            t483 = inline1038
            var entry__39 SubstEntry = vec_get__Vec_10SubstEntry(subst__34, t483)
            var t485 string = entry__39.name
            var t486 bool = t485 == name__35
            if t486 {
                var t487 Typ = entry__39.ty
                var t488 Option__Typ = Some{
                    _0: t487,
                }
                ref_set__Ref_11Option__Typ(found__37, t488)
                var inline1029 bool = true
                ref_set__Ref_4bool(done__38, inline1029)
                continue
            } else {
                var t490 int
                var inline1036 int = ref_get__Ref_3int(i__36)
                t490 = inline1036
                var t491 int = t490 - 1
                ref_set__Ref_3int(i__36, t491)
                continue
            }
        } else {
            break Loop_loop480
        }
    }
    var inline1042 Option__Typ = ref_get__Ref_11Option__Typ(found__37)
    return inline1042
}

func occurs(st__40 CheckerState, tvr__41 *ref_Tv_x, ty__42 Typ) Result__unit__string {
    switch ty__42.(type) {
    case TVar:
        var x208 *ref_Tv_x = ty__42.(TVar)._0
        var t503 bool = ptr_eq__Ref_2Tv(tvr__41, x208)
        if t503 {
            var t504 Result__unit__string = Result__unit__string_Err{
                _0: "occurs check",
            }
            return t504
        } else {
            var mtmp212 Tv
            var inline1059 Tv = ref_get__Ref_2Tv(x208)
            mtmp212 = inline1059
            switch mtmp212.(type) {
            case Unbound:
                var x213 string = mtmp212.(Unbound)._0
                var x214 int32 = mtmp212.(Unbound)._1
                var mtmp216 Tv
                var inline1057 Tv = ref_get__Ref_2Tv(tvr__41)
                mtmp216 = inline1057
                var jp508 int32
                switch mtmp216.(type) {
                case Unbound:
                    var x218 int32 = mtmp216.(Unbound)._1
                    var inline1053 bool = x218 < x214
                    if inline1053 {
                        jp508 = x218
                    } else {
                        jp508 = x214
                    }
                default:
                    jp508 = x214
                }
                var t509 Tv = Unbound{
                    _0: x213,
                    _1: jp508,
                }
                ref_set__Ref_2Tv(x208, t509)
                var t510 Result__unit__string = Result__unit__string_Ok{
                    _0: struct{}{},
                }
                return t510
            case Link:
                var x215 Typ = mtmp212.(Link)._0
                var t512 Result__unit__string = occurs(st__40, tvr__41, x215)
                return t512
            default:
                panic("non-exhaustive match")
            }
        }
    case TArrow:
        var x210 Typ = ty__42.(TArrow)._0
        var x211 Typ = ty__42.(TArrow)._1
        var mtmp221 Result__unit__string = occurs(st__40, tvr__41, x210)
        switch mtmp221.(type) {
        case Result__unit__string_Ok:
            var t515 Result__unit__string = occurs(st__40, tvr__41, x211)
            return t515
        case Result__unit__string_Err:
            var x223 string = mtmp221.(Result__unit__string_Err)._0
            var t516 Result__unit__string = Result__unit__string_Err{
                _0: x223,
            }
            return t516
        default:
            panic("non-exhaustive match")
        }
    default:
        var t517 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        return t517
    }
}

func unify(st__52 CheckerState, t1__53 Typ, t2__54 Typ) Result__unit__string {
    switch t2__54.(type) {
    case TVar:
        var x227 *ref_Tv_x = t2__54.(TVar)._0
        switch t1__53.(type) {
        case TVar:
            var x231 *ref_Tv_x = t1__53.(TVar)._0
            var t526 bool = ptr_eq__Ref_2Tv(x231, x227)
            if t526 {
                var t527 Result__unit__string = Result__unit__string_Ok{
                    _0: struct{}{},
                }
                return t527
            } else {
                var mtmp235 Tv
                var inline1065 Tv = ref_get__Ref_2Tv(x231)
                mtmp235 = inline1065
                switch mtmp235.(type) {
                case Unbound:
                    var mtmp239 Tv
                    var inline1063 Tv = ref_get__Ref_2Tv(x227)
                    mtmp239 = inline1063
                    switch mtmp239.(type) {
                    case Unbound:
                        var t532 Typ = TVar{
                            _0: x227,
                        }
                        var mtmp243 Result__unit__string = occurs(st__52, x231, t532)
                        switch mtmp243.(type) {
                        case Result__unit__string_Ok:
                            var t535 Typ = TVar{
                                _0: x227,
                            }
                            var t536 Tv = Link{
                                _0: t535,
                            }
                            ref_set__Ref_2Tv(x231, t536)
                            var t537 Result__unit__string = Result__unit__string_Ok{
                                _0: struct{}{},
                            }
                            return t537
                        case Result__unit__string_Err:
                            var x245 string = mtmp243.(Result__unit__string_Err)._0
                            var t538 Result__unit__string = Result__unit__string_Err{
                                _0: x245,
                            }
                            return t538
                        default:
                            panic("non-exhaustive match")
                        }
                    case Link:
                        var x242 Typ = mtmp239.(Link)._0
                        var t539 Typ = TVar{
                            _0: x231,
                        }
                        var t540 Result__unit__string = unify(st__52, t539, x242)
                        return t540
                    default:
                        panic("non-exhaustive match")
                    }
                case Link:
                    var x238 Typ = mtmp235.(Link)._0
                    var t541 Typ = TVar{
                        _0: x227,
                    }
                    var t542 Result__unit__string = unify(st__52, x238, t541)
                    return t542
                default:
                    panic("non-exhaustive match")
                }
            }
        default:
            var mtmp247 Tv
            var inline1069 Tv = ref_get__Ref_2Tv(x227)
            mtmp247 = inline1069
            switch mtmp247.(type) {
            case Unbound:
                var mtmp251 Result__unit__string = occurs(st__52, x227, t1__53)
                switch mtmp251.(type) {
                case Result__unit__string_Ok:
                    var t547 Tv = Link{
                        _0: t1__53,
                    }
                    ref_set__Ref_2Tv(x227, t547)
                    var t548 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    return t548
                case Result__unit__string_Err:
                    var x253 string = mtmp251.(Result__unit__string_Err)._0
                    var t549 Result__unit__string = Result__unit__string_Err{
                        _0: x253,
                    }
                    return t549
                default:
                    panic("non-exhaustive match")
                }
            case Link:
                var x250 Typ = mtmp247.(Link)._0
                var t550 Result__unit__string = unify(st__52, t1__53, x250)
                return t550
            default:
                panic("non-exhaustive match")
            }
        }
    case TArrow:
        var x229 Typ = t2__54.(TArrow)._0
        var x230 Typ = t2__54.(TArrow)._1
        switch t1__53.(type) {
        case TVar:
            var x255 *ref_Tv_x = t1__53.(TVar)._0
            var mtmp259 Tv
            var inline1073 Tv = ref_get__Ref_2Tv(x255)
            mtmp259 = inline1073
            switch mtmp259.(type) {
            case Unbound:
                var mtmp263 Result__unit__string = occurs(st__52, x255, t2__54)
                switch mtmp263.(type) {
                case Result__unit__string_Ok:
                    var t557 Tv = Link{
                        _0: t2__54,
                    }
                    ref_set__Ref_2Tv(x255, t557)
                    var t558 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    return t558
                case Result__unit__string_Err:
                    var x265 string = mtmp263.(Result__unit__string_Err)._0
                    var t559 Result__unit__string = Result__unit__string_Err{
                        _0: x265,
                    }
                    return t559
                default:
                    panic("non-exhaustive match")
                }
            case Link:
                var x262 Typ = mtmp259.(Link)._0
                var t560 Result__unit__string = unify(st__52, x262, t2__54)
                return t560
            default:
                panic("non-exhaustive match")
            }
        case TArrow:
            var x257 Typ = t1__53.(TArrow)._0
            var x258 Typ = t1__53.(TArrow)._1
            var mtmp267 Result__unit__string = unify(st__52, x257, x229)
            switch mtmp267.(type) {
            case Result__unit__string_Ok:
                var t563 Result__unit__string = unify(st__52, x258, x230)
                return t563
            case Result__unit__string_Err:
                var x269 string = mtmp267.(Result__unit__string_Err)._0
                var t564 Result__unit__string = Result__unit__string_Err{
                    _0: x269,
                }
                return t564
            default:
                panic("non-exhaustive match")
            }
        default:
            var t565 Result__unit__string = Result__unit__string_Err{
                _0: "unify error",
            }
            return t565
        }
    default:
        switch t1__53.(type) {
        case TVar:
            var x270 *ref_Tv_x = t1__53.(TVar)._0
            var mtmp274 Tv
            var inline1077 Tv = ref_get__Ref_2Tv(x270)
            mtmp274 = inline1077
            switch mtmp274.(type) {
            case Unbound:
                var mtmp278 Result__unit__string = occurs(st__52, x270, t2__54)
                switch mtmp278.(type) {
                case Result__unit__string_Ok:
                    var t572 Tv = Link{
                        _0: t2__54,
                    }
                    ref_set__Ref_2Tv(x270, t572)
                    var t573 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    return t573
                case Result__unit__string_Err:
                    var x280 string = mtmp278.(Result__unit__string_Err)._0
                    var t574 Result__unit__string = Result__unit__string_Err{
                        _0: x280,
                    }
                    return t574
                default:
                    panic("non-exhaustive match")
                }
            case Link:
                var x277 Typ = mtmp274.(Link)._0
                var t575 Result__unit__string = unify(st__52, x277, t2__54)
                return t575
            default:
                panic("non-exhaustive match")
            }
        default:
            var t576 Result__unit__string = Result__unit__string_Err{
                _0: "unify error",
            }
            return t576
        }
    }
}

func gen(st__73 CheckerState, ty__74 Typ) Typ {
    switch ty__74.(type) {
    case TVar:
        var x282 *ref_Tv_x = ty__74.(TVar)._0
        var mtmp286 Tv
        var inline1081 Tv = ref_get__Ref_2Tv(x282)
        mtmp286 = inline1081
        switch mtmp286.(type) {
        case Unbound:
            var x287 string = mtmp286.(Unbound)._0
            var x288 int32 = mtmp286.(Unbound)._1
            var t583 *ref_int32_x = st__73.current_level
            var cur__78 int32
            var inline1079 int32 = ref_get__Ref_5int32(t583)
            cur__78 = inline1079
            var t586 bool = x288 > cur__78
            if t586 {
                var t587 Typ = QVar{
                    _0: x287,
                }
                return t587
            } else {
                var t588 Typ = TVar{
                    _0: x282,
                }
                return t588
            }
        case Link:
            var x289 Typ = mtmp286.(Link)._0
            var t589 Typ = gen(st__73, x289)
            return t589
        default:
            panic("non-exhaustive match")
        }
    case TArrow:
        var x284 Typ = ty__74.(TArrow)._0
        var x285 Typ = ty__74.(TArrow)._1
        var t590 Typ = gen(st__73, x284)
        var t591 Typ = gen(st__73, x285)
        var t592 Typ = TArrow{
            _0: t590,
            _1: t591,
        }
        return t592
    default:
        return ty__74
    }
}

func inst_loop(st__83 CheckerState, subst__84 *_goml_vec_SubstEntry, ty__85 Typ) Tuple2_3Typ_16Vec_10SubstEntry {
    switch ty__85.(type) {
    case TVar:
        var x290 *ref_Tv_x = ty__85.(TVar)._0
        var mtmp294 Tv
        var inline1083 Tv = ref_get__Ref_2Tv(x290)
        mtmp294 = inline1083
        switch mtmp294.(type) {
        case Link:
            var x297 Typ = mtmp294.(Link)._0
            var t599 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, subst__84, x297)
            return t599
        default:
            var t600 Typ = TVar{
                _0: x290,
            }
            var t601 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: t600,
                _1: subst__84,
            }
            return t601
        }
    case QVar:
        var x291 string = ty__85.(QVar)._0
        var mtmp298 Option__Typ = subst_lookup(subst__84, x291)
        switch mtmp298.(type) {
        case None:
            var tv__88 Typ
            var inline1085 string = gensym(st__83)
            var inline1086 *ref_int32_x = st__83.current_level
            var inline1087 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline1086)
            var inline1088 Tv = Unbound{
                _0: inline1085,
                _1: inline1087,
            }
            var inline1089 *ref_Tv_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Tv(inline1088)
            var inline1090 Typ = TVar{
                _0: inline1089,
            }
            tv__88 = inline1090
            var t604 SubstEntry = SubstEntry{
                name: x291,
                ty: tv__88,
            }
            var new_subst__89 *_goml_vec_SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SubstEntry(subst__84, t604)
            var t605 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: tv__88,
                _1: new_subst__89,
            }
            return t605
        case Some:
            var x299 Typ = mtmp298.(Some)._0
            var t606 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: x299,
                _1: subst__84,
            }
            return t606
        default:
            panic("non-exhaustive match")
        }
    case TArrow:
        var x292 Typ = ty__85.(TArrow)._0
        var x293 Typ = ty__85.(TArrow)._1
        var mtmp300 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, subst__84, x292)
        var x301 Typ = mtmp300._0
        var x302 *_goml_vec_SubstEntry = mtmp300._1
        var mtmp303 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, x302, x293)
        var x304 Typ = mtmp303._0
        var x305 *_goml_vec_SubstEntry = mtmp303._1
        var t607 Typ = TArrow{
            _0: x301,
            _1: x304,
        }
        var t608 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
            _0: t607,
            _1: x305,
        }
        return t608
    default:
        panic("non-exhaustive match")
    }
}

func typeof(st__102 CheckerState, env__103 *_goml_vec_EnvEntry, e__104 Exp) Result__Typ__string {
    switch e__104.(type) {
    case Var:
        var x309 string = e__104.(Var)._0
        var mtmp317 Option__Typ = env_lookup(env__103, x309)
        switch mtmp317.(type) {
        case None:
            var t617 Result__Typ__string = Result__Typ__string_Err{
                _0: "unbound var",
            }
            return t617
        case Some:
            var x318 Typ = mtmp317.(Some)._0
            var t618 Typ
            var inline1094 *_goml_vec_SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SubstEntry()
            var inline1095 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__102, inline1094, x318)
            var inline1096 Typ = inline1095._0
            t618 = inline1096
            var t619 Result__Typ__string = Result__Typ__string_Ok{
                _0: t618,
            }
            return t619
        default:
            panic("non-exhaustive match")
        }
    case App:
        var x310 Exp = e__104.(App)._0
        var x311 Exp = e__104.(App)._1
        var mtmp319 Result__Typ__string = typeof(st__102, env__103, x310)
        switch mtmp319.(type) {
        case Result__Typ__string_Ok:
            var x320 Typ = mtmp319.(Result__Typ__string_Ok)._0
            var mtmp322 Result__Typ__string = typeof(st__102, env__103, x311)
            switch mtmp322.(type) {
            case Result__Typ__string_Ok:
                var x323 Typ = mtmp322.(Result__Typ__string_Ok)._0
                var ty_res__119 Typ
                var inline1099 string = gensym(st__102)
                var inline1100 *ref_int32_x = st__102.current_level
                var inline1101 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline1100)
                var inline1102 Tv = Unbound{
                    _0: inline1099,
                    _1: inline1101,
                }
                var inline1103 *ref_Tv_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Tv(inline1102)
                var inline1104 Typ = TVar{
                    _0: inline1103,
                }
                ty_res__119 = inline1104
                var arrow__120 Typ = TArrow{
                    _0: x323,
                    _1: ty_res__119,
                }
                var mtmp325 Result__unit__string = unify(st__102, x320, arrow__120)
                switch mtmp325.(type) {
                case Result__unit__string_Ok:
                    var t626 Result__Typ__string = Result__Typ__string_Ok{
                        _0: ty_res__119,
                    }
                    return t626
                case Result__unit__string_Err:
                    var x327 string = mtmp325.(Result__unit__string_Err)._0
                    var t627 Result__Typ__string = Result__Typ__string_Err{
                        _0: x327,
                    }
                    return t627
                default:
                    panic("non-exhaustive match")
                }
            case Result__Typ__string_Err:
                var x324 string = mtmp322.(Result__Typ__string_Err)._0
                var t628 Result__Typ__string = Result__Typ__string_Err{
                    _0: x324,
                }
                return t628
            default:
                panic("non-exhaustive match")
            }
        case Result__Typ__string_Err:
            var x321 string = mtmp319.(Result__Typ__string_Err)._0
            var t629 Result__Typ__string = Result__Typ__string_Err{
                _0: x321,
            }
            return t629
        default:
            panic("non-exhaustive match")
        }
    case Lam:
        var x312 string = e__104.(Lam)._0
        var x313 Exp = e__104.(Lam)._1
        var ty_x__109 Typ
        var inline1106 string = gensym(st__102)
        var inline1107 *ref_int32_x = st__102.current_level
        var inline1108 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline1107)
        var inline1109 Tv = Unbound{
            _0: inline1106,
            _1: inline1108,
        }
        var inline1110 *ref_Tv_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Tv(inline1109)
        var inline1111 Typ = TVar{
            _0: inline1110,
        }
        ty_x__109 = inline1111
        var t630 EnvEntry = EnvEntry{
            name: x312,
            ty: ty_x__109,
        }
        var env2__110 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__EnvEntry(env__103, t630)
        var mtmp328 Result__Typ__string = typeof(st__102, env2__110, x313)
        switch mtmp328.(type) {
        case Result__Typ__string_Ok:
            var x329 Typ = mtmp328.(Result__Typ__string_Ok)._0
            var t633 Typ = TArrow{
                _0: ty_x__109,
                _1: x329,
            }
            var t634 Result__Typ__string = Result__Typ__string_Ok{
                _0: t633,
            }
            return t634
        case Result__Typ__string_Err:
            var x330 string = mtmp328.(Result__Typ__string_Err)._0
            var t635 Result__Typ__string = Result__Typ__string_Err{
                _0: x330,
            }
            return t635
        default:
            panic("non-exhaustive match")
        }
    case Let:
        var x314 string = e__104.(Let)._0
        var x315 Exp = e__104.(Let)._1
        var x316 Exp = e__104.(Let)._2
        var inline1119 *ref_int32_x = st__102.current_level
        var inline1120 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline1119)
        var inline1121 *ref_int32_x = st__102.current_level
        var inline1122 int32 = inline1120 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline1121, inline1122)
        var ty_e__125 Result__Typ__string = typeof(st__102, env__103, x315)
        var inline1113 *ref_int32_x = st__102.current_level
        var inline1114 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline1113)
        var inline1115 *ref_int32_x = st__102.current_level
        var inline1116 int32 = inline1114 - 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline1115, inline1116)
        switch ty_e__125.(type) {
        case Result__Typ__string_Ok:
            var x333 Typ = ty_e__125.(Result__Typ__string_Ok)._0
            var t638 Typ = gen(st__102, x333)
            var t639 EnvEntry = EnvEntry{
                name: x314,
                ty: t638,
            }
            var env2__128 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__EnvEntry(env__103, t639)
            var t640 Result__Typ__string = typeof(st__102, env2__128, x316)
            return t640
        case Result__Typ__string_Err:
            var x334 string = ty_e__125.(Result__Typ__string_Err)._0
            var t641 Result__Typ__string = Result__Typ__string_Err{
                _0: x334,
            }
            return t641
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func exp_var(name__129 string) Exp {
    var t644 Exp = Var{
        _0: name__129,
    }
    return t644
}

func exp_lam(name__130 string, body__131 Exp) Exp {
    var t647 Exp = Lam{
        _0: name__130,
        _1: body__131,
    }
    return t647
}

func exp_app(a__132 Exp, b__133 Exp) Exp {
    var t650 Exp = App{
        _0: a__132,
        _1: b__133,
    }
    return t650
}

func exp_let(name__134 string, a__135 Exp, b__136 Exp) Exp {
    var t653 Exp = Let{
        _0: name__134,
        _1: a__135,
        _2: b__136,
    }
    return t653
}

func show_result(label__137 string, res__138 Result__Typ__string) struct{} {
    switch res__138.(type) {
    case Result__Typ__string_Ok:
        var x335 Typ = res__138.(Result__Typ__string_Ok)._0
        var t656 string = label__137 + ": "
        var t657 string = typ_to_string(x335)
        var t658 string = t656 + t657
        var inline1125 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t658)
        _goml_runtime_core_string_println(inline1125)
        return struct{}{}
    case Result__Typ__string_Err:
        var x336 string = res__138.(Result__Typ__string_Err)._0
        var t660 string = label__137 + ": "
        var t661 string = t660 + x336
        var inline1128 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t661)
        _goml_runtime_core_string_println(inline1128)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var st__141 CheckerState = state_new()
    var t664 Exp = exp_var("x")
    var id__142 Exp = exp_lam("x", t664)
    var t665 Exp = exp_var("x")
    var t666 Exp = exp_var("y")
    var t667 Exp = exp_app(t665, t666)
    var t668 Exp = exp_lam("y", t667)
    var c1__143 Exp = exp_lam("x", t668)
    reset_type_variables(st__141)
    var t669 *_goml_vec_EnvEntry = env_empty()
    var t670 Result__Typ__string = typeof(st__141, t669, id__142)
    show_result("id", t670)
    reset_type_variables(st__141)
    var t671 *_goml_vec_EnvEntry = env_empty()
    var t672 Result__Typ__string = typeof(st__141, t671, c1__143)
    show_result("c1", t672)
    reset_type_variables(st__141)
    var t673 *_goml_vec_EnvEntry = env_empty()
    var t674 Exp = exp_var("x")
    var t675 Exp = exp_let("x", c1__143, t674)
    var t676 Result__Typ__string = typeof(st__141, t673, t675)
    show_result("let_x_c1_x", t676)
    reset_type_variables(st__141)
    var t677 *_goml_vec_EnvEntry = env_empty()
    var t678 Exp = exp_var("z")
    var t679 Exp = exp_lam("z", t678)
    var t680 Exp = exp_var("y")
    var t681 Exp = exp_let("y", t679, t680)
    var t682 Result__Typ__string = typeof(st__141, t677, t681)
    show_result("let_y_id_y", t682)
    reset_type_variables(st__141)
    var t683 *_goml_vec_EnvEntry = env_empty()
    var t684 Exp = exp_var("z")
    var t685 Exp = exp_lam("z", t684)
    var t686 Exp = exp_var("y")
    var t687 Exp = exp_let("y", t685, t686)
    var t688 Exp = exp_lam("x", t687)
    var t689 Result__Typ__string = typeof(st__141, t683, t688)
    show_result("lam_x_let_y_id_y", t689)
    reset_type_variables(st__141)
    var t690 *_goml_vec_EnvEntry = env_empty()
    var t691 Exp = exp_var("z")
    var t692 Exp = exp_lam("z", t691)
    var t693 Exp = exp_var("y")
    var t694 Exp = exp_var("x")
    var t695 Exp = exp_app(t693, t694)
    var t696 Exp = exp_let("y", t692, t695)
    var t697 Exp = exp_lam("x", t696)
    var t698 Result__Typ__string = typeof(st__141, t690, t697)
    show_result("lam_x_let_y_id_yx", t698)
    reset_type_variables(st__141)
    var t699 *_goml_vec_EnvEntry = env_empty()
    var t700 Exp = exp_var("x")
    var t701 Exp = exp_var("x")
    var t702 Exp = exp_app(t700, t701)
    var t703 Exp = exp_lam("x", t702)
    var t704 Result__Typ__string = typeof(st__141, t699, t703)
    show_result("self_apply", t704)
    reset_type_variables(st__141)
    var t705 *_goml_vec_EnvEntry = env_empty()
    var t706 Exp = exp_var("x")
    var t707 Exp = exp_var("x")
    var t708 Exp = exp_let("x", t706, t707)
    var t709 Result__Typ__string = typeof(st__141, t705, t708)
    show_result("unbound_var", t709)
    reset_type_variables(st__141)
    var t710 *_goml_vec_EnvEntry = env_empty()
    var t711 Exp = exp_var("y")
    var t712 Exp = exp_var("y")
    var t713 Exp = exp_var("z")
    var t714 Exp = exp_app(t712, t713)
    var t715 Exp = exp_lam("z", t714)
    var t716 Exp = exp_app(t711, t715)
    var t717 Exp = exp_lam("y", t716)
    var t718 Result__Typ__string = typeof(st__141, t710, t717)
    show_result("max_heiber", t718)
    reset_type_variables(st__141)
    var t719 *_goml_vec_EnvEntry = env_empty()
    var t720 Exp = exp_var("k")
    var t721 Exp = exp_var("k")
    var t722 Exp = exp_var("x")
    var t723 Exp = exp_app(t721, t722)
    var t724 Exp = exp_var("y")
    var t725 Exp = exp_app(t723, t724)
    var t726 Exp = exp_app(t720, t725)
    var t727 Exp = exp_var("k")
    var t728 Exp = exp_var("y")
    var t729 Exp = exp_app(t727, t728)
    var t730 Exp = exp_var("x")
    var t731 Exp = exp_app(t729, t730)
    var t732 Exp = exp_app(t726, t731)
    var t733 Exp = exp_lam("k", t732)
    var t734 Exp = exp_lam("y", t733)
    var t735 Exp = exp_lam("x", t734)
    var t736 Result__Typ__string = typeof(st__141, t719, t735)
    show_result("kirang", t736)
    reset_type_variables(st__141)
    var t737 *_goml_vec_EnvEntry = env_empty()
    var t738 Exp = exp_var("id")
    var t739 Exp = exp_var("id")
    var t740 Exp = exp_app(t738, t739)
    var t741 Exp = exp_let("id", id__142, t740)
    var t742 Result__Typ__string = typeof(st__141, t737, t741)
    show_result("let_id_idid", t742)
    reset_type_variables(st__141)
    var t743 *_goml_vec_EnvEntry = env_empty()
    var t744 Exp = exp_var("x")
    var t745 Exp = exp_app(t744, id__142)
    var t746 Exp = exp_var("z")
    var t747 Exp = exp_let("z", t745, t746)
    var t748 Exp = exp_var("y")
    var t749 Exp = exp_let("y", t747, t748)
    var t750 Exp = exp_let("x", c1__143, t749)
    var t751 Result__Typ__string = typeof(st__141, t743, t750)
    show_result("nested_lets", t751)
    reset_type_variables(st__141)
    var t752 *_goml_vec_EnvEntry = env_empty()
    var t753 Exp = exp_var("x")
    var t754 Exp = exp_var("y")
    var t755 Exp = exp_app(t753, t754)
    var t756 Exp = exp_var("y")
    var t757 Exp = exp_var("x")
    var t758 Exp = exp_app(t756, t757)
    var t759 Exp = exp_lam("x", t758)
    var t760 Exp = exp_let("x", t755, t759)
    var t761 Exp = exp_lam("y", t760)
    var t762 Exp = exp_lam("x", t761)
    var t763 Result__Typ__string = typeof(st__141, t752, t762)
    show_result("fun_x_fun_y_let_x_xy_fun_x_yx", t763)
    reset_type_variables(st__141)
    var t764 *_goml_vec_EnvEntry = env_empty()
    var t765 Exp = exp_var("x")
    var t766 Exp = exp_var("y")
    var t767 Exp = exp_let("y", t765, t766)
    var t768 Exp = exp_lam("x", t767)
    var t769 Result__Typ__string = typeof(st__141, t764, t768)
    show_result("sound_gen_1", t769)
    reset_type_variables(st__141)
    var t770 *_goml_vec_EnvEntry = env_empty()
    var t771 Exp = exp_var("x")
    var t772 Exp = exp_lam("z", t771)
    var t773 Exp = exp_var("y")
    var t774 Exp = exp_let("y", t772, t773)
    var t775 Exp = exp_lam("x", t774)
    var t776 Result__Typ__string = typeof(st__141, t770, t775)
    show_result("sound_gen_2", t776)
    reset_type_variables(st__141)
    var t777 *_goml_vec_EnvEntry = env_empty()
    var t778 Exp = exp_var("x")
    var t779 Exp = exp_var("z")
    var t780 Exp = exp_app(t778, t779)
    var t781 Exp = exp_lam("z", t780)
    var t782 Exp = exp_var("y")
    var t783 Exp = exp_let("y", t781, t782)
    var t784 Exp = exp_lam("x", t783)
    var t785 Result__Typ__string = typeof(st__141, t777, t784)
    show_result("sound_gen_3", t785)
    reset_type_variables(st__141)
    var t786 *_goml_vec_EnvEntry = env_empty()
    var t787 Exp = exp_var("x")
    var t788 Exp = exp_var("y")
    var t789 Exp = exp_app(t787, t788)
    var t790 Exp = exp_var("x")
    var t791 Exp = exp_var("y")
    var t792 Exp = exp_app(t790, t791)
    var t793 Exp = exp_let("x", t789, t792)
    var t794 Exp = exp_lam("y", t793)
    var t795 Exp = exp_lam("x", t794)
    var t796 Result__Typ__string = typeof(st__141, t786, t795)
    show_result("double_apply", t796)
    reset_type_variables(st__141)
    var t797 *_goml_vec_EnvEntry = env_empty()
    var t798 Exp = exp_var("x")
    var t799 Exp = exp_var("y")
    var t800 Exp = exp_var("y")
    var t801 Exp
    var inline1187 Exp = App{
        _0: t799,
        _1: t800,
    }
    t801 = inline1187
    var t802 Exp
    var inline1184 string = "y"
    var inline1185 Exp = Let{
        _0: inline1184,
        _1: t798,
        _2: t801,
    }
    t802 = inline1185
    var t803 Exp
    var inline1181 string = "x"
    var inline1182 Exp = Lam{
        _0: inline1181,
        _1: t802,
    }
    t803 = inline1182
    var t804 Result__Typ__string = typeof(st__141, t797, t803)
    show_result("sound_gen_occurs", t804)
    var inline1178 *ref_int32_x = st__141.gensym_counter
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline1178, 0)
    var t805 *_goml_vec_EnvEntry
    var inline1176 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__EnvEntry()
    t805 = inline1176
    var t806 Exp
    var inline1173 string = "x"
    var inline1174 Exp = Var{
        _0: inline1173,
    }
    t806 = inline1174
    var t807 Exp
    var inline1171 Exp = App{
        _0: t806,
        _1: id__142,
    }
    t807 = inline1171
    var t808 Exp
    var inline1168 string = "z"
    var inline1169 Exp = Var{
        _0: inline1168,
    }
    t808 = inline1169
    var t809 Exp
    var inline1165 string = "z"
    var inline1166 Exp = Let{
        _0: inline1165,
        _1: t807,
        _2: t808,
    }
    t809 = inline1166
    var t810 Exp
    var inline1162 string = "y"
    var inline1163 Exp = Var{
        _0: inline1162,
    }
    t810 = inline1163
    var t811 Exp
    var inline1159 string = "y"
    var inline1160 Exp = Let{
        _0: inline1159,
        _1: t809,
        _2: t810,
    }
    t811 = inline1160
    var t812 Exp
    var inline1156 string = "x"
    var inline1157 Exp = Lam{
        _0: inline1156,
        _1: t811,
    }
    t812 = inline1157
    var t813 Result__Typ__string = typeof(st__141, t805, t812)
    var inline1143 string = "fun_x_let_y_let_z_x_id_z_y"
    switch t813.(type) {
    case Result__Typ__string_Ok:
        var inline1144 Typ = t813.(Result__Typ__string_Ok)._0
        var inline1146 string = inline1143 + ": "
        var inline1147 string = typ_to_string(inline1144)
        var inline1148 string = inline1146 + inline1147
        println__T_string(inline1148)
    case Result__Typ__string_Err:
        var inline1150 string = t813.(Result__Typ__string_Err)._0
        var inline1152 string = inline1143 + ": "
        var inline1153 string = inline1152 + inline1150
        println__T_string(inline1153)
    default:
        panic("non-exhaustive match")
    }
    var inline1139 string = ""
    var inline1140 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline1139)
    _goml_runtime_core_string_println(inline1140)
    var inline1135 string = "All Done"
    var inline1136 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline1135)
    _goml_runtime_core_string_println(inline1136)
    var inline1131 string = ""
    var inline1132 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline1131)
    _goml_runtime_core_string_println(inline1132)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__272 *ref_int32_x, value__273 int32) struct{} {
    ref_set__Ref_5int32(self__272, value__273)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__271 *ref_int32_x) int32 {
    var t821 int32 = ref_get__Ref_5int32(self__271)
    return t821
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Tv(value__270 Tv) *ref_Tv_x {
    var t830 *ref_Tv_x = ref__Ref_2Tv(value__270)
    return t830
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__EnvEntry() *_goml_vec_EnvEntry {
    var t836 *_goml_vec_EnvEntry = vec_new__Vec_8EnvEntry()
    return t836
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SubstEntry(self__176 *_goml_vec_SubstEntry, elem__177 SubstEntry) *_goml_vec_SubstEntry {
    var t871 int
    var inline1202 int = vec_len__Vec_10SubstEntry(self__176)
    t871 = inline1202
    var t872 int = t871 + 1
    var result__178 *_goml_vec_SubstEntry
    var inline1200 *_goml_vec_SubstEntry = vec_with_capacity__Vec_10SubstEntry(t872)
    result__178 = inline1200
    var index__179 int = 0
    Loop_loop874:
    for {
        var t875 int
        var inline1196 int = vec_len__Vec_10SubstEntry(self__176)
        t875 = inline1196
        var t876 bool = index__179 < t875
        if t876 {
            var t877 SubstEntry = vec_get__Vec_10SubstEntry(self__176, index__179)
            vec_push__Vec_10SubstEntry(result__178, t877)
            var compound_old80 int = index__179
            var compound_value81 int = 1
            var t878 int = compound_old80 + compound_value81
            index__179 = t878
            continue
        } else {
            break Loop_loop874
        }
    }
    vec_push__Vec_10SubstEntry(result__178, elem__177)
    return result__178
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SubstEntry() *_goml_vec_SubstEntry {
    var t882 *_goml_vec_SubstEntry = vec_new__Vec_10SubstEntry()
    return t882
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__EnvEntry(self__176 *_goml_vec_EnvEntry, elem__177 EnvEntry) *_goml_vec_EnvEntry {
    var t885 int
    var inline1212 int = vec_len__Vec_8EnvEntry(self__176)
    t885 = inline1212
    var t886 int = t885 + 1
    var result__178 *_goml_vec_EnvEntry
    var inline1210 *_goml_vec_EnvEntry = vec_with_capacity__Vec_8EnvEntry(t886)
    result__178 = inline1210
    var index__179 int = 0
    Loop_loop888:
    for {
        var t889 int
        var inline1206 int = vec_len__Vec_8EnvEntry(self__176)
        t889 = inline1206
        var t890 bool = index__179 < t889
        if t890 {
            var t891 EnvEntry = vec_get__Vec_8EnvEntry(self__176, index__179)
            vec_push__Vec_8EnvEntry(result__178, t891)
            var compound_old80 int = index__179
            var compound_value81 int = 1
            var t892 int = compound_old80 + compound_value81
            index__179 = t892
            continue
        } else {
            break Loop_loop888
        }
    }
    vec_push__Vec_8EnvEntry(result__178, elem__177)
    return result__178
}

func println__T_string(value__1 string) struct{} {
    var t895 string
    t895 = value__1
    _goml_runtime_core_string_println(t895)
    return struct{}{}
}

func char_to_string(value__29 rune) string {
    var t901 uint32 = uint32(rune(value__29))
    var t902 bool
    var inline1215 bool = t901 <= 1114111
    if inline1215 {
        var inline1216 bool = t901 >= 55296
        var inline1218 bool
        if inline1216 {
            var inline1220 bool = t901 <= 57343
            inline1218 = inline1220
        } else {
            inline1218 = false
        }
        var inline1219 bool = !inline1218
        t902 = inline1219
    } else {
        t902 = false
    }
    if t902 {
        var t903 string = _goml_runtime_core_char_to_string(value__29)
        return t903
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
