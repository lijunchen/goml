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
    var t385 *ref_int32_x
    var inline954 int32 = 0
    var inline955 *ref_int32_x = ref__Ref_5int32(inline954)
    t385 = inline955
    var t386 *ref_int32_x
    var inline951 int32 = 1
    var inline952 *ref_int32_x = ref__Ref_5int32(inline951)
    t386 = inline952
    var t387 CheckerState = CheckerState{
        gensym_counter: t385,
        current_level: t386,
    }
    return t387
}

func reset_type_variables(st__2 CheckerState) struct{} {
    var inline966 *ref_int32_x = st__2.gensym_counter
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline966, 0)
    var inline963 *ref_int32_x = st__2.current_level
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline963, 1)
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
    var t413 *ref_int32_x = st__10.gensym_counter
    var n__11 int32
    var inline983 int32 = ref_get__Ref_5int32(t413)
    n__11 = inline983
    var t414 *ref_int32_x = st__10.gensym_counter
    var t415 int32 = n__11 + 1
    ref_set__Ref_5int32(t414, t415)
    var t418 bool = n__11 < 26
    if t418 {
        var t419 rune = nth_letter(n__11)
        var inline977 string = char_to_string(t419)
        return inline977
    } else {
        var t421 string
        var inline979 string = _goml_runtime_core_int32_to_string(n__11)
        t421 = inline979
        var t422 string = "t" + t421
        return t422
    }
}

func typ_is_arrow(ty__15 Typ) bool {
    switch ty__15.(type) {
    case TVar:
        var x193 *ref_Tv_x = ty__15.(TVar)._0
        var mtmp197 Tv
        var inline1000 Tv = ref_get__Ref_2Tv(x193)
        mtmp197 = inline1000
        switch mtmp197.(type) {
        case Link:
            var x200 Typ = mtmp197.(Link)._0
            var t435 bool = typ_is_arrow(x200)
            return t435
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
        var x201 *ref_Tv_x = ty__18.(TVar)._0
        var mtmp205 Tv
        var inline1002 Tv = ref_get__Ref_2Tv(x201)
        mtmp205 = inline1002
        switch mtmp205.(type) {
        case Unbound:
            var x206 string = mtmp205.(Unbound)._0
            var t442 string = "'" + x206
            return t442
        case Link:
            var x208 Typ = mtmp205.(Link)._0
            var t443 string = typ_to_string(x208)
            return t443
        default:
            panic("non-exhaustive match")
        }
    case QVar:
        var x202 string = ty__18.(QVar)._0
        var t444 string = "'" + x202
        return t444
    case TArrow:
        var x203 Typ = ty__18.(TArrow)._0
        var x204 Typ = ty__18.(TArrow)._1
        var t449 bool = typ_is_arrow(x203)
        var jp446 string
        if t449 {
            var t450 string = typ_to_string(x203)
            var t451 string = "(" + t450
            var t452 string = t451 + ")"
            jp446 = t452
        } else {
            var t453 string = typ_to_string(x203)
            jp446 = t453
        }
        var s2__26 string = typ_to_string(x204)
        var t447 string = jp446 + " -> "
        var t448 string = t447 + s2__26
        return t448
    default:
        panic("non-exhaustive match")
    }
}

func env_empty() *_goml_vec_EnvEntry {
    var inline1004 *_goml_vec_EnvEntry = vec_new__Vec_8EnvEntry()
    return inline1004
}

func env_lookup(env__28 *_goml_vec_EnvEntry, name__29 string) Option__Typ {
    var t458 int
    var inline1030 int = vec_len__Vec_8EnvEntry(env__28)
    t458 = inline1030
    var t459 int = t458 - 1
    var i__30 *ref_int_x
    var inline1028 *ref_int_x = ref__Ref_3int(t459)
    i__30 = inline1028
    var found__31 *ref_Option__Typ_x
    var inline1026 *ref_Option__Typ_x = ref__Ref_11Option__Typ(None{})
    found__31 = inline1026
    var done__32 *ref_bool_x
    var inline1023 bool = false
    var inline1024 *ref_bool_x = ref__Ref_4bool(inline1023)
    done__32 = inline1024
    Loop_loop462:
    for {
        var t475 bool
        var inline1019 bool = ref_get__Ref_4bool(done__32)
        t475 = inline1019
        var t476 bool = !t475
        var jp464 bool
        if t476 {
            var t477 int
            var inline1006 int = ref_get__Ref_3int(i__30)
            t477 = inline1006
            var t478 bool = t477 >= 0
            jp464 = t478
        } else {
            jp464 = false
        }
        if jp464 {
            var t465 int
            var inline1017 int = ref_get__Ref_3int(i__30)
            t465 = inline1017
            var entry__33 EnvEntry = vec_get__Vec_8EnvEntry(env__28, t465)
            var t467 string = entry__33.name
            var t468 bool = t467 == name__29
            if t468 {
                var t469 Typ = entry__33.ty
                var t470 Option__Typ = Some{
                    _0: t469,
                }
                ref_set__Ref_11Option__Typ(found__31, t470)
                var inline1008 bool = true
                ref_set__Ref_4bool(done__32, inline1008)
                continue
            } else {
                var t472 int
                var inline1015 int = ref_get__Ref_3int(i__30)
                t472 = inline1015
                var t473 int = t472 - 1
                ref_set__Ref_3int(i__30, t473)
                continue
            }
        } else {
            break Loop_loop462
        }
    }
    var inline1021 Option__Typ = ref_get__Ref_11Option__Typ(found__31)
    return inline1021
}

func subst_lookup(subst__34 *_goml_vec_SubstEntry, name__35 string) Option__Typ {
    var t481 int
    var inline1056 int = vec_len__Vec_10SubstEntry(subst__34)
    t481 = inline1056
    var t482 int = t481 - 1
    var i__36 *ref_int_x
    var inline1054 *ref_int_x = ref__Ref_3int(t482)
    i__36 = inline1054
    var found__37 *ref_Option__Typ_x
    var inline1052 *ref_Option__Typ_x = ref__Ref_11Option__Typ(None{})
    found__37 = inline1052
    var done__38 *ref_bool_x
    var inline1049 bool = false
    var inline1050 *ref_bool_x = ref__Ref_4bool(inline1049)
    done__38 = inline1050
    Loop_loop485:
    for {
        var t498 bool
        var inline1045 bool = ref_get__Ref_4bool(done__38)
        t498 = inline1045
        var t499 bool = !t498
        var jp487 bool
        if t499 {
            var t500 int
            var inline1032 int = ref_get__Ref_3int(i__36)
            t500 = inline1032
            var t501 bool = t500 >= 0
            jp487 = t501
        } else {
            jp487 = false
        }
        if jp487 {
            var t488 int
            var inline1043 int = ref_get__Ref_3int(i__36)
            t488 = inline1043
            var entry__39 SubstEntry = vec_get__Vec_10SubstEntry(subst__34, t488)
            var t490 string = entry__39.name
            var t491 bool = t490 == name__35
            if t491 {
                var t492 Typ = entry__39.ty
                var t493 Option__Typ = Some{
                    _0: t492,
                }
                ref_set__Ref_11Option__Typ(found__37, t493)
                var inline1034 bool = true
                ref_set__Ref_4bool(done__38, inline1034)
                continue
            } else {
                var t495 int
                var inline1041 int = ref_get__Ref_3int(i__36)
                t495 = inline1041
                var t496 int = t495 - 1
                ref_set__Ref_3int(i__36, t496)
                continue
            }
        } else {
            break Loop_loop485
        }
    }
    var inline1047 Option__Typ = ref_get__Ref_11Option__Typ(found__37)
    return inline1047
}

func occurs(st__40 CheckerState, tvr__41 *ref_Tv_x, ty__42 Typ) Result__unit__string {
    switch ty__42.(type) {
    case TVar:
        var x213 *ref_Tv_x = ty__42.(TVar)._0
        var t508 bool = ptr_eq__Ref_2Tv(tvr__41, x213)
        if t508 {
            var t509 Result__unit__string = Result__unit__string_Err{
                _0: "occurs check",
            }
            return t509
        } else {
            var mtmp217 Tv
            var inline1064 Tv = ref_get__Ref_2Tv(x213)
            mtmp217 = inline1064
            switch mtmp217.(type) {
            case Unbound:
                var x218 string = mtmp217.(Unbound)._0
                var x219 int32 = mtmp217.(Unbound)._1
                var mtmp221 Tv
                var inline1062 Tv = ref_get__Ref_2Tv(tvr__41)
                mtmp221 = inline1062
                var jp513 int32
                switch mtmp221.(type) {
                case Unbound:
                    var x223 int32 = mtmp221.(Unbound)._1
                    var inline1058 bool = x223 < x219
                    if inline1058 {
                        jp513 = x223
                    } else {
                        jp513 = x219
                    }
                default:
                    jp513 = x219
                }
                var t514 Tv = Unbound{
                    _0: x218,
                    _1: jp513,
                }
                ref_set__Ref_2Tv(x213, t514)
                var t515 Result__unit__string = Result__unit__string_Ok{
                    _0: struct{}{},
                }
                return t515
            case Link:
                var x220 Typ = mtmp217.(Link)._0
                var t517 Result__unit__string = occurs(st__40, tvr__41, x220)
                return t517
            default:
                panic("non-exhaustive match")
            }
        }
    case TArrow:
        var x215 Typ = ty__42.(TArrow)._0
        var x216 Typ = ty__42.(TArrow)._1
        var mtmp226 Result__unit__string = occurs(st__40, tvr__41, x215)
        switch mtmp226.(type) {
        case Result__unit__string_Ok:
            var t520 Result__unit__string = occurs(st__40, tvr__41, x216)
            return t520
        case Result__unit__string_Err:
            var x228 string = mtmp226.(Result__unit__string_Err)._0
            var t521 Result__unit__string = Result__unit__string_Err{
                _0: x228,
            }
            return t521
        default:
            panic("non-exhaustive match")
        }
    default:
        var t522 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        return t522
    }
}

func unify(st__52 CheckerState, t1__53 Typ, t2__54 Typ) Result__unit__string {
    switch t2__54.(type) {
    case TVar:
        var x232 *ref_Tv_x = t2__54.(TVar)._0
        switch t1__53.(type) {
        case TVar:
            var x236 *ref_Tv_x = t1__53.(TVar)._0
            var t531 bool = ptr_eq__Ref_2Tv(x236, x232)
            if t531 {
                var t532 Result__unit__string = Result__unit__string_Ok{
                    _0: struct{}{},
                }
                return t532
            } else {
                var mtmp240 Tv
                var inline1070 Tv = ref_get__Ref_2Tv(x236)
                mtmp240 = inline1070
                switch mtmp240.(type) {
                case Unbound:
                    var mtmp244 Tv
                    var inline1068 Tv = ref_get__Ref_2Tv(x232)
                    mtmp244 = inline1068
                    switch mtmp244.(type) {
                    case Unbound:
                        var t537 Typ = TVar{
                            _0: x232,
                        }
                        var mtmp248 Result__unit__string = occurs(st__52, x236, t537)
                        switch mtmp248.(type) {
                        case Result__unit__string_Ok:
                            var t540 Typ = TVar{
                                _0: x232,
                            }
                            var t541 Tv = Link{
                                _0: t540,
                            }
                            ref_set__Ref_2Tv(x236, t541)
                            var t542 Result__unit__string = Result__unit__string_Ok{
                                _0: struct{}{},
                            }
                            return t542
                        case Result__unit__string_Err:
                            var x250 string = mtmp248.(Result__unit__string_Err)._0
                            var t543 Result__unit__string = Result__unit__string_Err{
                                _0: x250,
                            }
                            return t543
                        default:
                            panic("non-exhaustive match")
                        }
                    case Link:
                        var x247 Typ = mtmp244.(Link)._0
                        var t544 Typ = TVar{
                            _0: x236,
                        }
                        var t545 Result__unit__string = unify(st__52, t544, x247)
                        return t545
                    default:
                        panic("non-exhaustive match")
                    }
                case Link:
                    var x243 Typ = mtmp240.(Link)._0
                    var t546 Typ = TVar{
                        _0: x232,
                    }
                    var t547 Result__unit__string = unify(st__52, x243, t546)
                    return t547
                default:
                    panic("non-exhaustive match")
                }
            }
        default:
            var mtmp252 Tv
            var inline1074 Tv = ref_get__Ref_2Tv(x232)
            mtmp252 = inline1074
            switch mtmp252.(type) {
            case Unbound:
                var mtmp256 Result__unit__string = occurs(st__52, x232, t1__53)
                switch mtmp256.(type) {
                case Result__unit__string_Ok:
                    var t552 Tv = Link{
                        _0: t1__53,
                    }
                    ref_set__Ref_2Tv(x232, t552)
                    var t553 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    return t553
                case Result__unit__string_Err:
                    var x258 string = mtmp256.(Result__unit__string_Err)._0
                    var t554 Result__unit__string = Result__unit__string_Err{
                        _0: x258,
                    }
                    return t554
                default:
                    panic("non-exhaustive match")
                }
            case Link:
                var x255 Typ = mtmp252.(Link)._0
                var t555 Result__unit__string = unify(st__52, t1__53, x255)
                return t555
            default:
                panic("non-exhaustive match")
            }
        }
    case TArrow:
        var x234 Typ = t2__54.(TArrow)._0
        var x235 Typ = t2__54.(TArrow)._1
        switch t1__53.(type) {
        case TVar:
            var x260 *ref_Tv_x = t1__53.(TVar)._0
            var mtmp264 Tv
            var inline1078 Tv = ref_get__Ref_2Tv(x260)
            mtmp264 = inline1078
            switch mtmp264.(type) {
            case Unbound:
                var mtmp268 Result__unit__string = occurs(st__52, x260, t2__54)
                switch mtmp268.(type) {
                case Result__unit__string_Ok:
                    var t562 Tv = Link{
                        _0: t2__54,
                    }
                    ref_set__Ref_2Tv(x260, t562)
                    var t563 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    return t563
                case Result__unit__string_Err:
                    var x270 string = mtmp268.(Result__unit__string_Err)._0
                    var t564 Result__unit__string = Result__unit__string_Err{
                        _0: x270,
                    }
                    return t564
                default:
                    panic("non-exhaustive match")
                }
            case Link:
                var x267 Typ = mtmp264.(Link)._0
                var t565 Result__unit__string = unify(st__52, x267, t2__54)
                return t565
            default:
                panic("non-exhaustive match")
            }
        case TArrow:
            var x262 Typ = t1__53.(TArrow)._0
            var x263 Typ = t1__53.(TArrow)._1
            var mtmp272 Result__unit__string = unify(st__52, x262, x234)
            switch mtmp272.(type) {
            case Result__unit__string_Ok:
                var t568 Result__unit__string = unify(st__52, x263, x235)
                return t568
            case Result__unit__string_Err:
                var x274 string = mtmp272.(Result__unit__string_Err)._0
                var t569 Result__unit__string = Result__unit__string_Err{
                    _0: x274,
                }
                return t569
            default:
                panic("non-exhaustive match")
            }
        default:
            var t570 Result__unit__string = Result__unit__string_Err{
                _0: "unify error",
            }
            return t570
        }
    default:
        switch t1__53.(type) {
        case TVar:
            var x275 *ref_Tv_x = t1__53.(TVar)._0
            var mtmp279 Tv
            var inline1082 Tv = ref_get__Ref_2Tv(x275)
            mtmp279 = inline1082
            switch mtmp279.(type) {
            case Unbound:
                var mtmp283 Result__unit__string = occurs(st__52, x275, t2__54)
                switch mtmp283.(type) {
                case Result__unit__string_Ok:
                    var t577 Tv = Link{
                        _0: t2__54,
                    }
                    ref_set__Ref_2Tv(x275, t577)
                    var t578 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    return t578
                case Result__unit__string_Err:
                    var x285 string = mtmp283.(Result__unit__string_Err)._0
                    var t579 Result__unit__string = Result__unit__string_Err{
                        _0: x285,
                    }
                    return t579
                default:
                    panic("non-exhaustive match")
                }
            case Link:
                var x282 Typ = mtmp279.(Link)._0
                var t580 Result__unit__string = unify(st__52, x282, t2__54)
                return t580
            default:
                panic("non-exhaustive match")
            }
        default:
            var t581 Result__unit__string = Result__unit__string_Err{
                _0: "unify error",
            }
            return t581
        }
    }
}

func gen(st__73 CheckerState, ty__74 Typ) Typ {
    switch ty__74.(type) {
    case TVar:
        var x287 *ref_Tv_x = ty__74.(TVar)._0
        var mtmp291 Tv
        var inline1086 Tv = ref_get__Ref_2Tv(x287)
        mtmp291 = inline1086
        switch mtmp291.(type) {
        case Unbound:
            var x292 string = mtmp291.(Unbound)._0
            var x293 int32 = mtmp291.(Unbound)._1
            var t588 *ref_int32_x = st__73.current_level
            var cur__78 int32
            var inline1084 int32 = ref_get__Ref_5int32(t588)
            cur__78 = inline1084
            var t591 bool = x293 > cur__78
            if t591 {
                var t592 Typ = QVar{
                    _0: x292,
                }
                return t592
            } else {
                var t593 Typ = TVar{
                    _0: x287,
                }
                return t593
            }
        case Link:
            var x294 Typ = mtmp291.(Link)._0
            var t594 Typ = gen(st__73, x294)
            return t594
        default:
            panic("non-exhaustive match")
        }
    case TArrow:
        var x289 Typ = ty__74.(TArrow)._0
        var x290 Typ = ty__74.(TArrow)._1
        var t595 Typ = gen(st__73, x289)
        var t596 Typ = gen(st__73, x290)
        var t597 Typ = TArrow{
            _0: t595,
            _1: t596,
        }
        return t597
    default:
        return ty__74
    }
}

func inst_loop(st__83 CheckerState, subst__84 *_goml_vec_SubstEntry, ty__85 Typ) Tuple2_3Typ_16Vec_10SubstEntry {
    switch ty__85.(type) {
    case TVar:
        var x295 *ref_Tv_x = ty__85.(TVar)._0
        var mtmp299 Tv
        var inline1088 Tv = ref_get__Ref_2Tv(x295)
        mtmp299 = inline1088
        switch mtmp299.(type) {
        case Link:
            var x302 Typ = mtmp299.(Link)._0
            var t604 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, subst__84, x302)
            return t604
        default:
            var t605 Typ = TVar{
                _0: x295,
            }
            var t606 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: t605,
                _1: subst__84,
            }
            return t606
        }
    case QVar:
        var x296 string = ty__85.(QVar)._0
        var mtmp303 Option__Typ = subst_lookup(subst__84, x296)
        switch mtmp303.(type) {
        case None:
            var tv__88 Typ
            var inline1090 string = gensym(st__83)
            var inline1091 *ref_int32_x = st__83.current_level
            var inline1092 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline1091)
            var inline1093 Tv = Unbound{
                _0: inline1090,
                _1: inline1092,
            }
            var inline1094 *ref_Tv_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Tv(inline1093)
            var inline1095 Typ = TVar{
                _0: inline1094,
            }
            tv__88 = inline1095
            var t609 SubstEntry = SubstEntry{
                name: x296,
                ty: tv__88,
            }
            var new_subst__89 *_goml_vec_SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SubstEntry(subst__84, t609)
            var t610 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: tv__88,
                _1: new_subst__89,
            }
            return t610
        case Some:
            var x304 Typ = mtmp303.(Some)._0
            var t611 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: x304,
                _1: subst__84,
            }
            return t611
        default:
            panic("non-exhaustive match")
        }
    case TArrow:
        var x297 Typ = ty__85.(TArrow)._0
        var x298 Typ = ty__85.(TArrow)._1
        var mtmp305 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, subst__84, x297)
        var x306 Typ = mtmp305._0
        var x307 *_goml_vec_SubstEntry = mtmp305._1
        var mtmp308 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, x307, x298)
        var x309 Typ = mtmp308._0
        var x310 *_goml_vec_SubstEntry = mtmp308._1
        var t612 Typ = TArrow{
            _0: x306,
            _1: x309,
        }
        var t613 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
            _0: t612,
            _1: x310,
        }
        return t613
    default:
        panic("non-exhaustive match")
    }
}

func typeof(st__102 CheckerState, env__103 *_goml_vec_EnvEntry, e__104 Exp) Result__Typ__string {
    switch e__104.(type) {
    case Var:
        var x314 string = e__104.(Var)._0
        var mtmp322 Option__Typ = env_lookup(env__103, x314)
        switch mtmp322.(type) {
        case None:
            var t622 Result__Typ__string = Result__Typ__string_Err{
                _0: "unbound var",
            }
            return t622
        case Some:
            var x323 Typ = mtmp322.(Some)._0
            var t623 Typ
            var inline1099 *_goml_vec_SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SubstEntry()
            var inline1100 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__102, inline1099, x323)
            var inline1101 Typ = inline1100._0
            t623 = inline1101
            var t624 Result__Typ__string = Result__Typ__string_Ok{
                _0: t623,
            }
            return t624
        default:
            panic("non-exhaustive match")
        }
    case App:
        var x315 Exp = e__104.(App)._0
        var x316 Exp = e__104.(App)._1
        var mtmp324 Result__Typ__string = typeof(st__102, env__103, x315)
        switch mtmp324.(type) {
        case Result__Typ__string_Ok:
            var x325 Typ = mtmp324.(Result__Typ__string_Ok)._0
            var mtmp327 Result__Typ__string = typeof(st__102, env__103, x316)
            switch mtmp327.(type) {
            case Result__Typ__string_Ok:
                var x328 Typ = mtmp327.(Result__Typ__string_Ok)._0
                var ty_res__119 Typ
                var inline1104 string = gensym(st__102)
                var inline1105 *ref_int32_x = st__102.current_level
                var inline1106 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline1105)
                var inline1107 Tv = Unbound{
                    _0: inline1104,
                    _1: inline1106,
                }
                var inline1108 *ref_Tv_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Tv(inline1107)
                var inline1109 Typ = TVar{
                    _0: inline1108,
                }
                ty_res__119 = inline1109
                var arrow__120 Typ = TArrow{
                    _0: x328,
                    _1: ty_res__119,
                }
                var mtmp330 Result__unit__string = unify(st__102, x325, arrow__120)
                switch mtmp330.(type) {
                case Result__unit__string_Ok:
                    var t631 Result__Typ__string = Result__Typ__string_Ok{
                        _0: ty_res__119,
                    }
                    return t631
                case Result__unit__string_Err:
                    var x332 string = mtmp330.(Result__unit__string_Err)._0
                    var t632 Result__Typ__string = Result__Typ__string_Err{
                        _0: x332,
                    }
                    return t632
                default:
                    panic("non-exhaustive match")
                }
            case Result__Typ__string_Err:
                var x329 string = mtmp327.(Result__Typ__string_Err)._0
                var t633 Result__Typ__string = Result__Typ__string_Err{
                    _0: x329,
                }
                return t633
            default:
                panic("non-exhaustive match")
            }
        case Result__Typ__string_Err:
            var x326 string = mtmp324.(Result__Typ__string_Err)._0
            var t634 Result__Typ__string = Result__Typ__string_Err{
                _0: x326,
            }
            return t634
        default:
            panic("non-exhaustive match")
        }
    case Lam:
        var x317 string = e__104.(Lam)._0
        var x318 Exp = e__104.(Lam)._1
        var ty_x__109 Typ
        var inline1111 string = gensym(st__102)
        var inline1112 *ref_int32_x = st__102.current_level
        var inline1113 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline1112)
        var inline1114 Tv = Unbound{
            _0: inline1111,
            _1: inline1113,
        }
        var inline1115 *ref_Tv_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Tv(inline1114)
        var inline1116 Typ = TVar{
            _0: inline1115,
        }
        ty_x__109 = inline1116
        var t635 EnvEntry = EnvEntry{
            name: x317,
            ty: ty_x__109,
        }
        var env2__110 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__EnvEntry(env__103, t635)
        var mtmp333 Result__Typ__string = typeof(st__102, env2__110, x318)
        switch mtmp333.(type) {
        case Result__Typ__string_Ok:
            var x334 Typ = mtmp333.(Result__Typ__string_Ok)._0
            var t638 Typ = TArrow{
                _0: ty_x__109,
                _1: x334,
            }
            var t639 Result__Typ__string = Result__Typ__string_Ok{
                _0: t638,
            }
            return t639
        case Result__Typ__string_Err:
            var x335 string = mtmp333.(Result__Typ__string_Err)._0
            var t640 Result__Typ__string = Result__Typ__string_Err{
                _0: x335,
            }
            return t640
        default:
            panic("non-exhaustive match")
        }
    case Let:
        var x319 string = e__104.(Let)._0
        var x320 Exp = e__104.(Let)._1
        var x321 Exp = e__104.(Let)._2
        var inline1124 *ref_int32_x = st__102.current_level
        var inline1125 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline1124)
        var inline1126 *ref_int32_x = st__102.current_level
        var inline1127 int32 = inline1125 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline1126, inline1127)
        var ty_e__125 Result__Typ__string = typeof(st__102, env__103, x320)
        var inline1118 *ref_int32_x = st__102.current_level
        var inline1119 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline1118)
        var inline1120 *ref_int32_x = st__102.current_level
        var inline1121 int32 = inline1119 - 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline1120, inline1121)
        switch ty_e__125.(type) {
        case Result__Typ__string_Ok:
            var x338 Typ = ty_e__125.(Result__Typ__string_Ok)._0
            var t643 Typ = gen(st__102, x338)
            var t644 EnvEntry = EnvEntry{
                name: x319,
                ty: t643,
            }
            var env2__128 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__EnvEntry(env__103, t644)
            var t645 Result__Typ__string = typeof(st__102, env2__128, x321)
            return t645
        case Result__Typ__string_Err:
            var x339 string = ty_e__125.(Result__Typ__string_Err)._0
            var t646 Result__Typ__string = Result__Typ__string_Err{
                _0: x339,
            }
            return t646
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func exp_var(name__129 string) Exp {
    var t649 Exp = Var{
        _0: name__129,
    }
    return t649
}

func exp_lam(name__130 string, body__131 Exp) Exp {
    var t652 Exp = Lam{
        _0: name__130,
        _1: body__131,
    }
    return t652
}

func exp_app(a__132 Exp, b__133 Exp) Exp {
    var t655 Exp = App{
        _0: a__132,
        _1: b__133,
    }
    return t655
}

func exp_let(name__134 string, a__135 Exp, b__136 Exp) Exp {
    var t658 Exp = Let{
        _0: name__134,
        _1: a__135,
        _2: b__136,
    }
    return t658
}

func show_result(label__137 string, res__138 Result__Typ__string) struct{} {
    switch res__138.(type) {
    case Result__Typ__string_Ok:
        var x340 Typ = res__138.(Result__Typ__string_Ok)._0
        var t661 string = label__137 + ": "
        var t662 string = typ_to_string(x340)
        var t663 string = t661 + t662
        var inline1130 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t663)
        _goml_runtime_core_string_println(inline1130)
        return struct{}{}
    case Result__Typ__string_Err:
        var x341 string = res__138.(Result__Typ__string_Err)._0
        var t665 string = label__137 + ": "
        var t666 string = t665 + x341
        var inline1133 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t666)
        _goml_runtime_core_string_println(inline1133)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var st__141 CheckerState = state_new()
    var t669 Exp = exp_var("x")
    var id__142 Exp = exp_lam("x", t669)
    var t670 Exp = exp_var("x")
    var t671 Exp = exp_var("y")
    var t672 Exp = exp_app(t670, t671)
    var t673 Exp = exp_lam("y", t672)
    var c1__143 Exp = exp_lam("x", t673)
    reset_type_variables(st__141)
    var t674 *_goml_vec_EnvEntry = env_empty()
    var t675 Result__Typ__string = typeof(st__141, t674, id__142)
    show_result("id", t675)
    reset_type_variables(st__141)
    var t676 *_goml_vec_EnvEntry = env_empty()
    var t677 Result__Typ__string = typeof(st__141, t676, c1__143)
    show_result("c1", t677)
    reset_type_variables(st__141)
    var t678 *_goml_vec_EnvEntry = env_empty()
    var t679 Exp = exp_var("x")
    var t680 Exp = exp_let("x", c1__143, t679)
    var t681 Result__Typ__string = typeof(st__141, t678, t680)
    show_result("let_x_c1_x", t681)
    reset_type_variables(st__141)
    var t682 *_goml_vec_EnvEntry = env_empty()
    var t683 Exp = exp_var("z")
    var t684 Exp = exp_lam("z", t683)
    var t685 Exp = exp_var("y")
    var t686 Exp = exp_let("y", t684, t685)
    var t687 Result__Typ__string = typeof(st__141, t682, t686)
    show_result("let_y_id_y", t687)
    reset_type_variables(st__141)
    var t688 *_goml_vec_EnvEntry = env_empty()
    var t689 Exp = exp_var("z")
    var t690 Exp = exp_lam("z", t689)
    var t691 Exp = exp_var("y")
    var t692 Exp = exp_let("y", t690, t691)
    var t693 Exp = exp_lam("x", t692)
    var t694 Result__Typ__string = typeof(st__141, t688, t693)
    show_result("lam_x_let_y_id_y", t694)
    reset_type_variables(st__141)
    var t695 *_goml_vec_EnvEntry = env_empty()
    var t696 Exp = exp_var("z")
    var t697 Exp = exp_lam("z", t696)
    var t698 Exp = exp_var("y")
    var t699 Exp = exp_var("x")
    var t700 Exp = exp_app(t698, t699)
    var t701 Exp = exp_let("y", t697, t700)
    var t702 Exp = exp_lam("x", t701)
    var t703 Result__Typ__string = typeof(st__141, t695, t702)
    show_result("lam_x_let_y_id_yx", t703)
    reset_type_variables(st__141)
    var t704 *_goml_vec_EnvEntry = env_empty()
    var t705 Exp = exp_var("x")
    var t706 Exp = exp_var("x")
    var t707 Exp = exp_app(t705, t706)
    var t708 Exp = exp_lam("x", t707)
    var t709 Result__Typ__string = typeof(st__141, t704, t708)
    show_result("self_apply", t709)
    reset_type_variables(st__141)
    var t710 *_goml_vec_EnvEntry = env_empty()
    var t711 Exp = exp_var("x")
    var t712 Exp = exp_var("x")
    var t713 Exp = exp_let("x", t711, t712)
    var t714 Result__Typ__string = typeof(st__141, t710, t713)
    show_result("unbound_var", t714)
    reset_type_variables(st__141)
    var t715 *_goml_vec_EnvEntry = env_empty()
    var t716 Exp = exp_var("y")
    var t717 Exp = exp_var("y")
    var t718 Exp = exp_var("z")
    var t719 Exp = exp_app(t717, t718)
    var t720 Exp = exp_lam("z", t719)
    var t721 Exp = exp_app(t716, t720)
    var t722 Exp = exp_lam("y", t721)
    var t723 Result__Typ__string = typeof(st__141, t715, t722)
    show_result("max_heiber", t723)
    reset_type_variables(st__141)
    var t724 *_goml_vec_EnvEntry = env_empty()
    var t725 Exp = exp_var("k")
    var t726 Exp = exp_var("k")
    var t727 Exp = exp_var("x")
    var t728 Exp = exp_app(t726, t727)
    var t729 Exp = exp_var("y")
    var t730 Exp = exp_app(t728, t729)
    var t731 Exp = exp_app(t725, t730)
    var t732 Exp = exp_var("k")
    var t733 Exp = exp_var("y")
    var t734 Exp = exp_app(t732, t733)
    var t735 Exp = exp_var("x")
    var t736 Exp = exp_app(t734, t735)
    var t737 Exp = exp_app(t731, t736)
    var t738 Exp = exp_lam("k", t737)
    var t739 Exp = exp_lam("y", t738)
    var t740 Exp = exp_lam("x", t739)
    var t741 Result__Typ__string = typeof(st__141, t724, t740)
    show_result("kirang", t741)
    reset_type_variables(st__141)
    var t742 *_goml_vec_EnvEntry = env_empty()
    var t743 Exp = exp_var("id")
    var t744 Exp = exp_var("id")
    var t745 Exp = exp_app(t743, t744)
    var t746 Exp = exp_let("id", id__142, t745)
    var t747 Result__Typ__string = typeof(st__141, t742, t746)
    show_result("let_id_idid", t747)
    reset_type_variables(st__141)
    var t748 *_goml_vec_EnvEntry = env_empty()
    var t749 Exp = exp_var("x")
    var t750 Exp = exp_app(t749, id__142)
    var t751 Exp = exp_var("z")
    var t752 Exp = exp_let("z", t750, t751)
    var t753 Exp = exp_var("y")
    var t754 Exp = exp_let("y", t752, t753)
    var t755 Exp = exp_let("x", c1__143, t754)
    var t756 Result__Typ__string = typeof(st__141, t748, t755)
    show_result("nested_lets", t756)
    reset_type_variables(st__141)
    var t757 *_goml_vec_EnvEntry = env_empty()
    var t758 Exp = exp_var("x")
    var t759 Exp = exp_var("y")
    var t760 Exp = exp_app(t758, t759)
    var t761 Exp = exp_var("y")
    var t762 Exp = exp_var("x")
    var t763 Exp = exp_app(t761, t762)
    var t764 Exp = exp_lam("x", t763)
    var t765 Exp = exp_let("x", t760, t764)
    var t766 Exp = exp_lam("y", t765)
    var t767 Exp = exp_lam("x", t766)
    var t768 Result__Typ__string = typeof(st__141, t757, t767)
    show_result("fun_x_fun_y_let_x_xy_fun_x_yx", t768)
    reset_type_variables(st__141)
    var t769 *_goml_vec_EnvEntry = env_empty()
    var t770 Exp = exp_var("x")
    var t771 Exp = exp_var("y")
    var t772 Exp = exp_let("y", t770, t771)
    var t773 Exp = exp_lam("x", t772)
    var t774 Result__Typ__string = typeof(st__141, t769, t773)
    show_result("sound_gen_1", t774)
    reset_type_variables(st__141)
    var t775 *_goml_vec_EnvEntry = env_empty()
    var t776 Exp = exp_var("x")
    var t777 Exp = exp_lam("z", t776)
    var t778 Exp = exp_var("y")
    var t779 Exp = exp_let("y", t777, t778)
    var t780 Exp = exp_lam("x", t779)
    var t781 Result__Typ__string = typeof(st__141, t775, t780)
    show_result("sound_gen_2", t781)
    reset_type_variables(st__141)
    var t782 *_goml_vec_EnvEntry = env_empty()
    var t783 Exp = exp_var("x")
    var t784 Exp = exp_var("z")
    var t785 Exp = exp_app(t783, t784)
    var t786 Exp = exp_lam("z", t785)
    var t787 Exp = exp_var("y")
    var t788 Exp = exp_let("y", t786, t787)
    var t789 Exp = exp_lam("x", t788)
    var t790 Result__Typ__string = typeof(st__141, t782, t789)
    show_result("sound_gen_3", t790)
    reset_type_variables(st__141)
    var t791 *_goml_vec_EnvEntry = env_empty()
    var t792 Exp = exp_var("x")
    var t793 Exp = exp_var("y")
    var t794 Exp = exp_app(t792, t793)
    var t795 Exp = exp_var("x")
    var t796 Exp = exp_var("y")
    var t797 Exp = exp_app(t795, t796)
    var t798 Exp = exp_let("x", t794, t797)
    var t799 Exp = exp_lam("y", t798)
    var t800 Exp = exp_lam("x", t799)
    var t801 Result__Typ__string = typeof(st__141, t791, t800)
    show_result("double_apply", t801)
    reset_type_variables(st__141)
    var t802 *_goml_vec_EnvEntry = env_empty()
    var t803 Exp = exp_var("x")
    var t804 Exp = exp_var("y")
    var t805 Exp = exp_var("y")
    var t806 Exp
    var inline1192 Exp = App{
        _0: t804,
        _1: t805,
    }
    t806 = inline1192
    var t807 Exp
    var inline1189 string = "y"
    var inline1190 Exp = Let{
        _0: inline1189,
        _1: t803,
        _2: t806,
    }
    t807 = inline1190
    var t808 Exp
    var inline1186 string = "x"
    var inline1187 Exp = Lam{
        _0: inline1186,
        _1: t807,
    }
    t808 = inline1187
    var t809 Result__Typ__string = typeof(st__141, t802, t808)
    show_result("sound_gen_occurs", t809)
    var inline1183 *ref_int32_x = st__141.gensym_counter
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline1183, 0)
    var t810 *_goml_vec_EnvEntry
    var inline1181 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__EnvEntry()
    t810 = inline1181
    var t811 Exp
    var inline1178 string = "x"
    var inline1179 Exp = Var{
        _0: inline1178,
    }
    t811 = inline1179
    var t812 Exp
    var inline1176 Exp = App{
        _0: t811,
        _1: id__142,
    }
    t812 = inline1176
    var t813 Exp
    var inline1173 string = "z"
    var inline1174 Exp = Var{
        _0: inline1173,
    }
    t813 = inline1174
    var t814 Exp
    var inline1170 string = "z"
    var inline1171 Exp = Let{
        _0: inline1170,
        _1: t812,
        _2: t813,
    }
    t814 = inline1171
    var t815 Exp
    var inline1167 string = "y"
    var inline1168 Exp = Var{
        _0: inline1167,
    }
    t815 = inline1168
    var t816 Exp
    var inline1164 string = "y"
    var inline1165 Exp = Let{
        _0: inline1164,
        _1: t814,
        _2: t815,
    }
    t816 = inline1165
    var t817 Exp
    var inline1161 string = "x"
    var inline1162 Exp = Lam{
        _0: inline1161,
        _1: t816,
    }
    t817 = inline1162
    var t818 Result__Typ__string = typeof(st__141, t810, t817)
    var inline1148 string = "fun_x_let_y_let_z_x_id_z_y"
    switch t818.(type) {
    case Result__Typ__string_Ok:
        var inline1149 Typ = t818.(Result__Typ__string_Ok)._0
        var inline1151 string = inline1148 + ": "
        var inline1152 string = typ_to_string(inline1149)
        var inline1153 string = inline1151 + inline1152
        println__T_string(inline1153)
    case Result__Typ__string_Err:
        var inline1155 string = t818.(Result__Typ__string_Err)._0
        var inline1157 string = inline1148 + ": "
        var inline1158 string = inline1157 + inline1155
        println__T_string(inline1158)
    default:
        panic("non-exhaustive match")
    }
    var inline1144 string = ""
    var inline1145 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline1144)
    _goml_runtime_core_string_println(inline1145)
    var inline1140 string = "All Done"
    var inline1141 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline1140)
    _goml_runtime_core_string_println(inline1141)
    var inline1136 string = ""
    var inline1137 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline1136)
    _goml_runtime_core_string_println(inline1137)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__275 *ref_int32_x, value__276 int32) struct{} {
    ref_set__Ref_5int32(self__275, value__276)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__274 *ref_int32_x) int32 {
    var t826 int32 = ref_get__Ref_5int32(self__274)
    return t826
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Tv(value__273 Tv) *ref_Tv_x {
    var t835 *ref_Tv_x = ref__Ref_2Tv(value__273)
    return t835
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__EnvEntry() *_goml_vec_EnvEntry {
    var t841 *_goml_vec_EnvEntry = vec_new__Vec_8EnvEntry()
    return t841
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SubstEntry(self__176 *_goml_vec_SubstEntry, elem__177 SubstEntry) *_goml_vec_SubstEntry {
    var t876 int
    var inline1207 int = vec_len__Vec_10SubstEntry(self__176)
    t876 = inline1207
    var t877 int = t876 + 1
    var result__178 *_goml_vec_SubstEntry
    var inline1205 *_goml_vec_SubstEntry = vec_with_capacity__Vec_10SubstEntry(t877)
    result__178 = inline1205
    var index__179 int = 0
    Loop_loop879:
    for {
        var t880 int
        var inline1201 int = vec_len__Vec_10SubstEntry(self__176)
        t880 = inline1201
        var t881 bool = index__179 < t880
        if t881 {
            var t882 SubstEntry = vec_get__Vec_10SubstEntry(self__176, index__179)
            vec_push__Vec_10SubstEntry(result__178, t882)
            var compound_old80 int = index__179
            var compound_value81 int = 1
            var t883 int = compound_old80 + compound_value81
            index__179 = t883
            continue
        } else {
            break Loop_loop879
        }
    }
    vec_push__Vec_10SubstEntry(result__178, elem__177)
    return result__178
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SubstEntry() *_goml_vec_SubstEntry {
    var t887 *_goml_vec_SubstEntry = vec_new__Vec_10SubstEntry()
    return t887
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__EnvEntry(self__176 *_goml_vec_EnvEntry, elem__177 EnvEntry) *_goml_vec_EnvEntry {
    var t890 int
    var inline1217 int = vec_len__Vec_8EnvEntry(self__176)
    t890 = inline1217
    var t891 int = t890 + 1
    var result__178 *_goml_vec_EnvEntry
    var inline1215 *_goml_vec_EnvEntry = vec_with_capacity__Vec_8EnvEntry(t891)
    result__178 = inline1215
    var index__179 int = 0
    Loop_loop893:
    for {
        var t894 int
        var inline1211 int = vec_len__Vec_8EnvEntry(self__176)
        t894 = inline1211
        var t895 bool = index__179 < t894
        if t895 {
            var t896 EnvEntry = vec_get__Vec_8EnvEntry(self__176, index__179)
            vec_push__Vec_8EnvEntry(result__178, t896)
            var compound_old80 int = index__179
            var compound_value81 int = 1
            var t897 int = compound_old80 + compound_value81
            index__179 = t897
            continue
        } else {
            break Loop_loop893
        }
    }
    vec_push__Vec_8EnvEntry(result__178, elem__177)
    return result__178
}

func println__T_string(value__1 string) struct{} {
    var t900 string
    t900 = value__1
    _goml_runtime_core_string_println(t900)
    return struct{}{}
}

func char_to_string(value__29 rune) string {
    var t906 uint32 = uint32(rune(value__29))
    var t907 bool
    var inline1220 bool = t906 <= 1114111
    if inline1220 {
        var inline1221 bool = t906 >= 55296
        var inline1223 bool
        if inline1221 {
            var inline1225 bool = t906 <= 57343
            inline1223 = inline1225
        } else {
            inline1223 = false
        }
        var inline1224 bool = !inline1223
        t907 = inline1224
    } else {
        t907 = false
    }
    if t907 {
        var t908 string = _goml_runtime_core_char_to_string(value__29)
        return t908
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
