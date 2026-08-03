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
    var t375 *ref_int32_x
    var inline947 int32 = 0
    var inline948 *ref_int32_x = ref__Ref_5int32(inline947)
    t375 = inline948
    var t376 *ref_int32_x
    var inline944 int32 = 1
    var inline945 *ref_int32_x = ref__Ref_5int32(inline944)
    t376 = inline945
    var t377 CheckerState = CheckerState{
        gensym_counter: t375,
        current_level: t376,
    }
    return t377
}

func reset_type_variables(st__2 CheckerState) struct{} {
    var inline959 *ref_int32_x = st__2.gensym_counter
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline959, 0)
    var inline956 *ref_int32_x = st__2.current_level
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline956, 1)
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
    var t403 *ref_int32_x = st__10.gensym_counter
    var n__11 int32
    var inline976 int32 = ref_get__Ref_5int32(t403)
    n__11 = inline976
    var t404 *ref_int32_x = st__10.gensym_counter
    var t405 int32 = n__11 + 1
    ref_set__Ref_5int32(t404, t405)
    var t408 bool = n__11 < 26
    if t408 {
        var t409 rune = nth_letter(n__11)
        var inline970 string = char_to_string(t409)
        return inline970
    } else {
        var t411 string
        var inline972 string = _goml_runtime_core_int32_to_string(n__11)
        t411 = inline972
        var t412 string = "t" + t411
        return t412
    }
}

func typ_is_arrow(ty__15 Typ) bool {
    switch ty__15.(type) {
    case TVar:
        var x183 *ref_Tv_x = ty__15.(TVar)._0
        var mtmp187 Tv
        var inline993 Tv = ref_get__Ref_2Tv(x183)
        mtmp187 = inline993
        switch mtmp187.(type) {
        case Link:
            var x190 Typ = mtmp187.(Link)._0
            var t425 bool = typ_is_arrow(x190)
            return t425
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
        var x191 *ref_Tv_x = ty__18.(TVar)._0
        var mtmp195 Tv
        var inline995 Tv = ref_get__Ref_2Tv(x191)
        mtmp195 = inline995
        switch mtmp195.(type) {
        case Unbound:
            var x196 string = mtmp195.(Unbound)._0
            var t432 string = "'" + x196
            return t432
        case Link:
            var x198 Typ = mtmp195.(Link)._0
            var t433 string = typ_to_string(x198)
            return t433
        default:
            panic("non-exhaustive match")
        }
    case QVar:
        var x192 string = ty__18.(QVar)._0
        var t434 string = "'" + x192
        return t434
    case TArrow:
        var x193 Typ = ty__18.(TArrow)._0
        var x194 Typ = ty__18.(TArrow)._1
        var t439 bool = typ_is_arrow(x193)
        var jp436 string
        if t439 {
            var t440 string = typ_to_string(x193)
            var t441 string = "(" + t440
            var t442 string = t441 + ")"
            jp436 = t442
        } else {
            var t443 string = typ_to_string(x193)
            jp436 = t443
        }
        var s2__26 string = typ_to_string(x194)
        var t437 string = jp436 + " -> "
        var t438 string = t437 + s2__26
        return t438
    default:
        panic("non-exhaustive match")
    }
}

func env_empty() *_goml_vec_EnvEntry {
    var inline997 *_goml_vec_EnvEntry = vec_new__Vec_8EnvEntry()
    return inline997
}

func env_lookup(env__28 *_goml_vec_EnvEntry, name__29 string) Option__Typ {
    var t448 int
    var inline1025 int = vec_len__Vec_8EnvEntry(env__28)
    t448 = inline1025
    var t449 int = t448 - 1
    var i__30 *ref_int_x
    var inline1023 *ref_int_x = ref__Ref_3int(t449)
    i__30 = inline1023
    var found__31 *ref_Option__Typ_x
    var inline1021 *ref_Option__Typ_x = ref__Ref_11Option__Typ(None{})
    found__31 = inline1021
    var done__32 *ref_bool_x
    var inline1018 bool = false
    var inline1019 *ref_bool_x = ref__Ref_4bool(inline1018)
    done__32 = inline1019
    Loop_loop452:
    for {
        var t465 bool
        var inline1014 bool = ref_get__Ref_4bool(done__32)
        t465 = inline1014
        var t466 bool = !t465
        var jp454 bool
        if t466 {
            var t467 int
            var inline999 int = ref_get__Ref_3int(i__30)
            t467 = inline999
            var t468 bool = t467 >= 0
            jp454 = t468
        } else {
            jp454 = false
        }
        if jp454 {
            var t455 int
            var inline1012 int = ref_get__Ref_3int(i__30)
            t455 = inline1012
            var entry__33 EnvEntry = vec_get__Vec_8EnvEntry(env__28, t455)
            var t457 string = entry__33.name
            var t458 bool
            var inline1010 bool = t457 == name__29
            t458 = inline1010
            if t458 {
                var t459 Typ = entry__33.ty
                var t460 Option__Typ = Some{
                    _0: t459,
                }
                ref_set__Ref_11Option__Typ(found__31, t460)
                var inline1001 bool = true
                ref_set__Ref_4bool(done__32, inline1001)
                continue
            } else {
                var t462 int
                var inline1008 int = ref_get__Ref_3int(i__30)
                t462 = inline1008
                var t463 int = t462 - 1
                ref_set__Ref_3int(i__30, t463)
                continue
            }
        } else {
            break Loop_loop452
        }
    }
    var inline1016 Option__Typ = ref_get__Ref_11Option__Typ(found__31)
    return inline1016
}

func subst_lookup(subst__34 *_goml_vec_SubstEntry, name__35 string) Option__Typ {
    var t471 int
    var inline1053 int = vec_len__Vec_10SubstEntry(subst__34)
    t471 = inline1053
    var t472 int = t471 - 1
    var i__36 *ref_int_x
    var inline1051 *ref_int_x = ref__Ref_3int(t472)
    i__36 = inline1051
    var found__37 *ref_Option__Typ_x
    var inline1049 *ref_Option__Typ_x = ref__Ref_11Option__Typ(None{})
    found__37 = inline1049
    var done__38 *ref_bool_x
    var inline1046 bool = false
    var inline1047 *ref_bool_x = ref__Ref_4bool(inline1046)
    done__38 = inline1047
    Loop_loop475:
    for {
        var t488 bool
        var inline1042 bool = ref_get__Ref_4bool(done__38)
        t488 = inline1042
        var t489 bool = !t488
        var jp477 bool
        if t489 {
            var t490 int
            var inline1027 int = ref_get__Ref_3int(i__36)
            t490 = inline1027
            var t491 bool = t490 >= 0
            jp477 = t491
        } else {
            jp477 = false
        }
        if jp477 {
            var t478 int
            var inline1040 int = ref_get__Ref_3int(i__36)
            t478 = inline1040
            var entry__39 SubstEntry = vec_get__Vec_10SubstEntry(subst__34, t478)
            var t480 string = entry__39.name
            var t481 bool
            var inline1038 bool = t480 == name__35
            t481 = inline1038
            if t481 {
                var t482 Typ = entry__39.ty
                var t483 Option__Typ = Some{
                    _0: t482,
                }
                ref_set__Ref_11Option__Typ(found__37, t483)
                var inline1029 bool = true
                ref_set__Ref_4bool(done__38, inline1029)
                continue
            } else {
                var t485 int
                var inline1036 int = ref_get__Ref_3int(i__36)
                t485 = inline1036
                var t486 int = t485 - 1
                ref_set__Ref_3int(i__36, t486)
                continue
            }
        } else {
            break Loop_loop475
        }
    }
    var inline1044 Option__Typ = ref_get__Ref_11Option__Typ(found__37)
    return inline1044
}

func occurs(st__40 CheckerState, tvr__41 *ref_Tv_x, ty__42 Typ) Result__unit__string {
    switch ty__42.(type) {
    case TVar:
        var x203 *ref_Tv_x = ty__42.(TVar)._0
        var t498 bool = ptr_eq__Ref_2Tv(tvr__41, x203)
        if t498 {
            var t499 Result__unit__string = Result__unit__string_Err{
                _0: "occurs check",
            }
            return t499
        } else {
            var mtmp207 Tv
            var inline1061 Tv = ref_get__Ref_2Tv(x203)
            mtmp207 = inline1061
            switch mtmp207.(type) {
            case Unbound:
                var x208 string = mtmp207.(Unbound)._0
                var x209 int32 = mtmp207.(Unbound)._1
                var mtmp211 Tv
                var inline1059 Tv = ref_get__Ref_2Tv(tvr__41)
                mtmp211 = inline1059
                var jp503 int32
                switch mtmp211.(type) {
                case Unbound:
                    var x213 int32 = mtmp211.(Unbound)._1
                    var inline1055 bool = x213 < x209
                    if inline1055 {
                        jp503 = x213
                    } else {
                        jp503 = x209
                    }
                default:
                    jp503 = x209
                }
                var t504 Tv = Unbound{
                    _0: x208,
                    _1: jp503,
                }
                ref_set__Ref_2Tv(x203, t504)
                var t505 Result__unit__string = Result__unit__string_Ok{
                    _0: struct{}{},
                }
                return t505
            case Link:
                var x210 Typ = mtmp207.(Link)._0
                var t507 Result__unit__string = occurs(st__40, tvr__41, x210)
                return t507
            default:
                panic("non-exhaustive match")
            }
        }
    case TArrow:
        var x205 Typ = ty__42.(TArrow)._0
        var x206 Typ = ty__42.(TArrow)._1
        var mtmp216 Result__unit__string = occurs(st__40, tvr__41, x205)
        switch mtmp216.(type) {
        case Result__unit__string_Ok:
            var t510 Result__unit__string = occurs(st__40, tvr__41, x206)
            return t510
        case Result__unit__string_Err:
            var x218 string = mtmp216.(Result__unit__string_Err)._0
            var t511 Result__unit__string = Result__unit__string_Err{
                _0: x218,
            }
            return t511
        default:
            panic("non-exhaustive match")
        }
    default:
        var t512 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        return t512
    }
}

func unify(st__52 CheckerState, t1__53 Typ, t2__54 Typ) Result__unit__string {
    switch t2__54.(type) {
    case TVar:
        var x222 *ref_Tv_x = t2__54.(TVar)._0
        switch t1__53.(type) {
        case TVar:
            var x226 *ref_Tv_x = t1__53.(TVar)._0
            var t521 bool = ptr_eq__Ref_2Tv(x226, x222)
            if t521 {
                var t522 Result__unit__string = Result__unit__string_Ok{
                    _0: struct{}{},
                }
                return t522
            } else {
                var mtmp230 Tv
                var inline1067 Tv = ref_get__Ref_2Tv(x226)
                mtmp230 = inline1067
                switch mtmp230.(type) {
                case Unbound:
                    var mtmp234 Tv
                    var inline1065 Tv = ref_get__Ref_2Tv(x222)
                    mtmp234 = inline1065
                    switch mtmp234.(type) {
                    case Unbound:
                        var t527 Typ = TVar{
                            _0: x222,
                        }
                        var mtmp238 Result__unit__string = occurs(st__52, x226, t527)
                        switch mtmp238.(type) {
                        case Result__unit__string_Ok:
                            var t530 Typ = TVar{
                                _0: x222,
                            }
                            var t531 Tv = Link{
                                _0: t530,
                            }
                            ref_set__Ref_2Tv(x226, t531)
                            var t532 Result__unit__string = Result__unit__string_Ok{
                                _0: struct{}{},
                            }
                            return t532
                        case Result__unit__string_Err:
                            var x240 string = mtmp238.(Result__unit__string_Err)._0
                            var t533 Result__unit__string = Result__unit__string_Err{
                                _0: x240,
                            }
                            return t533
                        default:
                            panic("non-exhaustive match")
                        }
                    case Link:
                        var x237 Typ = mtmp234.(Link)._0
                        var t534 Typ = TVar{
                            _0: x226,
                        }
                        var t535 Result__unit__string = unify(st__52, t534, x237)
                        return t535
                    default:
                        panic("non-exhaustive match")
                    }
                case Link:
                    var x233 Typ = mtmp230.(Link)._0
                    var t536 Typ = TVar{
                        _0: x222,
                    }
                    var t537 Result__unit__string = unify(st__52, x233, t536)
                    return t537
                default:
                    panic("non-exhaustive match")
                }
            }
        default:
            var mtmp242 Tv
            var inline1071 Tv = ref_get__Ref_2Tv(x222)
            mtmp242 = inline1071
            switch mtmp242.(type) {
            case Unbound:
                var mtmp246 Result__unit__string = occurs(st__52, x222, t1__53)
                switch mtmp246.(type) {
                case Result__unit__string_Ok:
                    var t542 Tv = Link{
                        _0: t1__53,
                    }
                    ref_set__Ref_2Tv(x222, t542)
                    var t543 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    return t543
                case Result__unit__string_Err:
                    var x248 string = mtmp246.(Result__unit__string_Err)._0
                    var t544 Result__unit__string = Result__unit__string_Err{
                        _0: x248,
                    }
                    return t544
                default:
                    panic("non-exhaustive match")
                }
            case Link:
                var x245 Typ = mtmp242.(Link)._0
                var t545 Result__unit__string = unify(st__52, t1__53, x245)
                return t545
            default:
                panic("non-exhaustive match")
            }
        }
    case TArrow:
        var x224 Typ = t2__54.(TArrow)._0
        var x225 Typ = t2__54.(TArrow)._1
        switch t1__53.(type) {
        case TVar:
            var x250 *ref_Tv_x = t1__53.(TVar)._0
            var mtmp254 Tv
            var inline1075 Tv = ref_get__Ref_2Tv(x250)
            mtmp254 = inline1075
            switch mtmp254.(type) {
            case Unbound:
                var mtmp258 Result__unit__string = occurs(st__52, x250, t2__54)
                switch mtmp258.(type) {
                case Result__unit__string_Ok:
                    var t552 Tv = Link{
                        _0: t2__54,
                    }
                    ref_set__Ref_2Tv(x250, t552)
                    var t553 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    return t553
                case Result__unit__string_Err:
                    var x260 string = mtmp258.(Result__unit__string_Err)._0
                    var t554 Result__unit__string = Result__unit__string_Err{
                        _0: x260,
                    }
                    return t554
                default:
                    panic("non-exhaustive match")
                }
            case Link:
                var x257 Typ = mtmp254.(Link)._0
                var t555 Result__unit__string = unify(st__52, x257, t2__54)
                return t555
            default:
                panic("non-exhaustive match")
            }
        case TArrow:
            var x252 Typ = t1__53.(TArrow)._0
            var x253 Typ = t1__53.(TArrow)._1
            var mtmp262 Result__unit__string = unify(st__52, x252, x224)
            switch mtmp262.(type) {
            case Result__unit__string_Ok:
                var t558 Result__unit__string = unify(st__52, x253, x225)
                return t558
            case Result__unit__string_Err:
                var x264 string = mtmp262.(Result__unit__string_Err)._0
                var t559 Result__unit__string = Result__unit__string_Err{
                    _0: x264,
                }
                return t559
            default:
                panic("non-exhaustive match")
            }
        default:
            var t560 Result__unit__string = Result__unit__string_Err{
                _0: "unify error",
            }
            return t560
        }
    default:
        switch t1__53.(type) {
        case TVar:
            var x265 *ref_Tv_x = t1__53.(TVar)._0
            var mtmp269 Tv
            var inline1079 Tv = ref_get__Ref_2Tv(x265)
            mtmp269 = inline1079
            switch mtmp269.(type) {
            case Unbound:
                var mtmp273 Result__unit__string = occurs(st__52, x265, t2__54)
                switch mtmp273.(type) {
                case Result__unit__string_Ok:
                    var t567 Tv = Link{
                        _0: t2__54,
                    }
                    ref_set__Ref_2Tv(x265, t567)
                    var t568 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    return t568
                case Result__unit__string_Err:
                    var x275 string = mtmp273.(Result__unit__string_Err)._0
                    var t569 Result__unit__string = Result__unit__string_Err{
                        _0: x275,
                    }
                    return t569
                default:
                    panic("non-exhaustive match")
                }
            case Link:
                var x272 Typ = mtmp269.(Link)._0
                var t570 Result__unit__string = unify(st__52, x272, t2__54)
                return t570
            default:
                panic("non-exhaustive match")
            }
        default:
            var t571 Result__unit__string = Result__unit__string_Err{
                _0: "unify error",
            }
            return t571
        }
    }
}

func gen(st__73 CheckerState, ty__74 Typ) Typ {
    switch ty__74.(type) {
    case TVar:
        var x277 *ref_Tv_x = ty__74.(TVar)._0
        var mtmp281 Tv
        var inline1083 Tv = ref_get__Ref_2Tv(x277)
        mtmp281 = inline1083
        switch mtmp281.(type) {
        case Unbound:
            var x282 string = mtmp281.(Unbound)._0
            var x283 int32 = mtmp281.(Unbound)._1
            var t578 *ref_int32_x = st__73.current_level
            var cur__78 int32
            var inline1081 int32 = ref_get__Ref_5int32(t578)
            cur__78 = inline1081
            var t581 bool = x283 > cur__78
            if t581 {
                var t582 Typ = QVar{
                    _0: x282,
                }
                return t582
            } else {
                var t583 Typ = TVar{
                    _0: x277,
                }
                return t583
            }
        case Link:
            var x284 Typ = mtmp281.(Link)._0
            var t584 Typ = gen(st__73, x284)
            return t584
        default:
            panic("non-exhaustive match")
        }
    case TArrow:
        var x279 Typ = ty__74.(TArrow)._0
        var x280 Typ = ty__74.(TArrow)._1
        var t585 Typ = gen(st__73, x279)
        var t586 Typ = gen(st__73, x280)
        var t587 Typ = TArrow{
            _0: t585,
            _1: t586,
        }
        return t587
    default:
        return ty__74
    }
}

func inst_loop(st__83 CheckerState, subst__84 *_goml_vec_SubstEntry, ty__85 Typ) Tuple2_3Typ_16Vec_10SubstEntry {
    switch ty__85.(type) {
    case TVar:
        var x285 *ref_Tv_x = ty__85.(TVar)._0
        var mtmp289 Tv
        var inline1085 Tv = ref_get__Ref_2Tv(x285)
        mtmp289 = inline1085
        switch mtmp289.(type) {
        case Link:
            var x292 Typ = mtmp289.(Link)._0
            var t594 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, subst__84, x292)
            return t594
        default:
            var t595 Typ = TVar{
                _0: x285,
            }
            var t596 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: t595,
                _1: subst__84,
            }
            return t596
        }
    case QVar:
        var x286 string = ty__85.(QVar)._0
        var mtmp293 Option__Typ = subst_lookup(subst__84, x286)
        switch mtmp293.(type) {
        case None:
            var tv__88 Typ
            var inline1087 string = gensym(st__83)
            var inline1088 *ref_int32_x = st__83.current_level
            var inline1089 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline1088)
            var inline1090 Tv = Unbound{
                _0: inline1087,
                _1: inline1089,
            }
            var inline1091 *ref_Tv_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Tv(inline1090)
            var inline1092 Typ = TVar{
                _0: inline1091,
            }
            tv__88 = inline1092
            var t599 SubstEntry = SubstEntry{
                name: x286,
                ty: tv__88,
            }
            var new_subst__89 *_goml_vec_SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SubstEntry(subst__84, t599)
            var t600 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: tv__88,
                _1: new_subst__89,
            }
            return t600
        case Some:
            var x294 Typ = mtmp293.(Some)._0
            var t601 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: x294,
                _1: subst__84,
            }
            return t601
        default:
            panic("non-exhaustive match")
        }
    case TArrow:
        var x287 Typ = ty__85.(TArrow)._0
        var x288 Typ = ty__85.(TArrow)._1
        var mtmp295 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, subst__84, x287)
        var x296 Typ = mtmp295._0
        var x297 *_goml_vec_SubstEntry = mtmp295._1
        var mtmp298 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, x297, x288)
        var x299 Typ = mtmp298._0
        var x300 *_goml_vec_SubstEntry = mtmp298._1
        var t602 Typ = TArrow{
            _0: x296,
            _1: x299,
        }
        var t603 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
            _0: t602,
            _1: x300,
        }
        return t603
    default:
        panic("non-exhaustive match")
    }
}

func typeof(st__102 CheckerState, env__103 *_goml_vec_EnvEntry, e__104 Exp) Result__Typ__string {
    switch e__104.(type) {
    case Var:
        var x304 string = e__104.(Var)._0
        var mtmp312 Option__Typ = env_lookup(env__103, x304)
        switch mtmp312.(type) {
        case None:
            var t612 Result__Typ__string = Result__Typ__string_Err{
                _0: "unbound var",
            }
            return t612
        case Some:
            var x313 Typ = mtmp312.(Some)._0
            var t613 Typ
            var inline1096 *_goml_vec_SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SubstEntry()
            var inline1097 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__102, inline1096, x313)
            var inline1098 Typ = inline1097._0
            t613 = inline1098
            var t614 Result__Typ__string = Result__Typ__string_Ok{
                _0: t613,
            }
            return t614
        default:
            panic("non-exhaustive match")
        }
    case App:
        var x305 Exp = e__104.(App)._0
        var x306 Exp = e__104.(App)._1
        var mtmp314 Result__Typ__string = typeof(st__102, env__103, x305)
        switch mtmp314.(type) {
        case Result__Typ__string_Ok:
            var x315 Typ = mtmp314.(Result__Typ__string_Ok)._0
            var mtmp317 Result__Typ__string = typeof(st__102, env__103, x306)
            switch mtmp317.(type) {
            case Result__Typ__string_Ok:
                var x318 Typ = mtmp317.(Result__Typ__string_Ok)._0
                var ty_res__119 Typ
                var inline1102 string = gensym(st__102)
                var inline1103 *ref_int32_x = st__102.current_level
                var inline1104 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline1103)
                var inline1105 Tv = Unbound{
                    _0: inline1102,
                    _1: inline1104,
                }
                var inline1106 *ref_Tv_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Tv(inline1105)
                var inline1107 Typ = TVar{
                    _0: inline1106,
                }
                ty_res__119 = inline1107
                var arrow__120 Typ = TArrow{
                    _0: x318,
                    _1: ty_res__119,
                }
                var mtmp320 Result__unit__string = unify(st__102, x315, arrow__120)
                switch mtmp320.(type) {
                case Result__unit__string_Ok:
                    var t621 Result__Typ__string = Result__Typ__string_Ok{
                        _0: ty_res__119,
                    }
                    return t621
                case Result__unit__string_Err:
                    var x322 string = mtmp320.(Result__unit__string_Err)._0
                    var t622 Result__Typ__string = Result__Typ__string_Err{
                        _0: x322,
                    }
                    return t622
                default:
                    panic("non-exhaustive match")
                }
            case Result__Typ__string_Err:
                var x319 string = mtmp317.(Result__Typ__string_Err)._0
                var t623 Result__Typ__string = Result__Typ__string_Err{
                    _0: x319,
                }
                return t623
            default:
                panic("non-exhaustive match")
            }
        case Result__Typ__string_Err:
            var x316 string = mtmp314.(Result__Typ__string_Err)._0
            var t624 Result__Typ__string = Result__Typ__string_Err{
                _0: x316,
            }
            return t624
        default:
            panic("non-exhaustive match")
        }
    case Lam:
        var x307 string = e__104.(Lam)._0
        var x308 Exp = e__104.(Lam)._1
        var ty_x__109 Typ
        var inline1109 string = gensym(st__102)
        var inline1110 *ref_int32_x = st__102.current_level
        var inline1111 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline1110)
        var inline1112 Tv = Unbound{
            _0: inline1109,
            _1: inline1111,
        }
        var inline1113 *ref_Tv_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Tv(inline1112)
        var inline1114 Typ = TVar{
            _0: inline1113,
        }
        ty_x__109 = inline1114
        var t625 EnvEntry = EnvEntry{
            name: x307,
            ty: ty_x__109,
        }
        var env2__110 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__EnvEntry(env__103, t625)
        var mtmp323 Result__Typ__string = typeof(st__102, env2__110, x308)
        switch mtmp323.(type) {
        case Result__Typ__string_Ok:
            var x324 Typ = mtmp323.(Result__Typ__string_Ok)._0
            var t628 Typ = TArrow{
                _0: ty_x__109,
                _1: x324,
            }
            var t629 Result__Typ__string = Result__Typ__string_Ok{
                _0: t628,
            }
            return t629
        case Result__Typ__string_Err:
            var x325 string = mtmp323.(Result__Typ__string_Err)._0
            var t630 Result__Typ__string = Result__Typ__string_Err{
                _0: x325,
            }
            return t630
        default:
            panic("non-exhaustive match")
        }
    case Let:
        var x309 string = e__104.(Let)._0
        var x310 Exp = e__104.(Let)._1
        var x311 Exp = e__104.(Let)._2
        var inline1122 *ref_int32_x = st__102.current_level
        var inline1123 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline1122)
        var inline1124 *ref_int32_x = st__102.current_level
        var inline1125 int32 = inline1123 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline1124, inline1125)
        var ty_e__125 Result__Typ__string = typeof(st__102, env__103, x310)
        var inline1116 *ref_int32_x = st__102.current_level
        var inline1117 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline1116)
        var inline1118 *ref_int32_x = st__102.current_level
        var inline1119 int32 = inline1117 - 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline1118, inline1119)
        switch ty_e__125.(type) {
        case Result__Typ__string_Ok:
            var x328 Typ = ty_e__125.(Result__Typ__string_Ok)._0
            var t633 Typ = gen(st__102, x328)
            var t634 EnvEntry = EnvEntry{
                name: x309,
                ty: t633,
            }
            var env2__128 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__EnvEntry(env__103, t634)
            var t635 Result__Typ__string = typeof(st__102, env2__128, x311)
            return t635
        case Result__Typ__string_Err:
            var x329 string = ty_e__125.(Result__Typ__string_Err)._0
            var t636 Result__Typ__string = Result__Typ__string_Err{
                _0: x329,
            }
            return t636
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func exp_var(name__129 string) Exp {
    var t639 Exp = Var{
        _0: name__129,
    }
    return t639
}

func exp_lam(name__130 string, body__131 Exp) Exp {
    var t642 Exp = Lam{
        _0: name__130,
        _1: body__131,
    }
    return t642
}

func exp_app(a__132 Exp, b__133 Exp) Exp {
    var t645 Exp = App{
        _0: a__132,
        _1: b__133,
    }
    return t645
}

func exp_let(name__134 string, a__135 Exp, b__136 Exp) Exp {
    var t648 Exp = Let{
        _0: name__134,
        _1: a__135,
        _2: b__136,
    }
    return t648
}

func show_result(label__137 string, res__138 Result__Typ__string) struct{} {
    switch res__138.(type) {
    case Result__Typ__string_Ok:
        var x330 Typ = res__138.(Result__Typ__string_Ok)._0
        var t651 string = label__137 + ": "
        var t652 string = typ_to_string(x330)
        var t653 string = t651 + t652
        var inline1128 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t653)
        _goml_runtime_core_string_println(inline1128)
        return struct{}{}
    case Result__Typ__string_Err:
        var x331 string = res__138.(Result__Typ__string_Err)._0
        var t655 string = label__137 + ": "
        var t656 string = t655 + x331
        var inline1131 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t656)
        _goml_runtime_core_string_println(inline1131)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var st__141 CheckerState = state_new()
    var t659 Exp = exp_var("x")
    var id__142 Exp = exp_lam("x", t659)
    var t660 Exp = exp_var("x")
    var t661 Exp = exp_var("y")
    var t662 Exp = exp_app(t660, t661)
    var t663 Exp = exp_lam("y", t662)
    var c1__143 Exp = exp_lam("x", t663)
    reset_type_variables(st__141)
    var t664 *_goml_vec_EnvEntry = env_empty()
    var t665 Result__Typ__string = typeof(st__141, t664, id__142)
    show_result("id", t665)
    reset_type_variables(st__141)
    var t666 *_goml_vec_EnvEntry = env_empty()
    var t667 Result__Typ__string = typeof(st__141, t666, c1__143)
    show_result("c1", t667)
    reset_type_variables(st__141)
    var t668 *_goml_vec_EnvEntry = env_empty()
    var t669 Exp = exp_var("x")
    var t670 Exp = exp_let("x", c1__143, t669)
    var t671 Result__Typ__string = typeof(st__141, t668, t670)
    show_result("let_x_c1_x", t671)
    reset_type_variables(st__141)
    var t672 *_goml_vec_EnvEntry = env_empty()
    var t673 Exp = exp_var("z")
    var t674 Exp = exp_lam("z", t673)
    var t675 Exp = exp_var("y")
    var t676 Exp = exp_let("y", t674, t675)
    var t677 Result__Typ__string = typeof(st__141, t672, t676)
    show_result("let_y_id_y", t677)
    reset_type_variables(st__141)
    var t678 *_goml_vec_EnvEntry = env_empty()
    var t679 Exp = exp_var("z")
    var t680 Exp = exp_lam("z", t679)
    var t681 Exp = exp_var("y")
    var t682 Exp = exp_let("y", t680, t681)
    var t683 Exp = exp_lam("x", t682)
    var t684 Result__Typ__string = typeof(st__141, t678, t683)
    show_result("lam_x_let_y_id_y", t684)
    reset_type_variables(st__141)
    var t685 *_goml_vec_EnvEntry = env_empty()
    var t686 Exp = exp_var("z")
    var t687 Exp = exp_lam("z", t686)
    var t688 Exp = exp_var("y")
    var t689 Exp = exp_var("x")
    var t690 Exp = exp_app(t688, t689)
    var t691 Exp = exp_let("y", t687, t690)
    var t692 Exp = exp_lam("x", t691)
    var t693 Result__Typ__string = typeof(st__141, t685, t692)
    show_result("lam_x_let_y_id_yx", t693)
    reset_type_variables(st__141)
    var t694 *_goml_vec_EnvEntry = env_empty()
    var t695 Exp = exp_var("x")
    var t696 Exp = exp_var("x")
    var t697 Exp = exp_app(t695, t696)
    var t698 Exp = exp_lam("x", t697)
    var t699 Result__Typ__string = typeof(st__141, t694, t698)
    show_result("self_apply", t699)
    reset_type_variables(st__141)
    var t700 *_goml_vec_EnvEntry = env_empty()
    var t701 Exp = exp_var("x")
    var t702 Exp = exp_var("x")
    var t703 Exp = exp_let("x", t701, t702)
    var t704 Result__Typ__string = typeof(st__141, t700, t703)
    show_result("unbound_var", t704)
    reset_type_variables(st__141)
    var t705 *_goml_vec_EnvEntry = env_empty()
    var t706 Exp = exp_var("y")
    var t707 Exp = exp_var("y")
    var t708 Exp = exp_var("z")
    var t709 Exp = exp_app(t707, t708)
    var t710 Exp = exp_lam("z", t709)
    var t711 Exp = exp_app(t706, t710)
    var t712 Exp = exp_lam("y", t711)
    var t713 Result__Typ__string = typeof(st__141, t705, t712)
    show_result("max_heiber", t713)
    reset_type_variables(st__141)
    var t714 *_goml_vec_EnvEntry = env_empty()
    var t715 Exp = exp_var("k")
    var t716 Exp = exp_var("k")
    var t717 Exp = exp_var("x")
    var t718 Exp = exp_app(t716, t717)
    var t719 Exp = exp_var("y")
    var t720 Exp = exp_app(t718, t719)
    var t721 Exp = exp_app(t715, t720)
    var t722 Exp = exp_var("k")
    var t723 Exp = exp_var("y")
    var t724 Exp = exp_app(t722, t723)
    var t725 Exp = exp_var("x")
    var t726 Exp = exp_app(t724, t725)
    var t727 Exp = exp_app(t721, t726)
    var t728 Exp = exp_lam("k", t727)
    var t729 Exp = exp_lam("y", t728)
    var t730 Exp = exp_lam("x", t729)
    var t731 Result__Typ__string = typeof(st__141, t714, t730)
    show_result("kirang", t731)
    reset_type_variables(st__141)
    var t732 *_goml_vec_EnvEntry = env_empty()
    var t733 Exp = exp_var("id")
    var t734 Exp = exp_var("id")
    var t735 Exp = exp_app(t733, t734)
    var t736 Exp = exp_let("id", id__142, t735)
    var t737 Result__Typ__string = typeof(st__141, t732, t736)
    show_result("let_id_idid", t737)
    reset_type_variables(st__141)
    var t738 *_goml_vec_EnvEntry = env_empty()
    var t739 Exp = exp_var("x")
    var t740 Exp = exp_app(t739, id__142)
    var t741 Exp = exp_var("z")
    var t742 Exp = exp_let("z", t740, t741)
    var t743 Exp = exp_var("y")
    var t744 Exp = exp_let("y", t742, t743)
    var t745 Exp = exp_let("x", c1__143, t744)
    var t746 Result__Typ__string = typeof(st__141, t738, t745)
    show_result("nested_lets", t746)
    reset_type_variables(st__141)
    var t747 *_goml_vec_EnvEntry = env_empty()
    var t748 Exp = exp_var("x")
    var t749 Exp = exp_var("y")
    var t750 Exp = exp_app(t748, t749)
    var t751 Exp = exp_var("y")
    var t752 Exp = exp_var("x")
    var t753 Exp = exp_app(t751, t752)
    var t754 Exp = exp_lam("x", t753)
    var t755 Exp = exp_let("x", t750, t754)
    var t756 Exp = exp_lam("y", t755)
    var t757 Exp = exp_lam("x", t756)
    var t758 Result__Typ__string = typeof(st__141, t747, t757)
    show_result("fun_x_fun_y_let_x_xy_fun_x_yx", t758)
    reset_type_variables(st__141)
    var t759 *_goml_vec_EnvEntry = env_empty()
    var t760 Exp = exp_var("x")
    var t761 Exp = exp_var("y")
    var t762 Exp = exp_let("y", t760, t761)
    var t763 Exp = exp_lam("x", t762)
    var t764 Result__Typ__string = typeof(st__141, t759, t763)
    show_result("sound_gen_1", t764)
    reset_type_variables(st__141)
    var t765 *_goml_vec_EnvEntry = env_empty()
    var t766 Exp = exp_var("x")
    var t767 Exp = exp_lam("z", t766)
    var t768 Exp = exp_var("y")
    var t769 Exp = exp_let("y", t767, t768)
    var t770 Exp = exp_lam("x", t769)
    var t771 Result__Typ__string = typeof(st__141, t765, t770)
    show_result("sound_gen_2", t771)
    reset_type_variables(st__141)
    var t772 *_goml_vec_EnvEntry = env_empty()
    var t773 Exp = exp_var("x")
    var t774 Exp = exp_var("z")
    var t775 Exp = exp_app(t773, t774)
    var t776 Exp = exp_lam("z", t775)
    var t777 Exp = exp_var("y")
    var t778 Exp = exp_let("y", t776, t777)
    var t779 Exp = exp_lam("x", t778)
    var t780 Result__Typ__string = typeof(st__141, t772, t779)
    show_result("sound_gen_3", t780)
    reset_type_variables(st__141)
    var t781 *_goml_vec_EnvEntry = env_empty()
    var t782 Exp = exp_var("x")
    var t783 Exp = exp_var("y")
    var t784 Exp = exp_app(t782, t783)
    var t785 Exp = exp_var("x")
    var t786 Exp = exp_var("y")
    var t787 Exp = exp_app(t785, t786)
    var t788 Exp = exp_let("x", t784, t787)
    var t789 Exp = exp_lam("y", t788)
    var t790 Exp = exp_lam("x", t789)
    var t791 Result__Typ__string = typeof(st__141, t781, t790)
    show_result("double_apply", t791)
    reset_type_variables(st__141)
    var t792 *_goml_vec_EnvEntry = env_empty()
    var t793 Exp = exp_var("x")
    var t794 Exp = exp_var("y")
    var t795 Exp = exp_var("y")
    var t796 Exp
    var inline1190 Exp = App{
        _0: t794,
        _1: t795,
    }
    t796 = inline1190
    var t797 Exp
    var inline1187 string = "y"
    var inline1188 Exp = Let{
        _0: inline1187,
        _1: t793,
        _2: t796,
    }
    t797 = inline1188
    var t798 Exp
    var inline1184 string = "x"
    var inline1185 Exp = Lam{
        _0: inline1184,
        _1: t797,
    }
    t798 = inline1185
    var t799 Result__Typ__string = typeof(st__141, t792, t798)
    show_result("sound_gen_occurs", t799)
    var inline1181 *ref_int32_x = st__141.gensym_counter
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline1181, 0)
    var t800 *_goml_vec_EnvEntry
    var inline1179 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__EnvEntry()
    t800 = inline1179
    var t801 Exp
    var inline1176 string = "x"
    var inline1177 Exp = Var{
        _0: inline1176,
    }
    t801 = inline1177
    var t802 Exp
    var inline1174 Exp = App{
        _0: t801,
        _1: id__142,
    }
    t802 = inline1174
    var t803 Exp
    var inline1171 string = "z"
    var inline1172 Exp = Var{
        _0: inline1171,
    }
    t803 = inline1172
    var t804 Exp
    var inline1168 string = "z"
    var inline1169 Exp = Let{
        _0: inline1168,
        _1: t802,
        _2: t803,
    }
    t804 = inline1169
    var t805 Exp
    var inline1165 string = "y"
    var inline1166 Exp = Var{
        _0: inline1165,
    }
    t805 = inline1166
    var t806 Exp
    var inline1162 string = "y"
    var inline1163 Exp = Let{
        _0: inline1162,
        _1: t804,
        _2: t805,
    }
    t806 = inline1163
    var t807 Exp
    var inline1159 string = "x"
    var inline1160 Exp = Lam{
        _0: inline1159,
        _1: t806,
    }
    t807 = inline1160
    var t808 Result__Typ__string = typeof(st__141, t800, t807)
    var inline1146 string = "fun_x_let_y_let_z_x_id_z_y"
    switch t808.(type) {
    case Result__Typ__string_Ok:
        var inline1147 Typ = t808.(Result__Typ__string_Ok)._0
        var inline1149 string = inline1146 + ": "
        var inline1150 string = typ_to_string(inline1147)
        var inline1151 string = inline1149 + inline1150
        println__T_string(inline1151)
    case Result__Typ__string_Err:
        var inline1153 string = t808.(Result__Typ__string_Err)._0
        var inline1155 string = inline1146 + ": "
        var inline1156 string = inline1155 + inline1153
        println__T_string(inline1156)
    default:
        panic("non-exhaustive match")
    }
    var inline1142 string = ""
    var inline1143 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline1142)
    _goml_runtime_core_string_println(inline1143)
    var inline1138 string = "All Done"
    var inline1139 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline1138)
    _goml_runtime_core_string_println(inline1139)
    var inline1134 string = ""
    var inline1135 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline1134)
    _goml_runtime_core_string_println(inline1135)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__238 *ref_int32_x, value__239 int32) struct{} {
    ref_set__Ref_5int32(self__238, value__239)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__237 *ref_int32_x) int32 {
    var t816 int32 = ref_get__Ref_5int32(self__237)
    return t816
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Tv(value__236 Tv) *ref_Tv_x {
    var t825 *ref_Tv_x = ref__Ref_2Tv(value__236)
    return t825
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__EnvEntry() *_goml_vec_EnvEntry {
    var t831 *_goml_vec_EnvEntry = vec_new__Vec_8EnvEntry()
    return t831
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SubstEntry(self__157 *_goml_vec_SubstEntry, elem__158 SubstEntry) *_goml_vec_SubstEntry {
    var t869 int
    var inline1205 int = vec_len__Vec_10SubstEntry(self__157)
    t869 = inline1205
    var t870 int = t869 + 1
    var result__159 *_goml_vec_SubstEntry
    var inline1203 *_goml_vec_SubstEntry = vec_with_capacity__Vec_10SubstEntry(t870)
    result__159 = inline1203
    var index__160 int = 0
    Loop_loop872:
    for {
        var t873 int
        var inline1199 int = vec_len__Vec_10SubstEntry(self__157)
        t873 = inline1199
        var t874 bool = index__160 < t873
        if t874 {
            var t875 SubstEntry = vec_get__Vec_10SubstEntry(self__157, index__160)
            vec_push__Vec_10SubstEntry(result__159, t875)
            var compound_old60 int = index__160
            var compound_value61 int = 1
            var t876 int = compound_old60 + compound_value61
            index__160 = t876
            continue
        } else {
            break Loop_loop872
        }
    }
    vec_push__Vec_10SubstEntry(result__159, elem__158)
    return result__159
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SubstEntry() *_goml_vec_SubstEntry {
    var t880 *_goml_vec_SubstEntry = vec_new__Vec_10SubstEntry()
    return t880
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__EnvEntry(self__157 *_goml_vec_EnvEntry, elem__158 EnvEntry) *_goml_vec_EnvEntry {
    var t883 int
    var inline1215 int = vec_len__Vec_8EnvEntry(self__157)
    t883 = inline1215
    var t884 int = t883 + 1
    var result__159 *_goml_vec_EnvEntry
    var inline1213 *_goml_vec_EnvEntry = vec_with_capacity__Vec_8EnvEntry(t884)
    result__159 = inline1213
    var index__160 int = 0
    Loop_loop886:
    for {
        var t887 int
        var inline1209 int = vec_len__Vec_8EnvEntry(self__157)
        t887 = inline1209
        var t888 bool = index__160 < t887
        if t888 {
            var t889 EnvEntry = vec_get__Vec_8EnvEntry(self__157, index__160)
            vec_push__Vec_8EnvEntry(result__159, t889)
            var compound_old60 int = index__160
            var compound_value61 int = 1
            var t890 int = compound_old60 + compound_value61
            index__160 = t890
            continue
        } else {
            break Loop_loop886
        }
    }
    vec_push__Vec_8EnvEntry(result__159, elem__158)
    return result__159
}

func println__T_string(value__31 string) struct{} {
    var t893 string
    t893 = value__31
    _goml_runtime_core_string_println(t893)
    return struct{}{}
}

func char_to_string(value__29 rune) string {
    var t899 uint32 = uint32(rune(value__29))
    var t900 bool
    var inline1218 bool = t899 <= 1114111
    if inline1218 {
        var inline1219 bool = t899 >= 55296
        var inline1221 bool
        if inline1219 {
            var inline1223 bool = t899 <= 57343
            inline1221 = inline1223
        } else {
            inline1221 = false
        }
        var inline1222 bool = !inline1221
        t900 = inline1222
    } else {
        t900 = false
    }
    if t900 {
        var t901 string = _goml_runtime_core_char_to_string(value__29)
        return t901
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
