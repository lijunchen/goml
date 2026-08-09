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
    var t370 *ref_int32_x
    var inline939 int32 = 0
    var inline940 *ref_int32_x = ref__Ref_5int32(inline939)
    t370 = inline940
    var t371 *ref_int32_x
    var inline936 int32 = 1
    var inline937 *ref_int32_x = ref__Ref_5int32(inline936)
    t371 = inline937
    var t372 CheckerState = CheckerState{
        gensym_counter: t370,
        current_level: t371,
    }
    return t372
}

func reset_type_variables(st__2 CheckerState) struct{} {
    var inline951 *ref_int32_x = st__2.gensym_counter
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline951, 0)
    var inline948 *ref_int32_x = st__2.current_level
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline948, 1)
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
    var t398 *ref_int32_x = st__10.gensym_counter
    var n__11 int32
    var inline968 int32 = ref_get__Ref_5int32(t398)
    n__11 = inline968
    var t399 *ref_int32_x = st__10.gensym_counter
    var t400 int32 = n__11 + 1
    ref_set__Ref_5int32(t399, t400)
    var t403 bool = n__11 < 26
    if t403 {
        var t404 rune = nth_letter(n__11)
        var inline962 string = char_to_string(t404)
        return inline962
    } else {
        var t406 string
        var inline964 string = _goml_runtime_core_int32_to_string(n__11)
        t406 = inline964
        var t407 string = "t" + t406
        return t407
    }
}

func typ_is_arrow(ty__15 Typ) bool {
    switch ty__15.(type) {
    case TVar:
        var x178 *ref_Tv_x = ty__15.(TVar)._0
        var mtmp182 Tv
        var inline985 Tv = ref_get__Ref_2Tv(x178)
        mtmp182 = inline985
        switch mtmp182.(type) {
        case Link:
            var x185 Typ = mtmp182.(Link)._0
            var t420 bool = typ_is_arrow(x185)
            return t420
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
        var x186 *ref_Tv_x = ty__18.(TVar)._0
        var mtmp190 Tv
        var inline987 Tv = ref_get__Ref_2Tv(x186)
        mtmp190 = inline987
        switch mtmp190.(type) {
        case Unbound:
            var x191 string = mtmp190.(Unbound)._0
            var t427 string = "'" + x191
            return t427
        case Link:
            var x193 Typ = mtmp190.(Link)._0
            var t428 string = typ_to_string(x193)
            return t428
        default:
            panic("non-exhaustive match")
        }
    case QVar:
        var x187 string = ty__18.(QVar)._0
        var t429 string = "'" + x187
        return t429
    case TArrow:
        var x188 Typ = ty__18.(TArrow)._0
        var x189 Typ = ty__18.(TArrow)._1
        var t434 bool = typ_is_arrow(x188)
        var jp431 string
        if t434 {
            var t435 string = typ_to_string(x188)
            var t436 string = "(" + t435
            var t437 string = t436 + ")"
            jp431 = t437
        } else {
            var t438 string = typ_to_string(x188)
            jp431 = t438
        }
        var s2__26 string = typ_to_string(x189)
        var t432 string = jp431 + " -> "
        var t433 string = t432 + s2__26
        return t433
    default:
        panic("non-exhaustive match")
    }
}

func env_empty() *_goml_vec_EnvEntry {
    var inline989 *_goml_vec_EnvEntry = vec_new__Vec_8EnvEntry()
    return inline989
}

func env_lookup(env__28 *_goml_vec_EnvEntry, name__29 string) Option__Typ {
    var t443 int
    var inline1015 int = vec_len__Vec_8EnvEntry(env__28)
    t443 = inline1015
    var t444 int = t443 - 1
    var i__30 *ref_int_x
    var inline1013 *ref_int_x = ref__Ref_3int(t444)
    i__30 = inline1013
    var found__31 *ref_Option__Typ_x
    var inline1011 *ref_Option__Typ_x = ref__Ref_11Option__Typ(None{})
    found__31 = inline1011
    var done__32 *ref_bool_x
    var inline1008 bool = false
    var inline1009 *ref_bool_x = ref__Ref_4bool(inline1008)
    done__32 = inline1009
    Loop_loop447:
    for {
        var t460 bool
        var inline1004 bool = ref_get__Ref_4bool(done__32)
        t460 = inline1004
        var t461 bool = !t460
        var jp449 bool
        if t461 {
            var t462 int
            var inline991 int = ref_get__Ref_3int(i__30)
            t462 = inline991
            var t463 bool = t462 >= 0
            jp449 = t463
        } else {
            jp449 = false
        }
        if jp449 {
            var t450 int
            var inline1002 int = ref_get__Ref_3int(i__30)
            t450 = inline1002
            var entry__33 EnvEntry = vec_get__Vec_8EnvEntry(env__28, t450)
            var t452 string = entry__33.name
            var t453 bool = t452 == name__29
            if t453 {
                var t454 Typ = entry__33.ty
                var t455 Option__Typ = Some{
                    _0: t454,
                }
                ref_set__Ref_11Option__Typ(found__31, t455)
                var inline993 bool = true
                ref_set__Ref_4bool(done__32, inline993)
                continue
            } else {
                var t457 int
                var inline1000 int = ref_get__Ref_3int(i__30)
                t457 = inline1000
                var t458 int = t457 - 1
                ref_set__Ref_3int(i__30, t458)
                continue
            }
        } else {
            break Loop_loop447
        }
    }
    var inline1006 Option__Typ = ref_get__Ref_11Option__Typ(found__31)
    return inline1006
}

func subst_lookup(subst__34 *_goml_vec_SubstEntry, name__35 string) Option__Typ {
    var t466 int
    var inline1041 int = vec_len__Vec_10SubstEntry(subst__34)
    t466 = inline1041
    var t467 int = t466 - 1
    var i__36 *ref_int_x
    var inline1039 *ref_int_x = ref__Ref_3int(t467)
    i__36 = inline1039
    var found__37 *ref_Option__Typ_x
    var inline1037 *ref_Option__Typ_x = ref__Ref_11Option__Typ(None{})
    found__37 = inline1037
    var done__38 *ref_bool_x
    var inline1034 bool = false
    var inline1035 *ref_bool_x = ref__Ref_4bool(inline1034)
    done__38 = inline1035
    Loop_loop470:
    for {
        var t483 bool
        var inline1030 bool = ref_get__Ref_4bool(done__38)
        t483 = inline1030
        var t484 bool = !t483
        var jp472 bool
        if t484 {
            var t485 int
            var inline1017 int = ref_get__Ref_3int(i__36)
            t485 = inline1017
            var t486 bool = t485 >= 0
            jp472 = t486
        } else {
            jp472 = false
        }
        if jp472 {
            var t473 int
            var inline1028 int = ref_get__Ref_3int(i__36)
            t473 = inline1028
            var entry__39 SubstEntry = vec_get__Vec_10SubstEntry(subst__34, t473)
            var t475 string = entry__39.name
            var t476 bool = t475 == name__35
            if t476 {
                var t477 Typ = entry__39.ty
                var t478 Option__Typ = Some{
                    _0: t477,
                }
                ref_set__Ref_11Option__Typ(found__37, t478)
                var inline1019 bool = true
                ref_set__Ref_4bool(done__38, inline1019)
                continue
            } else {
                var t480 int
                var inline1026 int = ref_get__Ref_3int(i__36)
                t480 = inline1026
                var t481 int = t480 - 1
                ref_set__Ref_3int(i__36, t481)
                continue
            }
        } else {
            break Loop_loop470
        }
    }
    var inline1032 Option__Typ = ref_get__Ref_11Option__Typ(found__37)
    return inline1032
}

func occurs(st__40 CheckerState, tvr__41 *ref_Tv_x, ty__42 Typ) Result__unit__string {
    switch ty__42.(type) {
    case TVar:
        var x198 *ref_Tv_x = ty__42.(TVar)._0
        var t493 bool = ptr_eq__Ref_2Tv(tvr__41, x198)
        if t493 {
            var t494 Result__unit__string = Result__unit__string_Err{
                _0: "occurs check",
            }
            return t494
        } else {
            var mtmp202 Tv
            var inline1049 Tv = ref_get__Ref_2Tv(x198)
            mtmp202 = inline1049
            switch mtmp202.(type) {
            case Unbound:
                var x203 string = mtmp202.(Unbound)._0
                var x204 int32 = mtmp202.(Unbound)._1
                var mtmp206 Tv
                var inline1047 Tv = ref_get__Ref_2Tv(tvr__41)
                mtmp206 = inline1047
                var jp498 int32
                switch mtmp206.(type) {
                case Unbound:
                    var x208 int32 = mtmp206.(Unbound)._1
                    var inline1043 bool = x208 < x204
                    if inline1043 {
                        jp498 = x208
                    } else {
                        jp498 = x204
                    }
                default:
                    jp498 = x204
                }
                var t499 Tv = Unbound{
                    _0: x203,
                    _1: jp498,
                }
                ref_set__Ref_2Tv(x198, t499)
                var t500 Result__unit__string = Result__unit__string_Ok{
                    _0: struct{}{},
                }
                return t500
            case Link:
                var x205 Typ = mtmp202.(Link)._0
                var t502 Result__unit__string = occurs(st__40, tvr__41, x205)
                return t502
            default:
                panic("non-exhaustive match")
            }
        }
    case TArrow:
        var x200 Typ = ty__42.(TArrow)._0
        var x201 Typ = ty__42.(TArrow)._1
        var mtmp211 Result__unit__string = occurs(st__40, tvr__41, x200)
        switch mtmp211.(type) {
        case Result__unit__string_Ok:
            var t505 Result__unit__string = occurs(st__40, tvr__41, x201)
            return t505
        case Result__unit__string_Err:
            var x213 string = mtmp211.(Result__unit__string_Err)._0
            var t506 Result__unit__string = Result__unit__string_Err{
                _0: x213,
            }
            return t506
        default:
            panic("non-exhaustive match")
        }
    default:
        var t507 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        return t507
    }
}

func unify(st__52 CheckerState, t1__53 Typ, t2__54 Typ) Result__unit__string {
    switch t2__54.(type) {
    case TVar:
        var x217 *ref_Tv_x = t2__54.(TVar)._0
        switch t1__53.(type) {
        case TVar:
            var x221 *ref_Tv_x = t1__53.(TVar)._0
            var t516 bool = ptr_eq__Ref_2Tv(x221, x217)
            if t516 {
                var t517 Result__unit__string = Result__unit__string_Ok{
                    _0: struct{}{},
                }
                return t517
            } else {
                var mtmp225 Tv
                var inline1055 Tv = ref_get__Ref_2Tv(x221)
                mtmp225 = inline1055
                switch mtmp225.(type) {
                case Unbound:
                    var mtmp229 Tv
                    var inline1053 Tv = ref_get__Ref_2Tv(x217)
                    mtmp229 = inline1053
                    switch mtmp229.(type) {
                    case Unbound:
                        var t522 Typ = TVar{
                            _0: x217,
                        }
                        var mtmp233 Result__unit__string = occurs(st__52, x221, t522)
                        switch mtmp233.(type) {
                        case Result__unit__string_Ok:
                            var t525 Typ = TVar{
                                _0: x217,
                            }
                            var t526 Tv = Link{
                                _0: t525,
                            }
                            ref_set__Ref_2Tv(x221, t526)
                            var t527 Result__unit__string = Result__unit__string_Ok{
                                _0: struct{}{},
                            }
                            return t527
                        case Result__unit__string_Err:
                            var x235 string = mtmp233.(Result__unit__string_Err)._0
                            var t528 Result__unit__string = Result__unit__string_Err{
                                _0: x235,
                            }
                            return t528
                        default:
                            panic("non-exhaustive match")
                        }
                    case Link:
                        var x232 Typ = mtmp229.(Link)._0
                        var t529 Typ = TVar{
                            _0: x221,
                        }
                        var t530 Result__unit__string = unify(st__52, t529, x232)
                        return t530
                    default:
                        panic("non-exhaustive match")
                    }
                case Link:
                    var x228 Typ = mtmp225.(Link)._0
                    var t531 Typ = TVar{
                        _0: x217,
                    }
                    var t532 Result__unit__string = unify(st__52, x228, t531)
                    return t532
                default:
                    panic("non-exhaustive match")
                }
            }
        default:
            var mtmp237 Tv
            var inline1059 Tv = ref_get__Ref_2Tv(x217)
            mtmp237 = inline1059
            switch mtmp237.(type) {
            case Unbound:
                var mtmp241 Result__unit__string = occurs(st__52, x217, t1__53)
                switch mtmp241.(type) {
                case Result__unit__string_Ok:
                    var t537 Tv = Link{
                        _0: t1__53,
                    }
                    ref_set__Ref_2Tv(x217, t537)
                    var t538 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    return t538
                case Result__unit__string_Err:
                    var x243 string = mtmp241.(Result__unit__string_Err)._0
                    var t539 Result__unit__string = Result__unit__string_Err{
                        _0: x243,
                    }
                    return t539
                default:
                    panic("non-exhaustive match")
                }
            case Link:
                var x240 Typ = mtmp237.(Link)._0
                var t540 Result__unit__string = unify(st__52, t1__53, x240)
                return t540
            default:
                panic("non-exhaustive match")
            }
        }
    case TArrow:
        var x219 Typ = t2__54.(TArrow)._0
        var x220 Typ = t2__54.(TArrow)._1
        switch t1__53.(type) {
        case TVar:
            var x245 *ref_Tv_x = t1__53.(TVar)._0
            var mtmp249 Tv
            var inline1063 Tv = ref_get__Ref_2Tv(x245)
            mtmp249 = inline1063
            switch mtmp249.(type) {
            case Unbound:
                var mtmp253 Result__unit__string = occurs(st__52, x245, t2__54)
                switch mtmp253.(type) {
                case Result__unit__string_Ok:
                    var t547 Tv = Link{
                        _0: t2__54,
                    }
                    ref_set__Ref_2Tv(x245, t547)
                    var t548 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    return t548
                case Result__unit__string_Err:
                    var x255 string = mtmp253.(Result__unit__string_Err)._0
                    var t549 Result__unit__string = Result__unit__string_Err{
                        _0: x255,
                    }
                    return t549
                default:
                    panic("non-exhaustive match")
                }
            case Link:
                var x252 Typ = mtmp249.(Link)._0
                var t550 Result__unit__string = unify(st__52, x252, t2__54)
                return t550
            default:
                panic("non-exhaustive match")
            }
        case TArrow:
            var x247 Typ = t1__53.(TArrow)._0
            var x248 Typ = t1__53.(TArrow)._1
            var mtmp257 Result__unit__string = unify(st__52, x247, x219)
            switch mtmp257.(type) {
            case Result__unit__string_Ok:
                var t553 Result__unit__string = unify(st__52, x248, x220)
                return t553
            case Result__unit__string_Err:
                var x259 string = mtmp257.(Result__unit__string_Err)._0
                var t554 Result__unit__string = Result__unit__string_Err{
                    _0: x259,
                }
                return t554
            default:
                panic("non-exhaustive match")
            }
        default:
            var t555 Result__unit__string = Result__unit__string_Err{
                _0: "unify error",
            }
            return t555
        }
    default:
        switch t1__53.(type) {
        case TVar:
            var x260 *ref_Tv_x = t1__53.(TVar)._0
            var mtmp264 Tv
            var inline1067 Tv = ref_get__Ref_2Tv(x260)
            mtmp264 = inline1067
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
        default:
            var t566 Result__unit__string = Result__unit__string_Err{
                _0: "unify error",
            }
            return t566
        }
    }
}

func gen(st__73 CheckerState, ty__74 Typ) Typ {
    switch ty__74.(type) {
    case TVar:
        var x272 *ref_Tv_x = ty__74.(TVar)._0
        var mtmp276 Tv
        var inline1071 Tv = ref_get__Ref_2Tv(x272)
        mtmp276 = inline1071
        switch mtmp276.(type) {
        case Unbound:
            var x277 string = mtmp276.(Unbound)._0
            var x278 int32 = mtmp276.(Unbound)._1
            var t573 *ref_int32_x = st__73.current_level
            var cur__78 int32
            var inline1069 int32 = ref_get__Ref_5int32(t573)
            cur__78 = inline1069
            var t576 bool = x278 > cur__78
            if t576 {
                var t577 Typ = QVar{
                    _0: x277,
                }
                return t577
            } else {
                var t578 Typ = TVar{
                    _0: x272,
                }
                return t578
            }
        case Link:
            var x279 Typ = mtmp276.(Link)._0
            var t579 Typ = gen(st__73, x279)
            return t579
        default:
            panic("non-exhaustive match")
        }
    case TArrow:
        var x274 Typ = ty__74.(TArrow)._0
        var x275 Typ = ty__74.(TArrow)._1
        var t580 Typ = gen(st__73, x274)
        var t581 Typ = gen(st__73, x275)
        var t582 Typ = TArrow{
            _0: t580,
            _1: t581,
        }
        return t582
    default:
        return ty__74
    }
}

func inst_loop(st__83 CheckerState, subst__84 *_goml_vec_SubstEntry, ty__85 Typ) Tuple2_3Typ_16Vec_10SubstEntry {
    switch ty__85.(type) {
    case TVar:
        var x280 *ref_Tv_x = ty__85.(TVar)._0
        var mtmp284 Tv
        var inline1073 Tv = ref_get__Ref_2Tv(x280)
        mtmp284 = inline1073
        switch mtmp284.(type) {
        case Link:
            var x287 Typ = mtmp284.(Link)._0
            var t589 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, subst__84, x287)
            return t589
        default:
            var t590 Typ = TVar{
                _0: x280,
            }
            var t591 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: t590,
                _1: subst__84,
            }
            return t591
        }
    case QVar:
        var x281 string = ty__85.(QVar)._0
        var mtmp288 Option__Typ = subst_lookup(subst__84, x281)
        switch mtmp288.(type) {
        case None:
            var tv__88 Typ
            var inline1075 string = gensym(st__83)
            var inline1076 *ref_int32_x = st__83.current_level
            var inline1077 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline1076)
            var inline1078 Tv = Unbound{
                _0: inline1075,
                _1: inline1077,
            }
            var inline1079 *ref_Tv_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Tv(inline1078)
            var inline1080 Typ = TVar{
                _0: inline1079,
            }
            tv__88 = inline1080
            var t594 SubstEntry = SubstEntry{
                name: x281,
                ty: tv__88,
            }
            var new_subst__89 *_goml_vec_SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SubstEntry(subst__84, t594)
            var t595 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: tv__88,
                _1: new_subst__89,
            }
            return t595
        case Some:
            var x289 Typ = mtmp288.(Some)._0
            var t596 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: x289,
                _1: subst__84,
            }
            return t596
        default:
            panic("non-exhaustive match")
        }
    case TArrow:
        var x282 Typ = ty__85.(TArrow)._0
        var x283 Typ = ty__85.(TArrow)._1
        var mtmp290 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, subst__84, x282)
        var x291 Typ = mtmp290._0
        var x292 *_goml_vec_SubstEntry = mtmp290._1
        var mtmp293 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, x292, x283)
        var x294 Typ = mtmp293._0
        var x295 *_goml_vec_SubstEntry = mtmp293._1
        var t597 Typ = TArrow{
            _0: x291,
            _1: x294,
        }
        var t598 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
            _0: t597,
            _1: x295,
        }
        return t598
    default:
        panic("non-exhaustive match")
    }
}

func typeof(st__102 CheckerState, env__103 *_goml_vec_EnvEntry, e__104 Exp) Result__Typ__string {
    switch e__104.(type) {
    case Var:
        var x299 string = e__104.(Var)._0
        var mtmp307 Option__Typ = env_lookup(env__103, x299)
        switch mtmp307.(type) {
        case None:
            var t607 Result__Typ__string = Result__Typ__string_Err{
                _0: "unbound var",
            }
            return t607
        case Some:
            var x308 Typ = mtmp307.(Some)._0
            var t608 Typ
            var inline1084 *_goml_vec_SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SubstEntry()
            var inline1085 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__102, inline1084, x308)
            var inline1086 Typ = inline1085._0
            t608 = inline1086
            var t609 Result__Typ__string = Result__Typ__string_Ok{
                _0: t608,
            }
            return t609
        default:
            panic("non-exhaustive match")
        }
    case App:
        var x300 Exp = e__104.(App)._0
        var x301 Exp = e__104.(App)._1
        var mtmp309 Result__Typ__string = typeof(st__102, env__103, x300)
        switch mtmp309.(type) {
        case Result__Typ__string_Ok:
            var x310 Typ = mtmp309.(Result__Typ__string_Ok)._0
            var mtmp312 Result__Typ__string = typeof(st__102, env__103, x301)
            switch mtmp312.(type) {
            case Result__Typ__string_Ok:
                var x313 Typ = mtmp312.(Result__Typ__string_Ok)._0
                var ty_res__119 Typ
                var inline1090 string = gensym(st__102)
                var inline1091 *ref_int32_x = st__102.current_level
                var inline1092 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline1091)
                var inline1093 Tv = Unbound{
                    _0: inline1090,
                    _1: inline1092,
                }
                var inline1094 *ref_Tv_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Tv(inline1093)
                var inline1095 Typ = TVar{
                    _0: inline1094,
                }
                ty_res__119 = inline1095
                var arrow__120 Typ = TArrow{
                    _0: x313,
                    _1: ty_res__119,
                }
                var mtmp315 Result__unit__string = unify(st__102, x310, arrow__120)
                switch mtmp315.(type) {
                case Result__unit__string_Ok:
                    var t616 Result__Typ__string = Result__Typ__string_Ok{
                        _0: ty_res__119,
                    }
                    return t616
                case Result__unit__string_Err:
                    var x317 string = mtmp315.(Result__unit__string_Err)._0
                    var t617 Result__Typ__string = Result__Typ__string_Err{
                        _0: x317,
                    }
                    return t617
                default:
                    panic("non-exhaustive match")
                }
            case Result__Typ__string_Err:
                var x314 string = mtmp312.(Result__Typ__string_Err)._0
                var t618 Result__Typ__string = Result__Typ__string_Err{
                    _0: x314,
                }
                return t618
            default:
                panic("non-exhaustive match")
            }
        case Result__Typ__string_Err:
            var x311 string = mtmp309.(Result__Typ__string_Err)._0
            var t619 Result__Typ__string = Result__Typ__string_Err{
                _0: x311,
            }
            return t619
        default:
            panic("non-exhaustive match")
        }
    case Lam:
        var x302 string = e__104.(Lam)._0
        var x303 Exp = e__104.(Lam)._1
        var ty_x__109 Typ
        var inline1097 string = gensym(st__102)
        var inline1098 *ref_int32_x = st__102.current_level
        var inline1099 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline1098)
        var inline1100 Tv = Unbound{
            _0: inline1097,
            _1: inline1099,
        }
        var inline1101 *ref_Tv_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Tv(inline1100)
        var inline1102 Typ = TVar{
            _0: inline1101,
        }
        ty_x__109 = inline1102
        var t620 EnvEntry = EnvEntry{
            name: x302,
            ty: ty_x__109,
        }
        var env2__110 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__EnvEntry(env__103, t620)
        var mtmp318 Result__Typ__string = typeof(st__102, env2__110, x303)
        switch mtmp318.(type) {
        case Result__Typ__string_Ok:
            var x319 Typ = mtmp318.(Result__Typ__string_Ok)._0
            var t623 Typ = TArrow{
                _0: ty_x__109,
                _1: x319,
            }
            var t624 Result__Typ__string = Result__Typ__string_Ok{
                _0: t623,
            }
            return t624
        case Result__Typ__string_Err:
            var x320 string = mtmp318.(Result__Typ__string_Err)._0
            var t625 Result__Typ__string = Result__Typ__string_Err{
                _0: x320,
            }
            return t625
        default:
            panic("non-exhaustive match")
        }
    case Let:
        var x304 string = e__104.(Let)._0
        var x305 Exp = e__104.(Let)._1
        var x306 Exp = e__104.(Let)._2
        var inline1110 *ref_int32_x = st__102.current_level
        var inline1111 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline1110)
        var inline1112 *ref_int32_x = st__102.current_level
        var inline1113 int32 = inline1111 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline1112, inline1113)
        var ty_e__125 Result__Typ__string = typeof(st__102, env__103, x305)
        var inline1104 *ref_int32_x = st__102.current_level
        var inline1105 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline1104)
        var inline1106 *ref_int32_x = st__102.current_level
        var inline1107 int32 = inline1105 - 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline1106, inline1107)
        switch ty_e__125.(type) {
        case Result__Typ__string_Ok:
            var x323 Typ = ty_e__125.(Result__Typ__string_Ok)._0
            var t628 Typ = gen(st__102, x323)
            var t629 EnvEntry = EnvEntry{
                name: x304,
                ty: t628,
            }
            var env2__128 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__EnvEntry(env__103, t629)
            var t630 Result__Typ__string = typeof(st__102, env2__128, x306)
            return t630
        case Result__Typ__string_Err:
            var x324 string = ty_e__125.(Result__Typ__string_Err)._0
            var t631 Result__Typ__string = Result__Typ__string_Err{
                _0: x324,
            }
            return t631
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func exp_var(name__129 string) Exp {
    var t634 Exp = Var{
        _0: name__129,
    }
    return t634
}

func exp_lam(name__130 string, body__131 Exp) Exp {
    var t637 Exp = Lam{
        _0: name__130,
        _1: body__131,
    }
    return t637
}

func exp_app(a__132 Exp, b__133 Exp) Exp {
    var t640 Exp = App{
        _0: a__132,
        _1: b__133,
    }
    return t640
}

func exp_let(name__134 string, a__135 Exp, b__136 Exp) Exp {
    var t643 Exp = Let{
        _0: name__134,
        _1: a__135,
        _2: b__136,
    }
    return t643
}

func show_result(label__137 string, res__138 Result__Typ__string) struct{} {
    switch res__138.(type) {
    case Result__Typ__string_Ok:
        var x325 Typ = res__138.(Result__Typ__string_Ok)._0
        var t646 string = label__137 + ": "
        var t647 string = typ_to_string(x325)
        var t648 string = t646 + t647
        var inline1116 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t648)
        _goml_runtime_core_string_println(inline1116)
        return struct{}{}
    case Result__Typ__string_Err:
        var x326 string = res__138.(Result__Typ__string_Err)._0
        var t650 string = label__137 + ": "
        var t651 string = t650 + x326
        var inline1119 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t651)
        _goml_runtime_core_string_println(inline1119)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var st__141 CheckerState = state_new()
    var t654 Exp = exp_var("x")
    var id__142 Exp = exp_lam("x", t654)
    var t655 Exp = exp_var("x")
    var t656 Exp = exp_var("y")
    var t657 Exp = exp_app(t655, t656)
    var t658 Exp = exp_lam("y", t657)
    var c1__143 Exp = exp_lam("x", t658)
    reset_type_variables(st__141)
    var t659 *_goml_vec_EnvEntry = env_empty()
    var t660 Result__Typ__string = typeof(st__141, t659, id__142)
    show_result("id", t660)
    reset_type_variables(st__141)
    var t661 *_goml_vec_EnvEntry = env_empty()
    var t662 Result__Typ__string = typeof(st__141, t661, c1__143)
    show_result("c1", t662)
    reset_type_variables(st__141)
    var t663 *_goml_vec_EnvEntry = env_empty()
    var t664 Exp = exp_var("x")
    var t665 Exp = exp_let("x", c1__143, t664)
    var t666 Result__Typ__string = typeof(st__141, t663, t665)
    show_result("let_x_c1_x", t666)
    reset_type_variables(st__141)
    var t667 *_goml_vec_EnvEntry = env_empty()
    var t668 Exp = exp_var("z")
    var t669 Exp = exp_lam("z", t668)
    var t670 Exp = exp_var("y")
    var t671 Exp = exp_let("y", t669, t670)
    var t672 Result__Typ__string = typeof(st__141, t667, t671)
    show_result("let_y_id_y", t672)
    reset_type_variables(st__141)
    var t673 *_goml_vec_EnvEntry = env_empty()
    var t674 Exp = exp_var("z")
    var t675 Exp = exp_lam("z", t674)
    var t676 Exp = exp_var("y")
    var t677 Exp = exp_let("y", t675, t676)
    var t678 Exp = exp_lam("x", t677)
    var t679 Result__Typ__string = typeof(st__141, t673, t678)
    show_result("lam_x_let_y_id_y", t679)
    reset_type_variables(st__141)
    var t680 *_goml_vec_EnvEntry = env_empty()
    var t681 Exp = exp_var("z")
    var t682 Exp = exp_lam("z", t681)
    var t683 Exp = exp_var("y")
    var t684 Exp = exp_var("x")
    var t685 Exp = exp_app(t683, t684)
    var t686 Exp = exp_let("y", t682, t685)
    var t687 Exp = exp_lam("x", t686)
    var t688 Result__Typ__string = typeof(st__141, t680, t687)
    show_result("lam_x_let_y_id_yx", t688)
    reset_type_variables(st__141)
    var t689 *_goml_vec_EnvEntry = env_empty()
    var t690 Exp = exp_var("x")
    var t691 Exp = exp_var("x")
    var t692 Exp = exp_app(t690, t691)
    var t693 Exp = exp_lam("x", t692)
    var t694 Result__Typ__string = typeof(st__141, t689, t693)
    show_result("self_apply", t694)
    reset_type_variables(st__141)
    var t695 *_goml_vec_EnvEntry = env_empty()
    var t696 Exp = exp_var("x")
    var t697 Exp = exp_var("x")
    var t698 Exp = exp_let("x", t696, t697)
    var t699 Result__Typ__string = typeof(st__141, t695, t698)
    show_result("unbound_var", t699)
    reset_type_variables(st__141)
    var t700 *_goml_vec_EnvEntry = env_empty()
    var t701 Exp = exp_var("y")
    var t702 Exp = exp_var("y")
    var t703 Exp = exp_var("z")
    var t704 Exp = exp_app(t702, t703)
    var t705 Exp = exp_lam("z", t704)
    var t706 Exp = exp_app(t701, t705)
    var t707 Exp = exp_lam("y", t706)
    var t708 Result__Typ__string = typeof(st__141, t700, t707)
    show_result("max_heiber", t708)
    reset_type_variables(st__141)
    var t709 *_goml_vec_EnvEntry = env_empty()
    var t710 Exp = exp_var("k")
    var t711 Exp = exp_var("k")
    var t712 Exp = exp_var("x")
    var t713 Exp = exp_app(t711, t712)
    var t714 Exp = exp_var("y")
    var t715 Exp = exp_app(t713, t714)
    var t716 Exp = exp_app(t710, t715)
    var t717 Exp = exp_var("k")
    var t718 Exp = exp_var("y")
    var t719 Exp = exp_app(t717, t718)
    var t720 Exp = exp_var("x")
    var t721 Exp = exp_app(t719, t720)
    var t722 Exp = exp_app(t716, t721)
    var t723 Exp = exp_lam("k", t722)
    var t724 Exp = exp_lam("y", t723)
    var t725 Exp = exp_lam("x", t724)
    var t726 Result__Typ__string = typeof(st__141, t709, t725)
    show_result("kirang", t726)
    reset_type_variables(st__141)
    var t727 *_goml_vec_EnvEntry = env_empty()
    var t728 Exp = exp_var("id")
    var t729 Exp = exp_var("id")
    var t730 Exp = exp_app(t728, t729)
    var t731 Exp = exp_let("id", id__142, t730)
    var t732 Result__Typ__string = typeof(st__141, t727, t731)
    show_result("let_id_idid", t732)
    reset_type_variables(st__141)
    var t733 *_goml_vec_EnvEntry = env_empty()
    var t734 Exp = exp_var("x")
    var t735 Exp = exp_app(t734, id__142)
    var t736 Exp = exp_var("z")
    var t737 Exp = exp_let("z", t735, t736)
    var t738 Exp = exp_var("y")
    var t739 Exp = exp_let("y", t737, t738)
    var t740 Exp = exp_let("x", c1__143, t739)
    var t741 Result__Typ__string = typeof(st__141, t733, t740)
    show_result("nested_lets", t741)
    reset_type_variables(st__141)
    var t742 *_goml_vec_EnvEntry = env_empty()
    var t743 Exp = exp_var("x")
    var t744 Exp = exp_var("y")
    var t745 Exp = exp_app(t743, t744)
    var t746 Exp = exp_var("y")
    var t747 Exp = exp_var("x")
    var t748 Exp = exp_app(t746, t747)
    var t749 Exp = exp_lam("x", t748)
    var t750 Exp = exp_let("x", t745, t749)
    var t751 Exp = exp_lam("y", t750)
    var t752 Exp = exp_lam("x", t751)
    var t753 Result__Typ__string = typeof(st__141, t742, t752)
    show_result("fun_x_fun_y_let_x_xy_fun_x_yx", t753)
    reset_type_variables(st__141)
    var t754 *_goml_vec_EnvEntry = env_empty()
    var t755 Exp = exp_var("x")
    var t756 Exp = exp_var("y")
    var t757 Exp = exp_let("y", t755, t756)
    var t758 Exp = exp_lam("x", t757)
    var t759 Result__Typ__string = typeof(st__141, t754, t758)
    show_result("sound_gen_1", t759)
    reset_type_variables(st__141)
    var t760 *_goml_vec_EnvEntry = env_empty()
    var t761 Exp = exp_var("x")
    var t762 Exp = exp_lam("z", t761)
    var t763 Exp = exp_var("y")
    var t764 Exp = exp_let("y", t762, t763)
    var t765 Exp = exp_lam("x", t764)
    var t766 Result__Typ__string = typeof(st__141, t760, t765)
    show_result("sound_gen_2", t766)
    reset_type_variables(st__141)
    var t767 *_goml_vec_EnvEntry = env_empty()
    var t768 Exp = exp_var("x")
    var t769 Exp = exp_var("z")
    var t770 Exp = exp_app(t768, t769)
    var t771 Exp = exp_lam("z", t770)
    var t772 Exp = exp_var("y")
    var t773 Exp = exp_let("y", t771, t772)
    var t774 Exp = exp_lam("x", t773)
    var t775 Result__Typ__string = typeof(st__141, t767, t774)
    show_result("sound_gen_3", t775)
    reset_type_variables(st__141)
    var t776 *_goml_vec_EnvEntry = env_empty()
    var t777 Exp = exp_var("x")
    var t778 Exp = exp_var("y")
    var t779 Exp = exp_app(t777, t778)
    var t780 Exp = exp_var("x")
    var t781 Exp = exp_var("y")
    var t782 Exp = exp_app(t780, t781)
    var t783 Exp = exp_let("x", t779, t782)
    var t784 Exp = exp_lam("y", t783)
    var t785 Exp = exp_lam("x", t784)
    var t786 Result__Typ__string = typeof(st__141, t776, t785)
    show_result("double_apply", t786)
    reset_type_variables(st__141)
    var t787 *_goml_vec_EnvEntry = env_empty()
    var t788 Exp = exp_var("x")
    var t789 Exp = exp_var("y")
    var t790 Exp = exp_var("y")
    var t791 Exp
    var inline1178 Exp = App{
        _0: t789,
        _1: t790,
    }
    t791 = inline1178
    var t792 Exp
    var inline1175 string = "y"
    var inline1176 Exp = Let{
        _0: inline1175,
        _1: t788,
        _2: t791,
    }
    t792 = inline1176
    var t793 Exp
    var inline1172 string = "x"
    var inline1173 Exp = Lam{
        _0: inline1172,
        _1: t792,
    }
    t793 = inline1173
    var t794 Result__Typ__string = typeof(st__141, t787, t793)
    show_result("sound_gen_occurs", t794)
    var inline1169 *ref_int32_x = st__141.gensym_counter
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline1169, 0)
    var t795 *_goml_vec_EnvEntry
    var inline1167 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__EnvEntry()
    t795 = inline1167
    var t796 Exp
    var inline1164 string = "x"
    var inline1165 Exp = Var{
        _0: inline1164,
    }
    t796 = inline1165
    var t797 Exp
    var inline1162 Exp = App{
        _0: t796,
        _1: id__142,
    }
    t797 = inline1162
    var t798 Exp
    var inline1159 string = "z"
    var inline1160 Exp = Var{
        _0: inline1159,
    }
    t798 = inline1160
    var t799 Exp
    var inline1156 string = "z"
    var inline1157 Exp = Let{
        _0: inline1156,
        _1: t797,
        _2: t798,
    }
    t799 = inline1157
    var t800 Exp
    var inline1153 string = "y"
    var inline1154 Exp = Var{
        _0: inline1153,
    }
    t800 = inline1154
    var t801 Exp
    var inline1150 string = "y"
    var inline1151 Exp = Let{
        _0: inline1150,
        _1: t799,
        _2: t800,
    }
    t801 = inline1151
    var t802 Exp
    var inline1147 string = "x"
    var inline1148 Exp = Lam{
        _0: inline1147,
        _1: t801,
    }
    t802 = inline1148
    var t803 Result__Typ__string = typeof(st__141, t795, t802)
    var inline1134 string = "fun_x_let_y_let_z_x_id_z_y"
    switch t803.(type) {
    case Result__Typ__string_Ok:
        var inline1135 Typ = t803.(Result__Typ__string_Ok)._0
        var inline1137 string = inline1134 + ": "
        var inline1138 string = typ_to_string(inline1135)
        var inline1139 string = inline1137 + inline1138
        println__T_string(inline1139)
    case Result__Typ__string_Err:
        var inline1141 string = t803.(Result__Typ__string_Err)._0
        var inline1143 string = inline1134 + ": "
        var inline1144 string = inline1143 + inline1141
        println__T_string(inline1144)
    default:
        panic("non-exhaustive match")
    }
    var inline1130 string = ""
    var inline1131 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline1130)
    _goml_runtime_core_string_println(inline1131)
    var inline1126 string = "All Done"
    var inline1127 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline1126)
    _goml_runtime_core_string_println(inline1127)
    var inline1122 string = ""
    var inline1123 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline1122)
    _goml_runtime_core_string_println(inline1123)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__259 *ref_int32_x, value__260 int32) struct{} {
    ref_set__Ref_5int32(self__259, value__260)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__258 *ref_int32_x) int32 {
    var t811 int32 = ref_get__Ref_5int32(self__258)
    return t811
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Tv(value__257 Tv) *ref_Tv_x {
    var t820 *ref_Tv_x = ref__Ref_2Tv(value__257)
    return t820
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__EnvEntry() *_goml_vec_EnvEntry {
    var t826 *_goml_vec_EnvEntry = vec_new__Vec_8EnvEntry()
    return t826
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SubstEntry(self__178 *_goml_vec_SubstEntry, elem__179 SubstEntry) *_goml_vec_SubstEntry {
    var t861 int
    var inline1193 int = vec_len__Vec_10SubstEntry(self__178)
    t861 = inline1193
    var t862 int = t861 + 1
    var result__180 *_goml_vec_SubstEntry
    var inline1191 *_goml_vec_SubstEntry = vec_with_capacity__Vec_10SubstEntry(t862)
    result__180 = inline1191
    var index__181 int = 0
    Loop_loop864:
    for {
        var t865 int
        var inline1187 int = vec_len__Vec_10SubstEntry(self__178)
        t865 = inline1187
        var t866 bool = index__181 < t865
        if t866 {
            var t867 SubstEntry = vec_get__Vec_10SubstEntry(self__178, index__181)
            vec_push__Vec_10SubstEntry(result__180, t867)
            var compound_old80 int = index__181
            var compound_value81 int = 1
            var t868 int = compound_old80 + compound_value81
            index__181 = t868
            continue
        } else {
            break Loop_loop864
        }
    }
    vec_push__Vec_10SubstEntry(result__180, elem__179)
    return result__180
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SubstEntry() *_goml_vec_SubstEntry {
    var t872 *_goml_vec_SubstEntry = vec_new__Vec_10SubstEntry()
    return t872
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__EnvEntry(self__178 *_goml_vec_EnvEntry, elem__179 EnvEntry) *_goml_vec_EnvEntry {
    var t875 int
    var inline1203 int = vec_len__Vec_8EnvEntry(self__178)
    t875 = inline1203
    var t876 int = t875 + 1
    var result__180 *_goml_vec_EnvEntry
    var inline1201 *_goml_vec_EnvEntry = vec_with_capacity__Vec_8EnvEntry(t876)
    result__180 = inline1201
    var index__181 int = 0
    Loop_loop878:
    for {
        var t879 int
        var inline1197 int = vec_len__Vec_8EnvEntry(self__178)
        t879 = inline1197
        var t880 bool = index__181 < t879
        if t880 {
            var t881 EnvEntry = vec_get__Vec_8EnvEntry(self__178, index__181)
            vec_push__Vec_8EnvEntry(result__180, t881)
            var compound_old80 int = index__181
            var compound_value81 int = 1
            var t882 int = compound_old80 + compound_value81
            index__181 = t882
            continue
        } else {
            break Loop_loop878
        }
    }
    vec_push__Vec_8EnvEntry(result__180, elem__179)
    return result__180
}

func println__T_string(value__31 string) struct{} {
    var t885 string
    t885 = value__31
    _goml_runtime_core_string_println(t885)
    return struct{}{}
}

func char_to_string(value__29 rune) string {
    var t891 uint32 = uint32(rune(value__29))
    var t892 bool
    var inline1206 bool = t891 <= 1114111
    if inline1206 {
        var inline1207 bool = t891 >= 55296
        var inline1209 bool
        if inline1207 {
            var inline1211 bool = t891 <= 57343
            inline1209 = inline1211
        } else {
            inline1209 = false
        }
        var inline1210 bool = !inline1209
        t892 = inline1210
    } else {
        t892 = false
    }
    if t892 {
        var t893 string = _goml_runtime_core_char_to_string(value__29)
        return t893
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
