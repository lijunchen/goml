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
    var t334 *ref_int32_x
    var inline906 int32 = 0
    var inline907 *ref_int32_x = ref__Ref_5int32(inline906)
    t334 = inline907
    var t335 *ref_int32_x
    var inline903 int32 = 1
    var inline904 *ref_int32_x = ref__Ref_5int32(inline903)
    t335 = inline904
    var t336 CheckerState = CheckerState{
        gensym_counter: t334,
        current_level: t335,
    }
    return t336
}

func reset_type_variables(st__2 CheckerState) struct{} {
    var inline918 *ref_int32_x = st__2.gensym_counter
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline918, 0)
    var inline915 *ref_int32_x = st__2.current_level
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline915, 1)
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
    var t362 *ref_int32_x = st__10.gensym_counter
    var n__11 int32
    var inline935 int32 = ref_get__Ref_5int32(t362)
    n__11 = inline935
    var t363 *ref_int32_x = st__10.gensym_counter
    var t364 int32 = n__11 + 1
    ref_set__Ref_5int32(t363, t364)
    var t367 bool = n__11 < 26
    if t367 {
        var t368 rune = nth_letter(n__11)
        var inline929 string = char_to_string(t368)
        return inline929
    } else {
        var t370 string
        var inline931 string = _goml_runtime_core_int32_to_string(n__11)
        t370 = inline931
        var t371 string = "t" + t370
        return t371
    }
}

func typ_is_arrow(ty__15 Typ) bool {
    switch ty__15.(type) {
    case TVar:
        var x142 *ref_Tv_x = ty__15.(TVar)._0
        var mtmp146 Tv
        var inline952 Tv = ref_get__Ref_2Tv(x142)
        mtmp146 = inline952
        switch mtmp146.(type) {
        case Link:
            var x149 Typ = mtmp146.(Link)._0
            var t384 bool = typ_is_arrow(x149)
            return t384
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
        var x150 *ref_Tv_x = ty__18.(TVar)._0
        var mtmp154 Tv
        var inline954 Tv = ref_get__Ref_2Tv(x150)
        mtmp154 = inline954
        switch mtmp154.(type) {
        case Unbound:
            var x155 string = mtmp154.(Unbound)._0
            var t391 string = "'" + x155
            return t391
        case Link:
            var x157 Typ = mtmp154.(Link)._0
            var t392 string = typ_to_string(x157)
            return t392
        default:
            panic("non-exhaustive match")
        }
    case QVar:
        var x151 string = ty__18.(QVar)._0
        var t393 string = "'" + x151
        return t393
    case TArrow:
        var x152 Typ = ty__18.(TArrow)._0
        var x153 Typ = ty__18.(TArrow)._1
        var t398 bool = typ_is_arrow(x152)
        var jp395 string
        if t398 {
            var t399 string = typ_to_string(x152)
            var t400 string = "(" + t399
            var t401 string = t400 + ")"
            jp395 = t401
        } else {
            var t402 string = typ_to_string(x152)
            jp395 = t402
        }
        var s2__26 string = typ_to_string(x153)
        var t396 string = jp395 + " -> "
        var t397 string = t396 + s2__26
        return t397
    default:
        panic("non-exhaustive match")
    }
}

func env_empty() *_goml_vec_EnvEntry {
    var inline956 *_goml_vec_EnvEntry = vec_new__Vec_8EnvEntry()
    return inline956
}

func env_lookup(env__28 *_goml_vec_EnvEntry, name__29 string) Option__Typ {
    var t407 int
    var inline984 int = vec_len__Vec_8EnvEntry(env__28)
    t407 = inline984
    var t408 int = t407 - 1
    var i__30 *ref_int_x
    var inline982 *ref_int_x = ref__Ref_3int(t408)
    i__30 = inline982
    var found__31 *ref_Option__Typ_x
    var inline980 *ref_Option__Typ_x = ref__Ref_11Option__Typ(None{})
    found__31 = inline980
    var done__32 *ref_bool_x
    var inline977 bool = false
    var inline978 *ref_bool_x = ref__Ref_4bool(inline977)
    done__32 = inline978
    Loop_loop411:
    for {
        var t424 bool
        var inline973 bool = ref_get__Ref_4bool(done__32)
        t424 = inline973
        var t425 bool = !t424
        var jp413 bool
        if t425 {
            var t426 int
            var inline958 int = ref_get__Ref_3int(i__30)
            t426 = inline958
            var t427 bool = t426 >= 0
            jp413 = t427
        } else {
            jp413 = false
        }
        if jp413 {
            var t414 int
            var inline971 int = ref_get__Ref_3int(i__30)
            t414 = inline971
            var entry__33 EnvEntry = vec_get__Vec_8EnvEntry(env__28, t414)
            var t416 string = entry__33.name
            var t417 bool
            var inline969 bool = t416 == name__29
            t417 = inline969
            if t417 {
                var t418 Typ = entry__33.ty
                var t419 Option__Typ = Some{
                    _0: t418,
                }
                ref_set__Ref_11Option__Typ(found__31, t419)
                var inline960 bool = true
                ref_set__Ref_4bool(done__32, inline960)
                continue
            } else {
                var t421 int
                var inline967 int = ref_get__Ref_3int(i__30)
                t421 = inline967
                var t422 int = t421 - 1
                ref_set__Ref_3int(i__30, t422)
                continue
            }
        } else {
            break Loop_loop411
        }
    }
    var inline975 Option__Typ = ref_get__Ref_11Option__Typ(found__31)
    return inline975
}

func subst_lookup(subst__34 *_goml_vec_SubstEntry, name__35 string) Option__Typ {
    var t430 int
    var inline1012 int = vec_len__Vec_10SubstEntry(subst__34)
    t430 = inline1012
    var t431 int = t430 - 1
    var i__36 *ref_int_x
    var inline1010 *ref_int_x = ref__Ref_3int(t431)
    i__36 = inline1010
    var found__37 *ref_Option__Typ_x
    var inline1008 *ref_Option__Typ_x = ref__Ref_11Option__Typ(None{})
    found__37 = inline1008
    var done__38 *ref_bool_x
    var inline1005 bool = false
    var inline1006 *ref_bool_x = ref__Ref_4bool(inline1005)
    done__38 = inline1006
    Loop_loop434:
    for {
        var t447 bool
        var inline1001 bool = ref_get__Ref_4bool(done__38)
        t447 = inline1001
        var t448 bool = !t447
        var jp436 bool
        if t448 {
            var t449 int
            var inline986 int = ref_get__Ref_3int(i__36)
            t449 = inline986
            var t450 bool = t449 >= 0
            jp436 = t450
        } else {
            jp436 = false
        }
        if jp436 {
            var t437 int
            var inline999 int = ref_get__Ref_3int(i__36)
            t437 = inline999
            var entry__39 SubstEntry = vec_get__Vec_10SubstEntry(subst__34, t437)
            var t439 string = entry__39.name
            var t440 bool
            var inline997 bool = t439 == name__35
            t440 = inline997
            if t440 {
                var t441 Typ = entry__39.ty
                var t442 Option__Typ = Some{
                    _0: t441,
                }
                ref_set__Ref_11Option__Typ(found__37, t442)
                var inline988 bool = true
                ref_set__Ref_4bool(done__38, inline988)
                continue
            } else {
                var t444 int
                var inline995 int = ref_get__Ref_3int(i__36)
                t444 = inline995
                var t445 int = t444 - 1
                ref_set__Ref_3int(i__36, t445)
                continue
            }
        } else {
            break Loop_loop434
        }
    }
    var inline1003 Option__Typ = ref_get__Ref_11Option__Typ(found__37)
    return inline1003
}

func occurs(st__40 CheckerState, tvr__41 *ref_Tv_x, ty__42 Typ) Result__unit__string {
    switch ty__42.(type) {
    case TVar:
        var x162 *ref_Tv_x = ty__42.(TVar)._0
        var t457 bool = ptr_eq__Ref_2Tv(tvr__41, x162)
        if t457 {
            var t458 Result__unit__string = Result__unit__string_Err{
                _0: "occurs check",
            }
            return t458
        } else {
            var mtmp166 Tv
            var inline1020 Tv = ref_get__Ref_2Tv(x162)
            mtmp166 = inline1020
            switch mtmp166.(type) {
            case Unbound:
                var x167 string = mtmp166.(Unbound)._0
                var x168 int32 = mtmp166.(Unbound)._1
                var mtmp170 Tv
                var inline1018 Tv = ref_get__Ref_2Tv(tvr__41)
                mtmp170 = inline1018
                var jp462 int32
                switch mtmp170.(type) {
                case Unbound:
                    var x172 int32 = mtmp170.(Unbound)._1
                    var inline1014 bool = x172 < x168
                    if inline1014 {
                        jp462 = x172
                    } else {
                        jp462 = x168
                    }
                default:
                    jp462 = x168
                }
                var t463 Tv = Unbound{
                    _0: x167,
                    _1: jp462,
                }
                ref_set__Ref_2Tv(x162, t463)
                var t464 Result__unit__string = Result__unit__string_Ok{
                    _0: struct{}{},
                }
                return t464
            case Link:
                var x169 Typ = mtmp166.(Link)._0
                var t466 Result__unit__string = occurs(st__40, tvr__41, x169)
                return t466
            default:
                panic("non-exhaustive match")
            }
        }
    case TArrow:
        var x164 Typ = ty__42.(TArrow)._0
        var x165 Typ = ty__42.(TArrow)._1
        var mtmp175 Result__unit__string = occurs(st__40, tvr__41, x164)
        switch mtmp175.(type) {
        case Result__unit__string_Ok:
            var t469 Result__unit__string = occurs(st__40, tvr__41, x165)
            return t469
        case Result__unit__string_Err:
            var x177 string = mtmp175.(Result__unit__string_Err)._0
            var t470 Result__unit__string = Result__unit__string_Err{
                _0: x177,
            }
            return t470
        default:
            panic("non-exhaustive match")
        }
    default:
        var t471 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        return t471
    }
}

func unify(st__52 CheckerState, t1__53 Typ, t2__54 Typ) Result__unit__string {
    switch t2__54.(type) {
    case TVar:
        var x181 *ref_Tv_x = t2__54.(TVar)._0
        switch t1__53.(type) {
        case TVar:
            var x185 *ref_Tv_x = t1__53.(TVar)._0
            var t480 bool = ptr_eq__Ref_2Tv(x185, x181)
            if t480 {
                var t481 Result__unit__string = Result__unit__string_Ok{
                    _0: struct{}{},
                }
                return t481
            } else {
                var mtmp189 Tv
                var inline1026 Tv = ref_get__Ref_2Tv(x185)
                mtmp189 = inline1026
                switch mtmp189.(type) {
                case Unbound:
                    var mtmp193 Tv
                    var inline1024 Tv = ref_get__Ref_2Tv(x181)
                    mtmp193 = inline1024
                    switch mtmp193.(type) {
                    case Unbound:
                        var t486 Typ = TVar{
                            _0: x181,
                        }
                        var mtmp197 Result__unit__string = occurs(st__52, x185, t486)
                        switch mtmp197.(type) {
                        case Result__unit__string_Ok:
                            var t489 Typ = TVar{
                                _0: x181,
                            }
                            var t490 Tv = Link{
                                _0: t489,
                            }
                            ref_set__Ref_2Tv(x185, t490)
                            var t491 Result__unit__string = Result__unit__string_Ok{
                                _0: struct{}{},
                            }
                            return t491
                        case Result__unit__string_Err:
                            var x199 string = mtmp197.(Result__unit__string_Err)._0
                            var t492 Result__unit__string = Result__unit__string_Err{
                                _0: x199,
                            }
                            return t492
                        default:
                            panic("non-exhaustive match")
                        }
                    case Link:
                        var x196 Typ = mtmp193.(Link)._0
                        var t493 Typ = TVar{
                            _0: x185,
                        }
                        var t494 Result__unit__string = unify(st__52, t493, x196)
                        return t494
                    default:
                        panic("non-exhaustive match")
                    }
                case Link:
                    var x192 Typ = mtmp189.(Link)._0
                    var t495 Typ = TVar{
                        _0: x181,
                    }
                    var t496 Result__unit__string = unify(st__52, x192, t495)
                    return t496
                default:
                    panic("non-exhaustive match")
                }
            }
        default:
            var mtmp201 Tv
            var inline1030 Tv = ref_get__Ref_2Tv(x181)
            mtmp201 = inline1030
            switch mtmp201.(type) {
            case Unbound:
                var mtmp205 Result__unit__string = occurs(st__52, x181, t1__53)
                switch mtmp205.(type) {
                case Result__unit__string_Ok:
                    var t501 Tv = Link{
                        _0: t1__53,
                    }
                    ref_set__Ref_2Tv(x181, t501)
                    var t502 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    return t502
                case Result__unit__string_Err:
                    var x207 string = mtmp205.(Result__unit__string_Err)._0
                    var t503 Result__unit__string = Result__unit__string_Err{
                        _0: x207,
                    }
                    return t503
                default:
                    panic("non-exhaustive match")
                }
            case Link:
                var x204 Typ = mtmp201.(Link)._0
                var t504 Result__unit__string = unify(st__52, t1__53, x204)
                return t504
            default:
                panic("non-exhaustive match")
            }
        }
    case TArrow:
        var x183 Typ = t2__54.(TArrow)._0
        var x184 Typ = t2__54.(TArrow)._1
        switch t1__53.(type) {
        case TVar:
            var x209 *ref_Tv_x = t1__53.(TVar)._0
            var mtmp213 Tv
            var inline1034 Tv = ref_get__Ref_2Tv(x209)
            mtmp213 = inline1034
            switch mtmp213.(type) {
            case Unbound:
                var mtmp217 Result__unit__string = occurs(st__52, x209, t2__54)
                switch mtmp217.(type) {
                case Result__unit__string_Ok:
                    var t511 Tv = Link{
                        _0: t2__54,
                    }
                    ref_set__Ref_2Tv(x209, t511)
                    var t512 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    return t512
                case Result__unit__string_Err:
                    var x219 string = mtmp217.(Result__unit__string_Err)._0
                    var t513 Result__unit__string = Result__unit__string_Err{
                        _0: x219,
                    }
                    return t513
                default:
                    panic("non-exhaustive match")
                }
            case Link:
                var x216 Typ = mtmp213.(Link)._0
                var t514 Result__unit__string = unify(st__52, x216, t2__54)
                return t514
            default:
                panic("non-exhaustive match")
            }
        case TArrow:
            var x211 Typ = t1__53.(TArrow)._0
            var x212 Typ = t1__53.(TArrow)._1
            var mtmp221 Result__unit__string = unify(st__52, x211, x183)
            switch mtmp221.(type) {
            case Result__unit__string_Ok:
                var t517 Result__unit__string = unify(st__52, x212, x184)
                return t517
            case Result__unit__string_Err:
                var x223 string = mtmp221.(Result__unit__string_Err)._0
                var t518 Result__unit__string = Result__unit__string_Err{
                    _0: x223,
                }
                return t518
            default:
                panic("non-exhaustive match")
            }
        default:
            var t519 Result__unit__string = Result__unit__string_Err{
                _0: "unify error",
            }
            return t519
        }
    default:
        switch t1__53.(type) {
        case TVar:
            var x224 *ref_Tv_x = t1__53.(TVar)._0
            var mtmp228 Tv
            var inline1038 Tv = ref_get__Ref_2Tv(x224)
            mtmp228 = inline1038
            switch mtmp228.(type) {
            case Unbound:
                var mtmp232 Result__unit__string = occurs(st__52, x224, t2__54)
                switch mtmp232.(type) {
                case Result__unit__string_Ok:
                    var t526 Tv = Link{
                        _0: t2__54,
                    }
                    ref_set__Ref_2Tv(x224, t526)
                    var t527 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    return t527
                case Result__unit__string_Err:
                    var x234 string = mtmp232.(Result__unit__string_Err)._0
                    var t528 Result__unit__string = Result__unit__string_Err{
                        _0: x234,
                    }
                    return t528
                default:
                    panic("non-exhaustive match")
                }
            case Link:
                var x231 Typ = mtmp228.(Link)._0
                var t529 Result__unit__string = unify(st__52, x231, t2__54)
                return t529
            default:
                panic("non-exhaustive match")
            }
        default:
            var t530 Result__unit__string = Result__unit__string_Err{
                _0: "unify error",
            }
            return t530
        }
    }
}

func gen(st__73 CheckerState, ty__74 Typ) Typ {
    switch ty__74.(type) {
    case TVar:
        var x236 *ref_Tv_x = ty__74.(TVar)._0
        var mtmp240 Tv
        var inline1042 Tv = ref_get__Ref_2Tv(x236)
        mtmp240 = inline1042
        switch mtmp240.(type) {
        case Unbound:
            var x241 string = mtmp240.(Unbound)._0
            var x242 int32 = mtmp240.(Unbound)._1
            var t537 *ref_int32_x = st__73.current_level
            var cur__78 int32
            var inline1040 int32 = ref_get__Ref_5int32(t537)
            cur__78 = inline1040
            var t540 bool = x242 > cur__78
            if t540 {
                var t541 Typ = QVar{
                    _0: x241,
                }
                return t541
            } else {
                var t542 Typ = TVar{
                    _0: x236,
                }
                return t542
            }
        case Link:
            var x243 Typ = mtmp240.(Link)._0
            var t543 Typ = gen(st__73, x243)
            return t543
        default:
            panic("non-exhaustive match")
        }
    case TArrow:
        var x238 Typ = ty__74.(TArrow)._0
        var x239 Typ = ty__74.(TArrow)._1
        var t544 Typ = gen(st__73, x238)
        var t545 Typ = gen(st__73, x239)
        var t546 Typ = TArrow{
            _0: t544,
            _1: t545,
        }
        return t546
    default:
        return ty__74
    }
}

func inst_loop(st__83 CheckerState, subst__84 *_goml_vec_SubstEntry, ty__85 Typ) Tuple2_3Typ_16Vec_10SubstEntry {
    switch ty__85.(type) {
    case TVar:
        var x244 *ref_Tv_x = ty__85.(TVar)._0
        var mtmp248 Tv
        var inline1044 Tv = ref_get__Ref_2Tv(x244)
        mtmp248 = inline1044
        switch mtmp248.(type) {
        case Link:
            var x251 Typ = mtmp248.(Link)._0
            var t553 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, subst__84, x251)
            return t553
        default:
            var t554 Typ = TVar{
                _0: x244,
            }
            var t555 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: t554,
                _1: subst__84,
            }
            return t555
        }
    case QVar:
        var x245 string = ty__85.(QVar)._0
        var mtmp252 Option__Typ = subst_lookup(subst__84, x245)
        switch mtmp252.(type) {
        case None:
            var tv__88 Typ
            var inline1046 string = gensym(st__83)
            var inline1047 *ref_int32_x = st__83.current_level
            var inline1048 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline1047)
            var inline1049 Tv = Unbound{
                _0: inline1046,
                _1: inline1048,
            }
            var inline1050 *ref_Tv_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Tv(inline1049)
            var inline1051 Typ = TVar{
                _0: inline1050,
            }
            tv__88 = inline1051
            var t558 SubstEntry = SubstEntry{
                name: x245,
                ty: tv__88,
            }
            var new_subst__89 *_goml_vec_SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SubstEntry(subst__84, t558)
            var t559 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: tv__88,
                _1: new_subst__89,
            }
            return t559
        case Some:
            var x253 Typ = mtmp252.(Some)._0
            var t560 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: x253,
                _1: subst__84,
            }
            return t560
        default:
            panic("non-exhaustive match")
        }
    case TArrow:
        var x246 Typ = ty__85.(TArrow)._0
        var x247 Typ = ty__85.(TArrow)._1
        var mtmp254 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, subst__84, x246)
        var x255 Typ = mtmp254._0
        var x256 *_goml_vec_SubstEntry = mtmp254._1
        var mtmp257 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, x256, x247)
        var x258 Typ = mtmp257._0
        var x259 *_goml_vec_SubstEntry = mtmp257._1
        var t561 Typ = TArrow{
            _0: x255,
            _1: x258,
        }
        var t562 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
            _0: t561,
            _1: x259,
        }
        return t562
    default:
        panic("non-exhaustive match")
    }
}

func typeof(st__102 CheckerState, env__103 *_goml_vec_EnvEntry, e__104 Exp) Result__Typ__string {
    switch e__104.(type) {
    case Var:
        var x263 string = e__104.(Var)._0
        var mtmp271 Option__Typ = env_lookup(env__103, x263)
        switch mtmp271.(type) {
        case None:
            var t571 Result__Typ__string = Result__Typ__string_Err{
                _0: "unbound var",
            }
            return t571
        case Some:
            var x272 Typ = mtmp271.(Some)._0
            var t572 Typ
            var inline1055 *_goml_vec_SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SubstEntry()
            var inline1056 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__102, inline1055, x272)
            var inline1057 Typ = inline1056._0
            t572 = inline1057
            var t573 Result__Typ__string = Result__Typ__string_Ok{
                _0: t572,
            }
            return t573
        default:
            panic("non-exhaustive match")
        }
    case App:
        var x264 Exp = e__104.(App)._0
        var x265 Exp = e__104.(App)._1
        var mtmp273 Result__Typ__string = typeof(st__102, env__103, x264)
        switch mtmp273.(type) {
        case Result__Typ__string_Ok:
            var x274 Typ = mtmp273.(Result__Typ__string_Ok)._0
            var mtmp276 Result__Typ__string = typeof(st__102, env__103, x265)
            switch mtmp276.(type) {
            case Result__Typ__string_Ok:
                var x277 Typ = mtmp276.(Result__Typ__string_Ok)._0
                var ty_res__119 Typ
                var inline1061 string = gensym(st__102)
                var inline1062 *ref_int32_x = st__102.current_level
                var inline1063 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline1062)
                var inline1064 Tv = Unbound{
                    _0: inline1061,
                    _1: inline1063,
                }
                var inline1065 *ref_Tv_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Tv(inline1064)
                var inline1066 Typ = TVar{
                    _0: inline1065,
                }
                ty_res__119 = inline1066
                var arrow__120 Typ = TArrow{
                    _0: x277,
                    _1: ty_res__119,
                }
                var mtmp279 Result__unit__string = unify(st__102, x274, arrow__120)
                switch mtmp279.(type) {
                case Result__unit__string_Ok:
                    var t580 Result__Typ__string = Result__Typ__string_Ok{
                        _0: ty_res__119,
                    }
                    return t580
                case Result__unit__string_Err:
                    var x281 string = mtmp279.(Result__unit__string_Err)._0
                    var t581 Result__Typ__string = Result__Typ__string_Err{
                        _0: x281,
                    }
                    return t581
                default:
                    panic("non-exhaustive match")
                }
            case Result__Typ__string_Err:
                var x278 string = mtmp276.(Result__Typ__string_Err)._0
                var t582 Result__Typ__string = Result__Typ__string_Err{
                    _0: x278,
                }
                return t582
            default:
                panic("non-exhaustive match")
            }
        case Result__Typ__string_Err:
            var x275 string = mtmp273.(Result__Typ__string_Err)._0
            var t583 Result__Typ__string = Result__Typ__string_Err{
                _0: x275,
            }
            return t583
        default:
            panic("non-exhaustive match")
        }
    case Lam:
        var x266 string = e__104.(Lam)._0
        var x267 Exp = e__104.(Lam)._1
        var ty_x__109 Typ
        var inline1068 string = gensym(st__102)
        var inline1069 *ref_int32_x = st__102.current_level
        var inline1070 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline1069)
        var inline1071 Tv = Unbound{
            _0: inline1068,
            _1: inline1070,
        }
        var inline1072 *ref_Tv_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Tv(inline1071)
        var inline1073 Typ = TVar{
            _0: inline1072,
        }
        ty_x__109 = inline1073
        var t584 EnvEntry = EnvEntry{
            name: x266,
            ty: ty_x__109,
        }
        var env2__110 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__EnvEntry(env__103, t584)
        var mtmp282 Result__Typ__string = typeof(st__102, env2__110, x267)
        switch mtmp282.(type) {
        case Result__Typ__string_Ok:
            var x283 Typ = mtmp282.(Result__Typ__string_Ok)._0
            var t587 Typ = TArrow{
                _0: ty_x__109,
                _1: x283,
            }
            var t588 Result__Typ__string = Result__Typ__string_Ok{
                _0: t587,
            }
            return t588
        case Result__Typ__string_Err:
            var x284 string = mtmp282.(Result__Typ__string_Err)._0
            var t589 Result__Typ__string = Result__Typ__string_Err{
                _0: x284,
            }
            return t589
        default:
            panic("non-exhaustive match")
        }
    case Let:
        var x268 string = e__104.(Let)._0
        var x269 Exp = e__104.(Let)._1
        var x270 Exp = e__104.(Let)._2
        var inline1081 *ref_int32_x = st__102.current_level
        var inline1082 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline1081)
        var inline1083 *ref_int32_x = st__102.current_level
        var inline1084 int32 = inline1082 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline1083, inline1084)
        var ty_e__125 Result__Typ__string = typeof(st__102, env__103, x269)
        var inline1075 *ref_int32_x = st__102.current_level
        var inline1076 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline1075)
        var inline1077 *ref_int32_x = st__102.current_level
        var inline1078 int32 = inline1076 - 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline1077, inline1078)
        switch ty_e__125.(type) {
        case Result__Typ__string_Ok:
            var x287 Typ = ty_e__125.(Result__Typ__string_Ok)._0
            var t592 Typ = gen(st__102, x287)
            var t593 EnvEntry = EnvEntry{
                name: x268,
                ty: t592,
            }
            var env2__128 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__EnvEntry(env__103, t593)
            var t594 Result__Typ__string = typeof(st__102, env2__128, x270)
            return t594
        case Result__Typ__string_Err:
            var x288 string = ty_e__125.(Result__Typ__string_Err)._0
            var t595 Result__Typ__string = Result__Typ__string_Err{
                _0: x288,
            }
            return t595
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func exp_var(name__129 string) Exp {
    var t598 Exp = Var{
        _0: name__129,
    }
    return t598
}

func exp_lam(name__130 string, body__131 Exp) Exp {
    var t601 Exp = Lam{
        _0: name__130,
        _1: body__131,
    }
    return t601
}

func exp_app(a__132 Exp, b__133 Exp) Exp {
    var t604 Exp = App{
        _0: a__132,
        _1: b__133,
    }
    return t604
}

func exp_let(name__134 string, a__135 Exp, b__136 Exp) Exp {
    var t607 Exp = Let{
        _0: name__134,
        _1: a__135,
        _2: b__136,
    }
    return t607
}

func show_result(label__137 string, res__138 Result__Typ__string) struct{} {
    switch res__138.(type) {
    case Result__Typ__string_Ok:
        var x289 Typ = res__138.(Result__Typ__string_Ok)._0
        var t610 string = label__137 + ": "
        var t611 string = typ_to_string(x289)
        var t612 string = t610 + t611
        var inline1087 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t612)
        _goml_runtime_core_string_println(inline1087)
        return struct{}{}
    case Result__Typ__string_Err:
        var x290 string = res__138.(Result__Typ__string_Err)._0
        var t614 string = label__137 + ": "
        var t615 string = t614 + x290
        var inline1090 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t615)
        _goml_runtime_core_string_println(inline1090)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var st__141 CheckerState = state_new()
    var t618 Exp = exp_var("x")
    var id__142 Exp = exp_lam("x", t618)
    var t619 Exp = exp_var("x")
    var t620 Exp = exp_var("y")
    var t621 Exp = exp_app(t619, t620)
    var t622 Exp = exp_lam("y", t621)
    var c1__143 Exp = exp_lam("x", t622)
    reset_type_variables(st__141)
    var t623 *_goml_vec_EnvEntry = env_empty()
    var t624 Result__Typ__string = typeof(st__141, t623, id__142)
    show_result("id", t624)
    reset_type_variables(st__141)
    var t625 *_goml_vec_EnvEntry = env_empty()
    var t626 Result__Typ__string = typeof(st__141, t625, c1__143)
    show_result("c1", t626)
    reset_type_variables(st__141)
    var t627 *_goml_vec_EnvEntry = env_empty()
    var t628 Exp = exp_var("x")
    var t629 Exp = exp_let("x", c1__143, t628)
    var t630 Result__Typ__string = typeof(st__141, t627, t629)
    show_result("let_x_c1_x", t630)
    reset_type_variables(st__141)
    var t631 *_goml_vec_EnvEntry = env_empty()
    var t632 Exp = exp_var("z")
    var t633 Exp = exp_lam("z", t632)
    var t634 Exp = exp_var("y")
    var t635 Exp = exp_let("y", t633, t634)
    var t636 Result__Typ__string = typeof(st__141, t631, t635)
    show_result("let_y_id_y", t636)
    reset_type_variables(st__141)
    var t637 *_goml_vec_EnvEntry = env_empty()
    var t638 Exp = exp_var("z")
    var t639 Exp = exp_lam("z", t638)
    var t640 Exp = exp_var("y")
    var t641 Exp = exp_let("y", t639, t640)
    var t642 Exp = exp_lam("x", t641)
    var t643 Result__Typ__string = typeof(st__141, t637, t642)
    show_result("lam_x_let_y_id_y", t643)
    reset_type_variables(st__141)
    var t644 *_goml_vec_EnvEntry = env_empty()
    var t645 Exp = exp_var("z")
    var t646 Exp = exp_lam("z", t645)
    var t647 Exp = exp_var("y")
    var t648 Exp = exp_var("x")
    var t649 Exp = exp_app(t647, t648)
    var t650 Exp = exp_let("y", t646, t649)
    var t651 Exp = exp_lam("x", t650)
    var t652 Result__Typ__string = typeof(st__141, t644, t651)
    show_result("lam_x_let_y_id_yx", t652)
    reset_type_variables(st__141)
    var t653 *_goml_vec_EnvEntry = env_empty()
    var t654 Exp = exp_var("x")
    var t655 Exp = exp_var("x")
    var t656 Exp = exp_app(t654, t655)
    var t657 Exp = exp_lam("x", t656)
    var t658 Result__Typ__string = typeof(st__141, t653, t657)
    show_result("self_apply", t658)
    reset_type_variables(st__141)
    var t659 *_goml_vec_EnvEntry = env_empty()
    var t660 Exp = exp_var("x")
    var t661 Exp = exp_var("x")
    var t662 Exp = exp_let("x", t660, t661)
    var t663 Result__Typ__string = typeof(st__141, t659, t662)
    show_result("unbound_var", t663)
    reset_type_variables(st__141)
    var t664 *_goml_vec_EnvEntry = env_empty()
    var t665 Exp = exp_var("y")
    var t666 Exp = exp_var("y")
    var t667 Exp = exp_var("z")
    var t668 Exp = exp_app(t666, t667)
    var t669 Exp = exp_lam("z", t668)
    var t670 Exp = exp_app(t665, t669)
    var t671 Exp = exp_lam("y", t670)
    var t672 Result__Typ__string = typeof(st__141, t664, t671)
    show_result("max_heiber", t672)
    reset_type_variables(st__141)
    var t673 *_goml_vec_EnvEntry = env_empty()
    var t674 Exp = exp_var("k")
    var t675 Exp = exp_var("k")
    var t676 Exp = exp_var("x")
    var t677 Exp = exp_app(t675, t676)
    var t678 Exp = exp_var("y")
    var t679 Exp = exp_app(t677, t678)
    var t680 Exp = exp_app(t674, t679)
    var t681 Exp = exp_var("k")
    var t682 Exp = exp_var("y")
    var t683 Exp = exp_app(t681, t682)
    var t684 Exp = exp_var("x")
    var t685 Exp = exp_app(t683, t684)
    var t686 Exp = exp_app(t680, t685)
    var t687 Exp = exp_lam("k", t686)
    var t688 Exp = exp_lam("y", t687)
    var t689 Exp = exp_lam("x", t688)
    var t690 Result__Typ__string = typeof(st__141, t673, t689)
    show_result("kirang", t690)
    reset_type_variables(st__141)
    var t691 *_goml_vec_EnvEntry = env_empty()
    var t692 Exp = exp_var("id")
    var t693 Exp = exp_var("id")
    var t694 Exp = exp_app(t692, t693)
    var t695 Exp = exp_let("id", id__142, t694)
    var t696 Result__Typ__string = typeof(st__141, t691, t695)
    show_result("let_id_idid", t696)
    reset_type_variables(st__141)
    var t697 *_goml_vec_EnvEntry = env_empty()
    var t698 Exp = exp_var("x")
    var t699 Exp = exp_app(t698, id__142)
    var t700 Exp = exp_var("z")
    var t701 Exp = exp_let("z", t699, t700)
    var t702 Exp = exp_var("y")
    var t703 Exp = exp_let("y", t701, t702)
    var t704 Exp = exp_let("x", c1__143, t703)
    var t705 Result__Typ__string = typeof(st__141, t697, t704)
    show_result("nested_lets", t705)
    reset_type_variables(st__141)
    var t706 *_goml_vec_EnvEntry = env_empty()
    var t707 Exp = exp_var("x")
    var t708 Exp = exp_var("y")
    var t709 Exp = exp_app(t707, t708)
    var t710 Exp = exp_var("y")
    var t711 Exp = exp_var("x")
    var t712 Exp = exp_app(t710, t711)
    var t713 Exp = exp_lam("x", t712)
    var t714 Exp = exp_let("x", t709, t713)
    var t715 Exp = exp_lam("y", t714)
    var t716 Exp = exp_lam("x", t715)
    var t717 Result__Typ__string = typeof(st__141, t706, t716)
    show_result("fun_x_fun_y_let_x_xy_fun_x_yx", t717)
    reset_type_variables(st__141)
    var t718 *_goml_vec_EnvEntry = env_empty()
    var t719 Exp = exp_var("x")
    var t720 Exp = exp_var("y")
    var t721 Exp = exp_let("y", t719, t720)
    var t722 Exp = exp_lam("x", t721)
    var t723 Result__Typ__string = typeof(st__141, t718, t722)
    show_result("sound_gen_1", t723)
    reset_type_variables(st__141)
    var t724 *_goml_vec_EnvEntry = env_empty()
    var t725 Exp = exp_var("x")
    var t726 Exp = exp_lam("z", t725)
    var t727 Exp = exp_var("y")
    var t728 Exp = exp_let("y", t726, t727)
    var t729 Exp = exp_lam("x", t728)
    var t730 Result__Typ__string = typeof(st__141, t724, t729)
    show_result("sound_gen_2", t730)
    reset_type_variables(st__141)
    var t731 *_goml_vec_EnvEntry = env_empty()
    var t732 Exp = exp_var("x")
    var t733 Exp = exp_var("z")
    var t734 Exp = exp_app(t732, t733)
    var t735 Exp = exp_lam("z", t734)
    var t736 Exp = exp_var("y")
    var t737 Exp = exp_let("y", t735, t736)
    var t738 Exp = exp_lam("x", t737)
    var t739 Result__Typ__string = typeof(st__141, t731, t738)
    show_result("sound_gen_3", t739)
    reset_type_variables(st__141)
    var t740 *_goml_vec_EnvEntry = env_empty()
    var t741 Exp = exp_var("x")
    var t742 Exp = exp_var("y")
    var t743 Exp = exp_app(t741, t742)
    var t744 Exp = exp_var("x")
    var t745 Exp = exp_var("y")
    var t746 Exp = exp_app(t744, t745)
    var t747 Exp = exp_let("x", t743, t746)
    var t748 Exp = exp_lam("y", t747)
    var t749 Exp = exp_lam("x", t748)
    var t750 Result__Typ__string = typeof(st__141, t740, t749)
    show_result("double_apply", t750)
    reset_type_variables(st__141)
    var t751 *_goml_vec_EnvEntry = env_empty()
    var t752 Exp = exp_var("x")
    var t753 Exp = exp_var("y")
    var t754 Exp = exp_var("y")
    var t755 Exp
    var inline1149 Exp = App{
        _0: t753,
        _1: t754,
    }
    t755 = inline1149
    var t756 Exp
    var inline1146 string = "y"
    var inline1147 Exp = Let{
        _0: inline1146,
        _1: t752,
        _2: t755,
    }
    t756 = inline1147
    var t757 Exp
    var inline1143 string = "x"
    var inline1144 Exp = Lam{
        _0: inline1143,
        _1: t756,
    }
    t757 = inline1144
    var t758 Result__Typ__string = typeof(st__141, t751, t757)
    show_result("sound_gen_occurs", t758)
    var inline1140 *ref_int32_x = st__141.gensym_counter
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline1140, 0)
    var t759 *_goml_vec_EnvEntry
    var inline1138 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__EnvEntry()
    t759 = inline1138
    var t760 Exp
    var inline1135 string = "x"
    var inline1136 Exp = Var{
        _0: inline1135,
    }
    t760 = inline1136
    var t761 Exp
    var inline1133 Exp = App{
        _0: t760,
        _1: id__142,
    }
    t761 = inline1133
    var t762 Exp
    var inline1130 string = "z"
    var inline1131 Exp = Var{
        _0: inline1130,
    }
    t762 = inline1131
    var t763 Exp
    var inline1127 string = "z"
    var inline1128 Exp = Let{
        _0: inline1127,
        _1: t761,
        _2: t762,
    }
    t763 = inline1128
    var t764 Exp
    var inline1124 string = "y"
    var inline1125 Exp = Var{
        _0: inline1124,
    }
    t764 = inline1125
    var t765 Exp
    var inline1121 string = "y"
    var inline1122 Exp = Let{
        _0: inline1121,
        _1: t763,
        _2: t764,
    }
    t765 = inline1122
    var t766 Exp
    var inline1118 string = "x"
    var inline1119 Exp = Lam{
        _0: inline1118,
        _1: t765,
    }
    t766 = inline1119
    var t767 Result__Typ__string = typeof(st__141, t759, t766)
    var inline1105 string = "fun_x_let_y_let_z_x_id_z_y"
    switch t767.(type) {
    case Result__Typ__string_Ok:
        var inline1106 Typ = t767.(Result__Typ__string_Ok)._0
        var inline1108 string = inline1105 + ": "
        var inline1109 string = typ_to_string(inline1106)
        var inline1110 string = inline1108 + inline1109
        println__T_string(inline1110)
    case Result__Typ__string_Err:
        var inline1112 string = t767.(Result__Typ__string_Err)._0
        var inline1114 string = inline1105 + ": "
        var inline1115 string = inline1114 + inline1112
        println__T_string(inline1115)
    default:
        panic("non-exhaustive match")
    }
    var inline1101 string = ""
    var inline1102 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline1101)
    _goml_runtime_core_string_println(inline1102)
    var inline1097 string = "All Done"
    var inline1098 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline1097)
    _goml_runtime_core_string_println(inline1098)
    var inline1093 string = ""
    var inline1094 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline1093)
    _goml_runtime_core_string_println(inline1094)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__217 *ref_int32_x, value__218 int32) struct{} {
    ref_set__Ref_5int32(self__217, value__218)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__216 *ref_int32_x) int32 {
    var t775 int32 = ref_get__Ref_5int32(self__216)
    return t775
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Tv(value__215 Tv) *ref_Tv_x {
    var t784 *ref_Tv_x = ref__Ref_2Tv(value__215)
    return t784
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__EnvEntry() *_goml_vec_EnvEntry {
    var t790 *_goml_vec_EnvEntry = vec_new__Vec_8EnvEntry()
    return t790
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SubstEntry(self__136 *_goml_vec_SubstEntry, elem__137 SubstEntry) *_goml_vec_SubstEntry {
    var t828 int
    var inline1164 int = vec_len__Vec_10SubstEntry(self__136)
    t828 = inline1164
    var t829 int = t828 + 1
    var result__138 *_goml_vec_SubstEntry
    var inline1162 *_goml_vec_SubstEntry = vec_with_capacity__Vec_10SubstEntry(t829)
    result__138 = inline1162
    var index__139 int = 0
    Loop_loop831:
    for {
        var t832 int
        var inline1158 int = vec_len__Vec_10SubstEntry(self__136)
        t832 = inline1158
        var t833 bool = index__139 < t832
        if t833 {
            var t834 SubstEntry = vec_get__Vec_10SubstEntry(self__136, index__139)
            vec_push__Vec_10SubstEntry(result__138, t834)
            var compound_old44 int = index__139
            var compound_value45 int = 1
            var t835 int = compound_old44 + compound_value45
            index__139 = t835
            continue
        } else {
            break Loop_loop831
        }
    }
    vec_push__Vec_10SubstEntry(result__138, elem__137)
    return result__138
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SubstEntry() *_goml_vec_SubstEntry {
    var t839 *_goml_vec_SubstEntry = vec_new__Vec_10SubstEntry()
    return t839
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__EnvEntry(self__136 *_goml_vec_EnvEntry, elem__137 EnvEntry) *_goml_vec_EnvEntry {
    var t842 int
    var inline1174 int = vec_len__Vec_8EnvEntry(self__136)
    t842 = inline1174
    var t843 int = t842 + 1
    var result__138 *_goml_vec_EnvEntry
    var inline1172 *_goml_vec_EnvEntry = vec_with_capacity__Vec_8EnvEntry(t843)
    result__138 = inline1172
    var index__139 int = 0
    Loop_loop845:
    for {
        var t846 int
        var inline1168 int = vec_len__Vec_8EnvEntry(self__136)
        t846 = inline1168
        var t847 bool = index__139 < t846
        if t847 {
            var t848 EnvEntry = vec_get__Vec_8EnvEntry(self__136, index__139)
            vec_push__Vec_8EnvEntry(result__138, t848)
            var compound_old44 int = index__139
            var compound_value45 int = 1
            var t849 int = compound_old44 + compound_value45
            index__139 = t849
            continue
        } else {
            break Loop_loop845
        }
    }
    vec_push__Vec_8EnvEntry(result__138, elem__137)
    return result__138
}

func println__T_string(value__31 string) struct{} {
    var t852 string
    t852 = value__31
    _goml_runtime_core_string_println(t852)
    return struct{}{}
}

func char_to_string(value__29 rune) string {
    var t858 uint32 = uint32(rune(value__29))
    var t859 bool
    var inline1177 bool = t858 <= 1114111
    if inline1177 {
        var inline1178 bool = t858 >= 55296
        var inline1180 bool
        if inline1178 {
            var inline1182 bool = t858 <= 57343
            inline1180 = inline1182
        } else {
            inline1180 = false
        }
        var inline1181 bool = !inline1180
        t859 = inline1181
    } else {
        t859 = false
    }
    if t859 {
        var t860 string = _goml_runtime_core_char_to_string(value__29)
        return t860
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
