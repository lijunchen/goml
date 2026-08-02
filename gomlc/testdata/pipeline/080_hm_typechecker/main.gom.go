package main

import (
    _goml_fmt "fmt"
    _goml_slices "slices"
    _goml_utf8 "unicode/utf8"
)

func _goml_runtime_core_char_to_string(x rune) string {
    if !_goml_utf8.ValidRune(x) {
        panic("invalid char")
    }
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
    var t353 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var t354 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(1)
    var t355 CheckerState = CheckerState{
        gensym_counter: t353,
        current_level: t354,
    }
    return t355
}

func reset_gensym(st__0 CheckerState) struct{} {
    var t357 *ref_int32_x = st__0.gensym_counter
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t357, 0)
    return struct{}{}
}

func reset_level(st__1 CheckerState) struct{} {
    var t359 *ref_int32_x = st__1.current_level
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t359, 1)
    return struct{}{}
}

func reset_type_variables(st__2 CheckerState) struct{} {
    reset_gensym(st__2)
    reset_level(st__2)
    return struct{}{}
}

func enter_level(st__3 CheckerState) struct{} {
    var t363 *ref_int32_x = st__3.current_level
    var l__4 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t363)
    var t364 *ref_int32_x = st__3.current_level
    var t365 int32 = l__4 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t364, t365)
    return struct{}{}
}

func leave_level(st__5 CheckerState) struct{} {
    var t367 *ref_int32_x = st__5.current_level
    var l__6 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t367)
    var t368 *ref_int32_x = st__5.current_level
    var t369 int32 = l__6 - 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t368, t369)
    return struct{}{}
}

func min_i32(a__7 int32, b__8 int32) int32 {
    var t374 bool = a__7 < b__8
    if t374 {
        return a__7
    } else {
        return b__8
    }
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
    var t381 *ref_int32_x = st__10.gensym_counter
    var n__11 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t381)
    var t382 *ref_int32_x = st__10.gensym_counter
    var t383 int32 = n__11 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(t382, t383)
    var t386 bool = n__11 < 26
    if t386 {
        var t387 rune = nth_letter(n__11)
        var t388 string = _goml_m_inherent_i_char_i_char_i_to__string(t387)
        return t388
    } else {
        var t389 string = _goml_m_inherent_i_int32_i_int32_i_to__string(n__11)
        var t390 string = "t" + t389
        return t390
    }
}

func newvar(st__12 CheckerState) Typ {
    var name__13 string = gensym(st__12)
    var t393 *ref_int32_x = st__12.current_level
    var level__14 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t393)
    var t394 Tv = Unbound{
        _0: name__13,
        _1: level__14,
    }
    var t395 *ref_Tv_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Tv(t394)
    var t396 Typ = TVar{
        _0: t395,
    }
    return t396
}

func typ_is_arrow(ty__15 Typ) bool {
    switch ty__15.(type) {
    case TVar:
        var x161 *ref_Tv_x = ty__15.(TVar)._0
        var mtmp165 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(x161)
        switch mtmp165.(type) {
        case Link:
            var x168 Typ = mtmp165.(Link)._0
            var t403 bool = typ_is_arrow(x168)
            return t403
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
        var x169 *ref_Tv_x = ty__18.(TVar)._0
        var mtmp173 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(x169)
        switch mtmp173.(type) {
        case Unbound:
            var x174 string = mtmp173.(Unbound)._0
            var t410 string = "'" + x174
            return t410
        case Link:
            var x176 Typ = mtmp173.(Link)._0
            var t411 string = typ_to_string(x176)
            return t411
        default:
            panic("non-exhaustive match")
        }
    case QVar:
        var x170 string = ty__18.(QVar)._0
        var t412 string = "'" + x170
        return t412
    case TArrow:
        var x171 Typ = ty__18.(TArrow)._0
        var x172 Typ = ty__18.(TArrow)._1
        var t417 bool = typ_is_arrow(x171)
        var jp414 string
        if t417 {
            var t418 string = typ_to_string(x171)
            var t419 string = "(" + t418
            var t420 string = t419 + ")"
            jp414 = t420
        } else {
            var t421 string = typ_to_string(x171)
            jp414 = t421
        }
        var s2__26 string = typ_to_string(x172)
        var t415 string = jp414 + " -> "
        var t416 string = t415 + s2__26
        return t416
    default:
        panic("non-exhaustive match")
    }
}

func env_empty() *_goml_vec_EnvEntry {
    var env__27 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__EnvEntry()
    return env__27
}

func env_lookup(env__28 *_goml_vec_EnvEntry, name__29 string) Option__Typ {
    var t426 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__EnvEntry(env__28)
    var t427 int = t426 - 1
    var i__30 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(t427)
    var found__31 *ref_Option__Typ_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_Typ_r_(None{})
    var done__32 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    Loop_loop430:
    for {
        var t443 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(done__32)
        var t444 bool = !t443
        var jp432 bool
        if t444 {
            var t445 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__30)
            var t446 bool = t445 >= 0
            jp432 = t446
        } else {
            jp432 = false
        }
        if jp432 {
            var t433 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__30)
            var entry__33 EnvEntry = vec_get__Vec_8EnvEntry(env__28, t433)
            var t435 string = entry__33.name
            var t436 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(t435, name__29)
            if t436 {
                var t437 Typ = entry__33.ty
                var t438 Option__Typ = Some{
                    _0: t437,
                }
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Option_l_Typ_r_(found__31, t438)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(done__32, true)
                continue
            } else {
                var t440 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__30)
                var t441 int = t440 - 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__30, t441)
                continue
            }
        } else {
            break Loop_loop430
        }
    }
    var t429 Option__Typ = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Option_l_Typ_r_(found__31)
    return t429
}

func subst_lookup(subst__34 *_goml_vec_SubstEntry, name__35 string) Option__Typ {
    var t449 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SubstEntry(subst__34)
    var t450 int = t449 - 1
    var i__36 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(t450)
    var found__37 *ref_Option__Typ_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_Typ_r_(None{})
    var done__38 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    Loop_loop453:
    for {
        var t466 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(done__38)
        var t467 bool = !t466
        var jp455 bool
        if t467 {
            var t468 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__36)
            var t469 bool = t468 >= 0
            jp455 = t469
        } else {
            jp455 = false
        }
        if jp455 {
            var t456 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__36)
            var entry__39 SubstEntry = vec_get__Vec_10SubstEntry(subst__34, t456)
            var t458 string = entry__39.name
            var t459 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(t458, name__35)
            if t459 {
                var t460 Typ = entry__39.ty
                var t461 Option__Typ = Some{
                    _0: t460,
                }
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Option_l_Typ_r_(found__37, t461)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(done__38, true)
                continue
            } else {
                var t463 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__36)
                var t464 int = t463 - 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(i__36, t464)
                continue
            }
        } else {
            break Loop_loop453
        }
    }
    var t452 Option__Typ = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Option_l_Typ_r_(found__37)
    return t452
}

func occurs(st__40 CheckerState, tvr__41 *ref_Tv_x, ty__42 Typ) Result__unit__string {
    switch ty__42.(type) {
    case TVar:
        var x181 *ref_Tv_x = ty__42.(TVar)._0
        var t476 bool = ptr_eq__Ref_2Tv(tvr__41, x181)
        if t476 {
            var t477 Result__unit__string = Result__unit__string_Err{
                _0: "occurs check",
            }
            return t477
        } else {
            var mtmp185 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(x181)
            switch mtmp185.(type) {
            case Unbound:
                var x186 string = mtmp185.(Unbound)._0
                var x187 int32 = mtmp185.(Unbound)._1
                var mtmp189 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(tvr__41)
                var jp481 int32
                switch mtmp189.(type) {
                case Unbound:
                    var x191 int32 = mtmp189.(Unbound)._1
                    var t484 int32 = min_i32(x191, x187)
                    jp481 = t484
                default:
                    jp481 = x187
                }
                var t482 Tv = Unbound{
                    _0: x186,
                    _1: jp481,
                }
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(x181, t482)
                var t483 Result__unit__string = Result__unit__string_Ok{
                    _0: struct{}{},
                }
                return t483
            case Link:
                var x188 Typ = mtmp185.(Link)._0
                var t485 Result__unit__string = occurs(st__40, tvr__41, x188)
                return t485
            default:
                panic("non-exhaustive match")
            }
        }
    case TArrow:
        var x183 Typ = ty__42.(TArrow)._0
        var x184 Typ = ty__42.(TArrow)._1
        var mtmp194 Result__unit__string = occurs(st__40, tvr__41, x183)
        switch mtmp194.(type) {
        case Result__unit__string_Ok:
            var t488 Result__unit__string = occurs(st__40, tvr__41, x184)
            return t488
        case Result__unit__string_Err:
            var x196 string = mtmp194.(Result__unit__string_Err)._0
            var t489 Result__unit__string = Result__unit__string_Err{
                _0: x196,
            }
            return t489
        default:
            panic("non-exhaustive match")
        }
    default:
        var t490 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        return t490
    }
}

func unify(st__52 CheckerState, t1__53 Typ, t2__54 Typ) Result__unit__string {
    switch t2__54.(type) {
    case TVar:
        var x200 *ref_Tv_x = t2__54.(TVar)._0
        switch t1__53.(type) {
        case TVar:
            var x204 *ref_Tv_x = t1__53.(TVar)._0
            var t499 bool = ptr_eq__Ref_2Tv(x204, x200)
            if t499 {
                var t500 Result__unit__string = Result__unit__string_Ok{
                    _0: struct{}{},
                }
                return t500
            } else {
                var mtmp208 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(x204)
                switch mtmp208.(type) {
                case Unbound:
                    var mtmp212 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(x200)
                    switch mtmp212.(type) {
                    case Unbound:
                        var t505 Typ = TVar{
                            _0: x200,
                        }
                        var mtmp216 Result__unit__string = occurs(st__52, x204, t505)
                        switch mtmp216.(type) {
                        case Result__unit__string_Ok:
                            var t508 Typ = TVar{
                                _0: x200,
                            }
                            var t509 Tv = Link{
                                _0: t508,
                            }
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(x204, t509)
                            var t510 Result__unit__string = Result__unit__string_Ok{
                                _0: struct{}{},
                            }
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
                    case Link:
                        var x215 Typ = mtmp212.(Link)._0
                        var t512 Typ = TVar{
                            _0: x204,
                        }
                        var t513 Result__unit__string = unify(st__52, t512, x215)
                        return t513
                    default:
                        panic("non-exhaustive match")
                    }
                case Link:
                    var x211 Typ = mtmp208.(Link)._0
                    var t514 Typ = TVar{
                        _0: x200,
                    }
                    var t515 Result__unit__string = unify(st__52, x211, t514)
                    return t515
                default:
                    panic("non-exhaustive match")
                }
            }
        default:
            var mtmp220 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(x200)
            switch mtmp220.(type) {
            case Unbound:
                var mtmp224 Result__unit__string = occurs(st__52, x200, t1__53)
                switch mtmp224.(type) {
                case Result__unit__string_Ok:
                    var t520 Tv = Link{
                        _0: t1__53,
                    }
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(x200, t520)
                    var t521 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    return t521
                case Result__unit__string_Err:
                    var x226 string = mtmp224.(Result__unit__string_Err)._0
                    var t522 Result__unit__string = Result__unit__string_Err{
                        _0: x226,
                    }
                    return t522
                default:
                    panic("non-exhaustive match")
                }
            case Link:
                var x223 Typ = mtmp220.(Link)._0
                var t523 Result__unit__string = unify(st__52, t1__53, x223)
                return t523
            default:
                panic("non-exhaustive match")
            }
        }
    case TArrow:
        var x202 Typ = t2__54.(TArrow)._0
        var x203 Typ = t2__54.(TArrow)._1
        switch t1__53.(type) {
        case TVar:
            var x228 *ref_Tv_x = t1__53.(TVar)._0
            var mtmp232 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(x228)
            switch mtmp232.(type) {
            case Unbound:
                var mtmp236 Result__unit__string = occurs(st__52, x228, t2__54)
                switch mtmp236.(type) {
                case Result__unit__string_Ok:
                    var t530 Tv = Link{
                        _0: t2__54,
                    }
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(x228, t530)
                    var t531 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    return t531
                case Result__unit__string_Err:
                    var x238 string = mtmp236.(Result__unit__string_Err)._0
                    var t532 Result__unit__string = Result__unit__string_Err{
                        _0: x238,
                    }
                    return t532
                default:
                    panic("non-exhaustive match")
                }
            case Link:
                var x235 Typ = mtmp232.(Link)._0
                var t533 Result__unit__string = unify(st__52, x235, t2__54)
                return t533
            default:
                panic("non-exhaustive match")
            }
        case TArrow:
            var x230 Typ = t1__53.(TArrow)._0
            var x231 Typ = t1__53.(TArrow)._1
            var mtmp240 Result__unit__string = unify(st__52, x230, x202)
            switch mtmp240.(type) {
            case Result__unit__string_Ok:
                var t536 Result__unit__string = unify(st__52, x231, x203)
                return t536
            case Result__unit__string_Err:
                var x242 string = mtmp240.(Result__unit__string_Err)._0
                var t537 Result__unit__string = Result__unit__string_Err{
                    _0: x242,
                }
                return t537
            default:
                panic("non-exhaustive match")
            }
        default:
            var t538 Result__unit__string = Result__unit__string_Err{
                _0: "unify error",
            }
            return t538
        }
    default:
        switch t1__53.(type) {
        case TVar:
            var x243 *ref_Tv_x = t1__53.(TVar)._0
            var mtmp247 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(x243)
            switch mtmp247.(type) {
            case Unbound:
                var mtmp251 Result__unit__string = occurs(st__52, x243, t2__54)
                switch mtmp251.(type) {
                case Result__unit__string_Ok:
                    var t545 Tv = Link{
                        _0: t2__54,
                    }
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(x243, t545)
                    var t546 Result__unit__string = Result__unit__string_Ok{
                        _0: struct{}{},
                    }
                    return t546
                case Result__unit__string_Err:
                    var x253 string = mtmp251.(Result__unit__string_Err)._0
                    var t547 Result__unit__string = Result__unit__string_Err{
                        _0: x253,
                    }
                    return t547
                default:
                    panic("non-exhaustive match")
                }
            case Link:
                var x250 Typ = mtmp247.(Link)._0
                var t548 Result__unit__string = unify(st__52, x250, t2__54)
                return t548
            default:
                panic("non-exhaustive match")
            }
        default:
            var t549 Result__unit__string = Result__unit__string_Err{
                _0: "unify error",
            }
            return t549
        }
    }
}

func gen(st__73 CheckerState, ty__74 Typ) Typ {
    switch ty__74.(type) {
    case TVar:
        var x255 *ref_Tv_x = ty__74.(TVar)._0
        var mtmp259 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(x255)
        switch mtmp259.(type) {
        case Unbound:
            var x260 string = mtmp259.(Unbound)._0
            var x261 int32 = mtmp259.(Unbound)._1
            var t556 *ref_int32_x = st__73.current_level
            var cur__78 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(t556)
            var t559 bool = x261 > cur__78
            if t559 {
                var t560 Typ = QVar{
                    _0: x260,
                }
                return t560
            } else {
                var t561 Typ = TVar{
                    _0: x255,
                }
                return t561
            }
        case Link:
            var x262 Typ = mtmp259.(Link)._0
            var t562 Typ = gen(st__73, x262)
            return t562
        default:
            panic("non-exhaustive match")
        }
    case TArrow:
        var x257 Typ = ty__74.(TArrow)._0
        var x258 Typ = ty__74.(TArrow)._1
        var t563 Typ = gen(st__73, x257)
        var t564 Typ = gen(st__73, x258)
        var t565 Typ = TArrow{
            _0: t563,
            _1: t564,
        }
        return t565
    default:
        return ty__74
    }
}

func inst_loop(st__83 CheckerState, subst__84 *_goml_vec_SubstEntry, ty__85 Typ) Tuple2_3Typ_16Vec_10SubstEntry {
    switch ty__85.(type) {
    case TVar:
        var x263 *ref_Tv_x = ty__85.(TVar)._0
        var mtmp267 Tv = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(x263)
        switch mtmp267.(type) {
        case Link:
            var x270 Typ = mtmp267.(Link)._0
            var t572 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, subst__84, x270)
            return t572
        default:
            var t573 Typ = TVar{
                _0: x263,
            }
            var t574 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: t573,
                _1: subst__84,
            }
            return t574
        }
    case QVar:
        var x264 string = ty__85.(QVar)._0
        var mtmp271 Option__Typ = subst_lookup(subst__84, x264)
        switch mtmp271.(type) {
        case None:
            var tv__88 Typ = newvar(st__83)
            var t577 SubstEntry = SubstEntry{
                name: x264,
                ty: tv__88,
            }
            var new_subst__89 *_goml_vec_SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SubstEntry(subst__84, t577)
            var t578 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: tv__88,
                _1: new_subst__89,
            }
            return t578
        case Some:
            var x272 Typ = mtmp271.(Some)._0
            var t579 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: x272,
                _1: subst__84,
            }
            return t579
        default:
            panic("non-exhaustive match")
        }
    case TArrow:
        var x265 Typ = ty__85.(TArrow)._0
        var x266 Typ = ty__85.(TArrow)._1
        var mtmp273 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, subst__84, x265)
        var x274 Typ = mtmp273._0
        var x275 *_goml_vec_SubstEntry = mtmp273._1
        var mtmp276 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, x275, x266)
        var x277 Typ = mtmp276._0
        var x278 *_goml_vec_SubstEntry = mtmp276._1
        var t580 Typ = TArrow{
            _0: x274,
            _1: x277,
        }
        var t581 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
            _0: t580,
            _1: x278,
        }
        return t581
    default:
        panic("non-exhaustive match")
    }
}

func inst(st__98 CheckerState, ty__99 Typ) Typ {
    var subst0__100 *_goml_vec_SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SubstEntry()
    var mtmp279 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__98, subst0__100, ty__99)
    var x280 Typ = mtmp279._0
    return x280
}

func typeof(st__102 CheckerState, env__103 *_goml_vec_EnvEntry, e__104 Exp) Result__Typ__string {
    switch e__104.(type) {
    case Var:
        var x282 string = e__104.(Var)._0
        var mtmp290 Option__Typ = env_lookup(env__103, x282)
        switch mtmp290.(type) {
        case None:
            var t590 Result__Typ__string = Result__Typ__string_Err{
                _0: "unbound var",
            }
            return t590
        case Some:
            var x291 Typ = mtmp290.(Some)._0
            var t591 Typ = inst(st__102, x291)
            var t592 Result__Typ__string = Result__Typ__string_Ok{
                _0: t591,
            }
            return t592
        default:
            panic("non-exhaustive match")
        }
    case App:
        var x283 Exp = e__104.(App)._0
        var x284 Exp = e__104.(App)._1
        var mtmp292 Result__Typ__string = typeof(st__102, env__103, x283)
        switch mtmp292.(type) {
        case Result__Typ__string_Ok:
            var x293 Typ = mtmp292.(Result__Typ__string_Ok)._0
            var mtmp295 Result__Typ__string = typeof(st__102, env__103, x284)
            switch mtmp295.(type) {
            case Result__Typ__string_Ok:
                var x296 Typ = mtmp295.(Result__Typ__string_Ok)._0
                var ty_res__119 Typ = newvar(st__102)
                var arrow__120 Typ = TArrow{
                    _0: x296,
                    _1: ty_res__119,
                }
                var mtmp298 Result__unit__string = unify(st__102, x293, arrow__120)
                switch mtmp298.(type) {
                case Result__unit__string_Ok:
                    var t599 Result__Typ__string = Result__Typ__string_Ok{
                        _0: ty_res__119,
                    }
                    return t599
                case Result__unit__string_Err:
                    var x300 string = mtmp298.(Result__unit__string_Err)._0
                    var t600 Result__Typ__string = Result__Typ__string_Err{
                        _0: x300,
                    }
                    return t600
                default:
                    panic("non-exhaustive match")
                }
            case Result__Typ__string_Err:
                var x297 string = mtmp295.(Result__Typ__string_Err)._0
                var t601 Result__Typ__string = Result__Typ__string_Err{
                    _0: x297,
                }
                return t601
            default:
                panic("non-exhaustive match")
            }
        case Result__Typ__string_Err:
            var x294 string = mtmp292.(Result__Typ__string_Err)._0
            var t602 Result__Typ__string = Result__Typ__string_Err{
                _0: x294,
            }
            return t602
        default:
            panic("non-exhaustive match")
        }
    case Lam:
        var x285 string = e__104.(Lam)._0
        var x286 Exp = e__104.(Lam)._1
        var ty_x__109 Typ = newvar(st__102)
        var t603 EnvEntry = EnvEntry{
            name: x285,
            ty: ty_x__109,
        }
        var env2__110 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__EnvEntry(env__103, t603)
        var mtmp301 Result__Typ__string = typeof(st__102, env2__110, x286)
        switch mtmp301.(type) {
        case Result__Typ__string_Ok:
            var x302 Typ = mtmp301.(Result__Typ__string_Ok)._0
            var t606 Typ = TArrow{
                _0: ty_x__109,
                _1: x302,
            }
            var t607 Result__Typ__string = Result__Typ__string_Ok{
                _0: t606,
            }
            return t607
        case Result__Typ__string_Err:
            var x303 string = mtmp301.(Result__Typ__string_Err)._0
            var t608 Result__Typ__string = Result__Typ__string_Err{
                _0: x303,
            }
            return t608
        default:
            panic("non-exhaustive match")
        }
    case Let:
        var x287 string = e__104.(Let)._0
        var x288 Exp = e__104.(Let)._1
        var x289 Exp = e__104.(Let)._2
        enter_level(st__102)
        var ty_e__125 Result__Typ__string = typeof(st__102, env__103, x288)
        leave_level(st__102)
        switch ty_e__125.(type) {
        case Result__Typ__string_Ok:
            var x306 Typ = ty_e__125.(Result__Typ__string_Ok)._0
            var t611 Typ = gen(st__102, x306)
            var t612 EnvEntry = EnvEntry{
                name: x287,
                ty: t611,
            }
            var env2__128 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__EnvEntry(env__103, t612)
            var t613 Result__Typ__string = typeof(st__102, env2__128, x289)
            return t613
        case Result__Typ__string_Err:
            var x307 string = ty_e__125.(Result__Typ__string_Err)._0
            var t614 Result__Typ__string = Result__Typ__string_Err{
                _0: x307,
            }
            return t614
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func exp_var(name__129 string) Exp {
    var t617 Exp = Var{
        _0: name__129,
    }
    return t617
}

func exp_lam(name__130 string, body__131 Exp) Exp {
    var t620 Exp = Lam{
        _0: name__130,
        _1: body__131,
    }
    return t620
}

func exp_app(a__132 Exp, b__133 Exp) Exp {
    var t623 Exp = App{
        _0: a__132,
        _1: b__133,
    }
    return t623
}

func exp_let(name__134 string, a__135 Exp, b__136 Exp) Exp {
    var t626 Exp = Let{
        _0: name__134,
        _1: a__135,
        _2: b__136,
    }
    return t626
}

func show_result(label__137 string, res__138 Result__Typ__string) struct{} {
    switch res__138.(type) {
    case Result__Typ__string_Ok:
        var x308 Typ = res__138.(Result__Typ__string_Ok)._0
        var t629 string = label__137 + ": "
        var t630 string = typ_to_string(x308)
        var t631 string = t629 + t630
        println__T_string(t631)
        return struct{}{}
    case Result__Typ__string_Err:
        var x309 string = res__138.(Result__Typ__string_Err)._0
        var t633 string = label__137 + ": "
        var t634 string = t633 + x309
        println__T_string(t634)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var st__141 CheckerState = state_new()
    var t637 Exp = exp_var("x")
    var id__142 Exp = exp_lam("x", t637)
    var t638 Exp = exp_var("x")
    var t639 Exp = exp_var("y")
    var t640 Exp = exp_app(t638, t639)
    var t641 Exp = exp_lam("y", t640)
    var c1__143 Exp = exp_lam("x", t641)
    reset_type_variables(st__141)
    var t642 *_goml_vec_EnvEntry = env_empty()
    var t643 Result__Typ__string = typeof(st__141, t642, id__142)
    show_result("id", t643)
    reset_type_variables(st__141)
    var t644 *_goml_vec_EnvEntry = env_empty()
    var t645 Result__Typ__string = typeof(st__141, t644, c1__143)
    show_result("c1", t645)
    reset_type_variables(st__141)
    var t646 *_goml_vec_EnvEntry = env_empty()
    var t647 Exp = exp_var("x")
    var t648 Exp = exp_let("x", c1__143, t647)
    var t649 Result__Typ__string = typeof(st__141, t646, t648)
    show_result("let_x_c1_x", t649)
    reset_type_variables(st__141)
    var t650 *_goml_vec_EnvEntry = env_empty()
    var t651 Exp = exp_var("z")
    var t652 Exp = exp_lam("z", t651)
    var t653 Exp = exp_var("y")
    var t654 Exp = exp_let("y", t652, t653)
    var t655 Result__Typ__string = typeof(st__141, t650, t654)
    show_result("let_y_id_y", t655)
    reset_type_variables(st__141)
    var t656 *_goml_vec_EnvEntry = env_empty()
    var t657 Exp = exp_var("z")
    var t658 Exp = exp_lam("z", t657)
    var t659 Exp = exp_var("y")
    var t660 Exp = exp_let("y", t658, t659)
    var t661 Exp = exp_lam("x", t660)
    var t662 Result__Typ__string = typeof(st__141, t656, t661)
    show_result("lam_x_let_y_id_y", t662)
    reset_type_variables(st__141)
    var t663 *_goml_vec_EnvEntry = env_empty()
    var t664 Exp = exp_var("z")
    var t665 Exp = exp_lam("z", t664)
    var t666 Exp = exp_var("y")
    var t667 Exp = exp_var("x")
    var t668 Exp = exp_app(t666, t667)
    var t669 Exp = exp_let("y", t665, t668)
    var t670 Exp = exp_lam("x", t669)
    var t671 Result__Typ__string = typeof(st__141, t663, t670)
    show_result("lam_x_let_y_id_yx", t671)
    reset_type_variables(st__141)
    var t672 *_goml_vec_EnvEntry = env_empty()
    var t673 Exp = exp_var("x")
    var t674 Exp = exp_var("x")
    var t675 Exp = exp_app(t673, t674)
    var t676 Exp = exp_lam("x", t675)
    var t677 Result__Typ__string = typeof(st__141, t672, t676)
    show_result("self_apply", t677)
    reset_type_variables(st__141)
    var t678 *_goml_vec_EnvEntry = env_empty()
    var t679 Exp = exp_var("x")
    var t680 Exp = exp_var("x")
    var t681 Exp = exp_let("x", t679, t680)
    var t682 Result__Typ__string = typeof(st__141, t678, t681)
    show_result("unbound_var", t682)
    reset_type_variables(st__141)
    var t683 *_goml_vec_EnvEntry = env_empty()
    var t684 Exp = exp_var("y")
    var t685 Exp = exp_var("y")
    var t686 Exp = exp_var("z")
    var t687 Exp = exp_app(t685, t686)
    var t688 Exp = exp_lam("z", t687)
    var t689 Exp = exp_app(t684, t688)
    var t690 Exp = exp_lam("y", t689)
    var t691 Result__Typ__string = typeof(st__141, t683, t690)
    show_result("max_heiber", t691)
    reset_type_variables(st__141)
    var t692 *_goml_vec_EnvEntry = env_empty()
    var t693 Exp = exp_var("k")
    var t694 Exp = exp_var("k")
    var t695 Exp = exp_var("x")
    var t696 Exp = exp_app(t694, t695)
    var t697 Exp = exp_var("y")
    var t698 Exp = exp_app(t696, t697)
    var t699 Exp = exp_app(t693, t698)
    var t700 Exp = exp_var("k")
    var t701 Exp = exp_var("y")
    var t702 Exp = exp_app(t700, t701)
    var t703 Exp = exp_var("x")
    var t704 Exp = exp_app(t702, t703)
    var t705 Exp = exp_app(t699, t704)
    var t706 Exp = exp_lam("k", t705)
    var t707 Exp = exp_lam("y", t706)
    var t708 Exp = exp_lam("x", t707)
    var t709 Result__Typ__string = typeof(st__141, t692, t708)
    show_result("kirang", t709)
    reset_type_variables(st__141)
    var t710 *_goml_vec_EnvEntry = env_empty()
    var t711 Exp = exp_var("id")
    var t712 Exp = exp_var("id")
    var t713 Exp = exp_app(t711, t712)
    var t714 Exp = exp_let("id", id__142, t713)
    var t715 Result__Typ__string = typeof(st__141, t710, t714)
    show_result("let_id_idid", t715)
    reset_type_variables(st__141)
    var t716 *_goml_vec_EnvEntry = env_empty()
    var t717 Exp = exp_var("x")
    var t718 Exp = exp_app(t717, id__142)
    var t719 Exp = exp_var("z")
    var t720 Exp = exp_let("z", t718, t719)
    var t721 Exp = exp_var("y")
    var t722 Exp = exp_let("y", t720, t721)
    var t723 Exp = exp_let("x", c1__143, t722)
    var t724 Result__Typ__string = typeof(st__141, t716, t723)
    show_result("nested_lets", t724)
    reset_type_variables(st__141)
    var t725 *_goml_vec_EnvEntry = env_empty()
    var t726 Exp = exp_var("x")
    var t727 Exp = exp_var("y")
    var t728 Exp = exp_app(t726, t727)
    var t729 Exp = exp_var("y")
    var t730 Exp = exp_var("x")
    var t731 Exp = exp_app(t729, t730)
    var t732 Exp = exp_lam("x", t731)
    var t733 Exp = exp_let("x", t728, t732)
    var t734 Exp = exp_lam("y", t733)
    var t735 Exp = exp_lam("x", t734)
    var t736 Result__Typ__string = typeof(st__141, t725, t735)
    show_result("fun_x_fun_y_let_x_xy_fun_x_yx", t736)
    reset_type_variables(st__141)
    var t737 *_goml_vec_EnvEntry = env_empty()
    var t738 Exp = exp_var("x")
    var t739 Exp = exp_var("y")
    var t740 Exp = exp_let("y", t738, t739)
    var t741 Exp = exp_lam("x", t740)
    var t742 Result__Typ__string = typeof(st__141, t737, t741)
    show_result("sound_gen_1", t742)
    reset_type_variables(st__141)
    var t743 *_goml_vec_EnvEntry = env_empty()
    var t744 Exp = exp_var("x")
    var t745 Exp = exp_lam("z", t744)
    var t746 Exp = exp_var("y")
    var t747 Exp = exp_let("y", t745, t746)
    var t748 Exp = exp_lam("x", t747)
    var t749 Result__Typ__string = typeof(st__141, t743, t748)
    show_result("sound_gen_2", t749)
    reset_type_variables(st__141)
    var t750 *_goml_vec_EnvEntry = env_empty()
    var t751 Exp = exp_var("x")
    var t752 Exp = exp_var("z")
    var t753 Exp = exp_app(t751, t752)
    var t754 Exp = exp_lam("z", t753)
    var t755 Exp = exp_var("y")
    var t756 Exp = exp_let("y", t754, t755)
    var t757 Exp = exp_lam("x", t756)
    var t758 Result__Typ__string = typeof(st__141, t750, t757)
    show_result("sound_gen_3", t758)
    reset_type_variables(st__141)
    var t759 *_goml_vec_EnvEntry = env_empty()
    var t760 Exp = exp_var("x")
    var t761 Exp = exp_var("y")
    var t762 Exp = exp_app(t760, t761)
    var t763 Exp = exp_var("x")
    var t764 Exp = exp_var("y")
    var t765 Exp = exp_app(t763, t764)
    var t766 Exp = exp_let("x", t762, t765)
    var t767 Exp = exp_lam("y", t766)
    var t768 Exp = exp_lam("x", t767)
    var t769 Result__Typ__string = typeof(st__141, t759, t768)
    show_result("double_apply", t769)
    reset_type_variables(st__141)
    var t770 *_goml_vec_EnvEntry = env_empty()
    var t771 Exp = exp_var("x")
    var t772 Exp = exp_var("y")
    var t773 Exp = exp_var("y")
    var t774 Exp = exp_app(t772, t773)
    var t775 Exp = exp_let("y", t771, t774)
    var t776 Exp = exp_lam("x", t775)
    var t777 Result__Typ__string = typeof(st__141, t770, t776)
    show_result("sound_gen_occurs", t777)
    reset_gensym(st__141)
    var t778 *_goml_vec_EnvEntry = env_empty()
    var t779 Exp = exp_var("x")
    var t780 Exp = exp_app(t779, id__142)
    var t781 Exp = exp_var("z")
    var t782 Exp = exp_let("z", t780, t781)
    var t783 Exp = exp_var("y")
    var t784 Exp = exp_let("y", t782, t783)
    var t785 Exp = exp_lam("x", t784)
    var t786 Result__Typ__string = typeof(st__141, t778, t785)
    show_result("fun_x_let_y_let_z_x_id_z_y", t786)
    println__T_string("")
    println__T_string("All Done")
    println__T_string("")
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__207 int32) *ref_int32_x {
    var t789 *ref_int32_x = ref__Ref_5int32(value__207)
    return t789
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__209 *ref_int32_x, value__210 int32) struct{} {
    ref_set__Ref_5int32(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__208 *ref_int32_x) int32 {
    var t794 int32 = ref_get__Ref_5int32(self__208)
    return t794
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__7 rune) string {
    var t797 string = _goml_runtime_core_char_to_string(self__7)
    return t797
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var t800 string = _goml_runtime_core_int32_to_string(self__6)
    return t800
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Tv(value__207 Tv) *ref_Tv_x {
    var t803 *ref_Tv_x = ref__Ref_2Tv(value__207)
    return t803
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Tv(self__208 *ref_Tv_x) Tv {
    var t806 Tv = ref_get__Ref_2Tv(self__208)
    return t806
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__EnvEntry() *_goml_vec_EnvEntry {
    var t809 *_goml_vec_EnvEntry = vec_new__Vec_8EnvEntry()
    return t809
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__EnvEntry(self__137 *_goml_vec_EnvEntry) int {
    var t812 int = vec_len__Vec_8EnvEntry(self__137)
    return t812
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var t815 *ref_int_x = ref__Ref_3int(value__207)
    return t815
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_Typ_r_(value__207 Option__Typ) *ref_Option__Typ_x {
    var t818 *ref_Option__Typ_x = ref__Ref_11Option__Typ(value__207)
    return t818
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__207 bool) *ref_bool_x {
    var t821 *ref_bool_x = ref__Ref_4bool(value__207)
    return t821
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__208 *ref_bool_x) bool {
    var t824 bool = ref_get__Ref_4bool(self__208)
    return t824
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var t827 int = ref_get__Ref_3int(self__208)
    return t827
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__55 string, other__56 string) bool {
    var t830 bool = self__55 == other__56
    return t830
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Option_l_Typ_r_(self__209 *ref_Option__Typ_x, value__210 Option__Typ) struct{} {
    ref_set__Ref_11Option__Typ(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(self__209 *ref_bool_x, value__210 bool) struct{} {
    ref_set__Ref_4bool(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Option_l_Typ_r_(self__208 *ref_Option__Typ_x) Option__Typ {
    var t839 Option__Typ = ref_get__Ref_11Option__Typ(self__208)
    return t839
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SubstEntry(self__137 *_goml_vec_SubstEntry) int {
    var t842 int = vec_len__Vec_10SubstEntry(self__137)
    return t842
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Tv(self__209 *ref_Tv_x, value__210 Tv) struct{} {
    ref_set__Ref_2Tv(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SubstEntry(self__128 *_goml_vec_SubstEntry, elem__129 SubstEntry) *_goml_vec_SubstEntry {
    var t847 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SubstEntry(self__128)
    var t848 int = t847 + 1
    var result__130 *_goml_vec_SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__SubstEntry(t848)
    var index__131 int = 0
    Loop_loop850:
    for {
        var t851 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SubstEntry(self__128)
        var t852 bool = index__131 < t851
        if t852 {
            var t853 SubstEntry = vec_get__Vec_10SubstEntry(self__128, index__131)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SubstEntry(result__130, t853)
            var compound_old38 int = index__131
            var compound_value39 int = 1
            var t854 int = compound_old38 + compound_value39
            index__131 = t854
            continue
        } else {
            break Loop_loop850
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SubstEntry(result__130, elem__129)
    return result__130
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SubstEntry() *_goml_vec_SubstEntry {
    var t858 *_goml_vec_SubstEntry = vec_new__Vec_10SubstEntry()
    return t858
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__EnvEntry(self__128 *_goml_vec_EnvEntry, elem__129 EnvEntry) *_goml_vec_EnvEntry {
    var t861 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__EnvEntry(self__128)
    var t862 int = t861 + 1
    var result__130 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__EnvEntry(t862)
    var index__131 int = 0
    Loop_loop864:
    for {
        var t865 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__EnvEntry(self__128)
        var t866 bool = index__131 < t865
        if t866 {
            var t867 EnvEntry = vec_get__Vec_8EnvEntry(self__128, index__131)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__EnvEntry(result__130, t867)
            var compound_old38 int = index__131
            var compound_value39 int = 1
            var t868 int = compound_old38 + compound_value39
            index__131 = t868
            continue
        } else {
            break Loop_loop864
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__EnvEntry(result__130, elem__129)
    return result__130
}

func println__T_string(value__1 string) struct{} {
    var t871 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t871)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__SubstEntry(capacity__125 int) *_goml_vec_SubstEntry {
    var t875 *_goml_vec_SubstEntry = vec_with_capacity__Vec_10SubstEntry(capacity__125)
    return t875
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SubstEntry(self__126 *_goml_vec_SubstEntry, elem__127 SubstEntry) struct{} {
    vec_push__Vec_10SubstEntry(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__EnvEntry(capacity__125 int) *_goml_vec_EnvEntry {
    var t880 *_goml_vec_EnvEntry = vec_with_capacity__Vec_8EnvEntry(capacity__125)
    return t880
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__EnvEntry(self__126 *_goml_vec_EnvEntry, elem__127 EnvEntry) struct{} {
    vec_push__Vec_8EnvEntry(self__126, elem__127)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
