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

type Ordering int32

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

type Result__unit__string struct {
    _tag int32
    _v0_0 struct{}
    _v1_0 string
}

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
    var t606 *ref_int32_x
    var inline1175 int32 = 0
    var inline1176 *ref_int32_x = ref__Ref_5int32(inline1175)
    t606 = inline1176
    var t607 *ref_int32_x
    var inline1172 int32 = 1
    var inline1173 *ref_int32_x = ref__Ref_5int32(inline1172)
    t607 = inline1173
    var t608 CheckerState = CheckerState{
        gensym_counter: t606,
        current_level: t607,
    }
    return t608
}

func reset_type_variables(st__2 CheckerState) struct{} {
    var inline1187 *ref_int32_x = st__2.gensym_counter
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline1187, 0)
    var inline1184 *ref_int32_x = st__2.current_level
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline1184, 1)
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
    var t634 *ref_int32_x = st__10.gensym_counter
    var n__11 int32
    var inline1204 int32 = ref_get__Ref_5int32(t634)
    n__11 = inline1204
    var t635 *ref_int32_x = st__10.gensym_counter
    var t636 int32 = n__11 + 1
    ref_set__Ref_5int32(t635, t636)
    var t639 bool = n__11 < 26
    if t639 {
        var t640 rune = nth_letter(n__11)
        var inline1198 string = char_to_string(t640)
        return inline1198
    } else {
        var t642 string
        var inline1200 string = _goml_runtime_core_int32_to_string(n__11)
        t642 = inline1200
        var t643 string = "t" + t642
        return t643
    }
}

func typ_is_arrow(ty__15 Typ) bool {
    switch ty__15.(type) {
    case TVar:
        var x414 *ref_Tv_x = ty__15.(TVar)._0
        var mtmp418 Tv
        var inline1221 Tv = ref_get__Ref_2Tv(x414)
        mtmp418 = inline1221
        switch mtmp418.(type) {
        case Link:
            var x421 Typ = mtmp418.(Link)._0
            var t656 bool = typ_is_arrow(x421)
            return t656
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
        var x422 *ref_Tv_x = ty__18.(TVar)._0
        var mtmp426 Tv
        var inline1223 Tv = ref_get__Ref_2Tv(x422)
        mtmp426 = inline1223
        switch mtmp426.(type) {
        case Unbound:
            var x427 string = mtmp426.(Unbound)._0
            var t663 string = "'" + x427
            return t663
        case Link:
            var x429 Typ = mtmp426.(Link)._0
            var t664 string = typ_to_string(x429)
            return t664
        default:
            panic("non-exhaustive match")
        }
    case QVar:
        var x423 string = ty__18.(QVar)._0
        var t665 string = "'" + x423
        return t665
    case TArrow:
        var x424 Typ = ty__18.(TArrow)._0
        var x425 Typ = ty__18.(TArrow)._1
        var t670 bool = typ_is_arrow(x424)
        var jp667 string
        if t670 {
            var t671 string = typ_to_string(x424)
            var t672 string = "(" + t671
            var t673 string = t672 + ")"
            jp667 = t673
        } else {
            var t674 string = typ_to_string(x424)
            jp667 = t674
        }
        var s2__26 string = typ_to_string(x425)
        var t668 string = jp667 + " -> "
        var t669 string = t668 + s2__26
        return t669
    default:
        panic("non-exhaustive match")
    }
}

func env_empty() *_goml_vec_EnvEntry {
    var inline1225 *_goml_vec_EnvEntry = vec_new__Vec_8EnvEntry()
    return inline1225
}

func env_lookup(env__28 *_goml_vec_EnvEntry, name__29 string) Option__Typ {
    var t679 int
    var inline1251 int = vec_len__Vec_8EnvEntry(env__28)
    t679 = inline1251
    var t680 int = t679 - 1
    var i__30 *ref_int_x
    var inline1249 *ref_int_x = ref__Ref_3int(t680)
    i__30 = inline1249
    var found__31 *ref_Option__Typ_x
    var inline1247 *ref_Option__Typ_x = ref__Ref_11Option__Typ(None{})
    found__31 = inline1247
    var done__32 *ref_bool_x
    var inline1244 bool = false
    var inline1245 *ref_bool_x = ref__Ref_4bool(inline1244)
    done__32 = inline1245
    Loop_loop683:
    for {
        var t696 bool
        var inline1240 bool = ref_get__Ref_4bool(done__32)
        t696 = inline1240
        var t697 bool = !t696
        var jp685 bool
        if t697 {
            var t698 int
            var inline1227 int = ref_get__Ref_3int(i__30)
            t698 = inline1227
            var t699 bool = t698 >= 0
            jp685 = t699
        } else {
            jp685 = false
        }
        if jp685 {
            var t686 int
            var inline1238 int = ref_get__Ref_3int(i__30)
            t686 = inline1238
            var entry__33 EnvEntry = vec_get__Vec_8EnvEntry(env__28, t686)
            var t688 string = entry__33.name
            var t689 bool = t688 == name__29
            if t689 {
                var t690 Typ = entry__33.ty
                var t691 Option__Typ = Some{
                    _0: t690,
                }
                ref_set__Ref_11Option__Typ(found__31, t691)
                var inline1229 bool = true
                ref_set__Ref_4bool(done__32, inline1229)
                continue
            } else {
                var t693 int
                var inline1236 int = ref_get__Ref_3int(i__30)
                t693 = inline1236
                var t694 int = t693 - 1
                ref_set__Ref_3int(i__30, t694)
                continue
            }
        } else {
            break Loop_loop683
        }
    }
    var inline1242 Option__Typ = ref_get__Ref_11Option__Typ(found__31)
    return inline1242
}

func subst_lookup(subst__34 *_goml_vec_SubstEntry, name__35 string) Option__Typ {
    var t702 int
    var inline1277 int = vec_len__Vec_10SubstEntry(subst__34)
    t702 = inline1277
    var t703 int = t702 - 1
    var i__36 *ref_int_x
    var inline1275 *ref_int_x = ref__Ref_3int(t703)
    i__36 = inline1275
    var found__37 *ref_Option__Typ_x
    var inline1273 *ref_Option__Typ_x = ref__Ref_11Option__Typ(None{})
    found__37 = inline1273
    var done__38 *ref_bool_x
    var inline1270 bool = false
    var inline1271 *ref_bool_x = ref__Ref_4bool(inline1270)
    done__38 = inline1271
    Loop_loop706:
    for {
        var t719 bool
        var inline1266 bool = ref_get__Ref_4bool(done__38)
        t719 = inline1266
        var t720 bool = !t719
        var jp708 bool
        if t720 {
            var t721 int
            var inline1253 int = ref_get__Ref_3int(i__36)
            t721 = inline1253
            var t722 bool = t721 >= 0
            jp708 = t722
        } else {
            jp708 = false
        }
        if jp708 {
            var t709 int
            var inline1264 int = ref_get__Ref_3int(i__36)
            t709 = inline1264
            var entry__39 SubstEntry = vec_get__Vec_10SubstEntry(subst__34, t709)
            var t711 string = entry__39.name
            var t712 bool = t711 == name__35
            if t712 {
                var t713 Typ = entry__39.ty
                var t714 Option__Typ = Some{
                    _0: t713,
                }
                ref_set__Ref_11Option__Typ(found__37, t714)
                var inline1255 bool = true
                ref_set__Ref_4bool(done__38, inline1255)
                continue
            } else {
                var t716 int
                var inline1262 int = ref_get__Ref_3int(i__36)
                t716 = inline1262
                var t717 int = t716 - 1
                ref_set__Ref_3int(i__36, t717)
                continue
            }
        } else {
            break Loop_loop706
        }
    }
    var inline1268 Option__Typ = ref_get__Ref_11Option__Typ(found__37)
    return inline1268
}

func occurs(st__40 CheckerState, tvr__41 *ref_Tv_x, ty__42 Typ) Result__unit__string {
    switch ty__42.(type) {
    case TVar:
        var x434 *ref_Tv_x = ty__42.(TVar)._0
        var t729 bool = ptr_eq__Ref_2Tv(tvr__41, x434)
        if t729 {
            var t730 Result__unit__string = Result__unit__string{
                _tag: 1,
                _v1_0: "occurs check",
            }
            return t730
        } else {
            var mtmp438 Tv
            var inline1285 Tv = ref_get__Ref_2Tv(x434)
            mtmp438 = inline1285
            switch mtmp438.(type) {
            case Unbound:
                var x439 string = mtmp438.(Unbound)._0
                var x440 int32 = mtmp438.(Unbound)._1
                var mtmp442 Tv
                var inline1283 Tv = ref_get__Ref_2Tv(tvr__41)
                mtmp442 = inline1283
                var jp734 int32
                switch mtmp442.(type) {
                case Unbound:
                    var x444 int32 = mtmp442.(Unbound)._1
                    var inline1279 bool = x444 < x440
                    if inline1279 {
                        jp734 = x444
                    } else {
                        jp734 = x440
                    }
                default:
                    jp734 = x440
                }
                var t735 Tv = Unbound{
                    _0: x439,
                    _1: jp734,
                }
                ref_set__Ref_2Tv(x434, t735)
                var t736 Result__unit__string = Result__unit__string{
                    _tag: 0,
                    _v0_0: struct{}{},
                }
                return t736
            case Link:
                var x441 Typ = mtmp438.(Link)._0
                var t738 Result__unit__string = occurs(st__40, tvr__41, x441)
                return t738
            default:
                panic("non-exhaustive match")
            }
        }
    case TArrow:
        var x436 Typ = ty__42.(TArrow)._0
        var x437 Typ = ty__42.(TArrow)._1
        var mtmp447 Result__unit__string = occurs(st__40, tvr__41, x436)
        switch mtmp447._tag {
        case 0:
            var t741 Result__unit__string = occurs(st__40, tvr__41, x437)
            return t741
        case 1:
            var x449 string = mtmp447._v1_0
            var t742 Result__unit__string = Result__unit__string{
                _tag: 1,
                _v1_0: x449,
            }
            return t742
        default:
            panic("non-exhaustive match")
        }
    default:
        var t743 Result__unit__string = Result__unit__string{
            _tag: 0,
            _v0_0: struct{}{},
        }
        return t743
    }
}

func unify(st__52 CheckerState, t1__53 Typ, t2__54 Typ) Result__unit__string {
    switch t2__54.(type) {
    case TVar:
        var x453 *ref_Tv_x = t2__54.(TVar)._0
        switch t1__53.(type) {
        case TVar:
            var x457 *ref_Tv_x = t1__53.(TVar)._0
            var t752 bool = ptr_eq__Ref_2Tv(x457, x453)
            if t752 {
                var t753 Result__unit__string = Result__unit__string{
                    _tag: 0,
                    _v0_0: struct{}{},
                }
                return t753
            } else {
                var mtmp461 Tv
                var inline1291 Tv = ref_get__Ref_2Tv(x457)
                mtmp461 = inline1291
                switch mtmp461.(type) {
                case Unbound:
                    var mtmp465 Tv
                    var inline1289 Tv = ref_get__Ref_2Tv(x453)
                    mtmp465 = inline1289
                    switch mtmp465.(type) {
                    case Unbound:
                        var t758 Typ = TVar{
                            _0: x453,
                        }
                        var mtmp469 Result__unit__string = occurs(st__52, x457, t758)
                        switch mtmp469._tag {
                        case 0:
                            var t761 Typ = TVar{
                                _0: x453,
                            }
                            var t762 Tv = Link{
                                _0: t761,
                            }
                            ref_set__Ref_2Tv(x457, t762)
                            var t763 Result__unit__string = Result__unit__string{
                                _tag: 0,
                                _v0_0: struct{}{},
                            }
                            return t763
                        case 1:
                            var x471 string = mtmp469._v1_0
                            var t764 Result__unit__string = Result__unit__string{
                                _tag: 1,
                                _v1_0: x471,
                            }
                            return t764
                        default:
                            panic("non-exhaustive match")
                        }
                    case Link:
                        var x468 Typ = mtmp465.(Link)._0
                        var t765 Typ = TVar{
                            _0: x457,
                        }
                        var t766 Result__unit__string = unify(st__52, t765, x468)
                        return t766
                    default:
                        panic("non-exhaustive match")
                    }
                case Link:
                    var x464 Typ = mtmp461.(Link)._0
                    var t767 Typ = TVar{
                        _0: x453,
                    }
                    var t768 Result__unit__string = unify(st__52, x464, t767)
                    return t768
                default:
                    panic("non-exhaustive match")
                }
            }
        default:
            var mtmp473 Tv
            var inline1295 Tv = ref_get__Ref_2Tv(x453)
            mtmp473 = inline1295
            switch mtmp473.(type) {
            case Unbound:
                var mtmp477 Result__unit__string = occurs(st__52, x453, t1__53)
                switch mtmp477._tag {
                case 0:
                    var t773 Tv = Link{
                        _0: t1__53,
                    }
                    ref_set__Ref_2Tv(x453, t773)
                    var t774 Result__unit__string = Result__unit__string{
                        _tag: 0,
                        _v0_0: struct{}{},
                    }
                    return t774
                case 1:
                    var x479 string = mtmp477._v1_0
                    var t775 Result__unit__string = Result__unit__string{
                        _tag: 1,
                        _v1_0: x479,
                    }
                    return t775
                default:
                    panic("non-exhaustive match")
                }
            case Link:
                var x476 Typ = mtmp473.(Link)._0
                var t776 Result__unit__string = unify(st__52, t1__53, x476)
                return t776
            default:
                panic("non-exhaustive match")
            }
        }
    case TArrow:
        var x455 Typ = t2__54.(TArrow)._0
        var x456 Typ = t2__54.(TArrow)._1
        switch t1__53.(type) {
        case TVar:
            var x481 *ref_Tv_x = t1__53.(TVar)._0
            var mtmp485 Tv
            var inline1299 Tv = ref_get__Ref_2Tv(x481)
            mtmp485 = inline1299
            switch mtmp485.(type) {
            case Unbound:
                var mtmp489 Result__unit__string = occurs(st__52, x481, t2__54)
                switch mtmp489._tag {
                case 0:
                    var t783 Tv = Link{
                        _0: t2__54,
                    }
                    ref_set__Ref_2Tv(x481, t783)
                    var t784 Result__unit__string = Result__unit__string{
                        _tag: 0,
                        _v0_0: struct{}{},
                    }
                    return t784
                case 1:
                    var x491 string = mtmp489._v1_0
                    var t785 Result__unit__string = Result__unit__string{
                        _tag: 1,
                        _v1_0: x491,
                    }
                    return t785
                default:
                    panic("non-exhaustive match")
                }
            case Link:
                var x488 Typ = mtmp485.(Link)._0
                var t786 Result__unit__string = unify(st__52, x488, t2__54)
                return t786
            default:
                panic("non-exhaustive match")
            }
        case TArrow:
            var x483 Typ = t1__53.(TArrow)._0
            var x484 Typ = t1__53.(TArrow)._1
            var mtmp493 Result__unit__string = unify(st__52, x483, x455)
            switch mtmp493._tag {
            case 0:
                var t789 Result__unit__string = unify(st__52, x484, x456)
                return t789
            case 1:
                var x495 string = mtmp493._v1_0
                var t790 Result__unit__string = Result__unit__string{
                    _tag: 1,
                    _v1_0: x495,
                }
                return t790
            default:
                panic("non-exhaustive match")
            }
        default:
            var t791 Result__unit__string = Result__unit__string{
                _tag: 1,
                _v1_0: "unify error",
            }
            return t791
        }
    default:
        switch t1__53.(type) {
        case TVar:
            var x496 *ref_Tv_x = t1__53.(TVar)._0
            var mtmp500 Tv
            var inline1303 Tv = ref_get__Ref_2Tv(x496)
            mtmp500 = inline1303
            switch mtmp500.(type) {
            case Unbound:
                var mtmp504 Result__unit__string = occurs(st__52, x496, t2__54)
                switch mtmp504._tag {
                case 0:
                    var t798 Tv = Link{
                        _0: t2__54,
                    }
                    ref_set__Ref_2Tv(x496, t798)
                    var t799 Result__unit__string = Result__unit__string{
                        _tag: 0,
                        _v0_0: struct{}{},
                    }
                    return t799
                case 1:
                    var x506 string = mtmp504._v1_0
                    var t800 Result__unit__string = Result__unit__string{
                        _tag: 1,
                        _v1_0: x506,
                    }
                    return t800
                default:
                    panic("non-exhaustive match")
                }
            case Link:
                var x503 Typ = mtmp500.(Link)._0
                var t801 Result__unit__string = unify(st__52, x503, t2__54)
                return t801
            default:
                panic("non-exhaustive match")
            }
        default:
            var t802 Result__unit__string = Result__unit__string{
                _tag: 1,
                _v1_0: "unify error",
            }
            return t802
        }
    }
}

func gen(st__73 CheckerState, ty__74 Typ) Typ {
    switch ty__74.(type) {
    case TVar:
        var x508 *ref_Tv_x = ty__74.(TVar)._0
        var mtmp512 Tv
        var inline1307 Tv = ref_get__Ref_2Tv(x508)
        mtmp512 = inline1307
        switch mtmp512.(type) {
        case Unbound:
            var x513 string = mtmp512.(Unbound)._0
            var x514 int32 = mtmp512.(Unbound)._1
            var t809 *ref_int32_x = st__73.current_level
            var cur__78 int32
            var inline1305 int32 = ref_get__Ref_5int32(t809)
            cur__78 = inline1305
            var t812 bool = x514 > cur__78
            if t812 {
                var t813 Typ = QVar{
                    _0: x513,
                }
                return t813
            } else {
                var t814 Typ = TVar{
                    _0: x508,
                }
                return t814
            }
        case Link:
            var x515 Typ = mtmp512.(Link)._0
            var t815 Typ = gen(st__73, x515)
            return t815
        default:
            panic("non-exhaustive match")
        }
    case TArrow:
        var x510 Typ = ty__74.(TArrow)._0
        var x511 Typ = ty__74.(TArrow)._1
        var t816 Typ = gen(st__73, x510)
        var t817 Typ = gen(st__73, x511)
        var t818 Typ = TArrow{
            _0: t816,
            _1: t817,
        }
        return t818
    default:
        return ty__74
    }
}

func inst_loop(st__83 CheckerState, subst__84 *_goml_vec_SubstEntry, ty__85 Typ) Tuple2_3Typ_16Vec_10SubstEntry {
    switch ty__85.(type) {
    case TVar:
        var x516 *ref_Tv_x = ty__85.(TVar)._0
        var mtmp520 Tv
        var inline1309 Tv = ref_get__Ref_2Tv(x516)
        mtmp520 = inline1309
        switch mtmp520.(type) {
        case Link:
            var x523 Typ = mtmp520.(Link)._0
            var t825 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, subst__84, x523)
            return t825
        default:
            var t826 Typ = TVar{
                _0: x516,
            }
            var t827 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: t826,
                _1: subst__84,
            }
            return t827
        }
    case QVar:
        var x517 string = ty__85.(QVar)._0
        var mtmp524 Option__Typ = subst_lookup(subst__84, x517)
        switch mtmp524.(type) {
        case None:
            var tv__88 Typ
            var inline1311 string = gensym(st__83)
            var inline1312 *ref_int32_x = st__83.current_level
            var inline1313 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline1312)
            var inline1314 Tv = Unbound{
                _0: inline1311,
                _1: inline1313,
            }
            var inline1315 *ref_Tv_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Tv(inline1314)
            var inline1316 Typ = TVar{
                _0: inline1315,
            }
            tv__88 = inline1316
            var t830 SubstEntry = SubstEntry{
                name: x517,
                ty: tv__88,
            }
            var new_subst__89 *_goml_vec_SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SubstEntry(subst__84, t830)
            var t831 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: tv__88,
                _1: new_subst__89,
            }
            return t831
        case Some:
            var x525 Typ = mtmp524.(Some)._0
            var t832 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: x525,
                _1: subst__84,
            }
            return t832
        default:
            panic("non-exhaustive match")
        }
    case TArrow:
        var x518 Typ = ty__85.(TArrow)._0
        var x519 Typ = ty__85.(TArrow)._1
        var mtmp526 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, subst__84, x518)
        var x527 Typ = mtmp526._0
        var x528 *_goml_vec_SubstEntry = mtmp526._1
        var mtmp529 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, x528, x519)
        var x530 Typ = mtmp529._0
        var x531 *_goml_vec_SubstEntry = mtmp529._1
        var t833 Typ = TArrow{
            _0: x527,
            _1: x530,
        }
        var t834 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
            _0: t833,
            _1: x531,
        }
        return t834
    default:
        panic("non-exhaustive match")
    }
}

func typeof(st__102 CheckerState, env__103 *_goml_vec_EnvEntry, e__104 Exp) Result__Typ__string {
    switch e__104.(type) {
    case Var:
        var x535 string = e__104.(Var)._0
        var mtmp543 Option__Typ = env_lookup(env__103, x535)
        switch mtmp543.(type) {
        case None:
            var t843 Result__Typ__string = Result__Typ__string_Err{
                _0: "unbound var",
            }
            return t843
        case Some:
            var x544 Typ = mtmp543.(Some)._0
            var t844 Typ
            var inline1320 *_goml_vec_SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SubstEntry()
            var inline1321 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__102, inline1320, x544)
            var inline1322 Typ = inline1321._0
            t844 = inline1322
            var t845 Result__Typ__string = Result__Typ__string_Ok{
                _0: t844,
            }
            return t845
        default:
            panic("non-exhaustive match")
        }
    case App:
        var x536 Exp = e__104.(App)._0
        var x537 Exp = e__104.(App)._1
        var mtmp545 Result__Typ__string = typeof(st__102, env__103, x536)
        switch mtmp545.(type) {
        case Result__Typ__string_Ok:
            var x546 Typ = mtmp545.(Result__Typ__string_Ok)._0
            var mtmp548 Result__Typ__string = typeof(st__102, env__103, x537)
            switch mtmp548.(type) {
            case Result__Typ__string_Ok:
                var x549 Typ = mtmp548.(Result__Typ__string_Ok)._0
                var ty_res__119 Typ
                var inline1325 string = gensym(st__102)
                var inline1326 *ref_int32_x = st__102.current_level
                var inline1327 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline1326)
                var inline1328 Tv = Unbound{
                    _0: inline1325,
                    _1: inline1327,
                }
                var inline1329 *ref_Tv_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Tv(inline1328)
                var inline1330 Typ = TVar{
                    _0: inline1329,
                }
                ty_res__119 = inline1330
                var arrow__120 Typ = TArrow{
                    _0: x549,
                    _1: ty_res__119,
                }
                var mtmp551 Result__unit__string = unify(st__102, x546, arrow__120)
                switch mtmp551._tag {
                case 0:
                    var t852 Result__Typ__string = Result__Typ__string_Ok{
                        _0: ty_res__119,
                    }
                    return t852
                case 1:
                    var x553 string = mtmp551._v1_0
                    var t853 Result__Typ__string = Result__Typ__string_Err{
                        _0: x553,
                    }
                    return t853
                default:
                    panic("non-exhaustive match")
                }
            case Result__Typ__string_Err:
                var x550 string = mtmp548.(Result__Typ__string_Err)._0
                var t854 Result__Typ__string = Result__Typ__string_Err{
                    _0: x550,
                }
                return t854
            default:
                panic("non-exhaustive match")
            }
        case Result__Typ__string_Err:
            var x547 string = mtmp545.(Result__Typ__string_Err)._0
            var t855 Result__Typ__string = Result__Typ__string_Err{
                _0: x547,
            }
            return t855
        default:
            panic("non-exhaustive match")
        }
    case Lam:
        var x538 string = e__104.(Lam)._0
        var x539 Exp = e__104.(Lam)._1
        var ty_x__109 Typ
        var inline1332 string = gensym(st__102)
        var inline1333 *ref_int32_x = st__102.current_level
        var inline1334 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline1333)
        var inline1335 Tv = Unbound{
            _0: inline1332,
            _1: inline1334,
        }
        var inline1336 *ref_Tv_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Tv(inline1335)
        var inline1337 Typ = TVar{
            _0: inline1336,
        }
        ty_x__109 = inline1337
        var t856 EnvEntry = EnvEntry{
            name: x538,
            ty: ty_x__109,
        }
        var env2__110 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__EnvEntry(env__103, t856)
        var mtmp554 Result__Typ__string = typeof(st__102, env2__110, x539)
        switch mtmp554.(type) {
        case Result__Typ__string_Ok:
            var x555 Typ = mtmp554.(Result__Typ__string_Ok)._0
            var t859 Typ = TArrow{
                _0: ty_x__109,
                _1: x555,
            }
            var t860 Result__Typ__string = Result__Typ__string_Ok{
                _0: t859,
            }
            return t860
        case Result__Typ__string_Err:
            var x556 string = mtmp554.(Result__Typ__string_Err)._0
            var t861 Result__Typ__string = Result__Typ__string_Err{
                _0: x556,
            }
            return t861
        default:
            panic("non-exhaustive match")
        }
    case Let:
        var x540 string = e__104.(Let)._0
        var x541 Exp = e__104.(Let)._1
        var x542 Exp = e__104.(Let)._2
        var inline1345 *ref_int32_x = st__102.current_level
        var inline1346 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline1345)
        var inline1347 *ref_int32_x = st__102.current_level
        var inline1348 int32 = inline1346 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline1347, inline1348)
        var ty_e__125 Result__Typ__string = typeof(st__102, env__103, x541)
        var inline1339 *ref_int32_x = st__102.current_level
        var inline1340 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline1339)
        var inline1341 *ref_int32_x = st__102.current_level
        var inline1342 int32 = inline1340 - 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline1341, inline1342)
        switch ty_e__125.(type) {
        case Result__Typ__string_Ok:
            var x559 Typ = ty_e__125.(Result__Typ__string_Ok)._0
            var t864 Typ = gen(st__102, x559)
            var t865 EnvEntry = EnvEntry{
                name: x540,
                ty: t864,
            }
            var env2__128 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__EnvEntry(env__103, t865)
            var t866 Result__Typ__string = typeof(st__102, env2__128, x542)
            return t866
        case Result__Typ__string_Err:
            var x560 string = ty_e__125.(Result__Typ__string_Err)._0
            var t867 Result__Typ__string = Result__Typ__string_Err{
                _0: x560,
            }
            return t867
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func exp_var(name__129 string) Exp {
    var t870 Exp = Var{
        _0: name__129,
    }
    return t870
}

func exp_lam(name__130 string, body__131 Exp) Exp {
    var t873 Exp = Lam{
        _0: name__130,
        _1: body__131,
    }
    return t873
}

func exp_app(a__132 Exp, b__133 Exp) Exp {
    var t876 Exp = App{
        _0: a__132,
        _1: b__133,
    }
    return t876
}

func exp_let(name__134 string, a__135 Exp, b__136 Exp) Exp {
    var t879 Exp = Let{
        _0: name__134,
        _1: a__135,
        _2: b__136,
    }
    return t879
}

func show_result(label__137 string, res__138 Result__Typ__string) struct{} {
    switch res__138.(type) {
    case Result__Typ__string_Ok:
        var x561 Typ = res__138.(Result__Typ__string_Ok)._0
        var t882 string = label__137 + ": "
        var t883 string = typ_to_string(x561)
        var t884 string = t882 + t883
        var inline1351 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t884)
        _goml_runtime_core_string_println(inline1351)
        return struct{}{}
    case Result__Typ__string_Err:
        var x562 string = res__138.(Result__Typ__string_Err)._0
        var t886 string = label__137 + ": "
        var t887 string = t886 + x562
        var inline1354 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t887)
        _goml_runtime_core_string_println(inline1354)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var st__141 CheckerState = state_new()
    var t890 Exp = exp_var("x")
    var id__142 Exp = exp_lam("x", t890)
    var t891 Exp = exp_var("x")
    var t892 Exp = exp_var("y")
    var t893 Exp = exp_app(t891, t892)
    var t894 Exp = exp_lam("y", t893)
    var c1__143 Exp = exp_lam("x", t894)
    reset_type_variables(st__141)
    var t895 *_goml_vec_EnvEntry = env_empty()
    var t896 Result__Typ__string = typeof(st__141, t895, id__142)
    show_result("id", t896)
    reset_type_variables(st__141)
    var t897 *_goml_vec_EnvEntry = env_empty()
    var t898 Result__Typ__string = typeof(st__141, t897, c1__143)
    show_result("c1", t898)
    reset_type_variables(st__141)
    var t899 *_goml_vec_EnvEntry = env_empty()
    var t900 Exp = exp_var("x")
    var t901 Exp = exp_let("x", c1__143, t900)
    var t902 Result__Typ__string = typeof(st__141, t899, t901)
    show_result("let_x_c1_x", t902)
    reset_type_variables(st__141)
    var t903 *_goml_vec_EnvEntry = env_empty()
    var t904 Exp = exp_var("z")
    var t905 Exp = exp_lam("z", t904)
    var t906 Exp = exp_var("y")
    var t907 Exp = exp_let("y", t905, t906)
    var t908 Result__Typ__string = typeof(st__141, t903, t907)
    show_result("let_y_id_y", t908)
    reset_type_variables(st__141)
    var t909 *_goml_vec_EnvEntry = env_empty()
    var t910 Exp = exp_var("z")
    var t911 Exp = exp_lam("z", t910)
    var t912 Exp = exp_var("y")
    var t913 Exp = exp_let("y", t911, t912)
    var t914 Exp = exp_lam("x", t913)
    var t915 Result__Typ__string = typeof(st__141, t909, t914)
    show_result("lam_x_let_y_id_y", t915)
    reset_type_variables(st__141)
    var t916 *_goml_vec_EnvEntry = env_empty()
    var t917 Exp = exp_var("z")
    var t918 Exp = exp_lam("z", t917)
    var t919 Exp = exp_var("y")
    var t920 Exp = exp_var("x")
    var t921 Exp = exp_app(t919, t920)
    var t922 Exp = exp_let("y", t918, t921)
    var t923 Exp = exp_lam("x", t922)
    var t924 Result__Typ__string = typeof(st__141, t916, t923)
    show_result("lam_x_let_y_id_yx", t924)
    reset_type_variables(st__141)
    var t925 *_goml_vec_EnvEntry = env_empty()
    var t926 Exp = exp_var("x")
    var t927 Exp = exp_var("x")
    var t928 Exp = exp_app(t926, t927)
    var t929 Exp = exp_lam("x", t928)
    var t930 Result__Typ__string = typeof(st__141, t925, t929)
    show_result("self_apply", t930)
    reset_type_variables(st__141)
    var t931 *_goml_vec_EnvEntry = env_empty()
    var t932 Exp = exp_var("x")
    var t933 Exp = exp_var("x")
    var t934 Exp = exp_let("x", t932, t933)
    var t935 Result__Typ__string = typeof(st__141, t931, t934)
    show_result("unbound_var", t935)
    reset_type_variables(st__141)
    var t936 *_goml_vec_EnvEntry = env_empty()
    var t937 Exp = exp_var("y")
    var t938 Exp = exp_var("y")
    var t939 Exp = exp_var("z")
    var t940 Exp = exp_app(t938, t939)
    var t941 Exp = exp_lam("z", t940)
    var t942 Exp = exp_app(t937, t941)
    var t943 Exp = exp_lam("y", t942)
    var t944 Result__Typ__string = typeof(st__141, t936, t943)
    show_result("max_heiber", t944)
    reset_type_variables(st__141)
    var t945 *_goml_vec_EnvEntry = env_empty()
    var t946 Exp = exp_var("k")
    var t947 Exp = exp_var("k")
    var t948 Exp = exp_var("x")
    var t949 Exp = exp_app(t947, t948)
    var t950 Exp = exp_var("y")
    var t951 Exp = exp_app(t949, t950)
    var t952 Exp = exp_app(t946, t951)
    var t953 Exp = exp_var("k")
    var t954 Exp = exp_var("y")
    var t955 Exp = exp_app(t953, t954)
    var t956 Exp = exp_var("x")
    var t957 Exp = exp_app(t955, t956)
    var t958 Exp = exp_app(t952, t957)
    var t959 Exp = exp_lam("k", t958)
    var t960 Exp = exp_lam("y", t959)
    var t961 Exp = exp_lam("x", t960)
    var t962 Result__Typ__string = typeof(st__141, t945, t961)
    show_result("kirang", t962)
    reset_type_variables(st__141)
    var t963 *_goml_vec_EnvEntry = env_empty()
    var t964 Exp = exp_var("id")
    var t965 Exp = exp_var("id")
    var t966 Exp = exp_app(t964, t965)
    var t967 Exp = exp_let("id", id__142, t966)
    var t968 Result__Typ__string = typeof(st__141, t963, t967)
    show_result("let_id_idid", t968)
    reset_type_variables(st__141)
    var t969 *_goml_vec_EnvEntry = env_empty()
    var t970 Exp = exp_var("x")
    var t971 Exp = exp_app(t970, id__142)
    var t972 Exp = exp_var("z")
    var t973 Exp = exp_let("z", t971, t972)
    var t974 Exp = exp_var("y")
    var t975 Exp = exp_let("y", t973, t974)
    var t976 Exp = exp_let("x", c1__143, t975)
    var t977 Result__Typ__string = typeof(st__141, t969, t976)
    show_result("nested_lets", t977)
    reset_type_variables(st__141)
    var t978 *_goml_vec_EnvEntry = env_empty()
    var t979 Exp = exp_var("x")
    var t980 Exp = exp_var("y")
    var t981 Exp = exp_app(t979, t980)
    var t982 Exp = exp_var("y")
    var t983 Exp = exp_var("x")
    var t984 Exp = exp_app(t982, t983)
    var t985 Exp = exp_lam("x", t984)
    var t986 Exp = exp_let("x", t981, t985)
    var t987 Exp = exp_lam("y", t986)
    var t988 Exp = exp_lam("x", t987)
    var t989 Result__Typ__string = typeof(st__141, t978, t988)
    show_result("fun_x_fun_y_let_x_xy_fun_x_yx", t989)
    reset_type_variables(st__141)
    var t990 *_goml_vec_EnvEntry = env_empty()
    var t991 Exp = exp_var("x")
    var t992 Exp = exp_var("y")
    var t993 Exp = exp_let("y", t991, t992)
    var t994 Exp = exp_lam("x", t993)
    var t995 Result__Typ__string = typeof(st__141, t990, t994)
    show_result("sound_gen_1", t995)
    reset_type_variables(st__141)
    var t996 *_goml_vec_EnvEntry = env_empty()
    var t997 Exp = exp_var("x")
    var t998 Exp = exp_lam("z", t997)
    var t999 Exp = exp_var("y")
    var t1000 Exp = exp_let("y", t998, t999)
    var t1001 Exp = exp_lam("x", t1000)
    var t1002 Result__Typ__string = typeof(st__141, t996, t1001)
    show_result("sound_gen_2", t1002)
    reset_type_variables(st__141)
    var t1003 *_goml_vec_EnvEntry = env_empty()
    var t1004 Exp = exp_var("x")
    var t1005 Exp = exp_var("z")
    var t1006 Exp = exp_app(t1004, t1005)
    var t1007 Exp = exp_lam("z", t1006)
    var t1008 Exp = exp_var("y")
    var t1009 Exp = exp_let("y", t1007, t1008)
    var t1010 Exp = exp_lam("x", t1009)
    var t1011 Result__Typ__string = typeof(st__141, t1003, t1010)
    show_result("sound_gen_3", t1011)
    reset_type_variables(st__141)
    var t1012 *_goml_vec_EnvEntry = env_empty()
    var t1013 Exp = exp_var("x")
    var t1014 Exp = exp_var("y")
    var t1015 Exp = exp_app(t1013, t1014)
    var t1016 Exp = exp_var("x")
    var t1017 Exp = exp_var("y")
    var t1018 Exp = exp_app(t1016, t1017)
    var t1019 Exp = exp_let("x", t1015, t1018)
    var t1020 Exp = exp_lam("y", t1019)
    var t1021 Exp = exp_lam("x", t1020)
    var t1022 Result__Typ__string = typeof(st__141, t1012, t1021)
    show_result("double_apply", t1022)
    reset_type_variables(st__141)
    var t1023 *_goml_vec_EnvEntry = env_empty()
    var t1024 Exp = exp_var("x")
    var t1025 Exp = exp_var("y")
    var t1026 Exp = exp_var("y")
    var t1027 Exp
    var inline1413 Exp = App{
        _0: t1025,
        _1: t1026,
    }
    t1027 = inline1413
    var t1028 Exp
    var inline1410 string = "y"
    var inline1411 Exp = Let{
        _0: inline1410,
        _1: t1024,
        _2: t1027,
    }
    t1028 = inline1411
    var t1029 Exp
    var inline1407 string = "x"
    var inline1408 Exp = Lam{
        _0: inline1407,
        _1: t1028,
    }
    t1029 = inline1408
    var t1030 Result__Typ__string = typeof(st__141, t1023, t1029)
    show_result("sound_gen_occurs", t1030)
    var inline1404 *ref_int32_x = st__141.gensym_counter
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline1404, 0)
    var t1031 *_goml_vec_EnvEntry
    var inline1402 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__EnvEntry()
    t1031 = inline1402
    var t1032 Exp
    var inline1399 string = "x"
    var inline1400 Exp = Var{
        _0: inline1399,
    }
    t1032 = inline1400
    var t1033 Exp
    var inline1397 Exp = App{
        _0: t1032,
        _1: id__142,
    }
    t1033 = inline1397
    var t1034 Exp
    var inline1394 string = "z"
    var inline1395 Exp = Var{
        _0: inline1394,
    }
    t1034 = inline1395
    var t1035 Exp
    var inline1391 string = "z"
    var inline1392 Exp = Let{
        _0: inline1391,
        _1: t1033,
        _2: t1034,
    }
    t1035 = inline1392
    var t1036 Exp
    var inline1388 string = "y"
    var inline1389 Exp = Var{
        _0: inline1388,
    }
    t1036 = inline1389
    var t1037 Exp
    var inline1385 string = "y"
    var inline1386 Exp = Let{
        _0: inline1385,
        _1: t1035,
        _2: t1036,
    }
    t1037 = inline1386
    var t1038 Exp
    var inline1382 string = "x"
    var inline1383 Exp = Lam{
        _0: inline1382,
        _1: t1037,
    }
    t1038 = inline1383
    var t1039 Result__Typ__string = typeof(st__141, t1031, t1038)
    var inline1369 string = "fun_x_let_y_let_z_x_id_z_y"
    switch t1039.(type) {
    case Result__Typ__string_Ok:
        var inline1370 Typ = t1039.(Result__Typ__string_Ok)._0
        var inline1372 string = inline1369 + ": "
        var inline1373 string = typ_to_string(inline1370)
        var inline1374 string = inline1372 + inline1373
        println__T_string(inline1374)
    case Result__Typ__string_Err:
        var inline1376 string = t1039.(Result__Typ__string_Err)._0
        var inline1378 string = inline1369 + ": "
        var inline1379 string = inline1378 + inline1376
        println__T_string(inline1379)
    default:
        panic("non-exhaustive match")
    }
    var inline1365 string = ""
    var inline1366 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline1365)
    _goml_runtime_core_string_println(inline1366)
    var inline1361 string = "All Done"
    var inline1362 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline1361)
    _goml_runtime_core_string_println(inline1362)
    var inline1357 string = ""
    var inline1358 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline1357)
    _goml_runtime_core_string_println(inline1358)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__433 *ref_int32_x, value__434 int32) struct{} {
    ref_set__Ref_5int32(self__433, value__434)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__432 *ref_int32_x) int32 {
    var t1047 int32 = ref_get__Ref_5int32(self__432)
    return t1047
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Tv(value__431 Tv) *ref_Tv_x {
    var t1056 *ref_Tv_x = ref__Ref_2Tv(value__431)
    return t1056
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__EnvEntry() *_goml_vec_EnvEntry {
    var t1062 *_goml_vec_EnvEntry = vec_new__Vec_8EnvEntry()
    return t1062
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SubstEntry(self__260 *_goml_vec_SubstEntry, elem__261 SubstEntry) *_goml_vec_SubstEntry {
    var t1097 int
    var inline1428 int = vec_len__Vec_10SubstEntry(self__260)
    t1097 = inline1428
    var t1098 int = t1097 + 1
    var result__262 *_goml_vec_SubstEntry
    var inline1426 *_goml_vec_SubstEntry = vec_with_capacity__Vec_10SubstEntry(t1098)
    result__262 = inline1426
    var index__263 int = 0
    Loop_loop1100:
    for {
        var t1101 int
        var inline1422 int = vec_len__Vec_10SubstEntry(self__260)
        t1101 = inline1422
        var t1102 bool = index__263 < t1101
        if t1102 {
            var t1103 SubstEntry = vec_get__Vec_10SubstEntry(self__260, index__263)
            vec_push__Vec_10SubstEntry(result__262, t1103)
            var compound_old190 int = index__263
            var compound_value191 int = 1
            var t1104 int = compound_old190 + compound_value191
            index__263 = t1104
            continue
        } else {
            break Loop_loop1100
        }
    }
    vec_push__Vec_10SubstEntry(result__262, elem__261)
    return result__262
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SubstEntry() *_goml_vec_SubstEntry {
    var t1108 *_goml_vec_SubstEntry = vec_new__Vec_10SubstEntry()
    return t1108
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__EnvEntry(self__260 *_goml_vec_EnvEntry, elem__261 EnvEntry) *_goml_vec_EnvEntry {
    var t1111 int
    var inline1438 int = vec_len__Vec_8EnvEntry(self__260)
    t1111 = inline1438
    var t1112 int = t1111 + 1
    var result__262 *_goml_vec_EnvEntry
    var inline1436 *_goml_vec_EnvEntry = vec_with_capacity__Vec_8EnvEntry(t1112)
    result__262 = inline1436
    var index__263 int = 0
    Loop_loop1114:
    for {
        var t1115 int
        var inline1432 int = vec_len__Vec_8EnvEntry(self__260)
        t1115 = inline1432
        var t1116 bool = index__263 < t1115
        if t1116 {
            var t1117 EnvEntry = vec_get__Vec_8EnvEntry(self__260, index__263)
            vec_push__Vec_8EnvEntry(result__262, t1117)
            var compound_old190 int = index__263
            var compound_value191 int = 1
            var t1118 int = compound_old190 + compound_value191
            index__263 = t1118
            continue
        } else {
            break Loop_loop1114
        }
    }
    vec_push__Vec_8EnvEntry(result__262, elem__261)
    return result__262
}

func println__T_string(value__1 string) struct{} {
    var t1121 string
    t1121 = value__1
    _goml_runtime_core_string_println(t1121)
    return struct{}{}
}

func char_to_string(value__29 rune) string {
    var t1127 uint32 = uint32(rune(value__29))
    var t1128 bool
    var inline1441 bool = t1127 <= 1114111
    if inline1441 {
        var inline1442 bool = t1127 >= 55296
        var inline1444 bool
        if inline1442 {
            var inline1446 bool = t1127 <= 57343
            inline1444 = inline1446
        } else {
            inline1444 = false
        }
        var inline1445 bool = !inline1444
        t1128 = inline1445
    } else {
        t1128 = false
    }
    if t1128 {
        var t1129 string = _goml_runtime_core_char_to_string(value__29)
        return t1129
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
