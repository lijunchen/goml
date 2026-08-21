package main

import (
    _goml_fmt "fmt"
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
        items: make([]EnvEntry, 0, capacity),
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
        items: make([]SubstEntry, 0, capacity),
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

type Option__Typ struct {
    _tag int32
    _v1_0 Typ
}

type Result__unit__string struct {
    _tag int32
    _v0_0 struct{}
    _v1_0 string
}

type Result__Typ__string struct {
    _tag int32
    _v0_0 Typ
    _v1_0 string
}

func state_new() CheckerState {
    var t609 *ref_int32_x
    var inline1178 int32 = 0
    var inline1179 *ref_int32_x = ref__Ref_5int32(inline1178)
    t609 = inline1179
    var t610 *ref_int32_x
    var inline1175 int32 = 1
    var inline1176 *ref_int32_x = ref__Ref_5int32(inline1175)
    t610 = inline1176
    var t611 CheckerState = CheckerState{
        gensym_counter: t609,
        current_level: t610,
    }
    return t611
}

func reset_type_variables(st__2 CheckerState) struct{} {
    var inline1190 *ref_int32_x = st__2.gensym_counter
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline1190, 0)
    var inline1187 *ref_int32_x = st__2.current_level
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline1187, 1)
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
    var t637 *ref_int32_x = st__10.gensym_counter
    var n__11 int32
    var inline1207 int32 = ref_get__Ref_5int32(t637)
    n__11 = inline1207
    var t638 *ref_int32_x = st__10.gensym_counter
    var t639 int32 = n__11 + 1
    ref_set__Ref_5int32(t638, t639)
    var t642 bool = n__11 < 26
    if t642 {
        var t643 rune = nth_letter(n__11)
        var inline1201 string = char_to_string(t643)
        return inline1201
    } else {
        var t645 string
        var inline1203 string = _goml_runtime_core_int32_to_string(n__11)
        t645 = inline1203
        var t646 string = "t" + t645
        return t646
    }
}

func typ_is_arrow(ty__15 Typ) bool {
    switch ty__15.(type) {
    case TVar:
        var x417 *ref_Tv_x = ty__15.(TVar)._0
        var mtmp421 Tv
        var inline1224 Tv = ref_get__Ref_2Tv(x417)
        mtmp421 = inline1224
        switch mtmp421.(type) {
        case Link:
            var x424 Typ = mtmp421.(Link)._0
            var t659 bool = typ_is_arrow(x424)
            return t659
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
        var x425 *ref_Tv_x = ty__18.(TVar)._0
        var mtmp429 Tv
        var inline1226 Tv = ref_get__Ref_2Tv(x425)
        mtmp429 = inline1226
        switch mtmp429.(type) {
        case Unbound:
            var x430 string = mtmp429.(Unbound)._0
            var t666 string = "'" + x430
            return t666
        case Link:
            var x432 Typ = mtmp429.(Link)._0
            var t667 string = typ_to_string(x432)
            return t667
        default:
            panic("non-exhaustive match")
        }
    case QVar:
        var x426 string = ty__18.(QVar)._0
        var t668 string = "'" + x426
        return t668
    case TArrow:
        var x427 Typ = ty__18.(TArrow)._0
        var x428 Typ = ty__18.(TArrow)._1
        var t673 bool = typ_is_arrow(x427)
        var jp670 string
        if t673 {
            var t674 string = typ_to_string(x427)
            var t675 string = "(" + t674
            var t676 string = t675 + ")"
            jp670 = t676
        } else {
            var t677 string = typ_to_string(x427)
            jp670 = t677
        }
        var s2__26 string = typ_to_string(x428)
        var t671 string = jp670 + " -> "
        var t672 string = t671 + s2__26
        return t672
    default:
        panic("non-exhaustive match")
    }
}

func env_empty() *_goml_vec_EnvEntry {
    var inline1228 *_goml_vec_EnvEntry = vec_new__Vec_8EnvEntry()
    return inline1228
}

func env_lookup(env__28 *_goml_vec_EnvEntry, name__29 string) Option__Typ {
    var t682 int
    var inline1254 int = vec_len__Vec_8EnvEntry(env__28)
    t682 = inline1254
    var t683 int = t682 - 1
    var i__30 *ref_int_x
    var inline1252 *ref_int_x = ref__Ref_3int(t683)
    i__30 = inline1252
    var found__31 *ref_Option__Typ_x
    var inline1250 *ref_Option__Typ_x = ref__Ref_11Option__Typ(Option__Typ{
        _tag: 0,
    })
    found__31 = inline1250
    var done__32 *ref_bool_x
    var inline1247 bool = false
    var inline1248 *ref_bool_x = ref__Ref_4bool(inline1247)
    done__32 = inline1248
    Loop_loop686:
    for {
        var t699 bool
        var inline1243 bool = ref_get__Ref_4bool(done__32)
        t699 = inline1243
        var t700 bool = !t699
        var jp688 bool
        if t700 {
            var t701 int
            var inline1230 int = ref_get__Ref_3int(i__30)
            t701 = inline1230
            var t702 bool = t701 >= 0
            jp688 = t702
        } else {
            jp688 = false
        }
        if jp688 {
            var t689 int
            var inline1241 int = ref_get__Ref_3int(i__30)
            t689 = inline1241
            var entry__33 EnvEntry = vec_get__Vec_8EnvEntry(env__28, t689)
            var t691 string = entry__33.name
            var t692 bool = t691 == name__29
            if t692 {
                var t693 Typ = entry__33.ty
                var t694 Option__Typ = Option__Typ{
                    _tag: 1,
                    _v1_0: t693,
                }
                ref_set__Ref_11Option__Typ(found__31, t694)
                var inline1232 bool = true
                ref_set__Ref_4bool(done__32, inline1232)
                continue
            } else {
                var t696 int
                var inline1239 int = ref_get__Ref_3int(i__30)
                t696 = inline1239
                var t697 int = t696 - 1
                ref_set__Ref_3int(i__30, t697)
                continue
            }
        } else {
            break Loop_loop686
        }
    }
    var inline1245 Option__Typ = ref_get__Ref_11Option__Typ(found__31)
    return inline1245
}

func subst_lookup(subst__34 *_goml_vec_SubstEntry, name__35 string) Option__Typ {
    var t705 int
    var inline1280 int = vec_len__Vec_10SubstEntry(subst__34)
    t705 = inline1280
    var t706 int = t705 - 1
    var i__36 *ref_int_x
    var inline1278 *ref_int_x = ref__Ref_3int(t706)
    i__36 = inline1278
    var found__37 *ref_Option__Typ_x
    var inline1276 *ref_Option__Typ_x = ref__Ref_11Option__Typ(Option__Typ{
        _tag: 0,
    })
    found__37 = inline1276
    var done__38 *ref_bool_x
    var inline1273 bool = false
    var inline1274 *ref_bool_x = ref__Ref_4bool(inline1273)
    done__38 = inline1274
    Loop_loop709:
    for {
        var t722 bool
        var inline1269 bool = ref_get__Ref_4bool(done__38)
        t722 = inline1269
        var t723 bool = !t722
        var jp711 bool
        if t723 {
            var t724 int
            var inline1256 int = ref_get__Ref_3int(i__36)
            t724 = inline1256
            var t725 bool = t724 >= 0
            jp711 = t725
        } else {
            jp711 = false
        }
        if jp711 {
            var t712 int
            var inline1267 int = ref_get__Ref_3int(i__36)
            t712 = inline1267
            var entry__39 SubstEntry = vec_get__Vec_10SubstEntry(subst__34, t712)
            var t714 string = entry__39.name
            var t715 bool = t714 == name__35
            if t715 {
                var t716 Typ = entry__39.ty
                var t717 Option__Typ = Option__Typ{
                    _tag: 1,
                    _v1_0: t716,
                }
                ref_set__Ref_11Option__Typ(found__37, t717)
                var inline1258 bool = true
                ref_set__Ref_4bool(done__38, inline1258)
                continue
            } else {
                var t719 int
                var inline1265 int = ref_get__Ref_3int(i__36)
                t719 = inline1265
                var t720 int = t719 - 1
                ref_set__Ref_3int(i__36, t720)
                continue
            }
        } else {
            break Loop_loop709
        }
    }
    var inline1271 Option__Typ = ref_get__Ref_11Option__Typ(found__37)
    return inline1271
}

func occurs(st__40 CheckerState, tvr__41 *ref_Tv_x, ty__42 Typ) Result__unit__string {
    switch ty__42.(type) {
    case TVar:
        var x437 *ref_Tv_x = ty__42.(TVar)._0
        var t732 bool = ptr_eq__Ref_2Tv(tvr__41, x437)
        if t732 {
            var t733 Result__unit__string = Result__unit__string{
                _tag: 1,
                _v1_0: "occurs check",
            }
            return t733
        } else {
            var mtmp441 Tv
            var inline1288 Tv = ref_get__Ref_2Tv(x437)
            mtmp441 = inline1288
            switch mtmp441.(type) {
            case Unbound:
                var x442 string = mtmp441.(Unbound)._0
                var x443 int32 = mtmp441.(Unbound)._1
                var mtmp445 Tv
                var inline1286 Tv = ref_get__Ref_2Tv(tvr__41)
                mtmp445 = inline1286
                var jp737 int32
                switch mtmp445.(type) {
                case Unbound:
                    var x447 int32 = mtmp445.(Unbound)._1
                    var inline1282 bool = x447 < x443
                    if inline1282 {
                        jp737 = x447
                    } else {
                        jp737 = x443
                    }
                default:
                    jp737 = x443
                }
                var t738 Tv = Unbound{
                    _0: x442,
                    _1: jp737,
                }
                ref_set__Ref_2Tv(x437, t738)
                var t739 Result__unit__string = Result__unit__string{
                    _tag: 0,
                    _v0_0: struct{}{},
                }
                return t739
            case Link:
                var x444 Typ = mtmp441.(Link)._0
                var t741 Result__unit__string = occurs(st__40, tvr__41, x444)
                return t741
            default:
                panic("non-exhaustive match")
            }
        }
    case TArrow:
        var x439 Typ = ty__42.(TArrow)._0
        var x440 Typ = ty__42.(TArrow)._1
        var mtmp450 Result__unit__string = occurs(st__40, tvr__41, x439)
        switch mtmp450._tag {
        case 0:
            var t744 Result__unit__string = occurs(st__40, tvr__41, x440)
            return t744
        case 1:
            var x452 string = mtmp450._v1_0
            var t745 Result__unit__string = Result__unit__string{
                _tag: 1,
                _v1_0: x452,
            }
            return t745
        default:
            panic("non-exhaustive match")
        }
    default:
        var t746 Result__unit__string = Result__unit__string{
            _tag: 0,
            _v0_0: struct{}{},
        }
        return t746
    }
}

func unify(st__52 CheckerState, t1__53 Typ, t2__54 Typ) Result__unit__string {
    switch t2__54.(type) {
    case TVar:
        var x456 *ref_Tv_x = t2__54.(TVar)._0
        switch t1__53.(type) {
        case TVar:
            var x460 *ref_Tv_x = t1__53.(TVar)._0
            var t755 bool = ptr_eq__Ref_2Tv(x460, x456)
            if t755 {
                var t756 Result__unit__string = Result__unit__string{
                    _tag: 0,
                    _v0_0: struct{}{},
                }
                return t756
            } else {
                var mtmp464 Tv
                var inline1294 Tv = ref_get__Ref_2Tv(x460)
                mtmp464 = inline1294
                switch mtmp464.(type) {
                case Unbound:
                    var mtmp468 Tv
                    var inline1292 Tv = ref_get__Ref_2Tv(x456)
                    mtmp468 = inline1292
                    switch mtmp468.(type) {
                    case Unbound:
                        var t761 Typ = TVar{
                            _0: x456,
                        }
                        var mtmp472 Result__unit__string = occurs(st__52, x460, t761)
                        switch mtmp472._tag {
                        case 0:
                            var t764 Typ = TVar{
                                _0: x456,
                            }
                            var t765 Tv = Link{
                                _0: t764,
                            }
                            ref_set__Ref_2Tv(x460, t765)
                            var t766 Result__unit__string = Result__unit__string{
                                _tag: 0,
                                _v0_0: struct{}{},
                            }
                            return t766
                        case 1:
                            var x474 string = mtmp472._v1_0
                            var t767 Result__unit__string = Result__unit__string{
                                _tag: 1,
                                _v1_0: x474,
                            }
                            return t767
                        default:
                            panic("non-exhaustive match")
                        }
                    case Link:
                        var x471 Typ = mtmp468.(Link)._0
                        var t768 Typ = TVar{
                            _0: x460,
                        }
                        var t769 Result__unit__string = unify(st__52, t768, x471)
                        return t769
                    default:
                        panic("non-exhaustive match")
                    }
                case Link:
                    var x467 Typ = mtmp464.(Link)._0
                    var t770 Typ = TVar{
                        _0: x456,
                    }
                    var t771 Result__unit__string = unify(st__52, x467, t770)
                    return t771
                default:
                    panic("non-exhaustive match")
                }
            }
        default:
            var mtmp476 Tv
            var inline1298 Tv = ref_get__Ref_2Tv(x456)
            mtmp476 = inline1298
            switch mtmp476.(type) {
            case Unbound:
                var mtmp480 Result__unit__string = occurs(st__52, x456, t1__53)
                switch mtmp480._tag {
                case 0:
                    var t776 Tv = Link{
                        _0: t1__53,
                    }
                    ref_set__Ref_2Tv(x456, t776)
                    var t777 Result__unit__string = Result__unit__string{
                        _tag: 0,
                        _v0_0: struct{}{},
                    }
                    return t777
                case 1:
                    var x482 string = mtmp480._v1_0
                    var t778 Result__unit__string = Result__unit__string{
                        _tag: 1,
                        _v1_0: x482,
                    }
                    return t778
                default:
                    panic("non-exhaustive match")
                }
            case Link:
                var x479 Typ = mtmp476.(Link)._0
                var t779 Result__unit__string = unify(st__52, t1__53, x479)
                return t779
            default:
                panic("non-exhaustive match")
            }
        }
    case TArrow:
        var x458 Typ = t2__54.(TArrow)._0
        var x459 Typ = t2__54.(TArrow)._1
        switch t1__53.(type) {
        case TVar:
            var x484 *ref_Tv_x = t1__53.(TVar)._0
            var mtmp488 Tv
            var inline1302 Tv = ref_get__Ref_2Tv(x484)
            mtmp488 = inline1302
            switch mtmp488.(type) {
            case Unbound:
                var mtmp492 Result__unit__string = occurs(st__52, x484, t2__54)
                switch mtmp492._tag {
                case 0:
                    var t786 Tv = Link{
                        _0: t2__54,
                    }
                    ref_set__Ref_2Tv(x484, t786)
                    var t787 Result__unit__string = Result__unit__string{
                        _tag: 0,
                        _v0_0: struct{}{},
                    }
                    return t787
                case 1:
                    var x494 string = mtmp492._v1_0
                    var t788 Result__unit__string = Result__unit__string{
                        _tag: 1,
                        _v1_0: x494,
                    }
                    return t788
                default:
                    panic("non-exhaustive match")
                }
            case Link:
                var x491 Typ = mtmp488.(Link)._0
                var t789 Result__unit__string = unify(st__52, x491, t2__54)
                return t789
            default:
                panic("non-exhaustive match")
            }
        case TArrow:
            var x486 Typ = t1__53.(TArrow)._0
            var x487 Typ = t1__53.(TArrow)._1
            var mtmp496 Result__unit__string = unify(st__52, x486, x458)
            switch mtmp496._tag {
            case 0:
                var t792 Result__unit__string = unify(st__52, x487, x459)
                return t792
            case 1:
                var x498 string = mtmp496._v1_0
                var t793 Result__unit__string = Result__unit__string{
                    _tag: 1,
                    _v1_0: x498,
                }
                return t793
            default:
                panic("non-exhaustive match")
            }
        default:
            var t794 Result__unit__string = Result__unit__string{
                _tag: 1,
                _v1_0: "unify error",
            }
            return t794
        }
    default:
        switch t1__53.(type) {
        case TVar:
            var x499 *ref_Tv_x = t1__53.(TVar)._0
            var mtmp503 Tv
            var inline1306 Tv = ref_get__Ref_2Tv(x499)
            mtmp503 = inline1306
            switch mtmp503.(type) {
            case Unbound:
                var mtmp507 Result__unit__string = occurs(st__52, x499, t2__54)
                switch mtmp507._tag {
                case 0:
                    var t801 Tv = Link{
                        _0: t2__54,
                    }
                    ref_set__Ref_2Tv(x499, t801)
                    var t802 Result__unit__string = Result__unit__string{
                        _tag: 0,
                        _v0_0: struct{}{},
                    }
                    return t802
                case 1:
                    var x509 string = mtmp507._v1_0
                    var t803 Result__unit__string = Result__unit__string{
                        _tag: 1,
                        _v1_0: x509,
                    }
                    return t803
                default:
                    panic("non-exhaustive match")
                }
            case Link:
                var x506 Typ = mtmp503.(Link)._0
                var t804 Result__unit__string = unify(st__52, x506, t2__54)
                return t804
            default:
                panic("non-exhaustive match")
            }
        default:
            var t805 Result__unit__string = Result__unit__string{
                _tag: 1,
                _v1_0: "unify error",
            }
            return t805
        }
    }
}

func gen(st__73 CheckerState, ty__74 Typ) Typ {
    switch ty__74.(type) {
    case TVar:
        var x511 *ref_Tv_x = ty__74.(TVar)._0
        var mtmp515 Tv
        var inline1310 Tv = ref_get__Ref_2Tv(x511)
        mtmp515 = inline1310
        switch mtmp515.(type) {
        case Unbound:
            var x516 string = mtmp515.(Unbound)._0
            var x517 int32 = mtmp515.(Unbound)._1
            var t812 *ref_int32_x = st__73.current_level
            var cur__78 int32
            var inline1308 int32 = ref_get__Ref_5int32(t812)
            cur__78 = inline1308
            var t815 bool = x517 > cur__78
            if t815 {
                var t816 Typ = QVar{
                    _0: x516,
                }
                return t816
            } else {
                var t817 Typ = TVar{
                    _0: x511,
                }
                return t817
            }
        case Link:
            var x518 Typ = mtmp515.(Link)._0
            var t818 Typ = gen(st__73, x518)
            return t818
        default:
            panic("non-exhaustive match")
        }
    case TArrow:
        var x513 Typ = ty__74.(TArrow)._0
        var x514 Typ = ty__74.(TArrow)._1
        var t819 Typ = gen(st__73, x513)
        var t820 Typ = gen(st__73, x514)
        var t821 Typ = TArrow{
            _0: t819,
            _1: t820,
        }
        return t821
    default:
        return ty__74
    }
}

func inst_loop(st__83 CheckerState, subst__84 *_goml_vec_SubstEntry, ty__85 Typ) Tuple2_3Typ_16Vec_10SubstEntry {
    switch ty__85.(type) {
    case TVar:
        var x519 *ref_Tv_x = ty__85.(TVar)._0
        var mtmp523 Tv
        var inline1312 Tv = ref_get__Ref_2Tv(x519)
        mtmp523 = inline1312
        switch mtmp523.(type) {
        case Link:
            var x526 Typ = mtmp523.(Link)._0
            var t828 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, subst__84, x526)
            return t828
        default:
            var t829 Typ = TVar{
                _0: x519,
            }
            var t830 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: t829,
                _1: subst__84,
            }
            return t830
        }
    case QVar:
        var x520 string = ty__85.(QVar)._0
        var mtmp527 Option__Typ = subst_lookup(subst__84, x520)
        switch mtmp527._tag {
        case 0:
            var tv__88 Typ
            var inline1314 string = gensym(st__83)
            var inline1315 *ref_int32_x = st__83.current_level
            var inline1316 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline1315)
            var inline1317 Tv = Unbound{
                _0: inline1314,
                _1: inline1316,
            }
            var inline1318 *ref_Tv_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Tv(inline1317)
            var inline1319 Typ = TVar{
                _0: inline1318,
            }
            tv__88 = inline1319
            var t833 SubstEntry = SubstEntry{
                name: x520,
                ty: tv__88,
            }
            var new_subst__89 *_goml_vec_SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SubstEntry(subst__84, t833)
            var t834 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: tv__88,
                _1: new_subst__89,
            }
            return t834
        case 1:
            var x528 Typ = mtmp527._v1_0
            var t835 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: x528,
                _1: subst__84,
            }
            return t835
        default:
            panic("non-exhaustive match")
        }
    case TArrow:
        var x521 Typ = ty__85.(TArrow)._0
        var x522 Typ = ty__85.(TArrow)._1
        var mtmp529 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, subst__84, x521)
        var x530 Typ = mtmp529._0
        var x531 *_goml_vec_SubstEntry = mtmp529._1
        var mtmp532 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, x531, x522)
        var x533 Typ = mtmp532._0
        var x534 *_goml_vec_SubstEntry = mtmp532._1
        var t836 Typ = TArrow{
            _0: x530,
            _1: x533,
        }
        var t837 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
            _0: t836,
            _1: x534,
        }
        return t837
    default:
        panic("non-exhaustive match")
    }
}

func typeof(st__102 CheckerState, env__103 *_goml_vec_EnvEntry, e__104 Exp) Result__Typ__string {
    switch e__104.(type) {
    case Var:
        var x538 string = e__104.(Var)._0
        var mtmp546 Option__Typ = env_lookup(env__103, x538)
        switch mtmp546._tag {
        case 0:
            var t846 Result__Typ__string = Result__Typ__string{
                _tag: 1,
                _v1_0: "unbound var",
            }
            return t846
        case 1:
            var x547 Typ = mtmp546._v1_0
            var t847 Typ
            var inline1323 *_goml_vec_SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SubstEntry()
            var inline1324 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__102, inline1323, x547)
            var inline1325 Typ = inline1324._0
            t847 = inline1325
            var t848 Result__Typ__string = Result__Typ__string{
                _tag: 0,
                _v0_0: t847,
            }
            return t848
        default:
            panic("non-exhaustive match")
        }
    case App:
        var x539 Exp = e__104.(App)._0
        var x540 Exp = e__104.(App)._1
        var mtmp548 Result__Typ__string = typeof(st__102, env__103, x539)
        switch mtmp548._tag {
        case 0:
            var x549 Typ = mtmp548._v0_0
            var mtmp551 Result__Typ__string = typeof(st__102, env__103, x540)
            switch mtmp551._tag {
            case 0:
                var x552 Typ = mtmp551._v0_0
                var ty_res__119 Typ
                var inline1328 string = gensym(st__102)
                var inline1329 *ref_int32_x = st__102.current_level
                var inline1330 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline1329)
                var inline1331 Tv = Unbound{
                    _0: inline1328,
                    _1: inline1330,
                }
                var inline1332 *ref_Tv_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Tv(inline1331)
                var inline1333 Typ = TVar{
                    _0: inline1332,
                }
                ty_res__119 = inline1333
                var arrow__120 Typ = TArrow{
                    _0: x552,
                    _1: ty_res__119,
                }
                var mtmp554 Result__unit__string = unify(st__102, x549, arrow__120)
                switch mtmp554._tag {
                case 0:
                    var t855 Result__Typ__string = Result__Typ__string{
                        _tag: 0,
                        _v0_0: ty_res__119,
                    }
                    return t855
                case 1:
                    var x556 string = mtmp554._v1_0
                    var t856 Result__Typ__string = Result__Typ__string{
                        _tag: 1,
                        _v1_0: x556,
                    }
                    return t856
                default:
                    panic("non-exhaustive match")
                }
            case 1:
                var x553 string = mtmp551._v1_0
                var t857 Result__Typ__string = Result__Typ__string{
                    _tag: 1,
                    _v1_0: x553,
                }
                return t857
            default:
                panic("non-exhaustive match")
            }
        case 1:
            var x550 string = mtmp548._v1_0
            var t858 Result__Typ__string = Result__Typ__string{
                _tag: 1,
                _v1_0: x550,
            }
            return t858
        default:
            panic("non-exhaustive match")
        }
    case Lam:
        var x541 string = e__104.(Lam)._0
        var x542 Exp = e__104.(Lam)._1
        var ty_x__109 Typ
        var inline1335 string = gensym(st__102)
        var inline1336 *ref_int32_x = st__102.current_level
        var inline1337 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline1336)
        var inline1338 Tv = Unbound{
            _0: inline1335,
            _1: inline1337,
        }
        var inline1339 *ref_Tv_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Tv(inline1338)
        var inline1340 Typ = TVar{
            _0: inline1339,
        }
        ty_x__109 = inline1340
        var t859 EnvEntry = EnvEntry{
            name: x541,
            ty: ty_x__109,
        }
        var env2__110 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__EnvEntry(env__103, t859)
        var mtmp557 Result__Typ__string = typeof(st__102, env2__110, x542)
        switch mtmp557._tag {
        case 0:
            var x558 Typ = mtmp557._v0_0
            var t862 Typ = TArrow{
                _0: ty_x__109,
                _1: x558,
            }
            var t863 Result__Typ__string = Result__Typ__string{
                _tag: 0,
                _v0_0: t862,
            }
            return t863
        case 1:
            var x559 string = mtmp557._v1_0
            var t864 Result__Typ__string = Result__Typ__string{
                _tag: 1,
                _v1_0: x559,
            }
            return t864
        default:
            panic("non-exhaustive match")
        }
    case Let:
        var x543 string = e__104.(Let)._0
        var x544 Exp = e__104.(Let)._1
        var x545 Exp = e__104.(Let)._2
        var inline1348 *ref_int32_x = st__102.current_level
        var inline1349 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline1348)
        var inline1350 *ref_int32_x = st__102.current_level
        var inline1351 int32 = inline1349 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline1350, inline1351)
        var ty_e__125 Result__Typ__string = typeof(st__102, env__103, x544)
        var inline1342 *ref_int32_x = st__102.current_level
        var inline1343 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(inline1342)
        var inline1344 *ref_int32_x = st__102.current_level
        var inline1345 int32 = inline1343 - 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline1344, inline1345)
        switch ty_e__125._tag {
        case 0:
            var x562 Typ = ty_e__125._v0_0
            var t867 Typ = gen(st__102, x562)
            var t868 EnvEntry = EnvEntry{
                name: x543,
                ty: t867,
            }
            var env2__128 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__EnvEntry(env__103, t868)
            var t869 Result__Typ__string = typeof(st__102, env2__128, x545)
            return t869
        case 1:
            var x563 string = ty_e__125._v1_0
            var t870 Result__Typ__string = Result__Typ__string{
                _tag: 1,
                _v1_0: x563,
            }
            return t870
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func exp_var(name__129 string) Exp {
    var t873 Exp = Var{
        _0: name__129,
    }
    return t873
}

func exp_lam(name__130 string, body__131 Exp) Exp {
    var t876 Exp = Lam{
        _0: name__130,
        _1: body__131,
    }
    return t876
}

func exp_app(a__132 Exp, b__133 Exp) Exp {
    var t879 Exp = App{
        _0: a__132,
        _1: b__133,
    }
    return t879
}

func exp_let(name__134 string, a__135 Exp, b__136 Exp) Exp {
    var t882 Exp = Let{
        _0: name__134,
        _1: a__135,
        _2: b__136,
    }
    return t882
}

func show_result(label__137 string, res__138 Result__Typ__string) struct{} {
    switch res__138._tag {
    case 0:
        var x564 Typ = res__138._v0_0
        var t885 string = label__137 + ": "
        var t886 string = typ_to_string(x564)
        var t887 string = t885 + t886
        var inline1354 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t887)
        _goml_runtime_core_string_println(inline1354)
        return struct{}{}
    case 1:
        var x565 string = res__138._v1_0
        var t889 string = label__137 + ": "
        var t890 string = t889 + x565
        var inline1357 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t890)
        _goml_runtime_core_string_println(inline1357)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var st__141 CheckerState = state_new()
    var t893 Exp = exp_var("x")
    var id__142 Exp = exp_lam("x", t893)
    var t894 Exp = exp_var("x")
    var t895 Exp = exp_var("y")
    var t896 Exp = exp_app(t894, t895)
    var t897 Exp = exp_lam("y", t896)
    var c1__143 Exp = exp_lam("x", t897)
    reset_type_variables(st__141)
    var t898 *_goml_vec_EnvEntry = env_empty()
    var t899 Result__Typ__string = typeof(st__141, t898, id__142)
    show_result("id", t899)
    reset_type_variables(st__141)
    var t900 *_goml_vec_EnvEntry = env_empty()
    var t901 Result__Typ__string = typeof(st__141, t900, c1__143)
    show_result("c1", t901)
    reset_type_variables(st__141)
    var t902 *_goml_vec_EnvEntry = env_empty()
    var t903 Exp = exp_var("x")
    var t904 Exp = exp_let("x", c1__143, t903)
    var t905 Result__Typ__string = typeof(st__141, t902, t904)
    show_result("let_x_c1_x", t905)
    reset_type_variables(st__141)
    var t906 *_goml_vec_EnvEntry = env_empty()
    var t907 Exp = exp_var("z")
    var t908 Exp = exp_lam("z", t907)
    var t909 Exp = exp_var("y")
    var t910 Exp = exp_let("y", t908, t909)
    var t911 Result__Typ__string = typeof(st__141, t906, t910)
    show_result("let_y_id_y", t911)
    reset_type_variables(st__141)
    var t912 *_goml_vec_EnvEntry = env_empty()
    var t913 Exp = exp_var("z")
    var t914 Exp = exp_lam("z", t913)
    var t915 Exp = exp_var("y")
    var t916 Exp = exp_let("y", t914, t915)
    var t917 Exp = exp_lam("x", t916)
    var t918 Result__Typ__string = typeof(st__141, t912, t917)
    show_result("lam_x_let_y_id_y", t918)
    reset_type_variables(st__141)
    var t919 *_goml_vec_EnvEntry = env_empty()
    var t920 Exp = exp_var("z")
    var t921 Exp = exp_lam("z", t920)
    var t922 Exp = exp_var("y")
    var t923 Exp = exp_var("x")
    var t924 Exp = exp_app(t922, t923)
    var t925 Exp = exp_let("y", t921, t924)
    var t926 Exp = exp_lam("x", t925)
    var t927 Result__Typ__string = typeof(st__141, t919, t926)
    show_result("lam_x_let_y_id_yx", t927)
    reset_type_variables(st__141)
    var t928 *_goml_vec_EnvEntry = env_empty()
    var t929 Exp = exp_var("x")
    var t930 Exp = exp_var("x")
    var t931 Exp = exp_app(t929, t930)
    var t932 Exp = exp_lam("x", t931)
    var t933 Result__Typ__string = typeof(st__141, t928, t932)
    show_result("self_apply", t933)
    reset_type_variables(st__141)
    var t934 *_goml_vec_EnvEntry = env_empty()
    var t935 Exp = exp_var("x")
    var t936 Exp = exp_var("x")
    var t937 Exp = exp_let("x", t935, t936)
    var t938 Result__Typ__string = typeof(st__141, t934, t937)
    show_result("unbound_var", t938)
    reset_type_variables(st__141)
    var t939 *_goml_vec_EnvEntry = env_empty()
    var t940 Exp = exp_var("y")
    var t941 Exp = exp_var("y")
    var t942 Exp = exp_var("z")
    var t943 Exp = exp_app(t941, t942)
    var t944 Exp = exp_lam("z", t943)
    var t945 Exp = exp_app(t940, t944)
    var t946 Exp = exp_lam("y", t945)
    var t947 Result__Typ__string = typeof(st__141, t939, t946)
    show_result("max_heiber", t947)
    reset_type_variables(st__141)
    var t948 *_goml_vec_EnvEntry = env_empty()
    var t949 Exp = exp_var("k")
    var t950 Exp = exp_var("k")
    var t951 Exp = exp_var("x")
    var t952 Exp = exp_app(t950, t951)
    var t953 Exp = exp_var("y")
    var t954 Exp = exp_app(t952, t953)
    var t955 Exp = exp_app(t949, t954)
    var t956 Exp = exp_var("k")
    var t957 Exp = exp_var("y")
    var t958 Exp = exp_app(t956, t957)
    var t959 Exp = exp_var("x")
    var t960 Exp = exp_app(t958, t959)
    var t961 Exp = exp_app(t955, t960)
    var t962 Exp = exp_lam("k", t961)
    var t963 Exp = exp_lam("y", t962)
    var t964 Exp = exp_lam("x", t963)
    var t965 Result__Typ__string = typeof(st__141, t948, t964)
    show_result("kirang", t965)
    reset_type_variables(st__141)
    var t966 *_goml_vec_EnvEntry = env_empty()
    var t967 Exp = exp_var("id")
    var t968 Exp = exp_var("id")
    var t969 Exp = exp_app(t967, t968)
    var t970 Exp = exp_let("id", id__142, t969)
    var t971 Result__Typ__string = typeof(st__141, t966, t970)
    show_result("let_id_idid", t971)
    reset_type_variables(st__141)
    var t972 *_goml_vec_EnvEntry = env_empty()
    var t973 Exp = exp_var("x")
    var t974 Exp = exp_app(t973, id__142)
    var t975 Exp = exp_var("z")
    var t976 Exp = exp_let("z", t974, t975)
    var t977 Exp = exp_var("y")
    var t978 Exp = exp_let("y", t976, t977)
    var t979 Exp = exp_let("x", c1__143, t978)
    var t980 Result__Typ__string = typeof(st__141, t972, t979)
    show_result("nested_lets", t980)
    reset_type_variables(st__141)
    var t981 *_goml_vec_EnvEntry = env_empty()
    var t982 Exp = exp_var("x")
    var t983 Exp = exp_var("y")
    var t984 Exp = exp_app(t982, t983)
    var t985 Exp = exp_var("y")
    var t986 Exp = exp_var("x")
    var t987 Exp = exp_app(t985, t986)
    var t988 Exp = exp_lam("x", t987)
    var t989 Exp = exp_let("x", t984, t988)
    var t990 Exp = exp_lam("y", t989)
    var t991 Exp = exp_lam("x", t990)
    var t992 Result__Typ__string = typeof(st__141, t981, t991)
    show_result("fun_x_fun_y_let_x_xy_fun_x_yx", t992)
    reset_type_variables(st__141)
    var t993 *_goml_vec_EnvEntry = env_empty()
    var t994 Exp = exp_var("x")
    var t995 Exp = exp_var("y")
    var t996 Exp = exp_let("y", t994, t995)
    var t997 Exp = exp_lam("x", t996)
    var t998 Result__Typ__string = typeof(st__141, t993, t997)
    show_result("sound_gen_1", t998)
    reset_type_variables(st__141)
    var t999 *_goml_vec_EnvEntry = env_empty()
    var t1000 Exp = exp_var("x")
    var t1001 Exp = exp_lam("z", t1000)
    var t1002 Exp = exp_var("y")
    var t1003 Exp = exp_let("y", t1001, t1002)
    var t1004 Exp = exp_lam("x", t1003)
    var t1005 Result__Typ__string = typeof(st__141, t999, t1004)
    show_result("sound_gen_2", t1005)
    reset_type_variables(st__141)
    var t1006 *_goml_vec_EnvEntry = env_empty()
    var t1007 Exp = exp_var("x")
    var t1008 Exp = exp_var("z")
    var t1009 Exp = exp_app(t1007, t1008)
    var t1010 Exp = exp_lam("z", t1009)
    var t1011 Exp = exp_var("y")
    var t1012 Exp = exp_let("y", t1010, t1011)
    var t1013 Exp = exp_lam("x", t1012)
    var t1014 Result__Typ__string = typeof(st__141, t1006, t1013)
    show_result("sound_gen_3", t1014)
    reset_type_variables(st__141)
    var t1015 *_goml_vec_EnvEntry = env_empty()
    var t1016 Exp = exp_var("x")
    var t1017 Exp = exp_var("y")
    var t1018 Exp = exp_app(t1016, t1017)
    var t1019 Exp = exp_var("x")
    var t1020 Exp = exp_var("y")
    var t1021 Exp = exp_app(t1019, t1020)
    var t1022 Exp = exp_let("x", t1018, t1021)
    var t1023 Exp = exp_lam("y", t1022)
    var t1024 Exp = exp_lam("x", t1023)
    var t1025 Result__Typ__string = typeof(st__141, t1015, t1024)
    show_result("double_apply", t1025)
    reset_type_variables(st__141)
    var t1026 *_goml_vec_EnvEntry = env_empty()
    var t1027 Exp = exp_var("x")
    var t1028 Exp = exp_var("y")
    var t1029 Exp = exp_var("y")
    var t1030 Exp
    var inline1416 Exp = App{
        _0: t1028,
        _1: t1029,
    }
    t1030 = inline1416
    var t1031 Exp
    var inline1413 string = "y"
    var inline1414 Exp = Let{
        _0: inline1413,
        _1: t1027,
        _2: t1030,
    }
    t1031 = inline1414
    var t1032 Exp
    var inline1410 string = "x"
    var inline1411 Exp = Lam{
        _0: inline1410,
        _1: t1031,
    }
    t1032 = inline1411
    var t1033 Result__Typ__string = typeof(st__141, t1026, t1032)
    show_result("sound_gen_occurs", t1033)
    var inline1407 *ref_int32_x = st__141.gensym_counter
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(inline1407, 0)
    var t1034 *_goml_vec_EnvEntry
    var inline1405 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__EnvEntry()
    t1034 = inline1405
    var t1035 Exp
    var inline1402 string = "x"
    var inline1403 Exp = Var{
        _0: inline1402,
    }
    t1035 = inline1403
    var t1036 Exp
    var inline1400 Exp = App{
        _0: t1035,
        _1: id__142,
    }
    t1036 = inline1400
    var t1037 Exp
    var inline1397 string = "z"
    var inline1398 Exp = Var{
        _0: inline1397,
    }
    t1037 = inline1398
    var t1038 Exp
    var inline1394 string = "z"
    var inline1395 Exp = Let{
        _0: inline1394,
        _1: t1036,
        _2: t1037,
    }
    t1038 = inline1395
    var t1039 Exp
    var inline1391 string = "y"
    var inline1392 Exp = Var{
        _0: inline1391,
    }
    t1039 = inline1392
    var t1040 Exp
    var inline1388 string = "y"
    var inline1389 Exp = Let{
        _0: inline1388,
        _1: t1038,
        _2: t1039,
    }
    t1040 = inline1389
    var t1041 Exp
    var inline1385 string = "x"
    var inline1386 Exp = Lam{
        _0: inline1385,
        _1: t1040,
    }
    t1041 = inline1386
    var t1042 Result__Typ__string = typeof(st__141, t1034, t1041)
    var inline1372 string = "fun_x_let_y_let_z_x_id_z_y"
    switch t1042._tag {
    case 0:
        var inline1373 Typ = t1042._v0_0
        var inline1375 string = inline1372 + ": "
        var inline1376 string = typ_to_string(inline1373)
        var inline1377 string = inline1375 + inline1376
        println__T_string(inline1377)
    case 1:
        var inline1379 string = t1042._v1_0
        var inline1381 string = inline1372 + ": "
        var inline1382 string = inline1381 + inline1379
        println__T_string(inline1382)
    default:
        panic("non-exhaustive match")
    }
    var inline1368 string = ""
    var inline1369 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline1368)
    _goml_runtime_core_string_println(inline1369)
    var inline1364 string = "All Done"
    var inline1365 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline1364)
    _goml_runtime_core_string_println(inline1365)
    var inline1360 string = ""
    var inline1361 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline1360)
    _goml_runtime_core_string_println(inline1361)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__433 *ref_int32_x, value__434 int32) struct{} {
    ref_set__Ref_5int32(self__433, value__434)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__432 *ref_int32_x) int32 {
    var t1050 int32 = ref_get__Ref_5int32(self__432)
    return t1050
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Tv(value__431 Tv) *ref_Tv_x {
    var t1059 *ref_Tv_x = ref__Ref_2Tv(value__431)
    return t1059
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__EnvEntry() *_goml_vec_EnvEntry {
    var t1065 *_goml_vec_EnvEntry = vec_new__Vec_8EnvEntry()
    return t1065
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SubstEntry(self__260 *_goml_vec_SubstEntry, elem__261 SubstEntry) *_goml_vec_SubstEntry {
    var t1100 int
    var inline1431 int = vec_len__Vec_10SubstEntry(self__260)
    t1100 = inline1431
    var t1101 int = t1100 + 1
    var result__262 *_goml_vec_SubstEntry
    var inline1429 *_goml_vec_SubstEntry = vec_with_capacity__Vec_10SubstEntry(t1101)
    result__262 = inline1429
    var index__263 int = 0
    Loop_loop1103:
    for {
        var t1104 int
        var inline1425 int = vec_len__Vec_10SubstEntry(self__260)
        t1104 = inline1425
        var t1105 bool = index__263 < t1104
        if t1105 {
            var t1106 SubstEntry = vec_get__Vec_10SubstEntry(self__260, index__263)
            vec_push__Vec_10SubstEntry(result__262, t1106)
            var compound_old190 int = index__263
            var compound_value191 int = 1
            var t1107 int = compound_old190 + compound_value191
            index__263 = t1107
            continue
        } else {
            break Loop_loop1103
        }
    }
    vec_push__Vec_10SubstEntry(result__262, elem__261)
    return result__262
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SubstEntry() *_goml_vec_SubstEntry {
    var t1111 *_goml_vec_SubstEntry = vec_new__Vec_10SubstEntry()
    return t1111
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__EnvEntry(self__260 *_goml_vec_EnvEntry, elem__261 EnvEntry) *_goml_vec_EnvEntry {
    var t1114 int
    var inline1441 int = vec_len__Vec_8EnvEntry(self__260)
    t1114 = inline1441
    var t1115 int = t1114 + 1
    var result__262 *_goml_vec_EnvEntry
    var inline1439 *_goml_vec_EnvEntry = vec_with_capacity__Vec_8EnvEntry(t1115)
    result__262 = inline1439
    var index__263 int = 0
    Loop_loop1117:
    for {
        var t1118 int
        var inline1435 int = vec_len__Vec_8EnvEntry(self__260)
        t1118 = inline1435
        var t1119 bool = index__263 < t1118
        if t1119 {
            var t1120 EnvEntry = vec_get__Vec_8EnvEntry(self__260, index__263)
            vec_push__Vec_8EnvEntry(result__262, t1120)
            var compound_old190 int = index__263
            var compound_value191 int = 1
            var t1121 int = compound_old190 + compound_value191
            index__263 = t1121
            continue
        } else {
            break Loop_loop1117
        }
    }
    vec_push__Vec_8EnvEntry(result__262, elem__261)
    return result__262
}

func println__T_string(value__1 string) struct{} {
    var t1124 string
    t1124 = value__1
    _goml_runtime_core_string_println(t1124)
    return struct{}{}
}

func char_to_string(value__29 rune) string {
    var t1130 uint32 = uint32(rune(value__29))
    var t1131 bool
    var inline1444 bool = t1130 <= 1114111
    if inline1444 {
        var inline1445 bool = t1130 >= 55296
        var inline1447 bool
        if inline1445 {
            var inline1449 bool = t1130 <= 57343
            inline1447 = inline1449
        } else {
            inline1447 = false
        }
        var inline1448 bool = !inline1447
        t1131 = inline1448
    } else {
        t1131 = false
    }
    if t1131 {
        var t1132 string = _goml_runtime_core_char_to_string(value__29)
        return t1132
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
