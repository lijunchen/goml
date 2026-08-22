package main

import (
    _goml_os "os"
)

func _goml_runtime_core_string_get(s string, i int) rune {
    return rune(s[i])
}

func _goml_runtime_core_string_from_utf8(bytes *_goml_vec_uint8) Tuple2_4bool_6string {
    return Tuple2_4bool_6string{
        _0: true,
        _1: string(bytes.items),
    }
}

func _goml_runtime_core_char_to_string(x rune) string {
    return string(x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_os.Stdout.WriteString(s + "\n")
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

type _goml_vec_uint8 struct {
    items []uint8
}

func vec_with_capacity__Vec_5uint8(capacity int) *_goml_vec_uint8 {
    return &_goml_vec_uint8{
        items: make([]uint8, 0, capacity),
    }
}

func vec_push__Vec_5uint8(vec *_goml_vec_uint8, elem uint8) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_5uint8(vec *_goml_vec_uint8, index int) uint8 {
    return vec.items[index]
}

func vec_len__Vec_5uint8(vec *_goml_vec_uint8) int {
    return int(len(vec.items))
}

type _goml_vec_uint32 struct {
    items []uint32
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

type Tuple2_4bool_6string struct {
    _0 bool
    _1 string
}

type FloatNatural struct {
    words *_goml_vec_uint32
}

type ParsedFloat struct {
    valid bool
    negative bool
    special int
    numerator FloatNatural
    decimal_exponent int
    binary_exponent int
    hexadecimal bool
    significant_digits int
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
    var t994 *ref_int32_x
    var inline1606 int32 = 0
    var inline1607 *ref_int32_x = ref__Ref_5int32(inline1606)
    t994 = inline1607
    var t995 *ref_int32_x
    var inline1603 int32 = 1
    var inline1604 *ref_int32_x = ref__Ref_5int32(inline1603)
    t995 = inline1604
    var t996 CheckerState = CheckerState{
        gensym_counter: t994,
        current_level: t995,
    }
    return t996
}

func reset_type_variables(st__2 CheckerState) struct{} {
    var inline1618 *ref_int32_x = st__2.gensym_counter
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__i32(inline1618, 0)
    var inline1615 *ref_int32_x = st__2.current_level
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__i32(inline1615, 1)
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
    var t1022 *ref_int32_x = st__10.gensym_counter
    var n__11 int32
    var inline1635 int32 = ref_get__Ref_5int32(t1022)
    n__11 = inline1635
    var t1023 *ref_int32_x = st__10.gensym_counter
    var t1024 int32 = n__11 + 1
    ref_set__Ref_5int32(t1023, t1024)
    var t1027 bool = n__11 < 26
    if t1027 {
        var t1028 rune = nth_letter(n__11)
        var inline1629 string = char_to_string(t1028)
        return inline1629
    } else {
        var t1030 string
        var inline1631 string = __goml_builtin_int32_to_string(n__11)
        t1030 = inline1631
        var t1031 string = "t" + t1030
        return t1031
    }
}

func typ_is_arrow(ty__15 Typ) bool {
    switch ty__15.(type) {
    case TVar:
        var x802 *ref_Tv_x = ty__15.(TVar)._0
        var mtmp806 Tv
        var inline1652 Tv = ref_get__Ref_2Tv(x802)
        mtmp806 = inline1652
        switch mtmp806.(type) {
        case Link:
            var x809 Typ = mtmp806.(Link)._0
            var t1044 bool = typ_is_arrow(x809)
            return t1044
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
        var x810 *ref_Tv_x = ty__18.(TVar)._0
        var mtmp814 Tv
        var inline1654 Tv = ref_get__Ref_2Tv(x810)
        mtmp814 = inline1654
        switch mtmp814.(type) {
        case Unbound:
            var x815 string = mtmp814.(Unbound)._0
            var t1051 string = "'" + x815
            return t1051
        case Link:
            var x817 Typ = mtmp814.(Link)._0
            var t1052 string = typ_to_string(x817)
            return t1052
        default:
            panic("non-exhaustive match")
        }
    case QVar:
        var x811 string = ty__18.(QVar)._0
        var t1053 string = "'" + x811
        return t1053
    case TArrow:
        var x812 Typ = ty__18.(TArrow)._0
        var x813 Typ = ty__18.(TArrow)._1
        var t1058 bool = typ_is_arrow(x812)
        var jp1055 string
        if t1058 {
            var t1059 string = typ_to_string(x812)
            var t1060 string = "(" + t1059
            var t1061 string = t1060 + ")"
            jp1055 = t1061
        } else {
            var t1062 string = typ_to_string(x812)
            jp1055 = t1062
        }
        var s2__26 string = typ_to_string(x813)
        var t1056 string = jp1055 + " -> "
        var t1057 string = t1056 + s2__26
        return t1057
    default:
        panic("non-exhaustive match")
    }
}

func env_empty() *_goml_vec_EnvEntry {
    var inline1656 *_goml_vec_EnvEntry = vec_new__Vec_8EnvEntry()
    return inline1656
}

func env_lookup(env__28 *_goml_vec_EnvEntry, name__29 string) Option__Typ {
    var t1067 int
    var inline1682 int = vec_len__Vec_8EnvEntry(env__28)
    t1067 = inline1682
    var t1068 int = t1067 - 1
    var i__30 *ref_int_x
    var inline1680 *ref_int_x = ref__Ref_3int(t1068)
    i__30 = inline1680
    var found__31 *ref_Option__Typ_x
    var inline1678 *ref_Option__Typ_x = ref__Ref_11Option__Typ(Option__Typ{
        _tag: 0,
    })
    found__31 = inline1678
    var done__32 *ref_bool_x
    var inline1675 bool = false
    var inline1676 *ref_bool_x = ref__Ref_4bool(inline1675)
    done__32 = inline1676
    Loop_loop1071:
    for {
        var t1084 bool
        var inline1671 bool = ref_get__Ref_4bool(done__32)
        t1084 = inline1671
        var t1085 bool = !t1084
        var jp1073 bool
        if t1085 {
            var t1086 int
            var inline1658 int = ref_get__Ref_3int(i__30)
            t1086 = inline1658
            var t1087 bool = t1086 >= 0
            jp1073 = t1087
        } else {
            jp1073 = false
        }
        if jp1073 {
            var t1074 int
            var inline1669 int = ref_get__Ref_3int(i__30)
            t1074 = inline1669
            var entry__33 EnvEntry = vec_get__Vec_8EnvEntry(env__28, t1074)
            var t1076 string = entry__33.name
            var t1077 bool = t1076 == name__29
            if t1077 {
                var t1078 Typ = entry__33.ty
                var t1079 Option__Typ = Option__Typ{
                    _tag: 1,
                    _v1_0: t1078,
                }
                ref_set__Ref_11Option__Typ(found__31, t1079)
                var inline1660 bool = true
                ref_set__Ref_4bool(done__32, inline1660)
                continue
            } else {
                var t1081 int
                var inline1667 int = ref_get__Ref_3int(i__30)
                t1081 = inline1667
                var t1082 int = t1081 - 1
                ref_set__Ref_3int(i__30, t1082)
                continue
            }
        } else {
            break Loop_loop1071
        }
    }
    var inline1673 Option__Typ = ref_get__Ref_11Option__Typ(found__31)
    return inline1673
}

func subst_lookup(subst__34 *_goml_vec_SubstEntry, name__35 string) Option__Typ {
    var t1090 int
    var inline1708 int = vec_len__Vec_10SubstEntry(subst__34)
    t1090 = inline1708
    var t1091 int = t1090 - 1
    var i__36 *ref_int_x
    var inline1706 *ref_int_x = ref__Ref_3int(t1091)
    i__36 = inline1706
    var found__37 *ref_Option__Typ_x
    var inline1704 *ref_Option__Typ_x = ref__Ref_11Option__Typ(Option__Typ{
        _tag: 0,
    })
    found__37 = inline1704
    var done__38 *ref_bool_x
    var inline1701 bool = false
    var inline1702 *ref_bool_x = ref__Ref_4bool(inline1701)
    done__38 = inline1702
    Loop_loop1094:
    for {
        var t1107 bool
        var inline1697 bool = ref_get__Ref_4bool(done__38)
        t1107 = inline1697
        var t1108 bool = !t1107
        var jp1096 bool
        if t1108 {
            var t1109 int
            var inline1684 int = ref_get__Ref_3int(i__36)
            t1109 = inline1684
            var t1110 bool = t1109 >= 0
            jp1096 = t1110
        } else {
            jp1096 = false
        }
        if jp1096 {
            var t1097 int
            var inline1695 int = ref_get__Ref_3int(i__36)
            t1097 = inline1695
            var entry__39 SubstEntry = vec_get__Vec_10SubstEntry(subst__34, t1097)
            var t1099 string = entry__39.name
            var t1100 bool = t1099 == name__35
            if t1100 {
                var t1101 Typ = entry__39.ty
                var t1102 Option__Typ = Option__Typ{
                    _tag: 1,
                    _v1_0: t1101,
                }
                ref_set__Ref_11Option__Typ(found__37, t1102)
                var inline1686 bool = true
                ref_set__Ref_4bool(done__38, inline1686)
                continue
            } else {
                var t1104 int
                var inline1693 int = ref_get__Ref_3int(i__36)
                t1104 = inline1693
                var t1105 int = t1104 - 1
                ref_set__Ref_3int(i__36, t1105)
                continue
            }
        } else {
            break Loop_loop1094
        }
    }
    var inline1699 Option__Typ = ref_get__Ref_11Option__Typ(found__37)
    return inline1699
}

func occurs(st__40 CheckerState, tvr__41 *ref_Tv_x, ty__42 Typ) Result__unit__string {
    switch ty__42.(type) {
    case TVar:
        var x822 *ref_Tv_x = ty__42.(TVar)._0
        var t1117 bool = ptr_eq__Ref_2Tv(tvr__41, x822)
        if t1117 {
            var t1118 Result__unit__string = Result__unit__string{
                _tag: 1,
                _v1_0: "occurs check",
            }
            return t1118
        } else {
            var mtmp826 Tv
            var inline1716 Tv = ref_get__Ref_2Tv(x822)
            mtmp826 = inline1716
            switch mtmp826.(type) {
            case Unbound:
                var x827 string = mtmp826.(Unbound)._0
                var x828 int32 = mtmp826.(Unbound)._1
                var mtmp830 Tv
                var inline1714 Tv = ref_get__Ref_2Tv(tvr__41)
                mtmp830 = inline1714
                var jp1122 int32
                switch mtmp830.(type) {
                case Unbound:
                    var x832 int32 = mtmp830.(Unbound)._1
                    var inline1710 bool = x832 < x828
                    if inline1710 {
                        jp1122 = x832
                    } else {
                        jp1122 = x828
                    }
                default:
                    jp1122 = x828
                }
                var t1123 Tv = Unbound{
                    _0: x827,
                    _1: jp1122,
                }
                ref_set__Ref_2Tv(x822, t1123)
                var t1124 Result__unit__string = Result__unit__string{
                    _tag: 0,
                    _v0_0: struct{}{},
                }
                return t1124
            case Link:
                var x829 Typ = mtmp826.(Link)._0
                var t1126 Result__unit__string = occurs(st__40, tvr__41, x829)
                return t1126
            default:
                panic("non-exhaustive match")
            }
        }
    case TArrow:
        var x824 Typ = ty__42.(TArrow)._0
        var x825 Typ = ty__42.(TArrow)._1
        var mtmp835 Result__unit__string = occurs(st__40, tvr__41, x824)
        switch mtmp835._tag {
        case 0:
            var t1129 Result__unit__string = occurs(st__40, tvr__41, x825)
            return t1129
        case 1:
            var x837 string = mtmp835._v1_0
            var t1130 Result__unit__string = Result__unit__string{
                _tag: 1,
                _v1_0: x837,
            }
            return t1130
        default:
            panic("non-exhaustive match")
        }
    default:
        var t1131 Result__unit__string = Result__unit__string{
            _tag: 0,
            _v0_0: struct{}{},
        }
        return t1131
    }
}

func unify(st__52 CheckerState, t1__53 Typ, t2__54 Typ) Result__unit__string {
    switch t2__54.(type) {
    case TVar:
        var x841 *ref_Tv_x = t2__54.(TVar)._0
        switch t1__53.(type) {
        case TVar:
            var x845 *ref_Tv_x = t1__53.(TVar)._0
            var t1140 bool = ptr_eq__Ref_2Tv(x845, x841)
            if t1140 {
                var t1141 Result__unit__string = Result__unit__string{
                    _tag: 0,
                    _v0_0: struct{}{},
                }
                return t1141
            } else {
                var mtmp849 Tv
                var inline1722 Tv = ref_get__Ref_2Tv(x845)
                mtmp849 = inline1722
                switch mtmp849.(type) {
                case Unbound:
                    var mtmp853 Tv
                    var inline1720 Tv = ref_get__Ref_2Tv(x841)
                    mtmp853 = inline1720
                    switch mtmp853.(type) {
                    case Unbound:
                        var t1146 Typ = TVar{
                            _0: x841,
                        }
                        var mtmp857 Result__unit__string = occurs(st__52, x845, t1146)
                        switch mtmp857._tag {
                        case 0:
                            var t1149 Typ = TVar{
                                _0: x841,
                            }
                            var t1150 Tv = Link{
                                _0: t1149,
                            }
                            ref_set__Ref_2Tv(x845, t1150)
                            var t1151 Result__unit__string = Result__unit__string{
                                _tag: 0,
                                _v0_0: struct{}{},
                            }
                            return t1151
                        case 1:
                            var x859 string = mtmp857._v1_0
                            var t1152 Result__unit__string = Result__unit__string{
                                _tag: 1,
                                _v1_0: x859,
                            }
                            return t1152
                        default:
                            panic("non-exhaustive match")
                        }
                    case Link:
                        var x856 Typ = mtmp853.(Link)._0
                        var t1153 Typ = TVar{
                            _0: x845,
                        }
                        var t1154 Result__unit__string = unify(st__52, t1153, x856)
                        return t1154
                    default:
                        panic("non-exhaustive match")
                    }
                case Link:
                    var x852 Typ = mtmp849.(Link)._0
                    var t1155 Typ = TVar{
                        _0: x841,
                    }
                    var t1156 Result__unit__string = unify(st__52, x852, t1155)
                    return t1156
                default:
                    panic("non-exhaustive match")
                }
            }
        default:
            var mtmp861 Tv
            var inline1726 Tv = ref_get__Ref_2Tv(x841)
            mtmp861 = inline1726
            switch mtmp861.(type) {
            case Unbound:
                var mtmp865 Result__unit__string = occurs(st__52, x841, t1__53)
                switch mtmp865._tag {
                case 0:
                    var t1161 Tv = Link{
                        _0: t1__53,
                    }
                    ref_set__Ref_2Tv(x841, t1161)
                    var t1162 Result__unit__string = Result__unit__string{
                        _tag: 0,
                        _v0_0: struct{}{},
                    }
                    return t1162
                case 1:
                    var x867 string = mtmp865._v1_0
                    var t1163 Result__unit__string = Result__unit__string{
                        _tag: 1,
                        _v1_0: x867,
                    }
                    return t1163
                default:
                    panic("non-exhaustive match")
                }
            case Link:
                var x864 Typ = mtmp861.(Link)._0
                var t1164 Result__unit__string = unify(st__52, t1__53, x864)
                return t1164
            default:
                panic("non-exhaustive match")
            }
        }
    case TArrow:
        var x843 Typ = t2__54.(TArrow)._0
        var x844 Typ = t2__54.(TArrow)._1
        switch t1__53.(type) {
        case TVar:
            var x869 *ref_Tv_x = t1__53.(TVar)._0
            var mtmp873 Tv
            var inline1730 Tv = ref_get__Ref_2Tv(x869)
            mtmp873 = inline1730
            switch mtmp873.(type) {
            case Unbound:
                var mtmp877 Result__unit__string = occurs(st__52, x869, t2__54)
                switch mtmp877._tag {
                case 0:
                    var t1171 Tv = Link{
                        _0: t2__54,
                    }
                    ref_set__Ref_2Tv(x869, t1171)
                    var t1172 Result__unit__string = Result__unit__string{
                        _tag: 0,
                        _v0_0: struct{}{},
                    }
                    return t1172
                case 1:
                    var x879 string = mtmp877._v1_0
                    var t1173 Result__unit__string = Result__unit__string{
                        _tag: 1,
                        _v1_0: x879,
                    }
                    return t1173
                default:
                    panic("non-exhaustive match")
                }
            case Link:
                var x876 Typ = mtmp873.(Link)._0
                var t1174 Result__unit__string = unify(st__52, x876, t2__54)
                return t1174
            default:
                panic("non-exhaustive match")
            }
        case TArrow:
            var x871 Typ = t1__53.(TArrow)._0
            var x872 Typ = t1__53.(TArrow)._1
            var mtmp881 Result__unit__string = unify(st__52, x871, x843)
            switch mtmp881._tag {
            case 0:
                var t1177 Result__unit__string = unify(st__52, x872, x844)
                return t1177
            case 1:
                var x883 string = mtmp881._v1_0
                var t1178 Result__unit__string = Result__unit__string{
                    _tag: 1,
                    _v1_0: x883,
                }
                return t1178
            default:
                panic("non-exhaustive match")
            }
        default:
            var t1179 Result__unit__string = Result__unit__string{
                _tag: 1,
                _v1_0: "unify error",
            }
            return t1179
        }
    default:
        switch t1__53.(type) {
        case TVar:
            var x884 *ref_Tv_x = t1__53.(TVar)._0
            var mtmp888 Tv
            var inline1734 Tv = ref_get__Ref_2Tv(x884)
            mtmp888 = inline1734
            switch mtmp888.(type) {
            case Unbound:
                var mtmp892 Result__unit__string = occurs(st__52, x884, t2__54)
                switch mtmp892._tag {
                case 0:
                    var t1186 Tv = Link{
                        _0: t2__54,
                    }
                    ref_set__Ref_2Tv(x884, t1186)
                    var t1187 Result__unit__string = Result__unit__string{
                        _tag: 0,
                        _v0_0: struct{}{},
                    }
                    return t1187
                case 1:
                    var x894 string = mtmp892._v1_0
                    var t1188 Result__unit__string = Result__unit__string{
                        _tag: 1,
                        _v1_0: x894,
                    }
                    return t1188
                default:
                    panic("non-exhaustive match")
                }
            case Link:
                var x891 Typ = mtmp888.(Link)._0
                var t1189 Result__unit__string = unify(st__52, x891, t2__54)
                return t1189
            default:
                panic("non-exhaustive match")
            }
        default:
            var t1190 Result__unit__string = Result__unit__string{
                _tag: 1,
                _v1_0: "unify error",
            }
            return t1190
        }
    }
}

func gen(st__73 CheckerState, ty__74 Typ) Typ {
    switch ty__74.(type) {
    case TVar:
        var x896 *ref_Tv_x = ty__74.(TVar)._0
        var mtmp900 Tv
        var inline1738 Tv = ref_get__Ref_2Tv(x896)
        mtmp900 = inline1738
        switch mtmp900.(type) {
        case Unbound:
            var x901 string = mtmp900.(Unbound)._0
            var x902 int32 = mtmp900.(Unbound)._1
            var t1197 *ref_int32_x = st__73.current_level
            var cur__78 int32
            var inline1736 int32 = ref_get__Ref_5int32(t1197)
            cur__78 = inline1736
            var t1200 bool = x902 > cur__78
            if t1200 {
                var t1201 Typ = QVar{
                    _0: x901,
                }
                return t1201
            } else {
                var t1202 Typ = TVar{
                    _0: x896,
                }
                return t1202
            }
        case Link:
            var x903 Typ = mtmp900.(Link)._0
            var t1203 Typ = gen(st__73, x903)
            return t1203
        default:
            panic("non-exhaustive match")
        }
    case TArrow:
        var x898 Typ = ty__74.(TArrow)._0
        var x899 Typ = ty__74.(TArrow)._1
        var t1204 Typ = gen(st__73, x898)
        var t1205 Typ = gen(st__73, x899)
        var t1206 Typ = TArrow{
            _0: t1204,
            _1: t1205,
        }
        return t1206
    default:
        return ty__74
    }
}

func inst_loop(st__83 CheckerState, subst__84 *_goml_vec_SubstEntry, ty__85 Typ) Tuple2_3Typ_16Vec_10SubstEntry {
    switch ty__85.(type) {
    case TVar:
        var x904 *ref_Tv_x = ty__85.(TVar)._0
        var mtmp908 Tv
        var inline1740 Tv = ref_get__Ref_2Tv(x904)
        mtmp908 = inline1740
        switch mtmp908.(type) {
        case Link:
            var x911 Typ = mtmp908.(Link)._0
            var t1213 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, subst__84, x911)
            return t1213
        default:
            var t1214 Typ = TVar{
                _0: x904,
            }
            var t1215 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: t1214,
                _1: subst__84,
            }
            return t1215
        }
    case QVar:
        var x905 string = ty__85.(QVar)._0
        var mtmp912 Option__Typ = subst_lookup(subst__84, x905)
        switch mtmp912._tag {
        case 0:
            var tv__88 Typ
            var inline1742 string = gensym(st__83)
            var inline1743 *ref_int32_x = st__83.current_level
            var inline1744 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__i32(inline1743)
            var inline1745 Tv = Unbound{
                _0: inline1742,
                _1: inline1744,
            }
            var inline1746 *ref_Tv_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Tv(inline1745)
            var inline1747 Typ = TVar{
                _0: inline1746,
            }
            tv__88 = inline1747
            var t1218 SubstEntry = SubstEntry{
                name: x905,
                ty: tv__88,
            }
            var new_subst__89 *_goml_vec_SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SubstEntry(subst__84, t1218)
            var t1219 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: tv__88,
                _1: new_subst__89,
            }
            return t1219
        case 1:
            var x913 Typ = mtmp912._v1_0
            var t1220 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
                _0: x913,
                _1: subst__84,
            }
            return t1220
        default:
            panic("non-exhaustive match")
        }
    case TArrow:
        var x906 Typ = ty__85.(TArrow)._0
        var x907 Typ = ty__85.(TArrow)._1
        var mtmp914 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, subst__84, x906)
        var x915 Typ = mtmp914._0
        var x916 *_goml_vec_SubstEntry = mtmp914._1
        var mtmp917 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__83, x916, x907)
        var x918 Typ = mtmp917._0
        var x919 *_goml_vec_SubstEntry = mtmp917._1
        var t1221 Typ = TArrow{
            _0: x915,
            _1: x918,
        }
        var t1222 Tuple2_3Typ_16Vec_10SubstEntry = Tuple2_3Typ_16Vec_10SubstEntry{
            _0: t1221,
            _1: x919,
        }
        return t1222
    default:
        panic("non-exhaustive match")
    }
}

func typeof(st__102 CheckerState, env__103 *_goml_vec_EnvEntry, e__104 Exp) Result__Typ__string {
    switch e__104.(type) {
    case Var:
        var x923 string = e__104.(Var)._0
        var mtmp931 Option__Typ = env_lookup(env__103, x923)
        switch mtmp931._tag {
        case 0:
            var t1231 Result__Typ__string = Result__Typ__string{
                _tag: 1,
                _v1_0: "unbound var",
            }
            return t1231
        case 1:
            var x932 Typ = mtmp931._v1_0
            var t1232 Typ
            var inline1751 *_goml_vec_SubstEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SubstEntry()
            var inline1752 Tuple2_3Typ_16Vec_10SubstEntry = inst_loop(st__102, inline1751, x932)
            var inline1753 Typ = inline1752._0
            t1232 = inline1753
            var t1233 Result__Typ__string = Result__Typ__string{
                _tag: 0,
                _v0_0: t1232,
            }
            return t1233
        default:
            panic("non-exhaustive match")
        }
    case App:
        var x924 Exp = e__104.(App)._0
        var x925 Exp = e__104.(App)._1
        var mtmp933 Result__Typ__string = typeof(st__102, env__103, x924)
        switch mtmp933._tag {
        case 0:
            var x934 Typ = mtmp933._v0_0
            var mtmp936 Result__Typ__string = typeof(st__102, env__103, x925)
            switch mtmp936._tag {
            case 0:
                var x937 Typ = mtmp936._v0_0
                var ty_res__119 Typ
                var inline1756 string = gensym(st__102)
                var inline1757 *ref_int32_x = st__102.current_level
                var inline1758 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__i32(inline1757)
                var inline1759 Tv = Unbound{
                    _0: inline1756,
                    _1: inline1758,
                }
                var inline1760 *ref_Tv_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Tv(inline1759)
                var inline1761 Typ = TVar{
                    _0: inline1760,
                }
                ty_res__119 = inline1761
                var arrow__120 Typ = TArrow{
                    _0: x937,
                    _1: ty_res__119,
                }
                var mtmp939 Result__unit__string = unify(st__102, x934, arrow__120)
                switch mtmp939._tag {
                case 0:
                    var t1240 Result__Typ__string = Result__Typ__string{
                        _tag: 0,
                        _v0_0: ty_res__119,
                    }
                    return t1240
                case 1:
                    var x941 string = mtmp939._v1_0
                    var t1241 Result__Typ__string = Result__Typ__string{
                        _tag: 1,
                        _v1_0: x941,
                    }
                    return t1241
                default:
                    panic("non-exhaustive match")
                }
            case 1:
                var x938 string = mtmp936._v1_0
                var t1242 Result__Typ__string = Result__Typ__string{
                    _tag: 1,
                    _v1_0: x938,
                }
                return t1242
            default:
                panic("non-exhaustive match")
            }
        case 1:
            var x935 string = mtmp933._v1_0
            var t1243 Result__Typ__string = Result__Typ__string{
                _tag: 1,
                _v1_0: x935,
            }
            return t1243
        default:
            panic("non-exhaustive match")
        }
    case Lam:
        var x926 string = e__104.(Lam)._0
        var x927 Exp = e__104.(Lam)._1
        var ty_x__109 Typ
        var inline1763 string = gensym(st__102)
        var inline1764 *ref_int32_x = st__102.current_level
        var inline1765 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__i32(inline1764)
        var inline1766 Tv = Unbound{
            _0: inline1763,
            _1: inline1765,
        }
        var inline1767 *ref_Tv_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Tv(inline1766)
        var inline1768 Typ = TVar{
            _0: inline1767,
        }
        ty_x__109 = inline1768
        var t1244 EnvEntry = EnvEntry{
            name: x926,
            ty: ty_x__109,
        }
        var env2__110 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__EnvEntry(env__103, t1244)
        var mtmp942 Result__Typ__string = typeof(st__102, env2__110, x927)
        switch mtmp942._tag {
        case 0:
            var x943 Typ = mtmp942._v0_0
            var t1247 Typ = TArrow{
                _0: ty_x__109,
                _1: x943,
            }
            var t1248 Result__Typ__string = Result__Typ__string{
                _tag: 0,
                _v0_0: t1247,
            }
            return t1248
        case 1:
            var x944 string = mtmp942._v1_0
            var t1249 Result__Typ__string = Result__Typ__string{
                _tag: 1,
                _v1_0: x944,
            }
            return t1249
        default:
            panic("non-exhaustive match")
        }
    case Let:
        var x928 string = e__104.(Let)._0
        var x929 Exp = e__104.(Let)._1
        var x930 Exp = e__104.(Let)._2
        var inline1776 *ref_int32_x = st__102.current_level
        var inline1777 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__i32(inline1776)
        var inline1778 *ref_int32_x = st__102.current_level
        var inline1779 int32 = inline1777 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__i32(inline1778, inline1779)
        var ty_e__125 Result__Typ__string = typeof(st__102, env__103, x929)
        var inline1770 *ref_int32_x = st__102.current_level
        var inline1771 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__i32(inline1770)
        var inline1772 *ref_int32_x = st__102.current_level
        var inline1773 int32 = inline1771 - 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__i32(inline1772, inline1773)
        switch ty_e__125._tag {
        case 0:
            var x947 Typ = ty_e__125._v0_0
            var t1252 Typ = gen(st__102, x947)
            var t1253 EnvEntry = EnvEntry{
                name: x928,
                ty: t1252,
            }
            var env2__128 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__EnvEntry(env__103, t1253)
            var t1254 Result__Typ__string = typeof(st__102, env2__128, x930)
            return t1254
        case 1:
            var x948 string = ty_e__125._v1_0
            var t1255 Result__Typ__string = Result__Typ__string{
                _tag: 1,
                _v1_0: x948,
            }
            return t1255
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func exp_var(name__129 string) Exp {
    var t1258 Exp = Var{
        _0: name__129,
    }
    return t1258
}

func exp_lam(name__130 string, body__131 Exp) Exp {
    var t1261 Exp = Lam{
        _0: name__130,
        _1: body__131,
    }
    return t1261
}

func exp_app(a__132 Exp, b__133 Exp) Exp {
    var t1264 Exp = App{
        _0: a__132,
        _1: b__133,
    }
    return t1264
}

func exp_let(name__134 string, a__135 Exp, b__136 Exp) Exp {
    var t1267 Exp = Let{
        _0: name__134,
        _1: a__135,
        _2: b__136,
    }
    return t1267
}

func show_result(label__137 string, res__138 Result__Typ__string) struct{} {
    switch res__138._tag {
    case 0:
        var x949 Typ = res__138._v0_0
        var t1270 string = label__137 + ": "
        var t1271 string = typ_to_string(x949)
        var t1272 string = t1270 + t1271
        var inline1782 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1272)
        _goml_runtime_core_string_println(inline1782)
        return struct{}{}
    case 1:
        var x950 string = res__138._v1_0
        var t1274 string = label__137 + ": "
        var t1275 string = t1274 + x950
        var inline1785 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1275)
        _goml_runtime_core_string_println(inline1785)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var st__141 CheckerState = state_new()
    var t1278 Exp = exp_var("x")
    var id__142 Exp = exp_lam("x", t1278)
    var t1279 Exp = exp_var("x")
    var t1280 Exp = exp_var("y")
    var t1281 Exp = exp_app(t1279, t1280)
    var t1282 Exp = exp_lam("y", t1281)
    var c1__143 Exp = exp_lam("x", t1282)
    reset_type_variables(st__141)
    var t1283 *_goml_vec_EnvEntry = env_empty()
    var t1284 Result__Typ__string = typeof(st__141, t1283, id__142)
    show_result("id", t1284)
    reset_type_variables(st__141)
    var t1285 *_goml_vec_EnvEntry = env_empty()
    var t1286 Result__Typ__string = typeof(st__141, t1285, c1__143)
    show_result("c1", t1286)
    reset_type_variables(st__141)
    var t1287 *_goml_vec_EnvEntry = env_empty()
    var t1288 Exp = exp_var("x")
    var t1289 Exp = exp_let("x", c1__143, t1288)
    var t1290 Result__Typ__string = typeof(st__141, t1287, t1289)
    show_result("let_x_c1_x", t1290)
    reset_type_variables(st__141)
    var t1291 *_goml_vec_EnvEntry = env_empty()
    var t1292 Exp = exp_var("z")
    var t1293 Exp = exp_lam("z", t1292)
    var t1294 Exp = exp_var("y")
    var t1295 Exp = exp_let("y", t1293, t1294)
    var t1296 Result__Typ__string = typeof(st__141, t1291, t1295)
    show_result("let_y_id_y", t1296)
    reset_type_variables(st__141)
    var t1297 *_goml_vec_EnvEntry = env_empty()
    var t1298 Exp = exp_var("z")
    var t1299 Exp = exp_lam("z", t1298)
    var t1300 Exp = exp_var("y")
    var t1301 Exp = exp_let("y", t1299, t1300)
    var t1302 Exp = exp_lam("x", t1301)
    var t1303 Result__Typ__string = typeof(st__141, t1297, t1302)
    show_result("lam_x_let_y_id_y", t1303)
    reset_type_variables(st__141)
    var t1304 *_goml_vec_EnvEntry = env_empty()
    var t1305 Exp = exp_var("z")
    var t1306 Exp = exp_lam("z", t1305)
    var t1307 Exp = exp_var("y")
    var t1308 Exp = exp_var("x")
    var t1309 Exp = exp_app(t1307, t1308)
    var t1310 Exp = exp_let("y", t1306, t1309)
    var t1311 Exp = exp_lam("x", t1310)
    var t1312 Result__Typ__string = typeof(st__141, t1304, t1311)
    show_result("lam_x_let_y_id_yx", t1312)
    reset_type_variables(st__141)
    var t1313 *_goml_vec_EnvEntry = env_empty()
    var t1314 Exp = exp_var("x")
    var t1315 Exp = exp_var("x")
    var t1316 Exp = exp_app(t1314, t1315)
    var t1317 Exp = exp_lam("x", t1316)
    var t1318 Result__Typ__string = typeof(st__141, t1313, t1317)
    show_result("self_apply", t1318)
    reset_type_variables(st__141)
    var t1319 *_goml_vec_EnvEntry = env_empty()
    var t1320 Exp = exp_var("x")
    var t1321 Exp = exp_var("x")
    var t1322 Exp = exp_let("x", t1320, t1321)
    var t1323 Result__Typ__string = typeof(st__141, t1319, t1322)
    show_result("unbound_var", t1323)
    reset_type_variables(st__141)
    var t1324 *_goml_vec_EnvEntry = env_empty()
    var t1325 Exp = exp_var("y")
    var t1326 Exp = exp_var("y")
    var t1327 Exp = exp_var("z")
    var t1328 Exp = exp_app(t1326, t1327)
    var t1329 Exp = exp_lam("z", t1328)
    var t1330 Exp = exp_app(t1325, t1329)
    var t1331 Exp = exp_lam("y", t1330)
    var t1332 Result__Typ__string = typeof(st__141, t1324, t1331)
    show_result("max_heiber", t1332)
    reset_type_variables(st__141)
    var t1333 *_goml_vec_EnvEntry = env_empty()
    var t1334 Exp = exp_var("k")
    var t1335 Exp = exp_var("k")
    var t1336 Exp = exp_var("x")
    var t1337 Exp = exp_app(t1335, t1336)
    var t1338 Exp = exp_var("y")
    var t1339 Exp = exp_app(t1337, t1338)
    var t1340 Exp = exp_app(t1334, t1339)
    var t1341 Exp = exp_var("k")
    var t1342 Exp = exp_var("y")
    var t1343 Exp = exp_app(t1341, t1342)
    var t1344 Exp = exp_var("x")
    var t1345 Exp = exp_app(t1343, t1344)
    var t1346 Exp = exp_app(t1340, t1345)
    var t1347 Exp = exp_lam("k", t1346)
    var t1348 Exp = exp_lam("y", t1347)
    var t1349 Exp = exp_lam("x", t1348)
    var t1350 Result__Typ__string = typeof(st__141, t1333, t1349)
    show_result("kirang", t1350)
    reset_type_variables(st__141)
    var t1351 *_goml_vec_EnvEntry = env_empty()
    var t1352 Exp = exp_var("id")
    var t1353 Exp = exp_var("id")
    var t1354 Exp = exp_app(t1352, t1353)
    var t1355 Exp = exp_let("id", id__142, t1354)
    var t1356 Result__Typ__string = typeof(st__141, t1351, t1355)
    show_result("let_id_idid", t1356)
    reset_type_variables(st__141)
    var t1357 *_goml_vec_EnvEntry = env_empty()
    var t1358 Exp = exp_var("x")
    var t1359 Exp = exp_app(t1358, id__142)
    var t1360 Exp = exp_var("z")
    var t1361 Exp = exp_let("z", t1359, t1360)
    var t1362 Exp = exp_var("y")
    var t1363 Exp = exp_let("y", t1361, t1362)
    var t1364 Exp = exp_let("x", c1__143, t1363)
    var t1365 Result__Typ__string = typeof(st__141, t1357, t1364)
    show_result("nested_lets", t1365)
    reset_type_variables(st__141)
    var t1366 *_goml_vec_EnvEntry = env_empty()
    var t1367 Exp = exp_var("x")
    var t1368 Exp = exp_var("y")
    var t1369 Exp = exp_app(t1367, t1368)
    var t1370 Exp = exp_var("y")
    var t1371 Exp = exp_var("x")
    var t1372 Exp = exp_app(t1370, t1371)
    var t1373 Exp = exp_lam("x", t1372)
    var t1374 Exp = exp_let("x", t1369, t1373)
    var t1375 Exp = exp_lam("y", t1374)
    var t1376 Exp = exp_lam("x", t1375)
    var t1377 Result__Typ__string = typeof(st__141, t1366, t1376)
    show_result("fun_x_fun_y_let_x_xy_fun_x_yx", t1377)
    reset_type_variables(st__141)
    var t1378 *_goml_vec_EnvEntry = env_empty()
    var t1379 Exp = exp_var("x")
    var t1380 Exp = exp_var("y")
    var t1381 Exp = exp_let("y", t1379, t1380)
    var t1382 Exp = exp_lam("x", t1381)
    var t1383 Result__Typ__string = typeof(st__141, t1378, t1382)
    show_result("sound_gen_1", t1383)
    reset_type_variables(st__141)
    var t1384 *_goml_vec_EnvEntry = env_empty()
    var t1385 Exp = exp_var("x")
    var t1386 Exp = exp_lam("z", t1385)
    var t1387 Exp = exp_var("y")
    var t1388 Exp = exp_let("y", t1386, t1387)
    var t1389 Exp = exp_lam("x", t1388)
    var t1390 Result__Typ__string = typeof(st__141, t1384, t1389)
    show_result("sound_gen_2", t1390)
    reset_type_variables(st__141)
    var t1391 *_goml_vec_EnvEntry = env_empty()
    var t1392 Exp = exp_var("x")
    var t1393 Exp = exp_var("z")
    var t1394 Exp = exp_app(t1392, t1393)
    var t1395 Exp = exp_lam("z", t1394)
    var t1396 Exp = exp_var("y")
    var t1397 Exp = exp_let("y", t1395, t1396)
    var t1398 Exp = exp_lam("x", t1397)
    var t1399 Result__Typ__string = typeof(st__141, t1391, t1398)
    show_result("sound_gen_3", t1399)
    reset_type_variables(st__141)
    var t1400 *_goml_vec_EnvEntry = env_empty()
    var t1401 Exp = exp_var("x")
    var t1402 Exp = exp_var("y")
    var t1403 Exp = exp_app(t1401, t1402)
    var t1404 Exp = exp_var("x")
    var t1405 Exp = exp_var("y")
    var t1406 Exp = exp_app(t1404, t1405)
    var t1407 Exp = exp_let("x", t1403, t1406)
    var t1408 Exp = exp_lam("y", t1407)
    var t1409 Exp = exp_lam("x", t1408)
    var t1410 Result__Typ__string = typeof(st__141, t1400, t1409)
    show_result("double_apply", t1410)
    reset_type_variables(st__141)
    var t1411 *_goml_vec_EnvEntry = env_empty()
    var t1412 Exp = exp_var("x")
    var t1413 Exp = exp_var("y")
    var t1414 Exp = exp_var("y")
    var t1415 Exp
    var inline1844 Exp = App{
        _0: t1413,
        _1: t1414,
    }
    t1415 = inline1844
    var t1416 Exp
    var inline1841 string = "y"
    var inline1842 Exp = Let{
        _0: inline1841,
        _1: t1412,
        _2: t1415,
    }
    t1416 = inline1842
    var t1417 Exp
    var inline1838 string = "x"
    var inline1839 Exp = Lam{
        _0: inline1838,
        _1: t1416,
    }
    t1417 = inline1839
    var t1418 Result__Typ__string = typeof(st__141, t1411, t1417)
    show_result("sound_gen_occurs", t1418)
    var inline1835 *ref_int32_x = st__141.gensym_counter
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__i32(inline1835, 0)
    var t1419 *_goml_vec_EnvEntry
    var inline1833 *_goml_vec_EnvEntry = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__EnvEntry()
    t1419 = inline1833
    var t1420 Exp
    var inline1830 string = "x"
    var inline1831 Exp = Var{
        _0: inline1830,
    }
    t1420 = inline1831
    var t1421 Exp
    var inline1828 Exp = App{
        _0: t1420,
        _1: id__142,
    }
    t1421 = inline1828
    var t1422 Exp
    var inline1825 string = "z"
    var inline1826 Exp = Var{
        _0: inline1825,
    }
    t1422 = inline1826
    var t1423 Exp
    var inline1822 string = "z"
    var inline1823 Exp = Let{
        _0: inline1822,
        _1: t1421,
        _2: t1422,
    }
    t1423 = inline1823
    var t1424 Exp
    var inline1819 string = "y"
    var inline1820 Exp = Var{
        _0: inline1819,
    }
    t1424 = inline1820
    var t1425 Exp
    var inline1816 string = "y"
    var inline1817 Exp = Let{
        _0: inline1816,
        _1: t1423,
        _2: t1424,
    }
    t1425 = inline1817
    var t1426 Exp
    var inline1813 string = "x"
    var inline1814 Exp = Lam{
        _0: inline1813,
        _1: t1425,
    }
    t1426 = inline1814
    var t1427 Result__Typ__string = typeof(st__141, t1419, t1426)
    var inline1800 string = "fun_x_let_y_let_z_x_id_z_y"
    switch t1427._tag {
    case 0:
        var inline1801 Typ = t1427._v0_0
        var inline1803 string = inline1800 + ": "
        var inline1804 string = typ_to_string(inline1801)
        var inline1805 string = inline1803 + inline1804
        println__T_string(inline1805)
    case 1:
        var inline1807 string = t1427._v1_0
        var inline1809 string = inline1800 + ": "
        var inline1810 string = inline1809 + inline1807
        println__T_string(inline1810)
    default:
        panic("non-exhaustive match")
    }
    var inline1796 string = ""
    var inline1797 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline1796)
    _goml_runtime_core_string_println(inline1797)
    var inline1792 string = "All Done"
    var inline1793 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline1792)
    _goml_runtime_core_string_println(inline1793)
    var inline1788 string = ""
    var inline1789 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline1788)
    _goml_runtime_core_string_println(inline1789)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__i32(self__686 *ref_int32_x, value__687 int32) struct{} {
    ref_set__Ref_5int32(self__686, value__687)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__i32(self__685 *ref_int32_x) int32 {
    var t1435 int32 = ref_get__Ref_5int32(self__685)
    return t1435
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Tv(value__684 Tv) *ref_Tv_x {
    var t1444 *ref_Tv_x = ref__Ref_2Tv(value__684)
    return t1444
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__EnvEntry() *_goml_vec_EnvEntry {
    var t1450 *_goml_vec_EnvEntry = vec_new__Vec_8EnvEntry()
    return t1450
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SubstEntry(self__513 *_goml_vec_SubstEntry, elem__514 SubstEntry) *_goml_vec_SubstEntry {
    var t1485 int
    var inline1862 int = vec_len__Vec_10SubstEntry(self__513)
    t1485 = inline1862
    var t1486 int = t1485 + 1
    var result__515 *_goml_vec_SubstEntry
    var inline1860 *_goml_vec_SubstEntry = vec_with_capacity__Vec_10SubstEntry(t1486)
    result__515 = inline1860
    var index__516 int = 0
    Loop_loop1488:
    for {
        var t1489 int
        var inline1856 int = vec_len__Vec_10SubstEntry(self__513)
        t1489 = inline1856
        var t1490 bool = index__516 < t1489
        if t1490 {
            var t1491 SubstEntry = vec_get__Vec_10SubstEntry(self__513, index__516)
            vec_push__Vec_10SubstEntry(result__515, t1491)
            var compound_old575 int = index__516
            var compound_value576 int = 1
            var t1492 int = compound_old575 + compound_value576
            index__516 = t1492
            continue
        } else {
            break Loop_loop1488
        }
    }
    vec_push__Vec_10SubstEntry(result__515, elem__514)
    return result__515
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SubstEntry() *_goml_vec_SubstEntry {
    var t1496 *_goml_vec_SubstEntry = vec_new__Vec_10SubstEntry()
    return t1496
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__EnvEntry(self__513 *_goml_vec_EnvEntry, elem__514 EnvEntry) *_goml_vec_EnvEntry {
    var t1499 int
    var inline1872 int = vec_len__Vec_8EnvEntry(self__513)
    t1499 = inline1872
    var t1500 int = t1499 + 1
    var result__515 *_goml_vec_EnvEntry
    var inline1870 *_goml_vec_EnvEntry = vec_with_capacity__Vec_8EnvEntry(t1500)
    result__515 = inline1870
    var index__516 int = 0
    Loop_loop1502:
    for {
        var t1503 int
        var inline1866 int = vec_len__Vec_8EnvEntry(self__513)
        t1503 = inline1866
        var t1504 bool = index__516 < t1503
        if t1504 {
            var t1505 EnvEntry = vec_get__Vec_8EnvEntry(self__513, index__516)
            vec_push__Vec_8EnvEntry(result__515, t1505)
            var compound_old575 int = index__516
            var compound_value576 int = 1
            var t1506 int = compound_old575 + compound_value576
            index__516 = t1506
            continue
        } else {
            break Loop_loop1502
        }
    }
    vec_push__Vec_8EnvEntry(result__515, elem__514)
    return result__515
}

func println__T_string(value__1 string) struct{} {
    var t1509 string
    t1509 = value__1
    _goml_runtime_core_string_println(t1509)
    return struct{}{}
}

func char_to_string(value__282 rune) string {
    var t1515 uint32 = uint32(rune(value__282))
    var t1516 bool
    var inline1875 bool = t1515 <= 1114111
    if inline1875 {
        var inline1876 bool = t1515 >= 55296
        var inline1878 bool
        if inline1876 {
            var inline1880 bool = t1515 <= 57343
            inline1878 = inline1880
        } else {
            inline1878 = false
        }
        var inline1879 bool = !inline1878
        t1516 = inline1879
    } else {
        t1516 = false
    }
    if t1516 {
        var t1517 string = _goml_runtime_core_char_to_string(value__282)
        return t1517
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func __goml_builtin_int32_to_string(value__225 int32) string {
    var t1520 int64 = int64(int32(value__225))
    var inline1882 bool = t1520 < 0
    if inline1882 {
        var inline1883 uint64 = uint64(int64(t1520))
        var inline1884 uint64 = 0 - inline1883
        var inline1885 string = decimal_string(inline1884)
        var inline1886 string = "-" + inline1885
        return inline1886
    } else {
        var inline1887 uint64 = uint64(int64(t1520))
        var inline1888 string = decimal_string(inline1887)
        return inline1888
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func decimal_string(value__208 uint64) string {
    var t1577 bool = value__208 == 0
    if t1577 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop1570:
        for {
            var t1571 bool = remaining__210 > 0
            if t1571 {
                var t1572_rhs uint64 = 10
                var t1572 uint64 = remaining__210 % t1572_rhs
                var t1573 uint8 = uint8(uint64(t1572))
                var t1574 uint8 = t1573 + 48
                vec_push__Vec_5uint8(reversed__209, t1574)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t1575 uint64 = compound_old353 / compound_value354
                remaining__210 = t1575
                continue
            } else {
                break Loop_loop1570
            }
        }
        var t1559 int
        var inline1898 int = vec_len__Vec_5uint8(reversed__209)
        t1559 = inline1898
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t1559)
        var offset__212 int = 0
        Loop_loop1561:
        for {
            var t1562 int
            var inline1896 int = vec_len__Vec_5uint8(reversed__209)
            t1562 = inline1896
            var t1563 bool = offset__212 < t1562
            if t1563 {
                var t1564 int
                var inline1894 int = vec_len__Vec_5uint8(reversed__209)
                t1564 = inline1894
                var t1565 int = t1564 - offset__212
                var t1566 int = t1565 - 1
                var t1567 uint8 = vec_get__Vec_5uint8(reversed__209, t1566)
                vec_push__Vec_5uint8(bytes__211, t1567)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t1568 int = compound_old358 + compound_value359
                offset__212 = t1568
                continue
            } else {
                break Loop_loop1561
            }
        }
        var mtmp362 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__211)
        var x364 string = mtmp362._1
        return x364
    }
}

func main() {
    main0()
}
