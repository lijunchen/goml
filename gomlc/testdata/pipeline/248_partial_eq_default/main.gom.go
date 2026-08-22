package main

import (
    _goml_os "os"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_string_from_utf8(bytes *_goml_vec_uint8) Tuple2_4bool_6string {
    return Tuple2_4bool_6string{
        _0: true,
        _1: string(bytes.items),
    }
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_os.Stdout.WriteString(s + "\n")
    return struct{}{}
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

type ref_NoDefault_x struct {
    value NoDefault
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

type Settings struct {
    enabled bool
    retries int
    label string
}

type NoDefault struct {
    value int
}

type Empty struct {}

type PairDefaults__isize struct {
    first int
    second int
    nested Option__isize
}

type Ordering int32

type State struct {
    _tag int32
    _v1_0 int
}

type Message struct {
    _tag int32
    _v0_0 string
    _v0_1 int
}

type Event struct {
    _tag int32
    _v0_0 string
    _v0_1 int
}

type Option__isize struct {
    _tag int32
    _v1_0 int
}

type Lazy__NoDefault struct {
    _tag int32
    _v1_0 NoDefault
}

type Selected__isize struct {
    _tag int32
    _v0_0 int
    _v0_1 int
    _v1_0 *ref_NoDefault_x
}

func _goml_m_trait__impl_i_PartialEq_i_Settings_i_eq(self__0 Settings, other__1 Settings) bool {
    var jp828 bool
    var t832 bool = self__0.enabled
    var t833 bool = other__1.enabled
    var inline1029 bool = t832 == t833
    jp828 = inline1029
    var jp823 bool
    if jp828 {
        var t829 int = self__0.retries
        var t830 int = other__1.retries
        var inline1031 bool = t829 == t830
        jp823 = inline1031
    } else {
        jp823 = false
    }
    if jp823 {
        var t824 string = self__0.label
        var t825 string = other__1.label
        var inline1033 bool = t824 == t825
        return inline1033
    } else {
        return false
    }
}

func _goml_m_trait__impl_i_Default_i_Settings_i_default() Settings {
    var t837 bool
    t837 = false
    var t838 int
    t838 = 0
    var t839 string
    t839 = ""
    var t840 Settings = Settings{
        enabled: t837,
        retries: t838,
        label: t839,
    }
    return t840
}

func _goml_m_trait__impl_i_Debug_i_Settings_i_debug(self__2 Settings) string {
    var x797 bool = self__2.enabled
    var x798 int = self__2.retries
    var x799 string = self__2.label
    var t843 string = "Settings { " + "enabled: "
    var t844 string
    var inline1042 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(x797)
    t844 = inline1042
    var t845 string = t843 + t844
    var t846 string = t845 + ", "
    var t847 string = t846 + "retries: "
    var t848 string
    var inline1040 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(x798)
    t848 = inline1040
    var t849 string = t847 + t848
    var t850 string = t849 + ", "
    var t851 string = t850 + "label: "
    var t852 string
    var inline1038 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x799)
    t852 = inline1038
    var t853 string = t851 + t852
    var t854 string = t853 + " }"
    return t854
}

func _goml_m_trait__impl_i_Default_i_Empty_i_default() Empty {
    var t857 Empty = Empty{}
    return t857
}

func _goml_m_trait__impl_i_Debug_i_Empty_i_debug(self__6 Empty) string {
    return "Empty {}"
}

func _goml_m_trait__impl_i_Debug_i_State_i_debug(self__7 State) string {
    switch self__7._tag {
    case 0:
        return "State::Idle"
    case 1:
        var x800 int = self__7._v1_0
        var t866 string
        var inline1044 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(x800)
        t866 = inline1044
        var t867 string = "State::Running(" + t866
        var t868 string = t867 + ")"
        return t868
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_Debug_i_Message_i_debug(self__9 Message) string {
    switch self__9._tag {
    case 0:
        var x801 string = self__9._v0_0
        var x802 int = self__9._v0_1
        var t878 string
        var inline1050 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x801)
        t878 = inline1050
        var t879 string = "Message::Data(" + t878
        var t880 string = t879 + ", "
        var t881 string
        var inline1048 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(x802)
        t881 = inline1048
        var t882 string = t880 + t881
        var t883 string = t882 + ")"
        return t883
    case 1:
        return "Message::Empty"
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var settings__15 Settings = _goml_m_trait__impl_i_Default_i_Settings_i_default()
    var t902 string = _goml_m_trait__impl_i_Debug_i_Settings_i_debug(settings__15)
    println__T_string(t902)
    var t903 Settings = Settings{
        enabled: false,
        retries: 0,
        label: "",
    }
    var t904 bool = _goml_m_trait__impl_i_PartialEq_i_Settings_i_eq(settings__15, t903)
    var t905 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t904)
    println__T_string(t905)
    var empty__16 Empty = _goml_m_trait__impl_i_Default_i_Empty_i_default()
    var t906 string = _goml_m_trait__impl_i_Debug_i_Empty_i_debug(empty__16)
    println__T_string(t906)
    var pair__17 PairDefaults__isize = _goml_m_trait__impl_i_Default_i_PairDefaults____isize_i_default()
    var t907 int = pair__17.first
    var t908 int = pair__17.second
    var t909 int = t907 + t908
    var t910 string = _goml_m_inherent_i_isize_i_isize_i_to__string(t909)
    println__T_string(t910)
    var t911 Option__isize = pair__17.nested
    var t912 bool = _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__none____T__isize(t911)
    var t913 string
    var inline1102 string = _goml_runtime_core_bool_to_string(t912)
    t913 = inline1102
    println__T_string(t913)
    var state__18 State
    state__18 = State{
        _tag: 0,
    }
    var t914 string = _goml_m_trait__impl_i_Debug_i_State_i_debug(state__18)
    var inline1098 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t914)
    _goml_runtime_core_string_println(inline1098)
    var message__19 Message
    var inline1094 string = _goml_m_trait__impl_i_Default_i_string_i_default()
    var inline1095 int = _goml_m_trait__impl_i_Default_i_isize_i_default()
    var inline1096 Message = Message{
        _tag: 0,
        _v0_0: inline1094,
        _v0_1: inline1095,
    }
    message__19 = inline1096
    var t915 string = _goml_m_trait__impl_i_Debug_i_Message_i_debug(message__19)
    var inline1091 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t915)
    _goml_runtime_core_string_println(inline1091)
    var event__20 Event
    var inline1087 string = _goml_m_trait__impl_i_Default_i_string_i_default()
    var inline1088 int = _goml_m_trait__impl_i_Default_i_isize_i_default()
    var inline1089 Event = Event{
        _tag: 0,
        _v0_0: inline1087,
        _v0_1: inline1088,
    }
    event__20 = inline1089
    var t916 string
    switch event__20._tag {
    case 0:
        var inline1074 string = event__20._v0_0
        var inline1075 int = event__20._v0_1
        var inline1078 string = "Event::Data { " + "name: "
        var inline1079 string = _goml_m_trait__impl_i_Debug_i_string_i_debug(inline1074)
        var inline1080 string = inline1078 + inline1079
        var inline1081 string = inline1080 + ", "
        var inline1082 string = inline1081 + "count: "
        var inline1083 string = _goml_m_trait__impl_i_Debug_i_isize_i_debug(inline1075)
        var inline1084 string = inline1082 + inline1083
        var inline1085 string = inline1084 + " }"
        t916 = inline1085
    case 1:
        t916 = "Event::Empty"
    default:
        panic("non-exhaustive match")
    }
    var inline1071 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t916)
    _goml_runtime_core_string_println(inline1071)
    var lazy__21 Lazy__NoDefault
    lazy__21 = Lazy__NoDefault{
        _tag: 0,
    }
    var jp918 string
    switch lazy__21._tag {
    case 0:
        jp918 = "empty"
    case 1:
        jp918 = "value"
    default:
        panic("non-exhaustive match")
    }
    var inline1067 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp918)
    _goml_runtime_core_string_println(inline1067)
    var selected__22 Selected__isize
    var inline1063 int = _goml_m_trait__impl_i_Default_i_isize_i_default()
    var inline1064 int = _goml_m_trait__impl_i_Default_i_isize_i_default()
    var inline1065 Selected__isize = Selected__isize{
        _tag: 0,
        _v0_0: inline1063,
        _v0_1: inline1064,
    }
    selected__22 = inline1065
    var jp920 string
    switch selected__22._tag {
    case 0:
        var x815 int = selected__22._v0_0
        var x816 int = selected__22._v0_1
        var t922 int = x815 + x816
        var inline1058 string = __goml_builtin_int_to_string(t922)
        jp920 = inline1058
    case 1:
        jp920 = "ignored"
    default:
        panic("non-exhaustive match")
    }
    var inline1060 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp920)
    _goml_runtime_core_string_println(inline1060)
    return struct{}{}
}

func _goml_m_trait__impl_i_Default_i_isize_i_default() int {
    return 0
}

func _goml_m_trait__impl_i_Default_i_string_i_default() string {
    return ""
}

func _goml_m_trait__impl_i_Debug_i_isize_i_debug(self__419 int) string {
    var inline1106 string = __goml_builtin_int_to_string(self__419)
    return inline1106
}

func _goml_m_trait__impl_i_Debug_i_string_i_debug(self__417 string) string {
    return self__417
}

func println__T_string(value__1 string) struct{} {
    var t949 string
    t949 = value__1
    _goml_runtime_core_string_println(t949)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__401 bool) string {
    var t953 string = _goml_runtime_core_bool_to_string(self__401)
    return t953
}

func _goml_m_trait__impl_i_Default_i_PairDefaults____isize_i_default() PairDefaults__isize {
    var t956 int
    t956 = 0
    var t957 int
    t957 = 0
    var t958 Option__isize
    t958 = Option__isize{
        _tag: 0,
    }
    var t959 PairDefaults__isize = PairDefaults__isize{
        first: t956,
        second: t957,
        nested: t958,
    }
    return t959
}

func _goml_m_inherent_i_isize_i_isize_i_to__string(self__285 int) string {
    var inline1113 int64 = int64(int(self__285))
    var inline1114 string = signed_decimal_string(inline1113)
    return inline1114
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__none____T__isize(self__719 Option__isize) bool {
    var t965 bool
    switch self__719._tag {
    case 0:
        t965 = false
    case 1:
        t965 = true
    default:
        panic("non-exhaustive match")
    }
    var t966 bool = !t965
    return t966
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__404 int) string {
    var inline1119 int64 = int64(int(self__404))
    var inline1120 string = signed_decimal_string(inline1119)
    return inline1120
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func __goml_builtin_int_to_string(value__222 int) string {
    var t983 int64 = int64(int(value__222))
    var inline1122 bool = t983 < 0
    if inline1122 {
        var inline1123 uint64 = uint64(int64(t983))
        var inline1124 uint64 = 0 - inline1123
        var inline1125 string = decimal_string(inline1124)
        var inline1126 string = "-" + inline1125
        return inline1126
    } else {
        var inline1127 uint64 = uint64(int64(t983))
        var inline1128 string = decimal_string(inline1127)
        return inline1128
    }
}

func signed_decimal_string(value__214 int64) string {
    var t993 bool = value__214 < 0
    if t993 {
        var t994 uint64 = uint64(int64(value__214))
        var t995 uint64 = 0 - t994
        var t996 string = decimal_string(t995)
        var t997 string = "-" + t996
        return t997
    } else {
        var t998 uint64 = uint64(int64(value__214))
        var t999 string = decimal_string(t998)
        return t999
    }
}

func decimal_string(value__208 uint64) string {
    var t1022 bool = value__208 == 0
    if t1022 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop1015:
        for {
            var t1016 bool = remaining__210 > 0
            if t1016 {
                var t1017_rhs uint64 = 10
                var t1017 uint64 = remaining__210 % t1017_rhs
                var t1018 uint8 = uint8(uint64(t1017))
                var t1019 uint8 = t1018 + 48
                vec_push__Vec_5uint8(reversed__209, t1019)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t1020 uint64 = compound_old353 / compound_value354
                remaining__210 = t1020
                continue
            } else {
                break Loop_loop1015
            }
        }
        var t1004 int
        var inline1138 int = vec_len__Vec_5uint8(reversed__209)
        t1004 = inline1138
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t1004)
        var offset__212 int = 0
        Loop_loop1006:
        for {
            var t1007 int
            var inline1136 int = vec_len__Vec_5uint8(reversed__209)
            t1007 = inline1136
            var t1008 bool = offset__212 < t1007
            if t1008 {
                var t1009 int
                var inline1134 int = vec_len__Vec_5uint8(reversed__209)
                t1009 = inline1134
                var t1010 int = t1009 - offset__212
                var t1011 int = t1010 - 1
                var t1012 uint8 = vec_get__Vec_5uint8(reversed__209, t1011)
                vec_push__Vec_5uint8(bytes__211, t1012)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t1013 int = compound_old358 + compound_value359
                offset__212 = t1013
                continue
            } else {
                break Loop_loop1006
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
