package main

import (
    _goml_os "os"
)

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

type _goml_vec_int struct {
    items []int
}

func vec_new__Vec_3int() *_goml_vec_int {
    return &_goml_vec_int{
        items: nil,
    }
}

func vec_get__Vec_3int(vec *_goml_vec_int, index int) int {
    return vec.items[index]
}

func vec_set__Vec_3int(vec *_goml_vec_int, index int, value int) struct{} {
    vec.items[index] = value
    return struct{}{}
}

func vec_len__Vec_3int(vec *_goml_vec_int) int {
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

type ref_string_x struct {
    value string
}

func ref__Ref_6string(value string) *ref_string_x {
    return &ref_string_x{
        value: value,
    }
}

func ref_get__Ref_6string(reference *ref_string_x) string {
    return reference.value
}

func ref_set__Ref_6string(reference *ref_string_x, value string) struct{} {
    reference.value = value
    return struct{}{}
}

type hashmap_string_int_x_entry struct {
    active bool
    key string
    value int
}

type hashmap_string_int_x struct {
    indices map[string]int
    entries []hashmap_string_int_x_entry
    len int
}

func hashmap_new__HashMap_6string_3int() *hashmap_string_int_x {
    return &hashmap_string_int_x{
        indices: make(map[string]int),
        entries: nil,
        len: 0,
    }
}

func hashmap_len__HashMap_6string_3int(m *hashmap_string_int_x) int {
    if m == nil {
        return 0
    }
    return m.len
}

func hashmap_lookup__HashMap_6string_3int(m *hashmap_string_int_x, key string) (int, bool) {
    if m == nil {
        var zero int
        return zero, false
    }
    var index int
    var found bool
    index, found = m.indices[key]
    if !found {
        var zero int
        return zero, false
    }
    var entry hashmap_string_int_x_entry = m.entries[index]
    if entry.active {
        return entry.value, true
    }
    var zero int
    return zero, false
}

func hashmap_get__HashMap_6string_3int(m *hashmap_string_int_x, key string) Option__isize {
    var value int
    var ok bool
    value, ok = hashmap_lookup__HashMap_6string_3int(m, key)
    if ok {
        return Option__isize{
            _tag: 1,
            _v1_0: value,
        }
    }
    return Option__isize{
        _tag: 0,
    }
}

func hashmap_set__HashMap_6string_3int(m *hashmap_string_int_x, key string, value int) struct{} {
    if m == nil {
        return struct{}{}
    }
    var index int
    var found bool
    index, found = m.indices[key]
    if found {
        var entry hashmap_string_int_x_entry = m.entries[index]
        if entry.active {
            m.entries[index].value = value
            return struct{}{}
        }
        m.entries[index] = hashmap_string_int_x_entry{
            active: true,
            key: key,
            value: value,
        }
        m.len = m.len + 1
        return struct{}{}
    }
    index = len(m.entries)
    m.indices[key] = index
    m.entries = append(m.entries, hashmap_string_int_x_entry{
        active: true,
        key: key,
        value: value,
    })
    m.len = m.len + 1
    return struct{}{}
}

type Tuple2_3int_3int struct {
    _0 int
    _1 int
}

type Tuple2_6string_3int struct {
    _0 string
    _1 int
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

type Point struct {
    x int
    y int
}

type Ordering int32

type Option__isize struct {
    _tag int32
    _v1_0 int
}

func record(log__0 *ref_string_x, label__1 string, value__2 int) int {
    var t837 string
    var inline1008 string = ref_get__Ref_6string(log__0)
    t837 = inline1008
    var t838 string = t837 + label__1
    ref_set__Ref_6string(log__0, t838)
    return value__2
}

func record_point(log__3 *ref_string_x, label__4 string, value__5 Point) Point {
    var t841 string
    var inline1012 string = ref_get__Ref_6string(log__3)
    t841 = inline1012
    var t842 string = t841 + label__4
    ref_set__Ref_6string(log__3, t842)
    return value__5
}

func record_vec(log__6 *ref_string_x, label__7 string, value__8 *_goml_vec_int) *_goml_vec_int {
    var t845 string
    var inline1016 string = ref_get__Ref_6string(log__6)
    t845 = inline1016
    var t846 string = t845 + label__7
    ref_set__Ref_6string(log__6, t846)
    return value__8
}

func main0() struct{} {
    var number__9 int = 5
    var compound_old799 int = number__9
    var compound_value800 int = 3
    var t848 int = compound_old799 + compound_value800
    number__9 = t848
    var compound_old802 int = number__9
    var compound_value803 int = 2
    var t850 int = compound_old802 * compound_value803
    number__9 = t850
    var compound_old805 int = number__9
    var compound_value806 int = 1
    var t852 int = compound_old805 >> compound_value806
    number__9 = t852
    var t854 string = _goml_m_inherent_i_isize_i_isize_i_to__string(number__9)
    println__T_string(t854)
    var direct__10 Point = Point{
        x: 3,
        y: 4,
    }
    var place_root809 Point = direct__10
    var place810 int = place_root809.x
    var value811 int = 5
    var t855 int = place810 + value811
    var t856 int = place_root809.y
    var t857 Point = Point{
        x: t855,
        y: t856,
    }
    direct__10 = t857
    var t859 int = direct__10.x
    var t860 string = _goml_m_inherent_i_isize_i_isize_i_to__string(t859)
    var t861 string = "" + t860
    var t862 string = t861 + ","
    var t863 int = direct__10.y
    var t864 string = _goml_m_inherent_i_isize_i_isize_i_to__string(t863)
    var t865 string = t862 + t864
    println__T_string(t865)
    var pair__11 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: 2,
        _1: 9,
    }
    var place_root814 Tuple2_3int_3int = pair__11
    var place815 int = place_root814._0
    var value816 int = 3
    var t866 int = place815 * value816
    var t867 int = place_root814._1
    var t868 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: t866,
        _1: t867,
    }
    pair__11 = t868
    var t870 int = pair__11._0
    var t871 string = _goml_m_inherent_i_isize_i_isize_i_to__string(t870)
    var t872 string = "" + t871
    var t873 string = t872 + ","
    var t874 int = pair__11._1
    var t875 string = _goml_m_inherent_i_isize_i_isize_i_to__string(t874)
    var t876 string = t873 + t875
    println__T_string(t876)
    var log__12 *ref_string_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string("")
    var base__13 Point = Point{
        x: 1,
        y: 2,
    }
    var struct_update_field__789__0 int = record(log__12, "F", 7)
    var struct_update_base__789 Point = record_point(log__12, "B", base__13)
    var t877 int = struct_update_base__789.y
    var t879 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    println__T_string(t879)
    var t881 string = _goml_m_inherent_i_isize_i_isize_i_to__string(struct_update_field__789__0)
    var t882 string = "" + t881
    var t883 string = t882 + ","
    var t885 string = _goml_m_inherent_i_isize_i_isize_i_to__string(t877)
    var t886 string = t883 + t885
    println__T_string(t886)
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__12, "")
    var t887 int = record(log__12, "A", 10)
    var t888 int = record(log__12, "B", 20)
    var t889 [2]int = [2]int{t887, t888}
    var values__15 *_goml_vec_int = func(values [2]int) *_goml_vec_int {
        var storage struct {
            vector _goml_vec_int
            values [2]int
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t889)
    var place_root822 *_goml_vec_int = record_vec(log__12, "R", values__15)
    var index823 int = record(log__12, "I", 1)
    var place824 int = vec_get__Vec_3int(place_root822, index823)
    var value825 int = record(log__12, "V", 5)
    var t890 int = place824 + value825
    vec_set__Vec_3int(place_root822, index823, t890)
    var t892 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    println__T_string(t892)
    var t893 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__isize(values__15, 0)
    var t894 string = _goml_m_inherent_i_isize_i_isize_i_to__string(t893)
    var t895 string = "" + t894
    var t896 string = t895 + ","
    var t897 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__isize(values__15, 1)
    var t898 string = _goml_m_inherent_i_isize_i_isize_i_to__string(t897)
    var t899 string = t896 + t898
    var inline1073 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t899)
    _goml_runtime_core_string_println(inline1073)
    var inline1070 string = ""
    ref_set__Ref_6string(log__12, inline1070)
    var t900 string = "" + "k"
    var t901 int
    var inline1064 string = "K"
    var inline1065 int = 1
    var inline1066 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    var inline1067 string = inline1066 + inline1064
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__12, inline1067)
    t901 = inline1065
    var t902 string
    var inline1062 string = __goml_builtin_int_to_string(t901)
    t902 = inline1062
    var t903 string = t900 + t902
    var t904 int
    var inline1056 string = "V"
    var inline1057 int = 11
    var inline1058 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    var inline1059 string = inline1058 + inline1056
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__12, inline1059)
    t904 = inline1057
    var t905 Tuple2_6string_3int = Tuple2_6string_3int{
        _0: t903,
        _1: t904,
    }
    var t906 int
    var inline1050 string = "A"
    var inline1051 int = 1
    var inline1052 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    var inline1053 string = inline1052 + inline1050
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__12, inline1053)
    t906 = inline1051
    var t907 Tuple2_6string_3int = Tuple2_6string_3int{
        _0: "same",
        _1: t906,
    }
    var t908 int
    var inline1044 string = "B"
    var inline1045 int = 2
    var inline1046 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    var inline1047 string = inline1046 + inline1044
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__12, inline1047)
    t908 = inline1045
    var t909 Tuple2_6string_3int = Tuple2_6string_3int{
        _0: "same",
        _1: t908,
    }
    var table__16 *hashmap_string_int_x = &hashmap_string_int_x{
        indices: make(map[string]int, 3),
        entries: make([]hashmap_string_int_x_entry, 0, 3),
        len: 0,
    }
    hashmap_set__HashMap_6string_3int(table__16, t905._0, t905._1)
    hashmap_set__HashMap_6string_3int(table__16, t907._0, t907._1)
    hashmap_set__HashMap_6string_3int(table__16, t909._0, t909._1)
    var t911 string
    var inline1042 string = ref_get__Ref_6string(log__12)
    t911 = inline1042
    var inline1039 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t911)
    _goml_runtime_core_string_println(inline1039)
    var mtmp831 Option__isize
    var inline1036 string = "same"
    var inline1037 Option__isize = hashmap_get__HashMap_6string_3int(table__16, inline1036)
    mtmp831 = inline1037
    var jp913 string
    switch mtmp831._tag {
    case 0:
        jp913 = "missing"
    case 1:
        var x832 int = mtmp831._v1_0
        var inline1018 string = __goml_builtin_int_to_string(x832)
        jp913 = inline1018
    default:
        panic("non-exhaustive match")
    }
    var inline1033 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp913)
    _goml_runtime_core_string_println(inline1033)
    var empty_values__18 *_goml_vec_int
    var inline1031 *_goml_vec_int = vec_new__Vec_3int()
    empty_values__18 = inline1031
    var empty_table__19 *hashmap_string_int_x
    var inline1029 *hashmap_string_int_x = hashmap_new__HashMap_6string_3int()
    empty_table__19 = inline1029
    var t914 string = "" + "empty="
    var t915 int
    var inline1027 int = vec_len__Vec_3int(empty_values__18)
    t915 = inline1027
    var t916 int
    var inline1025 int = hashmap_len__HashMap_6string_3int(empty_table__19)
    t916 = inline1025
    var t917 int = t915 + t916
    var t918 string
    var inline1023 string = __goml_builtin_int_to_string(t917)
    t918 = inline1023
    var t919 string = t914 + t918
    var t920 string = t919 + " {ok}"
    var inline1020 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t920)
    _goml_runtime_core_string_println(inline1020)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(self__685 *ref_string_x) string {
    var t924 string = ref_get__Ref_6string(self__685)
    return t924
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(self__686 *ref_string_x, value__687 string) struct{} {
    ref_set__Ref_6string(self__686, value__687)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t928 string
    t928 = value__1
    _goml_runtime_core_string_println(t928)
    return struct{}{}
}

func _goml_m_inherent_i_isize_i_isize_i_to__string(self__285 int) string {
    var inline1077 int64 = int64(int(self__285))
    var inline1078 string = signed_decimal_string(inline1077)
    return inline1078
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string(value__684 string) *ref_string_x {
    var t935 *ref_string_x = ref__Ref_6string(value__684)
    return t935
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__isize(self__521 *_goml_vec_int, index__522 int) int {
    var t938 int = vec_get__Vec_3int(self__521, index__522)
    return t938
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func __goml_builtin_int_to_string(value__222 int) string {
    var t958 int64 = int64(int(value__222))
    var inline1080 bool = t958 < 0
    if inline1080 {
        var inline1081 uint64 = uint64(int64(t958))
        var inline1082 uint64 = 0 - inline1081
        var inline1083 string = decimal_string(inline1082)
        var inline1084 string = "-" + inline1083
        return inline1084
    } else {
        var inline1085 uint64 = uint64(int64(t958))
        var inline1086 string = decimal_string(inline1085)
        return inline1086
    }
}

func signed_decimal_string(value__214 int64) string {
    var t964 bool = value__214 < 0
    if t964 {
        var t965 uint64 = uint64(int64(value__214))
        var t966 uint64 = 0 - t965
        var t967 string = decimal_string(t966)
        var t968 string = "-" + t967
        return t968
    } else {
        var t969 uint64 = uint64(int64(value__214))
        var t970 string = decimal_string(t969)
        return t970
    }
}

func decimal_string(value__208 uint64) string {
    var t993 bool = value__208 == 0
    if t993 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop986:
        for {
            var t987 bool = remaining__210 > 0
            if t987 {
                var t988_rhs uint64 = 10
                var t988 uint64 = remaining__210 % t988_rhs
                var t989 uint8 = uint8(uint64(t988))
                var t990 uint8 = t989 + 48
                vec_push__Vec_5uint8(reversed__209, t990)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t991 uint64 = compound_old353 / compound_value354
                remaining__210 = t991
                continue
            } else {
                break Loop_loop986
            }
        }
        var t975 int
        var inline1096 int = vec_len__Vec_5uint8(reversed__209)
        t975 = inline1096
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t975)
        var offset__212 int = 0
        Loop_loop977:
        for {
            var t978 int
            var inline1094 int = vec_len__Vec_5uint8(reversed__209)
            t978 = inline1094
            var t979 bool = offset__212 < t978
            if t979 {
                var t980 int
                var inline1092 int = vec_len__Vec_5uint8(reversed__209)
                t980 = inline1092
                var t981 int = t980 - offset__212
                var t982 int = t981 - 1
                var t983 uint8 = vec_get__Vec_5uint8(reversed__209, t982)
                vec_push__Vec_5uint8(bytes__211, t983)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t984 int = compound_old358 + compound_value359
                offset__212 = t984
                continue
            } else {
                break Loop_loop977
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
