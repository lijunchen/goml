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

func array_get__Array_2_3int(arr [2]int, index int) int {
    return arr[index]
}

func array_set__Array_2_3int(arr [2]int, index int, value int) [2]int {
    arr[index] = value
    return arr
}

func array_get__Array_3_3int(arr [3]int, index int) int {
    return arr[index]
}

func array_set__Array_3_3int(arr [3]int, index int, value int) [3]int {
    arr[index] = value
    return arr
}

func array_get__Array_2_5int32(arr [2]int32, index int) int32 {
    return arr[index]
}

func array_set__Array_2_5int32(arr [2]int32, index int, value int32) [2]int32 {
    arr[index] = value
    return arr
}

func array_get__Array_2_14Array_2_5int32(arr [2][2]int32, index int) [2]int32 {
    return arr[index]
}

func array_set__Array_2_14Array_2_5int32(arr [2][2]int32, index int, value [2]int32) [2][2]int32 {
    arr[index] = value
    return arr
}

type _goml_vec_int struct {
    items []int
}

func vec_new__Vec_3int() *_goml_vec_int {
    return &_goml_vec_int{
        items: nil,
    }
}

func vec_with_capacity__Vec_3int(capacity int) *_goml_vec_int {
    return &_goml_vec_int{
        items: make([]int, 0, capacity),
    }
}

func vec_push__Vec_3int(vec *_goml_vec_int, elem int) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_3int(vec *_goml_vec_int, index int) int {
    return vec.items[index]
}

func vec_len__Vec_3int(vec *_goml_vec_int) int {
    return int(len(vec.items))
}

type _goml_vec_int32 struct {
    items []int32
}

func vec_new__Vec_5int32() *_goml_vec_int32 {
    return &_goml_vec_int32{
        items: nil,
    }
}

func vec_with_capacity__Vec_5int32(capacity int) *_goml_vec_int32 {
    return &_goml_vec_int32{
        items: make([]int32, 0, capacity),
    }
}

func vec_push__Vec_5int32(vec *_goml_vec_int32, elem int32) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_5int32(vec *_goml_vec_int32, index int) int32 {
    return vec.items[index]
}

func vec_set__Vec_5int32(vec *_goml_vec_int32, index int, value int32) struct{} {
    vec.items[index] = value
    return struct{}{}
}

func vec_len__Vec_5int32(vec *_goml_vec_int32) int {
    return int(len(vec.items))
}

type _goml_vec_Array_2_5int32 struct {
    items [][2]int32
}

func vec_new__Vec_14Array_2_5int32() *_goml_vec_Array_2_5int32 {
    return &_goml_vec_Array_2_5int32{
        items: nil,
    }
}

func vec_with_capacity__Vec_14Array_2_5int32(capacity int) *_goml_vec_Array_2_5int32 {
    return &_goml_vec_Array_2_5int32{
        items: make([][2]int32, 0, capacity),
    }
}

func vec_push__Vec_14Array_2_5int32(vec *_goml_vec_Array_2_5int32, elem [2]int32) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_14Array_2_5int32(vec *_goml_vec_Array_2_5int32, index int) [2]int32 {
    return vec.items[index]
}

func vec_set__Vec_14Array_2_5int32(vec *_goml_vec_Array_2_5int32, index int, value [2]int32) struct{} {
    vec.items[index] = value
    return struct{}{}
}

func vec_len__Vec_14Array_2_5int32(vec *_goml_vec_Array_2_5int32) int {
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

type ref_Array_2_5int32_x struct {
    value [2]int32
}

func ref__Ref_14Array_2_5int32(value [2]int32) *ref_Array_2_5int32_x {
    return &ref_Array_2_5int32_x{
        value: value,
    }
}

func ref_get__Ref_14Array_2_5int32(reference *ref_Array_2_5int32_x) [2]int32 {
    return reference.value
}

func ref_set__Ref_14Array_2_5int32(reference *ref_Array_2_5int32_x, value [2]int32) struct{} {
    reference.value = value
    return struct{}{}
}

type hashmap_string_int32_x_entry struct {
    active bool
    key string
    value int32
}

type hashmap_string_int32_x struct {
    indices map[string]int
    entries []hashmap_string_int32_x_entry
    len int
}

func hashmap_new__HashMap_6string_5int32() *hashmap_string_int32_x {
    return &hashmap_string_int32_x{
        indices: make(map[string]int),
        entries: nil,
        len: 0,
    }
}

func hashmap_lookup__HashMap_6string_5int32(m *hashmap_string_int32_x, key string) (int32, bool) {
    if m == nil {
        var zero int32
        return zero, false
    }
    var index int
    var found bool
    index, found = m.indices[key]
    if !found {
        var zero int32
        return zero, false
    }
    var entry hashmap_string_int32_x_entry = m.entries[index]
    if entry.active {
        return entry.value, true
    }
    var zero int32
    return zero, false
}

func hashmap_get__HashMap_6string_5int32(m *hashmap_string_int32_x, key string) Option__i32 {
    var value int32
    var ok bool
    value, ok = hashmap_lookup__HashMap_6string_5int32(m, key)
    if ok {
        return Option__i32{
            _tag: 1,
            _v1_0: value,
        }
    }
    return Option__i32{
        _tag: 0,
    }
}

func hashmap_set__HashMap_6string_5int32(m *hashmap_string_int32_x, key string, value int32) struct{} {
    if m == nil {
        return struct{}{}
    }
    var index int
    var found bool
    index, found = m.indices[key]
    if found {
        var entry hashmap_string_int32_x_entry = m.entries[index]
        if entry.active {
            m.entries[index].value = value
            return struct{}{}
        }
        m.entries[index] = hashmap_string_int32_x_entry{
            active: true,
            key: key,
            value: value,
        }
        m.len = m.len + 1
        return struct{}{}
    }
    index = len(m.entries)
    m.indices[key] = index
    m.entries = append(m.entries, hashmap_string_int32_x_entry{
        active: true,
        key: key,
        value: value,
    })
    m.len = m.len + 1
    return struct{}{}
}

type Tuple2_11Array2_3int_3int struct {
    _0 [2]int
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

type Holder struct {
    data [2]int32
    vecs *_goml_vec_Array_2_5int32
}

type Ordering int32

type Option__i32 struct {
    _tag int32
    _v1_0 int32
}

func main0() struct{} {
    var t863 [2]int = [2]int{31, 32}
    var t864 int = array_get__Array_2_3int(t863, 1)
    println__T_isize(t864)
    var arr__2 [3]int = [3]int{1, 2, 3}
    var t865 int = array_get__Array_3_3int(arr__2, 0)
    println__T_isize(t865)
    var arr2__3 [3]int = [3]int{4, 5, 6}
    var place_root799 [3]int = arr2__3
    var index800 int = 1
    array_get__Array_3_3int(place_root799, index800)
    var value802 int = 50
    var t866 [3]int = array_set__Array_3_3int(place_root799, index800, value802)
    arr2__3 = t866
    var t868 int = array_get__Array_3_3int(arr2__3, 1)
    println__T_isize(t868)
    var t869 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__isize()
    var t870 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__isize(t869, 7)
    var t871 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__isize(t870, 8)
    var vec__4 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__isize(t871, 9)
    var t872 int = vec_get__Vec_3int(vec__4, 2)
    println__T_isize(t872)
    var t873 *_goml_vec_int32
    var inline1143 *_goml_vec_int32 = vec_new__Vec_5int32()
    t873 = inline1143
    var t874 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__i32(t873, 10)
    var t875 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__i32(t874, 11)
    var vec2__5 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__i32(t875, 12)
    var index807 int = 0
    vec_get__Vec_5int32(vec2__5, index807)
    var value809 int32 = 100
    vec_set__Vec_5int32(vec2__5, index807, value809)
    var t877 int32 = vec_get__Vec_5int32(vec2__5, 0)
    var inline1140 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t877)
    _goml_runtime_core_string_println(inline1140)
    var s__6 []int32
    var inline1136 int = 0
    var inline1137 int = 2
    var inline1138 []int32 = vec2__5.items[inline1136:inline1137]
    s__6 = inline1138
    var t878 int32 = s__6[1]
    var inline1133 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t878)
    _goml_runtime_core_string_println(inline1133)
    var map__7 *hashmap_string_int32_x
    var inline1131 *hashmap_string_int32_x = hashmap_new__HashMap_6string_5int32()
    map__7 = inline1131
    var index814 string = "a"
    hashmap_get__HashMap_6string_5int32(map__7, index814)
    var value816 int32 = 13
    hashmap_set__HashMap_6string_5int32(map__7, index814, value816)
    var t880 Option__i32 = hashmap_get__HashMap_6string_5int32(map__7, "a")
    switch t880._tag {
    case 0:
        println__T_string("none")
    case 1:
        var inline1127 int32 = t880._v1_0
        println__T_i32(inline1127)
    default:
        panic("non-exhaustive match")
    }
    var t881 Option__i32 = hashmap_get__HashMap_6string_5int32(map__7, "missing")
    switch t881._tag {
    case 0:
        println__T_string("none")
    case 1:
        var inline1122 int32 = t881._v1_0
        println__T_i32(inline1122)
    default:
        panic("non-exhaustive match")
    }
    var t882 [2]int32 = [2]int32{1, 2}
    var t883 [2]int32 = [2]int32{3, 4}
    var matrix__8 [2][2]int32 = [2][2]int32{t882, t883}
    var place_root820 [2][2]int32 = matrix__8
    var index821 int = 1
    var place822 [2]int32 = array_get__Array_2_14Array_2_5int32(place_root820, index821)
    var index823 int = 0
    array_get__Array_2_5int32(place822, index823)
    var value825 int32 = 30
    var t884 [2]int32 = array_set__Array_2_5int32(place822, index823, value825)
    var t885 [2][2]int32 = array_set__Array_2_14Array_2_5int32(place_root820, index821, t884)
    matrix__8 = t885
    var t887 [2]int32 = array_get__Array_2_14Array_2_5int32(matrix__8, 1)
    var t888 int32 = array_get__Array_2_5int32(t887, 0)
    var inline1118 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t888)
    _goml_runtime_core_string_println(inline1118)
    var t889 [2]int = [2]int{14, 15}
    var pair__9 Tuple2_11Array2_3int_3int = Tuple2_11Array2_3int_3int{
        _0: t889,
        _1: 0,
    }
    var place_root828 Tuple2_11Array2_3int_3int = pair__9
    var place829 [2]int = place_root828._0
    var index830 int = 1
    array_get__Array_2_3int(place829, index830)
    var value832 int = 150
    var t890 [2]int = array_set__Array_2_3int(place829, index830, value832)
    var t891 int = place_root828._1
    var t892 Tuple2_11Array2_3int_3int = Tuple2_11Array2_3int_3int{
        _0: t890,
        _1: t891,
    }
    pair__9 = t892
    var t894 [2]int = pair__9._0
    var t895 int = array_get__Array_2_3int(t894, 1)
    var inline1115 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t895)
    _goml_runtime_core_string_println(inline1115)
    var t896 [2]int32 = [2]int32{16, 17}
    var t897 *_goml_vec_Array_2_5int32
    var inline1113 *_goml_vec_Array_2_5int32 = vec_new__Vec_14Array_2_5int32()
    t897 = inline1113
    var t898 [2]int32 = [2]int32{18, 19}
    var t899 *_goml_vec_Array_2_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T___l_i32_x3b_2_r_(t897, t898)
    var t900 [2]int32 = [2]int32{20, 21}
    var t901 *_goml_vec_Array_2_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T___l_i32_x3b_2_r_(t899, t900)
    var holder__10 Holder = Holder{
        data: t896,
        vecs: t901,
    }
    var place_root835 Holder = holder__10
    var place836 [2]int32 = place_root835.data
    var index837 int = 0
    array_get__Array_2_5int32(place836, index837)
    var value839 int32 = 160
    var t902 [2]int32 = array_set__Array_2_5int32(place836, index837, value839)
    var t903 *_goml_vec_Array_2_5int32 = place_root835.vecs
    var t904 Holder = Holder{
        data: t902,
        vecs: t903,
    }
    holder__10 = t904
    var t906 [2]int32 = holder__10.data
    var t907 int32 = array_get__Array_2_5int32(t906, 0)
    var inline1110 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t907)
    _goml_runtime_core_string_println(inline1110)
    var place_root842 Holder = holder__10
    var place843 *_goml_vec_Array_2_5int32 = place_root842.vecs
    var index844 int = 1
    var place845 [2]int32 = vec_get__Vec_14Array_2_5int32(place843, index844)
    var index846 int = 0
    array_get__Array_2_5int32(place845, index846)
    var value848 int32 = 200
    var t908 [2]int32 = array_set__Array_2_5int32(place845, index846, value848)
    vec_set__Vec_14Array_2_5int32(place843, index844, t908)
    var t910 *_goml_vec_Array_2_5int32 = holder__10.vecs
    var t911 [2]int32 = vec_get__Vec_14Array_2_5int32(t910, 1)
    var t912 int32 = array_get__Array_2_5int32(t911, 0)
    var inline1107 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t912)
    _goml_runtime_core_string_println(inline1107)
    var t913 [2]int32 = [2]int32{22, 23}
    var r__11 *ref_Array_2_5int32_x
    var inline1105 *ref_Array_2_5int32_x = ref__Ref_14Array_2_5int32(t913)
    r__11 = inline1105
    var place_root852 [2]int32 = ref_get__Ref_14Array_2_5int32(r__11)
    var index853 int = 1
    array_get__Array_2_5int32(place_root852, index853)
    var value855 int32 = 230
    var t914 [2]int32 = array_set__Array_2_5int32(place_root852, index853, value855)
    ref_set__Ref_14Array_2_5int32(r__11, t914)
    var t916 [2]int32
    var inline1103 [2]int32 = ref_get__Ref_14Array_2_5int32(r__11)
    t916 = inline1103
    var t917 int32 = array_get__Array_2_5int32(t916, 1)
    var inline1100 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t917)
    _goml_runtime_core_string_println(inline1100)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t919 string
    t919 = value__1
    _goml_runtime_core_string_println(t919)
    return struct{}{}
}

func println__T_i32(value__1 int32) struct{} {
    var t922 string
    var inline1146 string = __goml_builtin_int32_to_string(value__1)
    t922 = inline1146
    _goml_runtime_core_string_println(t922)
    return struct{}{}
}

func println__T_isize(value__1 int) struct{} {
    var t925 string
    var inline1148 string = __goml_builtin_int_to_string(value__1)
    t925 = inline1148
    _goml_runtime_core_string_println(t925)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__isize() *_goml_vec_int {
    var t929 *_goml_vec_int = vec_new__Vec_3int()
    return t929
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__isize(self__513 *_goml_vec_int, elem__514 int) *_goml_vec_int {
    var t932 int
    var inline1158 int = vec_len__Vec_3int(self__513)
    t932 = inline1158
    var t933 int = t932 + 1
    var result__515 *_goml_vec_int
    var inline1156 *_goml_vec_int = vec_with_capacity__Vec_3int(t933)
    result__515 = inline1156
    var index__516 int = 0
    Loop_loop935:
    for {
        var t936 int
        var inline1152 int = vec_len__Vec_3int(self__513)
        t936 = inline1152
        var t937 bool = index__516 < t936
        if t937 {
            var t938 int = vec_get__Vec_3int(self__513, index__516)
            vec_push__Vec_3int(result__515, t938)
            var compound_old575 int = index__516
            var compound_value576 int = 1
            var t939 int = compound_old575 + compound_value576
            index__516 = t939
            continue
        } else {
            break Loop_loop935
        }
    }
    vec_push__Vec_3int(result__515, elem__514)
    return result__515
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__i32(self__513 *_goml_vec_int32, elem__514 int32) *_goml_vec_int32 {
    var t946 int
    var inline1168 int = vec_len__Vec_5int32(self__513)
    t946 = inline1168
    var t947 int = t946 + 1
    var result__515 *_goml_vec_int32
    var inline1166 *_goml_vec_int32 = vec_with_capacity__Vec_5int32(t947)
    result__515 = inline1166
    var index__516 int = 0
    Loop_loop949:
    for {
        var t950 int
        var inline1162 int = vec_len__Vec_5int32(self__513)
        t950 = inline1162
        var t951 bool = index__516 < t950
        if t951 {
            var t952 int32 = vec_get__Vec_5int32(self__513, index__516)
            vec_push__Vec_5int32(result__515, t952)
            var compound_old575 int = index__516
            var compound_value576 int = 1
            var t953 int = compound_old575 + compound_value576
            index__516 = t953
            continue
        } else {
            break Loop_loop949
        }
    }
    vec_push__Vec_5int32(result__515, elem__514)
    return result__515
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T___l_i32_x3b_2_r_(self__513 *_goml_vec_Array_2_5int32, elem__514 [2]int32) *_goml_vec_Array_2_5int32 {
    var t966 int
    var inline1178 int = vec_len__Vec_14Array_2_5int32(self__513)
    t966 = inline1178
    var t967 int = t966 + 1
    var result__515 *_goml_vec_Array_2_5int32
    var inline1176 *_goml_vec_Array_2_5int32 = vec_with_capacity__Vec_14Array_2_5int32(t967)
    result__515 = inline1176
    var index__516 int = 0
    Loop_loop969:
    for {
        var t970 int
        var inline1172 int = vec_len__Vec_14Array_2_5int32(self__513)
        t970 = inline1172
        var t971 bool = index__516 < t970
        if t971 {
            var t972 [2]int32 = vec_get__Vec_14Array_2_5int32(self__513, index__516)
            vec_push__Vec_14Array_2_5int32(result__515, t972)
            var compound_old575 int = index__516
            var compound_value576 int = 1
            var t973 int = compound_old575 + compound_value576
            index__516 = t973
            continue
        } else {
            break Loop_loop969
        }
    }
    vec_push__Vec_14Array_2_5int32(result__515, elem__514)
    return result__515
}

func _goml_m_trait__impl_i_ToString_i_i32_i_to__string(self__407 int32) string {
    var inline1180 int64 = int64(int32(self__407))
    var inline1181 string = signed_decimal_string(inline1180)
    return inline1181
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__404 int) string {
    var inline1183 int64 = int64(int(self__404))
    var inline1184 string = signed_decimal_string(inline1183)
    return inline1184
}

func __goml_builtin_int32_to_string(value__225 int32) string {
    var t1015 int64 = int64(int32(value__225))
    var inline1186 bool = t1015 < 0
    if inline1186 {
        var inline1187 uint64 = uint64(int64(t1015))
        var inline1188 uint64 = 0 - inline1187
        var inline1189 string = decimal_string(inline1188)
        var inline1190 string = "-" + inline1189
        return inline1190
    } else {
        var inline1191 uint64 = uint64(int64(t1015))
        var inline1192 string = decimal_string(inline1191)
        return inline1192
    }
}

func __goml_builtin_int_to_string(value__222 int) string {
    var t1019 int64 = int64(int(value__222))
    var inline1194 bool = t1019 < 0
    if inline1194 {
        var inline1195 uint64 = uint64(int64(t1019))
        var inline1196 uint64 = 0 - inline1195
        var inline1197 string = decimal_string(inline1196)
        var inline1198 string = "-" + inline1197
        return inline1198
    } else {
        var inline1199 uint64 = uint64(int64(t1019))
        var inline1200 string = decimal_string(inline1199)
        return inline1200
    }
}

func signed_decimal_string(value__214 int64) string {
    var t1025 bool = value__214 < 0
    if t1025 {
        var t1026 uint64 = uint64(int64(value__214))
        var t1027 uint64 = 0 - t1026
        var t1028 string = decimal_string(t1027)
        var t1029 string = "-" + t1028
        return t1029
    } else {
        var t1030 uint64 = uint64(int64(value__214))
        var t1031 string = decimal_string(t1030)
        return t1031
    }
}

func decimal_string(value__208 uint64) string {
    var t1054 bool = value__208 == 0
    if t1054 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop1047:
        for {
            var t1048 bool = remaining__210 > 0
            if t1048 {
                var t1049_rhs uint64 = 10
                var t1049 uint64 = remaining__210 % t1049_rhs
                var t1050 uint8 = uint8(uint64(t1049))
                var t1051 uint8 = t1050 + 48
                vec_push__Vec_5uint8(reversed__209, t1051)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t1052 uint64 = compound_old353 / compound_value354
                remaining__210 = t1052
                continue
            } else {
                break Loop_loop1047
            }
        }
        var t1036 int
        var inline1210 int = vec_len__Vec_5uint8(reversed__209)
        t1036 = inline1210
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t1036)
        var offset__212 int = 0
        Loop_loop1038:
        for {
            var t1039 int
            var inline1208 int = vec_len__Vec_5uint8(reversed__209)
            t1039 = inline1208
            var t1040 bool = offset__212 < t1039
            if t1040 {
                var t1041 int
                var inline1206 int = vec_len__Vec_5uint8(reversed__209)
                t1041 = inline1206
                var t1042 int = t1041 - offset__212
                var t1043 int = t1042 - 1
                var t1044 uint8 = vec_get__Vec_5uint8(reversed__209, t1043)
                vec_push__Vec_5uint8(bytes__211, t1044)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t1045 int = compound_old358 + compound_value359
                offset__212 = t1045
                continue
            } else {
                break Loop_loop1038
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
