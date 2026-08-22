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

func _goml_runtime_core_int32_hash(x int32) uint64 {
    return uint64(x)
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

type hashmap_Key_int32_x_entry struct {
    active bool
    key Key
    value int32
}

type hashmap_Key_int32_x struct {
    buckets map[uint64][]hashmap_Key_int32_x_entry
    hashes []uint64
    len int
}

func hashmap_new__HashMap_3Key_5int32() *hashmap_Key_int32_x {
    return &hashmap_Key_int32_x{
        buckets: make(map[uint64][]hashmap_Key_int32_x_entry),
        len: 0,
        hashes: nil,
    }
}

func hashmap_len__HashMap_3Key_5int32(m *hashmap_Key_int32_x) int {
    if m == nil {
        return 0
    }
    return m.len
}

func hashmap_lookup__HashMap_3Key_5int32(m *hashmap_Key_int32_x, key Key) (int32, bool) {
    if m == nil {
        var zero int32
        return zero, false
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_Key_i_hash(key)
    var bucket []hashmap_Key_int32_x_entry = m.buckets[h]
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_Key_int32_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_PartialEq_i_Key_i_eq(entry.key, key) {
            return entry.value, true
        }
        i = i + 1
    }
    var zero int32
    return zero, false
}

func hashmap_get__HashMap_3Key_5int32(m *hashmap_Key_int32_x, key Key) Option__i32 {
    var value int32
    var ok bool
    value, ok = hashmap_lookup__HashMap_3Key_5int32(m, key)
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

func hashmap_set__HashMap_3Key_5int32(m *hashmap_Key_int32_x, key Key, value int32) struct{} {
    var reuse_index int = -1
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_Key_i_hash(key)
    var bucket []hashmap_Key_int32_x_entry = m.buckets[h]
    if len(bucket) == 0 {
        m.hashes = append(m.hashes, h)
    }
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_Key_int32_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_PartialEq_i_Key_i_eq(entry.key, key) {
            bucket[i].value = value
            return struct{}{}
        }
        if !entry.active && reuse_index < 0 {
            reuse_index = i
        }
        i = i + 1
    }
    if reuse_index >= 0 {
        bucket[reuse_index] = hashmap_Key_int32_x_entry{
            active: true,
            key: key,
            value: value,
        }
        m.len = m.len + 1
        return struct{}{}
    }
    bucket = append(bucket, hashmap_Key_int32_x_entry{
        active: true,
        key: key,
        value: value,
    })
    m.buckets[h] = bucket
    m.len = m.len + 1
    return struct{}{}
}

func hashmap_remove__HashMap_3Key_5int32(m *hashmap_Key_int32_x, key Key) struct{} {
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_Key_i_hash(key)
    var bucket []hashmap_Key_int32_x_entry = m.buckets[h]
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_Key_int32_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_PartialEq_i_Key_i_eq(entry.key, key) {
            var zero hashmap_Key_int32_x_entry
            bucket[i] = zero
            m.len = m.len - 1
            return struct{}{}
        }
        i = i + 1
    }
    return struct{}{}
}

func hashmap_contains__HashMap_3Key_5int32(m *hashmap_Key_int32_x, key Key) bool {
    if m == nil {
        return false
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_Key_i_hash(key)
    var bucket []hashmap_Key_int32_x_entry = m.buckets[h]
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_Key_int32_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_PartialEq_i_Key_i_eq(entry.key, key) {
            return true
        }
        i = i + 1
    }
    return false
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

type Ordering int32

type Key struct {
    _tag int32
    _v1_0 int32
}

type Option__i32 struct {
    _tag int32
    _v1_0 int32
}

func _goml_m_trait__impl_i_Hash_i_Key_i_hash(self__0 Key) uint64 {
    switch self__0._tag {
    case 0:
        var t823_source int = 0
        var t823 uint64 = uint64(int(t823_source))
        var t824 uint64 = t823 + 14695981039346656037
        var h__1 uint64 = t824 + 1
        return h__1
    case 1:
        var x796 int32 = self__0._v1_0
        var t825_source int = 0
        var t825 uint64 = uint64(int(t825_source))
        var t826 uint64 = t825 + 14695981039346656037
        var h__3 uint64 = t826 + 2
        var t827_source int = 0
        var t827 uint64 = uint64(int(t827_source))
        var t828 uint64 = t827 + 1099511628211
        var t829 uint64 = h__3 * t828
        var t830 uint64
        var inline964 uint64 = _goml_runtime_core_int32_hash(x796)
        t830 = inline964
        var h__4 uint64 = t829 + t830
        return h__4
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_PartialEq_i_Key_i_eq(self__5 Key, other__6 Key) bool {
    switch other__6._tag {
    case 0:
        switch self__5._tag {
        case 0:
            return true
        default:
            return false
        }
    case 1:
        var x800 int32 = other__6._v1_0
        switch self__5._tag {
        case 1:
            var x802 int32 = self__5._v1_0
            var inline966 bool = x802 == x800
            return inline966
        default:
            return false
        }
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var v__11 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__isize()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__isize(v__11, 10)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__isize(v__11, 20)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__isize(v__11, 30)
    var t847 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__isize(v__11, 0)
    println__T_isize(t847)
    var t848 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__isize(v__11, 1)
    println__T_isize(t848)
    var t849 int
    var inline1020 int = 2
    var inline1021 int = vec_get__Vec_3int(v__11, inline1020)
    t849 = inline1021
    var inline1017 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t849)
    _goml_runtime_core_string_println(inline1017)
    var t850 int
    var inline1015 int = vec_len__Vec_3int(v__11)
    t850 = inline1015
    var inline1012 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t850)
    _goml_runtime_core_string_println(inline1012)
    var m__12 *hashmap_Key_int32_x
    var inline1010 *hashmap_Key_int32_x = hashmap_new__HashMap_3Key_5int32()
    m__12 = inline1010
    var inline1007 int32 = 10
    hashmap_set__HashMap_3Key_5int32(m__12, Key{
        _tag: 0,
    }, inline1007)
    var t851 Key = Key{
        _tag: 1,
        _v1_0: 1,
    }
    var inline1004 int32 = 20
    hashmap_set__HashMap_3Key_5int32(m__12, t851, inline1004)
    var t852 int
    var inline1002 int = hashmap_len__HashMap_3Key_5int32(m__12)
    t852 = inline1002
    var inline999 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t852)
    _goml_runtime_core_string_println(inline999)
    var t853 Option__i32
    var inline997 Option__i32 = hashmap_get__HashMap_3Key_5int32(m__12, Key{
        _tag: 0,
    })
    t853 = inline997
    switch t853._tag {
    case 0:
        println__T_string("none")
    case 1:
        var inline993 int32 = t853._v1_0
        println__T_i32(inline993)
    default:
        panic("non-exhaustive match")
    }
    var t854 Key = Key{
        _tag: 1,
        _v1_0: 1,
    }
    var t855 bool
    var inline990 bool = hashmap_contains__HashMap_3Key_5int32(m__12, t854)
    t855 = inline990
    var inline987 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t855)
    _goml_runtime_core_string_println(inline987)
    var t856 Key = Key{
        _tag: 1,
        _v1_0: 1,
    }
    hashmap_remove__HashMap_3Key_5int32(m__12, t856)
    var t857 Key = Key{
        _tag: 1,
        _v1_0: 1,
    }
    var t858 bool
    var inline983 bool = hashmap_contains__HashMap_3Key_5int32(m__12, t857)
    t858 = inline983
    var inline980 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t858)
    _goml_runtime_core_string_println(inline980)
    var t859 int
    var inline978 int = hashmap_len__HashMap_3Key_5int32(m__12)
    t859 = inline978
    var inline975 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t859)
    _goml_runtime_core_string_println(inline975)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t867 string
    t867 = value__1
    _goml_runtime_core_string_println(t867)
    return struct{}{}
}

func println__T_i32(value__1 int32) struct{} {
    var t870 string
    var inline1024 string = __goml_builtin_int32_to_string(value__1)
    t870 = inline1024
    _goml_runtime_core_string_println(t870)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__isize() *_goml_vec_int {
    var t874 *_goml_vec_int = vec_new__Vec_3int()
    return t874
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__isize(self__511 *_goml_vec_int, elem__512 int) struct{} {
    vec_push__Vec_3int(self__511, elem__512)
    return struct{}{}
}

func println__T_isize(value__1 int) struct{} {
    var t878 string
    var inline1026 string = __goml_builtin_int_to_string(value__1)
    t878 = inline1026
    _goml_runtime_core_string_println(t878)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__isize(self__521 *_goml_vec_int, index__522 int) int {
    var t882 int = vec_get__Vec_3int(self__521, index__522)
    return t882
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__404 int) string {
    var inline1033 int64 = int64(int(self__404))
    var inline1034 string = signed_decimal_string(inline1033)
    return inline1034
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__401 bool) string {
    var t915 string = _goml_runtime_core_bool_to_string(self__401)
    return t915
}

func __goml_builtin_int32_to_string(value__225 int32) string {
    var t918 int64 = int64(int32(value__225))
    var inline1036 bool = t918 < 0
    if inline1036 {
        var inline1037 uint64 = uint64(int64(t918))
        var inline1038 uint64 = 0 - inline1037
        var inline1039 string = decimal_string(inline1038)
        var inline1040 string = "-" + inline1039
        return inline1040
    } else {
        var inline1041 uint64 = uint64(int64(t918))
        var inline1042 string = decimal_string(inline1041)
        return inline1042
    }
}

func __goml_builtin_int_to_string(value__222 int) string {
    var t922 int64 = int64(int(value__222))
    var inline1044 bool = t922 < 0
    if inline1044 {
        var inline1045 uint64 = uint64(int64(t922))
        var inline1046 uint64 = 0 - inline1045
        var inline1047 string = decimal_string(inline1046)
        var inline1048 string = "-" + inline1047
        return inline1048
    } else {
        var inline1049 uint64 = uint64(int64(t922))
        var inline1050 string = decimal_string(inline1049)
        return inline1050
    }
}

func signed_decimal_string(value__214 int64) string {
    var t928 bool = value__214 < 0
    if t928 {
        var t929 uint64 = uint64(int64(value__214))
        var t930 uint64 = 0 - t929
        var t931 string = decimal_string(t930)
        var t932 string = "-" + t931
        return t932
    } else {
        var t933 uint64 = uint64(int64(value__214))
        var t934 string = decimal_string(t933)
        return t934
    }
}

func decimal_string(value__208 uint64) string {
    var t957 bool = value__208 == 0
    if t957 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop950:
        for {
            var t951 bool = remaining__210 > 0
            if t951 {
                var t952_rhs uint64 = 10
                var t952 uint64 = remaining__210 % t952_rhs
                var t953 uint8 = uint8(uint64(t952))
                var t954 uint8 = t953 + 48
                vec_push__Vec_5uint8(reversed__209, t954)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t955 uint64 = compound_old353 / compound_value354
                remaining__210 = t955
                continue
            } else {
                break Loop_loop950
            }
        }
        var t939 int
        var inline1060 int = vec_len__Vec_5uint8(reversed__209)
        t939 = inline1060
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t939)
        var offset__212 int = 0
        Loop_loop941:
        for {
            var t942 int
            var inline1058 int = vec_len__Vec_5uint8(reversed__209)
            t942 = inline1058
            var t943 bool = offset__212 < t942
            if t943 {
                var t944 int
                var inline1056 int = vec_len__Vec_5uint8(reversed__209)
                t944 = inline1056
                var t945 int = t944 - offset__212
                var t946 int = t945 - 1
                var t947 uint8 = vec_get__Vec_5uint8(reversed__209, t946)
                vec_push__Vec_5uint8(bytes__211, t947)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t948 int = compound_old358 + compound_value359
                offset__212 = t948
                continue
            } else {
                break Loop_loop941
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
