package main

import (
    _goml_os "os"
    _goml_reflect "reflect"
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

type ref_Point_x struct {
    value Point
}

func ref__Ref_5Point(value Point) *ref_Point_x {
    return &ref_Point_x{
        value: value,
    }
}

func ref_set__Ref_5Point(reference *ref_Point_x, value Point) struct{} {
    reference.value = value
    return struct{}{}
}

func ptr_eq__Ref_5Point(a *ref_Point_x, b *ref_Point_x) bool {
    return a == b
}

func ptr_hash__Ref_5Point(reference *ref_Point_x) uint64 {
    return uint64(_goml_reflect.ValueOf(reference).Pointer())
}

type ref_Key_x struct {
    value Key
}

func ref__Ref_3Key(value Key) *ref_Key_x {
    return &ref_Key_x{
        value: value,
    }
}

func ptr_eq__Ref_3Key(a *ref_Key_x, b *ref_Key_x) bool {
    return a == b
}

func ptr_hash__Ref_3Key(reference *ref_Key_x) uint64 {
    return uint64(_goml_reflect.ValueOf(reference).Pointer())
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

type hashmap_Ref_5Point_int32_x_entry struct {
    active bool
    key *ref_Point_x
    value int32
}

type hashmap_Ref_5Point_int32_x struct {
    buckets map[uint64][]hashmap_Ref_5Point_int32_x_entry
    hashes []uint64
    len int
}

func hashmap_new__HashMap_10Ref_5Point_5int32() *hashmap_Ref_5Point_int32_x {
    return &hashmap_Ref_5Point_int32_x{
        buckets: make(map[uint64][]hashmap_Ref_5Point_int32_x_entry),
        len: 0,
        hashes: nil,
    }
}

func hashmap_lookup__HashMap_10Ref_5Point_5int32(m *hashmap_Ref_5Point_int32_x, key *ref_Point_x) (int32, bool) {
    if m == nil {
        var zero int32
        return zero, false
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_Ref_l_Point_r__i_hash(key)
    var bucket []hashmap_Ref_5Point_int32_x_entry = m.buckets[h]
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_Ref_5Point_int32_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_PartialEq_i_Ref_l_Point_r__i_eq(entry.key, key) {
            return entry.value, true
        }
        i = i + 1
    }
    var zero int32
    return zero, false
}

func hashmap_get__HashMap_10Ref_5Point_5int32(m *hashmap_Ref_5Point_int32_x, key *ref_Point_x) Option__i32 {
    var value int32
    var ok bool
    value, ok = hashmap_lookup__HashMap_10Ref_5Point_5int32(m, key)
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

func hashmap_set__HashMap_10Ref_5Point_5int32(m *hashmap_Ref_5Point_int32_x, key *ref_Point_x, value int32) struct{} {
    var reuse_index int = -1
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_Ref_l_Point_r__i_hash(key)
    var bucket []hashmap_Ref_5Point_int32_x_entry = m.buckets[h]
    if len(bucket) == 0 {
        m.hashes = append(m.hashes, h)
    }
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_Ref_5Point_int32_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_PartialEq_i_Ref_l_Point_r__i_eq(entry.key, key) {
            bucket[i].value = value
            return struct{}{}
        }
        if !entry.active && reuse_index < 0 {
            reuse_index = i
        }
        i = i + 1
    }
    if reuse_index >= 0 {
        bucket[reuse_index] = hashmap_Ref_5Point_int32_x_entry{
            active: true,
            key: key,
            value: value,
        }
        m.len = m.len + 1
        return struct{}{}
    }
    bucket = append(bucket, hashmap_Ref_5Point_int32_x_entry{
        active: true,
        key: key,
        value: value,
    })
    m.buckets[h] = bucket
    m.len = m.len + 1
    return struct{}{}
}

type hashmap_Ref_3Key_int32_x_entry struct {
    active bool
    key *ref_Key_x
    value int32
}

type hashmap_Ref_3Key_int32_x struct {
    buckets map[uint64][]hashmap_Ref_3Key_int32_x_entry
    hashes []uint64
    len int
}

func hashmap_new__HashMap_8Ref_3Key_5int32() *hashmap_Ref_3Key_int32_x {
    return &hashmap_Ref_3Key_int32_x{
        buckets: make(map[uint64][]hashmap_Ref_3Key_int32_x_entry),
        len: 0,
        hashes: nil,
    }
}

func hashmap_lookup__HashMap_8Ref_3Key_5int32(m *hashmap_Ref_3Key_int32_x, key *ref_Key_x) (int32, bool) {
    if m == nil {
        var zero int32
        return zero, false
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_Ref_l_Key_r__i_hash(key)
    var bucket []hashmap_Ref_3Key_int32_x_entry = m.buckets[h]
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_Ref_3Key_int32_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_PartialEq_i_Ref_l_Key_r__i_eq(entry.key, key) {
            return entry.value, true
        }
        i = i + 1
    }
    var zero int32
    return zero, false
}

func hashmap_get__HashMap_8Ref_3Key_5int32(m *hashmap_Ref_3Key_int32_x, key *ref_Key_x) Option__i32 {
    var value int32
    var ok bool
    value, ok = hashmap_lookup__HashMap_8Ref_3Key_5int32(m, key)
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

func hashmap_set__HashMap_8Ref_3Key_5int32(m *hashmap_Ref_3Key_int32_x, key *ref_Key_x, value int32) struct{} {
    var reuse_index int = -1
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_Ref_l_Key_r__i_hash(key)
    var bucket []hashmap_Ref_3Key_int32_x_entry = m.buckets[h]
    if len(bucket) == 0 {
        m.hashes = append(m.hashes, h)
    }
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_Ref_3Key_int32_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_PartialEq_i_Ref_l_Key_r__i_eq(entry.key, key) {
            bucket[i].value = value
            return struct{}{}
        }
        if !entry.active && reuse_index < 0 {
            reuse_index = i
        }
        i = i + 1
    }
    if reuse_index >= 0 {
        bucket[reuse_index] = hashmap_Ref_3Key_int32_x_entry{
            active: true,
            key: key,
            value: value,
        }
        m.len = m.len + 1
        return struct{}{}
    }
    bucket = append(bucket, hashmap_Ref_3Key_int32_x_entry{
        active: true,
        key: key,
        value: value,
    })
    m.buckets[h] = bucket
    m.len = m.len + 1
    return struct{}{}
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
    x int32
    y int32
}

type Ordering int32

type Key struct {
    _tag int32
    _v1_0 int32
    _v2_0 Point
}

type Option__i32 struct {
    _tag int32
    _v1_0 int32
}

func _goml_m_trait__impl_i_Hash_i_Key_i_hash(self__6 Key) uint64 {
    switch self__6._tag {
    case 0:
        var t855_source int = 0
        var t855 uint64 = uint64(int(t855_source))
        var t856 uint64 = t855 + 14695981039346656037
        var h__7 uint64 = t856 + 1
        return h__7
    case 1:
        var x796 int32 = self__6._v1_0
        var t857_source int = 0
        var t857 uint64 = uint64(int(t857_source))
        var t858 uint64 = t857 + 14695981039346656037
        var h__9 uint64 = t858 + 2
        var t859_source int = 0
        var t859 uint64 = uint64(int(t859_source))
        var t860 uint64 = t859 + 1099511628211
        var t861 uint64 = h__9 * t860
        var t862 uint64
        var inline1045 uint64 = _goml_runtime_core_int32_hash(x796)
        t862 = inline1045
        var h__10 uint64 = t861 + t862
        return h__10
    case 2:
        var x797 Point = self__6._v2_0
        var t863_source int = 0
        var t863 uint64 = uint64(int(t863_source))
        var t864 uint64 = t863 + 14695981039346656037
        var h__12 uint64 = t864 + 3
        var t865_source int = 0
        var t865 uint64 = uint64(int(t865_source))
        var t866 uint64 = t865 + 1099511628211
        var t867 uint64 = h__12 * t866
        var t868 uint64
        var inline1047_source int = 0
        var inline1047 uint64 = uint64(int(inline1047_source))
        var inline1048 uint64 = inline1047 + 14695981039346656037
        var inline1049_source int = 0
        var inline1049 uint64 = uint64(int(inline1049_source))
        var inline1050 uint64 = inline1049 + 1099511628211
        var inline1051 uint64 = inline1048 * inline1050
        var inline1052 int32 = x797.x
        var inline1053 uint64 = _goml_m_trait__impl_i_Hash_i_i32_i_hash(inline1052)
        var inline1054 uint64 = inline1051 + inline1053
        var inline1055_source int = 0
        var inline1055 uint64 = uint64(int(inline1055_source))
        var inline1056 uint64 = inline1055 + 1099511628211
        var inline1057 uint64 = inline1054 * inline1056
        var inline1058 int32 = x797.y
        var inline1059 uint64 = _goml_m_trait__impl_i_Hash_i_i32_i_hash(inline1058)
        var inline1060 uint64 = inline1057 + inline1059
        t868 = inline1060
        var h__13 uint64 = t867 + t868
        return h__13
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_PartialEq_i_Key_i_eq(self__14 Key, other__15 Key) bool {
    switch other__15._tag {
    case 0:
        switch self__14._tag {
        case 0:
            return true
        default:
            return false
        }
    case 1:
        var x801 int32 = other__15._v1_0
        switch self__14._tag {
        case 1:
            var x805 int32 = self__14._v1_0
            var inline1062 bool = x805 == x801
            return inline1062
        default:
            return false
        }
    case 2:
        var x802 Point = other__15._v2_0
        switch self__14._tag {
        case 2:
            var x808 Point = self__14._v2_0
            var inline1065 bool
            var inline1069 int32 = x808.x
            var inline1070 int32 = x802.x
            var inline1071 bool = _goml_m_trait__impl_i_PartialEq_i_i32_i_eq(inline1069, inline1070)
            inline1065 = inline1071
            if inline1065 {
                var inline1066 int32 = x808.y
                var inline1067 int32 = x802.y
                var inline1068 bool = _goml_m_trait__impl_i_PartialEq_i_i32_i_eq(inline1066, inline1067)
                return inline1068
            } else {
                return false
            }
        default:
            return false
        }
    default:
        panic("non-exhaustive match")
    }
}

func print_opt_int(x__20 Option__i32) struct{} {
    switch x__20._tag {
    case 0:
        var inline1073 string = "none"
        var inline1074 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline1073)
        _goml_runtime_core_string_println(inline1074)
        return struct{}{}
    case 1:
        var x809 int32 = x__20._v1_0
        var inline1077 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(x809)
        _goml_runtime_core_string_println(inline1077)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var m1__22 *hashmap_Key_int32_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__Key____V__i32()
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__i32(m1__22, Key{
        _tag: 0,
    }, 10)
    var t890 Key = Key{
        _tag: 1,
        _v1_0: 1,
    }
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__i32(m1__22, t890, 20)
    var t891 int = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__i32(m1__22)
    println__T_isize(t891)
    var t892 Option__i32 = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__Key____V__i32(m1__22, Key{
        _tag: 0,
    })
    print_opt_int(t892)
    var t893 Key = Key{
        _tag: 1,
        _v1_0: 1,
    }
    var t894 bool = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__i32(m1__22, t893)
    println__T_bool(t894)
    var t895 Key = Key{
        _tag: 1,
        _v1_0: 1,
    }
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_remove____K__Key____V__i32(m1__22, t895)
    var t896 Key = Key{
        _tag: 1,
        _v1_0: 1,
    }
    var t897 bool = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__i32(m1__22, t896)
    println__T_bool(t897)
    var t898 int = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__i32(m1__22)
    println__T_isize(t898)
    var m2__23 *hashmap_Ref_5Point_int32_x = _goml_m_inherent_i_HashMap_i_H_h32f703bdb7b587c4ce71a39164c77ac4_nt_r_____V__i32()
    var t899 Point = Point{
        x: 1,
        y: 2,
    }
    var p1__24 *ref_Point_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Point(t899)
    var t900 Point = Point{
        x: 1,
        y: 2,
    }
    var p2__25 *ref_Point_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Point(t900)
    _goml_m_inherent_i_HashMap_i_H_h92e4b8ba44d2c4c111acc209f0277885_nt_r_____V__i32(m2__23, p1__24, 99)
    var t901 Option__i32 = _goml_m_inherent_i_HashMap_i_H_hf74f21a0b6b06890f12f275bddb17d70_nt_r_____V__i32(m2__23, p1__24)
    switch t901._tag {
    case 0:
        println__T_string("none")
    case 1:
        var inline1116 int32 = t901._v1_0
        println__T_i32(inline1116)
    default:
        panic("non-exhaustive match")
    }
    var t902 Option__i32
    var inline1113 Option__i32 = hashmap_get__HashMap_10Ref_5Point_5int32(m2__23, p2__25)
    t902 = inline1113
    switch t902._tag {
    case 0:
        println__T_string("none")
    case 1:
        var inline1109 int32 = t902._v1_0
        println__T_i32(inline1109)
    default:
        panic("non-exhaustive match")
    }
    var t903 Point = Point{
        x: 9,
        y: 8,
    }
    ref_set__Ref_5Point(p1__24, t903)
    var t904 Option__i32
    var inline1104 Option__i32 = hashmap_get__HashMap_10Ref_5Point_5int32(m2__23, p1__24)
    t904 = inline1104
    switch t904._tag {
    case 0:
        println__T_string("none")
    case 1:
        var inline1100 int32 = t904._v1_0
        println__T_i32(inline1100)
    default:
        panic("non-exhaustive match")
    }
    var t905 bool
    var inline1097 bool = ptr_eq__Ref_5Point(p1__24, p2__25)
    t905 = inline1097
    var inline1094 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t905)
    _goml_runtime_core_string_println(inline1094)
    var m3__26 *hashmap_Ref_3Key_int32_x
    var inline1092 *hashmap_Ref_3Key_int32_x = hashmap_new__HashMap_8Ref_3Key_5int32()
    m3__26 = inline1092
    var t906 Key = Key{
        _tag: 1,
        _v1_0: 7,
    }
    var k1__27 *ref_Key_x
    var inline1090 *ref_Key_x = ref__Ref_3Key(t906)
    k1__27 = inline1090
    var inline1087 int32 = 123
    hashmap_set__HashMap_8Ref_3Key_5int32(m3__26, k1__27, inline1087)
    var t907 Option__i32
    var inline1085 Option__i32 = hashmap_get__HashMap_8Ref_3Key_5int32(m3__26, k1__27)
    t907 = inline1085
    switch t907._tag {
    case 0:
        println__T_string("none")
        return struct{}{}
    case 1:
        var inline1081 int32 = t907._v1_0
        println__T_i32(inline1081)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_Hash_i_i32_i_hash(self__467 int32) uint64 {
    var t910 uint64 = _goml_runtime_core_int32_hash(self__467)
    return t910
}

func _goml_m_trait__impl_i_PartialEq_i_i32_i_eq(self__444 int32, other__445 int32) bool {
    var t913 bool = self__444 == other__445
    return t913
}

func println__T_string(value__1 string) struct{} {
    var t915 string
    t915 = value__1
    _goml_runtime_core_string_println(t915)
    return struct{}{}
}

func println__T_i32(value__1 int32) struct{} {
    var t918 string
    var inline1121 string = __goml_builtin_int32_to_string(value__1)
    t918 = inline1121
    _goml_runtime_core_string_println(t918)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__Key____V__i32() *hashmap_Key_int32_x {
    var t922 *hashmap_Key_int32_x = hashmap_new__HashMap_3Key_5int32()
    return t922
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__i32(self__675 *hashmap_Key_int32_x, key__676 Key, value__677 int32) struct{} {
    hashmap_set__HashMap_3Key_5int32(self__675, key__676, value__677)
    return struct{}{}
}

func println__T_isize(value__1 int) struct{} {
    var t926 string
    var inline1123 string = __goml_builtin_int_to_string(value__1)
    t926 = inline1123
    _goml_runtime_core_string_println(t926)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__i32(self__680 *hashmap_Key_int32_x) int {
    var t930 int = hashmap_len__HashMap_3Key_5int32(self__680)
    return t930
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__Key____V__i32(self__673 *hashmap_Key_int32_x, key__674 Key) Option__i32 {
    var t933 Option__i32 = hashmap_get__HashMap_3Key_5int32(self__673, key__674)
    return t933
}

func println__T_bool(value__1 bool) struct{} {
    var t935 string
    var inline1125 string = _goml_runtime_core_bool_to_string(value__1)
    t935 = inline1125
    _goml_runtime_core_string_println(t935)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__i32(self__681 *hashmap_Key_int32_x, key__682 Key) bool {
    var t939 bool = hashmap_contains__HashMap_3Key_5int32(self__681, key__682)
    return t939
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_remove____K__Key____V__i32(self__678 *hashmap_Key_int32_x, key__679 Key) struct{} {
    hashmap_remove__HashMap_3Key_5int32(self__678, key__679)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h32f703bdb7b587c4ce71a39164c77ac4_nt_r_____V__i32() *hashmap_Ref_5Point_int32_x {
    var t944 *hashmap_Ref_5Point_int32_x = hashmap_new__HashMap_10Ref_5Point_5int32()
    return t944
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Point(value__684 Point) *ref_Point_x {
    var t947 *ref_Point_x = ref__Ref_5Point(value__684)
    return t947
}

func _goml_m_inherent_i_HashMap_i_H_h92e4b8ba44d2c4c111acc209f0277885_nt_r_____V__i32(self__675 *hashmap_Ref_5Point_int32_x, key__676 *ref_Point_x, value__677 int32) struct{} {
    hashmap_set__HashMap_10Ref_5Point_5int32(self__675, key__676, value__677)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_hf74f21a0b6b06890f12f275bddb17d70_nt_r_____V__i32(self__673 *hashmap_Ref_5Point_int32_x, key__674 *ref_Point_x) Option__i32 {
    var t952 Option__i32 = hashmap_get__HashMap_10Ref_5Point_5int32(self__673, key__674)
    return t952
}

func _goml_m_trait__impl_i_PartialEq_i_Ref_l_Point_r__i_eq(self__473 *ref_Point_x, other__474 *ref_Point_x) bool {
    var t957 bool = ptr_eq__Ref_5Point(self__473, other__474)
    return t957
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func _goml_m_trait__impl_i_ToString_i_i32_i_to__string(self__407 int32) string {
    var inline1127 int64 = int64(int32(self__407))
    var inline1128 string = signed_decimal_string(inline1127)
    return inline1128
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__401 bool) string {
    var t979 string = _goml_runtime_core_bool_to_string(self__401)
    return t979
}

func __goml_builtin_int32_to_string(value__225 int32) string {
    var t982 int64 = int64(int32(value__225))
    var inline1133 bool = t982 < 0
    if inline1133 {
        var inline1134 uint64 = uint64(int64(t982))
        var inline1135 uint64 = 0 - inline1134
        var inline1136 string = decimal_string(inline1135)
        var inline1137 string = "-" + inline1136
        return inline1137
    } else {
        var inline1138 uint64 = uint64(int64(t982))
        var inline1139 string = decimal_string(inline1138)
        return inline1139
    }
}

func __goml_builtin_int_to_string(value__222 int) string {
    var t986 int64 = int64(int(value__222))
    var inline1141 bool = t986 < 0
    if inline1141 {
        var inline1142 uint64 = uint64(int64(t986))
        var inline1143 uint64 = 0 - inline1142
        var inline1144 string = decimal_string(inline1143)
        var inline1145 string = "-" + inline1144
        return inline1145
    } else {
        var inline1146 uint64 = uint64(int64(t986))
        var inline1147 string = decimal_string(inline1146)
        return inline1147
    }
}

func signed_decimal_string(value__214 int64) string {
    var t992 bool = value__214 < 0
    if t992 {
        var t993 uint64 = uint64(int64(value__214))
        var t994 uint64 = 0 - t993
        var t995 string = decimal_string(t994)
        var t996 string = "-" + t995
        return t996
    } else {
        var t997 uint64 = uint64(int64(value__214))
        var t998 string = decimal_string(t997)
        return t998
    }
}

func decimal_string(value__208 uint64) string {
    var t1021 bool = value__208 == 0
    if t1021 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop1014:
        for {
            var t1015 bool = remaining__210 > 0
            if t1015 {
                var t1016_rhs uint64 = 10
                var t1016 uint64 = remaining__210 % t1016_rhs
                var t1017 uint8 = uint8(uint64(t1016))
                var t1018 uint8 = t1017 + 48
                vec_push__Vec_5uint8(reversed__209, t1018)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t1019 uint64 = compound_old353 / compound_value354
                remaining__210 = t1019
                continue
            } else {
                break Loop_loop1014
            }
        }
        var t1003 int
        var inline1157 int = vec_len__Vec_5uint8(reversed__209)
        t1003 = inline1157
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t1003)
        var offset__212 int = 0
        Loop_loop1005:
        for {
            var t1006 int
            var inline1155 int = vec_len__Vec_5uint8(reversed__209)
            t1006 = inline1155
            var t1007 bool = offset__212 < t1006
            if t1007 {
                var t1008 int
                var inline1153 int = vec_len__Vec_5uint8(reversed__209)
                t1008 = inline1153
                var t1009 int = t1008 - offset__212
                var t1010 int = t1009 - 1
                var t1011 uint8 = vec_get__Vec_5uint8(reversed__209, t1010)
                vec_push__Vec_5uint8(bytes__211, t1011)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t1012 int = compound_old358 + compound_value359
                offset__212 = t1012
                continue
            } else {
                break Loop_loop1005
            }
        }
        var mtmp362 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__211)
        var x364 string = mtmp362._1
        return x364
    }
}

func _goml_m_trait__impl_i_Hash_i_Ref_l_Point_r__i_hash(self__475 *ref_Point_x) uint64 {
    var t1029 uint64 = ptr_hash__Ref_5Point(self__475)
    return t1029
}

func _goml_m_trait__impl_i_PartialEq_i_Ref_l_Key_r__i_eq(self__473 *ref_Key_x, other__474 *ref_Key_x) bool {
    var t1032 bool = ptr_eq__Ref_3Key(self__473, other__474)
    return t1032
}

func _goml_m_trait__impl_i_Hash_i_Ref_l_Key_r__i_hash(self__475 *ref_Key_x) uint64 {
    var t1035 uint64 = ptr_hash__Ref_3Key(self__475)
    return t1035
}

func main() {
    main0()
}
