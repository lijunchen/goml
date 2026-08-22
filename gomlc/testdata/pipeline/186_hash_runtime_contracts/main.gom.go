package main

import (
    _goml_fmt "fmt"
    _goml_reflect "reflect"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
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

type ref_CollisionKey_x struct {
    value CollisionKey
}

func ref__Ref_12CollisionKey(value CollisionKey) *ref_CollisionKey_x {
    return &ref_CollisionKey_x{
        value: value,
    }
}

func ref_set__Ref_12CollisionKey(reference *ref_CollisionKey_x, value CollisionKey) struct{} {
    reference.value = value
    return struct{}{}
}

func ptr_eq__Ref_12CollisionKey(a *ref_CollisionKey_x, b *ref_CollisionKey_x) bool {
    return a == b
}

func ptr_hash__Ref_12CollisionKey(reference *ref_CollisionKey_x) uint64 {
    return uint64(_goml_reflect.ValueOf(reference).Pointer())
}

type hashmap_CollisionKey_int32_x_entry struct {
    active bool
    key CollisionKey
    value int32
}

type hashmap_CollisionKey_int32_x struct {
    buckets map[uint64][]hashmap_CollisionKey_int32_x_entry
    hashes []uint64
    len int
}

func hashmap_new__HashMap_12CollisionKey_5int32() *hashmap_CollisionKey_int32_x {
    return &hashmap_CollisionKey_int32_x{
        buckets: make(map[uint64][]hashmap_CollisionKey_int32_x_entry),
        len: 0,
        hashes: nil,
    }
}

func hashmap_len__HashMap_12CollisionKey_5int32(m *hashmap_CollisionKey_int32_x) int {
    if m == nil {
        return 0
    }
    return m.len
}

func hashmap_lookup__HashMap_12CollisionKey_5int32(m *hashmap_CollisionKey_int32_x, key CollisionKey) (int32, bool) {
    if m == nil {
        var zero int32
        return zero, false
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_CollisionKey_i_hash(key)
    var bucket []hashmap_CollisionKey_int32_x_entry = m.buckets[h]
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_CollisionKey_int32_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_PartialEq_i_CollisionKey_i_eq(entry.key, key) {
            return entry.value, true
        }
        i = i + 1
    }
    var zero int32
    return zero, false
}

func hashmap_get__HashMap_12CollisionKey_5int32(m *hashmap_CollisionKey_int32_x, key CollisionKey) Option__i32 {
    var value int32
    var ok bool
    value, ok = hashmap_lookup__HashMap_12CollisionKey_5int32(m, key)
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

func hashmap_set__HashMap_12CollisionKey_5int32(m *hashmap_CollisionKey_int32_x, key CollisionKey, value int32) struct{} {
    var reuse_index int = -1
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_CollisionKey_i_hash(key)
    var bucket []hashmap_CollisionKey_int32_x_entry = m.buckets[h]
    if len(bucket) == 0 {
        m.hashes = append(m.hashes, h)
    }
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_CollisionKey_int32_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_PartialEq_i_CollisionKey_i_eq(entry.key, key) {
            bucket[i].value = value
            return struct{}{}
        }
        if !entry.active && reuse_index < 0 {
            reuse_index = i
        }
        i = i + 1
    }
    if reuse_index >= 0 {
        bucket[reuse_index] = hashmap_CollisionKey_int32_x_entry{
            active: true,
            key: key,
            value: value,
        }
        m.len = m.len + 1
        return struct{}{}
    }
    bucket = append(bucket, hashmap_CollisionKey_int32_x_entry{
        active: true,
        key: key,
        value: value,
    })
    m.buckets[h] = bucket
    m.len = m.len + 1
    return struct{}{}
}

func hashmap_remove__HashMap_12CollisionKey_5int32(m *hashmap_CollisionKey_int32_x, key CollisionKey) struct{} {
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_CollisionKey_i_hash(key)
    var bucket []hashmap_CollisionKey_int32_x_entry = m.buckets[h]
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_CollisionKey_int32_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_PartialEq_i_CollisionKey_i_eq(entry.key, key) {
            var zero hashmap_CollisionKey_int32_x_entry
            bucket[i] = zero
            m.len = m.len - 1
            return struct{}{}
        }
        i = i + 1
    }
    return struct{}{}
}

type hashmap_Ref_12CollisionKey_string_x_entry struct {
    active bool
    key *ref_CollisionKey_x
    value string
}

type hashmap_Ref_12CollisionKey_string_x struct {
    buckets map[uint64][]hashmap_Ref_12CollisionKey_string_x_entry
    hashes []uint64
    len int
}

func hashmap_new__HashMap_18Ref_12CollisionKey_6string() *hashmap_Ref_12CollisionKey_string_x {
    return &hashmap_Ref_12CollisionKey_string_x{
        buckets: make(map[uint64][]hashmap_Ref_12CollisionKey_string_x_entry),
        len: 0,
        hashes: nil,
    }
}

func hashmap_lookup__HashMap_18Ref_12CollisionKey_6string(m *hashmap_Ref_12CollisionKey_string_x, key *ref_CollisionKey_x) (string, bool) {
    if m == nil {
        var zero string
        return zero, false
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_Ref_l_CollisionKey_r__i_hash(key)
    var bucket []hashmap_Ref_12CollisionKey_string_x_entry = m.buckets[h]
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_Ref_12CollisionKey_string_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_PartialEq_i_Ref_l_CollisionKey_r__i_eq(entry.key, key) {
            return entry.value, true
        }
        i = i + 1
    }
    var zero string
    return zero, false
}

func hashmap_get__HashMap_18Ref_12CollisionKey_6string(m *hashmap_Ref_12CollisionKey_string_x, key *ref_CollisionKey_x) Option__string {
    var value string
    var ok bool
    value, ok = hashmap_lookup__HashMap_18Ref_12CollisionKey_6string(m, key)
    if ok {
        return Option__string{
            _tag: 1,
            _v1_0: value,
        }
    }
    return Option__string{
        _tag: 0,
    }
}

func hashmap_set__HashMap_18Ref_12CollisionKey_6string(m *hashmap_Ref_12CollisionKey_string_x, key *ref_CollisionKey_x, value string) struct{} {
    var reuse_index int = -1
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_Ref_l_CollisionKey_r__i_hash(key)
    var bucket []hashmap_Ref_12CollisionKey_string_x_entry = m.buckets[h]
    if len(bucket) == 0 {
        m.hashes = append(m.hashes, h)
    }
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_Ref_12CollisionKey_string_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_PartialEq_i_Ref_l_CollisionKey_r__i_eq(entry.key, key) {
            bucket[i].value = value
            return struct{}{}
        }
        if !entry.active && reuse_index < 0 {
            reuse_index = i
        }
        i = i + 1
    }
    if reuse_index >= 0 {
        bucket[reuse_index] = hashmap_Ref_12CollisionKey_string_x_entry{
            active: true,
            key: key,
            value: value,
        }
        m.len = m.len + 1
        return struct{}{}
    }
    bucket = append(bucket, hashmap_Ref_12CollisionKey_string_x_entry{
        active: true,
        key: key,
        value: value,
    })
    m.buckets[h] = bucket
    m.len = m.len + 1
    return struct{}{}
}

type FloatKey struct {
    value float64
}

type CollisionKey struct {
    value int32
}

type Ordering int32

const (
    Less Ordering = 0
    Equal Ordering = 1
    Greater Ordering = 2
)

type Option__Ordering struct {
    _tag int32
    _v1_0 Ordering
}

type Option__string struct {
    _tag int32
    _v1_0 string
}

type Option__i32 struct {
    _tag int32
    _v1_0 int32
}

func _goml_m_trait__impl_i_PartialEq_i_CollisionKey_i_eq(self__5 CollisionKey, other__6 CollisionKey) bool {
    var t960 int32 = self__5.value
    var t961 int32 = other__6.value
    var t962 bool = t960 == t961
    return t962
}

func _goml_m_trait__impl_i_Hash_i_CollisionKey_i_hash(self__7 CollisionKey) uint64 {
    return 1
}

func print_opt_int(value__10 Option__i32) struct{} {
    switch value__10._tag {
    case 0:
        var inline1935 string = "none"
        var inline1936 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline1935)
        _goml_runtime_core_string_println(inline1936)
        return struct{}{}
    case 1:
        var x414 int32 = value__10._v1_0
        var inline1939 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(x414)
        _goml_runtime_core_string_println(inline1939)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func float_comparison_contracts() struct{} {
    var zero32__12 float32 = 0
    var negative_zero32__13 float32 = -zero32__12
    var t974 bool = zero32__12 == negative_zero32__13
    var t975 string
    var inline1979 string = _goml_runtime_core_bool_to_string(t974)
    t975 = inline1979
    var inline1976 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t975)
    _goml_runtime_core_string_println(inline1976)
    var zero64__14 float64 = 0
    var negative_zero64__15 float64 = -zero64__14
    var t976 bool = zero64__14 == negative_zero64__15
    var t977 string
    var inline1974 string = _goml_runtime_core_bool_to_string(t976)
    t977 = inline1974
    var inline1971 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t977)
    _goml_runtime_core_string_println(inline1971)
    var t980 bool
    var inline1969 bool = _goml_m_trait__impl_i_PartialEq_i_f64_i_eq(zero64__14, negative_zero64__15)
    t980 = inline1969
    var t981 string
    var inline1965 string = _goml_runtime_core_bool_to_string(t980)
    t981 = inline1965
    var inline1962 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t981)
    _goml_runtime_core_string_println(inline1962)
    var nan__16 float64 = zero64__14 / zero64__14
    var t982 bool = nan__16 == nan__16
    var t983 string
    var inline1960 string = _goml_runtime_core_bool_to_string(t982)
    t983 = inline1960
    var inline1957 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t983)
    _goml_runtime_core_string_println(inline1957)
    var t984 Option__Ordering
    var inline1950 bool = nan__16 < nan__16
    if inline1950 {
        var inline1951 Option__Ordering = Option__Ordering{
            _tag: 1,
            _v1_0: Less,
        }
        t984 = inline1951
    } else {
        var inline1952 bool = nan__16 > nan__16
        if inline1952 {
            var inline1953 Option__Ordering = Option__Ordering{
                _tag: 1,
                _v1_0: Greater,
            }
            t984 = inline1953
        } else {
            var inline1954 bool = nan__16 == nan__16
            if inline1954 {
                var inline1955 Option__Ordering = Option__Ordering{
                    _tag: 1,
                    _v1_0: Equal,
                }
                t984 = inline1955
            } else {
                t984 = Option__Ordering{
                    _tag: 0,
                }
            }
        }
    }
    var t985 bool
    var inline1947 bool = _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__some____T__Ordering(t984)
    var inline1948 bool = !inline1947
    t985 = inline1948
    var t986 string
    var inline1945 string = _goml_runtime_core_bool_to_string(t985)
    t986 = inline1945
    var inline1942 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t986)
    _goml_runtime_core_string_println(inline1942)
    return struct{}{}
}

func collision_contracts() struct{} {
    var values__17 *hashmap_CollisionKey_int32_x = _goml_m_inherent_i_HashMap_i_H_hc55bb71e9219d0c59c91622ae099ea85_onKey____V__i32()
    var t988 CollisionKey = CollisionKey{
        value: 1,
    }
    _goml_m_inherent_i_HashMap_i_H_h13877db3209e2e8534a556971ced5ab2_onKey____V__i32(values__17, t988, 10)
    var t989 CollisionKey = CollisionKey{
        value: 2,
    }
    _goml_m_inherent_i_HashMap_i_H_h13877db3209e2e8534a556971ced5ab2_onKey____V__i32(values__17, t989, 20)
    var t990 CollisionKey = CollisionKey{
        value: 3,
    }
    _goml_m_inherent_i_HashMap_i_H_h13877db3209e2e8534a556971ced5ab2_onKey____V__i32(values__17, t990, 30)
    var t991 CollisionKey = CollisionKey{
        value: 2,
    }
    _goml_m_inherent_i_HashMap_i_H_h1189b7c51290244a02a1a6d496e4da69_onKey____V__i32(values__17, t991)
    var t992 CollisionKey = CollisionKey{
        value: 1,
    }
    var t993 Option__i32 = _goml_m_inherent_i_HashMap_i_H_h7d750efaf8a1803c39420b89990faf20_onKey____V__i32(values__17, t992)
    print_opt_int(t993)
    var t994 CollisionKey = CollisionKey{
        value: 2,
    }
    var t995 Option__i32 = _goml_m_inherent_i_HashMap_i_H_h7d750efaf8a1803c39420b89990faf20_onKey____V__i32(values__17, t994)
    print_opt_int(t995)
    var t996 CollisionKey = CollisionKey{
        value: 3,
    }
    var t997 Option__i32 = _goml_m_inherent_i_HashMap_i_H_h7d750efaf8a1803c39420b89990faf20_onKey____V__i32(values__17, t996)
    print_opt_int(t997)
    var t998 CollisionKey = CollisionKey{
        value: 4,
    }
    _goml_m_inherent_i_HashMap_i_H_h13877db3209e2e8534a556971ced5ab2_onKey____V__i32(values__17, t998, 40)
    var t999 int = _goml_m_inherent_i_HashMap_i_H_h282dac09c2296c58cbcd9cfca496474b_onKey____V__i32(values__17)
    println__T_isize(t999)
    var t1000 CollisionKey = CollisionKey{
        value: 4,
    }
    var t1001 Option__i32
    var inline2027 Option__i32 = hashmap_get__HashMap_12CollisionKey_5int32(values__17, t1000)
    t1001 = inline2027
    print_opt_int(t1001)
    var t1002 CollisionKey = CollisionKey{
        value: 4,
    }
    var inline2024 int32 = 41
    hashmap_set__HashMap_12CollisionKey_5int32(values__17, t1002, inline2024)
    var t1003 int
    var inline2022 int = hashmap_len__HashMap_12CollisionKey_5int32(values__17)
    t1003 = inline2022
    var inline2019 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t1003)
    _goml_runtime_core_string_println(inline2019)
    var t1004 CollisionKey = CollisionKey{
        value: 4,
    }
    var t1005 Option__i32
    var inline2017 Option__i32 = hashmap_get__HashMap_12CollisionKey_5int32(values__17, t1004)
    t1005 = inline2017
    switch t1005._tag {
    case 0:
        println__T_string("none")
    case 1:
        var inline2013 int32 = t1005._v1_0
        println__T_i32(inline2013)
    default:
        panic("non-exhaustive match")
    }
    var t1006 CollisionKey = CollisionKey{
        value: 99,
    }
    hashmap_remove__HashMap_12CollisionKey_5int32(values__17, t1006)
    var t1007 CollisionKey = CollisionKey{
        value: 99,
    }
    hashmap_remove__HashMap_12CollisionKey_5int32(values__17, t1007)
    var t1008 int
    var inline2006 int = hashmap_len__HashMap_12CollisionKey_5int32(values__17)
    t1008 = inline2006
    var inline2003 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t1008)
    _goml_runtime_core_string_println(inline2003)
    var index__18 *ref_int32_x
    var inline2000 int32 = 0
    var inline2001 *ref_int32_x = ref__Ref_5int32(inline2000)
    index__18 = inline2001
    Loop_loop1011:
    for {
        var t1012 int32
        var inline1993 int32 = ref_get__Ref_5int32(index__18)
        t1012 = inline1993
        var t1013 bool = t1012 < 2000
        if t1013 {
            var t1014 int32
            var inline1991 int32 = ref_get__Ref_5int32(index__18)
            t1014 = inline1991
            var t1015 int32 = 1000 + t1014
            var key__19 CollisionKey = CollisionKey{
                value: t1015,
            }
            var t1016 int32
            var inline1989 int32 = ref_get__Ref_5int32(index__18)
            t1016 = inline1989
            hashmap_set__HashMap_12CollisionKey_5int32(values__17, key__19, t1016)
            hashmap_remove__HashMap_12CollisionKey_5int32(values__17, key__19)
            var t1017 int32
            var inline1983 int32 = ref_get__Ref_5int32(index__18)
            t1017 = inline1983
            var t1018 int32 = t1017 + 1
            ref_set__Ref_5int32(index__18, t1018)
            continue
        } else {
            break Loop_loop1011
        }
    }
    var t1010 int
    var inline1998 int = hashmap_len__HashMap_12CollisionKey_5int32(values__17)
    t1010 = inline1998
    var inline1995 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t1010)
    _goml_runtime_core_string_println(inline1995)
    return struct{}{}
}

func reference_contracts() struct{} {
    var values__20 *hashmap_Ref_12CollisionKey_string_x = _goml_m_inherent_i_HashMap_i_H_h88509d3b0dfc2392ba920998ecb79184_r_____V__string()
    var t1020 CollisionKey = CollisionKey{
        value: 1,
    }
    var key__21 *ref_CollisionKey_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__CollisionKey(t1020)
    var t1021 CollisionKey = CollisionKey{
        value: 1,
    }
    var equal_value__23 *ref_CollisionKey_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__CollisionKey(t1021)
    var inline2069 string = "identity"
    hashmap_set__HashMap_18Ref_12CollisionKey_6string(values__20, key__21, inline2069)
    var t1022 bool
    var inline2067 bool = ptr_eq__Ref_12CollisionKey(key__21, key__21)
    t1022 = inline2067
    var inline2064 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1022)
    _goml_runtime_core_string_println(inline2064)
    var t1023 bool
    var inline2062 bool = ptr_eq__Ref_12CollisionKey(key__21, equal_value__23)
    t1023 = inline2062
    var inline2059 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1023)
    _goml_runtime_core_string_println(inline2059)
    var t1024 uint64
    var inline2057 uint64 = ptr_hash__Ref_12CollisionKey(key__21)
    t1024 = inline2057
    var t1025 uint64
    var inline2055 uint64 = ptr_hash__Ref_12CollisionKey(key__21)
    t1025 = inline2055
    var t1026 bool = t1024 == t1025
    var inline2052 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1026)
    _goml_runtime_core_string_println(inline2052)
    var t1027 Option__string
    var inline2050 Option__string = hashmap_get__HashMap_18Ref_12CollisionKey_6string(values__20, key__21)
    t1027 = inline2050
    switch t1027._tag {
    case 0:
        println__T_string("none")
    case 1:
        var inline2046 string = t1027._v1_0
        println__T_string(inline2046)
    default:
        panic("non-exhaustive match")
    }
    var t1028 Option__string
    var inline2043 Option__string = hashmap_get__HashMap_18Ref_12CollisionKey_6string(values__20, equal_value__23)
    t1028 = inline2043
    switch t1028._tag {
    case 0:
        println__T_string("none")
    case 1:
        var inline2039 string = t1028._v1_0
        println__T_string(inline2039)
    default:
        panic("non-exhaustive match")
    }
    var t1029 CollisionKey = CollisionKey{
        value: 99,
    }
    ref_set__Ref_12CollisionKey(key__21, t1029)
    var t1030 Option__string
    var inline2034 Option__string = hashmap_get__HashMap_18Ref_12CollisionKey_6string(values__20, key__21)
    t1030 = inline2034
    switch t1030._tag {
    case 0:
        println__T_string("none")
        return struct{}{}
    case 1:
        var inline2030 string = t1030._v1_0
        println__T_string(inline2030)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    float_comparison_contracts()
    collision_contracts()
    reference_contracts()
    return struct{}{}
}

func _goml_m_trait__impl_i_PartialEq_i_f64_i_eq(self__205 float64, other__206 float64) bool {
    var t1418 bool = self__205 == other__206
    return t1418
}

func println__T_string(value__1 string) struct{} {
    var t1442 string
    t1442 = value__1
    _goml_runtime_core_string_println(t1442)
    return struct{}{}
}

func println__T_i32(value__1 int32) struct{} {
    var t1445 string
    var inline2573 string = _goml_runtime_core_int32_to_string(value__1)
    t1445 = inline2573
    _goml_runtime_core_string_println(t1445)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t1449 string = _goml_runtime_core_bool_to_string(self__148)
    return t1449
}

func _goml_m_inherent_i_HashMap_i_H_hc55bb71e9219d0c59c91622ae099ea85_onKey____V__i32() *hashmap_CollisionKey_int32_x {
    var t1456 *hashmap_CollisionKey_int32_x = hashmap_new__HashMap_12CollisionKey_5int32()
    return t1456
}

func _goml_m_inherent_i_HashMap_i_H_h13877db3209e2e8534a556971ced5ab2_onKey____V__i32(self__422 *hashmap_CollisionKey_int32_x, key__423 CollisionKey, value__424 int32) struct{} {
    hashmap_set__HashMap_12CollisionKey_5int32(self__422, key__423, value__424)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h1189b7c51290244a02a1a6d496e4da69_onKey____V__i32(self__425 *hashmap_CollisionKey_int32_x, key__426 CollisionKey) struct{} {
    hashmap_remove__HashMap_12CollisionKey_5int32(self__425, key__426)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h7d750efaf8a1803c39420b89990faf20_onKey____V__i32(self__420 *hashmap_CollisionKey_int32_x, key__421 CollisionKey) Option__i32 {
    var t1463 Option__i32 = hashmap_get__HashMap_12CollisionKey_5int32(self__420, key__421)
    return t1463
}

func println__T_isize(value__1 int) struct{} {
    var t1465 string
    var inline2576 string = _goml_runtime_core_int_to_string(value__1)
    t1465 = inline2576
    _goml_runtime_core_string_println(t1465)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h282dac09c2296c58cbcd9cfca496474b_onKey____V__i32(self__427 *hashmap_CollisionKey_int32_x) int {
    var t1469 int = hashmap_len__HashMap_12CollisionKey_5int32(self__427)
    return t1469
}

func _goml_m_inherent_i_HashMap_i_H_h88509d3b0dfc2392ba920998ecb79184_r_____V__string() *hashmap_Ref_12CollisionKey_string_x {
    var t1480 *hashmap_Ref_12CollisionKey_string_x = hashmap_new__HashMap_18Ref_12CollisionKey_6string()
    return t1480
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__CollisionKey(value__431 CollisionKey) *ref_CollisionKey_x {
    var t1483 *ref_CollisionKey_x = ref__Ref_12CollisionKey(value__431)
    return t1483
}

func _goml_m_trait__impl_i_PartialEq_i_Ref_l_CollisionKey_r__i_eq(self__220 *ref_CollisionKey_x, other__221 *ref_CollisionKey_x) bool {
    var t1491 bool = ptr_eq__Ref_12CollisionKey(self__220, other__221)
    return t1491
}

func _goml_m_trait__impl_i_Hash_i_Ref_l_CollisionKey_r__i_hash(self__222 *ref_CollisionKey_x) uint64 {
    var t1494 uint64 = ptr_hash__Ref_12CollisionKey(self__222)
    return t1494
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_trait__impl_i_ToString_i_i32_i_to__string(self__154 int32) string {
    var t1504 string = _goml_runtime_core_int32_to_string(self__154)
    return t1504
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__some____T__Ordering(self__465 Option__Ordering) bool {
    switch self__465._tag {
    case 0:
        return false
    case 1:
        return true
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__151 int) string {
    var t1511 string = _goml_runtime_core_int_to_string(self__151)
    return t1511
}

func main() {
    main0()
}
