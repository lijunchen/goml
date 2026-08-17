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

func hashmap_get__HashMap_12CollisionKey_5int32(m *hashmap_CollisionKey_int32_x, key CollisionKey) Option__int32 {
    var value int32
    var ok bool
    value, ok = hashmap_lookup__HashMap_12CollisionKey_5int32(m, key)
    if ok {
        return Option__int32{
            _tag: 1,
            _v1_0: value,
        }
    }
    return Option__int32{
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

type Option__Ordering interface {
    isOption__Ordering()
}

type Option__Ordering_None struct {}

func (_ Option__Ordering_None) isOption__Ordering() {}

type Option__Ordering_Some struct {
    _0 Ordering
}

func (_ Option__Ordering_Some) isOption__Ordering() {}

type Option__string struct {
    _tag int32
    _v1_0 string
}

type Option__int32 struct {
    _tag int32
    _v1_0 int32
}

func _goml_m_trait__impl_i_PartialEq_i_CollisionKey_i_eq(self__5 CollisionKey, other__6 CollisionKey) bool {
    var t957 int32 = self__5.value
    var t958 int32 = other__6.value
    var t959 bool = t957 == t958
    return t959
}

func _goml_m_trait__impl_i_Hash_i_CollisionKey_i_hash(self__7 CollisionKey) uint64 {
    return 1
}

func print_opt_int(value__10 Option__int32) struct{} {
    switch value__10._tag {
    case 0:
        var inline1932 string = "none"
        var inline1933 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline1932)
        _goml_runtime_core_string_println(inline1933)
        return struct{}{}
    case 1:
        var x411 int32 = value__10._v1_0
        var inline1936 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x411)
        _goml_runtime_core_string_println(inline1936)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func float_comparison_contracts() struct{} {
    var zero32__12 float32 = 0
    var negative_zero32__13 float32 = -zero32__12
    var t971 bool = zero32__12 == negative_zero32__13
    var t972 string
    var inline1976 string = _goml_runtime_core_bool_to_string(t971)
    t972 = inline1976
    var inline1973 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t972)
    _goml_runtime_core_string_println(inline1973)
    var zero64__14 float64 = 0
    var negative_zero64__15 float64 = -zero64__14
    var t973 bool = zero64__14 == negative_zero64__15
    var t974 string
    var inline1971 string = _goml_runtime_core_bool_to_string(t973)
    t974 = inline1971
    var inline1968 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t974)
    _goml_runtime_core_string_println(inline1968)
    var t977 bool
    var inline1966 bool = _goml_m_trait__impl_i_PartialEq_i_float64_i_eq(zero64__14, negative_zero64__15)
    t977 = inline1966
    var t978 string
    var inline1962 string = _goml_runtime_core_bool_to_string(t977)
    t978 = inline1962
    var inline1959 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t978)
    _goml_runtime_core_string_println(inline1959)
    var nan__16 float64 = zero64__14 / zero64__14
    var t979 bool = nan__16 == nan__16
    var t980 string
    var inline1957 string = _goml_runtime_core_bool_to_string(t979)
    t980 = inline1957
    var inline1954 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t980)
    _goml_runtime_core_string_println(inline1954)
    var t981 Option__Ordering
    var inline1947 bool = nan__16 < nan__16
    if inline1947 {
        var inline1948 Option__Ordering = Option__Ordering_Some{
            _0: Less,
        }
        t981 = inline1948
    } else {
        var inline1949 bool = nan__16 > nan__16
        if inline1949 {
            var inline1950 Option__Ordering = Option__Ordering_Some{
                _0: Greater,
            }
            t981 = inline1950
        } else {
            var inline1951 bool = nan__16 == nan__16
            if inline1951 {
                var inline1952 Option__Ordering = Option__Ordering_Some{
                    _0: Equal,
                }
                t981 = inline1952
            } else {
                t981 = Option__Ordering_None{}
            }
        }
    }
    var t982 bool
    var inline1944 bool = _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__some____T__Ordering(t981)
    var inline1945 bool = !inline1944
    t982 = inline1945
    var t983 string
    var inline1942 string = _goml_runtime_core_bool_to_string(t982)
    t983 = inline1942
    var inline1939 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t983)
    _goml_runtime_core_string_println(inline1939)
    return struct{}{}
}

func collision_contracts() struct{} {
    var values__17 *hashmap_CollisionKey_int32_x = _goml_m_inherent_i_HashMap_i_H_hda117ae4fc64297aa6806f068e6c08ed_Key____V__int32()
    var t985 CollisionKey = CollisionKey{
        value: 1,
    }
    _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(values__17, t985, 10)
    var t986 CollisionKey = CollisionKey{
        value: 2,
    }
    _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(values__17, t986, 20)
    var t987 CollisionKey = CollisionKey{
        value: 3,
    }
    _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(values__17, t987, 30)
    var t988 CollisionKey = CollisionKey{
        value: 2,
    }
    _goml_m_inherent_i_HashMap_i_H_h28011e82243b31bd5417be204086ffb6_Key____V__int32(values__17, t988)
    var t989 CollisionKey = CollisionKey{
        value: 1,
    }
    var t990 Option__int32 = _goml_m_inherent_i_HashMap_i_H_hd089129fd747b5ac2054f800f07a211e_Key____V__int32(values__17, t989)
    print_opt_int(t990)
    var t991 CollisionKey = CollisionKey{
        value: 2,
    }
    var t992 Option__int32 = _goml_m_inherent_i_HashMap_i_H_hd089129fd747b5ac2054f800f07a211e_Key____V__int32(values__17, t991)
    print_opt_int(t992)
    var t993 CollisionKey = CollisionKey{
        value: 3,
    }
    var t994 Option__int32 = _goml_m_inherent_i_HashMap_i_H_hd089129fd747b5ac2054f800f07a211e_Key____V__int32(values__17, t993)
    print_opt_int(t994)
    var t995 CollisionKey = CollisionKey{
        value: 4,
    }
    _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(values__17, t995, 40)
    var t996 int = _goml_m_inherent_i_HashMap_i_H_h4aea484345d58a42435067138901add4_Key____V__int32(values__17)
    println__T_int(t996)
    var t997 CollisionKey = CollisionKey{
        value: 4,
    }
    var t998 Option__int32
    var inline2024 Option__int32 = hashmap_get__HashMap_12CollisionKey_5int32(values__17, t997)
    t998 = inline2024
    print_opt_int(t998)
    var t999 CollisionKey = CollisionKey{
        value: 4,
    }
    var inline2021 int32 = 41
    hashmap_set__HashMap_12CollisionKey_5int32(values__17, t999, inline2021)
    var t1000 int
    var inline2019 int = hashmap_len__HashMap_12CollisionKey_5int32(values__17)
    t1000 = inline2019
    var inline2016 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t1000)
    _goml_runtime_core_string_println(inline2016)
    var t1001 CollisionKey = CollisionKey{
        value: 4,
    }
    var t1002 Option__int32
    var inline2014 Option__int32 = hashmap_get__HashMap_12CollisionKey_5int32(values__17, t1001)
    t1002 = inline2014
    switch t1002._tag {
    case 0:
        println__T_string("none")
    case 1:
        var inline2010 int32 = t1002._v1_0
        println__T_int32(inline2010)
    default:
        panic("non-exhaustive match")
    }
    var t1003 CollisionKey = CollisionKey{
        value: 99,
    }
    hashmap_remove__HashMap_12CollisionKey_5int32(values__17, t1003)
    var t1004 CollisionKey = CollisionKey{
        value: 99,
    }
    hashmap_remove__HashMap_12CollisionKey_5int32(values__17, t1004)
    var t1005 int
    var inline2003 int = hashmap_len__HashMap_12CollisionKey_5int32(values__17)
    t1005 = inline2003
    var inline2000 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t1005)
    _goml_runtime_core_string_println(inline2000)
    var index__18 *ref_int32_x
    var inline1997 int32 = 0
    var inline1998 *ref_int32_x = ref__Ref_5int32(inline1997)
    index__18 = inline1998
    Loop_loop1008:
    for {
        var t1009 int32
        var inline1990 int32 = ref_get__Ref_5int32(index__18)
        t1009 = inline1990
        var t1010 bool = t1009 < 2000
        if t1010 {
            var t1011 int32
            var inline1988 int32 = ref_get__Ref_5int32(index__18)
            t1011 = inline1988
            var t1012 int32 = 1000 + t1011
            var key__19 CollisionKey = CollisionKey{
                value: t1012,
            }
            var t1013 int32
            var inline1986 int32 = ref_get__Ref_5int32(index__18)
            t1013 = inline1986
            hashmap_set__HashMap_12CollisionKey_5int32(values__17, key__19, t1013)
            hashmap_remove__HashMap_12CollisionKey_5int32(values__17, key__19)
            var t1014 int32
            var inline1980 int32 = ref_get__Ref_5int32(index__18)
            t1014 = inline1980
            var t1015 int32 = t1014 + 1
            ref_set__Ref_5int32(index__18, t1015)
            continue
        } else {
            break Loop_loop1008
        }
    }
    var t1007 int
    var inline1995 int = hashmap_len__HashMap_12CollisionKey_5int32(values__17)
    t1007 = inline1995
    var inline1992 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t1007)
    _goml_runtime_core_string_println(inline1992)
    return struct{}{}
}

func reference_contracts() struct{} {
    var values__20 *hashmap_Ref_12CollisionKey_string_x = _goml_m_inherent_i_HashMap_i_H_h88509d3b0dfc2392ba920998ecb79184_r_____V__string()
    var t1017 CollisionKey = CollisionKey{
        value: 1,
    }
    var key__21 *ref_CollisionKey_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__CollisionKey(t1017)
    var t1018 CollisionKey = CollisionKey{
        value: 1,
    }
    var equal_value__23 *ref_CollisionKey_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__CollisionKey(t1018)
    var inline2066 string = "identity"
    hashmap_set__HashMap_18Ref_12CollisionKey_6string(values__20, key__21, inline2066)
    var t1019 bool
    var inline2064 bool = ptr_eq__Ref_12CollisionKey(key__21, key__21)
    t1019 = inline2064
    var inline2061 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1019)
    _goml_runtime_core_string_println(inline2061)
    var t1020 bool
    var inline2059 bool = ptr_eq__Ref_12CollisionKey(key__21, equal_value__23)
    t1020 = inline2059
    var inline2056 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1020)
    _goml_runtime_core_string_println(inline2056)
    var t1021 uint64
    var inline2054 uint64 = ptr_hash__Ref_12CollisionKey(key__21)
    t1021 = inline2054
    var t1022 uint64
    var inline2052 uint64 = ptr_hash__Ref_12CollisionKey(key__21)
    t1022 = inline2052
    var t1023 bool = t1021 == t1022
    var inline2049 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1023)
    _goml_runtime_core_string_println(inline2049)
    var t1024 Option__string
    var inline2047 Option__string = hashmap_get__HashMap_18Ref_12CollisionKey_6string(values__20, key__21)
    t1024 = inline2047
    switch t1024._tag {
    case 0:
        println__T_string("none")
    case 1:
        var inline2043 string = t1024._v1_0
        println__T_string(inline2043)
    default:
        panic("non-exhaustive match")
    }
    var t1025 Option__string
    var inline2040 Option__string = hashmap_get__HashMap_18Ref_12CollisionKey_6string(values__20, equal_value__23)
    t1025 = inline2040
    switch t1025._tag {
    case 0:
        println__T_string("none")
    case 1:
        var inline2036 string = t1025._v1_0
        println__T_string(inline2036)
    default:
        panic("non-exhaustive match")
    }
    var t1026 CollisionKey = CollisionKey{
        value: 99,
    }
    ref_set__Ref_12CollisionKey(key__21, t1026)
    var t1027 Option__string
    var inline2031 Option__string = hashmap_get__HashMap_18Ref_12CollisionKey_6string(values__20, key__21)
    t1027 = inline2031
    switch t1027._tag {
    case 0:
        println__T_string("none")
        return struct{}{}
    case 1:
        var inline2027 string = t1027._v1_0
        println__T_string(inline2027)
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

func _goml_m_trait__impl_i_PartialEq_i_float64_i_eq(self__205 float64, other__206 float64) bool {
    var t1415 bool = self__205 == other__206
    return t1415
}

func println__T_string(value__1 string) struct{} {
    var t1439 string
    t1439 = value__1
    _goml_runtime_core_string_println(t1439)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t1442 string
    var inline2570 string = _goml_runtime_core_int32_to_string(value__1)
    t1442 = inline2570
    _goml_runtime_core_string_println(t1442)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t1446 string = _goml_runtime_core_bool_to_string(self__148)
    return t1446
}

func _goml_m_inherent_i_HashMap_i_H_hda117ae4fc64297aa6806f068e6c08ed_Key____V__int32() *hashmap_CollisionKey_int32_x {
    var t1453 *hashmap_CollisionKey_int32_x = hashmap_new__HashMap_12CollisionKey_5int32()
    return t1453
}

func _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(self__422 *hashmap_CollisionKey_int32_x, key__423 CollisionKey, value__424 int32) struct{} {
    hashmap_set__HashMap_12CollisionKey_5int32(self__422, key__423, value__424)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h28011e82243b31bd5417be204086ffb6_Key____V__int32(self__425 *hashmap_CollisionKey_int32_x, key__426 CollisionKey) struct{} {
    hashmap_remove__HashMap_12CollisionKey_5int32(self__425, key__426)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_hd089129fd747b5ac2054f800f07a211e_Key____V__int32(self__420 *hashmap_CollisionKey_int32_x, key__421 CollisionKey) Option__int32 {
    var t1460 Option__int32 = hashmap_get__HashMap_12CollisionKey_5int32(self__420, key__421)
    return t1460
}

func println__T_int(value__1 int) struct{} {
    var t1462 string
    var inline2573 string = _goml_runtime_core_int_to_string(value__1)
    t1462 = inline2573
    _goml_runtime_core_string_println(t1462)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h4aea484345d58a42435067138901add4_Key____V__int32(self__427 *hashmap_CollisionKey_int32_x) int {
    var t1466 int = hashmap_len__HashMap_12CollisionKey_5int32(self__427)
    return t1466
}

func _goml_m_inherent_i_HashMap_i_H_h88509d3b0dfc2392ba920998ecb79184_r_____V__string() *hashmap_Ref_12CollisionKey_string_x {
    var t1477 *hashmap_Ref_12CollisionKey_string_x = hashmap_new__HashMap_18Ref_12CollisionKey_6string()
    return t1477
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__CollisionKey(value__431 CollisionKey) *ref_CollisionKey_x {
    var t1480 *ref_CollisionKey_x = ref__Ref_12CollisionKey(value__431)
    return t1480
}

func _goml_m_trait__impl_i_PartialEq_i_Ref_l_CollisionKey_r__i_eq(self__220 *ref_CollisionKey_x, other__221 *ref_CollisionKey_x) bool {
    var t1488 bool = ptr_eq__Ref_12CollisionKey(self__220, other__221)
    return t1488
}

func _goml_m_trait__impl_i_Hash_i_Ref_l_CollisionKey_r__i_hash(self__222 *ref_CollisionKey_x) uint64 {
    var t1491 uint64 = ptr_hash__Ref_12CollisionKey(self__222)
    return t1491
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__154 int32) string {
    var t1501 string = _goml_runtime_core_int32_to_string(self__154)
    return t1501
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__some____T__Ordering(self__456 Option__Ordering) bool {
    switch self__456.(type) {
    case Option__Ordering_None:
        return false
    case Option__Ordering_Some:
        return true
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t1508 string = _goml_runtime_core_int_to_string(self__151)
    return t1508
}

func main() {
    main0()
}
