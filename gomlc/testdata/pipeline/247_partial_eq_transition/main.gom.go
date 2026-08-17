package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_int_hash(x int) uint64 {
    return uint64(x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type hashmap_LegacyKey_string_x_entry struct {
    active bool
    key LegacyKey
    value string
}

type hashmap_LegacyKey_string_x struct {
    buckets map[uint64][]hashmap_LegacyKey_string_x_entry
    hashes []uint64
    len int
}

func hashmap_new__HashMap_9LegacyKey_6string() *hashmap_LegacyKey_string_x {
    return &hashmap_LegacyKey_string_x{
        buckets: make(map[uint64][]hashmap_LegacyKey_string_x_entry),
        len: 0,
        hashes: nil,
    }
}

func hashmap_lookup__HashMap_9LegacyKey_6string(m *hashmap_LegacyKey_string_x, key LegacyKey) (string, bool) {
    if m == nil {
        var zero string
        return zero, false
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_LegacyKey_i_hash(key)
    var bucket []hashmap_LegacyKey_string_x_entry = m.buckets[h]
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_LegacyKey_string_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_PartialEq_i_LegacyKey_i_eq(entry.key, key) {
            return entry.value, true
        }
        i = i + 1
    }
    var zero string
    return zero, false
}

func hashmap_get__HashMap_9LegacyKey_6string(m *hashmap_LegacyKey_string_x, key LegacyKey) Option__string {
    var value string
    var ok bool
    value, ok = hashmap_lookup__HashMap_9LegacyKey_6string(m, key)
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

func hashmap_set__HashMap_9LegacyKey_6string(m *hashmap_LegacyKey_string_x, key LegacyKey, value string) struct{} {
    var reuse_index int = -1
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_LegacyKey_i_hash(key)
    var bucket []hashmap_LegacyKey_string_x_entry = m.buckets[h]
    if len(bucket) == 0 {
        m.hashes = append(m.hashes, h)
    }
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_LegacyKey_string_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_PartialEq_i_LegacyKey_i_eq(entry.key, key) {
            bucket[i].value = value
            return struct{}{}
        }
        if !entry.active && reuse_index < 0 {
            reuse_index = i
        }
        i = i + 1
    }
    if reuse_index >= 0 {
        bucket[reuse_index] = hashmap_LegacyKey_string_x_entry{
            active: true,
            key: key,
            value: value,
        }
        m.len = m.len + 1
        return struct{}{}
    }
    bucket = append(bucket, hashmap_LegacyKey_string_x_entry{
        active: true,
        key: key,
        value: value,
    })
    m.buckets[h] = bucket
    m.len = m.len + 1
    return struct{}{}
}

type hashmap_ModernKey_string_x_entry struct {
    active bool
    key ModernKey
    value string
}

type hashmap_ModernKey_string_x struct {
    buckets map[uint64][]hashmap_ModernKey_string_x_entry
    hashes []uint64
    len int
}

func hashmap_new__HashMap_9ModernKey_6string() *hashmap_ModernKey_string_x {
    return &hashmap_ModernKey_string_x{
        buckets: make(map[uint64][]hashmap_ModernKey_string_x_entry),
        len: 0,
        hashes: nil,
    }
}

func hashmap_lookup__HashMap_9ModernKey_6string(m *hashmap_ModernKey_string_x, key ModernKey) (string, bool) {
    if m == nil {
        var zero string
        return zero, false
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_ModernKey_i_hash(key)
    var bucket []hashmap_ModernKey_string_x_entry = m.buckets[h]
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_ModernKey_string_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_PartialEq_i_ModernKey_i_eq(entry.key, key) {
            return entry.value, true
        }
        i = i + 1
    }
    var zero string
    return zero, false
}

func hashmap_get__HashMap_9ModernKey_6string(m *hashmap_ModernKey_string_x, key ModernKey) Option__string {
    var value string
    var ok bool
    value, ok = hashmap_lookup__HashMap_9ModernKey_6string(m, key)
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

func hashmap_set__HashMap_9ModernKey_6string(m *hashmap_ModernKey_string_x, key ModernKey, value string) struct{} {
    var reuse_index int = -1
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_ModernKey_i_hash(key)
    var bucket []hashmap_ModernKey_string_x_entry = m.buckets[h]
    if len(bucket) == 0 {
        m.hashes = append(m.hashes, h)
    }
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_ModernKey_string_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_PartialEq_i_ModernKey_i_eq(entry.key, key) {
            bucket[i].value = value
            return struct{}{}
        }
        if !entry.active && reuse_index < 0 {
            reuse_index = i
        }
        i = i + 1
    }
    if reuse_index >= 0 {
        bucket[reuse_index] = hashmap_ModernKey_string_x_entry{
            active: true,
            key: key,
            value: value,
        }
        m.len = m.len + 1
        return struct{}{}
    }
    bucket = append(bucket, hashmap_ModernKey_string_x_entry{
        active: true,
        key: key,
        value: value,
    })
    m.buckets[h] = bucket
    m.len = m.len + 1
    return struct{}{}
}

type LegacyKey struct {
    value int
}

type ModernKey struct {
    value int
}

type Rank struct {
    value int
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

func _goml_m_trait__impl_i_PartialEq_i_LegacyKey_i_eq(self__0 LegacyKey, other__1 LegacyKey) bool {
    var t900 int = self__0.value
    var t901 int = other__1.value
    var inline1807 bool = t900 == t901
    return inline1807
}

func _goml_m_trait__impl_i_Hash_i_LegacyKey_i_hash(self__2 LegacyKey) uint64 {
    var t905_source int = 0
    var t905 uint64 = uint64(int(t905_source))
    var h__3 uint64 = t905 + 14695981039346656037
    var t906_source int = 0
    var t906 uint64 = uint64(int(t906_source))
    var t907 uint64 = t906 + 1099511628211
    var t908 uint64 = h__3 * t907
    var t909 int = self__2.value
    var t910 uint64
    var inline1809 uint64 = _goml_runtime_core_int_hash(t909)
    t910 = inline1809
    var h__4 uint64 = t908 + t910
    return h__4
}

func _goml_m_trait__impl_i_PartialEq_i_ModernKey_i_eq(self__5 ModernKey, other__6 ModernKey) bool {
    var t913 int = self__5.value
    var t914_rhs int = 10
    var t914 int = t913 % t914_rhs
    var t915 int = other__6.value
    var t916_rhs int = 10
    var t916 int = t915 % t916_rhs
    var t917 bool = t914 == t916
    return t917
}

func _goml_m_trait__impl_i_Hash_i_ModernKey_i_hash(self__7 ModernKey) uint64 {
    var t920 int = self__7.value
    var t921_rhs int = 10
    var t921 int = t920 % t921_rhs
    var t922 uint64 = uint64(int(t921))
    return t922
}

func main0() struct{} {
    var legacy__14 LegacyKey = LegacyKey{
        value: 7,
    }
    var modern__15 ModernKey = ModernKey{
        value: 9,
    }
    var t954 LegacyKey = LegacyKey{
        value: 7,
    }
    var t955 bool = equal__T_LegacyKey(legacy__14, t954)
    println__T_bool(t955)
    var t956 LegacyKey = LegacyKey{
        value: 8,
    }
    var t957 bool = _goml_m_trait__impl_i_PartialEq_i_LegacyKey_i_eq(legacy__14, t956)
    println__T_bool(t957)
    var t958 ModernKey = ModernKey{
        value: 19,
    }
    var t959 bool = equal__T_ModernKey(modern__15, t958)
    println__T_bool(t959)
    var t960 ModernKey = ModernKey{
        value: 20,
    }
    var t961 bool = _goml_m_trait__impl_i_PartialEq_i_ModernKey_i_eq(modern__15, t960)
    var t962 bool = !t961
    var t963 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t962)
    println__T_string(t963)
    var legacy_map__16 *hashmap_LegacyKey_string_x = _goml_m_inherent_i_HashMap_i_H_h4f35fad7fd3ed72455715cdf3969637d_ey____V__string()
    _goml_m_inherent_i_HashMap_i_H_h4c415936d3e2c958d5274434037d6231_ey____V__string(legacy_map__16, legacy__14, "legacy")
    var t964 LegacyKey = LegacyKey{
        value: 7,
    }
    var t965 Option__string
    var inline1878 Option__string = hashmap_get__HashMap_9LegacyKey_6string(legacy_map__16, t964)
    t965 = inline1878
    var t966 string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(t965, "missing")
    var inline1875 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t966)
    _goml_runtime_core_string_println(inline1875)
    var modern_map__17 *hashmap_ModernKey_string_x
    var inline1873 *hashmap_ModernKey_string_x = hashmap_new__HashMap_9ModernKey_6string()
    modern_map__17 = inline1873
    var inline1870 string = "modern"
    hashmap_set__HashMap_9ModernKey_6string(modern_map__17, modern__15, inline1870)
    var t967 ModernKey = ModernKey{
        value: 19,
    }
    var t968 Option__string
    var inline1868 Option__string = hashmap_get__HashMap_9ModernKey_6string(modern_map__17, t967)
    t968 = inline1868
    var t969 string
    var inline1864 string = "missing"
    switch t968._tag {
    case 0:
        t969 = inline1864
    case 1:
        var inline1865 string = t968._v1_0
        t969 = inline1865
    default:
        panic("non-exhaustive match")
    }
    var inline1861 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t969)
    _goml_runtime_core_string_println(inline1861)
    var high__18 Rank = Rank{
        value: 2,
    }
    var low__19 Rank = Rank{
        value: 1,
    }
    var t970 bool
    var inline1859 bool = _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_lt____Self__Rank(high__18, low__19)
    t970 = inline1859
    var t971 string
    var inline1857 string = _goml_runtime_core_bool_to_string(t970)
    t971 = inline1857
    var inline1854 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t971)
    _goml_runtime_core_string_println(inline1854)
    var t972 bool
    var inline1852 bool = _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_le____Self__Rank(high__18, high__18)
    t972 = inline1852
    var t973 string
    var inline1850 string = _goml_runtime_core_bool_to_string(t972)
    t973 = inline1850
    var inline1847 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t973)
    _goml_runtime_core_string_println(inline1847)
    var t974 bool
    var inline1845 bool = _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_gt____Self__Rank(low__19, high__18)
    t974 = inline1845
    var t975 string
    var inline1843 string = _goml_runtime_core_bool_to_string(t974)
    t975 = inline1843
    var inline1840 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t975)
    _goml_runtime_core_string_println(inline1840)
    var t976 bool
    var inline1838 bool = _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_ge____Self__Rank(low__19, low__19)
    t976 = inline1838
    var t977 string
    var inline1836 string = _goml_runtime_core_bool_to_string(t976)
    t977 = inline1836
    var inline1833 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t977)
    _goml_runtime_core_string_println(inline1833)
    return struct{}{}
}

func _goml_m_trait__impl_i_PartialEq_i_int_i_eq(self__185 int, other__186 int) bool {
    var t1365 bool = self__185 == other__186
    return t1365
}

func _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_lt____Self__Rank(self__0 Rank, other__1 Rank) bool {
    var commute_field2605 Ordering
    var inline2345 int = self__0.value
    var inline2346 int = other__1.value
    var inline2347 bool = inline2345 > inline2346
    var inline2349 Ordering
    if inline2347 {
        inline2349 = Less
    } else {
        var inline2351 int = self__0.value
        var inline2352 int = other__1.value
        var inline2353 bool = inline2351 < inline2352
        if inline2353 {
            inline2349 = Greater
        } else {
            inline2349 = Equal
        }
    }
    commute_field2605 = inline2349
    switch commute_field2605 {
    case Less:
        return true
    case Equal:
        return false
    case Greater:
        return false
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_le____Self__Rank(self__3 Rank, other__4 Rank) bool {
    var commute_field2608 Ordering
    var inline2356 int = self__3.value
    var inline2357 int = other__4.value
    var inline2358 bool = inline2356 > inline2357
    var inline2360 Ordering
    if inline2358 {
        inline2360 = Less
    } else {
        var inline2362 int = self__3.value
        var inline2363 int = other__4.value
        var inline2364 bool = inline2362 < inline2363
        if inline2364 {
            inline2360 = Greater
        } else {
            inline2360 = Equal
        }
    }
    commute_field2608 = inline2360
    var t1378 bool
    switch commute_field2608 {
    case Less:
        t1378 = false
    case Equal:
        t1378 = false
    case Greater:
        t1378 = true
    default:
        panic("non-exhaustive match")
    }
    var t1379 bool = !t1378
    return t1379
}

func _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_gt____Self__Rank(self__6 Rank, other__7 Rank) bool {
    var commute_field2611 Ordering
    var inline2367 int = self__6.value
    var inline2368 int = other__7.value
    var inline2369 bool = inline2367 > inline2368
    var inline2371 Ordering
    if inline2369 {
        inline2371 = Less
    } else {
        var inline2373 int = self__6.value
        var inline2374 int = other__7.value
        var inline2375 bool = inline2373 < inline2374
        if inline2375 {
            inline2371 = Greater
        } else {
            inline2371 = Equal
        }
    }
    commute_field2611 = inline2371
    switch commute_field2611 {
    case Less:
        return false
    case Equal:
        return false
    case Greater:
        return true
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_ge____Self__Rank(self__9 Rank, other__10 Rank) bool {
    var commute_field2614 Ordering
    var inline2378 int = self__9.value
    var inline2379 int = other__10.value
    var inline2380 bool = inline2378 > inline2379
    var inline2382 Ordering
    if inline2380 {
        inline2382 = Less
    } else {
        var inline2384 int = self__9.value
        var inline2385 int = other__10.value
        var inline2386 bool = inline2384 < inline2385
        if inline2386 {
            inline2382 = Greater
        } else {
            inline2382 = Equal
        }
    }
    commute_field2614 = inline2382
    var t1389 bool
    switch commute_field2614 {
    case Less:
        t1389 = true
    case Equal:
        t1389 = false
    case Greater:
        t1389 = false
    default:
        panic("non-exhaustive match")
    }
    var t1390 bool = !t1389
    return t1390
}

func println__T_bool(value__1 bool) struct{} {
    var t1392 string
    var inline2388 string = _goml_runtime_core_bool_to_string(value__1)
    t1392 = inline2388
    _goml_runtime_core_string_println(t1392)
    return struct{}{}
}

func equal__T_LegacyKey(left__12 LegacyKey, right__13 LegacyKey) bool {
    var inline2390 int = left__12.value
    var inline2391 int = right__13.value
    var inline2392 bool = _goml_m_trait__impl_i_PartialEq_i_int_i_eq(inline2390, inline2391)
    return inline2392
}

func equal__T_ModernKey(left__12 ModernKey, right__13 ModernKey) bool {
    var inline2394 int = left__12.value
    var inline2395_rhs int = 10
    var inline2395 int = inline2394 % inline2395_rhs
    var inline2396 int = right__13.value
    var inline2397_rhs int = 10
    var inline2397 int = inline2396 % inline2397_rhs
    var inline2398 bool = inline2395 == inline2397
    return inline2398
}

func println__T_string(value__1 string) struct{} {
    var t1401 string
    t1401 = value__1
    _goml_runtime_core_string_println(t1401)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t1405 string = _goml_runtime_core_bool_to_string(self__148)
    return t1405
}

func _goml_m_inherent_i_HashMap_i_H_h4f35fad7fd3ed72455715cdf3969637d_ey____V__string() *hashmap_LegacyKey_string_x {
    var t1408 *hashmap_LegacyKey_string_x = hashmap_new__HashMap_9LegacyKey_6string()
    return t1408
}

func _goml_m_inherent_i_HashMap_i_H_h4c415936d3e2c958d5274434037d6231_ey____V__string(self__422 *hashmap_LegacyKey_string_x, key__423 LegacyKey, value__424 string) struct{} {
    hashmap_set__HashMap_9LegacyKey_6string(self__422, key__423, value__424)
    return struct{}{}
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(self__458 Option__string, fallback__459 string) string {
    switch self__458._tag {
    case 0:
        return fallback__459
    case 1:
        var x387 string = self__458._v1_0
        return x387
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
