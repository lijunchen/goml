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

type Option__Ordering struct {
    _tag int32
    _v1_0 Ordering
}

type Option__string struct {
    _tag int32
    _v1_0 string
}

func _goml_m_trait__impl_i_PartialEq_i_LegacyKey_i_eq(self__0 LegacyKey, other__1 LegacyKey) bool {
    var t903 int = self__0.value
    var t904 int = other__1.value
    var inline1810 bool = t903 == t904
    return inline1810
}

func _goml_m_trait__impl_i_Hash_i_LegacyKey_i_hash(self__2 LegacyKey) uint64 {
    var t908_source int = 0
    var t908 uint64 = uint64(int(t908_source))
    var h__3 uint64 = t908 + 14695981039346656037
    var t909_source int = 0
    var t909 uint64 = uint64(int(t909_source))
    var t910 uint64 = t909 + 1099511628211
    var t911 uint64 = h__3 * t910
    var t912 int = self__2.value
    var t913 uint64
    var inline1812 uint64 = _goml_runtime_core_int_hash(t912)
    t913 = inline1812
    var h__4 uint64 = t911 + t913
    return h__4
}

func _goml_m_trait__impl_i_PartialEq_i_ModernKey_i_eq(self__5 ModernKey, other__6 ModernKey) bool {
    var t916 int = self__5.value
    var t917_rhs int = 10
    var t917 int = t916 % t917_rhs
    var t918 int = other__6.value
    var t919_rhs int = 10
    var t919 int = t918 % t919_rhs
    var t920 bool = t917 == t919
    return t920
}

func _goml_m_trait__impl_i_Hash_i_ModernKey_i_hash(self__7 ModernKey) uint64 {
    var t923 int = self__7.value
    var t924_rhs int = 10
    var t924 int = t923 % t924_rhs
    var t925 uint64 = uint64(int(t924))
    return t925
}

func main0() struct{} {
    var legacy__14 LegacyKey = LegacyKey{
        value: 7,
    }
    var modern__15 ModernKey = ModernKey{
        value: 9,
    }
    var t957 LegacyKey = LegacyKey{
        value: 7,
    }
    var t958 bool = equal__T_LegacyKey(legacy__14, t957)
    println__T_bool(t958)
    var t959 LegacyKey = LegacyKey{
        value: 8,
    }
    var t960 bool = _goml_m_trait__impl_i_PartialEq_i_LegacyKey_i_eq(legacy__14, t959)
    println__T_bool(t960)
    var t961 ModernKey = ModernKey{
        value: 19,
    }
    var t962 bool = equal__T_ModernKey(modern__15, t961)
    println__T_bool(t962)
    var t963 ModernKey = ModernKey{
        value: 20,
    }
    var t964 bool = _goml_m_trait__impl_i_PartialEq_i_ModernKey_i_eq(modern__15, t963)
    var t965 bool = !t964
    var t966 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t965)
    println__T_string(t966)
    var legacy_map__16 *hashmap_LegacyKey_string_x = _goml_m_inherent_i_HashMap_i_H_h4f35fad7fd3ed72455715cdf3969637d_ey____V__string()
    _goml_m_inherent_i_HashMap_i_H_h4c415936d3e2c958d5274434037d6231_ey____V__string(legacy_map__16, legacy__14, "legacy")
    var t967 LegacyKey = LegacyKey{
        value: 7,
    }
    var t968 Option__string
    var inline1881 Option__string = hashmap_get__HashMap_9LegacyKey_6string(legacy_map__16, t967)
    t968 = inline1881
    var t969 string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(t968, "missing")
    var inline1878 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t969)
    _goml_runtime_core_string_println(inline1878)
    var modern_map__17 *hashmap_ModernKey_string_x
    var inline1876 *hashmap_ModernKey_string_x = hashmap_new__HashMap_9ModernKey_6string()
    modern_map__17 = inline1876
    var inline1873 string = "modern"
    hashmap_set__HashMap_9ModernKey_6string(modern_map__17, modern__15, inline1873)
    var t970 ModernKey = ModernKey{
        value: 19,
    }
    var t971 Option__string
    var inline1871 Option__string = hashmap_get__HashMap_9ModernKey_6string(modern_map__17, t970)
    t971 = inline1871
    var t972 string
    var inline1867 string = "missing"
    switch t971._tag {
    case 0:
        t972 = inline1867
    case 1:
        var inline1868 string = t971._v1_0
        t972 = inline1868
    default:
        panic("non-exhaustive match")
    }
    var inline1864 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t972)
    _goml_runtime_core_string_println(inline1864)
    var high__18 Rank = Rank{
        value: 2,
    }
    var low__19 Rank = Rank{
        value: 1,
    }
    var t973 bool
    var inline1862 bool = _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_lt____Self__Rank(high__18, low__19)
    t973 = inline1862
    var t974 string
    var inline1860 string = _goml_runtime_core_bool_to_string(t973)
    t974 = inline1860
    var inline1857 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t974)
    _goml_runtime_core_string_println(inline1857)
    var t975 bool
    var inline1855 bool = _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_le____Self__Rank(high__18, high__18)
    t975 = inline1855
    var t976 string
    var inline1853 string = _goml_runtime_core_bool_to_string(t975)
    t976 = inline1853
    var inline1850 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t976)
    _goml_runtime_core_string_println(inline1850)
    var t977 bool
    var inline1848 bool = _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_gt____Self__Rank(low__19, high__18)
    t977 = inline1848
    var t978 string
    var inline1846 string = _goml_runtime_core_bool_to_string(t977)
    t978 = inline1846
    var inline1843 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t978)
    _goml_runtime_core_string_println(inline1843)
    var t979 bool
    var inline1841 bool = _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_ge____Self__Rank(low__19, low__19)
    t979 = inline1841
    var t980 string
    var inline1839 string = _goml_runtime_core_bool_to_string(t979)
    t980 = inline1839
    var inline1836 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t980)
    _goml_runtime_core_string_println(inline1836)
    return struct{}{}
}

func _goml_m_trait__impl_i_PartialEq_i_isize_i_eq(self__185 int, other__186 int) bool {
    var t1368 bool = self__185 == other__186
    return t1368
}

func _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_lt____Self__Rank(self__0 Rank, other__1 Rank) bool {
    var commute_field2608 Ordering
    var inline2348 int = self__0.value
    var inline2349 int = other__1.value
    var inline2350 bool = inline2348 > inline2349
    var inline2352 Ordering
    if inline2350 {
        inline2352 = Less
    } else {
        var inline2354 int = self__0.value
        var inline2355 int = other__1.value
        var inline2356 bool = inline2354 < inline2355
        if inline2356 {
            inline2352 = Greater
        } else {
            inline2352 = Equal
        }
    }
    commute_field2608 = inline2352
    switch commute_field2608 {
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
    var commute_field2611 Ordering
    var inline2359 int = self__3.value
    var inline2360 int = other__4.value
    var inline2361 bool = inline2359 > inline2360
    var inline2363 Ordering
    if inline2361 {
        inline2363 = Less
    } else {
        var inline2365 int = self__3.value
        var inline2366 int = other__4.value
        var inline2367 bool = inline2365 < inline2366
        if inline2367 {
            inline2363 = Greater
        } else {
            inline2363 = Equal
        }
    }
    commute_field2611 = inline2363
    var t1381 bool
    switch commute_field2611 {
    case Less:
        t1381 = false
    case Equal:
        t1381 = false
    case Greater:
        t1381 = true
    default:
        panic("non-exhaustive match")
    }
    var t1382 bool = !t1381
    return t1382
}

func _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_gt____Self__Rank(self__6 Rank, other__7 Rank) bool {
    var commute_field2614 Ordering
    var inline2370 int = self__6.value
    var inline2371 int = other__7.value
    var inline2372 bool = inline2370 > inline2371
    var inline2374 Ordering
    if inline2372 {
        inline2374 = Less
    } else {
        var inline2376 int = self__6.value
        var inline2377 int = other__7.value
        var inline2378 bool = inline2376 < inline2377
        if inline2378 {
            inline2374 = Greater
        } else {
            inline2374 = Equal
        }
    }
    commute_field2614 = inline2374
    switch commute_field2614 {
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
    var commute_field2617 Ordering
    var inline2381 int = self__9.value
    var inline2382 int = other__10.value
    var inline2383 bool = inline2381 > inline2382
    var inline2385 Ordering
    if inline2383 {
        inline2385 = Less
    } else {
        var inline2387 int = self__9.value
        var inline2388 int = other__10.value
        var inline2389 bool = inline2387 < inline2388
        if inline2389 {
            inline2385 = Greater
        } else {
            inline2385 = Equal
        }
    }
    commute_field2617 = inline2385
    var t1392 bool
    switch commute_field2617 {
    case Less:
        t1392 = true
    case Equal:
        t1392 = false
    case Greater:
        t1392 = false
    default:
        panic("non-exhaustive match")
    }
    var t1393 bool = !t1392
    return t1393
}

func println__T_bool(value__1 bool) struct{} {
    var t1395 string
    var inline2391 string = _goml_runtime_core_bool_to_string(value__1)
    t1395 = inline2391
    _goml_runtime_core_string_println(t1395)
    return struct{}{}
}

func equal__T_LegacyKey(left__12 LegacyKey, right__13 LegacyKey) bool {
    var inline2393 int = left__12.value
    var inline2394 int = right__13.value
    var inline2395 bool = _goml_m_trait__impl_i_PartialEq_i_isize_i_eq(inline2393, inline2394)
    return inline2395
}

func equal__T_ModernKey(left__12 ModernKey, right__13 ModernKey) bool {
    var inline2397 int = left__12.value
    var inline2398_rhs int = 10
    var inline2398 int = inline2397 % inline2398_rhs
    var inline2399 int = right__13.value
    var inline2400_rhs int = 10
    var inline2400 int = inline2399 % inline2400_rhs
    var inline2401 bool = inline2398 == inline2400
    return inline2401
}

func println__T_string(value__1 string) struct{} {
    var t1404 string
    t1404 = value__1
    _goml_runtime_core_string_println(t1404)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t1408 string = _goml_runtime_core_bool_to_string(self__148)
    return t1408
}

func _goml_m_inherent_i_HashMap_i_H_h4f35fad7fd3ed72455715cdf3969637d_ey____V__string() *hashmap_LegacyKey_string_x {
    var t1411 *hashmap_LegacyKey_string_x = hashmap_new__HashMap_9LegacyKey_6string()
    return t1411
}

func _goml_m_inherent_i_HashMap_i_H_h4c415936d3e2c958d5274434037d6231_ey____V__string(self__422 *hashmap_LegacyKey_string_x, key__423 LegacyKey, value__424 string) struct{} {
    hashmap_set__HashMap_9LegacyKey_6string(self__422, key__423, value__424)
    return struct{}{}
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(self__467 Option__string, fallback__468 string) string {
    switch self__467._tag {
    case 0:
        return fallback__468
    case 1:
        var x390 string = self__467._v1_0
        return x390
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
