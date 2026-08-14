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
        return Option__string_Some{
            _0: value,
        }
    }
    return Option__string_None{}
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
        return Option__string_Some{
            _0: value,
        }
    }
    return Option__string_None{}
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

type _goml_m_std_p_cmp_p_Ordering int32

const (
    Less _goml_m_std_p_cmp_p_Ordering = 0
    Equal _goml_m_std_p_cmp_p_Ordering = 1
    Greater _goml_m_std_p_cmp_p_Ordering = 2
)

type _goml_m_Option____std_p_cmp_p_Ordering interface {
    is_goml_m_Option____std_p_cmp_p_Ordering()
}

type _goml_m_Option____std_p_cmp_p_Ordering_None struct {}

func (_ _goml_m_Option____std_p_cmp_p_Ordering_None) is_goml_m_Option____std_p_cmp_p_Ordering() {}

type _goml_m_Option____std_p_cmp_p_Ordering_Some struct {
    _0 _goml_m_std_p_cmp_p_Ordering
}

func (_ _goml_m_Option____std_p_cmp_p_Ordering_Some) is_goml_m_Option____std_p_cmp_p_Ordering() {}

type Option__string interface {
    isOption__string()
}

type Option__string_None struct {}

func (_ Option__string_None) isOption__string() {}

type Option__string_Some struct {
    _0 string
}

func (_ Option__string_Some) isOption__string() {}

func _goml_m_trait__impl_i_PartialEq_i_LegacyKey_i_eq(self__0 LegacyKey, other__1 LegacyKey) bool {
    var t732 int = self__0.value
    var t733 int = other__1.value
    var inline1637 bool = t732 == t733
    return inline1637
}

func _goml_m_trait__impl_i_Hash_i_LegacyKey_i_hash(self__2 LegacyKey) uint64 {
    var t737_source int = 0
    var t737 uint64 = uint64(int(t737_source))
    var h__3 uint64 = t737 + 14695981039346656037
    var t738_source int = 0
    var t738 uint64 = uint64(int(t738_source))
    var t739 uint64 = t738 + 1099511628211
    var t740 uint64 = h__3 * t739
    var t741 int = self__2.value
    var t742 uint64
    var inline1639 uint64 = _goml_runtime_core_int_hash(t741)
    t742 = inline1639
    var h__4 uint64 = t740 + t742
    return h__4
}

func _goml_m_trait__impl_i_PartialEq_i_ModernKey_i_eq(self__5 ModernKey, other__6 ModernKey) bool {
    var t745 int = self__5.value
    var t746_rhs int = 10
    var t746 int = t745 % t746_rhs
    var t747 int = other__6.value
    var t748_rhs int = 10
    var t748 int = t747 % t748_rhs
    var t749 bool = t746 == t748
    return t749
}

func _goml_m_trait__impl_i_Hash_i_ModernKey_i_hash(self__7 ModernKey) uint64 {
    var t752 int = self__7.value
    var t753_rhs int = 10
    var t753 int = t752 % t753_rhs
    var t754 uint64 = uint64(int(t753))
    return t754
}

func main0() struct{} {
    var legacy__14 LegacyKey = LegacyKey{
        value: 7,
    }
    var modern__15 ModernKey = ModernKey{
        value: 9,
    }
    var t786 LegacyKey = LegacyKey{
        value: 7,
    }
    var t787 bool = equal__T_LegacyKey(legacy__14, t786)
    println__T_bool(t787)
    var t788 LegacyKey = LegacyKey{
        value: 8,
    }
    var t789 bool = _goml_m_trait__impl_i_PartialEq_i_LegacyKey_i_eq(legacy__14, t788)
    println__T_bool(t789)
    var t790 ModernKey = ModernKey{
        value: 19,
    }
    var t791 bool = equal__T_ModernKey(modern__15, t790)
    println__T_bool(t791)
    var t792 ModernKey = ModernKey{
        value: 20,
    }
    var t793 bool = _goml_m_trait__impl_i_PartialEq_i_ModernKey_i_eq(modern__15, t792)
    var t794 bool = !t793
    var t795 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t794)
    println__T_string(t795)
    var legacy_map__16 *hashmap_LegacyKey_string_x = _goml_m_inherent_i_HashMap_i_H_h4f35fad7fd3ed72455715cdf3969637d_ey____V__string()
    _goml_m_inherent_i_HashMap_i_H_h4c415936d3e2c958d5274434037d6231_ey____V__string(legacy_map__16, legacy__14, "legacy")
    var t796 LegacyKey = LegacyKey{
        value: 7,
    }
    var t797 Option__string
    var inline1708 Option__string = hashmap_get__HashMap_9LegacyKey_6string(legacy_map__16, t796)
    t797 = inline1708
    var t798 string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(t797, "missing")
    var inline1705 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t798)
    _goml_runtime_core_string_println(inline1705)
    var modern_map__17 *hashmap_ModernKey_string_x
    var inline1703 *hashmap_ModernKey_string_x = hashmap_new__HashMap_9ModernKey_6string()
    modern_map__17 = inline1703
    var inline1700 string = "modern"
    hashmap_set__HashMap_9ModernKey_6string(modern_map__17, modern__15, inline1700)
    var t799 ModernKey = ModernKey{
        value: 19,
    }
    var t800 Option__string
    var inline1698 Option__string = hashmap_get__HashMap_9ModernKey_6string(modern_map__17, t799)
    t800 = inline1698
    var t801 string
    var inline1694 string = "missing"
    switch t800.(type) {
    case Option__string_None:
        t801 = inline1694
    case Option__string_Some:
        var inline1695 string = t800.(Option__string_Some)._0
        t801 = inline1695
    default:
        panic("non-exhaustive match")
    }
    var inline1691 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t801)
    _goml_runtime_core_string_println(inline1691)
    var high__18 Rank = Rank{
        value: 2,
    }
    var low__19 Rank = Rank{
        value: 1,
    }
    var t802 bool
    var inline1689 bool = _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_lt____Self__Rank(high__18, low__19)
    t802 = inline1689
    var t803 string
    var inline1687 string = _goml_runtime_core_bool_to_string(t802)
    t803 = inline1687
    var inline1684 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t803)
    _goml_runtime_core_string_println(inline1684)
    var t804 bool
    var inline1682 bool = _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_le____Self__Rank(high__18, high__18)
    t804 = inline1682
    var t805 string
    var inline1680 string = _goml_runtime_core_bool_to_string(t804)
    t805 = inline1680
    var inline1677 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t805)
    _goml_runtime_core_string_println(inline1677)
    var t806 bool
    var inline1675 bool = _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_gt____Self__Rank(low__19, high__18)
    t806 = inline1675
    var t807 string
    var inline1673 string = _goml_runtime_core_bool_to_string(t806)
    t807 = inline1673
    var inline1670 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t807)
    _goml_runtime_core_string_println(inline1670)
    var t808 bool
    var inline1668 bool = _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_ge____Self__Rank(low__19, low__19)
    t808 = inline1668
    var t809 string
    var inline1666 string = _goml_runtime_core_bool_to_string(t808)
    t809 = inline1666
    var inline1663 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t809)
    _goml_runtime_core_string_println(inline1663)
    return struct{}{}
}

func _goml_m_trait__impl_i_PartialEq_i_int_i_eq(self__101 int, other__102 int) bool {
    var t1187 bool = self__101 == other__102
    return t1187
}

func _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_lt____Self__Rank(self__15 Rank, other__16 Rank) bool {
    var commute_field2435 _goml_m_std_p_cmp_p_Ordering
    var inline2175 int = self__15.value
    var inline2176 int = other__16.value
    var inline2177 bool = inline2175 > inline2176
    var inline2179 _goml_m_std_p_cmp_p_Ordering
    if inline2177 {
        inline2179 = Less
    } else {
        var inline2181 int = self__15.value
        var inline2182 int = other__16.value
        var inline2183 bool = inline2181 < inline2182
        if inline2183 {
            inline2179 = Greater
        } else {
            inline2179 = Equal
        }
    }
    commute_field2435 = inline2179
    switch commute_field2435 {
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

func _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_le____Self__Rank(self__18 Rank, other__19 Rank) bool {
    var commute_field2438 _goml_m_std_p_cmp_p_Ordering
    var inline2186 int = self__18.value
    var inline2187 int = other__19.value
    var inline2188 bool = inline2186 > inline2187
    var inline2190 _goml_m_std_p_cmp_p_Ordering
    if inline2188 {
        inline2190 = Less
    } else {
        var inline2192 int = self__18.value
        var inline2193 int = other__19.value
        var inline2194 bool = inline2192 < inline2193
        if inline2194 {
            inline2190 = Greater
        } else {
            inline2190 = Equal
        }
    }
    commute_field2438 = inline2190
    var t1200 bool
    switch commute_field2438 {
    case Less:
        t1200 = false
    case Equal:
        t1200 = false
    case Greater:
        t1200 = true
    default:
        panic("non-exhaustive match")
    }
    var t1201 bool = !t1200
    return t1201
}

func _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_gt____Self__Rank(self__21 Rank, other__22 Rank) bool {
    var commute_field2441 _goml_m_std_p_cmp_p_Ordering
    var inline2197 int = self__21.value
    var inline2198 int = other__22.value
    var inline2199 bool = inline2197 > inline2198
    var inline2201 _goml_m_std_p_cmp_p_Ordering
    if inline2199 {
        inline2201 = Less
    } else {
        var inline2203 int = self__21.value
        var inline2204 int = other__22.value
        var inline2205 bool = inline2203 < inline2204
        if inline2205 {
            inline2201 = Greater
        } else {
            inline2201 = Equal
        }
    }
    commute_field2441 = inline2201
    switch commute_field2441 {
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

func _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_ge____Self__Rank(self__24 Rank, other__25 Rank) bool {
    var commute_field2444 _goml_m_std_p_cmp_p_Ordering
    var inline2208 int = self__24.value
    var inline2209 int = other__25.value
    var inline2210 bool = inline2208 > inline2209
    var inline2212 _goml_m_std_p_cmp_p_Ordering
    if inline2210 {
        inline2212 = Less
    } else {
        var inline2214 int = self__24.value
        var inline2215 int = other__25.value
        var inline2216 bool = inline2214 < inline2215
        if inline2216 {
            inline2212 = Greater
        } else {
            inline2212 = Equal
        }
    }
    commute_field2444 = inline2212
    var t1211 bool
    switch commute_field2444 {
    case Less:
        t1211 = true
    case Equal:
        t1211 = false
    case Greater:
        t1211 = false
    default:
        panic("non-exhaustive match")
    }
    var t1212 bool = !t1211
    return t1212
}

func println__T_bool(value__1 bool) struct{} {
    var t1214 string
    var inline2218 string = _goml_runtime_core_bool_to_string(value__1)
    t1214 = inline2218
    _goml_runtime_core_string_println(t1214)
    return struct{}{}
}

func equal__T_LegacyKey(left__12 LegacyKey, right__13 LegacyKey) bool {
    var inline2220 int = left__12.value
    var inline2221 int = right__13.value
    var inline2222 bool = _goml_m_trait__impl_i_PartialEq_i_int_i_eq(inline2220, inline2221)
    return inline2222
}

func equal__T_ModernKey(left__12 ModernKey, right__13 ModernKey) bool {
    var inline2224 int = left__12.value
    var inline2225_rhs int = 10
    var inline2225 int = inline2224 % inline2225_rhs
    var inline2226 int = right__13.value
    var inline2227_rhs int = 10
    var inline2227 int = inline2226 % inline2227_rhs
    var inline2228 bool = inline2225 == inline2227
    return inline2228
}

func println__T_string(value__1 string) struct{} {
    var t1223 string
    t1223 = value__1
    _goml_runtime_core_string_println(t1223)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t1227 string = _goml_runtime_core_bool_to_string(self__64)
    return t1227
}

func _goml_m_inherent_i_HashMap_i_H_h4f35fad7fd3ed72455715cdf3969637d_ey____V__string() *hashmap_LegacyKey_string_x {
    var t1230 *hashmap_LegacyKey_string_x = hashmap_new__HashMap_9LegacyKey_6string()
    return t1230
}

func _goml_m_inherent_i_HashMap_i_H_h4c415936d3e2c958d5274434037d6231_ey____V__string(self__261 *hashmap_LegacyKey_string_x, key__262 LegacyKey, value__263 string) struct{} {
    hashmap_set__HashMap_9LegacyKey_6string(self__261, key__262, value__263)
    return struct{}{}
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(self__297 Option__string, fallback__298 string) string {
    switch self__297.(type) {
    case Option__string_None:
        return fallback__298
    case Option__string_Some:
        var x161 string = self__297.(Option__string_Some)._0
        return x161
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
