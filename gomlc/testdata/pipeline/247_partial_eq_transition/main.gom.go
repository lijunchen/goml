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
    var t737 int = self__0.value
    var t738 int = other__1.value
    var inline1642 bool = t737 == t738
    return inline1642
}

func _goml_m_trait__impl_i_Hash_i_LegacyKey_i_hash(self__2 LegacyKey) uint64 {
    var t742_source int = 0
    var t742 uint64 = uint64(int(t742_source))
    var h__3 uint64 = t742 + 14695981039346656037
    var t743_source int = 0
    var t743 uint64 = uint64(int(t743_source))
    var t744 uint64 = t743 + 1099511628211
    var t745 uint64 = h__3 * t744
    var t746 int = self__2.value
    var t747 uint64
    var inline1644 uint64 = _goml_runtime_core_int_hash(t746)
    t747 = inline1644
    var h__4 uint64 = t745 + t747
    return h__4
}

func _goml_m_trait__impl_i_PartialEq_i_ModernKey_i_eq(self__5 ModernKey, other__6 ModernKey) bool {
    var t750 int = self__5.value
    var t751_rhs int = 10
    var t751 int = t750 % t751_rhs
    var t752 int = other__6.value
    var t753_rhs int = 10
    var t753 int = t752 % t753_rhs
    var t754 bool = t751 == t753
    return t754
}

func _goml_m_trait__impl_i_Hash_i_ModernKey_i_hash(self__7 ModernKey) uint64 {
    var t757 int = self__7.value
    var t758_rhs int = 10
    var t758 int = t757 % t758_rhs
    var t759 uint64 = uint64(int(t758))
    return t759
}

func main0() struct{} {
    var legacy__14 LegacyKey = LegacyKey{
        value: 7,
    }
    var modern__15 ModernKey = ModernKey{
        value: 9,
    }
    var t791 LegacyKey = LegacyKey{
        value: 7,
    }
    var t792 bool = equal__T_LegacyKey(legacy__14, t791)
    println__T_bool(t792)
    var t793 LegacyKey = LegacyKey{
        value: 8,
    }
    var t794 bool = _goml_m_trait__impl_i_PartialEq_i_LegacyKey_i_eq(legacy__14, t793)
    println__T_bool(t794)
    var t795 ModernKey = ModernKey{
        value: 19,
    }
    var t796 bool = equal__T_ModernKey(modern__15, t795)
    println__T_bool(t796)
    var t797 ModernKey = ModernKey{
        value: 20,
    }
    var t798 bool = _goml_m_trait__impl_i_PartialEq_i_ModernKey_i_eq(modern__15, t797)
    var t799 bool = !t798
    var t800 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t799)
    println__T_string(t800)
    var legacy_map__16 *hashmap_LegacyKey_string_x = _goml_m_inherent_i_HashMap_i_H_h4f35fad7fd3ed72455715cdf3969637d_ey____V__string()
    _goml_m_inherent_i_HashMap_i_H_h4c415936d3e2c958d5274434037d6231_ey____V__string(legacy_map__16, legacy__14, "legacy")
    var t801 LegacyKey = LegacyKey{
        value: 7,
    }
    var t802 Option__string
    var inline1713 Option__string = hashmap_get__HashMap_9LegacyKey_6string(legacy_map__16, t801)
    t802 = inline1713
    var t803 string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(t802, "missing")
    var inline1710 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t803)
    _goml_runtime_core_string_println(inline1710)
    var modern_map__17 *hashmap_ModernKey_string_x
    var inline1708 *hashmap_ModernKey_string_x = hashmap_new__HashMap_9ModernKey_6string()
    modern_map__17 = inline1708
    var inline1705 string = "modern"
    hashmap_set__HashMap_9ModernKey_6string(modern_map__17, modern__15, inline1705)
    var t804 ModernKey = ModernKey{
        value: 19,
    }
    var t805 Option__string
    var inline1703 Option__string = hashmap_get__HashMap_9ModernKey_6string(modern_map__17, t804)
    t805 = inline1703
    var t806 string
    var inline1699 string = "missing"
    switch t805.(type) {
    case Option__string_None:
        t806 = inline1699
    case Option__string_Some:
        var inline1700 string = t805.(Option__string_Some)._0
        t806 = inline1700
    default:
        panic("non-exhaustive match")
    }
    var inline1696 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t806)
    _goml_runtime_core_string_println(inline1696)
    var high__18 Rank = Rank{
        value: 2,
    }
    var low__19 Rank = Rank{
        value: 1,
    }
    var t807 bool
    var inline1694 bool = _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_lt____Self__Rank(high__18, low__19)
    t807 = inline1694
    var t808 string
    var inline1692 string = _goml_runtime_core_bool_to_string(t807)
    t808 = inline1692
    var inline1689 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t808)
    _goml_runtime_core_string_println(inline1689)
    var t809 bool
    var inline1687 bool = _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_le____Self__Rank(high__18, high__18)
    t809 = inline1687
    var t810 string
    var inline1685 string = _goml_runtime_core_bool_to_string(t809)
    t810 = inline1685
    var inline1682 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t810)
    _goml_runtime_core_string_println(inline1682)
    var t811 bool
    var inline1680 bool = _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_gt____Self__Rank(low__19, high__18)
    t811 = inline1680
    var t812 string
    var inline1678 string = _goml_runtime_core_bool_to_string(t811)
    t812 = inline1678
    var inline1675 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t812)
    _goml_runtime_core_string_println(inline1675)
    var t813 bool
    var inline1673 bool = _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_ge____Self__Rank(low__19, low__19)
    t813 = inline1673
    var t814 string
    var inline1671 string = _goml_runtime_core_bool_to_string(t813)
    t814 = inline1671
    var inline1668 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t814)
    _goml_runtime_core_string_println(inline1668)
    return struct{}{}
}

func _goml_m_trait__impl_i_PartialEq_i_int_i_eq(self__101 int, other__102 int) bool {
    var t1192 bool = self__101 == other__102
    return t1192
}

func _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_lt____Self__Rank(self__15 Rank, other__16 Rank) bool {
    var commute_field2440 _goml_m_std_p_cmp_p_Ordering
    var inline2180 int = self__15.value
    var inline2181 int = other__16.value
    var inline2182 bool = inline2180 > inline2181
    var inline2184 _goml_m_std_p_cmp_p_Ordering
    if inline2182 {
        inline2184 = Less
    } else {
        var inline2186 int = self__15.value
        var inline2187 int = other__16.value
        var inline2188 bool = inline2186 < inline2187
        if inline2188 {
            inline2184 = Greater
        } else {
            inline2184 = Equal
        }
    }
    commute_field2440 = inline2184
    switch commute_field2440 {
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
    var commute_field2443 _goml_m_std_p_cmp_p_Ordering
    var inline2191 int = self__18.value
    var inline2192 int = other__19.value
    var inline2193 bool = inline2191 > inline2192
    var inline2195 _goml_m_std_p_cmp_p_Ordering
    if inline2193 {
        inline2195 = Less
    } else {
        var inline2197 int = self__18.value
        var inline2198 int = other__19.value
        var inline2199 bool = inline2197 < inline2198
        if inline2199 {
            inline2195 = Greater
        } else {
            inline2195 = Equal
        }
    }
    commute_field2443 = inline2195
    var t1205 bool
    switch commute_field2443 {
    case Less:
        t1205 = false
    case Equal:
        t1205 = false
    case Greater:
        t1205 = true
    default:
        panic("non-exhaustive match")
    }
    var t1206 bool = !t1205
    return t1206
}

func _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_gt____Self__Rank(self__21 Rank, other__22 Rank) bool {
    var commute_field2446 _goml_m_std_p_cmp_p_Ordering
    var inline2202 int = self__21.value
    var inline2203 int = other__22.value
    var inline2204 bool = inline2202 > inline2203
    var inline2206 _goml_m_std_p_cmp_p_Ordering
    if inline2204 {
        inline2206 = Less
    } else {
        var inline2208 int = self__21.value
        var inline2209 int = other__22.value
        var inline2210 bool = inline2208 < inline2209
        if inline2210 {
            inline2206 = Greater
        } else {
            inline2206 = Equal
        }
    }
    commute_field2446 = inline2206
    switch commute_field2446 {
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
    var commute_field2449 _goml_m_std_p_cmp_p_Ordering
    var inline2213 int = self__24.value
    var inline2214 int = other__25.value
    var inline2215 bool = inline2213 > inline2214
    var inline2217 _goml_m_std_p_cmp_p_Ordering
    if inline2215 {
        inline2217 = Less
    } else {
        var inline2219 int = self__24.value
        var inline2220 int = other__25.value
        var inline2221 bool = inline2219 < inline2220
        if inline2221 {
            inline2217 = Greater
        } else {
            inline2217 = Equal
        }
    }
    commute_field2449 = inline2217
    var t1216 bool
    switch commute_field2449 {
    case Less:
        t1216 = true
    case Equal:
        t1216 = false
    case Greater:
        t1216 = false
    default:
        panic("non-exhaustive match")
    }
    var t1217 bool = !t1216
    return t1217
}

func println__T_bool(value__1 bool) struct{} {
    var t1219 string
    var inline2223 string = _goml_runtime_core_bool_to_string(value__1)
    t1219 = inline2223
    _goml_runtime_core_string_println(t1219)
    return struct{}{}
}

func equal__T_LegacyKey(left__12 LegacyKey, right__13 LegacyKey) bool {
    var inline2225 int = left__12.value
    var inline2226 int = right__13.value
    var inline2227 bool = _goml_m_trait__impl_i_PartialEq_i_int_i_eq(inline2225, inline2226)
    return inline2227
}

func equal__T_ModernKey(left__12 ModernKey, right__13 ModernKey) bool {
    var inline2229 int = left__12.value
    var inline2230_rhs int = 10
    var inline2230 int = inline2229 % inline2230_rhs
    var inline2231 int = right__13.value
    var inline2232_rhs int = 10
    var inline2232 int = inline2231 % inline2232_rhs
    var inline2233 bool = inline2230 == inline2232
    return inline2233
}

func println__T_string(value__1 string) struct{} {
    var t1228 string
    t1228 = value__1
    _goml_runtime_core_string_println(t1228)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t1232 string = _goml_runtime_core_bool_to_string(self__64)
    return t1232
}

func _goml_m_inherent_i_HashMap_i_H_h4f35fad7fd3ed72455715cdf3969637d_ey____V__string() *hashmap_LegacyKey_string_x {
    var t1235 *hashmap_LegacyKey_string_x = hashmap_new__HashMap_9LegacyKey_6string()
    return t1235
}

func _goml_m_inherent_i_HashMap_i_H_h4c415936d3e2c958d5274434037d6231_ey____V__string(self__264 *hashmap_LegacyKey_string_x, key__265 LegacyKey, value__266 string) struct{} {
    hashmap_set__HashMap_9LegacyKey_6string(self__264, key__265, value__266)
    return struct{}{}
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(self__300 Option__string, fallback__301 string) string {
    switch self__300.(type) {
    case Option__string_None:
        return fallback__301
    case Option__string_Some:
        var x166 string = self__300.(Option__string_Some)._0
        return x166
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
