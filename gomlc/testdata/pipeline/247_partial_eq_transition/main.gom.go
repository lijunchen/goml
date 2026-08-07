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
    var t722 int = self__0.value
    var t723 int = other__1.value
    var inline1644 bool = t722 == t723
    return inline1644
}

func _goml_m_trait__impl_i_Hash_i_LegacyKey_i_hash(self__2 LegacyKey) uint64 {
    var t727_source int = 0
    var t727 uint64 = uint64(int(t727_source))
    var h__3 uint64 = t727 + 14695981039346656037
    var t728_source int = 0
    var t728 uint64 = uint64(int(t728_source))
    var t729 uint64 = t728 + 1099511628211
    var t730 uint64 = h__3 * t729
    var t731 int = self__2.value
    var t732 uint64
    var inline1646 uint64 = _goml_runtime_core_int_hash(t731)
    t732 = inline1646
    var h__4 uint64 = t730 + t732
    return h__4
}

func _goml_m_trait__impl_i_PartialEq_i_ModernKey_i_eq(self__5 ModernKey, other__6 ModernKey) bool {
    var t735 int = self__5.value
    var t736_rhs int = 10
    var t736 int = t735 % t736_rhs
    var t737 int = other__6.value
    var t738_rhs int = 10
    var t738 int = t737 % t738_rhs
    var inline1648 bool = t736 == t738
    return inline1648
}

func _goml_m_trait__impl_i_Hash_i_ModernKey_i_hash(self__7 ModernKey) uint64 {
    var t742 int = self__7.value
    var t743_rhs int = 10
    var t743 int = t742 % t743_rhs
    var t744 uint64 = uint64(int(t743))
    return t744
}

func main0() struct{} {
    var legacy__14 LegacyKey = LegacyKey{
        value: 7,
    }
    var modern__15 ModernKey = ModernKey{
        value: 9,
    }
    var t776 LegacyKey = LegacyKey{
        value: 7,
    }
    var t777 bool = equal__T_LegacyKey(legacy__14, t776)
    println__T_bool(t777)
    var t778 LegacyKey = LegacyKey{
        value: 8,
    }
    var t779 bool = _goml_m_trait__impl_i_PartialEq_i_LegacyKey_i_eq(legacy__14, t778)
    println__T_bool(t779)
    var t780 ModernKey = ModernKey{
        value: 19,
    }
    var t781 bool = equal__T_ModernKey(modern__15, t780)
    println__T_bool(t781)
    var t782 ModernKey = ModernKey{
        value: 20,
    }
    var t783 bool = _goml_m_trait__impl_i_PartialEq_i_ModernKey_i_eq(modern__15, t782)
    var t784 bool = !t783
    var t785 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t784)
    println__T_string(t785)
    var legacy_map__16 *hashmap_LegacyKey_string_x = _goml_m_inherent_i_HashMap_i_H_h4f35fad7fd3ed72455715cdf3969637d_ey____V__string()
    _goml_m_inherent_i_HashMap_i_H_h4c415936d3e2c958d5274434037d6231_ey____V__string(legacy_map__16, legacy__14, "legacy")
    var t786 LegacyKey = LegacyKey{
        value: 7,
    }
    var t787 Option__string
    var inline1719 Option__string = hashmap_get__HashMap_9LegacyKey_6string(legacy_map__16, t786)
    t787 = inline1719
    var t788 string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(t787, "missing")
    var inline1716 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t788)
    _goml_runtime_core_string_println(inline1716)
    var modern_map__17 *hashmap_ModernKey_string_x
    var inline1714 *hashmap_ModernKey_string_x = hashmap_new__HashMap_9ModernKey_6string()
    modern_map__17 = inline1714
    var inline1711 string = "modern"
    hashmap_set__HashMap_9ModernKey_6string(modern_map__17, modern__15, inline1711)
    var t789 ModernKey = ModernKey{
        value: 19,
    }
    var t790 Option__string
    var inline1709 Option__string = hashmap_get__HashMap_9ModernKey_6string(modern_map__17, t789)
    t790 = inline1709
    var t791 string
    var inline1705 string = "missing"
    switch t790.(type) {
    case Option__string_None:
        t791 = inline1705
    case Option__string_Some:
        var inline1706 string = t790.(Option__string_Some)._0
        t791 = inline1706
    default:
        panic("non-exhaustive match")
    }
    var inline1702 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t791)
    _goml_runtime_core_string_println(inline1702)
    var high__18 Rank = Rank{
        value: 2,
    }
    var low__19 Rank = Rank{
        value: 1,
    }
    var t792 bool
    var inline1700 bool = _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_lt____Self__Rank(high__18, low__19)
    t792 = inline1700
    var t793 string
    var inline1698 string = _goml_runtime_core_bool_to_string(t792)
    t793 = inline1698
    var inline1695 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t793)
    _goml_runtime_core_string_println(inline1695)
    var t794 bool
    var inline1693 bool = _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_le____Self__Rank(high__18, high__18)
    t794 = inline1693
    var t795 string
    var inline1691 string = _goml_runtime_core_bool_to_string(t794)
    t795 = inline1691
    var inline1688 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t795)
    _goml_runtime_core_string_println(inline1688)
    var t796 bool
    var inline1686 bool = _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_gt____Self__Rank(low__19, high__18)
    t796 = inline1686
    var t797 string
    var inline1684 string = _goml_runtime_core_bool_to_string(t796)
    t797 = inline1684
    var inline1681 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t797)
    _goml_runtime_core_string_println(inline1681)
    var t798 bool
    var inline1679 bool = _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_ge____Self__Rank(low__19, low__19)
    t798 = inline1679
    var t799 string
    var inline1677 string = _goml_runtime_core_bool_to_string(t798)
    t799 = inline1677
    var inline1674 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t799)
    _goml_runtime_core_string_println(inline1674)
    return struct{}{}
}

func _goml_m_trait__impl_i_PartialEq_i_int_i_eq(self__103 int, other__104 int) bool {
    var t1186 bool = self__103 == other__104
    return t1186
}

func _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_lt____Self__Rank(self__15 Rank, other__16 Rank) bool {
    var commute_field2446 _goml_m_std_p_cmp_p_Ordering
    var inline2186 int = self__15.value
    var inline2187 int = other__16.value
    var inline2188 bool = inline2186 > inline2187
    var inline2190 _goml_m_std_p_cmp_p_Ordering
    if inline2188 {
        inline2190 = Less
    } else {
        var inline2192 int = self__15.value
        var inline2193 int = other__16.value
        var inline2194 bool = inline2192 < inline2193
        if inline2194 {
            inline2190 = Greater
        } else {
            inline2190 = Equal
        }
    }
    commute_field2446 = inline2190
    switch commute_field2446 {
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
    var commute_field2449 _goml_m_std_p_cmp_p_Ordering
    var inline2197 int = self__18.value
    var inline2198 int = other__19.value
    var inline2199 bool = inline2197 > inline2198
    var inline2201 _goml_m_std_p_cmp_p_Ordering
    if inline2199 {
        inline2201 = Less
    } else {
        var inline2203 int = self__18.value
        var inline2204 int = other__19.value
        var inline2205 bool = inline2203 < inline2204
        if inline2205 {
            inline2201 = Greater
        } else {
            inline2201 = Equal
        }
    }
    commute_field2449 = inline2201
    var t1199 bool
    switch commute_field2449 {
    case Less:
        t1199 = false
    case Equal:
        t1199 = false
    case Greater:
        t1199 = true
    default:
        panic("non-exhaustive match")
    }
    var t1200 bool = !t1199
    return t1200
}

func _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_gt____Self__Rank(self__21 Rank, other__22 Rank) bool {
    var commute_field2452 _goml_m_std_p_cmp_p_Ordering
    var inline2208 int = self__21.value
    var inline2209 int = other__22.value
    var inline2210 bool = inline2208 > inline2209
    var inline2212 _goml_m_std_p_cmp_p_Ordering
    if inline2210 {
        inline2212 = Less
    } else {
        var inline2214 int = self__21.value
        var inline2215 int = other__22.value
        var inline2216 bool = inline2214 < inline2215
        if inline2216 {
            inline2212 = Greater
        } else {
            inline2212 = Equal
        }
    }
    commute_field2452 = inline2212
    switch commute_field2452 {
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
    var commute_field2455 _goml_m_std_p_cmp_p_Ordering
    var inline2219 int = self__24.value
    var inline2220 int = other__25.value
    var inline2221 bool = inline2219 > inline2220
    var inline2223 _goml_m_std_p_cmp_p_Ordering
    if inline2221 {
        inline2223 = Less
    } else {
        var inline2225 int = self__24.value
        var inline2226 int = other__25.value
        var inline2227 bool = inline2225 < inline2226
        if inline2227 {
            inline2223 = Greater
        } else {
            inline2223 = Equal
        }
    }
    commute_field2455 = inline2223
    var t1210 bool
    switch commute_field2455 {
    case Less:
        t1210 = true
    case Equal:
        t1210 = false
    case Greater:
        t1210 = false
    default:
        panic("non-exhaustive match")
    }
    var t1211 bool = !t1210
    return t1211
}

func println__T_bool(value__31 bool) struct{} {
    var t1213 string
    var inline2229 string = _goml_runtime_core_bool_to_string(value__31)
    t1213 = inline2229
    _goml_runtime_core_string_println(t1213)
    return struct{}{}
}

func equal__T_LegacyKey(left__12 LegacyKey, right__13 LegacyKey) bool {
    var inline2231 int = left__12.value
    var inline2232 int = right__13.value
    var inline2233 bool = _goml_m_trait__impl_i_PartialEq_i_int_i_eq(inline2231, inline2232)
    return inline2233
}

func equal__T_ModernKey(left__12 ModernKey, right__13 ModernKey) bool {
    var inline2235 int = left__12.value
    var inline2236_rhs int = 10
    var inline2236 int = inline2235 % inline2236_rhs
    var inline2237 int = right__13.value
    var inline2238_rhs int = 10
    var inline2238 int = inline2237 % inline2238_rhs
    var inline2239 bool = _goml_m_trait__impl_i_PartialEq_i_int_i_eq(inline2236, inline2238)
    return inline2239
}

func println__T_string(value__31 string) struct{} {
    var t1222 string
    t1222 = value__31
    _goml_runtime_core_string_println(t1222)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t1226 string = _goml_runtime_core_bool_to_string(self__66)
    return t1226
}

func _goml_m_inherent_i_HashMap_i_H_h4f35fad7fd3ed72455715cdf3969637d_ey____V__string() *hashmap_LegacyKey_string_x {
    var t1229 *hashmap_LegacyKey_string_x = hashmap_new__HashMap_9LegacyKey_6string()
    return t1229
}

func _goml_m_inherent_i_HashMap_i_H_h4c415936d3e2c958d5274434037d6231_ey____V__string(self__248 *hashmap_LegacyKey_string_x, key__249 LegacyKey, value__250 string) struct{} {
    hashmap_set__HashMap_9LegacyKey_6string(self__248, key__249, value__250)
    return struct{}{}
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(self__290 Option__string, fallback__291 string) string {
    switch self__290.(type) {
    case Option__string_None:
        return fallback__291
    case Option__string_Some:
        var x152 string = self__290.(Option__string_Some)._0
        return x152
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
