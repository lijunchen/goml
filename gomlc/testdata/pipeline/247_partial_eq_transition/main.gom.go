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
    var inline1627 bool = t722 == t723
    return inline1627
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
    var inline1629 uint64 = _goml_runtime_core_int_hash(t731)
    t732 = inline1629
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
    var t739 bool = t736 == t738
    return t739
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
    var inline1698 Option__string = hashmap_get__HashMap_9LegacyKey_6string(legacy_map__16, t786)
    t787 = inline1698
    var t788 string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(t787, "missing")
    var inline1695 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t788)
    _goml_runtime_core_string_println(inline1695)
    var modern_map__17 *hashmap_ModernKey_string_x
    var inline1693 *hashmap_ModernKey_string_x = hashmap_new__HashMap_9ModernKey_6string()
    modern_map__17 = inline1693
    var inline1690 string = "modern"
    hashmap_set__HashMap_9ModernKey_6string(modern_map__17, modern__15, inline1690)
    var t789 ModernKey = ModernKey{
        value: 19,
    }
    var t790 Option__string
    var inline1688 Option__string = hashmap_get__HashMap_9ModernKey_6string(modern_map__17, t789)
    t790 = inline1688
    var t791 string
    var inline1684 string = "missing"
    switch t790.(type) {
    case Option__string_None:
        t791 = inline1684
    case Option__string_Some:
        var inline1685 string = t790.(Option__string_Some)._0
        t791 = inline1685
    default:
        panic("non-exhaustive match")
    }
    var inline1681 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t791)
    _goml_runtime_core_string_println(inline1681)
    var high__18 Rank = Rank{
        value: 2,
    }
    var low__19 Rank = Rank{
        value: 1,
    }
    var t792 bool
    var inline1679 bool = _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_lt____Self__Rank(high__18, low__19)
    t792 = inline1679
    var t793 string
    var inline1677 string = _goml_runtime_core_bool_to_string(t792)
    t793 = inline1677
    var inline1674 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t793)
    _goml_runtime_core_string_println(inline1674)
    var t794 bool
    var inline1672 bool = _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_le____Self__Rank(high__18, high__18)
    t794 = inline1672
    var t795 string
    var inline1670 string = _goml_runtime_core_bool_to_string(t794)
    t795 = inline1670
    var inline1667 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t795)
    _goml_runtime_core_string_println(inline1667)
    var t796 bool
    var inline1665 bool = _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_gt____Self__Rank(low__19, high__18)
    t796 = inline1665
    var t797 string
    var inline1663 string = _goml_runtime_core_bool_to_string(t796)
    t797 = inline1663
    var inline1660 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t797)
    _goml_runtime_core_string_println(inline1660)
    var t798 bool
    var inline1658 bool = _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_ge____Self__Rank(low__19, low__19)
    t798 = inline1658
    var t799 string
    var inline1656 string = _goml_runtime_core_bool_to_string(t798)
    t799 = inline1656
    var inline1653 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t799)
    _goml_runtime_core_string_println(inline1653)
    return struct{}{}
}

func _goml_m_trait__impl_i_PartialEq_i_int_i_eq(self__103 int, other__104 int) bool {
    var t1177 bool = self__103 == other__104
    return t1177
}

func _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_lt____Self__Rank(self__15 Rank, other__16 Rank) bool {
    var commute_field2425 _goml_m_std_p_cmp_p_Ordering
    var inline2165 int = self__15.value
    var inline2166 int = other__16.value
    var inline2167 bool = inline2165 > inline2166
    var inline2169 _goml_m_std_p_cmp_p_Ordering
    if inline2167 {
        inline2169 = Less
    } else {
        var inline2171 int = self__15.value
        var inline2172 int = other__16.value
        var inline2173 bool = inline2171 < inline2172
        if inline2173 {
            inline2169 = Greater
        } else {
            inline2169 = Equal
        }
    }
    commute_field2425 = inline2169
    switch commute_field2425 {
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
    var commute_field2428 _goml_m_std_p_cmp_p_Ordering
    var inline2176 int = self__18.value
    var inline2177 int = other__19.value
    var inline2178 bool = inline2176 > inline2177
    var inline2180 _goml_m_std_p_cmp_p_Ordering
    if inline2178 {
        inline2180 = Less
    } else {
        var inline2182 int = self__18.value
        var inline2183 int = other__19.value
        var inline2184 bool = inline2182 < inline2183
        if inline2184 {
            inline2180 = Greater
        } else {
            inline2180 = Equal
        }
    }
    commute_field2428 = inline2180
    var t1190 bool
    switch commute_field2428 {
    case Less:
        t1190 = false
    case Equal:
        t1190 = false
    case Greater:
        t1190 = true
    default:
        panic("non-exhaustive match")
    }
    var t1191 bool = !t1190
    return t1191
}

func _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_gt____Self__Rank(self__21 Rank, other__22 Rank) bool {
    var commute_field2431 _goml_m_std_p_cmp_p_Ordering
    var inline2187 int = self__21.value
    var inline2188 int = other__22.value
    var inline2189 bool = inline2187 > inline2188
    var inline2191 _goml_m_std_p_cmp_p_Ordering
    if inline2189 {
        inline2191 = Less
    } else {
        var inline2193 int = self__21.value
        var inline2194 int = other__22.value
        var inline2195 bool = inline2193 < inline2194
        if inline2195 {
            inline2191 = Greater
        } else {
            inline2191 = Equal
        }
    }
    commute_field2431 = inline2191
    switch commute_field2431 {
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
    var commute_field2434 _goml_m_std_p_cmp_p_Ordering
    var inline2198 int = self__24.value
    var inline2199 int = other__25.value
    var inline2200 bool = inline2198 > inline2199
    var inline2202 _goml_m_std_p_cmp_p_Ordering
    if inline2200 {
        inline2202 = Less
    } else {
        var inline2204 int = self__24.value
        var inline2205 int = other__25.value
        var inline2206 bool = inline2204 < inline2205
        if inline2206 {
            inline2202 = Greater
        } else {
            inline2202 = Equal
        }
    }
    commute_field2434 = inline2202
    var t1201 bool
    switch commute_field2434 {
    case Less:
        t1201 = true
    case Equal:
        t1201 = false
    case Greater:
        t1201 = false
    default:
        panic("non-exhaustive match")
    }
    var t1202 bool = !t1201
    return t1202
}

func println__T_bool(value__31 bool) struct{} {
    var t1204 string
    var inline2208 string = _goml_runtime_core_bool_to_string(value__31)
    t1204 = inline2208
    _goml_runtime_core_string_println(t1204)
    return struct{}{}
}

func equal__T_LegacyKey(left__12 LegacyKey, right__13 LegacyKey) bool {
    var inline2210 int = left__12.value
    var inline2211 int = right__13.value
    var inline2212 bool = _goml_m_trait__impl_i_PartialEq_i_int_i_eq(inline2210, inline2211)
    return inline2212
}

func equal__T_ModernKey(left__12 ModernKey, right__13 ModernKey) bool {
    var inline2214 int = left__12.value
    var inline2215_rhs int = 10
    var inline2215 int = inline2214 % inline2215_rhs
    var inline2216 int = right__13.value
    var inline2217_rhs int = 10
    var inline2217 int = inline2216 % inline2217_rhs
    var inline2218 bool = inline2215 == inline2217
    return inline2218
}

func println__T_string(value__31 string) struct{} {
    var t1213 string
    t1213 = value__31
    _goml_runtime_core_string_println(t1213)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t1217 string = _goml_runtime_core_bool_to_string(self__66)
    return t1217
}

func _goml_m_inherent_i_HashMap_i_H_h4f35fad7fd3ed72455715cdf3969637d_ey____V__string() *hashmap_LegacyKey_string_x {
    var t1220 *hashmap_LegacyKey_string_x = hashmap_new__HashMap_9LegacyKey_6string()
    return t1220
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
