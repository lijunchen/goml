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

func _goml_runtime_core_int_hash(x int) uint64 {
    return uint64(x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_os.Stdout.WriteString(s + "\n")
    return struct{}{}
}

type _goml_vec_uint32 struct {
    items []uint32
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
    var t1288 int = self__0.value
    var t1289 int = other__1.value
    var inline2195 bool = t1288 == t1289
    return inline2195
}

func _goml_m_trait__impl_i_Hash_i_LegacyKey_i_hash(self__2 LegacyKey) uint64 {
    var t1293_source int = 0
    var t1293 uint64 = uint64(int(t1293_source))
    var h__3 uint64 = t1293 + 14695981039346656037
    var t1294_source int = 0
    var t1294 uint64 = uint64(int(t1294_source))
    var t1295 uint64 = t1294 + 1099511628211
    var t1296 uint64 = h__3 * t1295
    var t1297 int = self__2.value
    var t1298 uint64
    var inline2197 uint64 = _goml_runtime_core_int_hash(t1297)
    t1298 = inline2197
    var h__4 uint64 = t1296 + t1298
    return h__4
}

func _goml_m_trait__impl_i_PartialEq_i_ModernKey_i_eq(self__5 ModernKey, other__6 ModernKey) bool {
    var t1301 int = self__5.value
    var t1302_rhs int = 10
    var t1302 int = t1301 % t1302_rhs
    var t1303 int = other__6.value
    var t1304_rhs int = 10
    var t1304 int = t1303 % t1304_rhs
    var t1305 bool = t1302 == t1304
    return t1305
}

func _goml_m_trait__impl_i_Hash_i_ModernKey_i_hash(self__7 ModernKey) uint64 {
    var t1308 int = self__7.value
    var t1309_rhs int = 10
    var t1309 int = t1308 % t1309_rhs
    var t1310 uint64 = uint64(int(t1309))
    return t1310
}

func main0() struct{} {
    var legacy__14 LegacyKey = LegacyKey{
        value: 7,
    }
    var modern__15 ModernKey = ModernKey{
        value: 9,
    }
    var t1342 LegacyKey = LegacyKey{
        value: 7,
    }
    var t1343 bool = equal__T_LegacyKey(legacy__14, t1342)
    println__T_bool(t1343)
    var t1344 LegacyKey = LegacyKey{
        value: 8,
    }
    var t1345 bool = _goml_m_trait__impl_i_PartialEq_i_LegacyKey_i_eq(legacy__14, t1344)
    println__T_bool(t1345)
    var t1346 ModernKey = ModernKey{
        value: 19,
    }
    var t1347 bool = equal__T_ModernKey(modern__15, t1346)
    println__T_bool(t1347)
    var t1348 ModernKey = ModernKey{
        value: 20,
    }
    var t1349 bool = _goml_m_trait__impl_i_PartialEq_i_ModernKey_i_eq(modern__15, t1348)
    var t1350 bool = !t1349
    var t1351 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t1350)
    println__T_string(t1351)
    var legacy_map__16 *hashmap_LegacyKey_string_x = _goml_m_inherent_i_HashMap_i_H_h4f35fad7fd3ed72455715cdf3969637d_ey____V__string()
    _goml_m_inherent_i_HashMap_i_H_h4c415936d3e2c958d5274434037d6231_ey____V__string(legacy_map__16, legacy__14, "legacy")
    var t1352 LegacyKey = LegacyKey{
        value: 7,
    }
    var t1353 Option__string
    var inline2266 Option__string = hashmap_get__HashMap_9LegacyKey_6string(legacy_map__16, t1352)
    t1353 = inline2266
    var t1354 string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(t1353, "missing")
    var inline2263 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1354)
    _goml_runtime_core_string_println(inline2263)
    var modern_map__17 *hashmap_ModernKey_string_x
    var inline2261 *hashmap_ModernKey_string_x = hashmap_new__HashMap_9ModernKey_6string()
    modern_map__17 = inline2261
    var inline2258 string = "modern"
    hashmap_set__HashMap_9ModernKey_6string(modern_map__17, modern__15, inline2258)
    var t1355 ModernKey = ModernKey{
        value: 19,
    }
    var t1356 Option__string
    var inline2256 Option__string = hashmap_get__HashMap_9ModernKey_6string(modern_map__17, t1355)
    t1356 = inline2256
    var t1357 string
    var inline2252 string = "missing"
    switch t1356._tag {
    case 0:
        t1357 = inline2252
    case 1:
        var inline2253 string = t1356._v1_0
        t1357 = inline2253
    default:
        panic("non-exhaustive match")
    }
    var inline2249 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1357)
    _goml_runtime_core_string_println(inline2249)
    var high__18 Rank = Rank{
        value: 2,
    }
    var low__19 Rank = Rank{
        value: 1,
    }
    var t1358 bool
    var inline2247 bool = _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_lt____Self__Rank(high__18, low__19)
    t1358 = inline2247
    var t1359 string
    var inline2245 string = _goml_runtime_core_bool_to_string(t1358)
    t1359 = inline2245
    var inline2242 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1359)
    _goml_runtime_core_string_println(inline2242)
    var t1360 bool
    var inline2240 bool = _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_le____Self__Rank(high__18, high__18)
    t1360 = inline2240
    var t1361 string
    var inline2238 string = _goml_runtime_core_bool_to_string(t1360)
    t1361 = inline2238
    var inline2235 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1361)
    _goml_runtime_core_string_println(inline2235)
    var t1362 bool
    var inline2233 bool = _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_gt____Self__Rank(low__19, high__18)
    t1362 = inline2233
    var t1363 string
    var inline2231 string = _goml_runtime_core_bool_to_string(t1362)
    t1363 = inline2231
    var inline2228 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1363)
    _goml_runtime_core_string_println(inline2228)
    var t1364 bool
    var inline2226 bool = _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_ge____Self__Rank(low__19, low__19)
    t1364 = inline2226
    var t1365 string
    var inline2224 string = _goml_runtime_core_bool_to_string(t1364)
    t1365 = inline2224
    var inline2221 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1365)
    _goml_runtime_core_string_println(inline2221)
    return struct{}{}
}

func _goml_m_trait__impl_i_PartialEq_i_isize_i_eq(self__438 int, other__439 int) bool {
    var t1753 bool = self__438 == other__439
    return t1753
}

func _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_lt____Self__Rank(self__0 Rank, other__1 Rank) bool {
    var commute_field2993 Ordering
    var inline2733 int = self__0.value
    var inline2734 int = other__1.value
    var inline2735 bool = inline2733 > inline2734
    var inline2737 Ordering
    if inline2735 {
        inline2737 = Less
    } else {
        var inline2739 int = self__0.value
        var inline2740 int = other__1.value
        var inline2741 bool = inline2739 < inline2740
        if inline2741 {
            inline2737 = Greater
        } else {
            inline2737 = Equal
        }
    }
    commute_field2993 = inline2737
    switch commute_field2993 {
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
    var commute_field2996 Ordering
    var inline2744 int = self__3.value
    var inline2745 int = other__4.value
    var inline2746 bool = inline2744 > inline2745
    var inline2748 Ordering
    if inline2746 {
        inline2748 = Less
    } else {
        var inline2750 int = self__3.value
        var inline2751 int = other__4.value
        var inline2752 bool = inline2750 < inline2751
        if inline2752 {
            inline2748 = Greater
        } else {
            inline2748 = Equal
        }
    }
    commute_field2996 = inline2748
    var t1766 bool
    switch commute_field2996 {
    case Less:
        t1766 = false
    case Equal:
        t1766 = false
    case Greater:
        t1766 = true
    default:
        panic("non-exhaustive match")
    }
    var t1767 bool = !t1766
    return t1767
}

func _goml_m_std_p_cmp_p_trait__default_i_PartialOrd_i_gt____Self__Rank(self__6 Rank, other__7 Rank) bool {
    var commute_field2999 Ordering
    var inline2755 int = self__6.value
    var inline2756 int = other__7.value
    var inline2757 bool = inline2755 > inline2756
    var inline2759 Ordering
    if inline2757 {
        inline2759 = Less
    } else {
        var inline2761 int = self__6.value
        var inline2762 int = other__7.value
        var inline2763 bool = inline2761 < inline2762
        if inline2763 {
            inline2759 = Greater
        } else {
            inline2759 = Equal
        }
    }
    commute_field2999 = inline2759
    switch commute_field2999 {
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
    var commute_field3002 Ordering
    var inline2766 int = self__9.value
    var inline2767 int = other__10.value
    var inline2768 bool = inline2766 > inline2767
    var inline2770 Ordering
    if inline2768 {
        inline2770 = Less
    } else {
        var inline2772 int = self__9.value
        var inline2773 int = other__10.value
        var inline2774 bool = inline2772 < inline2773
        if inline2774 {
            inline2770 = Greater
        } else {
            inline2770 = Equal
        }
    }
    commute_field3002 = inline2770
    var t1777 bool
    switch commute_field3002 {
    case Less:
        t1777 = true
    case Equal:
        t1777 = false
    case Greater:
        t1777 = false
    default:
        panic("non-exhaustive match")
    }
    var t1778 bool = !t1777
    return t1778
}

func println__T_bool(value__1 bool) struct{} {
    var t1780 string
    var inline2776 string = _goml_runtime_core_bool_to_string(value__1)
    t1780 = inline2776
    _goml_runtime_core_string_println(t1780)
    return struct{}{}
}

func equal__T_LegacyKey(left__12 LegacyKey, right__13 LegacyKey) bool {
    var inline2778 int = left__12.value
    var inline2779 int = right__13.value
    var inline2780 bool = _goml_m_trait__impl_i_PartialEq_i_isize_i_eq(inline2778, inline2779)
    return inline2780
}

func equal__T_ModernKey(left__12 ModernKey, right__13 ModernKey) bool {
    var inline2782 int = left__12.value
    var inline2783_rhs int = 10
    var inline2783 int = inline2782 % inline2783_rhs
    var inline2784 int = right__13.value
    var inline2785_rhs int = 10
    var inline2785 int = inline2784 % inline2785_rhs
    var inline2786 bool = inline2783 == inline2785
    return inline2786
}

func println__T_string(value__1 string) struct{} {
    var t1789 string
    t1789 = value__1
    _goml_runtime_core_string_println(t1789)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__401 bool) string {
    var t1793 string = _goml_runtime_core_bool_to_string(self__401)
    return t1793
}

func _goml_m_inherent_i_HashMap_i_H_h4f35fad7fd3ed72455715cdf3969637d_ey____V__string() *hashmap_LegacyKey_string_x {
    var t1796 *hashmap_LegacyKey_string_x = hashmap_new__HashMap_9LegacyKey_6string()
    return t1796
}

func _goml_m_inherent_i_HashMap_i_H_h4c415936d3e2c958d5274434037d6231_ey____V__string(self__675 *hashmap_LegacyKey_string_x, key__676 LegacyKey, value__677 string) struct{} {
    hashmap_set__HashMap_9LegacyKey_6string(self__675, key__676, value__677)
    return struct{}{}
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(self__720 Option__string, fallback__721 string) string {
    switch self__720._tag {
    case 0:
        return fallback__721
    case 1:
        var x775 string = self__720._v1_0
        return x775
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func main() {
    main0()
}
