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
        return Option__int32_Some{
            _0: value,
        }
    }
    return Option__int32_None{}
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
        return Option__string_Some{
            _0: value,
        }
    }
    return Option__string_None{}
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

type Option__int32 interface {
    isOption__int32()
}

type Option__int32_None struct {}

func (_ Option__int32_None) isOption__int32() {}

type Option__int32_Some struct {
    _0 int32
}

func (_ Option__int32_Some) isOption__int32() {}

func _goml_m_trait__impl_i_PartialEq_i_CollisionKey_i_eq(self__5 CollisionKey, other__6 CollisionKey) bool {
    var t794 int32 = self__5.value
    var t795 int32 = other__6.value
    var t796 bool = t794 == t795
    return t796
}

func _goml_m_trait__impl_i_Hash_i_CollisionKey_i_hash(self__7 CollisionKey) uint64 {
    return 1
}

func print_opt_int(value__10 Option__int32) struct{} {
    switch value__10.(type) {
    case Option__int32_None:
        var inline1767 string = "none"
        var inline1768 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline1767)
        _goml_runtime_core_string_println(inline1768)
        return struct{}{}
    case Option__int32_Some:
        var x190 int32 = value__10.(Option__int32_Some)._0
        var inline1771 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x190)
        _goml_runtime_core_string_println(inline1771)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func float_comparison_contracts() struct{} {
    var zero32__12 float32 = 0
    var negative_zero32__13 float32 = -zero32__12
    var t808 bool = zero32__12 == negative_zero32__13
    var t809 string
    var inline1811 string = _goml_runtime_core_bool_to_string(t808)
    t809 = inline1811
    var inline1808 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t809)
    _goml_runtime_core_string_println(inline1808)
    var zero64__14 float64 = 0
    var negative_zero64__15 float64 = -zero64__14
    var t810 bool = zero64__14 == negative_zero64__15
    var t811 string
    var inline1806 string = _goml_runtime_core_bool_to_string(t810)
    t811 = inline1806
    var inline1803 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t811)
    _goml_runtime_core_string_println(inline1803)
    var t814 bool
    var inline1801 bool = _goml_m_trait__impl_i_PartialEq_i_float64_i_eq(zero64__14, negative_zero64__15)
    t814 = inline1801
    var t815 string
    var inline1797 string = _goml_runtime_core_bool_to_string(t814)
    t815 = inline1797
    var inline1794 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t815)
    _goml_runtime_core_string_println(inline1794)
    var nan__16 float64 = zero64__14 / zero64__14
    var t816 bool = nan__16 == nan__16
    var t817 string
    var inline1792 string = _goml_runtime_core_bool_to_string(t816)
    t817 = inline1792
    var inline1789 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t817)
    _goml_runtime_core_string_println(inline1789)
    var t818 _goml_m_Option____std_p_cmp_p_Ordering
    var inline1782 bool = nan__16 < nan__16
    if inline1782 {
        var inline1783 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
            _0: Less,
        }
        t818 = inline1783
    } else {
        var inline1784 bool = nan__16 > nan__16
        if inline1784 {
            var inline1785 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: Greater,
            }
            t818 = inline1785
        } else {
            var inline1786 bool = nan__16 == nan__16
            if inline1786 {
                var inline1787 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                    _0: Equal,
                }
                t818 = inline1787
            } else {
                t818 = _goml_m_Option____std_p_cmp_p_Ordering_None{}
            }
        }
    }
    var t819 bool
    var inline1779 bool = _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__some____T__std_p_cmp_p_Ordering(t818)
    var inline1780 bool = !inline1779
    t819 = inline1780
    var t820 string
    var inline1777 string = _goml_runtime_core_bool_to_string(t819)
    t820 = inline1777
    var inline1774 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t820)
    _goml_runtime_core_string_println(inline1774)
    return struct{}{}
}

func collision_contracts() struct{} {
    var values__17 *hashmap_CollisionKey_int32_x = _goml_m_inherent_i_HashMap_i_H_hda117ae4fc64297aa6806f068e6c08ed_Key____V__int32()
    var t822 CollisionKey = CollisionKey{
        value: 1,
    }
    _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(values__17, t822, 10)
    var t823 CollisionKey = CollisionKey{
        value: 2,
    }
    _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(values__17, t823, 20)
    var t824 CollisionKey = CollisionKey{
        value: 3,
    }
    _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(values__17, t824, 30)
    var t825 CollisionKey = CollisionKey{
        value: 2,
    }
    _goml_m_inherent_i_HashMap_i_H_h28011e82243b31bd5417be204086ffb6_Key____V__int32(values__17, t825)
    var t826 CollisionKey = CollisionKey{
        value: 1,
    }
    var t827 Option__int32 = _goml_m_inherent_i_HashMap_i_H_hd089129fd747b5ac2054f800f07a211e_Key____V__int32(values__17, t826)
    print_opt_int(t827)
    var t828 CollisionKey = CollisionKey{
        value: 2,
    }
    var t829 Option__int32 = _goml_m_inherent_i_HashMap_i_H_hd089129fd747b5ac2054f800f07a211e_Key____V__int32(values__17, t828)
    print_opt_int(t829)
    var t830 CollisionKey = CollisionKey{
        value: 3,
    }
    var t831 Option__int32 = _goml_m_inherent_i_HashMap_i_H_hd089129fd747b5ac2054f800f07a211e_Key____V__int32(values__17, t830)
    print_opt_int(t831)
    var t832 CollisionKey = CollisionKey{
        value: 4,
    }
    _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(values__17, t832, 40)
    var t833 int = _goml_m_inherent_i_HashMap_i_H_h4aea484345d58a42435067138901add4_Key____V__int32(values__17)
    println__T_int(t833)
    var t834 CollisionKey = CollisionKey{
        value: 4,
    }
    var t835 Option__int32
    var inline1859 Option__int32 = hashmap_get__HashMap_12CollisionKey_5int32(values__17, t834)
    t835 = inline1859
    print_opt_int(t835)
    var t836 CollisionKey = CollisionKey{
        value: 4,
    }
    var inline1856 int32 = 41
    hashmap_set__HashMap_12CollisionKey_5int32(values__17, t836, inline1856)
    var t837 int
    var inline1854 int = hashmap_len__HashMap_12CollisionKey_5int32(values__17)
    t837 = inline1854
    var inline1851 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t837)
    _goml_runtime_core_string_println(inline1851)
    var t838 CollisionKey = CollisionKey{
        value: 4,
    }
    var t839 Option__int32
    var inline1849 Option__int32 = hashmap_get__HashMap_12CollisionKey_5int32(values__17, t838)
    t839 = inline1849
    switch t839.(type) {
    case Option__int32_None:
        println__T_string("none")
    case Option__int32_Some:
        var inline1845 int32 = t839.(Option__int32_Some)._0
        println__T_int32(inline1845)
    default:
        panic("non-exhaustive match")
    }
    var t840 CollisionKey = CollisionKey{
        value: 99,
    }
    hashmap_remove__HashMap_12CollisionKey_5int32(values__17, t840)
    var t841 CollisionKey = CollisionKey{
        value: 99,
    }
    hashmap_remove__HashMap_12CollisionKey_5int32(values__17, t841)
    var t842 int
    var inline1838 int = hashmap_len__HashMap_12CollisionKey_5int32(values__17)
    t842 = inline1838
    var inline1835 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t842)
    _goml_runtime_core_string_println(inline1835)
    var index__18 *ref_int32_x
    var inline1832 int32 = 0
    var inline1833 *ref_int32_x = ref__Ref_5int32(inline1832)
    index__18 = inline1833
    Loop_loop845:
    for {
        var t846 int32
        var inline1825 int32 = ref_get__Ref_5int32(index__18)
        t846 = inline1825
        var t847 bool = t846 < 2000
        if t847 {
            var t848 int32
            var inline1823 int32 = ref_get__Ref_5int32(index__18)
            t848 = inline1823
            var t849 int32 = 1000 + t848
            var key__19 CollisionKey = CollisionKey{
                value: t849,
            }
            var t850 int32
            var inline1821 int32 = ref_get__Ref_5int32(index__18)
            t850 = inline1821
            hashmap_set__HashMap_12CollisionKey_5int32(values__17, key__19, t850)
            hashmap_remove__HashMap_12CollisionKey_5int32(values__17, key__19)
            var t851 int32
            var inline1815 int32 = ref_get__Ref_5int32(index__18)
            t851 = inline1815
            var t852 int32 = t851 + 1
            ref_set__Ref_5int32(index__18, t852)
            continue
        } else {
            break Loop_loop845
        }
    }
    var t844 int
    var inline1830 int = hashmap_len__HashMap_12CollisionKey_5int32(values__17)
    t844 = inline1830
    var inline1827 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t844)
    _goml_runtime_core_string_println(inline1827)
    return struct{}{}
}

func reference_contracts() struct{} {
    var values__20 *hashmap_Ref_12CollisionKey_string_x = _goml_m_inherent_i_HashMap_i_H_h88509d3b0dfc2392ba920998ecb79184_r_____V__string()
    var t854 CollisionKey = CollisionKey{
        value: 1,
    }
    var key__21 *ref_CollisionKey_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__CollisionKey(t854)
    var t855 CollisionKey = CollisionKey{
        value: 1,
    }
    var equal_value__23 *ref_CollisionKey_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__CollisionKey(t855)
    var inline1901 string = "identity"
    hashmap_set__HashMap_18Ref_12CollisionKey_6string(values__20, key__21, inline1901)
    var t856 bool
    var inline1899 bool = ptr_eq__Ref_12CollisionKey(key__21, key__21)
    t856 = inline1899
    var inline1896 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t856)
    _goml_runtime_core_string_println(inline1896)
    var t857 bool
    var inline1894 bool = ptr_eq__Ref_12CollisionKey(key__21, equal_value__23)
    t857 = inline1894
    var inline1891 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t857)
    _goml_runtime_core_string_println(inline1891)
    var t858 uint64
    var inline1889 uint64 = ptr_hash__Ref_12CollisionKey(key__21)
    t858 = inline1889
    var t859 uint64
    var inline1887 uint64 = ptr_hash__Ref_12CollisionKey(key__21)
    t859 = inline1887
    var t860 bool = t858 == t859
    var inline1884 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t860)
    _goml_runtime_core_string_println(inline1884)
    var t861 Option__string
    var inline1882 Option__string = hashmap_get__HashMap_18Ref_12CollisionKey_6string(values__20, key__21)
    t861 = inline1882
    switch t861.(type) {
    case Option__string_None:
        println__T_string("none")
    case Option__string_Some:
        var inline1878 string = t861.(Option__string_Some)._0
        println__T_string(inline1878)
    default:
        panic("non-exhaustive match")
    }
    var t862 Option__string
    var inline1875 Option__string = hashmap_get__HashMap_18Ref_12CollisionKey_6string(values__20, equal_value__23)
    t862 = inline1875
    switch t862.(type) {
    case Option__string_None:
        println__T_string("none")
    case Option__string_Some:
        var inline1871 string = t862.(Option__string_Some)._0
        println__T_string(inline1871)
    default:
        panic("non-exhaustive match")
    }
    var t863 CollisionKey = CollisionKey{
        value: 99,
    }
    ref_set__Ref_12CollisionKey(key__21, t863)
    var t864 Option__string
    var inline1866 Option__string = hashmap_get__HashMap_18Ref_12CollisionKey_6string(values__20, key__21)
    t864 = inline1866
    switch t864.(type) {
    case Option__string_None:
        println__T_string("none")
        return struct{}{}
    case Option__string_Some:
        var inline1862 string = t864.(Option__string_Some)._0
        println__T_string(inline1862)
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

func _goml_m_trait__impl_i_PartialEq_i_float64_i_eq(self__121 float64, other__122 float64) bool {
    var t1242 bool = self__121 == other__122
    return t1242
}

func println__T_string(value__1 string) struct{} {
    var t1266 string
    t1266 = value__1
    _goml_runtime_core_string_println(t1266)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t1269 string
    var inline2405 string = _goml_runtime_core_int32_to_string(value__1)
    t1269 = inline2405
    _goml_runtime_core_string_println(t1269)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t1273 string = _goml_runtime_core_bool_to_string(self__64)
    return t1273
}

func _goml_m_inherent_i_HashMap_i_H_hda117ae4fc64297aa6806f068e6c08ed_Key____V__int32() *hashmap_CollisionKey_int32_x {
    var t1280 *hashmap_CollisionKey_int32_x = hashmap_new__HashMap_12CollisionKey_5int32()
    return t1280
}

func _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(self__264 *hashmap_CollisionKey_int32_x, key__265 CollisionKey, value__266 int32) struct{} {
    hashmap_set__HashMap_12CollisionKey_5int32(self__264, key__265, value__266)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h28011e82243b31bd5417be204086ffb6_Key____V__int32(self__267 *hashmap_CollisionKey_int32_x, key__268 CollisionKey) struct{} {
    hashmap_remove__HashMap_12CollisionKey_5int32(self__267, key__268)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_hd089129fd747b5ac2054f800f07a211e_Key____V__int32(self__262 *hashmap_CollisionKey_int32_x, key__263 CollisionKey) Option__int32 {
    var t1287 Option__int32 = hashmap_get__HashMap_12CollisionKey_5int32(self__262, key__263)
    return t1287
}

func println__T_int(value__1 int) struct{} {
    var t1289 string
    var inline2408 string = _goml_runtime_core_int_to_string(value__1)
    t1289 = inline2408
    _goml_runtime_core_string_println(t1289)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h4aea484345d58a42435067138901add4_Key____V__int32(self__269 *hashmap_CollisionKey_int32_x) int {
    var t1293 int = hashmap_len__HashMap_12CollisionKey_5int32(self__269)
    return t1293
}

func _goml_m_inherent_i_HashMap_i_H_h88509d3b0dfc2392ba920998ecb79184_r_____V__string() *hashmap_Ref_12CollisionKey_string_x {
    var t1304 *hashmap_Ref_12CollisionKey_string_x = hashmap_new__HashMap_18Ref_12CollisionKey_6string()
    return t1304
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__CollisionKey(value__273 CollisionKey) *ref_CollisionKey_x {
    var t1307 *ref_CollisionKey_x = ref__Ref_12CollisionKey(value__273)
    return t1307
}

func _goml_m_trait__impl_i_PartialEq_i_Ref_l_CollisionKey_r__i_eq(self__136 *ref_CollisionKey_x, other__137 *ref_CollisionKey_x) bool {
    var t1315 bool = ptr_eq__Ref_12CollisionKey(self__136, other__137)
    return t1315
}

func _goml_m_trait__impl_i_Hash_i_Ref_l_CollisionKey_r__i_hash(self__138 *ref_CollisionKey_x) uint64 {
    var t1318 uint64 = ptr_hash__Ref_12CollisionKey(self__138)
    return t1318
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t1328 string = _goml_runtime_core_int32_to_string(self__70)
    return t1328
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__some____T__std_p_cmp_p_Ordering(self__298 _goml_m_Option____std_p_cmp_p_Ordering) bool {
    switch self__298.(type) {
    case _goml_m_Option____std_p_cmp_p_Ordering_None:
        return false
    case _goml_m_Option____std_p_cmp_p_Ordering_Some:
        return true
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t1335 string = _goml_runtime_core_int_to_string(self__67)
    return t1335
}

func main() {
    main0()
}
