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
    var t779 int32 = self__5.value
    var t780 int32 = other__6.value
    var t781 bool = t779 == t780
    return t781
}

func _goml_m_trait__impl_i_Hash_i_CollisionKey_i_hash(self__7 CollisionKey) uint64 {
    return 1
}

func print_opt_int(value__10 Option__int32) struct{} {
    switch value__10.(type) {
    case Option__int32_None:
        var inline1752 string = "none"
        var inline1753 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline1752)
        _goml_runtime_core_string_println(inline1753)
        return struct{}{}
    case Option__int32_Some:
        var x175 int32 = value__10.(Option__int32_Some)._0
        var inline1756 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x175)
        _goml_runtime_core_string_println(inline1756)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func float_comparison_contracts() struct{} {
    var zero32__12 float32 = 0
    var negative_zero32__13 float32 = -zero32__12
    var t793 bool = zero32__12 == negative_zero32__13
    var t794 string
    var inline1796 string = _goml_runtime_core_bool_to_string(t793)
    t794 = inline1796
    var inline1793 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t794)
    _goml_runtime_core_string_println(inline1793)
    var zero64__14 float64 = 0
    var negative_zero64__15 float64 = -zero64__14
    var t795 bool = zero64__14 == negative_zero64__15
    var t796 string
    var inline1791 string = _goml_runtime_core_bool_to_string(t795)
    t796 = inline1791
    var inline1788 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t796)
    _goml_runtime_core_string_println(inline1788)
    var t799 bool
    var inline1786 bool = _goml_m_trait__impl_i_PartialEq_i_float64_i_eq(zero64__14, negative_zero64__15)
    t799 = inline1786
    var t800 string
    var inline1782 string = _goml_runtime_core_bool_to_string(t799)
    t800 = inline1782
    var inline1779 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t800)
    _goml_runtime_core_string_println(inline1779)
    var nan__16 float64 = zero64__14 / zero64__14
    var t801 bool = nan__16 == nan__16
    var t802 string
    var inline1777 string = _goml_runtime_core_bool_to_string(t801)
    t802 = inline1777
    var inline1774 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t802)
    _goml_runtime_core_string_println(inline1774)
    var t803 _goml_m_Option____std_p_cmp_p_Ordering
    var inline1767 bool = nan__16 < nan__16
    if inline1767 {
        var inline1768 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
            _0: Less,
        }
        t803 = inline1768
    } else {
        var inline1769 bool = nan__16 > nan__16
        if inline1769 {
            var inline1770 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: Greater,
            }
            t803 = inline1770
        } else {
            var inline1771 bool = nan__16 == nan__16
            if inline1771 {
                var inline1772 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                    _0: Equal,
                }
                t803 = inline1772
            } else {
                t803 = _goml_m_Option____std_p_cmp_p_Ordering_None{}
            }
        }
    }
    var t804 bool
    var inline1764 bool = _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__some____T__std_p_cmp_p_Ordering(t803)
    var inline1765 bool = !inline1764
    t804 = inline1765
    var t805 string
    var inline1762 string = _goml_runtime_core_bool_to_string(t804)
    t805 = inline1762
    var inline1759 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t805)
    _goml_runtime_core_string_println(inline1759)
    return struct{}{}
}

func collision_contracts() struct{} {
    var values__17 *hashmap_CollisionKey_int32_x = _goml_m_inherent_i_HashMap_i_H_hda117ae4fc64297aa6806f068e6c08ed_Key____V__int32()
    var t807 CollisionKey = CollisionKey{
        value: 1,
    }
    _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(values__17, t807, 10)
    var t808 CollisionKey = CollisionKey{
        value: 2,
    }
    _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(values__17, t808, 20)
    var t809 CollisionKey = CollisionKey{
        value: 3,
    }
    _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(values__17, t809, 30)
    var t810 CollisionKey = CollisionKey{
        value: 2,
    }
    _goml_m_inherent_i_HashMap_i_H_h28011e82243b31bd5417be204086ffb6_Key____V__int32(values__17, t810)
    var t811 CollisionKey = CollisionKey{
        value: 1,
    }
    var t812 Option__int32 = _goml_m_inherent_i_HashMap_i_H_hd089129fd747b5ac2054f800f07a211e_Key____V__int32(values__17, t811)
    print_opt_int(t812)
    var t813 CollisionKey = CollisionKey{
        value: 2,
    }
    var t814 Option__int32 = _goml_m_inherent_i_HashMap_i_H_hd089129fd747b5ac2054f800f07a211e_Key____V__int32(values__17, t813)
    print_opt_int(t814)
    var t815 CollisionKey = CollisionKey{
        value: 3,
    }
    var t816 Option__int32 = _goml_m_inherent_i_HashMap_i_H_hd089129fd747b5ac2054f800f07a211e_Key____V__int32(values__17, t815)
    print_opt_int(t816)
    var t817 CollisionKey = CollisionKey{
        value: 4,
    }
    _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(values__17, t817, 40)
    var t818 int = _goml_m_inherent_i_HashMap_i_H_h4aea484345d58a42435067138901add4_Key____V__int32(values__17)
    println__T_int(t818)
    var t819 CollisionKey = CollisionKey{
        value: 4,
    }
    var t820 Option__int32
    var inline1844 Option__int32 = hashmap_get__HashMap_12CollisionKey_5int32(values__17, t819)
    t820 = inline1844
    print_opt_int(t820)
    var t821 CollisionKey = CollisionKey{
        value: 4,
    }
    var inline1841 int32 = 41
    hashmap_set__HashMap_12CollisionKey_5int32(values__17, t821, inline1841)
    var t822 int
    var inline1839 int = hashmap_len__HashMap_12CollisionKey_5int32(values__17)
    t822 = inline1839
    var inline1836 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t822)
    _goml_runtime_core_string_println(inline1836)
    var t823 CollisionKey = CollisionKey{
        value: 4,
    }
    var t824 Option__int32
    var inline1834 Option__int32 = hashmap_get__HashMap_12CollisionKey_5int32(values__17, t823)
    t824 = inline1834
    switch t824.(type) {
    case Option__int32_None:
        println__T_string("none")
    case Option__int32_Some:
        var inline1830 int32 = t824.(Option__int32_Some)._0
        println__T_int32(inline1830)
    default:
        panic("non-exhaustive match")
    }
    var t825 CollisionKey = CollisionKey{
        value: 99,
    }
    hashmap_remove__HashMap_12CollisionKey_5int32(values__17, t825)
    var t826 CollisionKey = CollisionKey{
        value: 99,
    }
    hashmap_remove__HashMap_12CollisionKey_5int32(values__17, t826)
    var t827 int
    var inline1823 int = hashmap_len__HashMap_12CollisionKey_5int32(values__17)
    t827 = inline1823
    var inline1820 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t827)
    _goml_runtime_core_string_println(inline1820)
    var index__18 *ref_int32_x
    var inline1817 int32 = 0
    var inline1818 *ref_int32_x = ref__Ref_5int32(inline1817)
    index__18 = inline1818
    Loop_loop830:
    for {
        var t831 int32
        var inline1810 int32 = ref_get__Ref_5int32(index__18)
        t831 = inline1810
        var t832 bool = t831 < 2000
        if t832 {
            var t833 int32
            var inline1808 int32 = ref_get__Ref_5int32(index__18)
            t833 = inline1808
            var t834 int32 = 1000 + t833
            var key__19 CollisionKey = CollisionKey{
                value: t834,
            }
            var t835 int32
            var inline1806 int32 = ref_get__Ref_5int32(index__18)
            t835 = inline1806
            hashmap_set__HashMap_12CollisionKey_5int32(values__17, key__19, t835)
            hashmap_remove__HashMap_12CollisionKey_5int32(values__17, key__19)
            var t836 int32
            var inline1800 int32 = ref_get__Ref_5int32(index__18)
            t836 = inline1800
            var t837 int32 = t836 + 1
            ref_set__Ref_5int32(index__18, t837)
            continue
        } else {
            break Loop_loop830
        }
    }
    var t829 int
    var inline1815 int = hashmap_len__HashMap_12CollisionKey_5int32(values__17)
    t829 = inline1815
    var inline1812 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t829)
    _goml_runtime_core_string_println(inline1812)
    return struct{}{}
}

func reference_contracts() struct{} {
    var values__20 *hashmap_Ref_12CollisionKey_string_x = _goml_m_inherent_i_HashMap_i_H_h88509d3b0dfc2392ba920998ecb79184_r_____V__string()
    var t839 CollisionKey = CollisionKey{
        value: 1,
    }
    var key__21 *ref_CollisionKey_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__CollisionKey(t839)
    var t840 CollisionKey = CollisionKey{
        value: 1,
    }
    var equal_value__23 *ref_CollisionKey_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__CollisionKey(t840)
    var inline1886 string = "identity"
    hashmap_set__HashMap_18Ref_12CollisionKey_6string(values__20, key__21, inline1886)
    var t841 bool
    var inline1884 bool = ptr_eq__Ref_12CollisionKey(key__21, key__21)
    t841 = inline1884
    var inline1881 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t841)
    _goml_runtime_core_string_println(inline1881)
    var t842 bool
    var inline1879 bool = ptr_eq__Ref_12CollisionKey(key__21, equal_value__23)
    t842 = inline1879
    var inline1876 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t842)
    _goml_runtime_core_string_println(inline1876)
    var t843 uint64
    var inline1874 uint64 = ptr_hash__Ref_12CollisionKey(key__21)
    t843 = inline1874
    var t844 uint64
    var inline1872 uint64 = ptr_hash__Ref_12CollisionKey(key__21)
    t844 = inline1872
    var t845 bool = t843 == t844
    var inline1869 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t845)
    _goml_runtime_core_string_println(inline1869)
    var t846 Option__string
    var inline1867 Option__string = hashmap_get__HashMap_18Ref_12CollisionKey_6string(values__20, key__21)
    t846 = inline1867
    switch t846.(type) {
    case Option__string_None:
        println__T_string("none")
    case Option__string_Some:
        var inline1863 string = t846.(Option__string_Some)._0
        println__T_string(inline1863)
    default:
        panic("non-exhaustive match")
    }
    var t847 Option__string
    var inline1860 Option__string = hashmap_get__HashMap_18Ref_12CollisionKey_6string(values__20, equal_value__23)
    t847 = inline1860
    switch t847.(type) {
    case Option__string_None:
        println__T_string("none")
    case Option__string_Some:
        var inline1856 string = t847.(Option__string_Some)._0
        println__T_string(inline1856)
    default:
        panic("non-exhaustive match")
    }
    var t848 CollisionKey = CollisionKey{
        value: 99,
    }
    ref_set__Ref_12CollisionKey(key__21, t848)
    var t849 Option__string
    var inline1851 Option__string = hashmap_get__HashMap_18Ref_12CollisionKey_6string(values__20, key__21)
    t849 = inline1851
    switch t849.(type) {
    case Option__string_None:
        println__T_string("none")
        return struct{}{}
    case Option__string_Some:
        var inline1847 string = t849.(Option__string_Some)._0
        println__T_string(inline1847)
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

func _goml_m_trait__impl_i_PartialEq_i_float64_i_eq(self__123 float64, other__124 float64) bool {
    var t1227 bool = self__123 == other__124
    return t1227
}

func println__T_string(value__31 string) struct{} {
    var t1251 string
    t1251 = value__31
    _goml_runtime_core_string_println(t1251)
    return struct{}{}
}

func println__T_int32(value__31 int32) struct{} {
    var t1254 string
    var inline2390 string = _goml_runtime_core_int32_to_string(value__31)
    t1254 = inline2390
    _goml_runtime_core_string_println(t1254)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t1258 string = _goml_runtime_core_bool_to_string(self__66)
    return t1258
}

func _goml_m_inherent_i_HashMap_i_H_hda117ae4fc64297aa6806f068e6c08ed_Key____V__int32() *hashmap_CollisionKey_int32_x {
    var t1265 *hashmap_CollisionKey_int32_x = hashmap_new__HashMap_12CollisionKey_5int32()
    return t1265
}

func _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(self__248 *hashmap_CollisionKey_int32_x, key__249 CollisionKey, value__250 int32) struct{} {
    hashmap_set__HashMap_12CollisionKey_5int32(self__248, key__249, value__250)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h28011e82243b31bd5417be204086ffb6_Key____V__int32(self__251 *hashmap_CollisionKey_int32_x, key__252 CollisionKey) struct{} {
    hashmap_remove__HashMap_12CollisionKey_5int32(self__251, key__252)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_hd089129fd747b5ac2054f800f07a211e_Key____V__int32(self__246 *hashmap_CollisionKey_int32_x, key__247 CollisionKey) Option__int32 {
    var t1272 Option__int32 = hashmap_get__HashMap_12CollisionKey_5int32(self__246, key__247)
    return t1272
}

func println__T_int(value__31 int) struct{} {
    var t1274 string
    var inline2394 string = _goml_runtime_core_int_to_string(value__31)
    t1274 = inline2394
    _goml_runtime_core_string_println(t1274)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h4aea484345d58a42435067138901add4_Key____V__int32(self__253 *hashmap_CollisionKey_int32_x) int {
    var t1278 int = hashmap_len__HashMap_12CollisionKey_5int32(self__253)
    return t1278
}

func _goml_m_inherent_i_HashMap_i_H_h88509d3b0dfc2392ba920998ecb79184_r_____V__string() *hashmap_Ref_12CollisionKey_string_x {
    var t1289 *hashmap_Ref_12CollisionKey_string_x = hashmap_new__HashMap_18Ref_12CollisionKey_6string()
    return t1289
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__CollisionKey(value__257 CollisionKey) *ref_CollisionKey_x {
    var t1292 *ref_CollisionKey_x = ref__Ref_12CollisionKey(value__257)
    return t1292
}

func _goml_m_trait__impl_i_PartialEq_i_Ref_l_CollisionKey_r__i_eq(self__138 *ref_CollisionKey_x, other__139 *ref_CollisionKey_x) bool {
    var t1300 bool = ptr_eq__Ref_12CollisionKey(self__138, other__139)
    return t1300
}

func _goml_m_trait__impl_i_Hash_i_Ref_l_CollisionKey_r__i_hash(self__140 *ref_CollisionKey_x) uint64 {
    var t1303 uint64 = ptr_hash__Ref_12CollisionKey(self__140)
    return t1303
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t1313 string = _goml_runtime_core_int32_to_string(self__72)
    return t1313
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__some____T__std_p_cmp_p_Ordering(self__288 _goml_m_Option____std_p_cmp_p_Ordering) bool {
    switch self__288.(type) {
    case _goml_m_Option____std_p_cmp_p_Ordering_None:
        return false
    case _goml_m_Option____std_p_cmp_p_Ordering_Some:
        return true
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t1320 string = _goml_runtime_core_int_to_string(self__69)
    return t1320
}

func main() {
    main0()
}
