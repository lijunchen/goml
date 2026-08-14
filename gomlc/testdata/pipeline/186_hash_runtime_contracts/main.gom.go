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
    var t789 int32 = self__5.value
    var t790 int32 = other__6.value
    var t791 bool = t789 == t790
    return t791
}

func _goml_m_trait__impl_i_Hash_i_CollisionKey_i_hash(self__7 CollisionKey) uint64 {
    return 1
}

func print_opt_int(value__10 Option__int32) struct{} {
    switch value__10.(type) {
    case Option__int32_None:
        var inline1762 string = "none"
        var inline1763 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline1762)
        _goml_runtime_core_string_println(inline1763)
        return struct{}{}
    case Option__int32_Some:
        var x185 int32 = value__10.(Option__int32_Some)._0
        var inline1766 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x185)
        _goml_runtime_core_string_println(inline1766)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func float_comparison_contracts() struct{} {
    var zero32__12 float32 = 0
    var negative_zero32__13 float32 = -zero32__12
    var t803 bool = zero32__12 == negative_zero32__13
    var t804 string
    var inline1806 string = _goml_runtime_core_bool_to_string(t803)
    t804 = inline1806
    var inline1803 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t804)
    _goml_runtime_core_string_println(inline1803)
    var zero64__14 float64 = 0
    var negative_zero64__15 float64 = -zero64__14
    var t805 bool = zero64__14 == negative_zero64__15
    var t806 string
    var inline1801 string = _goml_runtime_core_bool_to_string(t805)
    t806 = inline1801
    var inline1798 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t806)
    _goml_runtime_core_string_println(inline1798)
    var t809 bool
    var inline1796 bool = _goml_m_trait__impl_i_PartialEq_i_float64_i_eq(zero64__14, negative_zero64__15)
    t809 = inline1796
    var t810 string
    var inline1792 string = _goml_runtime_core_bool_to_string(t809)
    t810 = inline1792
    var inline1789 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t810)
    _goml_runtime_core_string_println(inline1789)
    var nan__16 float64 = zero64__14 / zero64__14
    var t811 bool = nan__16 == nan__16
    var t812 string
    var inline1787 string = _goml_runtime_core_bool_to_string(t811)
    t812 = inline1787
    var inline1784 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t812)
    _goml_runtime_core_string_println(inline1784)
    var t813 _goml_m_Option____std_p_cmp_p_Ordering
    var inline1777 bool = nan__16 < nan__16
    if inline1777 {
        var inline1778 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
            _0: Less,
        }
        t813 = inline1778
    } else {
        var inline1779 bool = nan__16 > nan__16
        if inline1779 {
            var inline1780 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                _0: Greater,
            }
            t813 = inline1780
        } else {
            var inline1781 bool = nan__16 == nan__16
            if inline1781 {
                var inline1782 _goml_m_Option____std_p_cmp_p_Ordering = _goml_m_Option____std_p_cmp_p_Ordering_Some{
                    _0: Equal,
                }
                t813 = inline1782
            } else {
                t813 = _goml_m_Option____std_p_cmp_p_Ordering_None{}
            }
        }
    }
    var t814 bool
    var inline1774 bool = _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__some____T__std_p_cmp_p_Ordering(t813)
    var inline1775 bool = !inline1774
    t814 = inline1775
    var t815 string
    var inline1772 string = _goml_runtime_core_bool_to_string(t814)
    t815 = inline1772
    var inline1769 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t815)
    _goml_runtime_core_string_println(inline1769)
    return struct{}{}
}

func collision_contracts() struct{} {
    var values__17 *hashmap_CollisionKey_int32_x = _goml_m_inherent_i_HashMap_i_H_hda117ae4fc64297aa6806f068e6c08ed_Key____V__int32()
    var t817 CollisionKey = CollisionKey{
        value: 1,
    }
    _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(values__17, t817, 10)
    var t818 CollisionKey = CollisionKey{
        value: 2,
    }
    _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(values__17, t818, 20)
    var t819 CollisionKey = CollisionKey{
        value: 3,
    }
    _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(values__17, t819, 30)
    var t820 CollisionKey = CollisionKey{
        value: 2,
    }
    _goml_m_inherent_i_HashMap_i_H_h28011e82243b31bd5417be204086ffb6_Key____V__int32(values__17, t820)
    var t821 CollisionKey = CollisionKey{
        value: 1,
    }
    var t822 Option__int32 = _goml_m_inherent_i_HashMap_i_H_hd089129fd747b5ac2054f800f07a211e_Key____V__int32(values__17, t821)
    print_opt_int(t822)
    var t823 CollisionKey = CollisionKey{
        value: 2,
    }
    var t824 Option__int32 = _goml_m_inherent_i_HashMap_i_H_hd089129fd747b5ac2054f800f07a211e_Key____V__int32(values__17, t823)
    print_opt_int(t824)
    var t825 CollisionKey = CollisionKey{
        value: 3,
    }
    var t826 Option__int32 = _goml_m_inherent_i_HashMap_i_H_hd089129fd747b5ac2054f800f07a211e_Key____V__int32(values__17, t825)
    print_opt_int(t826)
    var t827 CollisionKey = CollisionKey{
        value: 4,
    }
    _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(values__17, t827, 40)
    var t828 int = _goml_m_inherent_i_HashMap_i_H_h4aea484345d58a42435067138901add4_Key____V__int32(values__17)
    println__T_int(t828)
    var t829 CollisionKey = CollisionKey{
        value: 4,
    }
    var t830 Option__int32
    var inline1854 Option__int32 = hashmap_get__HashMap_12CollisionKey_5int32(values__17, t829)
    t830 = inline1854
    print_opt_int(t830)
    var t831 CollisionKey = CollisionKey{
        value: 4,
    }
    var inline1851 int32 = 41
    hashmap_set__HashMap_12CollisionKey_5int32(values__17, t831, inline1851)
    var t832 int
    var inline1849 int = hashmap_len__HashMap_12CollisionKey_5int32(values__17)
    t832 = inline1849
    var inline1846 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t832)
    _goml_runtime_core_string_println(inline1846)
    var t833 CollisionKey = CollisionKey{
        value: 4,
    }
    var t834 Option__int32
    var inline1844 Option__int32 = hashmap_get__HashMap_12CollisionKey_5int32(values__17, t833)
    t834 = inline1844
    switch t834.(type) {
    case Option__int32_None:
        println__T_string("none")
    case Option__int32_Some:
        var inline1840 int32 = t834.(Option__int32_Some)._0
        println__T_int32(inline1840)
    default:
        panic("non-exhaustive match")
    }
    var t835 CollisionKey = CollisionKey{
        value: 99,
    }
    hashmap_remove__HashMap_12CollisionKey_5int32(values__17, t835)
    var t836 CollisionKey = CollisionKey{
        value: 99,
    }
    hashmap_remove__HashMap_12CollisionKey_5int32(values__17, t836)
    var t837 int
    var inline1833 int = hashmap_len__HashMap_12CollisionKey_5int32(values__17)
    t837 = inline1833
    var inline1830 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t837)
    _goml_runtime_core_string_println(inline1830)
    var index__18 *ref_int32_x
    var inline1827 int32 = 0
    var inline1828 *ref_int32_x = ref__Ref_5int32(inline1827)
    index__18 = inline1828
    Loop_loop840:
    for {
        var t841 int32
        var inline1820 int32 = ref_get__Ref_5int32(index__18)
        t841 = inline1820
        var t842 bool = t841 < 2000
        if t842 {
            var t843 int32
            var inline1818 int32 = ref_get__Ref_5int32(index__18)
            t843 = inline1818
            var t844 int32 = 1000 + t843
            var key__19 CollisionKey = CollisionKey{
                value: t844,
            }
            var t845 int32
            var inline1816 int32 = ref_get__Ref_5int32(index__18)
            t845 = inline1816
            hashmap_set__HashMap_12CollisionKey_5int32(values__17, key__19, t845)
            hashmap_remove__HashMap_12CollisionKey_5int32(values__17, key__19)
            var t846 int32
            var inline1810 int32 = ref_get__Ref_5int32(index__18)
            t846 = inline1810
            var t847 int32 = t846 + 1
            ref_set__Ref_5int32(index__18, t847)
            continue
        } else {
            break Loop_loop840
        }
    }
    var t839 int
    var inline1825 int = hashmap_len__HashMap_12CollisionKey_5int32(values__17)
    t839 = inline1825
    var inline1822 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t839)
    _goml_runtime_core_string_println(inline1822)
    return struct{}{}
}

func reference_contracts() struct{} {
    var values__20 *hashmap_Ref_12CollisionKey_string_x = _goml_m_inherent_i_HashMap_i_H_h88509d3b0dfc2392ba920998ecb79184_r_____V__string()
    var t849 CollisionKey = CollisionKey{
        value: 1,
    }
    var key__21 *ref_CollisionKey_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__CollisionKey(t849)
    var t850 CollisionKey = CollisionKey{
        value: 1,
    }
    var equal_value__23 *ref_CollisionKey_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__CollisionKey(t850)
    var inline1896 string = "identity"
    hashmap_set__HashMap_18Ref_12CollisionKey_6string(values__20, key__21, inline1896)
    var t851 bool
    var inline1894 bool = ptr_eq__Ref_12CollisionKey(key__21, key__21)
    t851 = inline1894
    var inline1891 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t851)
    _goml_runtime_core_string_println(inline1891)
    var t852 bool
    var inline1889 bool = ptr_eq__Ref_12CollisionKey(key__21, equal_value__23)
    t852 = inline1889
    var inline1886 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t852)
    _goml_runtime_core_string_println(inline1886)
    var t853 uint64
    var inline1884 uint64 = ptr_hash__Ref_12CollisionKey(key__21)
    t853 = inline1884
    var t854 uint64
    var inline1882 uint64 = ptr_hash__Ref_12CollisionKey(key__21)
    t854 = inline1882
    var t855 bool = t853 == t854
    var inline1879 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t855)
    _goml_runtime_core_string_println(inline1879)
    var t856 Option__string
    var inline1877 Option__string = hashmap_get__HashMap_18Ref_12CollisionKey_6string(values__20, key__21)
    t856 = inline1877
    switch t856.(type) {
    case Option__string_None:
        println__T_string("none")
    case Option__string_Some:
        var inline1873 string = t856.(Option__string_Some)._0
        println__T_string(inline1873)
    default:
        panic("non-exhaustive match")
    }
    var t857 Option__string
    var inline1870 Option__string = hashmap_get__HashMap_18Ref_12CollisionKey_6string(values__20, equal_value__23)
    t857 = inline1870
    switch t857.(type) {
    case Option__string_None:
        println__T_string("none")
    case Option__string_Some:
        var inline1866 string = t857.(Option__string_Some)._0
        println__T_string(inline1866)
    default:
        panic("non-exhaustive match")
    }
    var t858 CollisionKey = CollisionKey{
        value: 99,
    }
    ref_set__Ref_12CollisionKey(key__21, t858)
    var t859 Option__string
    var inline1861 Option__string = hashmap_get__HashMap_18Ref_12CollisionKey_6string(values__20, key__21)
    t859 = inline1861
    switch t859.(type) {
    case Option__string_None:
        println__T_string("none")
        return struct{}{}
    case Option__string_Some:
        var inline1857 string = t859.(Option__string_Some)._0
        println__T_string(inline1857)
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
    var t1237 bool = self__121 == other__122
    return t1237
}

func println__T_string(value__1 string) struct{} {
    var t1261 string
    t1261 = value__1
    _goml_runtime_core_string_println(t1261)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t1264 string
    var inline2400 string = _goml_runtime_core_int32_to_string(value__1)
    t1264 = inline2400
    _goml_runtime_core_string_println(t1264)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t1268 string = _goml_runtime_core_bool_to_string(self__64)
    return t1268
}

func _goml_m_inherent_i_HashMap_i_H_hda117ae4fc64297aa6806f068e6c08ed_Key____V__int32() *hashmap_CollisionKey_int32_x {
    var t1275 *hashmap_CollisionKey_int32_x = hashmap_new__HashMap_12CollisionKey_5int32()
    return t1275
}

func _goml_m_inherent_i_HashMap_i_H_h16eb43877a9769652cbdd61c4f534eec_Key____V__int32(self__261 *hashmap_CollisionKey_int32_x, key__262 CollisionKey, value__263 int32) struct{} {
    hashmap_set__HashMap_12CollisionKey_5int32(self__261, key__262, value__263)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h28011e82243b31bd5417be204086ffb6_Key____V__int32(self__264 *hashmap_CollisionKey_int32_x, key__265 CollisionKey) struct{} {
    hashmap_remove__HashMap_12CollisionKey_5int32(self__264, key__265)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_hd089129fd747b5ac2054f800f07a211e_Key____V__int32(self__259 *hashmap_CollisionKey_int32_x, key__260 CollisionKey) Option__int32 {
    var t1282 Option__int32 = hashmap_get__HashMap_12CollisionKey_5int32(self__259, key__260)
    return t1282
}

func println__T_int(value__1 int) struct{} {
    var t1284 string
    var inline2403 string = _goml_runtime_core_int_to_string(value__1)
    t1284 = inline2403
    _goml_runtime_core_string_println(t1284)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h4aea484345d58a42435067138901add4_Key____V__int32(self__266 *hashmap_CollisionKey_int32_x) int {
    var t1288 int = hashmap_len__HashMap_12CollisionKey_5int32(self__266)
    return t1288
}

func _goml_m_inherent_i_HashMap_i_H_h88509d3b0dfc2392ba920998ecb79184_r_____V__string() *hashmap_Ref_12CollisionKey_string_x {
    var t1299 *hashmap_Ref_12CollisionKey_string_x = hashmap_new__HashMap_18Ref_12CollisionKey_6string()
    return t1299
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__CollisionKey(value__270 CollisionKey) *ref_CollisionKey_x {
    var t1302 *ref_CollisionKey_x = ref__Ref_12CollisionKey(value__270)
    return t1302
}

func _goml_m_trait__impl_i_PartialEq_i_Ref_l_CollisionKey_r__i_eq(self__136 *ref_CollisionKey_x, other__137 *ref_CollisionKey_x) bool {
    var t1310 bool = ptr_eq__Ref_12CollisionKey(self__136, other__137)
    return t1310
}

func _goml_m_trait__impl_i_Hash_i_Ref_l_CollisionKey_r__i_hash(self__138 *ref_CollisionKey_x) uint64 {
    var t1313 uint64 = ptr_hash__Ref_12CollisionKey(self__138)
    return t1313
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t1323 string = _goml_runtime_core_int32_to_string(self__70)
    return t1323
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_is__some____T__std_p_cmp_p_Ordering(self__295 _goml_m_Option____std_p_cmp_p_Ordering) bool {
    switch self__295.(type) {
    case _goml_m_Option____std_p_cmp_p_Ordering_None:
        return false
    case _goml_m_Option____std_p_cmp_p_Ordering_Some:
        return true
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t1330 string = _goml_runtime_core_int_to_string(self__67)
    return t1330
}

func main() {
    main0()
}
