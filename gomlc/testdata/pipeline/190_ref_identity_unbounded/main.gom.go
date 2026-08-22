package main

import (
    _goml_os "os"
    _goml_reflect "reflect"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_string_from_utf8(bytes *_goml_vec_uint8) Tuple2_4bool_6string {
    return Tuple2_4bool_6string{
        _0: true,
        _1: string(bytes.items),
    }
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_os.Stdout.WriteString(s + "\n")
    return struct{}{}
}

type _goml_vec_uint8 struct {
    items []uint8
}

func vec_with_capacity__Vec_5uint8(capacity int) *_goml_vec_uint8 {
    return &_goml_vec_uint8{
        items: make([]uint8, 0, capacity),
    }
}

func vec_push__Vec_5uint8(vec *_goml_vec_uint8, elem uint8) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_5uint8(vec *_goml_vec_uint8, index int) uint8 {
    return vec.items[index]
}

func vec_len__Vec_5uint8(vec *_goml_vec_uint8) int {
    return int(len(vec.items))
}

type _goml_vec_uint32 struct {
    items []uint32
}

type ref_Node_x struct {
    value Node
}

func ref__Ref_4Node(value Node) *ref_Node_x {
    return &ref_Node_x{
        value: value,
    }
}

func ref_set__Ref_4Node(reference *ref_Node_x, value Node) struct{} {
    reference.value = value
    return struct{}{}
}

func ptr_eq__Ref_4Node(a *ref_Node_x, b *ref_Node_x) bool {
    return a == b
}

func ptr_hash__Ref_4Node(reference *ref_Node_x) uint64 {
    return uint64(_goml_reflect.ValueOf(reference).Pointer())
}

type hashmap_Ref_4Node_string_x_entry struct {
    active bool
    key *ref_Node_x
    value string
}

type hashmap_Ref_4Node_string_x struct {
    buckets map[uint64][]hashmap_Ref_4Node_string_x_entry
    hashes []uint64
    len int
}

func hashmap_new__HashMap_9Ref_4Node_6string() *hashmap_Ref_4Node_string_x {
    return &hashmap_Ref_4Node_string_x{
        buckets: make(map[uint64][]hashmap_Ref_4Node_string_x_entry),
        len: 0,
        hashes: nil,
    }
}

func hashmap_len__HashMap_9Ref_4Node_6string(m *hashmap_Ref_4Node_string_x) int {
    if m == nil {
        return 0
    }
    return m.len
}

func hashmap_lookup__HashMap_9Ref_4Node_6string(m *hashmap_Ref_4Node_string_x, key *ref_Node_x) (string, bool) {
    if m == nil {
        var zero string
        return zero, false
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_Ref_l_Node_r__i_hash(key)
    var bucket []hashmap_Ref_4Node_string_x_entry = m.buckets[h]
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_Ref_4Node_string_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_PartialEq_i_Ref_l_Node_r__i_eq(entry.key, key) {
            return entry.value, true
        }
        i = i + 1
    }
    var zero string
    return zero, false
}

func hashmap_get__HashMap_9Ref_4Node_6string(m *hashmap_Ref_4Node_string_x, key *ref_Node_x) Option__string {
    var value string
    var ok bool
    value, ok = hashmap_lookup__HashMap_9Ref_4Node_6string(m, key)
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

func hashmap_set__HashMap_9Ref_4Node_6string(m *hashmap_Ref_4Node_string_x, key *ref_Node_x, value string) struct{} {
    var reuse_index int = -1
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_Ref_l_Node_r__i_hash(key)
    var bucket []hashmap_Ref_4Node_string_x_entry = m.buckets[h]
    if len(bucket) == 0 {
        m.hashes = append(m.hashes, h)
    }
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_Ref_4Node_string_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_PartialEq_i_Ref_l_Node_r__i_eq(entry.key, key) {
            bucket[i].value = value
            return struct{}{}
        }
        if !entry.active && reuse_index < 0 {
            reuse_index = i
        }
        i = i + 1
    }
    if reuse_index >= 0 {
        bucket[reuse_index] = hashmap_Ref_4Node_string_x_entry{
            active: true,
            key: key,
            value: value,
        }
        m.len = m.len + 1
        return struct{}{}
    }
    bucket = append(bucket, hashmap_Ref_4Node_string_x_entry{
        active: true,
        key: key,
        value: value,
    })
    m.buckets[h] = bucket
    m.len = m.len + 1
    return struct{}{}
}

type Tuple2_4bool_6string struct {
    _0 bool
    _1 string
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

type Node struct {
    value int32
}

type Ordering int32

type Option__string struct {
    _tag int32
    _v1_0 string
}

func print_lookup(map__0 *hashmap_Ref_4Node_string_x, key__1 *ref_Node_x) struct{} {
    var mtmp796 Option__string
    var inline914 Option__string = hashmap_get__HashMap_9Ref_4Node_6string(map__0, key__1)
    mtmp796 = inline914
    switch mtmp796._tag {
    case 0:
        var inline907 string = "missing"
        var inline908 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline907)
        _goml_runtime_core_string_println(inline908)
        return struct{}{}
    case 1:
        var x797 string = mtmp796._v1_0
        var inline911 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x797)
        _goml_runtime_core_string_println(inline911)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t814 Node = Node{
        value: 1,
    }
    var first__3 *ref_Node_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Node(t814)
    var t815 Node = Node{
        value: 1,
    }
    var distinct__5 *ref_Node_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Node(t815)
    var t816 bool = _goml_m_trait__impl_i_PartialEq_i_Ref_l_Node_r__i_eq(first__3, first__3)
    println__T_bool(t816)
    var t817 bool = _goml_m_trait__impl_i_PartialEq_i_Ref_l_Node_r__i_eq(first__3, distinct__5)
    println__T_bool(t817)
    var t818 uint64 = _goml_m_trait__impl_i_Hash_i_Ref_l_Node_r__i_hash(first__3)
    var t819 uint64 = _goml_m_trait__impl_i_Hash_i_Ref_l_Node_r__i_hash(first__3)
    var t820 bool = t818 == t819
    var inline955 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t820)
    _goml_runtime_core_string_println(inline955)
    var map__6 *hashmap_Ref_4Node_string_x
    var inline953 *hashmap_Ref_4Node_string_x = hashmap_new__HashMap_9Ref_4Node_6string()
    map__6 = inline953
    var inline950 string = "first"
    hashmap_set__HashMap_9Ref_4Node_6string(map__6, first__3, inline950)
    print_lookup(map__6, first__3)
    var inline944 Option__string = _goml_m_inherent_i_HashMap_i_H_h9ede9e5e4b19497ae51b9af572b3f34a_r_____V__string(map__6, distinct__5)
    switch inline944._tag {
    case 0:
        println__T_string("missing")
    case 1:
        var inline946 string = inline944._v1_0
        println__T_string(inline946)
    default:
        panic("non-exhaustive match")
    }
    var t821 Node = Node{
        value: 2,
    }
    ref_set__Ref_4Node(first__3, t821)
    var inline936 Option__string = _goml_m_inherent_i_HashMap_i_H_h9ede9e5e4b19497ae51b9af572b3f34a_r_____V__string(map__6, first__3)
    switch inline936._tag {
    case 0:
        println__T_string("missing")
    case 1:
        var inline938 string = inline936._v1_0
        println__T_string(inline938)
    default:
        panic("non-exhaustive match")
    }
    var inline933 string = "distinct"
    hashmap_set__HashMap_9Ref_4Node_6string(map__6, distinct__5, inline933)
    var t822 int
    var inline931 int = hashmap_len__HashMap_9Ref_4Node_6string(map__6)
    t822 = inline931
    var inline928 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t822)
    _goml_runtime_core_string_println(inline928)
    var inline922 Option__string = _goml_m_inherent_i_HashMap_i_H_h9ede9e5e4b19497ae51b9af572b3f34a_r_____V__string(map__6, first__3)
    switch inline922._tag {
    case 0:
        println__T_string("missing")
    case 1:
        var inline924 string = inline922._v1_0
        println__T_string(inline924)
    default:
        panic("non-exhaustive match")
    }
    var inline916 Option__string = _goml_m_inherent_i_HashMap_i_H_h9ede9e5e4b19497ae51b9af572b3f34a_r_____V__string(map__6, distinct__5)
    switch inline916._tag {
    case 0:
        println__T_string("missing")
        return struct{}{}
    case 1:
        var inline918 string = inline916._v1_0
        println__T_string(inline918)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_HashMap_i_H_h9ede9e5e4b19497ae51b9af572b3f34a_r_____V__string(self__673 *hashmap_Ref_4Node_string_x, key__674 *ref_Node_x) Option__string {
    var t826 Option__string = hashmap_get__HashMap_9Ref_4Node_6string(self__673, key__674)
    return t826
}

func println__T_string(value__1 string) struct{} {
    var t828 string
    t828 = value__1
    _goml_runtime_core_string_println(t828)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Node(value__684 Node) *ref_Node_x {
    var t832 *ref_Node_x = ref__Ref_4Node(value__684)
    return t832
}

func println__T_bool(value__1 bool) struct{} {
    var t834 string
    var inline959 string = _goml_runtime_core_bool_to_string(value__1)
    t834 = inline959
    _goml_runtime_core_string_println(t834)
    return struct{}{}
}

func _goml_m_trait__impl_i_PartialEq_i_Ref_l_Node_r__i_eq(self__473 *ref_Node_x, other__474 *ref_Node_x) bool {
    var t838 bool = ptr_eq__Ref_4Node(self__473, other__474)
    return t838
}

func _goml_m_trait__impl_i_Hash_i_Ref_l_Node_r__i_hash(self__475 *ref_Node_x) uint64 {
    var t841 uint64 = ptr_hash__Ref_4Node(self__475)
    return t841
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__401 bool) string {
    var t859 string = _goml_runtime_core_bool_to_string(self__401)
    return t859
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__404 int) string {
    var inline963 int64 = int64(int(self__404))
    var inline964 string = signed_decimal_string(inline963)
    return inline964
}

func signed_decimal_string(value__214 int64) string {
    var t871 bool = value__214 < 0
    if t871 {
        var t872 uint64 = uint64(int64(value__214))
        var t873 uint64 = 0 - t872
        var t874 string = decimal_string(t873)
        var t875 string = "-" + t874
        return t875
    } else {
        var t876 uint64 = uint64(int64(value__214))
        var t877 string = decimal_string(t876)
        return t877
    }
}

func decimal_string(value__208 uint64) string {
    var t900 bool = value__208 == 0
    if t900 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop893:
        for {
            var t894 bool = remaining__210 > 0
            if t894 {
                var t895_rhs uint64 = 10
                var t895 uint64 = remaining__210 % t895_rhs
                var t896 uint8 = uint8(uint64(t895))
                var t897 uint8 = t896 + 48
                vec_push__Vec_5uint8(reversed__209, t897)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t898 uint64 = compound_old353 / compound_value354
                remaining__210 = t898
                continue
            } else {
                break Loop_loop893
            }
        }
        var t882 int
        var inline982 int = vec_len__Vec_5uint8(reversed__209)
        t882 = inline982
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t882)
        var offset__212 int = 0
        Loop_loop884:
        for {
            var t885 int
            var inline980 int = vec_len__Vec_5uint8(reversed__209)
            t885 = inline980
            var t886 bool = offset__212 < t885
            if t886 {
                var t887 int
                var inline978 int = vec_len__Vec_5uint8(reversed__209)
                t887 = inline978
                var t888 int = t887 - offset__212
                var t889 int = t888 - 1
                var t890 uint8 = vec_get__Vec_5uint8(reversed__209, t889)
                vec_push__Vec_5uint8(bytes__211, t890)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t891 int = compound_old358 + compound_value359
                offset__212 = t891
                continue
            } else {
                break Loop_loop884
            }
        }
        var mtmp362 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__211)
        var x364 string = mtmp362._1
        return x364
    }
}

func main() {
    main0()
}
