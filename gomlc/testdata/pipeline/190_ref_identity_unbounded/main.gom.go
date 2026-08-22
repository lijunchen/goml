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

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
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

type Node struct {
    value int32
}

type Ordering int32

type Option__string struct {
    _tag int32
    _v1_0 string
}

func print_lookup(map__0 *hashmap_Ref_4Node_string_x, key__1 *ref_Node_x) struct{} {
    var mtmp411 Option__string
    var inline486 Option__string = hashmap_get__HashMap_9Ref_4Node_6string(map__0, key__1)
    mtmp411 = inline486
    switch mtmp411._tag {
    case 0:
        var inline479 string = "missing"
        var inline480 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline479)
        _goml_runtime_core_string_println(inline480)
        return struct{}{}
    case 1:
        var x412 string = mtmp411._v1_0
        var inline483 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x412)
        _goml_runtime_core_string_println(inline483)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t429 Node = Node{
        value: 1,
    }
    var first__3 *ref_Node_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Node(t429)
    var t430 Node = Node{
        value: 1,
    }
    var distinct__5 *ref_Node_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Node(t430)
    var t431 bool = _goml_m_trait__impl_i_PartialEq_i_Ref_l_Node_r__i_eq(first__3, first__3)
    println__T_bool(t431)
    var t432 bool = _goml_m_trait__impl_i_PartialEq_i_Ref_l_Node_r__i_eq(first__3, distinct__5)
    println__T_bool(t432)
    var t433 uint64 = _goml_m_trait__impl_i_Hash_i_Ref_l_Node_r__i_hash(first__3)
    var t434 uint64 = _goml_m_trait__impl_i_Hash_i_Ref_l_Node_r__i_hash(first__3)
    var t435 bool = t433 == t434
    var inline527 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t435)
    _goml_runtime_core_string_println(inline527)
    var map__6 *hashmap_Ref_4Node_string_x
    var inline525 *hashmap_Ref_4Node_string_x = hashmap_new__HashMap_9Ref_4Node_6string()
    map__6 = inline525
    var inline522 string = "first"
    hashmap_set__HashMap_9Ref_4Node_6string(map__6, first__3, inline522)
    print_lookup(map__6, first__3)
    var inline516 Option__string = _goml_m_inherent_i_HashMap_i_H_h9ede9e5e4b19497ae51b9af572b3f34a_r_____V__string(map__6, distinct__5)
    switch inline516._tag {
    case 0:
        println__T_string("missing")
    case 1:
        var inline518 string = inline516._v1_0
        println__T_string(inline518)
    default:
        panic("non-exhaustive match")
    }
    var t436 Node = Node{
        value: 2,
    }
    ref_set__Ref_4Node(first__3, t436)
    var inline508 Option__string = _goml_m_inherent_i_HashMap_i_H_h9ede9e5e4b19497ae51b9af572b3f34a_r_____V__string(map__6, first__3)
    switch inline508._tag {
    case 0:
        println__T_string("missing")
    case 1:
        var inline510 string = inline508._v1_0
        println__T_string(inline510)
    default:
        panic("non-exhaustive match")
    }
    var inline505 string = "distinct"
    hashmap_set__HashMap_9Ref_4Node_6string(map__6, distinct__5, inline505)
    var t437 int
    var inline503 int = hashmap_len__HashMap_9Ref_4Node_6string(map__6)
    t437 = inline503
    var inline500 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t437)
    _goml_runtime_core_string_println(inline500)
    var inline494 Option__string = _goml_m_inherent_i_HashMap_i_H_h9ede9e5e4b19497ae51b9af572b3f34a_r_____V__string(map__6, first__3)
    switch inline494._tag {
    case 0:
        println__T_string("missing")
    case 1:
        var inline496 string = inline494._v1_0
        println__T_string(inline496)
    default:
        panic("non-exhaustive match")
    }
    var inline488 Option__string = _goml_m_inherent_i_HashMap_i_H_h9ede9e5e4b19497ae51b9af572b3f34a_r_____V__string(map__6, distinct__5)
    switch inline488._tag {
    case 0:
        println__T_string("missing")
        return struct{}{}
    case 1:
        var inline490 string = inline488._v1_0
        println__T_string(inline490)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_HashMap_i_H_h9ede9e5e4b19497ae51b9af572b3f34a_r_____V__string(self__420 *hashmap_Ref_4Node_string_x, key__421 *ref_Node_x) Option__string {
    var t441 Option__string = hashmap_get__HashMap_9Ref_4Node_6string(self__420, key__421)
    return t441
}

func println__T_string(value__1 string) struct{} {
    var t443 string
    t443 = value__1
    _goml_runtime_core_string_println(t443)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Node(value__431 Node) *ref_Node_x {
    var t447 *ref_Node_x = ref__Ref_4Node(value__431)
    return t447
}

func println__T_bool(value__1 bool) struct{} {
    var t449 string
    var inline531 string = _goml_runtime_core_bool_to_string(value__1)
    t449 = inline531
    _goml_runtime_core_string_println(t449)
    return struct{}{}
}

func _goml_m_trait__impl_i_PartialEq_i_Ref_l_Node_r__i_eq(self__220 *ref_Node_x, other__221 *ref_Node_x) bool {
    var t453 bool = ptr_eq__Ref_4Node(self__220, other__221)
    return t453
}

func _goml_m_trait__impl_i_Hash_i_Ref_l_Node_r__i_hash(self__222 *ref_Node_x) uint64 {
    var t456 uint64 = ptr_hash__Ref_4Node(self__222)
    return t456
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t474 string = _goml_runtime_core_bool_to_string(self__148)
    return t474
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__151 int) string {
    var t477 string = _goml_runtime_core_int_to_string(self__151)
    return t477
}

func main() {
    main0()
}
