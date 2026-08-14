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
        return Some{
            _0: value,
        }
    }
    return None{}
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

type Option__string interface {
    isOption__string()
}

type None struct {}

func (_ None) isOption__string() {}

type Some struct {
    _0 string
}

func (_ Some) isOption__string() {}

func print_lookup(map__0 *hashmap_Ref_4Node_string_x, key__1 *ref_Node_x) struct{} {
    var mtmp408 Option__string
    var inline483 Option__string = hashmap_get__HashMap_9Ref_4Node_6string(map__0, key__1)
    mtmp408 = inline483
    switch mtmp408.(type) {
    case None:
        var inline476 string = "missing"
        var inline477 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline476)
        _goml_runtime_core_string_println(inline477)
        return struct{}{}
    case Some:
        var x409 string = mtmp408.(Some)._0
        var inline480 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x409)
        _goml_runtime_core_string_println(inline480)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t426 Node = Node{
        value: 1,
    }
    var first__3 *ref_Node_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Node(t426)
    var t427 Node = Node{
        value: 1,
    }
    var distinct__5 *ref_Node_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Node(t427)
    var t428 bool = _goml_m_trait__impl_i_PartialEq_i_Ref_l_Node_r__i_eq(first__3, first__3)
    println__T_bool(t428)
    var t429 bool = _goml_m_trait__impl_i_PartialEq_i_Ref_l_Node_r__i_eq(first__3, distinct__5)
    println__T_bool(t429)
    var t430 uint64 = _goml_m_trait__impl_i_Hash_i_Ref_l_Node_r__i_hash(first__3)
    var t431 uint64 = _goml_m_trait__impl_i_Hash_i_Ref_l_Node_r__i_hash(first__3)
    var t432 bool = t430 == t431
    var inline524 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t432)
    _goml_runtime_core_string_println(inline524)
    var map__6 *hashmap_Ref_4Node_string_x
    var inline522 *hashmap_Ref_4Node_string_x = hashmap_new__HashMap_9Ref_4Node_6string()
    map__6 = inline522
    var inline519 string = "first"
    hashmap_set__HashMap_9Ref_4Node_6string(map__6, first__3, inline519)
    print_lookup(map__6, first__3)
    var inline513 Option__string = _goml_m_inherent_i_HashMap_i_H_h9ede9e5e4b19497ae51b9af572b3f34a_r_____V__string(map__6, distinct__5)
    switch inline513.(type) {
    case None:
        println__T_string("missing")
    case Some:
        var inline515 string = inline513.(Some)._0
        println__T_string(inline515)
    default:
        panic("non-exhaustive match")
    }
    var t433 Node = Node{
        value: 2,
    }
    ref_set__Ref_4Node(first__3, t433)
    var inline505 Option__string = _goml_m_inherent_i_HashMap_i_H_h9ede9e5e4b19497ae51b9af572b3f34a_r_____V__string(map__6, first__3)
    switch inline505.(type) {
    case None:
        println__T_string("missing")
    case Some:
        var inline507 string = inline505.(Some)._0
        println__T_string(inline507)
    default:
        panic("non-exhaustive match")
    }
    var inline502 string = "distinct"
    hashmap_set__HashMap_9Ref_4Node_6string(map__6, distinct__5, inline502)
    var t434 int
    var inline500 int = hashmap_len__HashMap_9Ref_4Node_6string(map__6)
    t434 = inline500
    var inline497 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t434)
    _goml_runtime_core_string_println(inline497)
    var inline491 Option__string = _goml_m_inherent_i_HashMap_i_H_h9ede9e5e4b19497ae51b9af572b3f34a_r_____V__string(map__6, first__3)
    switch inline491.(type) {
    case None:
        println__T_string("missing")
    case Some:
        var inline493 string = inline491.(Some)._0
        println__T_string(inline493)
    default:
        panic("non-exhaustive match")
    }
    var inline485 Option__string = _goml_m_inherent_i_HashMap_i_H_h9ede9e5e4b19497ae51b9af572b3f34a_r_____V__string(map__6, distinct__5)
    switch inline485.(type) {
    case None:
        println__T_string("missing")
        return struct{}{}
    case Some:
        var inline487 string = inline485.(Some)._0
        println__T_string(inline487)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_HashMap_i_H_h9ede9e5e4b19497ae51b9af572b3f34a_r_____V__string(self__420 *hashmap_Ref_4Node_string_x, key__421 *ref_Node_x) Option__string {
    var t438 Option__string = hashmap_get__HashMap_9Ref_4Node_6string(self__420, key__421)
    return t438
}

func println__T_string(value__1 string) struct{} {
    var t440 string
    t440 = value__1
    _goml_runtime_core_string_println(t440)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Node(value__431 Node) *ref_Node_x {
    var t444 *ref_Node_x = ref__Ref_4Node(value__431)
    return t444
}

func println__T_bool(value__1 bool) struct{} {
    var t446 string
    var inline528 string = _goml_runtime_core_bool_to_string(value__1)
    t446 = inline528
    _goml_runtime_core_string_println(t446)
    return struct{}{}
}

func _goml_m_trait__impl_i_PartialEq_i_Ref_l_Node_r__i_eq(self__220 *ref_Node_x, other__221 *ref_Node_x) bool {
    var t450 bool = ptr_eq__Ref_4Node(self__220, other__221)
    return t450
}

func _goml_m_trait__impl_i_Hash_i_Ref_l_Node_r__i_hash(self__222 *ref_Node_x) uint64 {
    var t453 uint64 = ptr_hash__Ref_4Node(self__222)
    return t453
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t471 string = _goml_runtime_core_bool_to_string(self__148)
    return t471
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t474 string = _goml_runtime_core_int_to_string(self__151)
    return t474
}

func main() {
    main0()
}
