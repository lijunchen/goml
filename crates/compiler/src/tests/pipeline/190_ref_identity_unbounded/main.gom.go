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

func _goml_runtime_core_int32_to_string(x int32) string {
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
    len int32
}

func hashmap_new__HashMap_9Ref_4Node_6string() *hashmap_Ref_4Node_string_x {
    return &hashmap_Ref_4Node_string_x{
        buckets: make(map[uint64][]hashmap_Ref_4Node_string_x_entry),
        len: 0,
        hashes: nil,
    }
}

func hashmap_len__HashMap_9Ref_4Node_6string(m *hashmap_Ref_4Node_string_x) int32 {
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
    var i int32 = 0
    for {
        if i >= int32(len(bucket)) {
            break
        }
        var entry hashmap_Ref_4Node_string_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_Eq_i_Ref_l_Node_r__i_eq(entry.key, key) {
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
    var reuse_index int32 = -1
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_Ref_l_Node_r__i_hash(key)
    var bucket []hashmap_Ref_4Node_string_x_entry = m.buckets[h]
    if len(bucket) == 0 {
        m.hashes = append(m.hashes, h)
    }
    var i int32 = 0
    for {
        if i >= int32(len(bucket)) {
            break
        }
        var entry hashmap_Ref_4Node_string_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_Eq_i_Ref_l_Node_r__i_eq(entry.key, key) {
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
    var mtmp61 Option__string = _goml_m_inherent_i_HashMap_i_H_h9ede9e5e4b19497ae51b9af572b3f34a_r_____V__string(map__0, key__1)
    switch mtmp61.(type) {
    case None:
        println__T_string("missing")
    case Some:
        var x62 string = mtmp61.(Some)._0
        var value__2 string = x62
        println__T_string(value__2)
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func main0() struct{} {
    var t79 Node = Node{
        value: 1,
    }
    var first__3 *ref_Node_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Node(t79)
    var alias__4 *ref_Node_x = first__3
    var t80 Node = Node{
        value: 1,
    }
    var distinct__5 *ref_Node_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Node(t80)
    var t81 bool = _goml_m_trait__impl_i_Eq_i_Ref_l_Node_r__i_eq(first__3, alias__4)
    println__T_bool(t81)
    var t82 bool = _goml_m_trait__impl_i_Eq_i_Ref_l_Node_r__i_eq(first__3, distinct__5)
    println__T_bool(t82)
    var t83 uint64 = _goml_m_trait__impl_i_Hash_i_Ref_l_Node_r__i_hash(first__3)
    var t84 uint64 = _goml_m_trait__impl_i_Hash_i_Ref_l_Node_r__i_hash(alias__4)
    var t85 bool = t83 == t84
    println__T_bool(t85)
    var map__6 *hashmap_Ref_4Node_string_x = _goml_m_inherent_i_HashMap_i_H_hc8e3bcc6e284996b27ce02c811c77a27_r_____V__string()
    _goml_m_inherent_i_HashMap_i_H_h9275bccd902bca60ee42106df1bbeb80_r_____V__string(map__6, first__3, "first")
    print_lookup(map__6, alias__4)
    print_lookup(map__6, distinct__5)
    var t86 Node = Node{
        value: 2,
    }
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Node(first__3, t86)
    print_lookup(map__6, first__3)
    _goml_m_inherent_i_HashMap_i_H_h9275bccd902bca60ee42106df1bbeb80_r_____V__string(map__6, distinct__5, "distinct")
    var t87 int32 = _goml_m_inherent_i_HashMap_i_H_h6a27e913e4698cdf8458d88775d0807f_r_____V__string(map__6)
    println__T_int32(t87)
    print_lookup(map__6, first__3)
    print_lookup(map__6, distinct__5)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h9ede9e5e4b19497ae51b9af572b3f34a_r_____V__string(self__193 *hashmap_Ref_4Node_string_x, key__194 *ref_Node_x) Option__string {
    var retv90 Option__string
    var t91 Option__string = hashmap_get__HashMap_9Ref_4Node_6string(self__193, key__194)
    retv90 = t91
    return retv90
}

func println__T_string(value__1 string) struct{} {
    var t93 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t93)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Node(value__204 Node) *ref_Node_x {
    var retv96 *ref_Node_x
    var t97 *ref_Node_x = ref__Ref_4Node(value__204)
    retv96 = t97
    return retv96
}

func println__T_bool(value__1 bool) struct{} {
    var t99 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t99)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_Ref_l_Node_r__i_eq(self__91 *ref_Node_x, other__92 *ref_Node_x) bool {
    var retv102 bool
    var t103 bool = ptr_eq__Ref_4Node(self__91, other__92)
    retv102 = t103
    return retv102
}

func _goml_m_trait__impl_i_Hash_i_Ref_l_Node_r__i_hash(self__93 *ref_Node_x) uint64 {
    var retv105 uint64
    var t106 uint64 = ptr_hash__Ref_4Node(self__93)
    retv105 = t106
    return retv105
}

func _goml_m_inherent_i_HashMap_i_H_hc8e3bcc6e284996b27ce02c811c77a27_r_____V__string() *hashmap_Ref_4Node_string_x {
    var retv108 *hashmap_Ref_4Node_string_x
    var t109 *hashmap_Ref_4Node_string_x = hashmap_new__HashMap_9Ref_4Node_6string()
    retv108 = t109
    return retv108
}

func _goml_m_inherent_i_HashMap_i_H_h9275bccd902bca60ee42106df1bbeb80_r_____V__string(self__195 *hashmap_Ref_4Node_string_x, key__196 *ref_Node_x, value__197 string) struct{} {
    hashmap_set__HashMap_9Ref_4Node_6string(self__195, key__196, value__197)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Node(self__206 *ref_Node_x, value__207 Node) struct{} {
    ref_set__Ref_4Node(self__206, value__207)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t115 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t115)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h6a27e913e4698cdf8458d88775d0807f_r_____V__string(self__200 *hashmap_Ref_4Node_string_x) int32 {
    var retv118 int32
    var t119 int32 = hashmap_len__HashMap_9Ref_4Node_6string(self__200)
    retv118 = t119
    return retv118
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv121 string
    retv121 = self__37
    return retv121
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__36 bool) string {
    var retv123 string
    var t124 string = _goml_runtime_core_bool_to_string(self__36)
    retv123 = t124
    return retv123
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__41 int32) string {
    var retv126 string
    var t127 string = _goml_runtime_core_int32_to_string(self__41)
    retv126 = t127
    return retv126
}

func main() {
    main0()
}
