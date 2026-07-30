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
    var mtmp68 Option__string = _goml_m_inherent_i_HashMap_i_H_h9ede9e5e4b19497ae51b9af572b3f34a_r_____V__string(map__0, key__1)
    switch mtmp68.(type) {
    case None:
        println__T_string("missing")
    case Some:
        var x69 string = mtmp68.(Some)._0
        var value__2 string = x69
        println__T_string(value__2)
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func main0() struct{} {
    var t86 Node = Node{
        value: 1,
    }
    var first__3 *ref_Node_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Node(t86)
    var alias__4 *ref_Node_x = first__3
    var t87 Node = Node{
        value: 1,
    }
    var distinct__5 *ref_Node_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Node(t87)
    var t88 bool = _goml_m_trait__impl_i_Eq_i_Ref_l_Node_r__i_eq(first__3, alias__4)
    println__T_bool(t88)
    var t89 bool = _goml_m_trait__impl_i_Eq_i_Ref_l_Node_r__i_eq(first__3, distinct__5)
    println__T_bool(t89)
    var t90 uint64 = _goml_m_trait__impl_i_Hash_i_Ref_l_Node_r__i_hash(first__3)
    var t91 uint64 = _goml_m_trait__impl_i_Hash_i_Ref_l_Node_r__i_hash(alias__4)
    var t92 bool = _goml_m_trait__impl_i_Eq_i_uint64_i_eq(t90, t91)
    println__T_bool(t92)
    var map__6 *hashmap_Ref_4Node_string_x = _goml_m_inherent_i_HashMap_i_H_hc8e3bcc6e284996b27ce02c811c77a27_r_____V__string()
    _goml_m_inherent_i_HashMap_i_H_h9275bccd902bca60ee42106df1bbeb80_r_____V__string(map__6, first__3, "first")
    print_lookup(map__6, alias__4)
    print_lookup(map__6, distinct__5)
    var t93 Node = Node{
        value: 2,
    }
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Node(first__3, t93)
    print_lookup(map__6, first__3)
    _goml_m_inherent_i_HashMap_i_H_h9275bccd902bca60ee42106df1bbeb80_r_____V__string(map__6, distinct__5, "distinct")
    var t94 int = _goml_m_inherent_i_HashMap_i_H_h6a27e913e4698cdf8458d88775d0807f_r_____V__string(map__6)
    println__T_int(t94)
    print_lookup(map__6, first__3)
    print_lookup(map__6, distinct__5)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h9ede9e5e4b19497ae51b9af572b3f34a_r_____V__string(self__196 *hashmap_Ref_4Node_string_x, key__197 *ref_Node_x) Option__string {
    var retv97 Option__string
    var t98 Option__string = hashmap_get__HashMap_9Ref_4Node_6string(self__196, key__197)
    retv97 = t98
    return retv97
}

func println__T_string(value__1 string) struct{} {
    var t100 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t100)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Node(value__207 Node) *ref_Node_x {
    var retv103 *ref_Node_x
    var t104 *ref_Node_x = ref__Ref_4Node(value__207)
    retv103 = t104
    return retv103
}

func println__T_bool(value__1 bool) struct{} {
    var t106 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t106)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_Ref_l_Node_r__i_eq(self__96 *ref_Node_x, other__97 *ref_Node_x) bool {
    var retv109 bool
    var t110 bool = ptr_eq__Ref_4Node(self__96, other__97)
    retv109 = t110
    return retv109
}

func _goml_m_trait__impl_i_Hash_i_Ref_l_Node_r__i_hash(self__98 *ref_Node_x) uint64 {
    var retv112 uint64
    var t113 uint64 = ptr_hash__Ref_4Node(self__98)
    retv112 = t113
    return retv112
}

func _goml_m_trait__impl_i_Eq_i_uint64_i_eq(self__75 uint64, other__76 uint64) bool {
    var retv115 bool
    var t116 bool = self__75 == other__76
    retv115 = t116
    return retv115
}

func _goml_m_inherent_i_HashMap_i_H_hc8e3bcc6e284996b27ce02c811c77a27_r_____V__string() *hashmap_Ref_4Node_string_x {
    var retv118 *hashmap_Ref_4Node_string_x
    var t119 *hashmap_Ref_4Node_string_x = hashmap_new__HashMap_9Ref_4Node_6string()
    retv118 = t119
    return retv118
}

func _goml_m_inherent_i_HashMap_i_H_h9275bccd902bca60ee42106df1bbeb80_r_____V__string(self__198 *hashmap_Ref_4Node_string_x, key__199 *ref_Node_x, value__200 string) struct{} {
    hashmap_set__HashMap_9Ref_4Node_6string(self__198, key__199, value__200)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Node(self__209 *ref_Node_x, value__210 Node) struct{} {
    ref_set__Ref_4Node(self__209, value__210)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t125 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t125)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h6a27e913e4698cdf8458d88775d0807f_r_____V__string(self__203 *hashmap_Ref_4Node_string_x) int {
    var retv128 int
    var t129 int = hashmap_len__HashMap_9Ref_4Node_6string(self__203)
    retv128 = t129
    return retv128
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv131 string
    retv131 = self__38
    return retv131
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv133 string
    var t134 string = _goml_runtime_core_bool_to_string(self__37)
    retv133 = t134
    return retv133
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv136 string
    var t137 string = _goml_runtime_core_int_to_string(self__40)
    retv136 = t137
    return retv136
}

func main() {
    main0()
}
