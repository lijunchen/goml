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
    var mtmp172 Option__string
    var inline247 Option__string = hashmap_get__HashMap_9Ref_4Node_6string(map__0, key__1)
    mtmp172 = inline247
    switch mtmp172.(type) {
    case None:
        var inline240 string = "missing"
        var inline241 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline240)
        _goml_runtime_core_string_println(inline241)
        return struct{}{}
    case Some:
        var x173 string = mtmp172.(Some)._0
        var inline244 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x173)
        _goml_runtime_core_string_println(inline244)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t190 Node = Node{
        value: 1,
    }
    var first__3 *ref_Node_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Node(t190)
    var t191 Node = Node{
        value: 1,
    }
    var distinct__5 *ref_Node_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Node(t191)
    var t192 bool = _goml_m_trait__impl_i_PartialEq_i_Ref_l_Node_r__i_eq(first__3, first__3)
    println__T_bool(t192)
    var t193 bool = _goml_m_trait__impl_i_PartialEq_i_Ref_l_Node_r__i_eq(first__3, distinct__5)
    println__T_bool(t193)
    var t194 uint64 = _goml_m_trait__impl_i_Hash_i_Ref_l_Node_r__i_hash(first__3)
    var t195 uint64 = _goml_m_trait__impl_i_Hash_i_Ref_l_Node_r__i_hash(first__3)
    var t196 bool = t194 == t195
    var inline288 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t196)
    _goml_runtime_core_string_println(inline288)
    var map__6 *hashmap_Ref_4Node_string_x
    var inline286 *hashmap_Ref_4Node_string_x = hashmap_new__HashMap_9Ref_4Node_6string()
    map__6 = inline286
    var inline283 string = "first"
    hashmap_set__HashMap_9Ref_4Node_6string(map__6, first__3, inline283)
    print_lookup(map__6, first__3)
    var inline277 Option__string = _goml_m_inherent_i_HashMap_i_H_h9ede9e5e4b19497ae51b9af572b3f34a_r_____V__string(map__6, distinct__5)
    switch inline277.(type) {
    case None:
        println__T_string("missing")
    case Some:
        var inline279 string = inline277.(Some)._0
        println__T_string(inline279)
    default:
        panic("non-exhaustive match")
    }
    var t197 Node = Node{
        value: 2,
    }
    ref_set__Ref_4Node(first__3, t197)
    var inline269 Option__string = _goml_m_inherent_i_HashMap_i_H_h9ede9e5e4b19497ae51b9af572b3f34a_r_____V__string(map__6, first__3)
    switch inline269.(type) {
    case None:
        println__T_string("missing")
    case Some:
        var inline271 string = inline269.(Some)._0
        println__T_string(inline271)
    default:
        panic("non-exhaustive match")
    }
    var inline266 string = "distinct"
    hashmap_set__HashMap_9Ref_4Node_6string(map__6, distinct__5, inline266)
    var t198 int
    var inline264 int = hashmap_len__HashMap_9Ref_4Node_6string(map__6)
    t198 = inline264
    var inline261 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t198)
    _goml_runtime_core_string_println(inline261)
    var inline255 Option__string = _goml_m_inherent_i_HashMap_i_H_h9ede9e5e4b19497ae51b9af572b3f34a_r_____V__string(map__6, first__3)
    switch inline255.(type) {
    case None:
        println__T_string("missing")
    case Some:
        var inline257 string = inline255.(Some)._0
        println__T_string(inline257)
    default:
        panic("non-exhaustive match")
    }
    var inline249 Option__string = _goml_m_inherent_i_HashMap_i_H_h9ede9e5e4b19497ae51b9af572b3f34a_r_____V__string(map__6, distinct__5)
    switch inline249.(type) {
    case None:
        println__T_string("missing")
        return struct{}{}
    case Some:
        var inline251 string = inline249.(Some)._0
        println__T_string(inline251)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_HashMap_i_H_h9ede9e5e4b19497ae51b9af572b3f34a_r_____V__string(self__246 *hashmap_Ref_4Node_string_x, key__247 *ref_Node_x) Option__string {
    var t202 Option__string = hashmap_get__HashMap_9Ref_4Node_6string(self__246, key__247)
    return t202
}

func println__T_string(value__31 string) struct{} {
    var t204 string
    t204 = value__31
    _goml_runtime_core_string_println(t204)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Node(value__257 Node) *ref_Node_x {
    var t208 *ref_Node_x = ref__Ref_4Node(value__257)
    return t208
}

func println__T_bool(value__31 bool) struct{} {
    var t210 string
    var inline292 string = _goml_runtime_core_bool_to_string(value__31)
    t210 = inline292
    _goml_runtime_core_string_println(t210)
    return struct{}{}
}

func _goml_m_trait__impl_i_PartialEq_i_Ref_l_Node_r__i_eq(self__138 *ref_Node_x, other__139 *ref_Node_x) bool {
    var t214 bool = ptr_eq__Ref_4Node(self__138, other__139)
    return t214
}

func _goml_m_trait__impl_i_Hash_i_Ref_l_Node_r__i_hash(self__140 *ref_Node_x) uint64 {
    var t217 uint64 = ptr_hash__Ref_4Node(self__140)
    return t217
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t235 string = _goml_runtime_core_bool_to_string(self__66)
    return t235
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t238 string = _goml_runtime_core_int_to_string(self__69)
    return t238
}

func main() {
    main0()
}
