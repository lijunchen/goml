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
    var mtmp187 Option__string
    var inline262 Option__string = hashmap_get__HashMap_9Ref_4Node_6string(map__0, key__1)
    mtmp187 = inline262
    switch mtmp187.(type) {
    case None:
        var inline255 string = "missing"
        var inline256 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline255)
        _goml_runtime_core_string_println(inline256)
        return struct{}{}
    case Some:
        var x188 string = mtmp187.(Some)._0
        var inline259 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x188)
        _goml_runtime_core_string_println(inline259)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t205 Node = Node{
        value: 1,
    }
    var first__3 *ref_Node_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Node(t205)
    var t206 Node = Node{
        value: 1,
    }
    var distinct__5 *ref_Node_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Node(t206)
    var t207 bool = _goml_m_trait__impl_i_PartialEq_i_Ref_l_Node_r__i_eq(first__3, first__3)
    println__T_bool(t207)
    var t208 bool = _goml_m_trait__impl_i_PartialEq_i_Ref_l_Node_r__i_eq(first__3, distinct__5)
    println__T_bool(t208)
    var t209 uint64 = _goml_m_trait__impl_i_Hash_i_Ref_l_Node_r__i_hash(first__3)
    var t210 uint64 = _goml_m_trait__impl_i_Hash_i_Ref_l_Node_r__i_hash(first__3)
    var t211 bool = t209 == t210
    var inline303 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t211)
    _goml_runtime_core_string_println(inline303)
    var map__6 *hashmap_Ref_4Node_string_x
    var inline301 *hashmap_Ref_4Node_string_x = hashmap_new__HashMap_9Ref_4Node_6string()
    map__6 = inline301
    var inline298 string = "first"
    hashmap_set__HashMap_9Ref_4Node_6string(map__6, first__3, inline298)
    print_lookup(map__6, first__3)
    var inline292 Option__string = _goml_m_inherent_i_HashMap_i_H_h9ede9e5e4b19497ae51b9af572b3f34a_r_____V__string(map__6, distinct__5)
    switch inline292.(type) {
    case None:
        println__T_string("missing")
    case Some:
        var inline294 string = inline292.(Some)._0
        println__T_string(inline294)
    default:
        panic("non-exhaustive match")
    }
    var t212 Node = Node{
        value: 2,
    }
    ref_set__Ref_4Node(first__3, t212)
    var inline284 Option__string = _goml_m_inherent_i_HashMap_i_H_h9ede9e5e4b19497ae51b9af572b3f34a_r_____V__string(map__6, first__3)
    switch inline284.(type) {
    case None:
        println__T_string("missing")
    case Some:
        var inline286 string = inline284.(Some)._0
        println__T_string(inline286)
    default:
        panic("non-exhaustive match")
    }
    var inline281 string = "distinct"
    hashmap_set__HashMap_9Ref_4Node_6string(map__6, distinct__5, inline281)
    var t213 int
    var inline279 int = hashmap_len__HashMap_9Ref_4Node_6string(map__6)
    t213 = inline279
    var inline276 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t213)
    _goml_runtime_core_string_println(inline276)
    var inline270 Option__string = _goml_m_inherent_i_HashMap_i_H_h9ede9e5e4b19497ae51b9af572b3f34a_r_____V__string(map__6, first__3)
    switch inline270.(type) {
    case None:
        println__T_string("missing")
    case Some:
        var inline272 string = inline270.(Some)._0
        println__T_string(inline272)
    default:
        panic("non-exhaustive match")
    }
    var inline264 Option__string = _goml_m_inherent_i_HashMap_i_H_h9ede9e5e4b19497ae51b9af572b3f34a_r_____V__string(map__6, distinct__5)
    switch inline264.(type) {
    case None:
        println__T_string("missing")
        return struct{}{}
    case Some:
        var inline266 string = inline264.(Some)._0
        println__T_string(inline266)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_HashMap_i_H_h9ede9e5e4b19497ae51b9af572b3f34a_r_____V__string(self__262 *hashmap_Ref_4Node_string_x, key__263 *ref_Node_x) Option__string {
    var t217 Option__string = hashmap_get__HashMap_9Ref_4Node_6string(self__262, key__263)
    return t217
}

func println__T_string(value__1 string) struct{} {
    var t219 string
    t219 = value__1
    _goml_runtime_core_string_println(t219)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Node(value__273 Node) *ref_Node_x {
    var t223 *ref_Node_x = ref__Ref_4Node(value__273)
    return t223
}

func println__T_bool(value__1 bool) struct{} {
    var t225 string
    var inline307 string = _goml_runtime_core_bool_to_string(value__1)
    t225 = inline307
    _goml_runtime_core_string_println(t225)
    return struct{}{}
}

func _goml_m_trait__impl_i_PartialEq_i_Ref_l_Node_r__i_eq(self__136 *ref_Node_x, other__137 *ref_Node_x) bool {
    var t229 bool = ptr_eq__Ref_4Node(self__136, other__137)
    return t229
}

func _goml_m_trait__impl_i_Hash_i_Ref_l_Node_r__i_hash(self__138 *ref_Node_x) uint64 {
    var t232 uint64 = ptr_hash__Ref_4Node(self__138)
    return t232
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t250 string = _goml_runtime_core_bool_to_string(self__64)
    return t250
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t253 string = _goml_runtime_core_int_to_string(self__67)
    return t253
}

func main() {
    main0()
}
