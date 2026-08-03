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
    var mtmp177 Option__string
    var inline255 Option__string = hashmap_get__HashMap_9Ref_4Node_6string(map__0, key__1)
    mtmp177 = inline255
    switch mtmp177.(type) {
    case None:
        var inline248 string = "missing"
        var inline249 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline248)
        _goml_runtime_core_string_println(inline249)
        return struct{}{}
    case Some:
        var x178 string = mtmp177.(Some)._0
        var inline252 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x178)
        _goml_runtime_core_string_println(inline252)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t195 Node = Node{
        value: 1,
    }
    var first__3 *ref_Node_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Node(t195)
    var t196 Node = Node{
        value: 1,
    }
    var distinct__5 *ref_Node_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Node(t196)
    var t197 bool = _goml_m_trait__impl_i_Eq_i_Ref_l_Node_r__i_eq(first__3, first__3)
    println__T_bool(t197)
    var t198 bool = _goml_m_trait__impl_i_Eq_i_Ref_l_Node_r__i_eq(first__3, distinct__5)
    println__T_bool(t198)
    var t199 uint64 = _goml_m_trait__impl_i_Hash_i_Ref_l_Node_r__i_hash(first__3)
    var t200 uint64 = _goml_m_trait__impl_i_Hash_i_Ref_l_Node_r__i_hash(first__3)
    var t201 bool = _goml_m_trait__impl_i_Eq_i_uint64_i_eq(t199, t200)
    var inline296 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t201)
    _goml_runtime_core_string_println(inline296)
    var map__6 *hashmap_Ref_4Node_string_x
    var inline294 *hashmap_Ref_4Node_string_x = hashmap_new__HashMap_9Ref_4Node_6string()
    map__6 = inline294
    var inline291 string = "first"
    hashmap_set__HashMap_9Ref_4Node_6string(map__6, first__3, inline291)
    print_lookup(map__6, first__3)
    var inline285 Option__string = _goml_m_inherent_i_HashMap_i_H_h9ede9e5e4b19497ae51b9af572b3f34a_r_____V__string(map__6, distinct__5)
    switch inline285.(type) {
    case None:
        println__T_string("missing")
    case Some:
        var inline287 string = inline285.(Some)._0
        println__T_string(inline287)
    default:
        panic("non-exhaustive match")
    }
    var t202 Node = Node{
        value: 2,
    }
    ref_set__Ref_4Node(first__3, t202)
    var inline277 Option__string = _goml_m_inherent_i_HashMap_i_H_h9ede9e5e4b19497ae51b9af572b3f34a_r_____V__string(map__6, first__3)
    switch inline277.(type) {
    case None:
        println__T_string("missing")
    case Some:
        var inline279 string = inline277.(Some)._0
        println__T_string(inline279)
    default:
        panic("non-exhaustive match")
    }
    var inline274 string = "distinct"
    hashmap_set__HashMap_9Ref_4Node_6string(map__6, distinct__5, inline274)
    var t203 int
    var inline272 int = hashmap_len__HashMap_9Ref_4Node_6string(map__6)
    t203 = inline272
    var inline269 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t203)
    _goml_runtime_core_string_println(inline269)
    var inline263 Option__string = _goml_m_inherent_i_HashMap_i_H_h9ede9e5e4b19497ae51b9af572b3f34a_r_____V__string(map__6, first__3)
    switch inline263.(type) {
    case None:
        println__T_string("missing")
    case Some:
        var inline265 string = inline263.(Some)._0
        println__T_string(inline265)
    default:
        panic("non-exhaustive match")
    }
    var inline257 Option__string = _goml_m_inherent_i_HashMap_i_H_h9ede9e5e4b19497ae51b9af572b3f34a_r_____V__string(map__6, distinct__5)
    switch inline257.(type) {
    case None:
        println__T_string("missing")
        return struct{}{}
    case Some:
        var inline259 string = inline257.(Some)._0
        println__T_string(inline259)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_HashMap_i_H_h9ede9e5e4b19497ae51b9af572b3f34a_r_____V__string(self__225 *hashmap_Ref_4Node_string_x, key__226 *ref_Node_x) Option__string {
    var t207 Option__string = hashmap_get__HashMap_9Ref_4Node_6string(self__225, key__226)
    return t207
}

func println__T_string(value__31 string) struct{} {
    var t209 string
    t209 = value__31
    _goml_runtime_core_string_println(t209)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Node(value__236 Node) *ref_Node_x {
    var t213 *ref_Node_x = ref__Ref_4Node(value__236)
    return t213
}

func println__T_bool(value__31 bool) struct{} {
    var t215 string
    var inline300 string = _goml_runtime_core_bool_to_string(value__31)
    t215 = inline300
    _goml_runtime_core_string_println(t215)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_Ref_l_Node_r__i_eq(self__125 *ref_Node_x, other__126 *ref_Node_x) bool {
    var t219 bool = ptr_eq__Ref_4Node(self__125, other__126)
    return t219
}

func _goml_m_trait__impl_i_Hash_i_Ref_l_Node_r__i_hash(self__127 *ref_Node_x) uint64 {
    var t222 uint64 = ptr_hash__Ref_4Node(self__127)
    return t222
}

func _goml_m_trait__impl_i_Eq_i_uint64_i_eq(self__104 uint64, other__105 uint64) bool {
    var t225 bool = self__104 == other__105
    return t225
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t243 string = _goml_runtime_core_bool_to_string(self__66)
    return t243
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t246 string = _goml_runtime_core_int_to_string(self__69)
    return t246
}

func main() {
    main0()
}
