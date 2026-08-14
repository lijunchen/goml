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
    var mtmp182 Option__string
    var inline257 Option__string = hashmap_get__HashMap_9Ref_4Node_6string(map__0, key__1)
    mtmp182 = inline257
    switch mtmp182.(type) {
    case None:
        var inline250 string = "missing"
        var inline251 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline250)
        _goml_runtime_core_string_println(inline251)
        return struct{}{}
    case Some:
        var x183 string = mtmp182.(Some)._0
        var inline254 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x183)
        _goml_runtime_core_string_println(inline254)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t200 Node = Node{
        value: 1,
    }
    var first__3 *ref_Node_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Node(t200)
    var t201 Node = Node{
        value: 1,
    }
    var distinct__5 *ref_Node_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Node(t201)
    var t202 bool = _goml_m_trait__impl_i_PartialEq_i_Ref_l_Node_r__i_eq(first__3, first__3)
    println__T_bool(t202)
    var t203 bool = _goml_m_trait__impl_i_PartialEq_i_Ref_l_Node_r__i_eq(first__3, distinct__5)
    println__T_bool(t203)
    var t204 uint64 = _goml_m_trait__impl_i_Hash_i_Ref_l_Node_r__i_hash(first__3)
    var t205 uint64 = _goml_m_trait__impl_i_Hash_i_Ref_l_Node_r__i_hash(first__3)
    var t206 bool = t204 == t205
    var inline298 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t206)
    _goml_runtime_core_string_println(inline298)
    var map__6 *hashmap_Ref_4Node_string_x
    var inline296 *hashmap_Ref_4Node_string_x = hashmap_new__HashMap_9Ref_4Node_6string()
    map__6 = inline296
    var inline293 string = "first"
    hashmap_set__HashMap_9Ref_4Node_6string(map__6, first__3, inline293)
    print_lookup(map__6, first__3)
    var inline287 Option__string = _goml_m_inherent_i_HashMap_i_H_h9ede9e5e4b19497ae51b9af572b3f34a_r_____V__string(map__6, distinct__5)
    switch inline287.(type) {
    case None:
        println__T_string("missing")
    case Some:
        var inline289 string = inline287.(Some)._0
        println__T_string(inline289)
    default:
        panic("non-exhaustive match")
    }
    var t207 Node = Node{
        value: 2,
    }
    ref_set__Ref_4Node(first__3, t207)
    var inline279 Option__string = _goml_m_inherent_i_HashMap_i_H_h9ede9e5e4b19497ae51b9af572b3f34a_r_____V__string(map__6, first__3)
    switch inline279.(type) {
    case None:
        println__T_string("missing")
    case Some:
        var inline281 string = inline279.(Some)._0
        println__T_string(inline281)
    default:
        panic("non-exhaustive match")
    }
    var inline276 string = "distinct"
    hashmap_set__HashMap_9Ref_4Node_6string(map__6, distinct__5, inline276)
    var t208 int
    var inline274 int = hashmap_len__HashMap_9Ref_4Node_6string(map__6)
    t208 = inline274
    var inline271 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t208)
    _goml_runtime_core_string_println(inline271)
    var inline265 Option__string = _goml_m_inherent_i_HashMap_i_H_h9ede9e5e4b19497ae51b9af572b3f34a_r_____V__string(map__6, first__3)
    switch inline265.(type) {
    case None:
        println__T_string("missing")
    case Some:
        var inline267 string = inline265.(Some)._0
        println__T_string(inline267)
    default:
        panic("non-exhaustive match")
    }
    var inline259 Option__string = _goml_m_inherent_i_HashMap_i_H_h9ede9e5e4b19497ae51b9af572b3f34a_r_____V__string(map__6, distinct__5)
    switch inline259.(type) {
    case None:
        println__T_string("missing")
        return struct{}{}
    case Some:
        var inline261 string = inline259.(Some)._0
        println__T_string(inline261)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_HashMap_i_H_h9ede9e5e4b19497ae51b9af572b3f34a_r_____V__string(self__259 *hashmap_Ref_4Node_string_x, key__260 *ref_Node_x) Option__string {
    var t212 Option__string = hashmap_get__HashMap_9Ref_4Node_6string(self__259, key__260)
    return t212
}

func println__T_string(value__1 string) struct{} {
    var t214 string
    t214 = value__1
    _goml_runtime_core_string_println(t214)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Node(value__270 Node) *ref_Node_x {
    var t218 *ref_Node_x = ref__Ref_4Node(value__270)
    return t218
}

func println__T_bool(value__1 bool) struct{} {
    var t220 string
    var inline302 string = _goml_runtime_core_bool_to_string(value__1)
    t220 = inline302
    _goml_runtime_core_string_println(t220)
    return struct{}{}
}

func _goml_m_trait__impl_i_PartialEq_i_Ref_l_Node_r__i_eq(self__136 *ref_Node_x, other__137 *ref_Node_x) bool {
    var t224 bool = ptr_eq__Ref_4Node(self__136, other__137)
    return t224
}

func _goml_m_trait__impl_i_Hash_i_Ref_l_Node_r__i_hash(self__138 *ref_Node_x) uint64 {
    var t227 uint64 = ptr_hash__Ref_4Node(self__138)
    return t227
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t245 string = _goml_runtime_core_bool_to_string(self__64)
    return t245
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t248 string = _goml_runtime_core_int_to_string(self__67)
    return t248
}

func main() {
    main0()
}
