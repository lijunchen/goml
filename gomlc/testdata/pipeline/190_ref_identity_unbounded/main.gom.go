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
    var mtmp136 Option__string
    var inline214 Option__string = hashmap_get__HashMap_9Ref_4Node_6string(map__0, key__1)
    mtmp136 = inline214
    switch mtmp136.(type) {
    case None:
        var inline207 string = "missing"
        var inline208 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline207)
        _goml_runtime_core_string_println(inline208)
        return struct{}{}
    case Some:
        var x137 string = mtmp136.(Some)._0
        var inline211 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x137)
        _goml_runtime_core_string_println(inline211)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t154 Node = Node{
        value: 1,
    }
    var first__3 *ref_Node_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Node(t154)
    var t155 Node = Node{
        value: 1,
    }
    var distinct__5 *ref_Node_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Node(t155)
    var t156 bool = _goml_m_trait__impl_i_Eq_i_Ref_l_Node_r__i_eq(first__3, first__3)
    println__T_bool(t156)
    var t157 bool = _goml_m_trait__impl_i_Eq_i_Ref_l_Node_r__i_eq(first__3, distinct__5)
    println__T_bool(t157)
    var t158 uint64 = _goml_m_trait__impl_i_Hash_i_Ref_l_Node_r__i_hash(first__3)
    var t159 uint64 = _goml_m_trait__impl_i_Hash_i_Ref_l_Node_r__i_hash(first__3)
    var t160 bool = _goml_m_trait__impl_i_Eq_i_uint64_i_eq(t158, t159)
    var inline255 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t160)
    _goml_runtime_core_string_println(inline255)
    var map__6 *hashmap_Ref_4Node_string_x
    var inline253 *hashmap_Ref_4Node_string_x = hashmap_new__HashMap_9Ref_4Node_6string()
    map__6 = inline253
    var inline250 string = "first"
    hashmap_set__HashMap_9Ref_4Node_6string(map__6, first__3, inline250)
    print_lookup(map__6, first__3)
    var inline244 Option__string = _goml_m_inherent_i_HashMap_i_H_h9ede9e5e4b19497ae51b9af572b3f34a_r_____V__string(map__6, distinct__5)
    switch inline244.(type) {
    case None:
        println__T_string("missing")
    case Some:
        var inline246 string = inline244.(Some)._0
        println__T_string(inline246)
    default:
        panic("non-exhaustive match")
    }
    var t161 Node = Node{
        value: 2,
    }
    ref_set__Ref_4Node(first__3, t161)
    var inline236 Option__string = _goml_m_inherent_i_HashMap_i_H_h9ede9e5e4b19497ae51b9af572b3f34a_r_____V__string(map__6, first__3)
    switch inline236.(type) {
    case None:
        println__T_string("missing")
    case Some:
        var inline238 string = inline236.(Some)._0
        println__T_string(inline238)
    default:
        panic("non-exhaustive match")
    }
    var inline233 string = "distinct"
    hashmap_set__HashMap_9Ref_4Node_6string(map__6, distinct__5, inline233)
    var t162 int
    var inline231 int = hashmap_len__HashMap_9Ref_4Node_6string(map__6)
    t162 = inline231
    var inline228 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t162)
    _goml_runtime_core_string_println(inline228)
    var inline222 Option__string = _goml_m_inherent_i_HashMap_i_H_h9ede9e5e4b19497ae51b9af572b3f34a_r_____V__string(map__6, first__3)
    switch inline222.(type) {
    case None:
        println__T_string("missing")
    case Some:
        var inline224 string = inline222.(Some)._0
        println__T_string(inline224)
    default:
        panic("non-exhaustive match")
    }
    var inline216 Option__string = _goml_m_inherent_i_HashMap_i_H_h9ede9e5e4b19497ae51b9af572b3f34a_r_____V__string(map__6, distinct__5)
    switch inline216.(type) {
    case None:
        println__T_string("missing")
        return struct{}{}
    case Some:
        var inline218 string = inline216.(Some)._0
        println__T_string(inline218)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_HashMap_i_H_h9ede9e5e4b19497ae51b9af572b3f34a_r_____V__string(self__221 *hashmap_Ref_4Node_string_x, key__222 *ref_Node_x) Option__string {
    var t166 Option__string = hashmap_get__HashMap_9Ref_4Node_6string(self__221, key__222)
    return t166
}

func println__T_string(value__31 string) struct{} {
    var t168 string
    t168 = value__31
    _goml_runtime_core_string_println(t168)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Node(value__232 Node) *ref_Node_x {
    var t172 *ref_Node_x = ref__Ref_4Node(value__232)
    return t172
}

func println__T_bool(value__31 bool) struct{} {
    var t174 string
    var inline259 string = _goml_runtime_core_bool_to_string(value__31)
    t174 = inline259
    _goml_runtime_core_string_println(t174)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_Ref_l_Node_r__i_eq(self__140 *ref_Node_x, other__141 *ref_Node_x) bool {
    var t178 bool = ptr_eq__Ref_4Node(self__140, other__141)
    return t178
}

func _goml_m_trait__impl_i_Hash_i_Ref_l_Node_r__i_hash(self__142 *ref_Node_x) uint64 {
    var t181 uint64 = ptr_hash__Ref_4Node(self__142)
    return t181
}

func _goml_m_trait__impl_i_Eq_i_uint64_i_eq(self__119 uint64, other__120 uint64) bool {
    var t184 bool = self__119 == other__120
    return t184
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t202 string = _goml_runtime_core_bool_to_string(self__66)
    return t202
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t205 string = _goml_runtime_core_int_to_string(self__69)
    return t205
}

func main() {
    main0()
}
