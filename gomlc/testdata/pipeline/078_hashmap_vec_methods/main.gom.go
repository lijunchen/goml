package main

import (
    _goml_fmt "fmt"
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

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_int32_hash(x int32) uint64 {
    return uint64(x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type _goml_vec_int struct {
    items []int
}

func vec_new__Vec_3int() *_goml_vec_int {
    return &_goml_vec_int{
        items: nil,
    }
}

func vec_push__Vec_3int(vec *_goml_vec_int, elem int) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_3int(vec *_goml_vec_int, index int) int {
    return vec.items[index]
}

func vec_len__Vec_3int(vec *_goml_vec_int) int {
    return int(len(vec.items))
}

type hashmap_Key_int32_x_entry struct {
    active bool
    key Key
    value int32
}

type hashmap_Key_int32_x struct {
    buckets map[uint64][]hashmap_Key_int32_x_entry
    hashes []uint64
    len int
}

func hashmap_new__HashMap_3Key_5int32() *hashmap_Key_int32_x {
    return &hashmap_Key_int32_x{
        buckets: make(map[uint64][]hashmap_Key_int32_x_entry),
        len: 0,
        hashes: nil,
    }
}

func hashmap_len__HashMap_3Key_5int32(m *hashmap_Key_int32_x) int {
    if m == nil {
        return 0
    }
    return m.len
}

func hashmap_lookup__HashMap_3Key_5int32(m *hashmap_Key_int32_x, key Key) (int32, bool) {
    if m == nil {
        var zero int32
        return zero, false
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_Key_i_hash(key)
    var bucket []hashmap_Key_int32_x_entry = m.buckets[h]
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_Key_int32_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_PartialEq_i_Key_i_eq(entry.key, key) {
            return entry.value, true
        }
        i = i + 1
    }
    var zero int32
    return zero, false
}

func hashmap_get__HashMap_3Key_5int32(m *hashmap_Key_int32_x, key Key) Option__int32 {
    var value int32
    var ok bool
    value, ok = hashmap_lookup__HashMap_3Key_5int32(m, key)
    if ok {
        return Some{
            _0: value,
        }
    }
    return None{}
}

func hashmap_set__HashMap_3Key_5int32(m *hashmap_Key_int32_x, key Key, value int32) struct{} {
    var reuse_index int = -1
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_Key_i_hash(key)
    var bucket []hashmap_Key_int32_x_entry = m.buckets[h]
    if len(bucket) == 0 {
        m.hashes = append(m.hashes, h)
    }
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_Key_int32_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_PartialEq_i_Key_i_eq(entry.key, key) {
            bucket[i].value = value
            return struct{}{}
        }
        if !entry.active && reuse_index < 0 {
            reuse_index = i
        }
        i = i + 1
    }
    if reuse_index >= 0 {
        bucket[reuse_index] = hashmap_Key_int32_x_entry{
            active: true,
            key: key,
            value: value,
        }
        m.len = m.len + 1
        return struct{}{}
    }
    bucket = append(bucket, hashmap_Key_int32_x_entry{
        active: true,
        key: key,
        value: value,
    })
    m.buckets[h] = bucket
    m.len = m.len + 1
    return struct{}{}
}

func hashmap_remove__HashMap_3Key_5int32(m *hashmap_Key_int32_x, key Key) struct{} {
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_Key_i_hash(key)
    var bucket []hashmap_Key_int32_x_entry = m.buckets[h]
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_Key_int32_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_PartialEq_i_Key_i_eq(entry.key, key) {
            var zero hashmap_Key_int32_x_entry
            bucket[i] = zero
            m.len = m.len - 1
            return struct{}{}
        }
        i = i + 1
    }
    return struct{}{}
}

func hashmap_contains__HashMap_3Key_5int32(m *hashmap_Key_int32_x, key Key) bool {
    if m == nil {
        return false
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_Key_i_hash(key)
    var bucket []hashmap_Key_int32_x_entry = m.buckets[h]
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_Key_int32_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_PartialEq_i_Key_i_eq(entry.key, key) {
            return true
        }
        i = i + 1
    }
    return false
}

type Key interface {
    isKey()
}

type A struct {}

func (_ A) isKey() {}

type B struct {
    _0 int32
}

func (_ B) isKey() {}

type Option__int32 interface {
    isOption__int32()
}

type None struct {}

func (_ None) isOption__int32() {}

type Some struct {
    _0 int32
}

func (_ Some) isOption__int32() {}

func _goml_m_trait__impl_i_Hash_i_Key_i_hash(self__0 Key) uint64 {
    switch self__0.(type) {
    case A:
        var t199_source int = 0
        var t199 uint64 = uint64(int(t199_source))
        var t200 uint64 = t199 + 14695981039346656037
        var h__1 uint64 = t200 + 1
        return h__1
    case B:
        var x172 int32 = self__0.(B)._0
        var t201_source int = 0
        var t201 uint64 = uint64(int(t201_source))
        var t202 uint64 = t201 + 14695981039346656037
        var h__3 uint64 = t202 + 2
        var t203_source int = 0
        var t203 uint64 = uint64(int(t203_source))
        var t204 uint64 = t203 + 1099511628211
        var t205 uint64 = h__3 * t204
        var t206 uint64
        var inline293 uint64 = _goml_runtime_core_int32_hash(x172)
        t206 = inline293
        var h__4 uint64 = t205 + t206
        return h__4
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_PartialEq_i_Key_i_eq(self__5 Key, other__6 Key) bool {
    switch other__6.(type) {
    case A:
        switch self__5.(type) {
        case A:
            return true
        default:
            return false
        }
    case B:
        var x176 int32 = other__6.(B)._0
        switch self__5.(type) {
        case B:
            var x178 int32 = self__5.(B)._0
            var inline295 bool = x178 == x176
            return inline295
        default:
            return false
        }
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var v__11 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(v__11, 10)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(v__11, 20)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(v__11, 30)
    var t223 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(v__11, 0)
    println__T_int(t223)
    var t224 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(v__11, 1)
    println__T_int(t224)
    var t225 int
    var inline349 int = 2
    var inline350 int = vec_get__Vec_3int(v__11, inline349)
    t225 = inline350
    var inline346 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t225)
    _goml_runtime_core_string_println(inline346)
    var t226 int
    var inline344 int = vec_len__Vec_3int(v__11)
    t226 = inline344
    var inline341 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t226)
    _goml_runtime_core_string_println(inline341)
    var m__12 *hashmap_Key_int32_x
    var inline339 *hashmap_Key_int32_x = hashmap_new__HashMap_3Key_5int32()
    m__12 = inline339
    var inline336 int32 = 10
    hashmap_set__HashMap_3Key_5int32(m__12, A{}, inline336)
    var t227 Key = B{
        _0: 1,
    }
    var inline333 int32 = 20
    hashmap_set__HashMap_3Key_5int32(m__12, t227, inline333)
    var t228 int
    var inline331 int = hashmap_len__HashMap_3Key_5int32(m__12)
    t228 = inline331
    var inline328 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t228)
    _goml_runtime_core_string_println(inline328)
    var t229 Option__int32
    var inline326 Option__int32 = hashmap_get__HashMap_3Key_5int32(m__12, A{})
    t229 = inline326
    switch t229.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline322 int32 = t229.(Some)._0
        println__T_int32(inline322)
    default:
        panic("non-exhaustive match")
    }
    var t230 Key = B{
        _0: 1,
    }
    var t231 bool
    var inline319 bool = hashmap_contains__HashMap_3Key_5int32(m__12, t230)
    t231 = inline319
    var inline316 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t231)
    _goml_runtime_core_string_println(inline316)
    var t232 Key = B{
        _0: 1,
    }
    hashmap_remove__HashMap_3Key_5int32(m__12, t232)
    var t233 Key = B{
        _0: 1,
    }
    var t234 bool
    var inline312 bool = hashmap_contains__HashMap_3Key_5int32(m__12, t233)
    t234 = inline312
    var inline309 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t234)
    _goml_runtime_core_string_println(inline309)
    var t235 int
    var inline307 int = hashmap_len__HashMap_3Key_5int32(m__12)
    t235 = inline307
    var inline304 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t235)
    _goml_runtime_core_string_println(inline304)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t243 string
    t243 = value__31
    _goml_runtime_core_string_println(t243)
    return struct{}{}
}

func println__T_int32(value__31 int32) struct{} {
    var t246 string
    var inline353 string = _goml_runtime_core_int32_to_string(value__31)
    t246 = inline353
    _goml_runtime_core_string_println(t246)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int() *_goml_vec_int {
    var t250 *_goml_vec_int = vec_new__Vec_3int()
    return t250
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(self__176 *_goml_vec_int, elem__177 int) struct{} {
    vec_push__Vec_3int(self__176, elem__177)
    return struct{}{}
}

func println__T_int(value__31 int) struct{} {
    var t254 string
    var inline355 string = _goml_runtime_core_int_to_string(value__31)
    t254 = inline355
    _goml_runtime_core_string_println(t254)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(self__182 *_goml_vec_int, index__183 int) int {
    var t258 int = vec_get__Vec_3int(self__182, index__183)
    return t258
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t288 string = _goml_runtime_core_int_to_string(self__69)
    return t288
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t291 string = _goml_runtime_core_bool_to_string(self__66)
    return t291
}

func main() {
    main0()
}
