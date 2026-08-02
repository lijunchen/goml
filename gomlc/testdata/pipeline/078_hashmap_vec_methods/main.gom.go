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
        if entry.active && _goml_m_trait__impl_i_Eq_i_Key_i_eq(entry.key, key) {
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
        if entry.active && _goml_m_trait__impl_i_Eq_i_Key_i_eq(entry.key, key) {
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
        if entry.active && _goml_m_trait__impl_i_Eq_i_Key_i_eq(entry.key, key) {
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
        if entry.active && _goml_m_trait__impl_i_Eq_i_Key_i_eq(entry.key, key) {
            return true
        }
        i = i + 1
    }
    return false
}

type Tuple2_3Key_3Key struct {
    _0 Key
    _1 Key
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

func _goml_m_trait__impl_i_Eq_i_Key_i_eq(self__0 Key, other__1 Key) bool {
    switch other__1.(type) {
    case A:
        switch self__0.(type) {
        case A:
            return true
        default:
            return false
        }
    case B:
        var x158 int32 = other__1.(B)._0
        switch self__0.(type) {
        case B:
            var x160 int32 = self__0.(B)._0
            var inline270 bool = x160 == x158
            return inline270
        default:
            return false
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_Hash_i_Key_i_hash(self__4 Key) uint64 {
    switch self__4.(type) {
    case A:
        var h__5 uint64 = 14695981039346656037 + 1
        return h__5
    case B:
        var x161 int32 = self__4.(B)._0
        var h__7 uint64 = 14695981039346656037 + 2
        var t193 uint64 = h__7 * 1099511628211
        var t194 uint64
        var inline272 uint64 = _goml_runtime_core_int32_hash(x161)
        t194 = inline272
        var h__8 uint64 = t193 + t194
        return h__8
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var v__11 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(v__11, 10)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(v__11, 20)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(v__11, 30)
    var t200 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(v__11, 0)
    println__T_int(t200)
    var t201 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(v__11, 1)
    println__T_int(t201)
    var t202 int
    var inline326 int = 2
    var inline327 int = vec_get__Vec_3int(v__11, inline326)
    t202 = inline327
    var inline323 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t202)
    _goml_runtime_core_string_println(inline323)
    var t203 int
    var inline321 int = vec_len__Vec_3int(v__11)
    t203 = inline321
    var inline318 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t203)
    _goml_runtime_core_string_println(inline318)
    var m__12 *hashmap_Key_int32_x
    var inline316 *hashmap_Key_int32_x = hashmap_new__HashMap_3Key_5int32()
    m__12 = inline316
    var inline313 int32 = 10
    hashmap_set__HashMap_3Key_5int32(m__12, A{}, inline313)
    var t204 Key = B{
        _0: 1,
    }
    var inline310 int32 = 20
    hashmap_set__HashMap_3Key_5int32(m__12, t204, inline310)
    var t205 int
    var inline308 int = hashmap_len__HashMap_3Key_5int32(m__12)
    t205 = inline308
    var inline305 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t205)
    _goml_runtime_core_string_println(inline305)
    var t206 Option__int32
    var inline303 Option__int32 = hashmap_get__HashMap_3Key_5int32(m__12, A{})
    t206 = inline303
    switch t206.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline299 int32 = t206.(Some)._0
        println__T_int32(inline299)
    default:
        panic("non-exhaustive match")
    }
    var t207 Key = B{
        _0: 1,
    }
    var t208 bool
    var inline296 bool = hashmap_contains__HashMap_3Key_5int32(m__12, t207)
    t208 = inline296
    var inline293 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t208)
    _goml_runtime_core_string_println(inline293)
    var t209 Key = B{
        _0: 1,
    }
    hashmap_remove__HashMap_3Key_5int32(m__12, t209)
    var t210 Key = B{
        _0: 1,
    }
    var t211 bool
    var inline289 bool = hashmap_contains__HashMap_3Key_5int32(m__12, t210)
    t211 = inline289
    var inline286 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t211)
    _goml_runtime_core_string_println(inline286)
    var t212 int
    var inline284 int = hashmap_len__HashMap_3Key_5int32(m__12)
    t212 = inline284
    var inline281 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t212)
    _goml_runtime_core_string_println(inline281)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t220 string
    t220 = value__1
    _goml_runtime_core_string_println(t220)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t223 string
    var inline330 string = _goml_runtime_core_int32_to_string(value__1)
    t223 = inline330
    _goml_runtime_core_string_println(t223)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int() *_goml_vec_int {
    var t227 *_goml_vec_int = vec_new__Vec_3int()
    return t227
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(self__126 *_goml_vec_int, elem__127 int) struct{} {
    vec_push__Vec_3int(self__126, elem__127)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t231 string
    var inline332 string = _goml_runtime_core_int_to_string(value__1)
    t231 = inline332
    _goml_runtime_core_string_println(t231)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(self__132 *_goml_vec_int, index__133 int) int {
    var t235 int = vec_get__Vec_3int(self__132, index__133)
    return t235
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var t265 string = _goml_runtime_core_int_to_string(self__40)
    return t265
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var t268 string = _goml_runtime_core_bool_to_string(self__37)
    return t268
}

func main() {
    main0()
}
