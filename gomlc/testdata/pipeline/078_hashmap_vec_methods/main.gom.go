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
        var x180 int32 = other__1.(B)._0
        switch self__0.(type) {
        case B:
            var x182 int32 = self__0.(B)._0
            var inline292 bool = x182 == x180
            return inline292
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
        var x183 int32 = self__4.(B)._0
        var h__7 uint64 = 14695981039346656037 + 2
        var t215 uint64 = h__7 * 1099511628211
        var t216 uint64
        var inline294 uint64 = _goml_runtime_core_int32_hash(x183)
        t216 = inline294
        var h__8 uint64 = t215 + t216
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
    var t222 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(v__11, 0)
    println__T_int(t222)
    var t223 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(v__11, 1)
    println__T_int(t223)
    var t224 int
    var inline348 int = 2
    var inline349 int = vec_get__Vec_3int(v__11, inline348)
    t224 = inline349
    var inline345 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t224)
    _goml_runtime_core_string_println(inline345)
    var t225 int
    var inline343 int = vec_len__Vec_3int(v__11)
    t225 = inline343
    var inline340 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t225)
    _goml_runtime_core_string_println(inline340)
    var m__12 *hashmap_Key_int32_x
    var inline338 *hashmap_Key_int32_x = hashmap_new__HashMap_3Key_5int32()
    m__12 = inline338
    var inline335 int32 = 10
    hashmap_set__HashMap_3Key_5int32(m__12, A{}, inline335)
    var t226 Key = B{
        _0: 1,
    }
    var inline332 int32 = 20
    hashmap_set__HashMap_3Key_5int32(m__12, t226, inline332)
    var t227 int
    var inline330 int = hashmap_len__HashMap_3Key_5int32(m__12)
    t227 = inline330
    var inline327 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t227)
    _goml_runtime_core_string_println(inline327)
    var t228 Option__int32
    var inline325 Option__int32 = hashmap_get__HashMap_3Key_5int32(m__12, A{})
    t228 = inline325
    switch t228.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline321 int32 = t228.(Some)._0
        println__T_int32(inline321)
    default:
        panic("non-exhaustive match")
    }
    var t229 Key = B{
        _0: 1,
    }
    var t230 bool
    var inline318 bool = hashmap_contains__HashMap_3Key_5int32(m__12, t229)
    t230 = inline318
    var inline315 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t230)
    _goml_runtime_core_string_println(inline315)
    var t231 Key = B{
        _0: 1,
    }
    hashmap_remove__HashMap_3Key_5int32(m__12, t231)
    var t232 Key = B{
        _0: 1,
    }
    var t233 bool
    var inline311 bool = hashmap_contains__HashMap_3Key_5int32(m__12, t232)
    t233 = inline311
    var inline308 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t233)
    _goml_runtime_core_string_println(inline308)
    var t234 int
    var inline306 int = hashmap_len__HashMap_3Key_5int32(m__12)
    t234 = inline306
    var inline303 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t234)
    _goml_runtime_core_string_println(inline303)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t242 string
    t242 = value__31
    _goml_runtime_core_string_println(t242)
    return struct{}{}
}

func println__T_int32(value__31 int32) struct{} {
    var t245 string
    var inline352 string = _goml_runtime_core_int32_to_string(value__31)
    t245 = inline352
    _goml_runtime_core_string_println(t245)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int() *_goml_vec_int {
    var t249 *_goml_vec_int = vec_new__Vec_3int()
    return t249
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(self__155 *_goml_vec_int, elem__156 int) struct{} {
    vec_push__Vec_3int(self__155, elem__156)
    return struct{}{}
}

func println__T_int(value__31 int) struct{} {
    var t253 string
    var inline354 string = _goml_runtime_core_int_to_string(value__31)
    t253 = inline354
    _goml_runtime_core_string_println(t253)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(self__161 *_goml_vec_int, index__162 int) int {
    var t257 int = vec_get__Vec_3int(self__161, index__162)
    return t257
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t287 string = _goml_runtime_core_int_to_string(self__69)
    return t287
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t290 string = _goml_runtime_core_bool_to_string(self__66)
    return t290
}

func main() {
    main0()
}
