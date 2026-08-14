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
        var t209_source int = 0
        var t209 uint64 = uint64(int(t209_source))
        var t210 uint64 = t209 + 14695981039346656037
        var h__1 uint64 = t210 + 1
        return h__1
    case B:
        var x182 int32 = self__0.(B)._0
        var t211_source int = 0
        var t211 uint64 = uint64(int(t211_source))
        var t212 uint64 = t211 + 14695981039346656037
        var h__3 uint64 = t212 + 2
        var t213_source int = 0
        var t213 uint64 = uint64(int(t213_source))
        var t214 uint64 = t213 + 1099511628211
        var t215 uint64 = h__3 * t214
        var t216 uint64
        var inline303 uint64 = _goml_runtime_core_int32_hash(x182)
        t216 = inline303
        var h__4 uint64 = t215 + t216
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
        var x186 int32 = other__6.(B)._0
        switch self__5.(type) {
        case B:
            var x188 int32 = self__5.(B)._0
            var inline305 bool = x188 == x186
            return inline305
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
    var t233 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(v__11, 0)
    println__T_int(t233)
    var t234 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(v__11, 1)
    println__T_int(t234)
    var t235 int
    var inline359 int = 2
    var inline360 int = vec_get__Vec_3int(v__11, inline359)
    t235 = inline360
    var inline356 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t235)
    _goml_runtime_core_string_println(inline356)
    var t236 int
    var inline354 int = vec_len__Vec_3int(v__11)
    t236 = inline354
    var inline351 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t236)
    _goml_runtime_core_string_println(inline351)
    var m__12 *hashmap_Key_int32_x
    var inline349 *hashmap_Key_int32_x = hashmap_new__HashMap_3Key_5int32()
    m__12 = inline349
    var inline346 int32 = 10
    hashmap_set__HashMap_3Key_5int32(m__12, A{}, inline346)
    var t237 Key = B{
        _0: 1,
    }
    var inline343 int32 = 20
    hashmap_set__HashMap_3Key_5int32(m__12, t237, inline343)
    var t238 int
    var inline341 int = hashmap_len__HashMap_3Key_5int32(m__12)
    t238 = inline341
    var inline338 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t238)
    _goml_runtime_core_string_println(inline338)
    var t239 Option__int32
    var inline336 Option__int32 = hashmap_get__HashMap_3Key_5int32(m__12, A{})
    t239 = inline336
    switch t239.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline332 int32 = t239.(Some)._0
        println__T_int32(inline332)
    default:
        panic("non-exhaustive match")
    }
    var t240 Key = B{
        _0: 1,
    }
    var t241 bool
    var inline329 bool = hashmap_contains__HashMap_3Key_5int32(m__12, t240)
    t241 = inline329
    var inline326 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t241)
    _goml_runtime_core_string_println(inline326)
    var t242 Key = B{
        _0: 1,
    }
    hashmap_remove__HashMap_3Key_5int32(m__12, t242)
    var t243 Key = B{
        _0: 1,
    }
    var t244 bool
    var inline322 bool = hashmap_contains__HashMap_3Key_5int32(m__12, t243)
    t244 = inline322
    var inline319 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t244)
    _goml_runtime_core_string_println(inline319)
    var t245 int
    var inline317 int = hashmap_len__HashMap_3Key_5int32(m__12)
    t245 = inline317
    var inline314 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t245)
    _goml_runtime_core_string_println(inline314)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t253 string
    t253 = value__1
    _goml_runtime_core_string_println(t253)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t256 string
    var inline363 string = _goml_runtime_core_int32_to_string(value__1)
    t256 = inline363
    _goml_runtime_core_string_println(t256)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int() *_goml_vec_int {
    var t260 *_goml_vec_int = vec_new__Vec_3int()
    return t260
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(self__174 *_goml_vec_int, elem__175 int) struct{} {
    vec_push__Vec_3int(self__174, elem__175)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t264 string
    var inline365 string = _goml_runtime_core_int_to_string(value__1)
    t264 = inline365
    _goml_runtime_core_string_println(t264)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(self__184 *_goml_vec_int, index__185 int) int {
    var t268 int = vec_get__Vec_3int(self__184, index__185)
    return t268
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t298 string = _goml_runtime_core_int_to_string(self__67)
    return t298
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t301 string = _goml_runtime_core_bool_to_string(self__64)
    return t301
}

func main() {
    main0()
}
