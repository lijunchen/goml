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

func _goml_m_trait__impl_i_Hash_i_Key_i_hash(self__0 Key) uint64 {
    switch self__0.(type) {
    case A:
        var t163_source int = 0
        var t163 uint64 = uint64(int(t163_source))
        var t164 uint64 = t163 + 14695981039346656037
        var h__1 uint64 = t164 + 1
        return h__1
    case B:
        var x136 int32 = self__0.(B)._0
        var t165_source int = 0
        var t165 uint64 = uint64(int(t165_source))
        var t166 uint64 = t165 + 14695981039346656037
        var h__3 uint64 = t166 + 2
        var t167_source int = 0
        var t167 uint64 = uint64(int(t167_source))
        var t168 uint64 = t167 + 1099511628211
        var t169 uint64 = h__3 * t168
        var t170 uint64
        var inline257 uint64 = _goml_runtime_core_int32_hash(x136)
        t170 = inline257
        var h__4 uint64 = t169 + t170
        return h__4
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_Eq_i_Key_i_eq(self__5 Key, other__6 Key) bool {
    switch other__6.(type) {
    case A:
        switch self__5.(type) {
        case A:
            return true
        default:
            return false
        }
    case B:
        var x140 int32 = other__6.(B)._0
        switch self__5.(type) {
        case B:
            var x142 int32 = self__5.(B)._0
            var inline259 bool = x142 == x140
            return inline259
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
    var t187 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(v__11, 0)
    println__T_int(t187)
    var t188 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(v__11, 1)
    println__T_int(t188)
    var t189 int
    var inline313 int = 2
    var inline314 int = vec_get__Vec_3int(v__11, inline313)
    t189 = inline314
    var inline310 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t189)
    _goml_runtime_core_string_println(inline310)
    var t190 int
    var inline308 int = vec_len__Vec_3int(v__11)
    t190 = inline308
    var inline305 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t190)
    _goml_runtime_core_string_println(inline305)
    var m__12 *hashmap_Key_int32_x
    var inline303 *hashmap_Key_int32_x = hashmap_new__HashMap_3Key_5int32()
    m__12 = inline303
    var inline300 int32 = 10
    hashmap_set__HashMap_3Key_5int32(m__12, A{}, inline300)
    var t191 Key = B{
        _0: 1,
    }
    var inline297 int32 = 20
    hashmap_set__HashMap_3Key_5int32(m__12, t191, inline297)
    var t192 int
    var inline295 int = hashmap_len__HashMap_3Key_5int32(m__12)
    t192 = inline295
    var inline292 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t192)
    _goml_runtime_core_string_println(inline292)
    var t193 Option__int32
    var inline290 Option__int32 = hashmap_get__HashMap_3Key_5int32(m__12, A{})
    t193 = inline290
    switch t193.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline286 int32 = t193.(Some)._0
        println__T_int32(inline286)
    default:
        panic("non-exhaustive match")
    }
    var t194 Key = B{
        _0: 1,
    }
    var t195 bool
    var inline283 bool = hashmap_contains__HashMap_3Key_5int32(m__12, t194)
    t195 = inline283
    var inline280 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t195)
    _goml_runtime_core_string_println(inline280)
    var t196 Key = B{
        _0: 1,
    }
    hashmap_remove__HashMap_3Key_5int32(m__12, t196)
    var t197 Key = B{
        _0: 1,
    }
    var t198 bool
    var inline276 bool = hashmap_contains__HashMap_3Key_5int32(m__12, t197)
    t198 = inline276
    var inline273 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t198)
    _goml_runtime_core_string_println(inline273)
    var t199 int
    var inline271 int = hashmap_len__HashMap_3Key_5int32(m__12)
    t199 = inline271
    var inline268 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t199)
    _goml_runtime_core_string_println(inline268)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t207 string
    t207 = value__31
    _goml_runtime_core_string_println(t207)
    return struct{}{}
}

func println__T_int32(value__31 int32) struct{} {
    var t210 string
    var inline317 string = _goml_runtime_core_int32_to_string(value__31)
    t210 = inline317
    _goml_runtime_core_string_println(t210)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int() *_goml_vec_int {
    var t214 *_goml_vec_int = vec_new__Vec_3int()
    return t214
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(self__151 *_goml_vec_int, elem__152 int) struct{} {
    vec_push__Vec_3int(self__151, elem__152)
    return struct{}{}
}

func println__T_int(value__31 int) struct{} {
    var t218 string
    var inline319 string = _goml_runtime_core_int_to_string(value__31)
    t218 = inline319
    _goml_runtime_core_string_println(t218)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(self__157 *_goml_vec_int, index__158 int) int {
    var t222 int = vec_get__Vec_3int(self__157, index__158)
    return t222
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t252 string = _goml_runtime_core_int_to_string(self__69)
    return t252
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t255 string = _goml_runtime_core_bool_to_string(self__66)
    return t255
}

func main() {
    main0()
}
