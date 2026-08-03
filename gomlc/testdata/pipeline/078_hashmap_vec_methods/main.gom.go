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
        var x139 int32 = other__1.(B)._0
        switch self__0.(type) {
        case B:
            var x141 int32 = self__0.(B)._0
            var inline251 bool = x141 == x139
            return inline251
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
        var x142 int32 = self__4.(B)._0
        var h__7 uint64 = 14695981039346656037 + 2
        var t174 uint64 = h__7 * 1099511628211
        var t175 uint64
        var inline253 uint64 = _goml_runtime_core_int32_hash(x142)
        t175 = inline253
        var h__8 uint64 = t174 + t175
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
    var t181 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(v__11, 0)
    println__T_int(t181)
    var t182 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(v__11, 1)
    println__T_int(t182)
    var t183 int
    var inline307 int = 2
    var inline308 int = vec_get__Vec_3int(v__11, inline307)
    t183 = inline308
    var inline304 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t183)
    _goml_runtime_core_string_println(inline304)
    var t184 int
    var inline302 int = vec_len__Vec_3int(v__11)
    t184 = inline302
    var inline299 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t184)
    _goml_runtime_core_string_println(inline299)
    var m__12 *hashmap_Key_int32_x
    var inline297 *hashmap_Key_int32_x = hashmap_new__HashMap_3Key_5int32()
    m__12 = inline297
    var inline294 int32 = 10
    hashmap_set__HashMap_3Key_5int32(m__12, A{}, inline294)
    var t185 Key = B{
        _0: 1,
    }
    var inline291 int32 = 20
    hashmap_set__HashMap_3Key_5int32(m__12, t185, inline291)
    var t186 int
    var inline289 int = hashmap_len__HashMap_3Key_5int32(m__12)
    t186 = inline289
    var inline286 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t186)
    _goml_runtime_core_string_println(inline286)
    var t187 Option__int32
    var inline284 Option__int32 = hashmap_get__HashMap_3Key_5int32(m__12, A{})
    t187 = inline284
    switch t187.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline280 int32 = t187.(Some)._0
        println__T_int32(inline280)
    default:
        panic("non-exhaustive match")
    }
    var t188 Key = B{
        _0: 1,
    }
    var t189 bool
    var inline277 bool = hashmap_contains__HashMap_3Key_5int32(m__12, t188)
    t189 = inline277
    var inline274 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t189)
    _goml_runtime_core_string_println(inline274)
    var t190 Key = B{
        _0: 1,
    }
    hashmap_remove__HashMap_3Key_5int32(m__12, t190)
    var t191 Key = B{
        _0: 1,
    }
    var t192 bool
    var inline270 bool = hashmap_contains__HashMap_3Key_5int32(m__12, t191)
    t192 = inline270
    var inline267 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t192)
    _goml_runtime_core_string_println(inline267)
    var t193 int
    var inline265 int = hashmap_len__HashMap_3Key_5int32(m__12)
    t193 = inline265
    var inline262 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t193)
    _goml_runtime_core_string_println(inline262)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t201 string
    t201 = value__31
    _goml_runtime_core_string_println(t201)
    return struct{}{}
}

func println__T_int32(value__31 int32) struct{} {
    var t204 string
    var inline311 string = _goml_runtime_core_int32_to_string(value__31)
    t204 = inline311
    _goml_runtime_core_string_println(t204)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int() *_goml_vec_int {
    var t208 *_goml_vec_int = vec_new__Vec_3int()
    return t208
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(self__134 *_goml_vec_int, elem__135 int) struct{} {
    vec_push__Vec_3int(self__134, elem__135)
    return struct{}{}
}

func println__T_int(value__31 int) struct{} {
    var t212 string
    var inline313 string = _goml_runtime_core_int_to_string(value__31)
    t212 = inline313
    _goml_runtime_core_string_println(t212)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(self__140 *_goml_vec_int, index__141 int) int {
    var t216 int = vec_get__Vec_3int(self__140, index__141)
    return t216
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t246 string = _goml_runtime_core_int_to_string(self__69)
    return t246
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t249 string = _goml_runtime_core_bool_to_string(self__66)
    return t249
}

func main() {
    main0()
}
