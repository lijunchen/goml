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

func _goml_runtime_core_uint_to_string(x uint) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_uint64_to_string(x uint64) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_uint_hash(x uint) uint64 {
    return uint64(x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type hashmap_uint_string_x_entry struct {
    active bool
    key uint
    value string
}

type hashmap_uint_string_x struct {
    buckets map[uint64][]hashmap_uint_string_x_entry
    hashes []uint64
    len int
}

func hashmap_new__HashMap_4uint_6string() *hashmap_uint_string_x {
    return &hashmap_uint_string_x{
        buckets: make(map[uint64][]hashmap_uint_string_x_entry),
        len: 0,
        hashes: nil,
    }
}

func hashmap_lookup__HashMap_4uint_6string(m *hashmap_uint_string_x, key uint) (string, bool) {
    if m == nil {
        var zero string
        return zero, false
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_uint_i_hash(key)
    var bucket []hashmap_uint_string_x_entry = m.buckets[h]
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_uint_string_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_PartialEq_i_uint_i_eq(entry.key, key) {
            return entry.value, true
        }
        i = i + 1
    }
    var zero string
    return zero, false
}

func hashmap_get__HashMap_4uint_6string(m *hashmap_uint_string_x, key uint) Option__string {
    var value string
    var ok bool
    value, ok = hashmap_lookup__HashMap_4uint_6string(m, key)
    if ok {
        return Some{
            _0: value,
        }
    }
    return None{}
}

func hashmap_set__HashMap_4uint_6string(m *hashmap_uint_string_x, key uint, value string) struct{} {
    var reuse_index int = -1
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_uint_i_hash(key)
    var bucket []hashmap_uint_string_x_entry = m.buckets[h]
    if len(bucket) == 0 {
        m.hashes = append(m.hashes, h)
    }
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_uint_string_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_PartialEq_i_uint_i_eq(entry.key, key) {
            bucket[i].value = value
            return struct{}{}
        }
        if !entry.active && reuse_index < 0 {
            reuse_index = i
        }
        i = i + 1
    }
    if reuse_index >= 0 {
        bucket[reuse_index] = hashmap_uint_string_x_entry{
            active: true,
            key: key,
            value: value,
        }
        m.len = m.len + 1
        return struct{}{}
    }
    bucket = append(bucket, hashmap_uint_string_x_entry{
        active: true,
        key: key,
        value: value,
    })
    m.buckets[h] = bucket
    m.len = m.len + 1
    return struct{}{}
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

func main0() struct{} {
    var left__3 uint = 19
    var right__4_source int = 2
    var right__4 uint = uint(int(right__4_source))
    var result__5 uint
    var inline268 uint = left__3 + right__4
    var inline269 uint = inline268 * 2
    result__5 = inline269
    var inline265 string = _goml_m_trait__impl_i_ToString_i_uint_i_to__string(result__5)
    _goml_runtime_core_string_println(inline265)
    var t189 string
    var inline263 string = _goml_runtime_core_uint_to_string(result__5)
    t189 = inline263
    var inline260 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t189)
    _goml_runtime_core_string_println(inline260)
    var t190 string
    switch result__5 {
    case 0:
        t190 = "zero"
    case 42:
        t190 = "answer"
    default:
        t190 = "other"
    }
    var inline256 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t190)
    _goml_runtime_core_string_println(inline256)
    var t191 bool = result__5 > left__3
    var inline253 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t191)
    _goml_runtime_core_string_println(inline253)
    var t192_rhs uint = 15
    var t192 uint = result__5 & t192_rhs
    var t193 uint64 = uint64(uint(t192))
    var inline250 string = _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(t193)
    _goml_runtime_core_string_println(inline250)
    var values__6 *hashmap_uint_string_x
    var inline248 *hashmap_uint_string_x = hashmap_new__HashMap_4uint_6string()
    values__6 = inline248
    var inline245 string = "stored"
    hashmap_set__HashMap_4uint_6string(values__6, result__5, inline245)
    var mtmp178 Option__string
    var inline242 uint = 42
    var inline243 Option__string = hashmap_get__HashMap_4uint_6string(values__6, inline242)
    mtmp178 = inline243
    switch mtmp178.(type) {
    case None:
        var inline235 string = "missing"
        var inline236 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline235)
        _goml_runtime_core_string_println(inline236)
        return struct{}{}
    case Some:
        var x179 string = mtmp178.(Some)._0
        var inline239 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x179)
        _goml_runtime_core_string_println(inline239)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_uint_i_to__string(self__274 uint) string {
    var t205 string = _goml_runtime_core_uint_to_string(self__274)
    return t205
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t224 string = _goml_runtime_core_bool_to_string(self__66)
    return t224
}

func _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(self__77 uint64) string {
    var t227 string = _goml_runtime_core_uint64_to_string(self__77)
    return t227
}

func _goml_m_trait__impl_i_PartialEq_i_uint_i_eq(self__276 uint, other__277 uint) bool {
    var t230 bool = self__276 == other__277
    return t230
}

func _goml_m_trait__impl_i_Hash_i_uint_i_hash(self__278 uint) uint64 {
    var t233 uint64 = _goml_runtime_core_uint_hash(self__278)
    return t233
}

func main() {
    main0()
}
