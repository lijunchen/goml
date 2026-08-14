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
    var inline278 uint = left__3 + right__4
    var inline279 uint = inline278 * 2
    result__5 = inline279
    var inline275 string = _goml_m_trait__impl_i_ToString_i_uint_i_to__string(result__5)
    _goml_runtime_core_string_println(inline275)
    var t199 string
    var inline273 string = _goml_runtime_core_uint_to_string(result__5)
    t199 = inline273
    var inline270 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t199)
    _goml_runtime_core_string_println(inline270)
    var t200 string
    switch result__5 {
    case 0:
        t200 = "zero"
    case 42:
        t200 = "answer"
    default:
        t200 = "other"
    }
    var inline266 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t200)
    _goml_runtime_core_string_println(inline266)
    var t201 bool = result__5 > left__3
    var inline263 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t201)
    _goml_runtime_core_string_println(inline263)
    var t202_rhs uint = 15
    var t202 uint = result__5 & t202_rhs
    var t203 uint64 = uint64(uint(t202))
    var inline260 string = _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(t203)
    _goml_runtime_core_string_println(inline260)
    var values__6 *hashmap_uint_string_x
    var inline258 *hashmap_uint_string_x = hashmap_new__HashMap_4uint_6string()
    values__6 = inline258
    var inline255 string = "stored"
    hashmap_set__HashMap_4uint_6string(values__6, result__5, inline255)
    var mtmp188 Option__string
    var inline252 uint = 42
    var inline253 Option__string = hashmap_get__HashMap_4uint_6string(values__6, inline252)
    mtmp188 = inline253
    switch mtmp188.(type) {
    case None:
        var inline245 string = "missing"
        var inline246 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline245)
        _goml_runtime_core_string_println(inline246)
        return struct{}{}
    case Some:
        var x189 string = mtmp188.(Some)._0
        var inline249 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x189)
        _goml_runtime_core_string_println(inline249)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_uint_i_to__string(self__281 uint) string {
    var t215 string = _goml_runtime_core_uint_to_string(self__281)
    return t215
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t234 string = _goml_runtime_core_bool_to_string(self__64)
    return t234
}

func _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(self__75 uint64) string {
    var t237 string = _goml_runtime_core_uint64_to_string(self__75)
    return t237
}

func _goml_m_trait__impl_i_PartialEq_i_uint_i_eq(self__283 uint, other__284 uint) bool {
    var t240 bool = self__283 == other__284
    return t240
}

func _goml_m_trait__impl_i_Hash_i_uint_i_hash(self__285 uint) uint64 {
    var t243 uint64 = _goml_runtime_core_uint_hash(self__285)
    return t243
}

func main() {
    main0()
}
