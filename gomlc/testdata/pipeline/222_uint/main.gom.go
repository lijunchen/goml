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
        if entry.active && _goml_m_trait__impl_i_Eq_i_uint_i_eq(entry.key, key) {
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
        if entry.active && _goml_m_trait__impl_i_Eq_i_uint_i_eq(entry.key, key) {
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
    var inline251 uint = left__3 + right__4
    var inline252 uint = inline251 * 2
    result__5 = inline252
    var inline248 string = _goml_m_trait__impl_i_ToString_i_uint_i_to__string(result__5)
    _goml_runtime_core_string_println(inline248)
    var t172 string
    var inline246 string = _goml_runtime_core_uint_to_string(result__5)
    t172 = inline246
    var inline243 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t172)
    _goml_runtime_core_string_println(inline243)
    var t173 string
    switch result__5 {
    case 0:
        t173 = "zero"
    case 42:
        t173 = "answer"
    default:
        t173 = "other"
    }
    var inline239 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t173)
    _goml_runtime_core_string_println(inline239)
    var t174 bool = result__5 > left__3
    var inline236 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t174)
    _goml_runtime_core_string_println(inline236)
    var t175_rhs uint = 15
    var t175 uint = result__5 & t175_rhs
    var t176 uint64 = uint64(uint(t175))
    var inline233 string = _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(t176)
    _goml_runtime_core_string_println(inline233)
    var values__6 *hashmap_uint_string_x
    var inline231 *hashmap_uint_string_x = hashmap_new__HashMap_4uint_6string()
    values__6 = inline231
    var inline228 string = "stored"
    hashmap_set__HashMap_4uint_6string(values__6, result__5, inline228)
    var mtmp161 Option__string
    var inline225 uint = 42
    var inline226 Option__string = hashmap_get__HashMap_4uint_6string(values__6, inline225)
    mtmp161 = inline226
    switch mtmp161.(type) {
    case None:
        var inline218 string = "missing"
        var inline219 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline218)
        _goml_runtime_core_string_println(inline219)
        return struct{}{}
    case Some:
        var x162 string = mtmp161.(Some)._0
        var inline222 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x162)
        _goml_runtime_core_string_println(inline222)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_uint_i_to__string(self__224 uint) string {
    var t188 string = _goml_runtime_core_uint_to_string(self__224)
    return t188
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var t207 string = _goml_runtime_core_bool_to_string(self__37)
    return t207
}

func _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(self__48 uint64) string {
    var t210 string = _goml_runtime_core_uint64_to_string(self__48)
    return t210
}

func _goml_m_trait__impl_i_Eq_i_uint_i_eq(self__225 uint, other__226 uint) bool {
    var t213 bool = self__225 == other__226
    return t213
}

func _goml_m_trait__impl_i_Hash_i_uint_i_hash(self__227 uint) uint64 {
    var t216 uint64 = _goml_runtime_core_uint_hash(self__227)
    return t216
}

func main() {
    main0()
}
