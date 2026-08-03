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
    var inline232 uint = left__3 + right__4
    var inline233 uint = inline232 * 2
    result__5 = inline233
    var inline229 string = _goml_m_trait__impl_i_ToString_i_uint_i_to__string(result__5)
    _goml_runtime_core_string_println(inline229)
    var t153 string
    var inline227 string = _goml_runtime_core_uint_to_string(result__5)
    t153 = inline227
    var inline224 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t153)
    _goml_runtime_core_string_println(inline224)
    var t154 string
    switch result__5 {
    case 0:
        t154 = "zero"
    case 42:
        t154 = "answer"
    default:
        t154 = "other"
    }
    var inline220 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t154)
    _goml_runtime_core_string_println(inline220)
    var t155 bool = result__5 > left__3
    var inline217 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t155)
    _goml_runtime_core_string_println(inline217)
    var t156_rhs uint = 15
    var t156 uint = result__5 & t156_rhs
    var t157 uint64 = uint64(uint(t156))
    var inline214 string = _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(t157)
    _goml_runtime_core_string_println(inline214)
    var values__6 *hashmap_uint_string_x
    var inline212 *hashmap_uint_string_x = hashmap_new__HashMap_4uint_6string()
    values__6 = inline212
    var inline209 string = "stored"
    hashmap_set__HashMap_4uint_6string(values__6, result__5, inline209)
    var mtmp142 Option__string
    var inline206 uint = 42
    var inline207 Option__string = hashmap_get__HashMap_4uint_6string(values__6, inline206)
    mtmp142 = inline207
    switch mtmp142.(type) {
    case None:
        var inline199 string = "missing"
        var inline200 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline199)
        _goml_runtime_core_string_println(inline200)
        return struct{}{}
    case Some:
        var x143 string = mtmp142.(Some)._0
        var inline203 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x143)
        _goml_runtime_core_string_println(inline203)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_uint_i_to__string(self__232 uint) string {
    var t169 string = _goml_runtime_core_uint_to_string(self__232)
    return t169
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t188 string = _goml_runtime_core_bool_to_string(self__66)
    return t188
}

func _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(self__77 uint64) string {
    var t191 string = _goml_runtime_core_uint64_to_string(self__77)
    return t191
}

func _goml_m_trait__impl_i_Eq_i_uint_i_eq(self__233 uint, other__234 uint) bool {
    var t194 bool = self__233 == other__234
    return t194
}

func _goml_m_trait__impl_i_Hash_i_uint_i_hash(self__235 uint) uint64 {
    var t197 uint64 = _goml_runtime_core_uint_hash(self__235)
    return t197
}

func main() {
    main0()
}
