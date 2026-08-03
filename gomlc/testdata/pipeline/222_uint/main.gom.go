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
    var inline273 uint = left__3 + right__4
    var inline274 uint = inline273 * 2
    result__5 = inline274
    var inline270 string = _goml_m_trait__impl_i_ToString_i_uint_i_to__string(result__5)
    _goml_runtime_core_string_println(inline270)
    var t194 string
    var inline268 string = _goml_runtime_core_uint_to_string(result__5)
    t194 = inline268
    var inline265 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t194)
    _goml_runtime_core_string_println(inline265)
    var t195 string
    switch result__5 {
    case 0:
        t195 = "zero"
    case 42:
        t195 = "answer"
    default:
        t195 = "other"
    }
    var inline261 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t195)
    _goml_runtime_core_string_println(inline261)
    var t196 bool = result__5 > left__3
    var inline258 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t196)
    _goml_runtime_core_string_println(inline258)
    var t197_rhs uint = 15
    var t197 uint = result__5 & t197_rhs
    var t198 uint64 = uint64(uint(t197))
    var inline255 string = _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(t198)
    _goml_runtime_core_string_println(inline255)
    var values__6 *hashmap_uint_string_x
    var inline253 *hashmap_uint_string_x = hashmap_new__HashMap_4uint_6string()
    values__6 = inline253
    var inline250 string = "stored"
    hashmap_set__HashMap_4uint_6string(values__6, result__5, inline250)
    var mtmp183 Option__string
    var inline247 uint = 42
    var inline248 Option__string = hashmap_get__HashMap_4uint_6string(values__6, inline247)
    mtmp183 = inline248
    switch mtmp183.(type) {
    case None:
        var inline240 string = "missing"
        var inline241 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline240)
        _goml_runtime_core_string_println(inline241)
        return struct{}{}
    case Some:
        var x184 string = mtmp183.(Some)._0
        var inline244 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x184)
        _goml_runtime_core_string_println(inline244)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_uint_i_to__string(self__253 uint) string {
    var t210 string = _goml_runtime_core_uint_to_string(self__253)
    return t210
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t229 string = _goml_runtime_core_bool_to_string(self__66)
    return t229
}

func _goml_m_trait__impl_i_ToString_i_uint64_i_to__string(self__77 uint64) string {
    var t232 string = _goml_runtime_core_uint64_to_string(self__77)
    return t232
}

func _goml_m_trait__impl_i_Eq_i_uint_i_eq(self__254 uint, other__255 uint) bool {
    var t235 bool = self__254 == other__255
    return t235
}

func _goml_m_trait__impl_i_Hash_i_uint_i_hash(self__256 uint) uint64 {
    var t238 uint64 = _goml_runtime_core_uint_hash(self__256)
    return t238
}

func main() {
    main0()
}
