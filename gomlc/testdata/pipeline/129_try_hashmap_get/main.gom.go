package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_hash(s string) uint64 {
    var h uint64 = 14695981039346656037
    var i int = 0
    for {
        if i >= int(len(s)) {
            break
        }
        h = h * 1099511628211 + uint64(s[i])
        i = i + 1
    }
    return h
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type hashmap_string_int32_x_entry struct {
    active bool
    key string
    value int32
}

type hashmap_string_int32_x struct {
    buckets map[uint64][]hashmap_string_int32_x_entry
    hashes []uint64
    len int
}

func hashmap_new__HashMap_6string_5int32() *hashmap_string_int32_x {
    return &hashmap_string_int32_x{
        buckets: make(map[uint64][]hashmap_string_int32_x_entry),
        len: 0,
        hashes: nil,
    }
}

func hashmap_lookup__HashMap_6string_5int32(m *hashmap_string_int32_x, key string) (int32, bool) {
    if m == nil {
        var zero int32
        return zero, false
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_string_i_hash(key)
    var bucket []hashmap_string_int32_x_entry = m.buckets[h]
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_string_int32_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_Eq_i_string_i_eq(entry.key, key) {
            return entry.value, true
        }
        i = i + 1
    }
    var zero int32
    return zero, false
}

func hashmap_get__HashMap_6string_5int32(m *hashmap_string_int32_x, key string) Option__int32 {
    var value int32
    var ok bool
    value, ok = hashmap_lookup__HashMap_6string_5int32(m, key)
    if ok {
        return Some{
            _0: value,
        }
    }
    return None{}
}

func hashmap_set__HashMap_6string_5int32(m *hashmap_string_int32_x, key string, value int32) struct{} {
    var reuse_index int = -1
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_string_i_hash(key)
    var bucket []hashmap_string_int32_x_entry = m.buckets[h]
    if len(bucket) == 0 {
        m.hashes = append(m.hashes, h)
    }
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_string_int32_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_Eq_i_string_i_eq(entry.key, key) {
            bucket[i].value = value
            return struct{}{}
        }
        if !entry.active && reuse_index < 0 {
            reuse_index = i
        }
        i = i + 1
    }
    if reuse_index >= 0 {
        bucket[reuse_index] = hashmap_string_int32_x_entry{
            active: true,
            key: key,
            value: value,
        }
        m.len = m.len + 1
        return struct{}{}
    }
    bucket = append(bucket, hashmap_string_int32_x_entry{
        active: true,
        key: key,
        value: value,
    })
    m.buckets[h] = bucket
    m.len = m.len + 1
    return struct{}{}
}

type Option__int32 interface {
    isOption__int32()
}

type None struct {}

func (_ None) isOption__int32() {}

type Some struct {
    _0 int32
}

func (_ Some) isOption__int32() {}

func fetch(flag__0 bool) Option__int32 {
    var m__1 *hashmap_string_int32_x
    var inline210 *hashmap_string_int32_x = hashmap_new__HashMap_6string_5int32()
    m__1 = inline210
    if flag__0 {
        var inline203 string = "a"
        var inline204 int32 = 7
        hashmap_set__HashMap_6string_5int32(m__1, inline203, inline204)
    } else {}
    var mtmp157 Option__int32
    var inline207 string = "a"
    var inline208 Option__int32 = hashmap_get__HashMap_6string_5int32(m__1, inline207)
    mtmp157 = inline208
    var jp166 int32
    switch mtmp157.(type) {
    case None:
        return None{}
    case Some:
        var x158 int32 = mtmp157.(Some)._0
        jp166 = x158
        var t167 int32 = jp166 + 1
        var t168 Option__int32 = Some{
            _0: t167,
        }
        return t168
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t176 Option__int32 = fetch(true)
    var t177 string
    switch t176.(type) {
    case None:
        t177 = "none"
    case Some:
        var inline225 int32 = t176.(Some)._0
        var inline227 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline225)
        var inline228 string = "some=" + inline227
        t177 = inline228
    default:
        panic("non-exhaustive match")
    }
    var inline222 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t177)
    _goml_runtime_core_string_println(inline222)
    var t178 Option__int32 = fetch(false)
    var t179 string
    switch t178.(type) {
    case None:
        t179 = "none"
    case Some:
        var inline217 int32 = t178.(Some)._0
        var inline219 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline217)
        var inline220 string = "some=" + inline219
        t179 = inline220
    default:
        panic("non-exhaustive match")
    }
    var inline214 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t179)
    _goml_runtime_core_string_println(inline214)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var t190 string = _goml_runtime_core_int32_to_string(self__6)
    return t190
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__55 string, other__56 string) bool {
    var t198 bool = self__55 == other__56
    return t198
}

func _goml_m_trait__impl_i_Hash_i_string_i_hash(self__83 string) uint64 {
    var t201 uint64 = _goml_runtime_core_string_hash(self__83)
    return t201
}

func main() {
    main0()
}
