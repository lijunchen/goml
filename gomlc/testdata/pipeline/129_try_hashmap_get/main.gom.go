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
        if entry.active && _goml_m_trait__impl_i_PartialEq_i_string_i_eq(entry.key, key) {
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
        if entry.active && _goml_m_trait__impl_i_PartialEq_i_string_i_eq(entry.key, key) {
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
    var inline242 *hashmap_string_int32_x = hashmap_new__HashMap_6string_5int32()
    m__1 = inline242
    if flag__0 {
        var inline235 string = "a"
        var inline236 int32 = 7
        hashmap_set__HashMap_6string_5int32(m__1, inline235, inline236)
    } else {}
    var mtmp189 Option__int32
    var inline239 string = "a"
    var inline240 Option__int32 = hashmap_get__HashMap_6string_5int32(m__1, inline239)
    mtmp189 = inline240
    var jp198 int32
    switch mtmp189.(type) {
    case None:
        return None{}
    case Some:
        var x190 int32 = mtmp189.(Some)._0
        jp198 = x190
        var t199 int32 = jp198 + 1
        var t200 Option__int32 = Some{
            _0: t199,
        }
        return t200
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t208 Option__int32 = fetch(true)
    var t209 string
    switch t208.(type) {
    case None:
        t209 = "none"
    case Some:
        var inline257 int32 = t208.(Some)._0
        var inline259 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline257)
        var inline260 string = "some=" + inline259
        t209 = inline260
    default:
        panic("non-exhaustive match")
    }
    var inline254 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t209)
    _goml_runtime_core_string_println(inline254)
    var t210 Option__int32 = fetch(false)
    var t211 string
    switch t210.(type) {
    case None:
        t211 = "none"
    case Some:
        var inline249 int32 = t210.(Some)._0
        var inline251 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline249)
        var inline252 string = "some=" + inline251
        t211 = inline252
    default:
        panic("non-exhaustive match")
    }
    var inline246 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t211)
    _goml_runtime_core_string_println(inline246)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t222 string = _goml_runtime_core_int32_to_string(self__33)
    return t222
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_PartialEq_i_string_i_eq(self__97 string, other__98 string) bool {
    var t230 bool = self__97 == other__98
    return t230
}

func _goml_m_trait__impl_i_Hash_i_string_i_hash(self__125 string) uint64 {
    var t233 uint64 = _goml_runtime_core_string_hash(self__125)
    return t233
}

func main() {
    main0()
}
