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
    var inline232 *hashmap_string_int32_x = hashmap_new__HashMap_6string_5int32()
    m__1 = inline232
    if flag__0 {
        var inline225 string = "a"
        var inline226 int32 = 7
        hashmap_set__HashMap_6string_5int32(m__1, inline225, inline226)
    } else {}
    var mtmp179 Option__int32
    var inline229 string = "a"
    var inline230 Option__int32 = hashmap_get__HashMap_6string_5int32(m__1, inline229)
    mtmp179 = inline230
    var jp188 int32
    switch mtmp179.(type) {
    case None:
        return None{}
    case Some:
        var x180 int32 = mtmp179.(Some)._0
        jp188 = x180
        var t189 int32 = jp188 + 1
        var t190 Option__int32 = Some{
            _0: t189,
        }
        return t190
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t198 Option__int32 = fetch(true)
    var t199 string
    switch t198.(type) {
    case None:
        t199 = "none"
    case Some:
        var inline247 int32 = t198.(Some)._0
        var inline249 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline247)
        var inline250 string = "some=" + inline249
        t199 = inline250
    default:
        panic("non-exhaustive match")
    }
    var inline244 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t199)
    _goml_runtime_core_string_println(inline244)
    var t200 Option__int32 = fetch(false)
    var t201 string
    switch t200.(type) {
    case None:
        t201 = "none"
    case Some:
        var inline239 int32 = t200.(Some)._0
        var inline241 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline239)
        var inline242 string = "some=" + inline241
        t201 = inline242
    default:
        panic("non-exhaustive match")
    }
    var inline236 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t201)
    _goml_runtime_core_string_println(inline236)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t212 string = _goml_runtime_core_int32_to_string(self__35)
    return t212
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__84 string, other__85 string) bool {
    var t220 bool = self__84 == other__85
    return t220
}

func _goml_m_trait__impl_i_Hash_i_string_i_hash(self__112 string) uint64 {
    var t223 uint64 = _goml_runtime_core_string_hash(self__112)
    return t223
}

func main() {
    main0()
}
