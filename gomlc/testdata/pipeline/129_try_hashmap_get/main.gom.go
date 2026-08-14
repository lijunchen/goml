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

type Ordering int32

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
    var inline463 *hashmap_string_int32_x = hashmap_new__HashMap_6string_5int32()
    m__1 = inline463
    if flag__0 {
        var inline456 string = "a"
        var inline457 int32 = 7
        hashmap_set__HashMap_6string_5int32(m__1, inline456, inline457)
    } else {}
    var mtmp410 Option__int32
    var inline460 string = "a"
    var inline461 Option__int32 = hashmap_get__HashMap_6string_5int32(m__1, inline460)
    mtmp410 = inline461
    var jp419 int32
    switch mtmp410.(type) {
    case None:
        return None{}
    case Some:
        var x411 int32 = mtmp410.(Some)._0
        jp419 = x411
        var t420 int32 = jp419 + 1
        var t421 Option__int32 = Some{
            _0: t420,
        }
        return t421
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t429 Option__int32 = fetch(true)
    var t430 string
    switch t429.(type) {
    case None:
        t430 = "none"
    case Some:
        var inline478 int32 = t429.(Some)._0
        var inline480 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline478)
        var inline481 string = "some=" + inline480
        t430 = inline481
    default:
        panic("non-exhaustive match")
    }
    var inline475 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t430)
    _goml_runtime_core_string_println(inline475)
    var t431 Option__int32 = fetch(false)
    var t432 string
    switch t431.(type) {
    case None:
        t432 = "none"
    case Some:
        var inline470 int32 = t431.(Some)._0
        var inline472 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline470)
        var inline473 string = "some=" + inline472
        t432 = inline473
    default:
        panic("non-exhaustive match")
    }
    var inline467 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t432)
    _goml_runtime_core_string_println(inline467)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t443 string = _goml_runtime_core_int32_to_string(self__33)
    return t443
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_trait__impl_i_PartialEq_i_string_i_eq(self__181 string, other__182 string) bool {
    var t451 bool = self__181 == other__182
    return t451
}

func _goml_m_trait__impl_i_Hash_i_string_i_hash(self__209 string) uint64 {
    var t454 uint64 = _goml_runtime_core_string_hash(self__209)
    return t454
}

func main() {
    main0()
}
