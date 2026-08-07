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
    var inline191 *hashmap_string_int32_x = hashmap_new__HashMap_6string_5int32()
    m__1 = inline191
    if flag__0 {
        var inline184 string = "a"
        var inline185 int32 = 7
        hashmap_set__HashMap_6string_5int32(m__1, inline184, inline185)
    } else {}
    var mtmp138 Option__int32
    var inline188 string = "a"
    var inline189 Option__int32 = hashmap_get__HashMap_6string_5int32(m__1, inline188)
    mtmp138 = inline189
    var jp147 int32
    switch mtmp138.(type) {
    case None:
        return None{}
    case Some:
        var x139 int32 = mtmp138.(Some)._0
        jp147 = x139
        var t148 int32 = jp147 + 1
        var t149 Option__int32 = Some{
            _0: t148,
        }
        return t149
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t157 Option__int32 = fetch(true)
    var t158 string
    switch t157.(type) {
    case None:
        t158 = "none"
    case Some:
        var inline206 int32 = t157.(Some)._0
        var inline208 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline206)
        var inline209 string = "some=" + inline208
        t158 = inline209
    default:
        panic("non-exhaustive match")
    }
    var inline203 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t158)
    _goml_runtime_core_string_println(inline203)
    var t159 Option__int32 = fetch(false)
    var t160 string
    switch t159.(type) {
    case None:
        t160 = "none"
    case Some:
        var inline198 int32 = t159.(Some)._0
        var inline200 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline198)
        var inline201 string = "some=" + inline200
        t160 = inline201
    default:
        panic("non-exhaustive match")
    }
    var inline195 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t160)
    _goml_runtime_core_string_println(inline195)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t171 string = _goml_runtime_core_int32_to_string(self__35)
    return t171
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__99 string, other__100 string) bool {
    var t179 bool = self__99 == other__100
    return t179
}

func _goml_m_trait__impl_i_Hash_i_string_i_hash(self__127 string) uint64 {
    var t182 uint64 = _goml_runtime_core_string_hash(self__127)
    return t182
}

func main() {
    main0()
}
