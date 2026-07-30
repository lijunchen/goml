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
    var retv116 Option__int32
    var m__1 *hashmap_string_int32_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32()
    if flag__0 {
        _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(m__1, "a", 7)
    } else {}
    var mtmp110 Option__int32 = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__string____V__int32(m__1, "a")
    var jp119 int32
    switch mtmp110.(type) {
    case None:
        retv116 = None{}
        return retv116
    case Some:
        var x111 int32 = mtmp110.(Some)._0
        var try_value__18 int32 = x111
        jp119 = try_value__18
        var value__2 int32 = jp119
        var t120 int32 = value__2 + 1
        var t121 Option__int32 = Some{
            _0: t120,
        }
        retv116 = t121
        return retv116
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__3 Option__int32) string {
    var retv123 string
    var jp125 string
    switch opt__3.(type) {
    case None:
        jp125 = "none"
    case Some:
        var x112 int32 = opt__3.(Some)._0
        var value__4 int32 = x112
        var t126 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__4)
        var t127 string = "some=" + t126
        jp125 = t127
    default:
        panic("non-exhaustive match")
    }
    retv123 = jp125
    return retv123
}

func main0() struct{} {
    var t129 Option__int32 = fetch(true)
    var t130 string = show(t129)
    println__T_string(t130)
    var t131 Option__int32 = fetch(false)
    var t132 string = show(t131)
    println__T_string(t132)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32() *hashmap_string_int32_x {
    var retv134 *hashmap_string_int32_x
    var t135 *hashmap_string_int32_x = hashmap_new__HashMap_6string_5int32()
    retv134 = t135
    return retv134
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(self__198 *hashmap_string_int32_x, key__199 string, value__200 int32) struct{} {
    hashmap_set__HashMap_6string_5int32(self__198, key__199, value__200)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__string____V__int32(self__196 *hashmap_string_int32_x, key__197 string) Option__int32 {
    var retv139 Option__int32
    var t140 Option__int32 = hashmap_get__HashMap_6string_5int32(self__196, key__197)
    retv139 = t140
    return retv139
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv142 string
    var t143 string = _goml_runtime_core_int32_to_string(self__6)
    retv142 = t143
    return retv142
}

func println__T_string(value__1 string) struct{} {
    var t145 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t145)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv148 string
    retv148 = self__38
    return retv148
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__55 string, other__56 string) bool {
    var retv150 bool
    var t151 bool = self__55 == other__56
    retv150 = t151
    return retv150
}

func _goml_m_trait__impl_i_Hash_i_string_i_hash(self__83 string) uint64 {
    var retv153 uint64
    var t154 uint64 = _goml_runtime_core_string_hash(self__83)
    retv153 = t154
    return retv153
}

func main() {
    main0()
}
