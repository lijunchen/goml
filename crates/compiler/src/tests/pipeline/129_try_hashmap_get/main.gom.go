package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_hash(s string) uint64 {
    var h uint64 = 14695981039346656037
    var i int32 = 0
    for {
        if i >= int32(len(s)) {
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
    len int32
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
    var i int32 = 0
    for {
        if i >= int32(len(bucket)) {
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
    var reuse_index int32 = -1
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_string_i_hash(key)
    var bucket []hashmap_string_int32_x_entry = m.buckets[h]
    if len(bucket) == 0 {
        m.hashes = append(m.hashes, h)
    }
    var i int32 = 0
    for {
        if i >= int32(len(bucket)) {
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
    var retv69 Option__int32
    var m__1 *hashmap_string_int32_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32()
    if flag__0 {
        _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(m__1, "a", 7)
    } else {}
    var mtmp63 Option__int32 = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__string____V__int32(m__1, "a")
    var jp72 int32
    switch mtmp63.(type) {
    case None:
        retv69 = None{}
        return retv69
    case Some:
        var x64 int32 = mtmp63.(Some)._0
        var try_value__18 int32 = x64
        jp72 = try_value__18
        var value__2 int32 = jp72
        var t73 int32 = value__2 + 1
        var t74 Option__int32 = Some{
            _0: t73,
        }
        retv69 = t74
        return retv69
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__3 Option__int32) string {
    var retv76 string
    var jp78 string
    switch opt__3.(type) {
    case None:
        jp78 = "none"
    case Some:
        var x65 int32 = opt__3.(Some)._0
        var value__4 int32 = x65
        var t79 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__4)
        var t80 string = "some=" + t79
        jp78 = t80
    default:
        panic("non-exhaustive match")
    }
    retv76 = jp78
    return retv76
}

func main0() struct{} {
    var t82 Option__int32 = fetch(true)
    var t83 string = show(t82)
    println__T_string(t83)
    var t84 Option__int32 = fetch(false)
    var t85 string = show(t84)
    println__T_string(t85)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32() *hashmap_string_int32_x {
    var retv87 *hashmap_string_int32_x
    var t88 *hashmap_string_int32_x = hashmap_new__HashMap_6string_5int32()
    retv87 = t88
    return retv87
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(self__195 *hashmap_string_int32_x, key__196 string, value__197 int32) struct{} {
    hashmap_set__HashMap_6string_5int32(self__195, key__196, value__197)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__string____V__int32(self__193 *hashmap_string_int32_x, key__194 string) Option__int32 {
    var retv92 Option__int32
    var t93 Option__int32 = hashmap_get__HashMap_6string_5int32(self__193, key__194)
    retv92 = t93
    return retv92
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv95 string
    var t96 string = _goml_runtime_core_int32_to_string(self__5)
    retv95 = t96
    return retv95
}

func println__T_string(value__1 string) struct{} {
    var t98 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t98)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv101 string
    retv101 = self__37
    return retv101
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__53 string, other__54 string) bool {
    var retv103 bool
    var t104 bool = self__53 == other__54
    retv103 = t104
    return retv103
}

func _goml_m_trait__impl_i_Hash_i_string_i_hash(self__79 string) uint64 {
    var retv106 uint64
    var t107 uint64 = _goml_runtime_core_string_hash(self__79)
    retv106 = t107
    return retv106
}

func main() {
    main0()
}
