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
    var retv66 Option__int32
    var m__1 *hashmap_string_int32_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32()
    if flag__0 {
        _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(m__1, "a", 7)
    } else {}
    var mtmp60 Option__int32 = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__string____V__int32(m__1, "a")
    var jp69 int32
    switch mtmp60.(type) {
    case None:
        retv66 = None{}
        return retv66
    case Some:
        var x61 int32 = mtmp60.(Some)._0
        var try_value__18 int32 = x61
        jp69 = try_value__18
        var value__2 int32 = jp69
        var t70 int32 = value__2 + 1
        var t71 Option__int32 = Some{
            _0: t70,
        }
        retv66 = t71
        return retv66
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__3 Option__int32) string {
    var retv73 string
    var jp75 string
    switch opt__3.(type) {
    case None:
        jp75 = "none"
    case Some:
        var x62 int32 = opt__3.(Some)._0
        var value__4 int32 = x62
        var t76 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__4)
        var t77 string = "some=" + t76
        jp75 = t77
    default:
        panic("non-exhaustive match")
    }
    retv73 = jp75
    return retv73
}

func main0() struct{} {
    var t79 Option__int32 = fetch(true)
    var t80 string = show(t79)
    println__T_string(t80)
    var t81 Option__int32 = fetch(false)
    var t82 string = show(t81)
    println__T_string(t82)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32() *hashmap_string_int32_x {
    var retv84 *hashmap_string_int32_x
    var t85 *hashmap_string_int32_x = hashmap_new__HashMap_6string_5int32()
    retv84 = t85
    return retv84
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(self__191 *hashmap_string_int32_x, key__192 string, value__193 int32) struct{} {
    hashmap_set__HashMap_6string_5int32(self__191, key__192, value__193)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__string____V__int32(self__189 *hashmap_string_int32_x, key__190 string) Option__int32 {
    var retv89 Option__int32
    var t90 Option__int32 = hashmap_get__HashMap_6string_5int32(self__189, key__190)
    retv89 = t90
    return retv89
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv92 string
    var t93 string = _goml_runtime_core_int32_to_string(self__2)
    retv92 = t93
    return retv92
}

func println__T_string(value__1 string) struct{} {
    var t95 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t95)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv98 string
    retv98 = self__34
    return retv98
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__50 string, other__51 string) bool {
    var retv100 bool
    var t101 bool = self__50 == other__51
    retv100 = t101
    return retv100
}

func _goml_m_trait__impl_i_Hash_i_string_i_hash(self__76 string) uint64 {
    var retv103 uint64
    var t104 uint64 = _goml_runtime_core_string_hash(self__76)
    retv103 = t104
    return retv103
}

func main() {
    main0()
}
