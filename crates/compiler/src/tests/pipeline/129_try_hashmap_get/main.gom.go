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
    len int32
}

func hashmap_new__HashMap_6string_5int32() *hashmap_string_int32_x {
    return &hashmap_string_int32_x{
        buckets: make(map[uint64][]hashmap_string_int32_x_entry),
        len: 0,
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
    if m == nil {
        return struct{}{}
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
            bucket[i].value = value
            return struct{}{}
        }
        i = i + 1
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
    var retv12 Option__int32
    var m__1 *hashmap_string_int32_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32()
    if flag__0 {
        _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(m__1, "a", 7)
    } else {}
    var mtmp6 Option__int32 = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__string____V__int32(m__1, "a")
    var jp15 int32
    switch mtmp6.(type) {
    case None:
        retv12 = None{}
        return retv12
    case Some:
        var x7 int32 = mtmp6.(Some)._0
        var try_value__18 int32 = x7
        jp15 = try_value__18
        var value__2 int32 = jp15
        var t16 int32 = value__2 + 1
        var t17 Option__int32 = Some{
            _0: t16,
        }
        retv12 = t17
        return retv12
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__3 Option__int32) string {
    var retv19 string
    var jp21 string
    switch opt__3.(type) {
    case None:
        jp21 = "none"
    case Some:
        var x8 int32 = opt__3.(Some)._0
        var value__4 int32 = x8
        var t22 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__4)
        var t23 string = "some=" + t22
        jp21 = t23
    default:
        panic("non-exhaustive match")
    }
    retv19 = jp21
    return retv19
}

func main0() struct{} {
    var t25 Option__int32 = fetch(true)
    var t26 string = show(t25)
    println__T_string(t26)
    var t27 Option__int32 = fetch(false)
    var t28 string = show(t27)
    println__T_string(t28)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32() *hashmap_string_int32_x {
    var retv30 *hashmap_string_int32_x
    var t31 *hashmap_string_int32_x = hashmap_new__HashMap_6string_5int32()
    retv30 = t31
    return retv30
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(self__94 *hashmap_string_int32_x, key__95 string, value__96 int32) struct{} {
    hashmap_set__HashMap_6string_5int32(self__94, key__95, value__96)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__string____V__int32(self__92 *hashmap_string_int32_x, key__93 string) Option__int32 {
    var retv35 Option__int32
    var t36 Option__int32 = hashmap_get__HashMap_6string_5int32(self__92, key__93)
    retv35 = t36
    return retv35
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv38 string
    var t39 string = _goml_runtime_core_int32_to_string(self__2)
    retv38 = t39
    return retv38
}

func println__T_string(value__1 string) struct{} {
    var t41 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t41)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv44 string
    retv44 = self__9
    return retv44
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__25 string, other__26 string) bool {
    var retv46 bool
    var t47 bool = self__25 == other__26
    retv46 = t47
    return retv46
}

func _goml_m_trait__impl_i_Hash_i_string_i_hash(self__51 string) uint64 {
    var retv49 uint64
    var t50 uint64 = _goml_runtime_core_string_hash(self__51)
    retv49 = t50
    return retv49
}

func main() {
    main0()
}
