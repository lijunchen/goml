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
    var retv30 Option__int32
    var m__1 *hashmap_string_int32_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32()
    if flag__0 {
        _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(m__1, "a", 7)
    } else {}
    var mtmp24 Option__int32 = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__string____V__int32(m__1, "a")
    var jp33 int32
    switch mtmp24.(type) {
    case None:
        retv30 = None{}
        return retv30
    case Some:
        var x25 int32 = mtmp24.(Some)._0
        var try_value__18 int32 = x25
        jp33 = try_value__18
        var value__2 int32 = jp33
        var t34 int32 = value__2 + 1
        var t35 Option__int32 = Some{
            _0: t34,
        }
        retv30 = t35
        return retv30
    default:
        panic("non-exhaustive match")
    }
}

func show(opt__3 Option__int32) string {
    var retv37 string
    var jp39 string
    switch opt__3.(type) {
    case None:
        jp39 = "none"
    case Some:
        var x26 int32 = opt__3.(Some)._0
        var value__4 int32 = x26
        var t40 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__4)
        var t41 string = "some=" + t40
        jp39 = t41
    default:
        panic("non-exhaustive match")
    }
    retv37 = jp39
    return retv37
}

func main0() struct{} {
    var t43 Option__int32 = fetch(true)
    var t44 string = show(t43)
    println__T_string(t44)
    var t45 Option__int32 = fetch(false)
    var t46 string = show(t45)
    println__T_string(t46)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32() *hashmap_string_int32_x {
    var retv48 *hashmap_string_int32_x
    var t49 *hashmap_string_int32_x = hashmap_new__HashMap_6string_5int32()
    retv48 = t49
    return retv48
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(self__132 *hashmap_string_int32_x, key__133 string, value__134 int32) struct{} {
    hashmap_set__HashMap_6string_5int32(self__132, key__133, value__134)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__string____V__int32(self__130 *hashmap_string_int32_x, key__131 string) Option__int32 {
    var retv53 Option__int32
    var t54 Option__int32 = hashmap_get__HashMap_6string_5int32(self__130, key__131)
    retv53 = t54
    return retv53
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv56 string
    var t57 string = _goml_runtime_core_int32_to_string(self__2)
    retv56 = t57
    return retv56
}

func println__T_string(value__1 string) struct{} {
    var t59 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t59)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv62 string
    retv62 = self__9
    return retv62
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__25 string, other__26 string) bool {
    var retv64 bool
    var t65 bool = self__25 == other__26
    retv64 = t65
    return retv64
}

func _goml_m_trait__impl_i_Hash_i_string_i_hash(self__51 string) uint64 {
    var retv67 uint64
    var t68 uint64 = _goml_runtime_core_string_hash(self__51)
    retv67 = t68
    return retv67
}

func main() {
    main0()
}
