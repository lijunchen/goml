package main

import (
    _goml_fmt "fmt"
)

func int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func string_hash(s string) uint64 {
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

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func _goml_trait_x5f_impl_x23_Eq_x23_string_x23_eq(self string, other string) bool {
    return self == other
}

func _goml_trait_x5f_impl_x23_Hash_x23_string_x23_hash(self string) uint64 {
    return string_hash(self)
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

func hashmap_get_native__HashMap_6string_5int32(m *hashmap_string_int32_x, key string) (int32, bool) {
    if m == nil {
        var zero int32
        return zero, false
    }
    var h uint64 = _goml_trait_x5f_impl_x23_Hash_x23_string_x23_hash(key)
    var bucket []hashmap_string_int32_x_entry = m.buckets[h]
    var i int32 = 0
    for {
        if i >= int32(len(bucket)) {
            break
        }
        var entry hashmap_string_int32_x_entry = bucket[i]
        if entry.active && _goml_trait_x5f_impl_x23_Eq_x23_string_x23_eq(entry.key, key) {
            return entry.value, true
        }
        i = i + 1
    }
    var zero int32
    return zero, false
}

func hashmap_set__HashMap_6string_5int32(m *hashmap_string_int32_x, key string, value int32) struct{} {
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_trait_x5f_impl_x23_Hash_x23_string_x23_hash(key)
    var bucket []hashmap_string_int32_x_entry = m.buckets[h]
    var i int32 = 0
    for {
        if i >= int32(len(bucket)) {
            break
        }
        var entry hashmap_string_int32_x_entry = bucket[i]
        if entry.active && _goml_trait_x5f_impl_x23_Eq_x23_string_x23_eq(entry.key, key) {
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

type GoError = error

func fetch__native(flag__0 bool) (int32, bool) {
    var m__1 *hashmap_string_int32_x = _goml_inherent_x23_HashMap_x23_HashMap_x5b_K_x2c_V_x5d__x23_new_x5f__x5f_K_x5f_string_x5f__x5f_V_x5f_int32()
    if flag__0 {
        _goml_inherent_x23_HashMap_x23_HashMap_x5b_K_x2c_V_x5d__x23_set_x5f__x5f_K_x5f_string_x5f__x5f_V_x5f_int32(m__1, "a", 7)
    } else {}
    var jp11 int32
    var mtmp2_value_0 int32
    var mtmp2_ok bool
    mtmp2_value_0, mtmp2_ok = _goml_inherent_x23_HashMap_x23_HashMap_x5b_K_x2c_V_x5d__x23_get_x5f__x5f_K_x5f_string_x5f__x5f_V_x5f_int32_x5f__x5f_native(m__1, "a")
    if !mtmp2_ok {
        var ret_zero int32
        return ret_zero, false
    }
    jp11 = mtmp2_value_0
    var value__2 int32 = jp11
    var t12 int32 = value__2 + 1
    return t12, true
}

func fetch(flag__0 bool) Option__int32 {
    var native_value_0 int32
    var native_ok bool
    native_value_0, native_ok = fetch__native(flag__0)
    if native_ok {
        return Some{
            _0: native_value_0,
        }
    }
    return None{}
}

func show(opt__3 Option__int32) string {
    var retv15 string
    var jp17 string
    switch opt__3.(type) {
    case None:
        jp17 = "none"
    case Some:
        var x4 int32 = opt__3.(Some)._0
        var value__4 int32 = x4
        var t18 string = int32_to_string(value__4)
        var t19 string = "some=" + t18
        jp17 = t19
    default:
        panic("non-exhaustive match")
    }
    retv15 = jp17
    return retv15
}

func main0() struct{} {
    var t21 Option__int32 = fetch(true)
    var t22 string = show(t21)
    println__T_string(t22)
    var t23 Option__int32 = fetch(false)
    var t24 string = show(t23)
    println__T_string(t24)
    return struct{}{}
}

func _goml_inherent_x23_HashMap_x23_HashMap_x5b_K_x2c_V_x5d__x23_new_x5f__x5f_K_x5f_string_x5f__x5f_V_x5f_int32() *hashmap_string_int32_x {
    var retv26 *hashmap_string_int32_x
    var t27 *hashmap_string_int32_x = hashmap_new__HashMap_6string_5int32()
    retv26 = t27
    return retv26
}

func _goml_inherent_x23_HashMap_x23_HashMap_x5b_K_x2c_V_x5d__x23_set_x5f__x5f_K_x5f_string_x5f__x5f_V_x5f_int32(self__86 *hashmap_string_int32_x, key__87 string, value__88 int32) struct{} {
    hashmap_set__HashMap_6string_5int32(self__86, key__87, value__88)
    return struct{}{}
}

func _goml_inherent_x23_HashMap_x23_HashMap_x5b_K_x2c_V_x5d__x23_get_x5f__x5f_K_x5f_string_x5f__x5f_V_x5f_int32_x5f__x5f_native(self__84 *hashmap_string_int32_x, key__85 string) (int32, bool) {
    var t32_value int32
    var t32_ok bool
    t32_value, t32_ok = hashmap_get_native__HashMap_6string_5int32(self__84, key__85)
    if !t32_ok {
        var ret_zero int32
        return ret_zero, false
    }
    return t32_value, true
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
