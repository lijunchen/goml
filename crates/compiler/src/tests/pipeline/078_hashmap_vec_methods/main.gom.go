package main

import (
    "fmt"
)

func bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func int32_to_string(x int32) string {
    return fmt.Sprintf("%d", x)
}

func int32_hash(x int32) uint64 {
    return uint64(x)
}

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

func _goml_trait_impl_Eq_int32_eq(self int32, other int32) bool {
    return self == other
}

func _goml_trait_impl_Hash_int32_hash(self int32) uint64 {
    return int32_hash(self)
}

type hashmap_key_int32_x_entry struct {
    active bool
    key Key
    value int32
}

type hashmap_key_int32_x struct {
    buckets map[uint64][]hashmap_key_int32_x_entry
    len int32
}

func hashmap_new__HashMap_Key_int32() *hashmap_key_int32_x {
    return &hashmap_key_int32_x{
        buckets: make(map[uint64][]hashmap_key_int32_x_entry),
        len: 0,
    }
}

func hashmap_len__HashMap_Key_int32(m *hashmap_key_int32_x) int32 {
    if m == nil {
        return 0
    }
    return m.len
}

func hashmap_get__HashMap_Key_int32(m *hashmap_key_int32_x, key Key) Option__int32 {
    if m == nil {
        return None{}
    }
    var h uint64 = _goml_trait_impl_Hash_Key_hash(key)
    var bucket []hashmap_key_int32_x_entry = m.buckets[h]
    var i int32 = 0
    for {
        if i >= int32(len(bucket)) {
            break
        }
        var entry hashmap_key_int32_x_entry = bucket[i]
        if entry.active && _goml_trait_impl_Eq_Key_eq(entry.key, key) {
            return Some{
                _0: entry.value,
            }
        }
        i = i + 1
    }
    return None{}
}

func hashmap_set__HashMap_Key_int32(m *hashmap_key_int32_x, key Key, value int32) struct{} {
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_trait_impl_Hash_Key_hash(key)
    var bucket []hashmap_key_int32_x_entry = m.buckets[h]
    var i int32 = 0
    for {
        if i >= int32(len(bucket)) {
            break
        }
        var entry hashmap_key_int32_x_entry = bucket[i]
        if entry.active && _goml_trait_impl_Eq_Key_eq(entry.key, key) {
            bucket[i].value = value
            return struct{}{}
        }
        i = i + 1
    }
    bucket = append(bucket, hashmap_key_int32_x_entry{
        active: true,
        key: key,
        value: value,
    })
    m.buckets[h] = bucket
    m.len = m.len + 1
    return struct{}{}
}

func hashmap_remove__HashMap_Key_int32(m *hashmap_key_int32_x, key Key) struct{} {
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_trait_impl_Hash_Key_hash(key)
    var bucket []hashmap_key_int32_x_entry = m.buckets[h]
    var i int32 = 0
    for {
        if i >= int32(len(bucket)) {
            break
        }
        var entry hashmap_key_int32_x_entry = bucket[i]
        if entry.active && _goml_trait_impl_Eq_Key_eq(entry.key, key) {
            bucket[i].active = false
            m.len = m.len - 1
            return struct{}{}
        }
        i = i + 1
    }
    return struct{}{}
}

func hashmap_contains__HashMap_Key_int32(m *hashmap_key_int32_x, key Key) bool {
    if m == nil {
        return false
    }
    var h uint64 = _goml_trait_impl_Hash_Key_hash(key)
    var bucket []hashmap_key_int32_x_entry = m.buckets[h]
    var i int32 = 0
    for {
        if i >= int32(len(bucket)) {
            break
        }
        var entry hashmap_key_int32_x_entry = bucket[i]
        if entry.active && _goml_trait_impl_Eq_Key_eq(entry.key, key) {
            return true
        }
        i = i + 1
    }
    return false
}

type Tuple2_Key_Key struct {
    _0 Key
    _1 Key
}

type Key interface {
    isKey()
}

type A struct {}

func (_ A) isKey() {}

type B struct {
    _0 int32
}

func (_ B) isKey() {}

type Option__int32 interface {
    isOption__int32()
}

type None struct {}

func (_ None) isOption__int32() {}

type Some struct {
    _0 int32
}

func (_ Some) isOption__int32() {}

func _goml_trait_impl_Eq_Key_eq(self__0 Key, other__1 Key) bool {
    var ret40 bool
    var mtmp0 Tuple2_Key_Key = Tuple2_Key_Key{
        _0: self__0,
        _1: other__1,
    }
    var x1 Key = mtmp0._0
    var x2 Key = mtmp0._1
    switch x2 := x2.(type) {
    case A:
        switch x1.(type) {
        case A:
            ret40 = true
        case B:
            ret40 = false
        }
    case B:
        var x3 int32 = x2._0
        switch x1 := x1.(type) {
        case A:
            ret40 = false
        case B:
            var x5 int32 = x1._0
            var __l1_0__2 int32 = x5
            var __r1_0__3 int32 = x3
            var t20 bool = _goml_trait_impl_Eq_int32_eq(__l1_0__2, __r1_0__3)
            ret40 = true && t20
        }
    }
    return ret40
}

func _goml_trait_impl_Hash_Key_hash(self__4 Key) uint64 {
    var ret41 uint64
    switch self__4 := self__4.(type) {
    case A:
        var h__5 uint64 = 14695981039346656037 + 1
        ret41 = h__5
    case B:
        var x6 int32 = self__4._0
        var __field1_0__6 int32 = x6
        var h__7 uint64 = 14695981039346656037 + 2
        var t21 uint64 = h__7 * 1099511628211
        var t22 uint64 = _goml_trait_impl_Hash_int32_hash(__field1_0__6)
        var h__8 uint64 = t21 + t22
        ret41 = h__8
    }
    return ret41
}

func print_opt_int(x__9 Option__int32) struct{} {
    var ret42 struct{}
    switch x__9 := x__9.(type) {
    case None:
        ret42 = println__T_string("none")
    case Some:
        var x7 int32 = x__9._0
        var v__10 int32 = x7
        ret42 = println__T_int32(v__10)
    }
    return ret42
}

func main0() struct{} {
    var ret43 struct{}
    var v__11 []int32 = nil
    var v__12 []int32 = append(v__11, 10)
    var v__13 []int32 = append(v__12, 20)
    var v__14 []int32 = append(v__13, 30)
    var t23 int32 = v__14[0]
    println__T_int32(t23)
    var t24 int32 = v__14[1]
    println__T_int32(t24)
    var t25 int32 = v__14[2]
    println__T_int32(t25)
    var t26 int32 = int32(len(v__14))
    println__T_int32(t26)
    var m__15 *hashmap_key_int32_x = hashmap_new__HashMap_Key_int32()
    var t27 Key = A{}
    hashmap_set__HashMap_Key_int32(m__15, t27, 10)
    var t28 Key = B{
        _0: 1,
    }
    hashmap_set__HashMap_Key_int32(m__15, t28, 20)
    var t29 int32 = hashmap_len__HashMap_Key_int32(m__15)
    println__T_int32(t29)
    var t31 Key = A{}
    var t30 Option__int32 = hashmap_get__HashMap_Key_int32(m__15, t31)
    print_opt_int(t30)
    var t33 Key = B{
        _0: 1,
    }
    var t32 bool = hashmap_contains__HashMap_Key_int32(m__15, t33)
    println__T_bool(t32)
    var t34 Key = B{
        _0: 1,
    }
    hashmap_remove__HashMap_Key_int32(m__15, t34)
    var t36 Key = B{
        _0: 1,
    }
    var t35 bool = hashmap_contains__HashMap_Key_int32(m__15, t36)
    println__T_bool(t35)
    var t37 int32 = hashmap_len__HashMap_Key_int32(m__15)
    println__T_int32(t37)
    ret43 = struct{}{}
    return ret43
}

func println__T_string(value__1 string) struct{} {
    var ret44 struct{}
    ret44 = string_println(value__1)
    return ret44
}

func println__T_int32(value__1 int32) struct{} {
    var ret45 struct{}
    var t38 string = int32_to_string(value__1)
    ret45 = string_println(t38)
    return ret45
}

func println__T_bool(value__1 bool) struct{} {
    var ret46 struct{}
    var t39 string = bool_to_string(value__1)
    ret46 = string_println(t39)
    return ret46
}

func main() {
    main0()
}
