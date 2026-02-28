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
    var mtmp0 Tuple2_Key_Key = Tuple2_Key_Key{
        _0: self__0,
        _1: other__1,
    }
    var x1 Key = mtmp0._0
    var x2 Key = mtmp0._1
    var jp21 bool
    switch x2.(type) {
    case A:
        var jp23 bool
        switch x1.(type) {
        case A:
            jp23 = true
        case B:
            jp23 = false
        default:
            panic("non-exhaustive match")
        }
        jp21 = jp23
        return jp21
    case B:
        var x3 int32 = x2.(B)._0
        var jp25 bool
        switch x1.(type) {
        case A:
            jp25 = false
        case B:
            var x5 int32 = x1.(B)._0
            var __l1_0__2 int32 = x5
            var __r1_0__3 int32 = x3
            var t26 bool = _goml_trait_impl_Eq_int32_eq(__l1_0__2, __r1_0__3)
            var t27 bool = true && t26
            jp25 = t27
        default:
            panic("non-exhaustive match")
        }
        jp21 = jp25
        return jp21
    default:
        panic("non-exhaustive match")
    }
}

func _goml_trait_impl_Hash_Key_hash(self__4 Key) uint64 {
    var jp29 uint64
    switch self__4.(type) {
    case A:
        var h__5 uint64 = 14695981039346656037 + 1
        jp29 = h__5
    case B:
        var x6 int32 = self__4.(B)._0
        var __field1_0__6 int32 = x6
        var h__7 uint64 = 14695981039346656037 + 2
        var t30 uint64 = h__7 * 1099511628211
        var t31 uint64 = _goml_trait_impl_Hash_int32_hash(__field1_0__6)
        var h__8 uint64 = t30 + t31
        jp29 = h__8
    default:
        panic("non-exhaustive match")
    }
    return jp29
}

func print_opt_int(x__9 Option__int32) struct{} {
    switch x__9.(type) {
    case None:
        println__T_string("none")
    case Some:
        var x7 int32 = x__9.(Some)._0
        var v__10 int32 = x7
        println__T_int32(v__10)
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func main0() struct{} {
    var v__11 []int32 = _goml_inherent_Vec_Vec_x5b_T_x5d__new__T_int32()
    var v__12 []int32 = _goml_inherent_Vec_Vec_x5b_T_x5d__push__T_int32(v__11, 10)
    var v__13 []int32 = _goml_inherent_Vec_Vec_x5b_T_x5d__push__T_int32(v__12, 20)
    var v__14 []int32 = _goml_inherent_Vec_Vec_x5b_T_x5d__push__T_int32(v__13, 30)
    var t35 int32 = _goml_inherent_Vec_Vec_x5b_T_x5d__get__T_int32(v__14, 0)
    println__T_int32(t35)
    var t36 int32 = _goml_inherent_Vec_Vec_x5b_T_x5d__get__T_int32(v__14, 1)
    println__T_int32(t36)
    var t37 int32 = _goml_inherent_Vec_Vec_x5b_T_x5d__get__T_int32(v__14, 2)
    println__T_int32(t37)
    var t38 int32 = _goml_inherent_Vec_Vec_x5b_T_x5d__len__T_int32(v__14)
    println__T_int32(t38)
    var m__15 *hashmap_key_int32_x = _goml_inherent_HashMap_HashMap_x5b_K_x2c_V_x5d__new__K_Key__V_int32()
    _goml_inherent_HashMap_HashMap_x5b_K_x2c_V_x5d__set__K_Key__V_int32(m__15, A{}, 10)
    var t39 Key = B{
        _0: 1,
    }
    _goml_inherent_HashMap_HashMap_x5b_K_x2c_V_x5d__set__K_Key__V_int32(m__15, t39, 20)
    var t40 int32 = _goml_inherent_HashMap_HashMap_x5b_K_x2c_V_x5d__len__K_Key__V_int32(m__15)
    println__T_int32(t40)
    var t41 Option__int32 = _goml_inherent_HashMap_HashMap_x5b_K_x2c_V_x5d__get__K_Key__V_int32(m__15, A{})
    print_opt_int(t41)
    var t42 Key = B{
        _0: 1,
    }
    var t43 bool = _goml_inherent_HashMap_HashMap_x5b_K_x2c_V_x5d__contains__K_Key__V_int32(m__15, t42)
    println__T_bool(t43)
    var t44 Key = B{
        _0: 1,
    }
    _goml_inherent_HashMap_HashMap_x5b_K_x2c_V_x5d__remove__K_Key__V_int32(m__15, t44)
    var t45 Key = B{
        _0: 1,
    }
    var t46 bool = _goml_inherent_HashMap_HashMap_x5b_K_x2c_V_x5d__contains__K_Key__V_int32(m__15, t45)
    println__T_bool(t46)
    var t47 int32 = _goml_inherent_HashMap_HashMap_x5b_K_x2c_V_x5d__len__K_Key__V_int32(m__15)
    println__T_int32(t47)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t48 struct{} = string_println(value__1)
    return t48
}

func println__T_int32(value__1 int32) struct{} {
    var t49 string = int32_to_string(value__1)
    var t50 struct{} = string_println(t49)
    return t50
}

func _goml_inherent_Vec_Vec_x5b_T_x5d__new__T_int32() []int32 {
    var t51 []int32 = nil
    return t51
}

func _goml_inherent_Vec_Vec_x5b_T_x5d__push__T_int32(self__66 []int32, elem__67 int32) []int32 {
    var t52 []int32 = append(self__66, elem__67)
    return t52
}

func _goml_inherent_Vec_Vec_x5b_T_x5d__get__T_int32(self__68 []int32, index__69 int32) int32 {
    var t53 int32 = self__68[index__69]
    return t53
}

func _goml_inherent_Vec_Vec_x5b_T_x5d__len__T_int32(self__70 []int32) int32 {
    var t54 int32 = int32(len(self__70))
    return t54
}

func _goml_inherent_HashMap_HashMap_x5b_K_x2c_V_x5d__new__K_Key__V_int32() *hashmap_key_int32_x {
    var t55 *hashmap_key_int32_x = hashmap_new__HashMap_Key_int32()
    return t55
}

func _goml_inherent_HashMap_HashMap_x5b_K_x2c_V_x5d__set__K_Key__V_int32(self__79 *hashmap_key_int32_x, key__80 Key, value__81 int32) struct{} {
    var t56 struct{} = hashmap_set__HashMap_Key_int32(self__79, key__80, value__81)
    return t56
}

func _goml_inherent_HashMap_HashMap_x5b_K_x2c_V_x5d__len__K_Key__V_int32(self__84 *hashmap_key_int32_x) int32 {
    var t57 int32 = hashmap_len__HashMap_Key_int32(self__84)
    return t57
}

func _goml_inherent_HashMap_HashMap_x5b_K_x2c_V_x5d__get__K_Key__V_int32(self__77 *hashmap_key_int32_x, key__78 Key) Option__int32 {
    var t58 Option__int32 = hashmap_get__HashMap_Key_int32(self__77, key__78)
    return t58
}

func _goml_inherent_HashMap_HashMap_x5b_K_x2c_V_x5d__contains__K_Key__V_int32(self__85 *hashmap_key_int32_x, key__86 Key) bool {
    var t59 bool = hashmap_contains__HashMap_Key_int32(self__85, key__86)
    return t59
}

func println__T_bool(value__1 bool) struct{} {
    var t60 string = bool_to_string(value__1)
    var t61 struct{} = string_println(t60)
    return t61
}

func _goml_inherent_HashMap_HashMap_x5b_K_x2c_V_x5d__remove__K_Key__V_int32(self__82 *hashmap_key_int32_x, key__83 Key) struct{} {
    var t62 struct{} = hashmap_remove__HashMap_Key_int32(self__82, key__83)
    return t62
}

func main() {
    main0()
}
