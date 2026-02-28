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
    var mtmp0 Tuple2_Key_Key
    var x1 Key
    var x2 Key
    var jp21 bool
    var jp23 bool
    var x3 int32
    var jp25 bool
    var x5 int32
    var __l1_0__2 int32
    var __r1_0__3 int32
    var t26 bool
    var t27 bool
    mtmp0 = Tuple2_Key_Key{
        _0: self__0,
        _1: other__1,
    }
    x1 = mtmp0._0
    x2 = mtmp0._1
    switch x2.(type) {
    case A:
        goto b2
    case B:
        goto b6
    default:
        panic("non-exhaustive match")
    }
    b1:
    return jp21
    b2:
    switch x1.(type) {
    case A:
        goto b4
    case B:
        goto b5
    default:
        panic("non-exhaustive match")
    }
    b3:
    jp21 = jp23
    goto b1
    b4:
    jp23 = true
    goto b3
    b5:
    jp23 = false
    goto b3
    b6:
    x3 = x2.(B)._0
    switch x1.(type) {
    case A:
        goto b8
    case B:
        goto b9
    default:
        panic("non-exhaustive match")
    }
    b7:
    jp21 = jp25
    goto b1
    b8:
    jp25 = false
    goto b7
    b9:
    x5 = x1.(B)._0
    __l1_0__2 = x5
    __r1_0__3 = x3
    t26 = _goml_trait_impl_Eq_int32_eq(__l1_0__2, __r1_0__3)
    t27 = true && t26
    jp25 = t27
    goto b7
}

func _goml_trait_impl_Hash_Key_hash(self__4 Key) uint64 {
    var jp29 uint64
    var h__5 uint64
    var x6 int32
    var __field1_0__6 int32
    var h__7 uint64
    var t30 uint64
    var t31 uint64
    var h__8 uint64
    switch self__4.(type) {
    case A:
        goto b2
    case B:
        goto b3
    default:
        panic("non-exhaustive match")
    }
    b1:
    return jp29
    b2:
    h__5 = 14695981039346656037 + 1
    jp29 = h__5
    goto b1
    b3:
    x6 = self__4.(B)._0
    __field1_0__6 = x6
    h__7 = 14695981039346656037 + 2
    t30 = h__7 * 1099511628211
    t31 = _goml_trait_impl_Hash_int32_hash(__field1_0__6)
    h__8 = t30 + t31
    jp29 = h__8
    goto b1
}

func print_opt_int(x__9 Option__int32) struct{} {
    var x7 int32
    var v__10 int32
    switch x__9.(type) {
    case None:
        goto b2
    case Some:
        goto b3
    default:
        panic("non-exhaustive match")
    }
    b1:
    return struct{}{}
    b2:
    println__T_string("none")
    goto b1
    b3:
    x7 = x__9.(Some)._0
    v__10 = x7
    println__T_int32(v__10)
    goto b1
}

func main0() struct{} {
    var v__11 []int32
    var v__12 []int32
    var v__13 []int32
    var v__14 []int32
    var t35 int32
    var t36 int32
    var t37 int32
    var t38 int32
    var m__15 *hashmap_key_int32_x
    var t39 Key
    var t40 int32
    var t41 Option__int32
    var t42 Key
    var t43 bool
    var t44 Key
    var t45 Key
    var t46 bool
    var t47 int32
    v__11 = _goml_inherent_Vec_Vec_x5b_T_x5d__new__T_int32()
    v__12 = _goml_inherent_Vec_Vec_x5b_T_x5d__push__T_int32(v__11, 10)
    v__13 = _goml_inherent_Vec_Vec_x5b_T_x5d__push__T_int32(v__12, 20)
    v__14 = _goml_inherent_Vec_Vec_x5b_T_x5d__push__T_int32(v__13, 30)
    t35 = _goml_inherent_Vec_Vec_x5b_T_x5d__get__T_int32(v__14, 0)
    println__T_int32(t35)
    t36 = _goml_inherent_Vec_Vec_x5b_T_x5d__get__T_int32(v__14, 1)
    println__T_int32(t36)
    t37 = _goml_inherent_Vec_Vec_x5b_T_x5d__get__T_int32(v__14, 2)
    println__T_int32(t37)
    t38 = _goml_inherent_Vec_Vec_x5b_T_x5d__len__T_int32(v__14)
    println__T_int32(t38)
    m__15 = _goml_inherent_HashMap_HashMap_x5b_K_x2c_V_x5d__new__K_Key__V_int32()
    _goml_inherent_HashMap_HashMap_x5b_K_x2c_V_x5d__set__K_Key__V_int32(m__15, A{}, 10)
    t39 = B{
        _0: 1,
    }
    _goml_inherent_HashMap_HashMap_x5b_K_x2c_V_x5d__set__K_Key__V_int32(m__15, t39, 20)
    t40 = _goml_inherent_HashMap_HashMap_x5b_K_x2c_V_x5d__len__K_Key__V_int32(m__15)
    println__T_int32(t40)
    t41 = _goml_inherent_HashMap_HashMap_x5b_K_x2c_V_x5d__get__K_Key__V_int32(m__15, A{})
    print_opt_int(t41)
    t42 = B{
        _0: 1,
    }
    t43 = _goml_inherent_HashMap_HashMap_x5b_K_x2c_V_x5d__contains__K_Key__V_int32(m__15, t42)
    println__T_bool(t43)
    t44 = B{
        _0: 1,
    }
    _goml_inherent_HashMap_HashMap_x5b_K_x2c_V_x5d__remove__K_Key__V_int32(m__15, t44)
    t45 = B{
        _0: 1,
    }
    t46 = _goml_inherent_HashMap_HashMap_x5b_K_x2c_V_x5d__contains__K_Key__V_int32(m__15, t45)
    println__T_bool(t46)
    t47 = _goml_inherent_HashMap_HashMap_x5b_K_x2c_V_x5d__len__K_Key__V_int32(m__15)
    println__T_int32(t47)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t48 struct{}
    t48 = string_println(value__1)
    return t48
}

func println__T_int32(value__1 int32) struct{} {
    var t49 string
    var t50 struct{}
    t49 = int32_to_string(value__1)
    t50 = string_println(t49)
    return t50
}

func _goml_inherent_Vec_Vec_x5b_T_x5d__new__T_int32() []int32 {
    var t51 []int32
    t51 = nil
    return t51
}

func _goml_inherent_Vec_Vec_x5b_T_x5d__push__T_int32(self__66 []int32, elem__67 int32) []int32 {
    var t52 []int32
    t52 = append(self__66, elem__67)
    return t52
}

func _goml_inherent_Vec_Vec_x5b_T_x5d__get__T_int32(self__68 []int32, index__69 int32) int32 {
    var t53 int32
    t53 = self__68[index__69]
    return t53
}

func _goml_inherent_Vec_Vec_x5b_T_x5d__len__T_int32(self__70 []int32) int32 {
    var t54 int32
    t54 = int32(len(self__70))
    return t54
}

func _goml_inherent_HashMap_HashMap_x5b_K_x2c_V_x5d__new__K_Key__V_int32() *hashmap_key_int32_x {
    var t55 *hashmap_key_int32_x
    t55 = hashmap_new__HashMap_Key_int32()
    return t55
}

func _goml_inherent_HashMap_HashMap_x5b_K_x2c_V_x5d__set__K_Key__V_int32(self__79 *hashmap_key_int32_x, key__80 Key, value__81 int32) struct{} {
    var t56 struct{}
    t56 = hashmap_set__HashMap_Key_int32(self__79, key__80, value__81)
    return t56
}

func _goml_inherent_HashMap_HashMap_x5b_K_x2c_V_x5d__len__K_Key__V_int32(self__84 *hashmap_key_int32_x) int32 {
    var t57 int32
    t57 = hashmap_len__HashMap_Key_int32(self__84)
    return t57
}

func _goml_inherent_HashMap_HashMap_x5b_K_x2c_V_x5d__get__K_Key__V_int32(self__77 *hashmap_key_int32_x, key__78 Key) Option__int32 {
    var t58 Option__int32
    t58 = hashmap_get__HashMap_Key_int32(self__77, key__78)
    return t58
}

func _goml_inherent_HashMap_HashMap_x5b_K_x2c_V_x5d__contains__K_Key__V_int32(self__85 *hashmap_key_int32_x, key__86 Key) bool {
    var t59 bool
    t59 = hashmap_contains__HashMap_Key_int32(self__85, key__86)
    return t59
}

func println__T_bool(value__1 bool) struct{} {
    var t60 string
    var t61 struct{}
    t60 = bool_to_string(value__1)
    t61 = string_println(t60)
    return t61
}

func _goml_inherent_HashMap_HashMap_x5b_K_x2c_V_x5d__remove__K_Key__V_int32(self__82 *hashmap_key_int32_x, key__83 Key) struct{} {
    var t62 struct{}
    t62 = hashmap_remove__HashMap_Key_int32(self__82, key__83)
    return t62
}

func main() {
    main0()
}
