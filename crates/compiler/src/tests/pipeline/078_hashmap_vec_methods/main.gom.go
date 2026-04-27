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

func hashmap_get_native__HashMap_Key_int32(m *hashmap_key_int32_x, key Key) (int32, bool) {
    if m == nil {
        var zero int32
        return zero, false
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
            return entry.value, true
        }
        i = i + 1
    }
    var zero int32
    return zero, false
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

type GoError = error

func _goml_trait_impl_Eq_Key_eq(self__0 Key, other__1 Key) bool {
    var retv21 bool
    var mtmp0 Tuple2_Key_Key = Tuple2_Key_Key{
        _0: self__0,
        _1: other__1,
    }
    var x1 Key = mtmp0._0
    var x2 Key = mtmp0._1
    var jp23 bool
    switch x2.(type) {
    case A:
        var jp25 bool
        switch x1.(type) {
        case A:
            jp25 = true
        case B:
            jp25 = false
        default:
            panic("non-exhaustive match")
        }
        jp23 = jp25
    case B:
        var x3 int32 = x2.(B)._0
        var jp27 bool
        switch x1.(type) {
        case A:
            jp27 = false
        case B:
            var x5 int32 = x1.(B)._0
            var __l1_0__2 int32 = x5
            var __r1_0__3 int32 = x3
            var t28 bool = _goml_trait_impl_Eq_int32_eq(__l1_0__2, __r1_0__3)
            var t29 bool = true && t28
            jp27 = t29
        default:
            panic("non-exhaustive match")
        }
        jp23 = jp27
    default:
        panic("non-exhaustive match")
    }
    retv21 = jp23
    return retv21
}

func _goml_trait_impl_Hash_Key_hash(self__4 Key) uint64 {
    var retv31 uint64
    var jp33 uint64
    switch self__4.(type) {
    case A:
        var h__5 uint64 = 14695981039346656037 + 1
        jp33 = h__5
    case B:
        var x6 int32 = self__4.(B)._0
        var __field1_0__6 int32 = x6
        var h__7 uint64 = 14695981039346656037 + 2
        var t34 uint64 = h__7 * 1099511628211
        var t35 uint64 = _goml_trait_impl_Hash_int32_hash(__field1_0__6)
        var h__8 uint64 = t34 + t35
        jp33 = h__8
    default:
        panic("non-exhaustive match")
    }
    retv31 = jp33
    return retv31
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
    var t41 int32 = _goml_inherent_Vec_Vec_x5b_T_x5d__get__T_int32(v__14, 0)
    println__T_int32(t41)
    var t42 int32 = _goml_inherent_Vec_Vec_x5b_T_x5d__get__T_int32(v__14, 1)
    println__T_int32(t42)
    var t43 int32 = _goml_inherent_Vec_Vec_x5b_T_x5d__get__T_int32(v__14, 2)
    println__T_int32(t43)
    var t44 int32 = _goml_inherent_Vec_Vec_x5b_T_x5d__len__T_int32(v__14)
    println__T_int32(t44)
    var m__15 *hashmap_key_int32_x = _goml_inherent_HashMap_HashMap_x5b_K_x2c_V_x5d__new__K_Key__V_int32()
    _goml_inherent_HashMap_HashMap_x5b_K_x2c_V_x5d__set__K_Key__V_int32(m__15, A{}, 10)
    var t45 Key = B{
        _0: 1,
    }
    _goml_inherent_HashMap_HashMap_x5b_K_x2c_V_x5d__set__K_Key__V_int32(m__15, t45, 20)
    var t46 int32 = _goml_inherent_HashMap_HashMap_x5b_K_x2c_V_x5d__len__K_Key__V_int32(m__15)
    println__T_int32(t46)
    var t47 Option__int32 = _goml_inherent_HashMap_HashMap_x5b_K_x2c_V_x5d__get__K_Key__V_int32(m__15, A{})
    print_opt_int(t47)
    var t48 Key = B{
        _0: 1,
    }
    var t49 bool = _goml_inherent_HashMap_HashMap_x5b_K_x2c_V_x5d__contains__K_Key__V_int32(m__15, t48)
    println__T_bool(t49)
    var t50 Key = B{
        _0: 1,
    }
    _goml_inherent_HashMap_HashMap_x5b_K_x2c_V_x5d__remove__K_Key__V_int32(m__15, t50)
    var t51 Key = B{
        _0: 1,
    }
    var t52 bool = _goml_inherent_HashMap_HashMap_x5b_K_x2c_V_x5d__contains__K_Key__V_int32(m__15, t51)
    println__T_bool(t52)
    var t53 int32 = _goml_inherent_HashMap_HashMap_x5b_K_x2c_V_x5d__len__K_Key__V_int32(m__15)
    println__T_int32(t53)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t57 string = int32_to_string(value__1)
    string_println(t57)
    return struct{}{}
}

func _goml_inherent_Vec_Vec_x5b_T_x5d__new__T_int32() []int32 {
    var retv60 []int32
    var t61 []int32 = nil
    retv60 = t61
    return retv60
}

func _goml_inherent_Vec_Vec_x5b_T_x5d__push__T_int32(self__67 []int32, elem__68 int32) []int32 {
    var retv63 []int32
    var t64 []int32 = append(self__67, elem__68)
    retv63 = t64
    return retv63
}

func _goml_inherent_Vec_Vec_x5b_T_x5d__get__T_int32(self__69 []int32, index__70 int32) int32 {
    var retv66 int32
    var t67 int32 = self__69[index__70]
    retv66 = t67
    return retv66
}

func _goml_inherent_Vec_Vec_x5b_T_x5d__len__T_int32(self__74 []int32) int32 {
    var retv69 int32
    var t70 int32 = int32(len(self__74))
    retv69 = t70
    return retv69
}

func _goml_inherent_HashMap_HashMap_x5b_K_x2c_V_x5d__new__K_Key__V_int32() *hashmap_key_int32_x {
    var retv72 *hashmap_key_int32_x
    var t73 *hashmap_key_int32_x = hashmap_new__HashMap_Key_int32()
    retv72 = t73
    return retv72
}

func _goml_inherent_HashMap_HashMap_x5b_K_x2c_V_x5d__set__K_Key__V_int32(self__83 *hashmap_key_int32_x, key__84 Key, value__85 int32) struct{} {
    hashmap_set__HashMap_Key_int32(self__83, key__84, value__85)
    return struct{}{}
}

func _goml_inherent_HashMap_HashMap_x5b_K_x2c_V_x5d__len__K_Key__V_int32(self__88 *hashmap_key_int32_x) int32 {
    var retv77 int32
    var t78 int32 = hashmap_len__HashMap_Key_int32(self__88)
    retv77 = t78
    return retv77
}

func _goml_inherent_HashMap_HashMap_x5b_K_x2c_V_x5d__get__K_Key__V_int32__native(self__81 *hashmap_key_int32_x, key__82 Key) (int32, bool) {
    var t81_value int32
    var t81_ok bool
    t81_value, t81_ok = hashmap_get_native__HashMap_Key_int32(self__81, key__82)
    if !t81_ok {
        var ret_zero int32
        return ret_zero, false
    }
    return t81_value, true
}

func _goml_inherent_HashMap_HashMap_x5b_K_x2c_V_x5d__get__K_Key__V_int32(self__81 *hashmap_key_int32_x, key__82 Key) Option__int32 {
    var native_value_0 int32
    var native_ok bool
    native_value_0, native_ok = _goml_inherent_HashMap_HashMap_x5b_K_x2c_V_x5d__get__K_Key__V_int32__native(self__81, key__82)
    if native_ok {
        return Some{
            _0: native_value_0,
        }
    }
    return None{}
}

func println__T_bool(value__1 bool) struct{} {
    var t83 string = bool_to_string(value__1)
    string_println(t83)
    return struct{}{}
}

func _goml_inherent_HashMap_HashMap_x5b_K_x2c_V_x5d__contains__K_Key__V_int32(self__89 *hashmap_key_int32_x, key__90 Key) bool {
    var retv86 bool
    var t87 bool = hashmap_contains__HashMap_Key_int32(self__89, key__90)
    retv86 = t87
    return retv86
}

func _goml_inherent_HashMap_HashMap_x5b_K_x2c_V_x5d__remove__K_Key__V_int32(self__86 *hashmap_key_int32_x, key__87 Key) struct{} {
    hashmap_remove__HashMap_Key_int32(self__86, key__87)
    return struct{}{}
}

func main() {
    main0()
}
