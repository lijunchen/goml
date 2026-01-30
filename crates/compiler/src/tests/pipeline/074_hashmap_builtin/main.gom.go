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

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

type ref_point_x struct {
    value Point
}

func ref__Ref_Point(value Point) *ref_point_x {
    return &ref_point_x{
        value: value,
    }
}

type ref_key_x struct {
    value Key
}

func ref__Ref_Key(value Key) *ref_key_x {
    return &ref_key_x{
        value: value,
    }
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
    }
    return false
}

type hashmap_ref_point_int32_x_entry struct {
    active bool
    key *ref_point_x
    value int32
}

type hashmap_ref_point_int32_x struct {
    buckets map[uint64][]hashmap_ref_point_int32_x_entry
    len int32
}

func hashmap_new__HashMap_Ref_Point_int32() *hashmap_ref_point_int32_x {
    return &hashmap_ref_point_int32_x{
        buckets: make(map[uint64][]hashmap_ref_point_int32_x_entry),
        len: 0,
    }
}

func hashmap_get__HashMap_Ref_Point_int32(m *hashmap_ref_point_int32_x, key *ref_point_x) Option__int32 {
    if m == nil {
        return None{}
    }
    var h uint64 = _goml_trait_impl_Hash_Ref_x5b_Point_x5d__hash(key)
    var bucket []hashmap_ref_point_int32_x_entry = m.buckets[h]
    var i int32 = 0
    for {
        if i >= int32(len(bucket)) {
            break
        }
        var entry hashmap_ref_point_int32_x_entry = bucket[i]
        if entry.active && _goml_trait_impl_Eq_Ref_x5b_Point_x5d__eq(entry.key, key) {
            return Some{
                _0: entry.value,
            }
        }
    }
    return None{}
}

func hashmap_set__HashMap_Ref_Point_int32(m *hashmap_ref_point_int32_x, key *ref_point_x, value int32) struct{} {
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_trait_impl_Hash_Ref_x5b_Point_x5d__hash(key)
    var bucket []hashmap_ref_point_int32_x_entry = m.buckets[h]
    var i int32 = 0
    for {
        if i >= int32(len(bucket)) {
            break
        }
        var entry hashmap_ref_point_int32_x_entry = bucket[i]
        if entry.active && _goml_trait_impl_Eq_Ref_x5b_Point_x5d__eq(entry.key, key) {
            bucket[i].value = value
            return struct{}{}
        }
    }
    bucket = append(bucket, hashmap_ref_point_int32_x_entry{
        active: true,
        key: key,
        value: value,
    })
    m.buckets[h] = bucket
    m.len = m.len + 1
    return struct{}{}
}

type hashmap_ref_key_int32_x_entry struct {
    active bool
    key *ref_key_x
    value int32
}

type hashmap_ref_key_int32_x struct {
    buckets map[uint64][]hashmap_ref_key_int32_x_entry
    len int32
}

func hashmap_new__HashMap_Ref_Key_int32() *hashmap_ref_key_int32_x {
    return &hashmap_ref_key_int32_x{
        buckets: make(map[uint64][]hashmap_ref_key_int32_x_entry),
        len: 0,
    }
}

func hashmap_get__HashMap_Ref_Key_int32(m *hashmap_ref_key_int32_x, key *ref_key_x) Option__int32 {
    if m == nil {
        return None{}
    }
    var h uint64 = _goml_trait_impl_Hash_Ref_x5b_Key_x5d__hash(key)
    var bucket []hashmap_ref_key_int32_x_entry = m.buckets[h]
    var i int32 = 0
    for {
        if i >= int32(len(bucket)) {
            break
        }
        var entry hashmap_ref_key_int32_x_entry = bucket[i]
        if entry.active && _goml_trait_impl_Eq_Ref_x5b_Key_x5d__eq(entry.key, key) {
            return Some{
                _0: entry.value,
            }
        }
    }
    return None{}
}

func hashmap_set__HashMap_Ref_Key_int32(m *hashmap_ref_key_int32_x, key *ref_key_x, value int32) struct{} {
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_trait_impl_Hash_Ref_x5b_Key_x5d__hash(key)
    var bucket []hashmap_ref_key_int32_x_entry = m.buckets[h]
    var i int32 = 0
    for {
        if i >= int32(len(bucket)) {
            break
        }
        var entry hashmap_ref_key_int32_x_entry = bucket[i]
        if entry.active && _goml_trait_impl_Eq_Ref_x5b_Key_x5d__eq(entry.key, key) {
            bucket[i].value = value
            return struct{}{}
        }
    }
    bucket = append(bucket, hashmap_ref_key_int32_x_entry{
        active: true,
        key: key,
        value: value,
    })
    m.buckets[h] = bucket
    m.len = m.len + 1
    return struct{}{}
}

type Tuple2_Key_Key struct {
    _0 Key
    _1 Key
}

type Point struct {
    x int32
    y int32
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

type P struct {
    _0 Point
}

func (_ P) isKey() {}

type Option__int32 interface {
    isOption__int32()
}

type None struct {}

func (_ None) isOption__int32() {}

type Some struct {
    _0 int32
}

func (_ Some) isOption__int32() {}

func _goml_trait_impl_Eq_Point_eq(self__0 Point, other__1 Point) bool {
    var ret67 bool
    var t28 int32 = self__0.x
    var t29 int32 = other__1.x
    var t27 bool = _goml_trait_impl_Eq_int32_eq(t28, t29)
    var t26 bool = true && t27
    var t31 int32 = self__0.y
    var t32 int32 = other__1.y
    var t30 bool = _goml_trait_impl_Eq_int32_eq(t31, t32)
    ret67 = t26 && t30
    return ret67
}

func _goml_trait_impl_Hash_Point_hash(self__2 Point) uint64 {
    var ret68 uint64
    var h__3 uint64 = 14695981039346656037
    var t33 uint64 = h__3 * 1099511628211
    var t35 int32 = self__2.x
    var t34 uint64 = _goml_trait_impl_Hash_int32_hash(t35)
    var h__4 uint64 = t33 + t34
    var t36 uint64 = h__4 * 1099511628211
    var t38 int32 = self__2.y
    var t37 uint64 = _goml_trait_impl_Hash_int32_hash(t38)
    var h__5 uint64 = t36 + t37
    ret68 = h__5
    return ret68
}

func _goml_trait_impl_Eq_Key_eq(self__6 Key, other__7 Key) bool {
    var ret69 bool
    var mtmp0 Tuple2_Key_Key = Tuple2_Key_Key{
        _0: self__6,
        _1: other__7,
    }
    var x1 Key = mtmp0._0
    var x2 Key = mtmp0._1
    switch x2 := x2.(type) {
    case A:
        switch x1.(type) {
        case A:
            ret69 = true
        case B:
            ret69 = false
        case P:
            ret69 = false
        }
    case B:
        var x3 int32 = x2._0
        switch x1 := x1.(type) {
        case A:
            ret69 = false
        case B:
            var x7 int32 = x1._0
            var __l1_0__8 int32 = x7
            var __r1_0__9 int32 = x3
            var t39 bool = _goml_trait_impl_Eq_int32_eq(__l1_0__8, __r1_0__9)
            ret69 = true && t39
        case P:
            ret69 = false
        }
    case P:
        var x4 Point = x2._0
        switch x1 := x1.(type) {
        case A:
            ret69 = false
        case B:
            ret69 = false
        case P:
            var x10 Point = x1._0
            var __l2_0__10 Point = x10
            var __r2_0__11 Point = x4
            var t40 bool = _goml_trait_impl_Eq_Point_eq(__l2_0__10, __r2_0__11)
            ret69 = true && t40
        }
    }
    return ret69
}

func _goml_trait_impl_Hash_Key_hash(self__12 Key) uint64 {
    var ret70 uint64
    switch self__12 := self__12.(type) {
    case A:
        var h__13 uint64 = 14695981039346656037 + 1
        ret70 = h__13
    case B:
        var x11 int32 = self__12._0
        var __field1_0__14 int32 = x11
        var h__15 uint64 = 14695981039346656037 + 2
        var t41 uint64 = h__15 * 1099511628211
        var t42 uint64 = _goml_trait_impl_Hash_int32_hash(__field1_0__14)
        var h__16 uint64 = t41 + t42
        ret70 = h__16
    case P:
        var x12 Point = self__12._0
        var __field2_0__17 Point = x12
        var h__18 uint64 = 14695981039346656037 + 3
        var t43 uint64 = h__18 * 1099511628211
        var t44 uint64 = _goml_trait_impl_Hash_Point_hash(__field2_0__17)
        var h__19 uint64 = t43 + t44
        ret70 = h__19
    }
    return ret70
}

func print_opt_int(x__20 Option__int32) struct{} {
    var ret71 struct{}
    switch x__20 := x__20.(type) {
    case None:
        ret71 = string_println("none")
    case Some:
        var x13 int32 = x__20._0
        var v__21 int32 = x13
        var t45 string = int32_to_string(v__21)
        ret71 = string_println(t45)
    }
    return ret71
}

func main0() struct{} {
    var ret72 struct{}
    var m1__22 *hashmap_key_int32_x = hashmap_new__HashMap_Key_int32()
    var t46 Key = A{}
    hashmap_set__HashMap_Key_int32(m1__22, t46, 10)
    var t47 Key = B{
        _0: 1,
    }
    hashmap_set__HashMap_Key_int32(m1__22, t47, 20)
    var t49 int32 = hashmap_len__HashMap_Key_int32(m1__22)
    var t48 string = int32_to_string(t49)
    string_println(t48)
    var t51 Key = A{}
    var t50 Option__int32 = hashmap_get__HashMap_Key_int32(m1__22, t51)
    print_opt_int(t50)
    var t54 Key = B{
        _0: 1,
    }
    var t53 bool = hashmap_contains__HashMap_Key_int32(m1__22, t54)
    var t52 string = bool_to_string(t53)
    string_println(t52)
    var t55 Key = B{
        _0: 1,
    }
    hashmap_remove__HashMap_Key_int32(m1__22, t55)
    var t58 Key = B{
        _0: 1,
    }
    var t57 bool = hashmap_contains__HashMap_Key_int32(m1__22, t58)
    var t56 string = bool_to_string(t57)
    string_println(t56)
    var t60 int32 = hashmap_len__HashMap_Key_int32(m1__22)
    var t59 string = int32_to_string(t60)
    string_println(t59)
    var m2__23 *hashmap_ref_point_int32_x = hashmap_new__HashMap_Ref_Point_int32()
    var t61 Point = Point{
        x: 1,
        y: 2,
    }
    var p1__24 *ref_point_x = ref__Ref_Point(t61)
    var t62 Point = Point{
        x: 1,
        y: 2,
    }
    var p2__25 *ref_point_x = ref__Ref_Point(t62)
    hashmap_set__HashMap_Ref_Point_int32(m2__23, p1__24, 99)
    var t63 Option__int32 = hashmap_get__HashMap_Ref_Point_int32(m2__23, p2__25)
    print_opt_int(t63)
    var m3__26 *hashmap_ref_key_int32_x = hashmap_new__HashMap_Ref_Key_int32()
    var t64 Key = B{
        _0: 7,
    }
    var k1__27 *ref_key_x = ref__Ref_Key(t64)
    var t65 Key = B{
        _0: 7,
    }
    var k2__28 *ref_key_x = ref__Ref_Key(t65)
    hashmap_set__HashMap_Ref_Key_int32(m3__26, k1__27, 123)
    var t66 Option__int32 = hashmap_get__HashMap_Ref_Key_int32(m3__26, k2__28)
    print_opt_int(t66)
    ret72 = struct{}{}
    return ret72
}

func main() {
    main0()
}
