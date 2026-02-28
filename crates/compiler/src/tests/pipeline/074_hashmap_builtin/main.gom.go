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

type ref_point_x struct {
    value Point
}

func ref__Ref_Point(value Point) *ref_point_x {
    return &ref_point_x{
        value: value,
    }
}

func ref_get__Ref_Point(reference *ref_point_x) Point {
    return reference.value
}

func _goml_trait_impl_Eq_Ref_x5b_Point_x5d__eq(self *ref_point_x, other *ref_point_x) bool {
    return _goml_trait_impl_Eq_Point_eq(ref_get__Ref_Point(self), ref_get__Ref_Point(other))
}

func _goml_trait_impl_Hash_Ref_x5b_Point_x5d__hash(self *ref_point_x) uint64 {
    return _goml_trait_impl_Hash_Point_hash(ref_get__Ref_Point(self))
}

type ref_key_x struct {
    value Key
}

func ref__Ref_Key(value Key) *ref_key_x {
    return &ref_key_x{
        value: value,
    }
}

func ref_get__Ref_Key(reference *ref_key_x) Key {
    return reference.value
}

func _goml_trait_impl_Eq_Ref_x5b_Key_x5d__eq(self *ref_key_x, other *ref_key_x) bool {
    return _goml_trait_impl_Eq_Key_eq(ref_get__Ref_Key(self), ref_get__Ref_Key(other))
}

func _goml_trait_impl_Hash_Ref_x5b_Key_x5d__hash(self *ref_key_x) uint64 {
    return _goml_trait_impl_Hash_Key_hash(ref_get__Ref_Key(self))
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
        i = i + 1
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
        i = i + 1
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
        i = i + 1
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
        i = i + 1
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
    var t26 int32
    var t27 int32
    var t28 bool
    var t29 bool
    var t30 int32
    var t31 int32
    var t32 bool
    var t33 bool
    t26 = self__0.x
    t27 = other__1.x
    t28 = _goml_trait_impl_Eq_int32_eq(t26, t27)
    t29 = true && t28
    t30 = self__0.y
    t31 = other__1.y
    t32 = _goml_trait_impl_Eq_int32_eq(t30, t31)
    t33 = t29 && t32
    return t33
}

func _goml_trait_impl_Hash_Point_hash(self__2 Point) uint64 {
    var h__3 uint64
    var t34 uint64
    var t35 int32
    var t36 uint64
    var h__4 uint64
    var t37 uint64
    var t38 int32
    var t39 uint64
    var h__5 uint64
    h__3 = 14695981039346656037
    t34 = h__3 * 1099511628211
    t35 = self__2.x
    t36 = _goml_trait_impl_Hash_int32_hash(t35)
    h__4 = t34 + t36
    t37 = h__4 * 1099511628211
    t38 = self__2.y
    t39 = _goml_trait_impl_Hash_int32_hash(t38)
    h__5 = t37 + t39
    return h__5
}

func _goml_trait_impl_Eq_Key_eq(self__6 Key, other__7 Key) bool {
    var mtmp0 Tuple2_Key_Key
    var x1 Key
    var x2 Key
    var jp41 bool
    var jp43 bool
    var x3 int32
    var jp45 bool
    var x7 int32
    var __l1_0__8 int32
    var __r1_0__9 int32
    var t46 bool
    var t47 bool
    var x4 Point
    var jp49 bool
    var x10 Point
    var __l2_0__10 Point
    var __r2_0__11 Point
    var t50 bool
    var t51 bool
    mtmp0 = Tuple2_Key_Key{
        _0: self__6,
        _1: other__7,
    }
    x1 = mtmp0._0
    x2 = mtmp0._1
    switch x2.(type) {
    case A:
        goto b2
    case B:
        goto b7
    case P:
        goto b12
    default:
        panic("non-exhaustive match")
    }
    b1:
    return jp41
    b2:
    switch x1.(type) {
    case A:
        goto b4
    case B:
        goto b5
    case P:
        goto b6
    default:
        panic("non-exhaustive match")
    }
    b3:
    jp41 = jp43
    goto b1
    b4:
    jp43 = true
    goto b3
    b5:
    jp43 = false
    goto b3
    b6:
    jp43 = false
    goto b3
    b7:
    x3 = x2.(B)._0
    switch x1.(type) {
    case A:
        goto b9
    case B:
        goto b10
    case P:
        goto b11
    default:
        panic("non-exhaustive match")
    }
    b8:
    jp41 = jp45
    goto b1
    b9:
    jp45 = false
    goto b8
    b10:
    x7 = x1.(B)._0
    __l1_0__8 = x7
    __r1_0__9 = x3
    t46 = _goml_trait_impl_Eq_int32_eq(__l1_0__8, __r1_0__9)
    t47 = true && t46
    jp45 = t47
    goto b8
    b11:
    jp45 = false
    goto b8
    b12:
    x4 = x2.(P)._0
    switch x1.(type) {
    case A:
        goto b14
    case B:
        goto b15
    case P:
        goto b16
    default:
        panic("non-exhaustive match")
    }
    b13:
    jp41 = jp49
    goto b1
    b14:
    jp49 = false
    goto b13
    b15:
    jp49 = false
    goto b13
    b16:
    x10 = x1.(P)._0
    __l2_0__10 = x10
    __r2_0__11 = x4
    t50 = _goml_trait_impl_Eq_Point_eq(__l2_0__10, __r2_0__11)
    t51 = true && t50
    jp49 = t51
    goto b13
}

func _goml_trait_impl_Hash_Key_hash(self__12 Key) uint64 {
    var jp53 uint64
    var h__13 uint64
    var x11 int32
    var __field1_0__14 int32
    var h__15 uint64
    var t54 uint64
    var t55 uint64
    var h__16 uint64
    var x12 Point
    var __field2_0__17 Point
    var h__18 uint64
    var t56 uint64
    var t57 uint64
    var h__19 uint64
    switch self__12.(type) {
    case A:
        goto b2
    case B:
        goto b3
    case P:
        goto b4
    default:
        panic("non-exhaustive match")
    }
    b1:
    return jp53
    b2:
    h__13 = 14695981039346656037 + 1
    jp53 = h__13
    goto b1
    b3:
    x11 = self__12.(B)._0
    __field1_0__14 = x11
    h__15 = 14695981039346656037 + 2
    t54 = h__15 * 1099511628211
    t55 = _goml_trait_impl_Hash_int32_hash(__field1_0__14)
    h__16 = t54 + t55
    jp53 = h__16
    goto b1
    b4:
    x12 = self__12.(P)._0
    __field2_0__17 = x12
    h__18 = 14695981039346656037 + 3
    t56 = h__18 * 1099511628211
    t57 = _goml_trait_impl_Hash_Point_hash(__field2_0__17)
    h__19 = t56 + t57
    jp53 = h__19
    goto b1
}

func print_opt_int(x__20 Option__int32) struct{} {
    var x13 int32
    var v__21 int32
    switch x__20.(type) {
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
    x13 = x__20.(Some)._0
    v__21 = x13
    println__T_int32(v__21)
    goto b1
}

func main0() struct{} {
    var m1__22 *hashmap_key_int32_x
    var t61 Key
    var t62 int32
    var t63 Option__int32
    var t64 Key
    var t65 bool
    var t66 Key
    var t67 Key
    var t68 bool
    var t69 int32
    var m2__23 *hashmap_ref_point_int32_x
    var t70 Point
    var p1__24 *ref_point_x
    var t71 Point
    var p2__25 *ref_point_x
    var t72 Option__int32
    var m3__26 *hashmap_ref_key_int32_x
    var t73 Key
    var k1__27 *ref_key_x
    var t74 Key
    var k2__28 *ref_key_x
    var t75 Option__int32
    m1__22 = hashmap_new__HashMap_Key_int32()
    hashmap_set__HashMap_Key_int32(m1__22, A{}, 10)
    t61 = B{
        _0: 1,
    }
    hashmap_set__HashMap_Key_int32(m1__22, t61, 20)
    t62 = hashmap_len__HashMap_Key_int32(m1__22)
    println__T_int32(t62)
    t63 = hashmap_get__HashMap_Key_int32(m1__22, A{})
    print_opt_int(t63)
    t64 = B{
        _0: 1,
    }
    t65 = hashmap_contains__HashMap_Key_int32(m1__22, t64)
    println__T_bool(t65)
    t66 = B{
        _0: 1,
    }
    hashmap_remove__HashMap_Key_int32(m1__22, t66)
    t67 = B{
        _0: 1,
    }
    t68 = hashmap_contains__HashMap_Key_int32(m1__22, t67)
    println__T_bool(t68)
    t69 = hashmap_len__HashMap_Key_int32(m1__22)
    println__T_int32(t69)
    m2__23 = hashmap_new__HashMap_Ref_Point_int32()
    t70 = Point{
        x: 1,
        y: 2,
    }
    p1__24 = ref__Ref_Point(t70)
    t71 = Point{
        x: 1,
        y: 2,
    }
    p2__25 = ref__Ref_Point(t71)
    hashmap_set__HashMap_Ref_Point_int32(m2__23, p1__24, 99)
    t72 = hashmap_get__HashMap_Ref_Point_int32(m2__23, p2__25)
    print_opt_int(t72)
    m3__26 = hashmap_new__HashMap_Ref_Key_int32()
    t73 = B{
        _0: 7,
    }
    k1__27 = ref__Ref_Key(t73)
    t74 = B{
        _0: 7,
    }
    k2__28 = ref__Ref_Key(t74)
    hashmap_set__HashMap_Ref_Key_int32(m3__26, k1__27, 123)
    t75 = hashmap_get__HashMap_Ref_Key_int32(m3__26, k2__28)
    print_opt_int(t75)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t76 struct{}
    t76 = string_println(value__1)
    return t76
}

func println__T_int32(value__1 int32) struct{} {
    var t77 string
    var t78 struct{}
    t77 = int32_to_string(value__1)
    t78 = string_println(t77)
    return t78
}

func println__T_bool(value__1 bool) struct{} {
    var t79 string
    var t80 struct{}
    t79 = bool_to_string(value__1)
    t80 = string_println(t79)
    return t80
}

func main() {
    main0()
}
