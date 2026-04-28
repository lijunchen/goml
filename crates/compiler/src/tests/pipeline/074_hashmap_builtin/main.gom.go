package main

import (
    _goml_fmt "fmt"
)

func bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func int32_hash(x int32) uint64 {
    return uint64(x)
}

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
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

func hashmap_get__HashMap_Key_int32(m *hashmap_key_int32_x, key Key) Option__int32 {
    var value int32
    var ok bool
    value, ok = hashmap_get_native__HashMap_Key_int32(m, key)
    if ok {
        return Some{
            _0: value,
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

func hashmap_get_native__HashMap_Ref_Point_int32(m *hashmap_ref_point_int32_x, key *ref_point_x) (int32, bool) {
    if m == nil {
        var zero int32
        return zero, false
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
            return entry.value, true
        }
        i = i + 1
    }
    var zero int32
    return zero, false
}

func hashmap_get__HashMap_Ref_Point_int32(m *hashmap_ref_point_int32_x, key *ref_point_x) Option__int32 {
    var value int32
    var ok bool
    value, ok = hashmap_get_native__HashMap_Ref_Point_int32(m, key)
    if ok {
        return Some{
            _0: value,
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

func hashmap_get_native__HashMap_Ref_Key_int32(m *hashmap_ref_key_int32_x, key *ref_key_x) (int32, bool) {
    if m == nil {
        var zero int32
        return zero, false
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
            return entry.value, true
        }
        i = i + 1
    }
    var zero int32
    return zero, false
}

func hashmap_get__HashMap_Ref_Key_int32(m *hashmap_ref_key_int32_x, key *ref_key_x) Option__int32 {
    var value int32
    var ok bool
    value, ok = hashmap_get_native__HashMap_Ref_Key_int32(m, key)
    if ok {
        return Some{
            _0: value,
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

type GoError = error

func _goml_trait_impl_Eq_Point_eq(self__0 Point, other__1 Point) bool {
    var retv27 bool
    var t28 int32 = self__0.x
    var t29 int32 = other__1.x
    var t30 bool = _goml_trait_impl_Eq_int32_eq(t28, t29)
    var t31 bool = true && t30
    var t32 int32 = self__0.y
    var t33 int32 = other__1.y
    var t34 bool = _goml_trait_impl_Eq_int32_eq(t32, t33)
    var t35 bool = t31 && t34
    retv27 = t35
    return retv27
}

func _goml_trait_impl_Hash_Point_hash(self__2 Point) uint64 {
    var retv37 uint64
    var h__3 uint64 = 14695981039346656037
    var t38 uint64 = h__3 * 1099511628211
    var t39 int32 = self__2.x
    var t40 uint64 = _goml_trait_impl_Hash_int32_hash(t39)
    var h__4 uint64 = t38 + t40
    var t41 uint64 = h__4 * 1099511628211
    var t42 int32 = self__2.y
    var t43 uint64 = _goml_trait_impl_Hash_int32_hash(t42)
    var h__5 uint64 = t41 + t43
    retv37 = h__5
    return retv37
}

func _goml_trait_impl_Eq_Key_eq(self__6 Key, other__7 Key) bool {
    var retv45 bool
    var mtmp0 Tuple2_Key_Key = Tuple2_Key_Key{
        _0: self__6,
        _1: other__7,
    }
    var x1 Key = mtmp0._0
    var x2 Key = mtmp0._1
    var jp47 bool
    switch x2.(type) {
    case A:
        var jp49 bool
        switch x1.(type) {
        case A:
            jp49 = true
        case B:
            jp49 = false
        case P:
            jp49 = false
        default:
            panic("non-exhaustive match")
        }
        jp47 = jp49
    case B:
        var x3 int32 = x2.(B)._0
        var jp51 bool
        switch x1.(type) {
        case A:
            jp51 = false
        case B:
            var x7 int32 = x1.(B)._0
            var __l1_0__8 int32 = x7
            var __r1_0__9 int32 = x3
            var t52 bool = _goml_trait_impl_Eq_int32_eq(__l1_0__8, __r1_0__9)
            var t53 bool = true && t52
            jp51 = t53
        case P:
            jp51 = false
        default:
            panic("non-exhaustive match")
        }
        jp47 = jp51
    case P:
        var x4 Point = x2.(P)._0
        var jp55 bool
        switch x1.(type) {
        case A:
            jp55 = false
        case B:
            jp55 = false
        case P:
            var x10 Point = x1.(P)._0
            var __l2_0__10 Point = x10
            var __r2_0__11 Point = x4
            var t56 bool = _goml_trait_impl_Eq_Point_eq(__l2_0__10, __r2_0__11)
            var t57 bool = true && t56
            jp55 = t57
        default:
            panic("non-exhaustive match")
        }
        jp47 = jp55
    default:
        panic("non-exhaustive match")
    }
    retv45 = jp47
    return retv45
}

func _goml_trait_impl_Hash_Key_hash(self__12 Key) uint64 {
    var retv59 uint64
    var jp61 uint64
    switch self__12.(type) {
    case A:
        var h__13 uint64 = 14695981039346656037 + 1
        jp61 = h__13
    case B:
        var x11 int32 = self__12.(B)._0
        var __field1_0__14 int32 = x11
        var h__15 uint64 = 14695981039346656037 + 2
        var t62 uint64 = h__15 * 1099511628211
        var t63 uint64 = _goml_trait_impl_Hash_int32_hash(__field1_0__14)
        var h__16 uint64 = t62 + t63
        jp61 = h__16
    case P:
        var x12 Point = self__12.(P)._0
        var __field2_0__17 Point = x12
        var h__18 uint64 = 14695981039346656037 + 3
        var t64 uint64 = h__18 * 1099511628211
        var t65 uint64 = _goml_trait_impl_Hash_Point_hash(__field2_0__17)
        var h__19 uint64 = t64 + t65
        jp61 = h__19
    default:
        panic("non-exhaustive match")
    }
    retv59 = jp61
    return retv59
}

func print_opt_int(x__20 Option__int32) struct{} {
    switch x__20.(type) {
    case None:
        println__T_string("none")
    case Some:
        var x13 int32 = x__20.(Some)._0
        var v__21 int32 = x13
        println__T_int32(v__21)
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func main0() struct{} {
    var m1__22 *hashmap_key_int32_x = hashmap_new__HashMap_Key_int32()
    hashmap_set__HashMap_Key_int32(m1__22, A{}, 10)
    var t71 Key = B{
        _0: 1,
    }
    hashmap_set__HashMap_Key_int32(m1__22, t71, 20)
    var t72 int32 = hashmap_len__HashMap_Key_int32(m1__22)
    println__T_int32(t72)
    var t73 Option__int32 = hashmap_get__HashMap_Key_int32(m1__22, A{})
    print_opt_int(t73)
    var t74 Key = B{
        _0: 1,
    }
    var t75 bool = hashmap_contains__HashMap_Key_int32(m1__22, t74)
    println__T_bool(t75)
    var t76 Key = B{
        _0: 1,
    }
    hashmap_remove__HashMap_Key_int32(m1__22, t76)
    var t77 Key = B{
        _0: 1,
    }
    var t78 bool = hashmap_contains__HashMap_Key_int32(m1__22, t77)
    println__T_bool(t78)
    var t79 int32 = hashmap_len__HashMap_Key_int32(m1__22)
    println__T_int32(t79)
    var m2__23 *hashmap_ref_point_int32_x = hashmap_new__HashMap_Ref_Point_int32()
    var t80 Point = Point{
        x: 1,
        y: 2,
    }
    var p1__24 *ref_point_x = ref__Ref_Point(t80)
    var t81 Point = Point{
        x: 1,
        y: 2,
    }
    var p2__25 *ref_point_x = ref__Ref_Point(t81)
    hashmap_set__HashMap_Ref_Point_int32(m2__23, p1__24, 99)
    var t82 Option__int32 = hashmap_get__HashMap_Ref_Point_int32(m2__23, p2__25)
    print_opt_int(t82)
    var m3__26 *hashmap_ref_key_int32_x = hashmap_new__HashMap_Ref_Key_int32()
    var t83 Key = B{
        _0: 7,
    }
    var k1__27 *ref_key_x = ref__Ref_Key(t83)
    var t84 Key = B{
        _0: 7,
    }
    var k2__28 *ref_key_x = ref__Ref_Key(t84)
    hashmap_set__HashMap_Ref_Key_int32(m3__26, k1__27, 123)
    var t85 Option__int32 = hashmap_get__HashMap_Ref_Key_int32(m3__26, k2__28)
    print_opt_int(t85)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t89 string = int32_to_string(value__1)
    string_println(t89)
    return struct{}{}
}

func println__T_bool(value__1 bool) struct{} {
    var t92 string = bool_to_string(value__1)
    string_println(t92)
    return struct{}{}
}

func main() {
    main0()
}
