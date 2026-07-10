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

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self int32, other int32) bool {
    return self == other
}

func _goml_m_trait__impl_i_Hash_i_int32_i_hash(self int32) uint64 {
    return int32_hash(self)
}

type ref_Point_x struct {
    value Point
}

func ref__Ref_5Point(value Point) *ref_Point_x {
    return &ref_Point_x{
        value: value,
    }
}

func ref_get__Ref_5Point(reference *ref_Point_x) Point {
    return reference.value
}

func _goml_m_trait__impl_i_Eq_i_Ref_l_Point_r__i_eq(self *ref_Point_x, other *ref_Point_x) bool {
    return _goml_m_trait__impl_i_Eq_i_Point_i_eq(ref_get__Ref_5Point(self), ref_get__Ref_5Point(other))
}

func _goml_m_trait__impl_i_Hash_i_Ref_l_Point_r__i_hash(self *ref_Point_x) uint64 {
    return _goml_m_trait__impl_i_Hash_i_Point_i_hash(ref_get__Ref_5Point(self))
}

type ref_Key_x struct {
    value Key
}

func ref__Ref_3Key(value Key) *ref_Key_x {
    return &ref_Key_x{
        value: value,
    }
}

func ref_get__Ref_3Key(reference *ref_Key_x) Key {
    return reference.value
}

func _goml_m_trait__impl_i_Eq_i_Ref_l_Key_r__i_eq(self *ref_Key_x, other *ref_Key_x) bool {
    return _goml_m_trait__impl_i_Eq_i_Key_i_eq(ref_get__Ref_3Key(self), ref_get__Ref_3Key(other))
}

func _goml_m_trait__impl_i_Hash_i_Ref_l_Key_r__i_hash(self *ref_Key_x) uint64 {
    return _goml_m_trait__impl_i_Hash_i_Key_i_hash(ref_get__Ref_3Key(self))
}

type hashmap_Key_int32_x_entry struct {
    active bool
    key Key
    value int32
}

type hashmap_Key_int32_x struct {
    buckets map[uint64][]hashmap_Key_int32_x_entry
    len int32
}

func hashmap_new__HashMap_3Key_5int32() *hashmap_Key_int32_x {
    return &hashmap_Key_int32_x{
        buckets: make(map[uint64][]hashmap_Key_int32_x_entry),
        len: 0,
    }
}

func hashmap_len__HashMap_3Key_5int32(m *hashmap_Key_int32_x) int32 {
    if m == nil {
        return 0
    }
    return m.len
}

func hashmap_lookup__HashMap_3Key_5int32(m *hashmap_Key_int32_x, key Key) (int32, bool) {
    if m == nil {
        var zero int32
        return zero, false
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_Key_i_hash(key)
    var bucket []hashmap_Key_int32_x_entry = m.buckets[h]
    var i int32 = 0
    for {
        if i >= int32(len(bucket)) {
            break
        }
        var entry hashmap_Key_int32_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_Eq_i_Key_i_eq(entry.key, key) {
            return entry.value, true
        }
        i = i + 1
    }
    var zero int32
    return zero, false
}

func hashmap_get__HashMap_3Key_5int32(m *hashmap_Key_int32_x, key Key) Option__int32 {
    var value int32
    var ok bool
    value, ok = hashmap_lookup__HashMap_3Key_5int32(m, key)
    if ok {
        return Some{
            _0: value,
        }
    }
    return None{}
}

func hashmap_set__HashMap_3Key_5int32(m *hashmap_Key_int32_x, key Key, value int32) struct{} {
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_Key_i_hash(key)
    var bucket []hashmap_Key_int32_x_entry = m.buckets[h]
    var i int32 = 0
    for {
        if i >= int32(len(bucket)) {
            break
        }
        var entry hashmap_Key_int32_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_Eq_i_Key_i_eq(entry.key, key) {
            bucket[i].value = value
            return struct{}{}
        }
        i = i + 1
    }
    bucket = append(bucket, hashmap_Key_int32_x_entry{
        active: true,
        key: key,
        value: value,
    })
    m.buckets[h] = bucket
    m.len = m.len + 1
    return struct{}{}
}

func hashmap_remove__HashMap_3Key_5int32(m *hashmap_Key_int32_x, key Key) struct{} {
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_Key_i_hash(key)
    var bucket []hashmap_Key_int32_x_entry = m.buckets[h]
    var i int32 = 0
    for {
        if i >= int32(len(bucket)) {
            break
        }
        var entry hashmap_Key_int32_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_Eq_i_Key_i_eq(entry.key, key) {
            bucket[i].active = false
            m.len = m.len - 1
            return struct{}{}
        }
        i = i + 1
    }
    return struct{}{}
}

func hashmap_contains__HashMap_3Key_5int32(m *hashmap_Key_int32_x, key Key) bool {
    if m == nil {
        return false
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_Key_i_hash(key)
    var bucket []hashmap_Key_int32_x_entry = m.buckets[h]
    var i int32 = 0
    for {
        if i >= int32(len(bucket)) {
            break
        }
        var entry hashmap_Key_int32_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_Eq_i_Key_i_eq(entry.key, key) {
            return true
        }
        i = i + 1
    }
    return false
}

type hashmap_Ref_5Point_int32_x_entry struct {
    active bool
    key *ref_Point_x
    value int32
}

type hashmap_Ref_5Point_int32_x struct {
    buckets map[uint64][]hashmap_Ref_5Point_int32_x_entry
    len int32
}

func hashmap_new__HashMap_10Ref_5Point_5int32() *hashmap_Ref_5Point_int32_x {
    return &hashmap_Ref_5Point_int32_x{
        buckets: make(map[uint64][]hashmap_Ref_5Point_int32_x_entry),
        len: 0,
    }
}

func hashmap_lookup__HashMap_10Ref_5Point_5int32(m *hashmap_Ref_5Point_int32_x, key *ref_Point_x) (int32, bool) {
    if m == nil {
        var zero int32
        return zero, false
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_Ref_l_Point_r__i_hash(key)
    var bucket []hashmap_Ref_5Point_int32_x_entry = m.buckets[h]
    var i int32 = 0
    for {
        if i >= int32(len(bucket)) {
            break
        }
        var entry hashmap_Ref_5Point_int32_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_Eq_i_Ref_l_Point_r__i_eq(entry.key, key) {
            return entry.value, true
        }
        i = i + 1
    }
    var zero int32
    return zero, false
}

func hashmap_get__HashMap_10Ref_5Point_5int32(m *hashmap_Ref_5Point_int32_x, key *ref_Point_x) Option__int32 {
    var value int32
    var ok bool
    value, ok = hashmap_lookup__HashMap_10Ref_5Point_5int32(m, key)
    if ok {
        return Some{
            _0: value,
        }
    }
    return None{}
}

func hashmap_set__HashMap_10Ref_5Point_5int32(m *hashmap_Ref_5Point_int32_x, key *ref_Point_x, value int32) struct{} {
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_Ref_l_Point_r__i_hash(key)
    var bucket []hashmap_Ref_5Point_int32_x_entry = m.buckets[h]
    var i int32 = 0
    for {
        if i >= int32(len(bucket)) {
            break
        }
        var entry hashmap_Ref_5Point_int32_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_Eq_i_Ref_l_Point_r__i_eq(entry.key, key) {
            bucket[i].value = value
            return struct{}{}
        }
        i = i + 1
    }
    bucket = append(bucket, hashmap_Ref_5Point_int32_x_entry{
        active: true,
        key: key,
        value: value,
    })
    m.buckets[h] = bucket
    m.len = m.len + 1
    return struct{}{}
}

type hashmap_Ref_3Key_int32_x_entry struct {
    active bool
    key *ref_Key_x
    value int32
}

type hashmap_Ref_3Key_int32_x struct {
    buckets map[uint64][]hashmap_Ref_3Key_int32_x_entry
    len int32
}

func hashmap_new__HashMap_8Ref_3Key_5int32() *hashmap_Ref_3Key_int32_x {
    return &hashmap_Ref_3Key_int32_x{
        buckets: make(map[uint64][]hashmap_Ref_3Key_int32_x_entry),
        len: 0,
    }
}

func hashmap_lookup__HashMap_8Ref_3Key_5int32(m *hashmap_Ref_3Key_int32_x, key *ref_Key_x) (int32, bool) {
    if m == nil {
        var zero int32
        return zero, false
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_Ref_l_Key_r__i_hash(key)
    var bucket []hashmap_Ref_3Key_int32_x_entry = m.buckets[h]
    var i int32 = 0
    for {
        if i >= int32(len(bucket)) {
            break
        }
        var entry hashmap_Ref_3Key_int32_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_Eq_i_Ref_l_Key_r__i_eq(entry.key, key) {
            return entry.value, true
        }
        i = i + 1
    }
    var zero int32
    return zero, false
}

func hashmap_get__HashMap_8Ref_3Key_5int32(m *hashmap_Ref_3Key_int32_x, key *ref_Key_x) Option__int32 {
    var value int32
    var ok bool
    value, ok = hashmap_lookup__HashMap_8Ref_3Key_5int32(m, key)
    if ok {
        return Some{
            _0: value,
        }
    }
    return None{}
}

func hashmap_set__HashMap_8Ref_3Key_5int32(m *hashmap_Ref_3Key_int32_x, key *ref_Key_x, value int32) struct{} {
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_Ref_l_Key_r__i_hash(key)
    var bucket []hashmap_Ref_3Key_int32_x_entry = m.buckets[h]
    var i int32 = 0
    for {
        if i >= int32(len(bucket)) {
            break
        }
        var entry hashmap_Ref_3Key_int32_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_Eq_i_Ref_l_Key_r__i_eq(entry.key, key) {
            bucket[i].value = value
            return struct{}{}
        }
        i = i + 1
    }
    bucket = append(bucket, hashmap_Ref_3Key_int32_x_entry{
        active: true,
        key: key,
        value: value,
    })
    m.buckets[h] = bucket
    m.len = m.len + 1
    return struct{}{}
}

type Tuple2_3Key_3Key struct {
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

func _goml_m_trait__impl_i_Eq_i_Point_i_eq(self__0 Point, other__1 Point) bool {
    var retv27 bool
    var jp31 bool
    if true {
        var t35 int32 = self__0.x
        var t36 int32 = other__1.x
        var t37 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(t35, t36)
        jp31 = t37
    } else {
        jp31 = false
    }
    var jp29 bool
    if jp31 {
        var t32 int32 = self__0.y
        var t33 int32 = other__1.y
        var t34 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(t32, t33)
        jp29 = t34
    } else {
        jp29 = false
    }
    retv27 = jp29
    return retv27
}

func _goml_m_trait__impl_i_Hash_i_Point_i_hash(self__2 Point) uint64 {
    var retv39 uint64
    var h__3 uint64 = 14695981039346656037
    var t40 uint64 = h__3 * 1099511628211
    var t41 int32 = self__2.x
    var t42 uint64 = _goml_m_trait__impl_i_Hash_i_int32_i_hash(t41)
    var h__4 uint64 = t40 + t42
    var t43 uint64 = h__4 * 1099511628211
    var t44 int32 = self__2.y
    var t45 uint64 = _goml_m_trait__impl_i_Hash_i_int32_i_hash(t44)
    var h__5 uint64 = t43 + t45
    retv39 = h__5
    return retv39
}

func _goml_m_trait__impl_i_Eq_i_Key_i_eq(self__6 Key, other__7 Key) bool {
    var retv47 bool
    var mtmp0 Tuple2_3Key_3Key = Tuple2_3Key_3Key{
        _0: self__6,
        _1: other__7,
    }
    var x1 Key = mtmp0._0
    var x2 Key = mtmp0._1
    var jp49 bool
    switch x2.(type) {
    case A:
        var jp51 bool
        switch x1.(type) {
        case A:
            jp51 = true
        case B:
            jp51 = false
        case P:
            jp51 = false
        default:
            panic("non-exhaustive match")
        }
        jp49 = jp51
    case B:
        var x3 int32 = x2.(B)._0
        var jp53 bool
        switch x1.(type) {
        case A:
            jp53 = false
        case B:
            var x7 int32 = x1.(B)._0
            var __l1_0__8 int32 = x7
            var __r1_0__9 int32 = x3
            var jp55 bool
            if true {
                var t56 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(__l1_0__8, __r1_0__9)
                jp55 = t56
            } else {
                jp55 = false
            }
            jp53 = jp55
        case P:
            jp53 = false
        default:
            panic("non-exhaustive match")
        }
        jp49 = jp53
    case P:
        var x4 Point = x2.(P)._0
        var jp58 bool
        switch x1.(type) {
        case A:
            jp58 = false
        case B:
            jp58 = false
        case P:
            var x10 Point = x1.(P)._0
            var __l2_0__10 Point = x10
            var __r2_0__11 Point = x4
            var jp60 bool
            if true {
                var t61 bool = _goml_m_trait__impl_i_Eq_i_Point_i_eq(__l2_0__10, __r2_0__11)
                jp60 = t61
            } else {
                jp60 = false
            }
            jp58 = jp60
        default:
            panic("non-exhaustive match")
        }
        jp49 = jp58
    default:
        panic("non-exhaustive match")
    }
    retv47 = jp49
    return retv47
}

func _goml_m_trait__impl_i_Hash_i_Key_i_hash(self__12 Key) uint64 {
    var retv63 uint64
    var jp65 uint64
    switch self__12.(type) {
    case A:
        var h__13 uint64 = 14695981039346656037 + 1
        jp65 = h__13
    case B:
        var x11 int32 = self__12.(B)._0
        var __field1_0__14 int32 = x11
        var h__15 uint64 = 14695981039346656037 + 2
        var t66 uint64 = h__15 * 1099511628211
        var t67 uint64 = _goml_m_trait__impl_i_Hash_i_int32_i_hash(__field1_0__14)
        var h__16 uint64 = t66 + t67
        jp65 = h__16
    case P:
        var x12 Point = self__12.(P)._0
        var __field2_0__17 Point = x12
        var h__18 uint64 = 14695981039346656037 + 3
        var t68 uint64 = h__18 * 1099511628211
        var t69 uint64 = _goml_m_trait__impl_i_Hash_i_Point_i_hash(__field2_0__17)
        var h__19 uint64 = t68 + t69
        jp65 = h__19
    default:
        panic("non-exhaustive match")
    }
    retv63 = jp65
    return retv63
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
    var m1__22 *hashmap_Key_int32_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__Key____V__int32()
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(m1__22, A{}, 10)
    var t75 Key = B{
        _0: 1,
    }
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(m1__22, t75, 20)
    var t76 int32 = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(m1__22)
    println__T_int32(t76)
    var t77 Option__int32 = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__Key____V__int32(m1__22, A{})
    print_opt_int(t77)
    var t78 Key = B{
        _0: 1,
    }
    var t79 bool = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(m1__22, t78)
    println__T_bool(t79)
    var t80 Key = B{
        _0: 1,
    }
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_remove____K__Key____V__int32(m1__22, t80)
    var t81 Key = B{
        _0: 1,
    }
    var t82 bool = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(m1__22, t81)
    println__T_bool(t82)
    var t83 int32 = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(m1__22)
    println__T_int32(t83)
    var m2__23 *hashmap_Ref_5Point_int32_x = _goml_m_inherent_i_HashMap_i_H_h8965aba642d8a393e2049e4587079c44__r_____V__int32()
    var t84 Point = Point{
        x: 1,
        y: 2,
    }
    var p1__24 *ref_Point_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Point(t84)
    var t85 Point = Point{
        x: 1,
        y: 2,
    }
    var p2__25 *ref_Point_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Point(t85)
    _goml_m_inherent_i_HashMap_i_H_h92a9b8ea001265f897bc67524ef74086__r_____V__int32(m2__23, p1__24, 99)
    var t86 Option__int32 = _goml_m_inherent_i_HashMap_i_H_h8816d503fd85179e4f674a4a9db321b9__r_____V__int32(m2__23, p2__25)
    print_opt_int(t86)
    var m3__26 *hashmap_Ref_3Key_int32_x = _goml_m_inherent_i_HashMap_i_H_h35e4bb85f0aaff40bae5445e99f77c49__r_____V__int32()
    var t87 Key = B{
        _0: 7,
    }
    var k1__27 *ref_Key_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Key(t87)
    var t88 Key = B{
        _0: 7,
    }
    var k2__28 *ref_Key_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Key(t88)
    _goml_m_inherent_i_HashMap_i_H_h8d7eb9d75c42de8655645502911541c0__r_____V__int32(m3__26, k1__27, 123)
    var t89 Option__int32 = _goml_m_inherent_i_HashMap_i_H_h488c149fa298920fe2fbb4be132c4bc3__r_____V__int32(m3__26, k2__28)
    print_opt_int(t89)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t93 string = int32_to_string(value__1)
    string_println(t93)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__Key____V__int32() *hashmap_Key_int32_x {
    var retv96 *hashmap_Key_int32_x
    var t97 *hashmap_Key_int32_x = hashmap_new__HashMap_3Key_5int32()
    retv96 = t97
    return retv96
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(self__85 *hashmap_Key_int32_x, key__86 Key, value__87 int32) struct{} {
    hashmap_set__HashMap_3Key_5int32(self__85, key__86, value__87)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(self__90 *hashmap_Key_int32_x) int32 {
    var retv101 int32
    var t102 int32 = hashmap_len__HashMap_3Key_5int32(self__90)
    retv101 = t102
    return retv101
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__Key____V__int32(self__83 *hashmap_Key_int32_x, key__84 Key) Option__int32 {
    var retv104 Option__int32
    var t105 Option__int32 = hashmap_get__HashMap_3Key_5int32(self__83, key__84)
    retv104 = t105
    return retv104
}

func println__T_bool(value__1 bool) struct{} {
    var t107 string = bool_to_string(value__1)
    string_println(t107)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(self__91 *hashmap_Key_int32_x, key__92 Key) bool {
    var retv110 bool
    var t111 bool = hashmap_contains__HashMap_3Key_5int32(self__91, key__92)
    retv110 = t111
    return retv110
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_remove____K__Key____V__int32(self__88 *hashmap_Key_int32_x, key__89 Key) struct{} {
    hashmap_remove__HashMap_3Key_5int32(self__88, key__89)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h8965aba642d8a393e2049e4587079c44__r_____V__int32() *hashmap_Ref_5Point_int32_x {
    var retv115 *hashmap_Ref_5Point_int32_x
    var t116 *hashmap_Ref_5Point_int32_x = hashmap_new__HashMap_10Ref_5Point_5int32()
    retv115 = t116
    return retv115
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Point(value__93 Point) *ref_Point_x {
    var retv118 *ref_Point_x
    var t119 *ref_Point_x = ref__Ref_5Point(value__93)
    retv118 = t119
    return retv118
}

func _goml_m_inherent_i_HashMap_i_H_h92a9b8ea001265f897bc67524ef74086__r_____V__int32(self__85 *hashmap_Ref_5Point_int32_x, key__86 *ref_Point_x, value__87 int32) struct{} {
    hashmap_set__HashMap_10Ref_5Point_5int32(self__85, key__86, value__87)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h8816d503fd85179e4f674a4a9db321b9__r_____V__int32(self__83 *hashmap_Ref_5Point_int32_x, key__84 *ref_Point_x) Option__int32 {
    var retv123 Option__int32
    var t124 Option__int32 = hashmap_get__HashMap_10Ref_5Point_5int32(self__83, key__84)
    retv123 = t124
    return retv123
}

func _goml_m_inherent_i_HashMap_i_H_h35e4bb85f0aaff40bae5445e99f77c49__r_____V__int32() *hashmap_Ref_3Key_int32_x {
    var retv126 *hashmap_Ref_3Key_int32_x
    var t127 *hashmap_Ref_3Key_int32_x = hashmap_new__HashMap_8Ref_3Key_5int32()
    retv126 = t127
    return retv126
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Key(value__93 Key) *ref_Key_x {
    var retv129 *ref_Key_x
    var t130 *ref_Key_x = ref__Ref_3Key(value__93)
    retv129 = t130
    return retv129
}

func _goml_m_inherent_i_HashMap_i_H_h8d7eb9d75c42de8655645502911541c0__r_____V__int32(self__85 *hashmap_Ref_3Key_int32_x, key__86 *ref_Key_x, value__87 int32) struct{} {
    hashmap_set__HashMap_8Ref_3Key_5int32(self__85, key__86, value__87)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h488c149fa298920fe2fbb4be132c4bc3__r_____V__int32(self__83 *hashmap_Ref_3Key_int32_x, key__84 *ref_Key_x) Option__int32 {
    var retv134 Option__int32
    var t135 Option__int32 = hashmap_get__HashMap_8Ref_3Key_5int32(self__83, key__84)
    retv134 = t135
    return retv134
}

func main() {
    main0()
}
