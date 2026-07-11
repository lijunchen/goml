package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_int32_hash(x int32) uint64 {
    return uint64(x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
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
    var retv49 bool
    var jp53 bool
    if true {
        var t57 int32 = self__0.x
        var t58 int32 = other__1.x
        var t59 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(t57, t58)
        jp53 = t59
    } else {
        jp53 = false
    }
    var jp51 bool
    if jp53 {
        var t54 int32 = self__0.y
        var t55 int32 = other__1.y
        var t56 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(t54, t55)
        jp51 = t56
    } else {
        jp51 = false
    }
    retv49 = jp51
    return retv49
}

func _goml_m_trait__impl_i_Hash_i_Point_i_hash(self__2 Point) uint64 {
    var retv61 uint64
    var h__3 uint64 = 14695981039346656037
    var t62 uint64 = h__3 * 1099511628211
    var t63 int32 = self__2.x
    var t64 uint64 = _goml_m_trait__impl_i_Hash_i_int32_i_hash(t63)
    var h__4 uint64 = t62 + t64
    var t65 uint64 = h__4 * 1099511628211
    var t66 int32 = self__2.y
    var t67 uint64 = _goml_m_trait__impl_i_Hash_i_int32_i_hash(t66)
    var h__5 uint64 = t65 + t67
    retv61 = h__5
    return retv61
}

func _goml_m_trait__impl_i_Eq_i_Key_i_eq(self__6 Key, other__7 Key) bool {
    var retv69 bool
    var mtmp22 Tuple2_3Key_3Key = Tuple2_3Key_3Key{
        _0: self__6,
        _1: other__7,
    }
    var x23 Key = mtmp22._0
    var x24 Key = mtmp22._1
    var jp71 bool
    switch x24.(type) {
    case A:
        var jp73 bool
        switch x23.(type) {
        case A:
            jp73 = true
        case B:
            jp73 = false
        case P:
            jp73 = false
        default:
            panic("non-exhaustive match")
        }
        jp71 = jp73
    case B:
        var x25 int32 = x24.(B)._0
        var jp75 bool
        switch x23.(type) {
        case A:
            jp75 = false
        case B:
            var x29 int32 = x23.(B)._0
            var __l1_0__8 int32 = x29
            var __r1_0__9 int32 = x25
            var jp77 bool
            if true {
                var t78 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(__l1_0__8, __r1_0__9)
                jp77 = t78
            } else {
                jp77 = false
            }
            jp75 = jp77
        case P:
            jp75 = false
        default:
            panic("non-exhaustive match")
        }
        jp71 = jp75
    case P:
        var x26 Point = x24.(P)._0
        var jp80 bool
        switch x23.(type) {
        case A:
            jp80 = false
        case B:
            jp80 = false
        case P:
            var x32 Point = x23.(P)._0
            var __l2_0__10 Point = x32
            var __r2_0__11 Point = x26
            var jp82 bool
            if true {
                var t83 bool = _goml_m_trait__impl_i_Eq_i_Point_i_eq(__l2_0__10, __r2_0__11)
                jp82 = t83
            } else {
                jp82 = false
            }
            jp80 = jp82
        default:
            panic("non-exhaustive match")
        }
        jp71 = jp80
    default:
        panic("non-exhaustive match")
    }
    retv69 = jp71
    return retv69
}

func _goml_m_trait__impl_i_Hash_i_Key_i_hash(self__12 Key) uint64 {
    var retv85 uint64
    var jp87 uint64
    switch self__12.(type) {
    case A:
        var h__13 uint64 = 14695981039346656037 + 1
        jp87 = h__13
    case B:
        var x33 int32 = self__12.(B)._0
        var __field1_0__14 int32 = x33
        var h__15 uint64 = 14695981039346656037 + 2
        var t88 uint64 = h__15 * 1099511628211
        var t89 uint64 = _goml_m_trait__impl_i_Hash_i_int32_i_hash(__field1_0__14)
        var h__16 uint64 = t88 + t89
        jp87 = h__16
    case P:
        var x34 Point = self__12.(P)._0
        var __field2_0__17 Point = x34
        var h__18 uint64 = 14695981039346656037 + 3
        var t90 uint64 = h__18 * 1099511628211
        var t91 uint64 = _goml_m_trait__impl_i_Hash_i_Point_i_hash(__field2_0__17)
        var h__19 uint64 = t90 + t91
        jp87 = h__19
    default:
        panic("non-exhaustive match")
    }
    retv85 = jp87
    return retv85
}

func print_opt_int(x__20 Option__int32) struct{} {
    switch x__20.(type) {
    case None:
        println__T_string("none")
    case Some:
        var x35 int32 = x__20.(Some)._0
        var v__21 int32 = x35
        println__T_int32(v__21)
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func main0() struct{} {
    var m1__22 *hashmap_Key_int32_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__Key____V__int32()
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(m1__22, A{}, 10)
    var t97 Key = B{
        _0: 1,
    }
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(m1__22, t97, 20)
    var t98 int32 = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(m1__22)
    println__T_int32(t98)
    var t99 Option__int32 = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__Key____V__int32(m1__22, A{})
    print_opt_int(t99)
    var t100 Key = B{
        _0: 1,
    }
    var t101 bool = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(m1__22, t100)
    println__T_bool(t101)
    var t102 Key = B{
        _0: 1,
    }
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_remove____K__Key____V__int32(m1__22, t102)
    var t103 Key = B{
        _0: 1,
    }
    var t104 bool = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(m1__22, t103)
    println__T_bool(t104)
    var t105 int32 = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(m1__22)
    println__T_int32(t105)
    var m2__23 *hashmap_Ref_5Point_int32_x = _goml_m_inherent_i_HashMap_i_H_h8965aba642d8a393e2049e4587079c44__r_____V__int32()
    var t106 Point = Point{
        x: 1,
        y: 2,
    }
    var p1__24 *ref_Point_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Point(t106)
    var t107 Point = Point{
        x: 1,
        y: 2,
    }
    var p2__25 *ref_Point_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Point(t107)
    _goml_m_inherent_i_HashMap_i_H_h92a9b8ea001265f897bc67524ef74086__r_____V__int32(m2__23, p1__24, 99)
    var t108 Option__int32 = _goml_m_inherent_i_HashMap_i_H_h8816d503fd85179e4f674a4a9db321b9__r_____V__int32(m2__23, p2__25)
    print_opt_int(t108)
    var m3__26 *hashmap_Ref_3Key_int32_x = _goml_m_inherent_i_HashMap_i_H_h35e4bb85f0aaff40bae5445e99f77c49__r_____V__int32()
    var t109 Key = B{
        _0: 7,
    }
    var k1__27 *ref_Key_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Key(t109)
    var t110 Key = B{
        _0: 7,
    }
    var k2__28 *ref_Key_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Key(t110)
    _goml_m_inherent_i_HashMap_i_H_h8d7eb9d75c42de8655645502911541c0__r_____V__int32(m3__26, k1__27, 123)
    var t111 Option__int32 = _goml_m_inherent_i_HashMap_i_H_h488c149fa298920fe2fbb4be132c4bc3__r_____V__int32(m3__26, k2__28)
    print_opt_int(t111)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__33 int32, other__34 int32) bool {
    var retv113 bool
    var t114 bool = self__33 == other__34
    retv113 = t114
    return retv113
}

func _goml_m_trait__impl_i_Hash_i_int32_i_hash(self__55 int32) uint64 {
    var retv116 uint64
    var t117 uint64 = _goml_runtime_core_int32_hash(self__55)
    retv116 = t117
    return retv116
}

func println__T_string(value__1 string) struct{} {
    var t119 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t119)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t122 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t122)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__Key____V__int32() *hashmap_Key_int32_x {
    var retv125 *hashmap_Key_int32_x
    var t126 *hashmap_Key_int32_x = hashmap_new__HashMap_3Key_5int32()
    retv125 = t126
    return retv125
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(self__129 *hashmap_Key_int32_x, key__130 Key, value__131 int32) struct{} {
    hashmap_set__HashMap_3Key_5int32(self__129, key__130, value__131)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(self__134 *hashmap_Key_int32_x) int32 {
    var retv130 int32
    var t131 int32 = hashmap_len__HashMap_3Key_5int32(self__134)
    retv130 = t131
    return retv130
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__Key____V__int32(self__127 *hashmap_Key_int32_x, key__128 Key) Option__int32 {
    var retv133 Option__int32
    var t134 Option__int32 = hashmap_get__HashMap_3Key_5int32(self__127, key__128)
    retv133 = t134
    return retv133
}

func println__T_bool(value__1 bool) struct{} {
    var t136 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t136)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(self__135 *hashmap_Key_int32_x, key__136 Key) bool {
    var retv139 bool
    var t140 bool = hashmap_contains__HashMap_3Key_5int32(self__135, key__136)
    retv139 = t140
    return retv139
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_remove____K__Key____V__int32(self__132 *hashmap_Key_int32_x, key__133 Key) struct{} {
    hashmap_remove__HashMap_3Key_5int32(self__132, key__133)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h8965aba642d8a393e2049e4587079c44__r_____V__int32() *hashmap_Ref_5Point_int32_x {
    var retv144 *hashmap_Ref_5Point_int32_x
    var t145 *hashmap_Ref_5Point_int32_x = hashmap_new__HashMap_10Ref_5Point_5int32()
    retv144 = t145
    return retv144
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Point(value__137 Point) *ref_Point_x {
    var retv147 *ref_Point_x
    var t148 *ref_Point_x = ref__Ref_5Point(value__137)
    retv147 = t148
    return retv147
}

func _goml_m_inherent_i_HashMap_i_H_h92a9b8ea001265f897bc67524ef74086__r_____V__int32(self__129 *hashmap_Ref_5Point_int32_x, key__130 *ref_Point_x, value__131 int32) struct{} {
    hashmap_set__HashMap_10Ref_5Point_5int32(self__129, key__130, value__131)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h8816d503fd85179e4f674a4a9db321b9__r_____V__int32(self__127 *hashmap_Ref_5Point_int32_x, key__128 *ref_Point_x) Option__int32 {
    var retv152 Option__int32
    var t153 Option__int32 = hashmap_get__HashMap_10Ref_5Point_5int32(self__127, key__128)
    retv152 = t153
    return retv152
}

func _goml_m_inherent_i_HashMap_i_H_h35e4bb85f0aaff40bae5445e99f77c49__r_____V__int32() *hashmap_Ref_3Key_int32_x {
    var retv155 *hashmap_Ref_3Key_int32_x
    var t156 *hashmap_Ref_3Key_int32_x = hashmap_new__HashMap_8Ref_3Key_5int32()
    retv155 = t156
    return retv155
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Key(value__137 Key) *ref_Key_x {
    var retv158 *ref_Key_x
    var t159 *ref_Key_x = ref__Ref_3Key(value__137)
    retv158 = t159
    return retv158
}

func _goml_m_inherent_i_HashMap_i_H_h8d7eb9d75c42de8655645502911541c0__r_____V__int32(self__129 *hashmap_Ref_3Key_int32_x, key__130 *ref_Key_x, value__131 int32) struct{} {
    hashmap_set__HashMap_8Ref_3Key_5int32(self__129, key__130, value__131)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h488c149fa298920fe2fbb4be132c4bc3__r_____V__int32(self__127 *hashmap_Ref_3Key_int32_x, key__128 *ref_Key_x) Option__int32 {
    var retv163 Option__int32
    var t164 Option__int32 = hashmap_get__HashMap_8Ref_3Key_5int32(self__127, key__128)
    retv163 = t164
    return retv163
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv166 string
    retv166 = self__9
    return retv166
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv168 string
    var t169 string = _goml_runtime_core_int32_to_string(self__13)
    retv168 = t169
    return retv168
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__8 bool) string {
    var retv171 string
    var t172 string = _goml_runtime_core_bool_to_string(self__8)
    retv171 = t172
    return retv171
}

func _goml_m_trait__impl_i_Eq_i_Ref_l_Point_r__i_eq(self__63 *ref_Point_x, other__64 *ref_Point_x) bool {
    var retv174 bool
    var a__65 Point = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Point(self__63)
    var b__66 Point = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Point(other__64)
    var t175 bool = _goml_m_trait__impl_i_Eq_i_Point_i_eq(a__65, b__66)
    retv174 = t175
    return retv174
}

func _goml_m_trait__impl_i_Hash_i_Ref_l_Point_r__i_hash(self__67 *ref_Point_x) uint64 {
    var retv177 uint64
    var v__68 Point = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Point(self__67)
    var t178 uint64 = _goml_m_trait__impl_i_Hash_i_Point_i_hash(v__68)
    retv177 = t178
    return retv177
}

func _goml_m_trait__impl_i_Eq_i_Ref_l_Key_r__i_eq(self__63 *ref_Key_x, other__64 *ref_Key_x) bool {
    var retv180 bool
    var a__65 Key = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Key(self__63)
    var b__66 Key = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Key(other__64)
    var t181 bool = _goml_m_trait__impl_i_Eq_i_Key_i_eq(a__65, b__66)
    retv180 = t181
    return retv180
}

func _goml_m_trait__impl_i_Hash_i_Ref_l_Key_r__i_hash(self__67 *ref_Key_x) uint64 {
    var retv183 uint64
    var v__68 Key = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Key(self__67)
    var t184 uint64 = _goml_m_trait__impl_i_Hash_i_Key_i_hash(v__68)
    retv183 = t184
    return retv183
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Point(self__138 *ref_Point_x) Point {
    var retv186 Point
    var t187 Point = ref_get__Ref_5Point(self__138)
    retv186 = t187
    return retv186
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Key(self__138 *ref_Key_x) Key {
    var retv189 Key
    var t190 Key = ref_get__Ref_3Key(self__138)
    retv189 = t190
    return retv189
}

func main() {
    main0()
}
