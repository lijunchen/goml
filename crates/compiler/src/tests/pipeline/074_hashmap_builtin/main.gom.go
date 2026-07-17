package main

import (
    _goml_fmt "fmt"
    _goml_reflect "reflect"
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

func ref_set__Ref_5Point(reference *ref_Point_x, value Point) struct{} {
    reference.value = value
    return struct{}{}
}

func ptr_eq__Ref_5Point(a *ref_Point_x, b *ref_Point_x) bool {
    return a == b
}

func ptr_hash__Ref_5Point(reference *ref_Point_x) uint64 {
    return uint64(_goml_reflect.ValueOf(reference).Pointer())
}

type ref_Key_x struct {
    value Key
}

func ref__Ref_3Key(value Key) *ref_Key_x {
    return &ref_Key_x{
        value: value,
    }
}

func ptr_eq__Ref_3Key(a *ref_Key_x, b *ref_Key_x) bool {
    return a == b
}

func ptr_hash__Ref_3Key(reference *ref_Key_x) uint64 {
    return uint64(_goml_reflect.ValueOf(reference).Pointer())
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
    var reuse_index int32 = -1
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
        if !entry.active && reuse_index < 0 {
            reuse_index = i
        }
        i = i + 1
    }
    if reuse_index >= 0 {
        bucket[reuse_index] = hashmap_Key_int32_x_entry{
            active: true,
            key: key,
            value: value,
        }
        m.len = m.len + 1
        return struct{}{}
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
            var zero hashmap_Key_int32_x_entry
            bucket[i] = zero
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
    var reuse_index int32 = -1
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
        if !entry.active && reuse_index < 0 {
            reuse_index = i
        }
        i = i + 1
    }
    if reuse_index >= 0 {
        bucket[reuse_index] = hashmap_Ref_5Point_int32_x_entry{
            active: true,
            key: key,
            value: value,
        }
        m.len = m.len + 1
        return struct{}{}
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
    var reuse_index int32 = -1
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
        if !entry.active && reuse_index < 0 {
            reuse_index = i
        }
        i = i + 1
    }
    if reuse_index >= 0 {
        bucket[reuse_index] = hashmap_Ref_3Key_int32_x_entry{
            active: true,
            key: key,
            value: value,
        }
        m.len = m.len + 1
        return struct{}{}
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
    var retv53 bool
    var jp57 bool
    if true {
        var t61 int32 = self__0.x
        var t62 int32 = other__1.x
        var t63 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(t61, t62)
        jp57 = t63
    } else {
        jp57 = false
    }
    var jp55 bool
    if jp57 {
        var t58 int32 = self__0.y
        var t59 int32 = other__1.y
        var t60 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(t58, t59)
        jp55 = t60
    } else {
        jp55 = false
    }
    retv53 = jp55
    return retv53
}

func _goml_m_trait__impl_i_Hash_i_Point_i_hash(self__2 Point) uint64 {
    var retv65 uint64
    var h__3 uint64 = 14695981039346656037
    var t66 uint64 = h__3 * 1099511628211
    var t67 int32 = self__2.x
    var t68 uint64 = _goml_m_trait__impl_i_Hash_i_int32_i_hash(t67)
    var h__4 uint64 = t66 + t68
    var t69 uint64 = h__4 * 1099511628211
    var t70 int32 = self__2.y
    var t71 uint64 = _goml_m_trait__impl_i_Hash_i_int32_i_hash(t70)
    var h__5 uint64 = t69 + t71
    retv65 = h__5
    return retv65
}

func _goml_m_trait__impl_i_Eq_i_Key_i_eq(self__6 Key, other__7 Key) bool {
    var retv73 bool
    var mtmp22 Tuple2_3Key_3Key = Tuple2_3Key_3Key{
        _0: self__6,
        _1: other__7,
    }
    var x23 Key = mtmp22._0
    var x24 Key = mtmp22._1
    var jp75 bool
    switch x24.(type) {
    case A:
        var jp77 bool
        switch x23.(type) {
        case A:
            jp77 = true
        case B:
            jp77 = false
        case P:
            jp77 = false
        default:
            panic("non-exhaustive match")
        }
        jp75 = jp77
    case B:
        var x25 int32 = x24.(B)._0
        var jp79 bool
        switch x23.(type) {
        case A:
            jp79 = false
        case B:
            var x29 int32 = x23.(B)._0
            var __l1_0__8 int32 = x29
            var __r1_0__9 int32 = x25
            var jp81 bool
            if true {
                var t82 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(__l1_0__8, __r1_0__9)
                jp81 = t82
            } else {
                jp81 = false
            }
            jp79 = jp81
        case P:
            jp79 = false
        default:
            panic("non-exhaustive match")
        }
        jp75 = jp79
    case P:
        var x26 Point = x24.(P)._0
        var jp84 bool
        switch x23.(type) {
        case A:
            jp84 = false
        case B:
            jp84 = false
        case P:
            var x32 Point = x23.(P)._0
            var __l2_0__10 Point = x32
            var __r2_0__11 Point = x26
            var jp86 bool
            if true {
                var t87 bool = _goml_m_trait__impl_i_Eq_i_Point_i_eq(__l2_0__10, __r2_0__11)
                jp86 = t87
            } else {
                jp86 = false
            }
            jp84 = jp86
        default:
            panic("non-exhaustive match")
        }
        jp75 = jp84
    default:
        panic("non-exhaustive match")
    }
    retv73 = jp75
    return retv73
}

func _goml_m_trait__impl_i_Hash_i_Key_i_hash(self__12 Key) uint64 {
    var retv89 uint64
    var jp91 uint64
    switch self__12.(type) {
    case A:
        var h__13 uint64 = 14695981039346656037 + 1
        jp91 = h__13
    case B:
        var x33 int32 = self__12.(B)._0
        var __field1_0__14 int32 = x33
        var h__15 uint64 = 14695981039346656037 + 2
        var t92 uint64 = h__15 * 1099511628211
        var t93 uint64 = _goml_m_trait__impl_i_Hash_i_int32_i_hash(__field1_0__14)
        var h__16 uint64 = t92 + t93
        jp91 = h__16
    case P:
        var x34 Point = self__12.(P)._0
        var __field2_0__17 Point = x34
        var h__18 uint64 = 14695981039346656037 + 3
        var t94 uint64 = h__18 * 1099511628211
        var t95 uint64 = _goml_m_trait__impl_i_Hash_i_Point_i_hash(__field2_0__17)
        var h__19 uint64 = t94 + t95
        jp91 = h__19
    default:
        panic("non-exhaustive match")
    }
    retv89 = jp91
    return retv89
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
    var t101 Key = B{
        _0: 1,
    }
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(m1__22, t101, 20)
    var t102 int32 = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(m1__22)
    println__T_int32(t102)
    var t103 Option__int32 = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__Key____V__int32(m1__22, A{})
    print_opt_int(t103)
    var t104 Key = B{
        _0: 1,
    }
    var t105 bool = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(m1__22, t104)
    println__T_bool(t105)
    var t106 Key = B{
        _0: 1,
    }
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_remove____K__Key____V__int32(m1__22, t106)
    var t107 Key = B{
        _0: 1,
    }
    var t108 bool = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(m1__22, t107)
    println__T_bool(t108)
    var t109 int32 = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(m1__22)
    println__T_int32(t109)
    var m2__23 *hashmap_Ref_5Point_int32_x = _goml_m_inherent_i_HashMap_i_H_h8965aba642d8a393e2049e4587079c44__r_____V__int32()
    var t110 Point = Point{
        x: 1,
        y: 2,
    }
    var p1__24 *ref_Point_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Point(t110)
    var t111 Point = Point{
        x: 1,
        y: 2,
    }
    var p2__25 *ref_Point_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Point(t111)
    _goml_m_inherent_i_HashMap_i_H_h92a9b8ea001265f897bc67524ef74086__r_____V__int32(m2__23, p1__24, 99)
    var t112 Option__int32 = _goml_m_inherent_i_HashMap_i_H_h8816d503fd85179e4f674a4a9db321b9__r_____V__int32(m2__23, p1__24)
    print_opt_int(t112)
    var t113 Option__int32 = _goml_m_inherent_i_HashMap_i_H_h8816d503fd85179e4f674a4a9db321b9__r_____V__int32(m2__23, p2__25)
    print_opt_int(t113)
    var t114 Point = Point{
        x: 9,
        y: 8,
    }
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Point(p1__24, t114)
    var t115 Option__int32 = _goml_m_inherent_i_HashMap_i_H_h8816d503fd85179e4f674a4a9db321b9__r_____V__int32(m2__23, p1__24)
    print_opt_int(t115)
    var t116 bool = _goml_m_trait__impl_i_Eq_i_Ref_l_Point_r__i_eq(p1__24, p2__25)
    println__T_bool(t116)
    var m3__26 *hashmap_Ref_3Key_int32_x = _goml_m_inherent_i_HashMap_i_H_h35e4bb85f0aaff40bae5445e99f77c49__r_____V__int32()
    var t117 Key = B{
        _0: 7,
    }
    var k1__27 *ref_Key_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Key(t117)
    var k2__28 *ref_Key_x = k1__27
    _goml_m_inherent_i_HashMap_i_H_h8d7eb9d75c42de8655645502911541c0__r_____V__int32(m3__26, k1__27, 123)
    var t118 Option__int32 = _goml_m_inherent_i_HashMap_i_H_h488c149fa298920fe2fbb4be132c4bc3__r_____V__int32(m3__26, k2__28)
    print_opt_int(t118)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__33 int32, other__34 int32) bool {
    var retv120 bool
    var t121 bool = self__33 == other__34
    retv120 = t121
    return retv120
}

func _goml_m_trait__impl_i_Hash_i_int32_i_hash(self__55 int32) uint64 {
    var retv123 uint64
    var t124 uint64 = _goml_runtime_core_int32_hash(self__55)
    retv123 = t124
    return retv123
}

func println__T_string(value__1 string) struct{} {
    var t126 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t126)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t129 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t129)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__Key____V__int32() *hashmap_Key_int32_x {
    var retv132 *hashmap_Key_int32_x
    var t133 *hashmap_Key_int32_x = hashmap_new__HashMap_3Key_5int32()
    retv132 = t133
    return retv132
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(self__129 *hashmap_Key_int32_x, key__130 Key, value__131 int32) struct{} {
    hashmap_set__HashMap_3Key_5int32(self__129, key__130, value__131)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(self__134 *hashmap_Key_int32_x) int32 {
    var retv137 int32
    var t138 int32 = hashmap_len__HashMap_3Key_5int32(self__134)
    retv137 = t138
    return retv137
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__Key____V__int32(self__127 *hashmap_Key_int32_x, key__128 Key) Option__int32 {
    var retv140 Option__int32
    var t141 Option__int32 = hashmap_get__HashMap_3Key_5int32(self__127, key__128)
    retv140 = t141
    return retv140
}

func println__T_bool(value__1 bool) struct{} {
    var t143 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t143)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(self__135 *hashmap_Key_int32_x, key__136 Key) bool {
    var retv146 bool
    var t147 bool = hashmap_contains__HashMap_3Key_5int32(self__135, key__136)
    retv146 = t147
    return retv146
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_remove____K__Key____V__int32(self__132 *hashmap_Key_int32_x, key__133 Key) struct{} {
    hashmap_remove__HashMap_3Key_5int32(self__132, key__133)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h8965aba642d8a393e2049e4587079c44__r_____V__int32() *hashmap_Ref_5Point_int32_x {
    var retv151 *hashmap_Ref_5Point_int32_x
    var t152 *hashmap_Ref_5Point_int32_x = hashmap_new__HashMap_10Ref_5Point_5int32()
    retv151 = t152
    return retv151
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Point(value__137 Point) *ref_Point_x {
    var retv154 *ref_Point_x
    var t155 *ref_Point_x = ref__Ref_5Point(value__137)
    retv154 = t155
    return retv154
}

func _goml_m_inherent_i_HashMap_i_H_h92a9b8ea001265f897bc67524ef74086__r_____V__int32(self__129 *hashmap_Ref_5Point_int32_x, key__130 *ref_Point_x, value__131 int32) struct{} {
    hashmap_set__HashMap_10Ref_5Point_5int32(self__129, key__130, value__131)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h8816d503fd85179e4f674a4a9db321b9__r_____V__int32(self__127 *hashmap_Ref_5Point_int32_x, key__128 *ref_Point_x) Option__int32 {
    var retv159 Option__int32
    var t160 Option__int32 = hashmap_get__HashMap_10Ref_5Point_5int32(self__127, key__128)
    retv159 = t160
    return retv159
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Point(self__139 *ref_Point_x, value__140 Point) struct{} {
    ref_set__Ref_5Point(self__139, value__140)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_Ref_l_Point_r__i_eq(self__63 *ref_Point_x, other__64 *ref_Point_x) bool {
    var retv164 bool
    var t165 bool = ptr_eq__Ref_5Point(self__63, other__64)
    retv164 = t165
    return retv164
}

func _goml_m_inherent_i_HashMap_i_H_h35e4bb85f0aaff40bae5445e99f77c49__r_____V__int32() *hashmap_Ref_3Key_int32_x {
    var retv167 *hashmap_Ref_3Key_int32_x
    var t168 *hashmap_Ref_3Key_int32_x = hashmap_new__HashMap_8Ref_3Key_5int32()
    retv167 = t168
    return retv167
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Key(value__137 Key) *ref_Key_x {
    var retv170 *ref_Key_x
    var t171 *ref_Key_x = ref__Ref_3Key(value__137)
    retv170 = t171
    return retv170
}

func _goml_m_inherent_i_HashMap_i_H_h8d7eb9d75c42de8655645502911541c0__r_____V__int32(self__129 *hashmap_Ref_3Key_int32_x, key__130 *ref_Key_x, value__131 int32) struct{} {
    hashmap_set__HashMap_8Ref_3Key_5int32(self__129, key__130, value__131)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h488c149fa298920fe2fbb4be132c4bc3__r_____V__int32(self__127 *hashmap_Ref_3Key_int32_x, key__128 *ref_Key_x) Option__int32 {
    var retv175 Option__int32
    var t176 Option__int32 = hashmap_get__HashMap_8Ref_3Key_5int32(self__127, key__128)
    retv175 = t176
    return retv175
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv178 string
    retv178 = self__9
    return retv178
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv180 string
    var t181 string = _goml_runtime_core_int32_to_string(self__13)
    retv180 = t181
    return retv180
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__8 bool) string {
    var retv183 string
    var t184 string = _goml_runtime_core_bool_to_string(self__8)
    retv183 = t184
    return retv183
}

func _goml_m_trait__impl_i_Hash_i_Ref_l_Point_r__i_hash(self__65 *ref_Point_x) uint64 {
    var retv186 uint64
    var t187 uint64 = ptr_hash__Ref_5Point(self__65)
    retv186 = t187
    return retv186
}

func _goml_m_trait__impl_i_Eq_i_Ref_l_Key_r__i_eq(self__63 *ref_Key_x, other__64 *ref_Key_x) bool {
    var retv189 bool
    var t190 bool = ptr_eq__Ref_3Key(self__63, other__64)
    retv189 = t190
    return retv189
}

func _goml_m_trait__impl_i_Hash_i_Ref_l_Key_r__i_hash(self__65 *ref_Key_x) uint64 {
    var retv192 uint64
    var t193 uint64 = ptr_hash__Ref_3Key(self__65)
    retv192 = t193
    return retv192
}

func main() {
    main0()
}
