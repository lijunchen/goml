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
    var retv31 bool
    var jp35 bool
    if true {
        var t39 int32 = self__0.x
        var t40 int32 = other__1.x
        var t41 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(t39, t40)
        jp35 = t41
    } else {
        jp35 = false
    }
    var jp33 bool
    if jp35 {
        var t36 int32 = self__0.y
        var t37 int32 = other__1.y
        var t38 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(t36, t37)
        jp33 = t38
    } else {
        jp33 = false
    }
    retv31 = jp33
    return retv31
}

func _goml_m_trait__impl_i_Hash_i_Point_i_hash(self__2 Point) uint64 {
    var retv43 uint64
    var h__3 uint64 = 14695981039346656037
    var t44 uint64 = h__3 * 1099511628211
    var t45 int32 = self__2.x
    var t46 uint64 = _goml_m_trait__impl_i_Hash_i_int32_i_hash(t45)
    var h__4 uint64 = t44 + t46
    var t47 uint64 = h__4 * 1099511628211
    var t48 int32 = self__2.y
    var t49 uint64 = _goml_m_trait__impl_i_Hash_i_int32_i_hash(t48)
    var h__5 uint64 = t47 + t49
    retv43 = h__5
    return retv43
}

func _goml_m_trait__impl_i_Eq_i_Key_i_eq(self__6 Key, other__7 Key) bool {
    var retv51 bool
    var mtmp4 Tuple2_3Key_3Key = Tuple2_3Key_3Key{
        _0: self__6,
        _1: other__7,
    }
    var x5 Key = mtmp4._0
    var x6 Key = mtmp4._1
    var jp53 bool
    switch x6.(type) {
    case A:
        var jp55 bool
        switch x5.(type) {
        case A:
            jp55 = true
        case B:
            jp55 = false
        case P:
            jp55 = false
        default:
            panic("non-exhaustive match")
        }
        jp53 = jp55
    case B:
        var x7 int32 = x6.(B)._0
        var jp57 bool
        switch x5.(type) {
        case A:
            jp57 = false
        case B:
            var x11 int32 = x5.(B)._0
            var __l1_0__8 int32 = x11
            var __r1_0__9 int32 = x7
            var jp59 bool
            if true {
                var t60 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(__l1_0__8, __r1_0__9)
                jp59 = t60
            } else {
                jp59 = false
            }
            jp57 = jp59
        case P:
            jp57 = false
        default:
            panic("non-exhaustive match")
        }
        jp53 = jp57
    case P:
        var x8 Point = x6.(P)._0
        var jp62 bool
        switch x5.(type) {
        case A:
            jp62 = false
        case B:
            jp62 = false
        case P:
            var x14 Point = x5.(P)._0
            var __l2_0__10 Point = x14
            var __r2_0__11 Point = x8
            var jp64 bool
            if true {
                var t65 bool = _goml_m_trait__impl_i_Eq_i_Point_i_eq(__l2_0__10, __r2_0__11)
                jp64 = t65
            } else {
                jp64 = false
            }
            jp62 = jp64
        default:
            panic("non-exhaustive match")
        }
        jp53 = jp62
    default:
        panic("non-exhaustive match")
    }
    retv51 = jp53
    return retv51
}

func _goml_m_trait__impl_i_Hash_i_Key_i_hash(self__12 Key) uint64 {
    var retv67 uint64
    var jp69 uint64
    switch self__12.(type) {
    case A:
        var h__13 uint64 = 14695981039346656037 + 1
        jp69 = h__13
    case B:
        var x15 int32 = self__12.(B)._0
        var __field1_0__14 int32 = x15
        var h__15 uint64 = 14695981039346656037 + 2
        var t70 uint64 = h__15 * 1099511628211
        var t71 uint64 = _goml_m_trait__impl_i_Hash_i_int32_i_hash(__field1_0__14)
        var h__16 uint64 = t70 + t71
        jp69 = h__16
    case P:
        var x16 Point = self__12.(P)._0
        var __field2_0__17 Point = x16
        var h__18 uint64 = 14695981039346656037 + 3
        var t72 uint64 = h__18 * 1099511628211
        var t73 uint64 = _goml_m_trait__impl_i_Hash_i_Point_i_hash(__field2_0__17)
        var h__19 uint64 = t72 + t73
        jp69 = h__19
    default:
        panic("non-exhaustive match")
    }
    retv67 = jp69
    return retv67
}

func print_opt_int(x__20 Option__int32) struct{} {
    switch x__20.(type) {
    case None:
        println__T_string("none")
    case Some:
        var x17 int32 = x__20.(Some)._0
        var v__21 int32 = x17
        println__T_int32(v__21)
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func main0() struct{} {
    var m1__22 *hashmap_Key_int32_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__Key____V__int32()
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(m1__22, A{}, 10)
    var t79 Key = B{
        _0: 1,
    }
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(m1__22, t79, 20)
    var t80 int32 = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(m1__22)
    println__T_int32(t80)
    var t81 Option__int32 = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__Key____V__int32(m1__22, A{})
    print_opt_int(t81)
    var t82 Key = B{
        _0: 1,
    }
    var t83 bool = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(m1__22, t82)
    println__T_bool(t83)
    var t84 Key = B{
        _0: 1,
    }
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_remove____K__Key____V__int32(m1__22, t84)
    var t85 Key = B{
        _0: 1,
    }
    var t86 bool = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(m1__22, t85)
    println__T_bool(t86)
    var t87 int32 = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(m1__22)
    println__T_int32(t87)
    var m2__23 *hashmap_Ref_5Point_int32_x = _goml_m_inherent_i_HashMap_i_H_h8965aba642d8a393e2049e4587079c44__r_____V__int32()
    var t88 Point = Point{
        x: 1,
        y: 2,
    }
    var p1__24 *ref_Point_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Point(t88)
    var t89 Point = Point{
        x: 1,
        y: 2,
    }
    var p2__25 *ref_Point_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Point(t89)
    _goml_m_inherent_i_HashMap_i_H_h92a9b8ea001265f897bc67524ef74086__r_____V__int32(m2__23, p1__24, 99)
    var t90 Option__int32 = _goml_m_inherent_i_HashMap_i_H_h8816d503fd85179e4f674a4a9db321b9__r_____V__int32(m2__23, p2__25)
    print_opt_int(t90)
    var m3__26 *hashmap_Ref_3Key_int32_x = _goml_m_inherent_i_HashMap_i_H_h35e4bb85f0aaff40bae5445e99f77c49__r_____V__int32()
    var t91 Key = B{
        _0: 7,
    }
    var k1__27 *ref_Key_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Key(t91)
    var t92 Key = B{
        _0: 7,
    }
    var k2__28 *ref_Key_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Key(t92)
    _goml_m_inherent_i_HashMap_i_H_h8d7eb9d75c42de8655645502911541c0__r_____V__int32(m3__26, k1__27, 123)
    var t93 Option__int32 = _goml_m_inherent_i_HashMap_i_H_h488c149fa298920fe2fbb4be132c4bc3__r_____V__int32(m3__26, k2__28)
    print_opt_int(t93)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__33 int32, other__34 int32) bool {
    var retv95 bool
    var t96 bool = self__33 == other__34
    retv95 = t96
    return retv95
}

func _goml_m_trait__impl_i_Hash_i_int32_i_hash(self__55 int32) uint64 {
    var retv98 uint64
    var t99 uint64 = _goml_runtime_core_int32_hash(self__55)
    retv98 = t99
    return retv98
}

func println__T_string(value__1 string) struct{} {
    var t101 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t101)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t104 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t104)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__Key____V__int32() *hashmap_Key_int32_x {
    var retv107 *hashmap_Key_int32_x
    var t108 *hashmap_Key_int32_x = hashmap_new__HashMap_3Key_5int32()
    retv107 = t108
    return retv107
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(self__94 *hashmap_Key_int32_x, key__95 Key, value__96 int32) struct{} {
    hashmap_set__HashMap_3Key_5int32(self__94, key__95, value__96)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(self__99 *hashmap_Key_int32_x) int32 {
    var retv112 int32
    var t113 int32 = hashmap_len__HashMap_3Key_5int32(self__99)
    retv112 = t113
    return retv112
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__Key____V__int32(self__92 *hashmap_Key_int32_x, key__93 Key) Option__int32 {
    var retv115 Option__int32
    var t116 Option__int32 = hashmap_get__HashMap_3Key_5int32(self__92, key__93)
    retv115 = t116
    return retv115
}

func println__T_bool(value__1 bool) struct{} {
    var t118 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t118)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(self__100 *hashmap_Key_int32_x, key__101 Key) bool {
    var retv121 bool
    var t122 bool = hashmap_contains__HashMap_3Key_5int32(self__100, key__101)
    retv121 = t122
    return retv121
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_remove____K__Key____V__int32(self__97 *hashmap_Key_int32_x, key__98 Key) struct{} {
    hashmap_remove__HashMap_3Key_5int32(self__97, key__98)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h8965aba642d8a393e2049e4587079c44__r_____V__int32() *hashmap_Ref_5Point_int32_x {
    var retv126 *hashmap_Ref_5Point_int32_x
    var t127 *hashmap_Ref_5Point_int32_x = hashmap_new__HashMap_10Ref_5Point_5int32()
    retv126 = t127
    return retv126
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Point(value__102 Point) *ref_Point_x {
    var retv129 *ref_Point_x
    var t130 *ref_Point_x = ref__Ref_5Point(value__102)
    retv129 = t130
    return retv129
}

func _goml_m_inherent_i_HashMap_i_H_h92a9b8ea001265f897bc67524ef74086__r_____V__int32(self__94 *hashmap_Ref_5Point_int32_x, key__95 *ref_Point_x, value__96 int32) struct{} {
    hashmap_set__HashMap_10Ref_5Point_5int32(self__94, key__95, value__96)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h8816d503fd85179e4f674a4a9db321b9__r_____V__int32(self__92 *hashmap_Ref_5Point_int32_x, key__93 *ref_Point_x) Option__int32 {
    var retv134 Option__int32
    var t135 Option__int32 = hashmap_get__HashMap_10Ref_5Point_5int32(self__92, key__93)
    retv134 = t135
    return retv134
}

func _goml_m_inherent_i_HashMap_i_H_h35e4bb85f0aaff40bae5445e99f77c49__r_____V__int32() *hashmap_Ref_3Key_int32_x {
    var retv137 *hashmap_Ref_3Key_int32_x
    var t138 *hashmap_Ref_3Key_int32_x = hashmap_new__HashMap_8Ref_3Key_5int32()
    retv137 = t138
    return retv137
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Key(value__102 Key) *ref_Key_x {
    var retv140 *ref_Key_x
    var t141 *ref_Key_x = ref__Ref_3Key(value__102)
    retv140 = t141
    return retv140
}

func _goml_m_inherent_i_HashMap_i_H_h8d7eb9d75c42de8655645502911541c0__r_____V__int32(self__94 *hashmap_Ref_3Key_int32_x, key__95 *ref_Key_x, value__96 int32) struct{} {
    hashmap_set__HashMap_8Ref_3Key_5int32(self__94, key__95, value__96)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h488c149fa298920fe2fbb4be132c4bc3__r_____V__int32(self__92 *hashmap_Ref_3Key_int32_x, key__93 *ref_Key_x) Option__int32 {
    var retv145 Option__int32
    var t146 Option__int32 = hashmap_get__HashMap_8Ref_3Key_5int32(self__92, key__93)
    retv145 = t146
    return retv145
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv148 string
    retv148 = self__9
    return retv148
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv150 string
    var t151 string = _goml_runtime_core_int32_to_string(self__13)
    retv150 = t151
    return retv150
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__8 bool) string {
    var retv153 string
    var t154 string = _goml_runtime_core_bool_to_string(self__8)
    retv153 = t154
    return retv153
}

func _goml_m_trait__impl_i_Eq_i_Ref_l_Point_r__i_eq(self__63 *ref_Point_x, other__64 *ref_Point_x) bool {
    var retv156 bool
    var a__65 Point = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Point(self__63)
    var b__66 Point = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Point(other__64)
    var t157 bool = _goml_m_trait__impl_i_Eq_i_Point_i_eq(a__65, b__66)
    retv156 = t157
    return retv156
}

func _goml_m_trait__impl_i_Hash_i_Ref_l_Point_r__i_hash(self__67 *ref_Point_x) uint64 {
    var retv159 uint64
    var v__68 Point = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Point(self__67)
    var t160 uint64 = _goml_m_trait__impl_i_Hash_i_Point_i_hash(v__68)
    retv159 = t160
    return retv159
}

func _goml_m_trait__impl_i_Eq_i_Ref_l_Key_r__i_eq(self__63 *ref_Key_x, other__64 *ref_Key_x) bool {
    var retv162 bool
    var a__65 Key = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Key(self__63)
    var b__66 Key = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Key(other__64)
    var t163 bool = _goml_m_trait__impl_i_Eq_i_Key_i_eq(a__65, b__66)
    retv162 = t163
    return retv162
}

func _goml_m_trait__impl_i_Hash_i_Ref_l_Key_r__i_hash(self__67 *ref_Key_x) uint64 {
    var retv165 uint64
    var v__68 Key = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Key(self__67)
    var t166 uint64 = _goml_m_trait__impl_i_Hash_i_Key_i_hash(v__68)
    retv165 = t166
    return retv165
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Point(self__103 *ref_Point_x) Point {
    var retv168 Point
    var t169 Point = ref_get__Ref_5Point(self__103)
    retv168 = t169
    return retv168
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Key(self__103 *ref_Key_x) Key {
    var retv171 Key
    var t172 Key = ref_get__Ref_3Key(self__103)
    retv171 = t172
    return retv171
}

func main() {
    main0()
}
