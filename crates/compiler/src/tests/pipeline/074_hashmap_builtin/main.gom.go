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
    var retv34 bool
    var jp38 bool
    if true {
        var t42 int32 = self__0.x
        var t43 int32 = other__1.x
        var t44 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(t42, t43)
        jp38 = t44
    } else {
        jp38 = false
    }
    var jp36 bool
    if jp38 {
        var t39 int32 = self__0.y
        var t40 int32 = other__1.y
        var t41 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(t39, t40)
        jp36 = t41
    } else {
        jp36 = false
    }
    retv34 = jp36
    return retv34
}

func _goml_m_trait__impl_i_Hash_i_Point_i_hash(self__2 Point) uint64 {
    var retv46 uint64
    var h__3 uint64 = 14695981039346656037
    var t47 uint64 = h__3 * 1099511628211
    var t48 int32 = self__2.x
    var t49 uint64 = _goml_m_trait__impl_i_Hash_i_int32_i_hash(t48)
    var h__4 uint64 = t47 + t49
    var t50 uint64 = h__4 * 1099511628211
    var t51 int32 = self__2.y
    var t52 uint64 = _goml_m_trait__impl_i_Hash_i_int32_i_hash(t51)
    var h__5 uint64 = t50 + t52
    retv46 = h__5
    return retv46
}

func _goml_m_trait__impl_i_Eq_i_Key_i_eq(self__6 Key, other__7 Key) bool {
    var retv54 bool
    var mtmp7 Tuple2_3Key_3Key = Tuple2_3Key_3Key{
        _0: self__6,
        _1: other__7,
    }
    var x8 Key = mtmp7._0
    var x9 Key = mtmp7._1
    var jp56 bool
    switch x9.(type) {
    case A:
        var jp58 bool
        switch x8.(type) {
        case A:
            jp58 = true
        case B:
            jp58 = false
        case P:
            jp58 = false
        default:
            panic("non-exhaustive match")
        }
        jp56 = jp58
    case B:
        var x10 int32 = x9.(B)._0
        var jp60 bool
        switch x8.(type) {
        case A:
            jp60 = false
        case B:
            var x14 int32 = x8.(B)._0
            var __l1_0__8 int32 = x14
            var __r1_0__9 int32 = x10
            var jp62 bool
            if true {
                var t63 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(__l1_0__8, __r1_0__9)
                jp62 = t63
            } else {
                jp62 = false
            }
            jp60 = jp62
        case P:
            jp60 = false
        default:
            panic("non-exhaustive match")
        }
        jp56 = jp60
    case P:
        var x11 Point = x9.(P)._0
        var jp65 bool
        switch x8.(type) {
        case A:
            jp65 = false
        case B:
            jp65 = false
        case P:
            var x17 Point = x8.(P)._0
            var __l2_0__10 Point = x17
            var __r2_0__11 Point = x11
            var jp67 bool
            if true {
                var t68 bool = _goml_m_trait__impl_i_Eq_i_Point_i_eq(__l2_0__10, __r2_0__11)
                jp67 = t68
            } else {
                jp67 = false
            }
            jp65 = jp67
        default:
            panic("non-exhaustive match")
        }
        jp56 = jp65
    default:
        panic("non-exhaustive match")
    }
    retv54 = jp56
    return retv54
}

func _goml_m_trait__impl_i_Hash_i_Key_i_hash(self__12 Key) uint64 {
    var retv70 uint64
    var jp72 uint64
    switch self__12.(type) {
    case A:
        var h__13 uint64 = 14695981039346656037 + 1
        jp72 = h__13
    case B:
        var x18 int32 = self__12.(B)._0
        var __field1_0__14 int32 = x18
        var h__15 uint64 = 14695981039346656037 + 2
        var t73 uint64 = h__15 * 1099511628211
        var t74 uint64 = _goml_m_trait__impl_i_Hash_i_int32_i_hash(__field1_0__14)
        var h__16 uint64 = t73 + t74
        jp72 = h__16
    case P:
        var x19 Point = self__12.(P)._0
        var __field2_0__17 Point = x19
        var h__18 uint64 = 14695981039346656037 + 3
        var t75 uint64 = h__18 * 1099511628211
        var t76 uint64 = _goml_m_trait__impl_i_Hash_i_Point_i_hash(__field2_0__17)
        var h__19 uint64 = t75 + t76
        jp72 = h__19
    default:
        panic("non-exhaustive match")
    }
    retv70 = jp72
    return retv70
}

func print_opt_int(x__20 Option__int32) struct{} {
    switch x__20.(type) {
    case None:
        println__T_string("none")
    case Some:
        var x20 int32 = x__20.(Some)._0
        var v__21 int32 = x20
        println__T_int32(v__21)
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func main0() struct{} {
    var m1__22 *hashmap_Key_int32_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__Key____V__int32()
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(m1__22, A{}, 10)
    var t82 Key = B{
        _0: 1,
    }
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(m1__22, t82, 20)
    var t83 int32 = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(m1__22)
    println__T_int32(t83)
    var t84 Option__int32 = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__Key____V__int32(m1__22, A{})
    print_opt_int(t84)
    var t85 Key = B{
        _0: 1,
    }
    var t86 bool = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(m1__22, t85)
    println__T_bool(t86)
    var t87 Key = B{
        _0: 1,
    }
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_remove____K__Key____V__int32(m1__22, t87)
    var t88 Key = B{
        _0: 1,
    }
    var t89 bool = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(m1__22, t88)
    println__T_bool(t89)
    var t90 int32 = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(m1__22)
    println__T_int32(t90)
    var m2__23 *hashmap_Ref_5Point_int32_x = _goml_m_inherent_i_HashMap_i_H_h8965aba642d8a393e2049e4587079c44__r_____V__int32()
    var t91 Point = Point{
        x: 1,
        y: 2,
    }
    var p1__24 *ref_Point_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Point(t91)
    var t92 Point = Point{
        x: 1,
        y: 2,
    }
    var p2__25 *ref_Point_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Point(t92)
    _goml_m_inherent_i_HashMap_i_H_h92a9b8ea001265f897bc67524ef74086__r_____V__int32(m2__23, p1__24, 99)
    var t93 Option__int32 = _goml_m_inherent_i_HashMap_i_H_h8816d503fd85179e4f674a4a9db321b9__r_____V__int32(m2__23, p2__25)
    print_opt_int(t93)
    var m3__26 *hashmap_Ref_3Key_int32_x = _goml_m_inherent_i_HashMap_i_H_h35e4bb85f0aaff40bae5445e99f77c49__r_____V__int32()
    var t94 Key = B{
        _0: 7,
    }
    var k1__27 *ref_Key_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Key(t94)
    var t95 Key = B{
        _0: 7,
    }
    var k2__28 *ref_Key_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Key(t95)
    _goml_m_inherent_i_HashMap_i_H_h8d7eb9d75c42de8655645502911541c0__r_____V__int32(m3__26, k1__27, 123)
    var t96 Option__int32 = _goml_m_inherent_i_HashMap_i_H_h488c149fa298920fe2fbb4be132c4bc3__r_____V__int32(m3__26, k2__28)
    print_opt_int(t96)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__33 int32, other__34 int32) bool {
    var retv98 bool
    var t99 bool = self__33 == other__34
    retv98 = t99
    return retv98
}

func _goml_m_trait__impl_i_Hash_i_int32_i_hash(self__55 int32) uint64 {
    var retv101 uint64
    var t102 uint64 = _goml_runtime_core_int32_hash(self__55)
    retv101 = t102
    return retv101
}

func println__T_string(value__1 string) struct{} {
    var t104 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t104)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t107 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t107)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__Key____V__int32() *hashmap_Key_int32_x {
    var retv110 *hashmap_Key_int32_x
    var t111 *hashmap_Key_int32_x = hashmap_new__HashMap_3Key_5int32()
    retv110 = t111
    return retv110
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(self__106 *hashmap_Key_int32_x, key__107 Key, value__108 int32) struct{} {
    hashmap_set__HashMap_3Key_5int32(self__106, key__107, value__108)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(self__111 *hashmap_Key_int32_x) int32 {
    var retv115 int32
    var t116 int32 = hashmap_len__HashMap_3Key_5int32(self__111)
    retv115 = t116
    return retv115
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__Key____V__int32(self__104 *hashmap_Key_int32_x, key__105 Key) Option__int32 {
    var retv118 Option__int32
    var t119 Option__int32 = hashmap_get__HashMap_3Key_5int32(self__104, key__105)
    retv118 = t119
    return retv118
}

func println__T_bool(value__1 bool) struct{} {
    var t121 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t121)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(self__112 *hashmap_Key_int32_x, key__113 Key) bool {
    var retv124 bool
    var t125 bool = hashmap_contains__HashMap_3Key_5int32(self__112, key__113)
    retv124 = t125
    return retv124
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_remove____K__Key____V__int32(self__109 *hashmap_Key_int32_x, key__110 Key) struct{} {
    hashmap_remove__HashMap_3Key_5int32(self__109, key__110)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h8965aba642d8a393e2049e4587079c44__r_____V__int32() *hashmap_Ref_5Point_int32_x {
    var retv129 *hashmap_Ref_5Point_int32_x
    var t130 *hashmap_Ref_5Point_int32_x = hashmap_new__HashMap_10Ref_5Point_5int32()
    retv129 = t130
    return retv129
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Point(value__114 Point) *ref_Point_x {
    var retv132 *ref_Point_x
    var t133 *ref_Point_x = ref__Ref_5Point(value__114)
    retv132 = t133
    return retv132
}

func _goml_m_inherent_i_HashMap_i_H_h92a9b8ea001265f897bc67524ef74086__r_____V__int32(self__106 *hashmap_Ref_5Point_int32_x, key__107 *ref_Point_x, value__108 int32) struct{} {
    hashmap_set__HashMap_10Ref_5Point_5int32(self__106, key__107, value__108)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h8816d503fd85179e4f674a4a9db321b9__r_____V__int32(self__104 *hashmap_Ref_5Point_int32_x, key__105 *ref_Point_x) Option__int32 {
    var retv137 Option__int32
    var t138 Option__int32 = hashmap_get__HashMap_10Ref_5Point_5int32(self__104, key__105)
    retv137 = t138
    return retv137
}

func _goml_m_inherent_i_HashMap_i_H_h35e4bb85f0aaff40bae5445e99f77c49__r_____V__int32() *hashmap_Ref_3Key_int32_x {
    var retv140 *hashmap_Ref_3Key_int32_x
    var t141 *hashmap_Ref_3Key_int32_x = hashmap_new__HashMap_8Ref_3Key_5int32()
    retv140 = t141
    return retv140
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Key(value__114 Key) *ref_Key_x {
    var retv143 *ref_Key_x
    var t144 *ref_Key_x = ref__Ref_3Key(value__114)
    retv143 = t144
    return retv143
}

func _goml_m_inherent_i_HashMap_i_H_h8d7eb9d75c42de8655645502911541c0__r_____V__int32(self__106 *hashmap_Ref_3Key_int32_x, key__107 *ref_Key_x, value__108 int32) struct{} {
    hashmap_set__HashMap_8Ref_3Key_5int32(self__106, key__107, value__108)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h488c149fa298920fe2fbb4be132c4bc3__r_____V__int32(self__104 *hashmap_Ref_3Key_int32_x, key__105 *ref_Key_x) Option__int32 {
    var retv148 Option__int32
    var t149 Option__int32 = hashmap_get__HashMap_8Ref_3Key_5int32(self__104, key__105)
    retv148 = t149
    return retv148
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv151 string
    retv151 = self__9
    return retv151
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv153 string
    var t154 string = _goml_runtime_core_int32_to_string(self__13)
    retv153 = t154
    return retv153
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__8 bool) string {
    var retv156 string
    var t157 string = _goml_runtime_core_bool_to_string(self__8)
    retv156 = t157
    return retv156
}

func _goml_m_trait__impl_i_Eq_i_Ref_l_Point_r__i_eq(self__63 *ref_Point_x, other__64 *ref_Point_x) bool {
    var retv159 bool
    var a__65 Point = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Point(self__63)
    var b__66 Point = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Point(other__64)
    var t160 bool = _goml_m_trait__impl_i_Eq_i_Point_i_eq(a__65, b__66)
    retv159 = t160
    return retv159
}

func _goml_m_trait__impl_i_Hash_i_Ref_l_Point_r__i_hash(self__67 *ref_Point_x) uint64 {
    var retv162 uint64
    var v__68 Point = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Point(self__67)
    var t163 uint64 = _goml_m_trait__impl_i_Hash_i_Point_i_hash(v__68)
    retv162 = t163
    return retv162
}

func _goml_m_trait__impl_i_Eq_i_Ref_l_Key_r__i_eq(self__63 *ref_Key_x, other__64 *ref_Key_x) bool {
    var retv165 bool
    var a__65 Key = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Key(self__63)
    var b__66 Key = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Key(other__64)
    var t166 bool = _goml_m_trait__impl_i_Eq_i_Key_i_eq(a__65, b__66)
    retv165 = t166
    return retv165
}

func _goml_m_trait__impl_i_Hash_i_Ref_l_Key_r__i_hash(self__67 *ref_Key_x) uint64 {
    var retv168 uint64
    var v__68 Key = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Key(self__67)
    var t169 uint64 = _goml_m_trait__impl_i_Hash_i_Key_i_hash(v__68)
    retv168 = t169
    return retv168
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Point(self__115 *ref_Point_x) Point {
    var retv171 Point
    var t172 Point = ref_get__Ref_5Point(self__115)
    retv171 = t172
    return retv171
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Key(self__115 *ref_Key_x) Key {
    var retv174 Key
    var t175 Key = ref_get__Ref_3Key(self__115)
    retv174 = t175
    return retv174
}

func main() {
    main0()
}
