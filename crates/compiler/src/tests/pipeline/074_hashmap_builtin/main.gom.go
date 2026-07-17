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
    hashes []uint64
    len int32
}

func hashmap_new__HashMap_3Key_5int32() *hashmap_Key_int32_x {
    return &hashmap_Key_int32_x{
        buckets: make(map[uint64][]hashmap_Key_int32_x_entry),
        len: 0,
        hashes: nil,
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
    if len(bucket) == 0 {
        m.hashes = append(m.hashes, h)
    }
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
    hashes []uint64
    len int32
}

func hashmap_new__HashMap_10Ref_5Point_5int32() *hashmap_Ref_5Point_int32_x {
    return &hashmap_Ref_5Point_int32_x{
        buckets: make(map[uint64][]hashmap_Ref_5Point_int32_x_entry),
        len: 0,
        hashes: nil,
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
    if len(bucket) == 0 {
        m.hashes = append(m.hashes, h)
    }
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
    hashes []uint64
    len int32
}

func hashmap_new__HashMap_8Ref_3Key_5int32() *hashmap_Ref_3Key_int32_x {
    return &hashmap_Ref_3Key_int32_x{
        buckets: make(map[uint64][]hashmap_Ref_3Key_int32_x_entry),
        len: 0,
        hashes: nil,
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
    if len(bucket) == 0 {
        m.hashes = append(m.hashes, h)
    }
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
    var retv89 bool
    var jp93 bool
    if true {
        var t97 int32 = self__0.x
        var t98 int32 = other__1.x
        var t99 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(t97, t98)
        jp93 = t99
    } else {
        jp93 = false
    }
    var jp91 bool
    if jp93 {
        var t94 int32 = self__0.y
        var t95 int32 = other__1.y
        var t96 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(t94, t95)
        jp91 = t96
    } else {
        jp91 = false
    }
    retv89 = jp91
    return retv89
}

func _goml_m_trait__impl_i_Hash_i_Point_i_hash(self__2 Point) uint64 {
    var retv101 uint64
    var h__3 uint64 = 14695981039346656037
    var t102 uint64 = h__3 * 1099511628211
    var t103 int32 = self__2.x
    var t104 uint64 = _goml_m_trait__impl_i_Hash_i_int32_i_hash(t103)
    var h__4 uint64 = t102 + t104
    var t105 uint64 = h__4 * 1099511628211
    var t106 int32 = self__2.y
    var t107 uint64 = _goml_m_trait__impl_i_Hash_i_int32_i_hash(t106)
    var h__5 uint64 = t105 + t107
    retv101 = h__5
    return retv101
}

func _goml_m_trait__impl_i_Eq_i_Key_i_eq(self__6 Key, other__7 Key) bool {
    var retv109 bool
    var mtmp58 Tuple2_3Key_3Key = Tuple2_3Key_3Key{
        _0: self__6,
        _1: other__7,
    }
    var x59 Key = mtmp58._0
    var x60 Key = mtmp58._1
    var jp111 bool
    switch x60.(type) {
    case A:
        var jp113 bool
        switch x59.(type) {
        case A:
            jp113 = true
        case B:
            jp113 = false
        case P:
            jp113 = false
        default:
            panic("non-exhaustive match")
        }
        jp111 = jp113
    case B:
        var x61 int32 = x60.(B)._0
        var jp115 bool
        switch x59.(type) {
        case A:
            jp115 = false
        case B:
            var x65 int32 = x59.(B)._0
            var __l1_0__8 int32 = x65
            var __r1_0__9 int32 = x61
            var jp117 bool
            if true {
                var t118 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(__l1_0__8, __r1_0__9)
                jp117 = t118
            } else {
                jp117 = false
            }
            jp115 = jp117
        case P:
            jp115 = false
        default:
            panic("non-exhaustive match")
        }
        jp111 = jp115
    case P:
        var x62 Point = x60.(P)._0
        var jp120 bool
        switch x59.(type) {
        case A:
            jp120 = false
        case B:
            jp120 = false
        case P:
            var x68 Point = x59.(P)._0
            var __l2_0__10 Point = x68
            var __r2_0__11 Point = x62
            var jp122 bool
            if true {
                var t123 bool = _goml_m_trait__impl_i_Eq_i_Point_i_eq(__l2_0__10, __r2_0__11)
                jp122 = t123
            } else {
                jp122 = false
            }
            jp120 = jp122
        default:
            panic("non-exhaustive match")
        }
        jp111 = jp120
    default:
        panic("non-exhaustive match")
    }
    retv109 = jp111
    return retv109
}

func _goml_m_trait__impl_i_Hash_i_Key_i_hash(self__12 Key) uint64 {
    var retv125 uint64
    var jp127 uint64
    switch self__12.(type) {
    case A:
        var h__13 uint64 = 14695981039346656037 + 1
        jp127 = h__13
    case B:
        var x69 int32 = self__12.(B)._0
        var __field1_0__14 int32 = x69
        var h__15 uint64 = 14695981039346656037 + 2
        var t128 uint64 = h__15 * 1099511628211
        var t129 uint64 = _goml_m_trait__impl_i_Hash_i_int32_i_hash(__field1_0__14)
        var h__16 uint64 = t128 + t129
        jp127 = h__16
    case P:
        var x70 Point = self__12.(P)._0
        var __field2_0__17 Point = x70
        var h__18 uint64 = 14695981039346656037 + 3
        var t130 uint64 = h__18 * 1099511628211
        var t131 uint64 = _goml_m_trait__impl_i_Hash_i_Point_i_hash(__field2_0__17)
        var h__19 uint64 = t130 + t131
        jp127 = h__19
    default:
        panic("non-exhaustive match")
    }
    retv125 = jp127
    return retv125
}

func print_opt_int(x__20 Option__int32) struct{} {
    switch x__20.(type) {
    case None:
        println__T_string("none")
    case Some:
        var x71 int32 = x__20.(Some)._0
        var v__21 int32 = x71
        println__T_int32(v__21)
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func main0() struct{} {
    var m1__22 *hashmap_Key_int32_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__Key____V__int32()
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(m1__22, A{}, 10)
    var t137 Key = B{
        _0: 1,
    }
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(m1__22, t137, 20)
    var t138 int32 = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(m1__22)
    println__T_int32(t138)
    var t139 Option__int32 = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__Key____V__int32(m1__22, A{})
    print_opt_int(t139)
    var t140 Key = B{
        _0: 1,
    }
    var t141 bool = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(m1__22, t140)
    println__T_bool(t141)
    var t142 Key = B{
        _0: 1,
    }
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_remove____K__Key____V__int32(m1__22, t142)
    var t143 Key = B{
        _0: 1,
    }
    var t144 bool = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(m1__22, t143)
    println__T_bool(t144)
    var t145 int32 = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(m1__22)
    println__T_int32(t145)
    var m2__23 *hashmap_Ref_5Point_int32_x = _goml_m_inherent_i_HashMap_i_H_h8965aba642d8a393e2049e4587079c44__r_____V__int32()
    var t146 Point = Point{
        x: 1,
        y: 2,
    }
    var p1__24 *ref_Point_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Point(t146)
    var t147 Point = Point{
        x: 1,
        y: 2,
    }
    var p2__25 *ref_Point_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Point(t147)
    _goml_m_inherent_i_HashMap_i_H_h92a9b8ea001265f897bc67524ef74086__r_____V__int32(m2__23, p1__24, 99)
    var t148 Option__int32 = _goml_m_inherent_i_HashMap_i_H_h8816d503fd85179e4f674a4a9db321b9__r_____V__int32(m2__23, p1__24)
    print_opt_int(t148)
    var t149 Option__int32 = _goml_m_inherent_i_HashMap_i_H_h8816d503fd85179e4f674a4a9db321b9__r_____V__int32(m2__23, p2__25)
    print_opt_int(t149)
    var t150 Point = Point{
        x: 9,
        y: 8,
    }
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Point(p1__24, t150)
    var t151 Option__int32 = _goml_m_inherent_i_HashMap_i_H_h8816d503fd85179e4f674a4a9db321b9__r_____V__int32(m2__23, p1__24)
    print_opt_int(t151)
    var t152 bool = _goml_m_trait__impl_i_Eq_i_Ref_l_Point_r__i_eq(p1__24, p2__25)
    println__T_bool(t152)
    var m3__26 *hashmap_Ref_3Key_int32_x = _goml_m_inherent_i_HashMap_i_H_h35e4bb85f0aaff40bae5445e99f77c49__r_____V__int32()
    var t153 Key = B{
        _0: 7,
    }
    var k1__27 *ref_Key_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Key(t153)
    var k2__28 *ref_Key_x = k1__27
    _goml_m_inherent_i_HashMap_i_H_h8d7eb9d75c42de8655645502911541c0__r_____V__int32(m3__26, k1__27, 123)
    var t154 Option__int32 = _goml_m_inherent_i_HashMap_i_H_h488c149fa298920fe2fbb4be132c4bc3__r_____V__int32(m3__26, k2__28)
    print_opt_int(t154)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__58 int32, other__59 int32) bool {
    var retv156 bool
    var t157 bool = self__58 == other__59
    retv156 = t157
    return retv156
}

func _goml_m_trait__impl_i_Hash_i_int32_i_hash(self__80 int32) uint64 {
    var retv159 uint64
    var t160 uint64 = _goml_runtime_core_int32_hash(self__80)
    retv159 = t160
    return retv159
}

func println__T_string(value__1 string) struct{} {
    var t162 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t162)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t165 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t165)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__Key____V__int32() *hashmap_Key_int32_x {
    var retv168 *hashmap_Key_int32_x
    var t169 *hashmap_Key_int32_x = hashmap_new__HashMap_3Key_5int32()
    retv168 = t169
    return retv168
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(self__192 *hashmap_Key_int32_x, key__193 Key, value__194 int32) struct{} {
    hashmap_set__HashMap_3Key_5int32(self__192, key__193, value__194)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(self__197 *hashmap_Key_int32_x) int32 {
    var retv173 int32
    var t174 int32 = hashmap_len__HashMap_3Key_5int32(self__197)
    retv173 = t174
    return retv173
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__Key____V__int32(self__190 *hashmap_Key_int32_x, key__191 Key) Option__int32 {
    var retv176 Option__int32
    var t177 Option__int32 = hashmap_get__HashMap_3Key_5int32(self__190, key__191)
    retv176 = t177
    return retv176
}

func println__T_bool(value__1 bool) struct{} {
    var t179 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t179)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(self__198 *hashmap_Key_int32_x, key__199 Key) bool {
    var retv182 bool
    var t183 bool = hashmap_contains__HashMap_3Key_5int32(self__198, key__199)
    retv182 = t183
    return retv182
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_remove____K__Key____V__int32(self__195 *hashmap_Key_int32_x, key__196 Key) struct{} {
    hashmap_remove__HashMap_3Key_5int32(self__195, key__196)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h8965aba642d8a393e2049e4587079c44__r_____V__int32() *hashmap_Ref_5Point_int32_x {
    var retv187 *hashmap_Ref_5Point_int32_x
    var t188 *hashmap_Ref_5Point_int32_x = hashmap_new__HashMap_10Ref_5Point_5int32()
    retv187 = t188
    return retv187
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Point(value__201 Point) *ref_Point_x {
    var retv190 *ref_Point_x
    var t191 *ref_Point_x = ref__Ref_5Point(value__201)
    retv190 = t191
    return retv190
}

func _goml_m_inherent_i_HashMap_i_H_h92a9b8ea001265f897bc67524ef74086__r_____V__int32(self__192 *hashmap_Ref_5Point_int32_x, key__193 *ref_Point_x, value__194 int32) struct{} {
    hashmap_set__HashMap_10Ref_5Point_5int32(self__192, key__193, value__194)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h8816d503fd85179e4f674a4a9db321b9__r_____V__int32(self__190 *hashmap_Ref_5Point_int32_x, key__191 *ref_Point_x) Option__int32 {
    var retv195 Option__int32
    var t196 Option__int32 = hashmap_get__HashMap_10Ref_5Point_5int32(self__190, key__191)
    retv195 = t196
    return retv195
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Point(self__203 *ref_Point_x, value__204 Point) struct{} {
    ref_set__Ref_5Point(self__203, value__204)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_Ref_l_Point_r__i_eq(self__88 *ref_Point_x, other__89 *ref_Point_x) bool {
    var retv200 bool
    var t201 bool = ptr_eq__Ref_5Point(self__88, other__89)
    retv200 = t201
    return retv200
}

func _goml_m_inherent_i_HashMap_i_H_h35e4bb85f0aaff40bae5445e99f77c49__r_____V__int32() *hashmap_Ref_3Key_int32_x {
    var retv203 *hashmap_Ref_3Key_int32_x
    var t204 *hashmap_Ref_3Key_int32_x = hashmap_new__HashMap_8Ref_3Key_5int32()
    retv203 = t204
    return retv203
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Key(value__201 Key) *ref_Key_x {
    var retv206 *ref_Key_x
    var t207 *ref_Key_x = ref__Ref_3Key(value__201)
    retv206 = t207
    return retv206
}

func _goml_m_inherent_i_HashMap_i_H_h8d7eb9d75c42de8655645502911541c0__r_____V__int32(self__192 *hashmap_Ref_3Key_int32_x, key__193 *ref_Key_x, value__194 int32) struct{} {
    hashmap_set__HashMap_8Ref_3Key_5int32(self__192, key__193, value__194)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h488c149fa298920fe2fbb4be132c4bc3__r_____V__int32(self__190 *hashmap_Ref_3Key_int32_x, key__191 *ref_Key_x) Option__int32 {
    var retv211 Option__int32
    var t212 Option__int32 = hashmap_get__HashMap_8Ref_3Key_5int32(self__190, key__191)
    retv211 = t212
    return retv211
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv214 string
    retv214 = self__34
    return retv214
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__38 int32) string {
    var retv216 string
    var t217 string = _goml_runtime_core_int32_to_string(self__38)
    retv216 = t217
    return retv216
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__33 bool) string {
    var retv219 string
    var t220 string = _goml_runtime_core_bool_to_string(self__33)
    retv219 = t220
    return retv219
}

func _goml_m_trait__impl_i_Hash_i_Ref_l_Point_r__i_hash(self__90 *ref_Point_x) uint64 {
    var retv222 uint64
    var t223 uint64 = ptr_hash__Ref_5Point(self__90)
    retv222 = t223
    return retv222
}

func _goml_m_trait__impl_i_Eq_i_Ref_l_Key_r__i_eq(self__88 *ref_Key_x, other__89 *ref_Key_x) bool {
    var retv225 bool
    var t226 bool = ptr_eq__Ref_3Key(self__88, other__89)
    retv225 = t226
    return retv225
}

func _goml_m_trait__impl_i_Hash_i_Ref_l_Key_r__i_hash(self__90 *ref_Key_x) uint64 {
    var retv228 uint64
    var t229 uint64 = ptr_hash__Ref_3Key(self__90)
    retv228 = t229
    return retv228
}

func main() {
    main0()
}
