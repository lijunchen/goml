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

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
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
    len int
}

func hashmap_new__HashMap_3Key_5int32() *hashmap_Key_int32_x {
    return &hashmap_Key_int32_x{
        buckets: make(map[uint64][]hashmap_Key_int32_x_entry),
        len: 0,
        hashes: nil,
    }
}

func hashmap_len__HashMap_3Key_5int32(m *hashmap_Key_int32_x) int {
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
    var i int = 0
    for {
        if i >= int(len(bucket)) {
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
    var reuse_index int = -1
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_Key_i_hash(key)
    var bucket []hashmap_Key_int32_x_entry = m.buckets[h]
    if len(bucket) == 0 {
        m.hashes = append(m.hashes, h)
    }
    var i int = 0
    for {
        if i >= int(len(bucket)) {
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
    var i int = 0
    for {
        if i >= int(len(bucket)) {
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
    var i int = 0
    for {
        if i >= int(len(bucket)) {
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
    len int
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
    var i int = 0
    for {
        if i >= int(len(bucket)) {
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
    var reuse_index int = -1
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_Ref_l_Point_r__i_hash(key)
    var bucket []hashmap_Ref_5Point_int32_x_entry = m.buckets[h]
    if len(bucket) == 0 {
        m.hashes = append(m.hashes, h)
    }
    var i int = 0
    for {
        if i >= int(len(bucket)) {
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
    len int
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
    var i int = 0
    for {
        if i >= int(len(bucket)) {
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
    var reuse_index int = -1
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_Ref_l_Key_r__i_hash(key)
    var bucket []hashmap_Ref_3Key_int32_x_entry = m.buckets[h]
    if len(bucket) == 0 {
        m.hashes = append(m.hashes, h)
    }
    var i int = 0
    for {
        if i >= int(len(bucket)) {
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
    var retv139 bool
    var jp143 bool
    if true {
        var t147 int32 = self__0.x
        var t148 int32 = other__1.x
        var t149 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(t147, t148)
        jp143 = t149
    } else {
        jp143 = false
    }
    var jp141 bool
    if jp143 {
        var t144 int32 = self__0.y
        var t145 int32 = other__1.y
        var t146 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(t144, t145)
        jp141 = t146
    } else {
        jp141 = false
    }
    retv139 = jp141
    return retv139
}

func _goml_m_trait__impl_i_Hash_i_Point_i_hash(self__2 Point) uint64 {
    var retv151 uint64
    var h__3 uint64 = 14695981039346656037
    var t152 uint64 = h__3 * 1099511628211
    var t153 int32 = self__2.x
    var t154 uint64 = _goml_m_trait__impl_i_Hash_i_int32_i_hash(t153)
    var h__4 uint64 = t152 + t154
    var t155 uint64 = h__4 * 1099511628211
    var t156 int32 = self__2.y
    var t157 uint64 = _goml_m_trait__impl_i_Hash_i_int32_i_hash(t156)
    var h__5 uint64 = t155 + t157
    retv151 = h__5
    return retv151
}

func _goml_m_trait__impl_i_Eq_i_Key_i_eq(self__6 Key, other__7 Key) bool {
    var retv159 bool
    var mtmp108 Tuple2_3Key_3Key = Tuple2_3Key_3Key{
        _0: self__6,
        _1: other__7,
    }
    var x109 Key = mtmp108._0
    var x110 Key = mtmp108._1
    var jp161 bool
    switch x110.(type) {
    case A:
        var jp163 bool
        switch x109.(type) {
        case A:
            jp163 = true
        default:
            jp163 = false
        }
        jp161 = jp163
    case B:
        var x111 int32 = x110.(B)._0
        var jp165 bool
        switch x109.(type) {
        case B:
            var x115 int32 = x109.(B)._0
            var __l1_0__8 int32 = x115
            var __r1_0__9 int32 = x111
            var jp167 bool
            if true {
                var t168 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(__l1_0__8, __r1_0__9)
                jp167 = t168
            } else {
                jp167 = false
            }
            jp165 = jp167
        default:
            jp165 = false
        }
        jp161 = jp165
    case P:
        var x112 Point = x110.(P)._0
        var jp170 bool
        switch x109.(type) {
        case P:
            var x118 Point = x109.(P)._0
            var __l2_0__10 Point = x118
            var __r2_0__11 Point = x112
            var jp172 bool
            if true {
                var t173 bool = _goml_m_trait__impl_i_Eq_i_Point_i_eq(__l2_0__10, __r2_0__11)
                jp172 = t173
            } else {
                jp172 = false
            }
            jp170 = jp172
        default:
            jp170 = false
        }
        jp161 = jp170
    default:
        panic("non-exhaustive match")
    }
    retv159 = jp161
    return retv159
}

func _goml_m_trait__impl_i_Hash_i_Key_i_hash(self__12 Key) uint64 {
    var retv175 uint64
    var jp177 uint64
    switch self__12.(type) {
    case A:
        var h__13 uint64 = 14695981039346656037 + 1
        jp177 = h__13
    case B:
        var x119 int32 = self__12.(B)._0
        var __field1_0__14 int32 = x119
        var h__15 uint64 = 14695981039346656037 + 2
        var t178 uint64 = h__15 * 1099511628211
        var t179 uint64 = _goml_m_trait__impl_i_Hash_i_int32_i_hash(__field1_0__14)
        var h__16 uint64 = t178 + t179
        jp177 = h__16
    case P:
        var x120 Point = self__12.(P)._0
        var __field2_0__17 Point = x120
        var h__18 uint64 = 14695981039346656037 + 3
        var t180 uint64 = h__18 * 1099511628211
        var t181 uint64 = _goml_m_trait__impl_i_Hash_i_Point_i_hash(__field2_0__17)
        var h__19 uint64 = t180 + t181
        jp177 = h__19
    default:
        panic("non-exhaustive match")
    }
    retv175 = jp177
    return retv175
}

func print_opt_int(x__20 Option__int32) struct{} {
    switch x__20.(type) {
    case None:
        println__T_string("none")
    case Some:
        var x121 int32 = x__20.(Some)._0
        var v__21 int32 = x121
        println__T_int32(v__21)
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func main0() struct{} {
    var m1__22 *hashmap_Key_int32_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__Key____V__int32()
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(m1__22, A{}, 10)
    var t187 Key = B{
        _0: 1,
    }
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(m1__22, t187, 20)
    var t188 int = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(m1__22)
    println__T_int(t188)
    var t189 Option__int32 = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__Key____V__int32(m1__22, A{})
    print_opt_int(t189)
    var t190 Key = B{
        _0: 1,
    }
    var t191 bool = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(m1__22, t190)
    println__T_bool(t191)
    var t192 Key = B{
        _0: 1,
    }
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_remove____K__Key____V__int32(m1__22, t192)
    var t193 Key = B{
        _0: 1,
    }
    var t194 bool = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(m1__22, t193)
    println__T_bool(t194)
    var t195 int = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(m1__22)
    println__T_int(t195)
    var m2__23 *hashmap_Ref_5Point_int32_x = _goml_m_inherent_i_HashMap_i_H_h8965aba642d8a393e2049e4587079c44__r_____V__int32()
    var t196 Point = Point{
        x: 1,
        y: 2,
    }
    var p1__24 *ref_Point_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Point(t196)
    var t197 Point = Point{
        x: 1,
        y: 2,
    }
    var p2__25 *ref_Point_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Point(t197)
    _goml_m_inherent_i_HashMap_i_H_h92a9b8ea001265f897bc67524ef74086__r_____V__int32(m2__23, p1__24, 99)
    var t198 Option__int32 = _goml_m_inherent_i_HashMap_i_H_h8816d503fd85179e4f674a4a9db321b9__r_____V__int32(m2__23, p1__24)
    print_opt_int(t198)
    var t199 Option__int32 = _goml_m_inherent_i_HashMap_i_H_h8816d503fd85179e4f674a4a9db321b9__r_____V__int32(m2__23, p2__25)
    print_opt_int(t199)
    var t200 Point = Point{
        x: 9,
        y: 8,
    }
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Point(p1__24, t200)
    var t201 Option__int32 = _goml_m_inherent_i_HashMap_i_H_h8816d503fd85179e4f674a4a9db321b9__r_____V__int32(m2__23, p1__24)
    print_opt_int(t201)
    var t202 bool = _goml_m_trait__impl_i_Eq_i_Ref_l_Point_r__i_eq(p1__24, p2__25)
    println__T_bool(t202)
    var m3__26 *hashmap_Ref_3Key_int32_x = _goml_m_inherent_i_HashMap_i_H_h35e4bb85f0aaff40bae5445e99f77c49__r_____V__int32()
    var t203 Key = B{
        _0: 7,
    }
    var k1__27 *ref_Key_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Key(t203)
    var k2__28 *ref_Key_x = k1__27
    _goml_m_inherent_i_HashMap_i_H_h8d7eb9d75c42de8655645502911541c0__r_____V__int32(m3__26, k1__27, 123)
    var t204 Option__int32 = _goml_m_inherent_i_HashMap_i_H_h488c149fa298920fe2fbb4be132c4bc3__r_____V__int32(m3__26, k2__28)
    print_opt_int(t204)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var retv206 bool
    var t207 bool = self__65 == other__66
    retv206 = t207
    return retv206
}

func _goml_m_trait__impl_i_Hash_i_int32_i_hash(self__88 int32) uint64 {
    var retv209 uint64
    var t210 uint64 = _goml_runtime_core_int32_hash(self__88)
    retv209 = t210
    return retv209
}

func println__T_string(value__1 string) struct{} {
    var t212 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t212)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t215 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t215)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__Key____V__int32() *hashmap_Key_int32_x {
    var retv218 *hashmap_Key_int32_x
    var t219 *hashmap_Key_int32_x = hashmap_new__HashMap_3Key_5int32()
    retv218 = t219
    return retv218
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(self__198 *hashmap_Key_int32_x, key__199 Key, value__200 int32) struct{} {
    hashmap_set__HashMap_3Key_5int32(self__198, key__199, value__200)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t223 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t223)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(self__203 *hashmap_Key_int32_x) int {
    var retv226 int
    var t227 int = hashmap_len__HashMap_3Key_5int32(self__203)
    retv226 = t227
    return retv226
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__Key____V__int32(self__196 *hashmap_Key_int32_x, key__197 Key) Option__int32 {
    var retv229 Option__int32
    var t230 Option__int32 = hashmap_get__HashMap_3Key_5int32(self__196, key__197)
    retv229 = t230
    return retv229
}

func println__T_bool(value__1 bool) struct{} {
    var t232 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t232)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(self__204 *hashmap_Key_int32_x, key__205 Key) bool {
    var retv235 bool
    var t236 bool = hashmap_contains__HashMap_3Key_5int32(self__204, key__205)
    retv235 = t236
    return retv235
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_remove____K__Key____V__int32(self__201 *hashmap_Key_int32_x, key__202 Key) struct{} {
    hashmap_remove__HashMap_3Key_5int32(self__201, key__202)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h8965aba642d8a393e2049e4587079c44__r_____V__int32() *hashmap_Ref_5Point_int32_x {
    var retv240 *hashmap_Ref_5Point_int32_x
    var t241 *hashmap_Ref_5Point_int32_x = hashmap_new__HashMap_10Ref_5Point_5int32()
    retv240 = t241
    return retv240
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Point(value__207 Point) *ref_Point_x {
    var retv243 *ref_Point_x
    var t244 *ref_Point_x = ref__Ref_5Point(value__207)
    retv243 = t244
    return retv243
}

func _goml_m_inherent_i_HashMap_i_H_h92a9b8ea001265f897bc67524ef74086__r_____V__int32(self__198 *hashmap_Ref_5Point_int32_x, key__199 *ref_Point_x, value__200 int32) struct{} {
    hashmap_set__HashMap_10Ref_5Point_5int32(self__198, key__199, value__200)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h8816d503fd85179e4f674a4a9db321b9__r_____V__int32(self__196 *hashmap_Ref_5Point_int32_x, key__197 *ref_Point_x) Option__int32 {
    var retv248 Option__int32
    var t249 Option__int32 = hashmap_get__HashMap_10Ref_5Point_5int32(self__196, key__197)
    retv248 = t249
    return retv248
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Point(self__209 *ref_Point_x, value__210 Point) struct{} {
    ref_set__Ref_5Point(self__209, value__210)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_Ref_l_Point_r__i_eq(self__96 *ref_Point_x, other__97 *ref_Point_x) bool {
    var retv253 bool
    var t254 bool = ptr_eq__Ref_5Point(self__96, other__97)
    retv253 = t254
    return retv253
}

func _goml_m_inherent_i_HashMap_i_H_h35e4bb85f0aaff40bae5445e99f77c49__r_____V__int32() *hashmap_Ref_3Key_int32_x {
    var retv256 *hashmap_Ref_3Key_int32_x
    var t257 *hashmap_Ref_3Key_int32_x = hashmap_new__HashMap_8Ref_3Key_5int32()
    retv256 = t257
    return retv256
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Key(value__207 Key) *ref_Key_x {
    var retv259 *ref_Key_x
    var t260 *ref_Key_x = ref__Ref_3Key(value__207)
    retv259 = t260
    return retv259
}

func _goml_m_inherent_i_HashMap_i_H_h8d7eb9d75c42de8655645502911541c0__r_____V__int32(self__198 *hashmap_Ref_3Key_int32_x, key__199 *ref_Key_x, value__200 int32) struct{} {
    hashmap_set__HashMap_8Ref_3Key_5int32(self__198, key__199, value__200)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h488c149fa298920fe2fbb4be132c4bc3__r_____V__int32(self__196 *hashmap_Ref_3Key_int32_x, key__197 *ref_Key_x) Option__int32 {
    var retv264 Option__int32
    var t265 Option__int32 = hashmap_get__HashMap_8Ref_3Key_5int32(self__196, key__197)
    retv264 = t265
    return retv264
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv267 string
    retv267 = self__38
    return retv267
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv269 string
    var t270 string = _goml_runtime_core_int32_to_string(self__43)
    retv269 = t270
    return retv269
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv272 string
    var t273 string = _goml_runtime_core_int_to_string(self__40)
    retv272 = t273
    return retv272
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv275 string
    var t276 string = _goml_runtime_core_bool_to_string(self__37)
    retv275 = t276
    return retv275
}

func _goml_m_trait__impl_i_Hash_i_Ref_l_Point_r__i_hash(self__98 *ref_Point_x) uint64 {
    var retv278 uint64
    var t279 uint64 = ptr_hash__Ref_5Point(self__98)
    retv278 = t279
    return retv278
}

func _goml_m_trait__impl_i_Eq_i_Ref_l_Key_r__i_eq(self__96 *ref_Key_x, other__97 *ref_Key_x) bool {
    var retv281 bool
    var t282 bool = ptr_eq__Ref_3Key(self__96, other__97)
    retv281 = t282
    return retv281
}

func _goml_m_trait__impl_i_Hash_i_Ref_l_Key_r__i_hash(self__98 *ref_Key_x) uint64 {
    var retv284 uint64
    var t285 uint64 = ptr_hash__Ref_3Key(self__98)
    retv284 = t285
    return retv284
}

func main() {
    main0()
}
