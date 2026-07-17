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

type _goml_vec_int32 struct {
    items []int32
}

func vec_new__Vec_5int32() *_goml_vec_int32 {
    return &_goml_vec_int32{
        items: nil,
    }
}

func vec_push__Vec_5int32(vec *_goml_vec_int32, elem int32) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_5int32(vec *_goml_vec_int32, index int32) int32 {
    return vec.items[index]
}

func vec_len__Vec_5int32(vec *_goml_vec_int32) int32 {
    return int32(len(vec.items))
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

type Tuple2_3Key_3Key struct {
    _0 Key
    _1 Key
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

type Option__int32 interface {
    isOption__int32()
}

type None struct {}

func (_ None) isOption__int32() {}

type Some struct {
    _0 int32
}

func (_ Some) isOption__int32() {}

func _goml_m_trait__impl_i_Eq_i_Key_i_eq(self__0 Key, other__1 Key) bool {
    var retv85 bool
    var mtmp61 Tuple2_3Key_3Key = Tuple2_3Key_3Key{
        _0: self__0,
        _1: other__1,
    }
    var x62 Key = mtmp61._0
    var x63 Key = mtmp61._1
    var jp87 bool
    switch x63.(type) {
    case A:
        var jp89 bool
        switch x62.(type) {
        case A:
            jp89 = true
        case B:
            jp89 = false
        default:
            panic("non-exhaustive match")
        }
        jp87 = jp89
    case B:
        var x64 int32 = x63.(B)._0
        var jp91 bool
        switch x62.(type) {
        case A:
            jp91 = false
        case B:
            var x66 int32 = x62.(B)._0
            var __l1_0__2 int32 = x66
            var __r1_0__3 int32 = x64
            var jp93 bool
            if true {
                var t94 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(__l1_0__2, __r1_0__3)
                jp93 = t94
            } else {
                jp93 = false
            }
            jp91 = jp93
        default:
            panic("non-exhaustive match")
        }
        jp87 = jp91
    default:
        panic("non-exhaustive match")
    }
    retv85 = jp87
    return retv85
}

func _goml_m_trait__impl_i_Hash_i_Key_i_hash(self__4 Key) uint64 {
    var retv96 uint64
    var jp98 uint64
    switch self__4.(type) {
    case A:
        var h__5 uint64 = 14695981039346656037 + 1
        jp98 = h__5
    case B:
        var x67 int32 = self__4.(B)._0
        var __field1_0__6 int32 = x67
        var h__7 uint64 = 14695981039346656037 + 2
        var t99 uint64 = h__7 * 1099511628211
        var t100 uint64 = _goml_m_trait__impl_i_Hash_i_int32_i_hash(__field1_0__6)
        var h__8 uint64 = t99 + t100
        jp98 = h__8
    default:
        panic("non-exhaustive match")
    }
    retv96 = jp98
    return retv96
}

func print_opt_int(x__9 Option__int32) struct{} {
    switch x__9.(type) {
    case None:
        println__T_string("none")
    case Some:
        var x68 int32 = x__9.(Some)._0
        var v__10 int32 = x68
        println__T_int32(v__10)
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func main0() struct{} {
    var v__11 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(v__11, 10)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(v__11, 20)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(v__11, 30)
    var t106 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(v__11, 0)
    println__T_int32(t106)
    var t107 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(v__11, 1)
    println__T_int32(t107)
    var t108 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(v__11, 2)
    println__T_int32(t108)
    var t109 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(v__11)
    println__T_int32(t109)
    var m__12 *hashmap_Key_int32_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__Key____V__int32()
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(m__12, A{}, 10)
    var t110 Key = B{
        _0: 1,
    }
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(m__12, t110, 20)
    var t111 int32 = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(m__12)
    println__T_int32(t111)
    var t112 Option__int32 = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__Key____V__int32(m__12, A{})
    print_opt_int(t112)
    var t113 Key = B{
        _0: 1,
    }
    var t114 bool = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(m__12, t113)
    println__T_bool(t114)
    var t115 Key = B{
        _0: 1,
    }
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_remove____K__Key____V__int32(m__12, t115)
    var t116 Key = B{
        _0: 1,
    }
    var t117 bool = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(m__12, t116)
    println__T_bool(t117)
    var t118 int32 = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(m__12)
    println__T_int32(t118)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__61 int32, other__62 int32) bool {
    var retv120 bool
    var t121 bool = self__61 == other__62
    retv120 = t121
    return retv120
}

func _goml_m_trait__impl_i_Hash_i_int32_i_hash(self__83 int32) uint64 {
    var retv123 uint64
    var t124 uint64 = _goml_runtime_core_int32_hash(self__83)
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

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv132 *_goml_vec_int32
    var t133 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv132 = t133
    return retv132
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__123 *_goml_vec_int32, elem__124 int32) struct{} {
    vec_push__Vec_5int32(self__123, elem__124)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__129 *_goml_vec_int32, index__130 int32) int32 {
    var retv137 int32
    var t138 int32 = vec_get__Vec_5int32(self__129, index__130)
    retv137 = t138
    return retv137
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__134 *_goml_vec_int32) int32 {
    var retv140 int32
    var t141 int32 = vec_len__Vec_5int32(self__134)
    retv140 = t141
    return retv140
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__Key____V__int32() *hashmap_Key_int32_x {
    var retv143 *hashmap_Key_int32_x
    var t144 *hashmap_Key_int32_x = hashmap_new__HashMap_3Key_5int32()
    retv143 = t144
    return retv143
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(self__195 *hashmap_Key_int32_x, key__196 Key, value__197 int32) struct{} {
    hashmap_set__HashMap_3Key_5int32(self__195, key__196, value__197)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(self__200 *hashmap_Key_int32_x) int32 {
    var retv148 int32
    var t149 int32 = hashmap_len__HashMap_3Key_5int32(self__200)
    retv148 = t149
    return retv148
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__Key____V__int32(self__193 *hashmap_Key_int32_x, key__194 Key) Option__int32 {
    var retv151 Option__int32
    var t152 Option__int32 = hashmap_get__HashMap_3Key_5int32(self__193, key__194)
    retv151 = t152
    return retv151
}

func println__T_bool(value__1 bool) struct{} {
    var t154 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t154)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(self__201 *hashmap_Key_int32_x, key__202 Key) bool {
    var retv157 bool
    var t158 bool = hashmap_contains__HashMap_3Key_5int32(self__201, key__202)
    retv157 = t158
    return retv157
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_remove____K__Key____V__int32(self__198 *hashmap_Key_int32_x, key__199 Key) struct{} {
    hashmap_remove__HashMap_3Key_5int32(self__198, key__199)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv162 string
    retv162 = self__37
    return retv162
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__41 int32) string {
    var retv164 string
    var t165 string = _goml_runtime_core_int32_to_string(self__41)
    retv164 = t165
    return retv164
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__36 bool) string {
    var retv167 string
    var t168 string = _goml_runtime_core_bool_to_string(self__36)
    retv167 = t168
    return retv167
}

func main() {
    main0()
}
