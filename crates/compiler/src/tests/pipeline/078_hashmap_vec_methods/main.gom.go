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
    var retv82 bool
    var mtmp58 Tuple2_3Key_3Key = Tuple2_3Key_3Key{
        _0: self__0,
        _1: other__1,
    }
    var x59 Key = mtmp58._0
    var x60 Key = mtmp58._1
    var jp84 bool
    switch x60.(type) {
    case A:
        var jp86 bool
        switch x59.(type) {
        case A:
            jp86 = true
        case B:
            jp86 = false
        default:
            panic("non-exhaustive match")
        }
        jp84 = jp86
    case B:
        var x61 int32 = x60.(B)._0
        var jp88 bool
        switch x59.(type) {
        case A:
            jp88 = false
        case B:
            var x63 int32 = x59.(B)._0
            var __l1_0__2 int32 = x63
            var __r1_0__3 int32 = x61
            var jp90 bool
            if true {
                var t91 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(__l1_0__2, __r1_0__3)
                jp90 = t91
            } else {
                jp90 = false
            }
            jp88 = jp90
        default:
            panic("non-exhaustive match")
        }
        jp84 = jp88
    default:
        panic("non-exhaustive match")
    }
    retv82 = jp84
    return retv82
}

func _goml_m_trait__impl_i_Hash_i_Key_i_hash(self__4 Key) uint64 {
    var retv93 uint64
    var jp95 uint64
    switch self__4.(type) {
    case A:
        var h__5 uint64 = 14695981039346656037 + 1
        jp95 = h__5
    case B:
        var x64 int32 = self__4.(B)._0
        var __field1_0__6 int32 = x64
        var h__7 uint64 = 14695981039346656037 + 2
        var t96 uint64 = h__7 * 1099511628211
        var t97 uint64 = _goml_m_trait__impl_i_Hash_i_int32_i_hash(__field1_0__6)
        var h__8 uint64 = t96 + t97
        jp95 = h__8
    default:
        panic("non-exhaustive match")
    }
    retv93 = jp95
    return retv93
}

func print_opt_int(x__9 Option__int32) struct{} {
    switch x__9.(type) {
    case None:
        println__T_string("none")
    case Some:
        var x65 int32 = x__9.(Some)._0
        var v__10 int32 = x65
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
    var t103 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(v__11, 0)
    println__T_int32(t103)
    var t104 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(v__11, 1)
    println__T_int32(t104)
    var t105 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(v__11, 2)
    println__T_int32(t105)
    var t106 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(v__11)
    println__T_int32(t106)
    var m__12 *hashmap_Key_int32_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__Key____V__int32()
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(m__12, A{}, 10)
    var t107 Key = B{
        _0: 1,
    }
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(m__12, t107, 20)
    var t108 int32 = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(m__12)
    println__T_int32(t108)
    var t109 Option__int32 = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__Key____V__int32(m__12, A{})
    print_opt_int(t109)
    var t110 Key = B{
        _0: 1,
    }
    var t111 bool = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(m__12, t110)
    println__T_bool(t111)
    var t112 Key = B{
        _0: 1,
    }
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_remove____K__Key____V__int32(m__12, t112)
    var t113 Key = B{
        _0: 1,
    }
    var t114 bool = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(m__12, t113)
    println__T_bool(t114)
    var t115 int32 = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(m__12)
    println__T_int32(t115)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__58 int32, other__59 int32) bool {
    var retv117 bool
    var t118 bool = self__58 == other__59
    retv117 = t118
    return retv117
}

func _goml_m_trait__impl_i_Hash_i_int32_i_hash(self__80 int32) uint64 {
    var retv120 uint64
    var t121 uint64 = _goml_runtime_core_int32_hash(self__80)
    retv120 = t121
    return retv120
}

func println__T_string(value__1 string) struct{} {
    var t123 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t123)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t126 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t126)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv129 *_goml_vec_int32
    var t130 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv129 = t130
    return retv129
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__120 *_goml_vec_int32, elem__121 int32) struct{} {
    vec_push__Vec_5int32(self__120, elem__121)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__126 *_goml_vec_int32, index__127 int32) int32 {
    var retv134 int32
    var t135 int32 = vec_get__Vec_5int32(self__126, index__127)
    retv134 = t135
    return retv134
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__131 *_goml_vec_int32) int32 {
    var retv137 int32
    var t138 int32 = vec_len__Vec_5int32(self__131)
    retv137 = t138
    return retv137
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__Key____V__int32() *hashmap_Key_int32_x {
    var retv140 *hashmap_Key_int32_x
    var t141 *hashmap_Key_int32_x = hashmap_new__HashMap_3Key_5int32()
    retv140 = t141
    return retv140
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(self__191 *hashmap_Key_int32_x, key__192 Key, value__193 int32) struct{} {
    hashmap_set__HashMap_3Key_5int32(self__191, key__192, value__193)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(self__196 *hashmap_Key_int32_x) int32 {
    var retv145 int32
    var t146 int32 = hashmap_len__HashMap_3Key_5int32(self__196)
    retv145 = t146
    return retv145
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__Key____V__int32(self__189 *hashmap_Key_int32_x, key__190 Key) Option__int32 {
    var retv148 Option__int32
    var t149 Option__int32 = hashmap_get__HashMap_3Key_5int32(self__189, key__190)
    retv148 = t149
    return retv148
}

func println__T_bool(value__1 bool) struct{} {
    var t151 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t151)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(self__197 *hashmap_Key_int32_x, key__198 Key) bool {
    var retv154 bool
    var t155 bool = hashmap_contains__HashMap_3Key_5int32(self__197, key__198)
    retv154 = t155
    return retv154
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_remove____K__Key____V__int32(self__194 *hashmap_Key_int32_x, key__195 Key) struct{} {
    hashmap_remove__HashMap_3Key_5int32(self__194, key__195)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv159 string
    retv159 = self__34
    return retv159
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__38 int32) string {
    var retv161 string
    var t162 string = _goml_runtime_core_int32_to_string(self__38)
    retv161 = t162
    return retv161
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__33 bool) string {
    var retv164 string
    var t165 string = _goml_runtime_core_bool_to_string(self__33)
    retv164 = t165
    return retv164
}

func main() {
    main0()
}
