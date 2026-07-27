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

type _goml_vec_int struct {
    items []int
}

func vec_new__Vec_3int() *_goml_vec_int {
    return &_goml_vec_int{
        items: nil,
    }
}

func vec_push__Vec_3int(vec *_goml_vec_int, elem int) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_3int(vec *_goml_vec_int, index int) int {
    return vec.items[index]
}

func vec_len__Vec_3int(vec *_goml_vec_int) int {
    return int(len(vec.items))
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
    var retv88 bool
    var mtmp64 Tuple2_3Key_3Key = Tuple2_3Key_3Key{
        _0: self__0,
        _1: other__1,
    }
    var x65 Key = mtmp64._0
    var x66 Key = mtmp64._1
    var jp90 bool
    switch x66.(type) {
    case A:
        var jp92 bool
        switch x65.(type) {
        case A:
            jp92 = true
        default:
            jp92 = false
        }
        jp90 = jp92
    case B:
        var x67 int32 = x66.(B)._0
        var jp94 bool
        switch x65.(type) {
        case B:
            var x69 int32 = x65.(B)._0
            var __l1_0__2 int32 = x69
            var __r1_0__3 int32 = x67
            var jp96 bool
            if true {
                var t97 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(__l1_0__2, __r1_0__3)
                jp96 = t97
            } else {
                jp96 = false
            }
            jp94 = jp96
        default:
            jp94 = false
        }
        jp90 = jp94
    default:
        panic("non-exhaustive match")
    }
    retv88 = jp90
    return retv88
}

func _goml_m_trait__impl_i_Hash_i_Key_i_hash(self__4 Key) uint64 {
    var retv99 uint64
    var jp101 uint64
    switch self__4.(type) {
    case A:
        var h__5 uint64 = 14695981039346656037 + 1
        jp101 = h__5
    case B:
        var x70 int32 = self__4.(B)._0
        var __field1_0__6 int32 = x70
        var h__7 uint64 = 14695981039346656037 + 2
        var t102 uint64 = h__7 * 1099511628211
        var t103 uint64 = _goml_m_trait__impl_i_Hash_i_int32_i_hash(__field1_0__6)
        var h__8 uint64 = t102 + t103
        jp101 = h__8
    default:
        panic("non-exhaustive match")
    }
    retv99 = jp101
    return retv99
}

func print_opt_int(x__9 Option__int32) struct{} {
    switch x__9.(type) {
    case None:
        println__T_string("none")
    case Some:
        var x71 int32 = x__9.(Some)._0
        var v__10 int32 = x71
        println__T_int32(v__10)
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func main0() struct{} {
    var v__11 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(v__11, 10)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(v__11, 20)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(v__11, 30)
    var t109 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(v__11, 0)
    println__T_int(t109)
    var t110 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(v__11, 1)
    println__T_int(t110)
    var t111 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(v__11, 2)
    println__T_int(t111)
    var t112 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int(v__11)
    println__T_int(t112)
    var m__12 *hashmap_Key_int32_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__Key____V__int32()
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(m__12, A{}, 10)
    var t113 Key = B{
        _0: 1,
    }
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(m__12, t113, 20)
    var t114 int = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(m__12)
    println__T_int(t114)
    var t115 Option__int32 = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__Key____V__int32(m__12, A{})
    print_opt_int(t115)
    var t116 Key = B{
        _0: 1,
    }
    var t117 bool = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(m__12, t116)
    println__T_bool(t117)
    var t118 Key = B{
        _0: 1,
    }
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_remove____K__Key____V__int32(m__12, t118)
    var t119 Key = B{
        _0: 1,
    }
    var t120 bool = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(m__12, t119)
    println__T_bool(t120)
    var t121 int = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(m__12)
    println__T_int(t121)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var retv123 bool
    var t124 bool = self__65 == other__66
    retv123 = t124
    return retv123
}

func _goml_m_trait__impl_i_Hash_i_int32_i_hash(self__88 int32) uint64 {
    var retv126 uint64
    var t127 uint64 = _goml_runtime_core_int32_hash(self__88)
    retv126 = t127
    return retv126
}

func println__T_string(value__1 string) struct{} {
    var t129 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t129)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t132 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t132)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int() *_goml_vec_int {
    var retv135 *_goml_vec_int
    var t136 *_goml_vec_int = vec_new__Vec_3int()
    retv135 = t136
    return retv135
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(self__128 *_goml_vec_int, elem__129 int) struct{} {
    vec_push__Vec_3int(self__128, elem__129)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t140 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t140)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(self__134 *_goml_vec_int, index__135 int) int {
    var retv143 int
    var t144 int = vec_get__Vec_3int(self__134, index__135)
    retv143 = t144
    return retv143
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int(self__139 *_goml_vec_int) int {
    var retv146 int
    var t147 int = vec_len__Vec_3int(self__139)
    retv146 = t147
    return retv146
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__Key____V__int32() *hashmap_Key_int32_x {
    var retv149 *hashmap_Key_int32_x
    var t150 *hashmap_Key_int32_x = hashmap_new__HashMap_3Key_5int32()
    retv149 = t150
    return retv149
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(self__200 *hashmap_Key_int32_x, key__201 Key, value__202 int32) struct{} {
    hashmap_set__HashMap_3Key_5int32(self__200, key__201, value__202)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(self__205 *hashmap_Key_int32_x) int {
    var retv154 int
    var t155 int = hashmap_len__HashMap_3Key_5int32(self__205)
    retv154 = t155
    return retv154
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__Key____V__int32(self__198 *hashmap_Key_int32_x, key__199 Key) Option__int32 {
    var retv157 Option__int32
    var t158 Option__int32 = hashmap_get__HashMap_3Key_5int32(self__198, key__199)
    retv157 = t158
    return retv157
}

func println__T_bool(value__1 bool) struct{} {
    var t160 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t160)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(self__206 *hashmap_Key_int32_x, key__207 Key) bool {
    var retv163 bool
    var t164 bool = hashmap_contains__HashMap_3Key_5int32(self__206, key__207)
    retv163 = t164
    return retv163
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_remove____K__Key____V__int32(self__203 *hashmap_Key_int32_x, key__204 Key) struct{} {
    hashmap_remove__HashMap_3Key_5int32(self__203, key__204)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv168 string
    retv168 = self__38
    return retv168
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv170 string
    var t171 string = _goml_runtime_core_int32_to_string(self__43)
    retv170 = t171
    return retv170
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv173 string
    var t174 string = _goml_runtime_core_int_to_string(self__40)
    retv173 = t174
    return retv173
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv176 string
    var t177 string = _goml_runtime_core_bool_to_string(self__37)
    retv176 = t177
    return retv176
}

func main() {
    main0()
}
