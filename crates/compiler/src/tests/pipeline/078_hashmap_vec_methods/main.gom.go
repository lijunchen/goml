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
    var retv46 bool
    var mtmp22 Tuple2_3Key_3Key = Tuple2_3Key_3Key{
        _0: self__0,
        _1: other__1,
    }
    var x23 Key = mtmp22._0
    var x24 Key = mtmp22._1
    var jp48 bool
    switch x24.(type) {
    case A:
        var jp50 bool
        switch x23.(type) {
        case A:
            jp50 = true
        case B:
            jp50 = false
        default:
            panic("non-exhaustive match")
        }
        jp48 = jp50
    case B:
        var x25 int32 = x24.(B)._0
        var jp52 bool
        switch x23.(type) {
        case A:
            jp52 = false
        case B:
            var x27 int32 = x23.(B)._0
            var __l1_0__2 int32 = x27
            var __r1_0__3 int32 = x25
            var jp54 bool
            if true {
                var t55 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(__l1_0__2, __r1_0__3)
                jp54 = t55
            } else {
                jp54 = false
            }
            jp52 = jp54
        default:
            panic("non-exhaustive match")
        }
        jp48 = jp52
    default:
        panic("non-exhaustive match")
    }
    retv46 = jp48
    return retv46
}

func _goml_m_trait__impl_i_Hash_i_Key_i_hash(self__4 Key) uint64 {
    var retv57 uint64
    var jp59 uint64
    switch self__4.(type) {
    case A:
        var h__5 uint64 = 14695981039346656037 + 1
        jp59 = h__5
    case B:
        var x28 int32 = self__4.(B)._0
        var __field1_0__6 int32 = x28
        var h__7 uint64 = 14695981039346656037 + 2
        var t60 uint64 = h__7 * 1099511628211
        var t61 uint64 = _goml_m_trait__impl_i_Hash_i_int32_i_hash(__field1_0__6)
        var h__8 uint64 = t60 + t61
        jp59 = h__8
    default:
        panic("non-exhaustive match")
    }
    retv57 = jp59
    return retv57
}

func print_opt_int(x__9 Option__int32) struct{} {
    switch x__9.(type) {
    case None:
        println__T_string("none")
    case Some:
        var x29 int32 = x__9.(Some)._0
        var v__10 int32 = x29
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
    var t67 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(v__11, 0)
    println__T_int32(t67)
    var t68 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(v__11, 1)
    println__T_int32(t68)
    var t69 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(v__11, 2)
    println__T_int32(t69)
    var t70 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(v__11)
    println__T_int32(t70)
    var m__12 *hashmap_Key_int32_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__Key____V__int32()
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(m__12, A{}, 10)
    var t71 Key = B{
        _0: 1,
    }
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(m__12, t71, 20)
    var t72 int32 = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(m__12)
    println__T_int32(t72)
    var t73 Option__int32 = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__Key____V__int32(m__12, A{})
    print_opt_int(t73)
    var t74 Key = B{
        _0: 1,
    }
    var t75 bool = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(m__12, t74)
    println__T_bool(t75)
    var t76 Key = B{
        _0: 1,
    }
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_remove____K__Key____V__int32(m__12, t76)
    var t77 Key = B{
        _0: 1,
    }
    var t78 bool = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(m__12, t77)
    println__T_bool(t78)
    var t79 int32 = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(m__12)
    println__T_int32(t79)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__33 int32, other__34 int32) bool {
    var retv81 bool
    var t82 bool = self__33 == other__34
    retv81 = t82
    return retv81
}

func _goml_m_trait__impl_i_Hash_i_int32_i_hash(self__55 int32) uint64 {
    var retv84 uint64
    var t85 uint64 = _goml_runtime_core_int32_hash(self__55)
    retv84 = t85
    return retv84
}

func println__T_string(value__1 string) struct{} {
    var t87 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t87)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t90 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t90)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv93 *_goml_vec_int32
    var t94 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv93 = t94
    return retv93
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__96 *_goml_vec_int32, elem__97 int32) struct{} {
    vec_push__Vec_5int32(self__96, elem__97)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__102 *_goml_vec_int32, index__103 int32) int32 {
    var retv98 int32
    var t99 int32 = vec_get__Vec_5int32(self__102, index__103)
    retv98 = t99
    return retv98
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__107 *_goml_vec_int32) int32 {
    var retv101 int32
    var t102 int32 = vec_len__Vec_5int32(self__107)
    retv101 = t102
    return retv101
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__Key____V__int32() *hashmap_Key_int32_x {
    var retv104 *hashmap_Key_int32_x
    var t105 *hashmap_Key_int32_x = hashmap_new__HashMap_3Key_5int32()
    retv104 = t105
    return retv104
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(self__129 *hashmap_Key_int32_x, key__130 Key, value__131 int32) struct{} {
    hashmap_set__HashMap_3Key_5int32(self__129, key__130, value__131)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(self__134 *hashmap_Key_int32_x) int32 {
    var retv109 int32
    var t110 int32 = hashmap_len__HashMap_3Key_5int32(self__134)
    retv109 = t110
    return retv109
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__Key____V__int32(self__127 *hashmap_Key_int32_x, key__128 Key) Option__int32 {
    var retv112 Option__int32
    var t113 Option__int32 = hashmap_get__HashMap_3Key_5int32(self__127, key__128)
    retv112 = t113
    return retv112
}

func println__T_bool(value__1 bool) struct{} {
    var t115 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t115)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(self__135 *hashmap_Key_int32_x, key__136 Key) bool {
    var retv118 bool
    var t119 bool = hashmap_contains__HashMap_3Key_5int32(self__135, key__136)
    retv118 = t119
    return retv118
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_remove____K__Key____V__int32(self__132 *hashmap_Key_int32_x, key__133 Key) struct{} {
    hashmap_remove__HashMap_3Key_5int32(self__132, key__133)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv123 string
    retv123 = self__9
    return retv123
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv125 string
    var t126 string = _goml_runtime_core_int32_to_string(self__13)
    retv125 = t126
    return retv125
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__8 bool) string {
    var retv128 string
    var t129 string = _goml_runtime_core_bool_to_string(self__8)
    retv128 = t129
    return retv128
}

func main() {
    main0()
}
