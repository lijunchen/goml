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
    var retv31 bool
    var mtmp7 Tuple2_3Key_3Key = Tuple2_3Key_3Key{
        _0: self__0,
        _1: other__1,
    }
    var x8 Key = mtmp7._0
    var x9 Key = mtmp7._1
    var jp33 bool
    switch x9.(type) {
    case A:
        var jp35 bool
        switch x8.(type) {
        case A:
            jp35 = true
        case B:
            jp35 = false
        default:
            panic("non-exhaustive match")
        }
        jp33 = jp35
    case B:
        var x10 int32 = x9.(B)._0
        var jp37 bool
        switch x8.(type) {
        case A:
            jp37 = false
        case B:
            var x12 int32 = x8.(B)._0
            var __l1_0__2 int32 = x12
            var __r1_0__3 int32 = x10
            var jp39 bool
            if true {
                var t40 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(__l1_0__2, __r1_0__3)
                jp39 = t40
            } else {
                jp39 = false
            }
            jp37 = jp39
        default:
            panic("non-exhaustive match")
        }
        jp33 = jp37
    default:
        panic("non-exhaustive match")
    }
    retv31 = jp33
    return retv31
}

func _goml_m_trait__impl_i_Hash_i_Key_i_hash(self__4 Key) uint64 {
    var retv42 uint64
    var jp44 uint64
    switch self__4.(type) {
    case A:
        var h__5 uint64 = 14695981039346656037 + 1
        jp44 = h__5
    case B:
        var x13 int32 = self__4.(B)._0
        var __field1_0__6 int32 = x13
        var h__7 uint64 = 14695981039346656037 + 2
        var t45 uint64 = h__7 * 1099511628211
        var t46 uint64 = _goml_m_trait__impl_i_Hash_i_int32_i_hash(__field1_0__6)
        var h__8 uint64 = t45 + t46
        jp44 = h__8
    default:
        panic("non-exhaustive match")
    }
    retv42 = jp44
    return retv42
}

func print_opt_int(x__9 Option__int32) struct{} {
    switch x__9.(type) {
    case None:
        println__T_string("none")
    case Some:
        var x14 int32 = x__9.(Some)._0
        var v__10 int32 = x14
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
    var t52 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(v__11, 0)
    println__T_int32(t52)
    var t53 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(v__11, 1)
    println__T_int32(t53)
    var t54 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(v__11, 2)
    println__T_int32(t54)
    var t55 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(v__11)
    println__T_int32(t55)
    var m__12 *hashmap_Key_int32_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__Key____V__int32()
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(m__12, A{}, 10)
    var t56 Key = B{
        _0: 1,
    }
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(m__12, t56, 20)
    var t57 int32 = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(m__12)
    println__T_int32(t57)
    var t58 Option__int32 = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__Key____V__int32(m__12, A{})
    print_opt_int(t58)
    var t59 Key = B{
        _0: 1,
    }
    var t60 bool = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(m__12, t59)
    println__T_bool(t60)
    var t61 Key = B{
        _0: 1,
    }
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_remove____K__Key____V__int32(m__12, t61)
    var t62 Key = B{
        _0: 1,
    }
    var t63 bool = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(m__12, t62)
    println__T_bool(t63)
    var t64 int32 = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(m__12)
    println__T_int32(t64)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__33 int32, other__34 int32) bool {
    var retv66 bool
    var t67 bool = self__33 == other__34
    retv66 = t67
    return retv66
}

func _goml_m_trait__impl_i_Hash_i_int32_i_hash(self__55 int32) uint64 {
    var retv69 uint64
    var t70 uint64 = _goml_runtime_core_int32_hash(self__55)
    retv69 = t70
    return retv69
}

func println__T_string(value__1 string) struct{} {
    var t72 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t72)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t75 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t75)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv78 *_goml_vec_int32
    var t79 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv78 = t79
    return retv78
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__73 *_goml_vec_int32, elem__74 int32) struct{} {
    vec_push__Vec_5int32(self__73, elem__74)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__79 *_goml_vec_int32, index__80 int32) int32 {
    var retv83 int32
    var t84 int32 = vec_get__Vec_5int32(self__79, index__80)
    retv83 = t84
    return retv83
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__84 *_goml_vec_int32) int32 {
    var retv86 int32
    var t87 int32 = vec_len__Vec_5int32(self__84)
    retv86 = t87
    return retv86
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__Key____V__int32() *hashmap_Key_int32_x {
    var retv89 *hashmap_Key_int32_x
    var t90 *hashmap_Key_int32_x = hashmap_new__HashMap_3Key_5int32()
    retv89 = t90
    return retv89
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__Key____V__int32(self__106 *hashmap_Key_int32_x, key__107 Key, value__108 int32) struct{} {
    hashmap_set__HashMap_3Key_5int32(self__106, key__107, value__108)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__Key____V__int32(self__111 *hashmap_Key_int32_x) int32 {
    var retv94 int32
    var t95 int32 = hashmap_len__HashMap_3Key_5int32(self__111)
    retv94 = t95
    return retv94
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__Key____V__int32(self__104 *hashmap_Key_int32_x, key__105 Key) Option__int32 {
    var retv97 Option__int32
    var t98 Option__int32 = hashmap_get__HashMap_3Key_5int32(self__104, key__105)
    retv97 = t98
    return retv97
}

func println__T_bool(value__1 bool) struct{} {
    var t100 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t100)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_contains____K__Key____V__int32(self__112 *hashmap_Key_int32_x, key__113 Key) bool {
    var retv103 bool
    var t104 bool = hashmap_contains__HashMap_3Key_5int32(self__112, key__113)
    retv103 = t104
    return retv103
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_remove____K__Key____V__int32(self__109 *hashmap_Key_int32_x, key__110 Key) struct{} {
    hashmap_remove__HashMap_3Key_5int32(self__109, key__110)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv108 string
    retv108 = self__9
    return retv108
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv110 string
    var t111 string = _goml_runtime_core_int32_to_string(self__13)
    retv110 = t111
    return retv110
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__8 bool) string {
    var retv113 string
    var t114 string = _goml_runtime_core_bool_to_string(self__8)
    retv113 = t114
    return retv113
}

func main() {
    main0()
}
