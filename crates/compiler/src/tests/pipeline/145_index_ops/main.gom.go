package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_hash(s string) uint64 {
    var h uint64 = 14695981039346656037
    var i int32 = 0
    for {
        if i >= int32(len(s)) {
            break
        }
        h = h * 1099511628211 + uint64(s[i])
        i = i + 1
    }
    return h
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func array_get__Array_2_5int32(arr [2]int32, index int32) int32 {
    return arr[index]
}

func array_set__Array_2_5int32(arr [2]int32, index int32, value int32) [2]int32 {
    arr[index] = value
    return arr
}

func array_get__Array_3_5int32(arr [3]int32, index int32) int32 {
    return arr[index]
}

func array_set__Array_3_5int32(arr [3]int32, index int32, value int32) [3]int32 {
    arr[index] = value
    return arr
}

func array_get__Array_2_14Array_2_5int32(arr [2][2]int32, index int32) [2]int32 {
    return arr[index]
}

func array_set__Array_2_14Array_2_5int32(arr [2][2]int32, index int32, value [2]int32) [2][2]int32 {
    arr[index] = value
    return arr
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

func vec_set__Vec_5int32(vec *_goml_vec_int32, index int32, value int32) struct{} {
    vec.items[index] = value
    return struct{}{}
}

func vec_len__Vec_5int32(vec *_goml_vec_int32) int32 {
    return int32(len(vec.items))
}

type _goml_vec_Array_2_5int32 struct {
    items [][2]int32
}

func vec_new__Vec_14Array_2_5int32() *_goml_vec_Array_2_5int32 {
    return &_goml_vec_Array_2_5int32{
        items: nil,
    }
}

func vec_push__Vec_14Array_2_5int32(vec *_goml_vec_Array_2_5int32, elem [2]int32) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_14Array_2_5int32(vec *_goml_vec_Array_2_5int32, index int32) [2]int32 {
    return vec.items[index]
}

func vec_set__Vec_14Array_2_5int32(vec *_goml_vec_Array_2_5int32, index int32, value [2]int32) struct{} {
    vec.items[index] = value
    return struct{}{}
}

func vec_len__Vec_14Array_2_5int32(vec *_goml_vec_Array_2_5int32) int32 {
    return int32(len(vec.items))
}

type ref_Array_2_5int32_x struct {
    value [2]int32
}

func ref__Ref_14Array_2_5int32(value [2]int32) *ref_Array_2_5int32_x {
    return &ref_Array_2_5int32_x{
        value: value,
    }
}

func ref_get__Ref_14Array_2_5int32(reference *ref_Array_2_5int32_x) [2]int32 {
    return reference.value
}

func ref_set__Ref_14Array_2_5int32(reference *ref_Array_2_5int32_x, value [2]int32) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_int32_x struct {
    value int32
}

func ref__Ref_5int32(value int32) *ref_int32_x {
    return &ref_int32_x{
        value: value,
    }
}

func ref_get__Ref_5int32(reference *ref_int32_x) int32 {
    return reference.value
}

func ref_set__Ref_5int32(reference *ref_int32_x, value int32) struct{} {
    reference.value = value
    return struct{}{}
}

type hashmap_string_int32_x_entry struct {
    active bool
    key string
    value int32
}

type hashmap_string_int32_x struct {
    buckets map[uint64][]hashmap_string_int32_x_entry
    len int32
}

func hashmap_new__HashMap_6string_5int32() *hashmap_string_int32_x {
    return &hashmap_string_int32_x{
        buckets: make(map[uint64][]hashmap_string_int32_x_entry),
        len: 0,
    }
}

func hashmap_lookup__HashMap_6string_5int32(m *hashmap_string_int32_x, key string) (int32, bool) {
    if m == nil {
        var zero int32
        return zero, false
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_string_i_hash(key)
    var bucket []hashmap_string_int32_x_entry = m.buckets[h]
    var i int32 = 0
    for {
        if i >= int32(len(bucket)) {
            break
        }
        var entry hashmap_string_int32_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_Eq_i_string_i_eq(entry.key, key) {
            return entry.value, true
        }
        i = i + 1
    }
    var zero int32
    return zero, false
}

func hashmap_get__HashMap_6string_5int32(m *hashmap_string_int32_x, key string) Option__int32 {
    var value int32
    var ok bool
    value, ok = hashmap_lookup__HashMap_6string_5int32(m, key)
    if ok {
        return Some{
            _0: value,
        }
    }
    return None{}
}

func hashmap_set__HashMap_6string_5int32(m *hashmap_string_int32_x, key string, value int32) struct{} {
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_string_i_hash(key)
    var bucket []hashmap_string_int32_x_entry = m.buckets[h]
    var i int32 = 0
    for {
        if i >= int32(len(bucket)) {
            break
        }
        var entry hashmap_string_int32_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_Eq_i_string_i_eq(entry.key, key) {
            bucket[i].value = value
            return struct{}{}
        }
        i = i + 1
    }
    bucket = append(bucket, hashmap_string_int32_x_entry{
        active: true,
        key: key,
        value: value,
    })
    m.buckets[h] = bucket
    m.len = m.len + 1
    return struct{}{}
}

type Tuple2_13Array2_5int32_5int32 struct {
    _0 [2]int32
    _1 int32
}

type Holder struct {
    data [2]int32
    vecs *_goml_vec_Array_2_5int32
}

type Option__int32 interface {
    isOption__int32()
}

type None struct {}

func (_ None) isOption__int32() {}

type Some struct {
    _0 int32
}

func (_ Some) isOption__int32() {}

func print_opt_int(x__0 Option__int32) struct{} {
    switch x__0.(type) {
    case None:
        println__T_string("none")
    case Some:
        var x4 int32 = x__0.(Some)._0
        var v__1 int32 = x4
        println__T_int32(v__1)
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func main0() struct{} {
    var t71 [2]int32 = [2]int32{31, 32}
    var t72 int32 = array_get__Array_2_5int32(t71, 1)
    println__T_int32(t72)
    var arr__2 [3]int32 = [3]int32{1, 2, 3}
    var t73 int32 = array_get__Array_3_5int32(arr__2, 0)
    println__T_int32(t73)
    var arr2__3 [3]int32 = [3]int32{4, 5, 6}
    var place_root7 [3]int32 = arr2__3
    var index8 int32 = 1
    array_get__Array_3_5int32(place_root7, index8)
    var value10 int32 = 50
    var t74 [3]int32 = array_set__Array_3_5int32(place_root7, index8, value10)
    arr2__3 = t74
    var t76 int32 = array_get__Array_3_5int32(arr2__3, 1)
    println__T_int32(t76)
    var t77 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    var t78 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t77, 7)
    var t79 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t78, 8)
    var vec__4 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t79, 9)
    var t80 int32 = vec_get__Vec_5int32(vec__4, 2)
    println__T_int32(t80)
    var t81 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    var t82 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t81, 10)
    var t83 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t82, 11)
    var vec2__5 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t83, 12)
    var place_root14 *_goml_vec_int32 = vec2__5
    var index15 int32 = 0
    vec_get__Vec_5int32(place_root14, index15)
    var value17 int32 = 100
    vec_set__Vec_5int32(place_root14, index15, value17)
    var t85 int32 = vec_get__Vec_5int32(vec2__5, 0)
    println__T_int32(t85)
    var s__6 []int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(vec2__5, 0, 2)
    var t86 int32 = s__6[1]
    println__T_int32(t86)
    var map__7 *hashmap_string_int32_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32()
    var place_root21 *hashmap_string_int32_x = map__7
    var index22 string = "a"
    hashmap_get__HashMap_6string_5int32(place_root21, index22)
    var value24 int32 = 13
    hashmap_set__HashMap_6string_5int32(place_root21, index22, value24)
    var t88 Option__int32 = hashmap_get__HashMap_6string_5int32(map__7, "a")
    print_opt_int(t88)
    var t89 Option__int32 = hashmap_get__HashMap_6string_5int32(map__7, "missing")
    print_opt_int(t89)
    var t90 [2]int32 = [2]int32{1, 2}
    var t91 [2]int32 = [2]int32{3, 4}
    var matrix__8 [2][2]int32 = [2][2]int32{t90, t91}
    var place_root28 [2][2]int32 = matrix__8
    var index29 int32 = 1
    var place30 [2]int32 = array_get__Array_2_14Array_2_5int32(place_root28, index29)
    var index31 int32 = 0
    array_get__Array_2_5int32(place30, index31)
    var value33 int32 = 30
    var t92 [2]int32 = array_set__Array_2_5int32(place30, index31, value33)
    var t93 [2][2]int32 = array_set__Array_2_14Array_2_5int32(place_root28, index29, t92)
    matrix__8 = t93
    var t95 [2]int32 = array_get__Array_2_14Array_2_5int32(matrix__8, 1)
    var t96 int32 = array_get__Array_2_5int32(t95, 0)
    println__T_int32(t96)
    var t97 [2]int32 = [2]int32{14, 15}
    var pair__9 Tuple2_13Array2_5int32_5int32 = Tuple2_13Array2_5int32_5int32{
        _0: t97,
        _1: 0,
    }
    var place_root36 Tuple2_13Array2_5int32_5int32 = pair__9
    var place37 [2]int32 = place_root36._0
    var index38 int32 = 1
    array_get__Array_2_5int32(place37, index38)
    var value40 int32 = 150
    var t98 [2]int32 = array_set__Array_2_5int32(place37, index38, value40)
    var t99 int32 = place_root36._1
    var t100 Tuple2_13Array2_5int32_5int32 = Tuple2_13Array2_5int32_5int32{
        _0: t98,
        _1: t99,
    }
    pair__9 = t100
    var t102 [2]int32 = pair__9._0
    var t103 int32 = array_get__Array_2_5int32(t102, 1)
    println__T_int32(t103)
    var t104 [2]int32 = [2]int32{16, 17}
    var t105 *_goml_vec_Array_2_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___l_int32_x3b_2_r_()
    var t106 [2]int32 = [2]int32{18, 19}
    var t107 *_goml_vec_Array_2_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T___l_int32_x3b_2_r_(t105, t106)
    var t108 [2]int32 = [2]int32{20, 21}
    var t109 *_goml_vec_Array_2_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T___l_int32_x3b_2_r_(t107, t108)
    var holder__10 Holder = Holder{
        data: t104,
        vecs: t109,
    }
    var place_root43 Holder = holder__10
    var place44 [2]int32 = place_root43.data
    var index45 int32 = 0
    array_get__Array_2_5int32(place44, index45)
    var value47 int32 = 160
    var t110 [2]int32 = array_set__Array_2_5int32(place44, index45, value47)
    var t111 *_goml_vec_Array_2_5int32 = place_root43.vecs
    var t112 Holder = Holder{
        data: t110,
        vecs: t111,
    }
    holder__10 = t112
    var t114 [2]int32 = holder__10.data
    var t115 int32 = array_get__Array_2_5int32(t114, 0)
    println__T_int32(t115)
    var place_root50 Holder = holder__10
    var place51 *_goml_vec_Array_2_5int32 = place_root50.vecs
    var index52 int32 = 1
    var place53 [2]int32 = vec_get__Vec_14Array_2_5int32(place51, index52)
    var index54 int32 = 0
    array_get__Array_2_5int32(place53, index54)
    var value56 int32 = 200
    var t116 [2]int32 = array_set__Array_2_5int32(place53, index54, value56)
    vec_set__Vec_14Array_2_5int32(place51, index52, t116)
    var t118 *_goml_vec_Array_2_5int32 = holder__10.vecs
    var t119 [2]int32 = vec_get__Vec_14Array_2_5int32(t118, 1)
    var t120 int32 = array_get__Array_2_5int32(t119, 0)
    println__T_int32(t120)
    var t121 [2]int32 = [2]int32{22, 23}
    var r__11 *ref_Array_2_5int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T___l_int32_x3b_2_r_(t121)
    var place_ref59 *ref_Array_2_5int32_x = r__11
    var place_root60 [2]int32 = ref_get__Ref_14Array_2_5int32(place_ref59)
    var index61 int32 = 1
    array_get__Array_2_5int32(place_root60, index61)
    var value63 int32 = 230
    var t122 [2]int32 = array_set__Array_2_5int32(place_root60, index61, value63)
    ref_set__Ref_14Array_2_5int32(place_ref59, t122)
    var t124 [2]int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T___l_int32_x3b_2_r_(r__11)
    var t125 int32 = array_get__Array_2_5int32(t124, 1)
    println__T_int32(t125)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t127 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t127)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t130 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t130)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv133 *_goml_vec_int32
    var t134 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv133 = t134
    return retv133
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(self__73 *_goml_vec_int32, elem__74 int32) *_goml_vec_int32 {
    var retv136 *_goml_vec_int32
    var result__75 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    var index__76 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop138:
    for {
        var t139 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__76)
        var t140 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__73)
        var t141 bool = t139 < t140
        if t141 {
            var t142 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__76)
            var t143 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__73, t142)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(result__75, t143)
            var t144 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__76)
            var t145 int32 = t144 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__76, t145)
            continue
        } else {
            break Loop_loop138
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(result__75, elem__74)
    retv136 = result__75
    return retv136
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(self__83 *_goml_vec_int32, start__84 int32, end__85 int32) []int32 {
    var retv147 []int32
    var t148 []int32 = self__83.items[start__84:end__85]
    retv147 = t148
    return retv147
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32() *hashmap_string_int32_x {
    var retv150 *hashmap_string_int32_x
    var t151 *hashmap_string_int32_x = hashmap_new__HashMap_6string_5int32()
    retv150 = t151
    return retv150
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___l_int32_x3b_2_r_() *_goml_vec_Array_2_5int32 {
    var retv153 *_goml_vec_Array_2_5int32
    var t154 *_goml_vec_Array_2_5int32 = vec_new__Vec_14Array_2_5int32()
    retv153 = t154
    return retv153
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T___l_int32_x3b_2_r_(self__73 *_goml_vec_Array_2_5int32, elem__74 [2]int32) *_goml_vec_Array_2_5int32 {
    var retv156 *_goml_vec_Array_2_5int32
    var result__75 *_goml_vec_Array_2_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___l_int32_x3b_2_r_()
    var index__76 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop158:
    for {
        var t159 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__76)
        var t160 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___l_int32_x3b_2_r_(self__73)
        var t161 bool = t159 < t160
        if t161 {
            var t162 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__76)
            var t163 [2]int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T___l_int32_x3b_2_r_(self__73, t162)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___l_int32_x3b_2_r_(result__75, t163)
            var t164 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__76)
            var t165 int32 = t164 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__76, t165)
            continue
        } else {
            break Loop_loop158
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___l_int32_x3b_2_r_(result__75, elem__74)
    retv156 = result__75
    return retv156
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T___l_int32_x3b_2_r_(value__102 [2]int32) *ref_Array_2_5int32_x {
    var retv167 *ref_Array_2_5int32_x
    var t168 *ref_Array_2_5int32_x = ref__Ref_14Array_2_5int32(value__102)
    retv167 = t168
    return retv167
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T___l_int32_x3b_2_r_(self__103 *ref_Array_2_5int32_x) [2]int32 {
    var retv170 [2]int32
    var t171 [2]int32 = ref_get__Ref_14Array_2_5int32(self__103)
    retv170 = t171
    return retv170
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv173 string
    retv173 = self__9
    return retv173
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv175 string
    var t176 string = _goml_runtime_core_int32_to_string(self__13)
    retv175 = t176
    return retv175
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__102 int32) *ref_int32_x {
    var retv178 *ref_int32_x
    var t179 *ref_int32_x = ref__Ref_5int32(value__102)
    retv178 = t179
    return retv178
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__103 *ref_int32_x) int32 {
    var retv181 int32
    var t182 int32 = ref_get__Ref_5int32(self__103)
    retv181 = t182
    return retv181
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__82 *_goml_vec_int32) int32 {
    var retv184 int32
    var t185 int32 = vec_len__Vec_5int32(self__82)
    retv184 = t185
    return retv184
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__71 *_goml_vec_int32, elem__72 int32) struct{} {
    vec_push__Vec_5int32(self__71, elem__72)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__77 *_goml_vec_int32, index__78 int32) int32 {
    var retv189 int32
    var t190 int32 = vec_get__Vec_5int32(self__77, index__78)
    retv189 = t190
    return retv189
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__104 *ref_int32_x, value__105 int32) struct{} {
    ref_set__Ref_5int32(self__104, value__105)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___l_int32_x3b_2_r_(self__82 *_goml_vec_Array_2_5int32) int32 {
    var retv194 int32
    var t195 int32 = vec_len__Vec_14Array_2_5int32(self__82)
    retv194 = t195
    return retv194
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___l_int32_x3b_2_r_(self__71 *_goml_vec_Array_2_5int32, elem__72 [2]int32) struct{} {
    vec_push__Vec_14Array_2_5int32(self__71, elem__72)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T___l_int32_x3b_2_r_(self__77 *_goml_vec_Array_2_5int32, index__78 int32) [2]int32 {
    var retv199 [2]int32
    var t200 [2]int32 = vec_get__Vec_14Array_2_5int32(self__77, index__78)
    retv199 = t200
    return retv199
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__25 string, other__26 string) bool {
    var retv202 bool
    var t203 bool = self__25 == other__26
    retv202 = t203
    return retv202
}

func _goml_m_trait__impl_i_Hash_i_string_i_hash(self__51 string) uint64 {
    var retv205 uint64
    var t206 uint64 = _goml_runtime_core_string_hash(self__51)
    retv205 = t206
    return retv205
}

func main() {
    main0()
}
