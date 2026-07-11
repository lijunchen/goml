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
        var x7 int32 = x__0.(Some)._0
        var v__1 int32 = x7
        println__T_int32(v__1)
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func main0() struct{} {
    var t74 [2]int32 = [2]int32{31, 32}
    var t75 int32 = array_get__Array_2_5int32(t74, 1)
    println__T_int32(t75)
    var arr__2 [3]int32 = [3]int32{1, 2, 3}
    var t76 int32 = array_get__Array_3_5int32(arr__2, 0)
    println__T_int32(t76)
    var arr2__3 [3]int32 = [3]int32{4, 5, 6}
    var place_root10 [3]int32 = arr2__3
    var index11 int32 = 1
    array_get__Array_3_5int32(place_root10, index11)
    var value13 int32 = 50
    var t77 [3]int32 = array_set__Array_3_5int32(place_root10, index11, value13)
    arr2__3 = t77
    var t79 int32 = array_get__Array_3_5int32(arr2__3, 1)
    println__T_int32(t79)
    var t80 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    var t81 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t80, 7)
    var t82 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t81, 8)
    var vec__4 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t82, 9)
    var t83 int32 = vec_get__Vec_5int32(vec__4, 2)
    println__T_int32(t83)
    var t84 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    var t85 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t84, 10)
    var t86 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t85, 11)
    var vec2__5 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t86, 12)
    var place_root17 *_goml_vec_int32 = vec2__5
    var index18 int32 = 0
    vec_get__Vec_5int32(place_root17, index18)
    var value20 int32 = 100
    vec_set__Vec_5int32(place_root17, index18, value20)
    var t88 int32 = vec_get__Vec_5int32(vec2__5, 0)
    println__T_int32(t88)
    var s__6 []int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(vec2__5, 0, 2)
    var t89 int32 = s__6[1]
    println__T_int32(t89)
    var map__7 *hashmap_string_int32_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32()
    var place_root24 *hashmap_string_int32_x = map__7
    var index25 string = "a"
    hashmap_get__HashMap_6string_5int32(place_root24, index25)
    var value27 int32 = 13
    hashmap_set__HashMap_6string_5int32(place_root24, index25, value27)
    var t91 Option__int32 = hashmap_get__HashMap_6string_5int32(map__7, "a")
    print_opt_int(t91)
    var t92 Option__int32 = hashmap_get__HashMap_6string_5int32(map__7, "missing")
    print_opt_int(t92)
    var t93 [2]int32 = [2]int32{1, 2}
    var t94 [2]int32 = [2]int32{3, 4}
    var matrix__8 [2][2]int32 = [2][2]int32{t93, t94}
    var place_root31 [2][2]int32 = matrix__8
    var index32 int32 = 1
    var place33 [2]int32 = array_get__Array_2_14Array_2_5int32(place_root31, index32)
    var index34 int32 = 0
    array_get__Array_2_5int32(place33, index34)
    var value36 int32 = 30
    var t95 [2]int32 = array_set__Array_2_5int32(place33, index34, value36)
    var t96 [2][2]int32 = array_set__Array_2_14Array_2_5int32(place_root31, index32, t95)
    matrix__8 = t96
    var t98 [2]int32 = array_get__Array_2_14Array_2_5int32(matrix__8, 1)
    var t99 int32 = array_get__Array_2_5int32(t98, 0)
    println__T_int32(t99)
    var t100 [2]int32 = [2]int32{14, 15}
    var pair__9 Tuple2_13Array2_5int32_5int32 = Tuple2_13Array2_5int32_5int32{
        _0: t100,
        _1: 0,
    }
    var place_root39 Tuple2_13Array2_5int32_5int32 = pair__9
    var place40 [2]int32 = place_root39._0
    var index41 int32 = 1
    array_get__Array_2_5int32(place40, index41)
    var value43 int32 = 150
    var t101 [2]int32 = array_set__Array_2_5int32(place40, index41, value43)
    var t102 int32 = place_root39._1
    var t103 Tuple2_13Array2_5int32_5int32 = Tuple2_13Array2_5int32_5int32{
        _0: t101,
        _1: t102,
    }
    pair__9 = t103
    var t105 [2]int32 = pair__9._0
    var t106 int32 = array_get__Array_2_5int32(t105, 1)
    println__T_int32(t106)
    var t107 [2]int32 = [2]int32{16, 17}
    var t108 *_goml_vec_Array_2_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___l_int32_x3b_2_r_()
    var t109 [2]int32 = [2]int32{18, 19}
    var t110 *_goml_vec_Array_2_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T___l_int32_x3b_2_r_(t108, t109)
    var t111 [2]int32 = [2]int32{20, 21}
    var t112 *_goml_vec_Array_2_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T___l_int32_x3b_2_r_(t110, t111)
    var holder__10 Holder = Holder{
        data: t107,
        vecs: t112,
    }
    var place_root46 Holder = holder__10
    var place47 [2]int32 = place_root46.data
    var index48 int32 = 0
    array_get__Array_2_5int32(place47, index48)
    var value50 int32 = 160
    var t113 [2]int32 = array_set__Array_2_5int32(place47, index48, value50)
    var t114 *_goml_vec_Array_2_5int32 = place_root46.vecs
    var t115 Holder = Holder{
        data: t113,
        vecs: t114,
    }
    holder__10 = t115
    var t117 [2]int32 = holder__10.data
    var t118 int32 = array_get__Array_2_5int32(t117, 0)
    println__T_int32(t118)
    var place_root53 Holder = holder__10
    var place54 *_goml_vec_Array_2_5int32 = place_root53.vecs
    var index55 int32 = 1
    var place56 [2]int32 = vec_get__Vec_14Array_2_5int32(place54, index55)
    var index57 int32 = 0
    array_get__Array_2_5int32(place56, index57)
    var value59 int32 = 200
    var t119 [2]int32 = array_set__Array_2_5int32(place56, index57, value59)
    vec_set__Vec_14Array_2_5int32(place54, index55, t119)
    var t121 *_goml_vec_Array_2_5int32 = holder__10.vecs
    var t122 [2]int32 = vec_get__Vec_14Array_2_5int32(t121, 1)
    var t123 int32 = array_get__Array_2_5int32(t122, 0)
    println__T_int32(t123)
    var t124 [2]int32 = [2]int32{22, 23}
    var r__11 *ref_Array_2_5int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T___l_int32_x3b_2_r_(t124)
    var place_ref62 *ref_Array_2_5int32_x = r__11
    var place_root63 [2]int32 = ref_get__Ref_14Array_2_5int32(place_ref62)
    var index64 int32 = 1
    array_get__Array_2_5int32(place_root63, index64)
    var value66 int32 = 230
    var t125 [2]int32 = array_set__Array_2_5int32(place_root63, index64, value66)
    ref_set__Ref_14Array_2_5int32(place_ref62, t125)
    var t127 [2]int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T___l_int32_x3b_2_r_(r__11)
    var t128 int32 = array_get__Array_2_5int32(t127, 1)
    println__T_int32(t128)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t130 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t130)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t133 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t133)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv136 *_goml_vec_int32
    var t137 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv136 = t137
    return retv136
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(self__75 *_goml_vec_int32, elem__76 int32) *_goml_vec_int32 {
    var retv139 *_goml_vec_int32
    var result__77 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    var index__78 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop141:
    for {
        var t142 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__78)
        var t143 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__75)
        var t144 bool = t142 < t143
        if t144 {
            var t145 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__78)
            var t146 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__75, t145)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(result__77, t146)
            var t147 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__78)
            var t148 int32 = t147 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__78, t148)
            continue
        } else {
            break Loop_loop141
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(result__77, elem__76)
    retv139 = result__77
    return retv139
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(self__85 *_goml_vec_int32, start__86 int32, end__87 int32) []int32 {
    var retv150 []int32
    var t151 []int32 = self__85.items[start__86:end__87]
    retv150 = t151
    return retv150
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32() *hashmap_string_int32_x {
    var retv153 *hashmap_string_int32_x
    var t154 *hashmap_string_int32_x = hashmap_new__HashMap_6string_5int32()
    retv153 = t154
    return retv153
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___l_int32_x3b_2_r_() *_goml_vec_Array_2_5int32 {
    var retv156 *_goml_vec_Array_2_5int32
    var t157 *_goml_vec_Array_2_5int32 = vec_new__Vec_14Array_2_5int32()
    retv156 = t157
    return retv156
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T___l_int32_x3b_2_r_(self__75 *_goml_vec_Array_2_5int32, elem__76 [2]int32) *_goml_vec_Array_2_5int32 {
    var retv159 *_goml_vec_Array_2_5int32
    var result__77 *_goml_vec_Array_2_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___l_int32_x3b_2_r_()
    var index__78 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop161:
    for {
        var t162 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__78)
        var t163 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___l_int32_x3b_2_r_(self__75)
        var t164 bool = t162 < t163
        if t164 {
            var t165 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__78)
            var t166 [2]int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T___l_int32_x3b_2_r_(self__75, t165)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___l_int32_x3b_2_r_(result__77, t166)
            var t167 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__78)
            var t168 int32 = t167 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__78, t168)
            continue
        } else {
            break Loop_loop161
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___l_int32_x3b_2_r_(result__77, elem__76)
    retv159 = result__77
    return retv159
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T___l_int32_x3b_2_r_(value__114 [2]int32) *ref_Array_2_5int32_x {
    var retv170 *ref_Array_2_5int32_x
    var t171 *ref_Array_2_5int32_x = ref__Ref_14Array_2_5int32(value__114)
    retv170 = t171
    return retv170
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T___l_int32_x3b_2_r_(self__115 *ref_Array_2_5int32_x) [2]int32 {
    var retv173 [2]int32
    var t174 [2]int32 = ref_get__Ref_14Array_2_5int32(self__115)
    retv173 = t174
    return retv173
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv176 string
    retv176 = self__9
    return retv176
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv178 string
    var t179 string = _goml_runtime_core_int32_to_string(self__13)
    retv178 = t179
    return retv178
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__114 int32) *ref_int32_x {
    var retv181 *ref_int32_x
    var t182 *ref_int32_x = ref__Ref_5int32(value__114)
    retv181 = t182
    return retv181
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__115 *ref_int32_x) int32 {
    var retv184 int32
    var t185 int32 = ref_get__Ref_5int32(self__115)
    retv184 = t185
    return retv184
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__84 *_goml_vec_int32) int32 {
    var retv187 int32
    var t188 int32 = vec_len__Vec_5int32(self__84)
    retv187 = t188
    return retv187
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__73 *_goml_vec_int32, elem__74 int32) struct{} {
    vec_push__Vec_5int32(self__73, elem__74)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__79 *_goml_vec_int32, index__80 int32) int32 {
    var retv192 int32
    var t193 int32 = vec_get__Vec_5int32(self__79, index__80)
    retv192 = t193
    return retv192
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__116 *ref_int32_x, value__117 int32) struct{} {
    ref_set__Ref_5int32(self__116, value__117)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___l_int32_x3b_2_r_(self__84 *_goml_vec_Array_2_5int32) int32 {
    var retv197 int32
    var t198 int32 = vec_len__Vec_14Array_2_5int32(self__84)
    retv197 = t198
    return retv197
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___l_int32_x3b_2_r_(self__73 *_goml_vec_Array_2_5int32, elem__74 [2]int32) struct{} {
    vec_push__Vec_14Array_2_5int32(self__73, elem__74)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T___l_int32_x3b_2_r_(self__79 *_goml_vec_Array_2_5int32, index__80 int32) [2]int32 {
    var retv202 [2]int32
    var t203 [2]int32 = vec_get__Vec_14Array_2_5int32(self__79, index__80)
    retv202 = t203
    return retv202
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__25 string, other__26 string) bool {
    var retv205 bool
    var t206 bool = self__25 == other__26
    retv205 = t206
    return retv205
}

func _goml_m_trait__impl_i_Hash_i_string_i_hash(self__51 string) uint64 {
    var retv208 uint64
    var t209 uint64 = _goml_runtime_core_string_hash(self__51)
    retv208 = t209
    return retv208
}

func main() {
    main0()
}
