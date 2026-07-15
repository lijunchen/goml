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
        var x22 int32 = x__0.(Some)._0
        var v__1 int32 = x22
        println__T_int32(v__1)
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func main0() struct{} {
    var t89 [2]int32 = [2]int32{31, 32}
    var t90 int32 = array_get__Array_2_5int32(t89, 1)
    println__T_int32(t90)
    var arr__2 [3]int32 = [3]int32{1, 2, 3}
    var t91 int32 = array_get__Array_3_5int32(arr__2, 0)
    println__T_int32(t91)
    var arr2__3 [3]int32 = [3]int32{4, 5, 6}
    var place_root25 [3]int32 = arr2__3
    var index26 int32 = 1
    array_get__Array_3_5int32(place_root25, index26)
    var value28 int32 = 50
    var t92 [3]int32 = array_set__Array_3_5int32(place_root25, index26, value28)
    arr2__3 = t92
    var t94 int32 = array_get__Array_3_5int32(arr2__3, 1)
    println__T_int32(t94)
    var t95 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    var t96 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t95, 7)
    var t97 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t96, 8)
    var vec__4 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t97, 9)
    var t98 int32 = vec_get__Vec_5int32(vec__4, 2)
    println__T_int32(t98)
    var t99 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    var t100 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t99, 10)
    var t101 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t100, 11)
    var vec2__5 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t101, 12)
    var place_root32 *_goml_vec_int32 = vec2__5
    var index33 int32 = 0
    vec_get__Vec_5int32(place_root32, index33)
    var value35 int32 = 100
    vec_set__Vec_5int32(place_root32, index33, value35)
    var t103 int32 = vec_get__Vec_5int32(vec2__5, 0)
    println__T_int32(t103)
    var s__6 []int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(vec2__5, 0, 2)
    var t104 int32 = s__6[1]
    println__T_int32(t104)
    var map__7 *hashmap_string_int32_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32()
    var place_root39 *hashmap_string_int32_x = map__7
    var index40 string = "a"
    hashmap_get__HashMap_6string_5int32(place_root39, index40)
    var value42 int32 = 13
    hashmap_set__HashMap_6string_5int32(place_root39, index40, value42)
    var t106 Option__int32 = hashmap_get__HashMap_6string_5int32(map__7, "a")
    print_opt_int(t106)
    var t107 Option__int32 = hashmap_get__HashMap_6string_5int32(map__7, "missing")
    print_opt_int(t107)
    var t108 [2]int32 = [2]int32{1, 2}
    var t109 [2]int32 = [2]int32{3, 4}
    var matrix__8 [2][2]int32 = [2][2]int32{t108, t109}
    var place_root46 [2][2]int32 = matrix__8
    var index47 int32 = 1
    var place48 [2]int32 = array_get__Array_2_14Array_2_5int32(place_root46, index47)
    var index49 int32 = 0
    array_get__Array_2_5int32(place48, index49)
    var value51 int32 = 30
    var t110 [2]int32 = array_set__Array_2_5int32(place48, index49, value51)
    var t111 [2][2]int32 = array_set__Array_2_14Array_2_5int32(place_root46, index47, t110)
    matrix__8 = t111
    var t113 [2]int32 = array_get__Array_2_14Array_2_5int32(matrix__8, 1)
    var t114 int32 = array_get__Array_2_5int32(t113, 0)
    println__T_int32(t114)
    var t115 [2]int32 = [2]int32{14, 15}
    var pair__9 Tuple2_13Array2_5int32_5int32 = Tuple2_13Array2_5int32_5int32{
        _0: t115,
        _1: 0,
    }
    var place_root54 Tuple2_13Array2_5int32_5int32 = pair__9
    var place55 [2]int32 = place_root54._0
    var index56 int32 = 1
    array_get__Array_2_5int32(place55, index56)
    var value58 int32 = 150
    var t116 [2]int32 = array_set__Array_2_5int32(place55, index56, value58)
    var t117 int32 = place_root54._1
    var t118 Tuple2_13Array2_5int32_5int32 = Tuple2_13Array2_5int32_5int32{
        _0: t116,
        _1: t117,
    }
    pair__9 = t118
    var t120 [2]int32 = pair__9._0
    var t121 int32 = array_get__Array_2_5int32(t120, 1)
    println__T_int32(t121)
    var t122 [2]int32 = [2]int32{16, 17}
    var t123 *_goml_vec_Array_2_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___l_int32_x3b_2_r_()
    var t124 [2]int32 = [2]int32{18, 19}
    var t125 *_goml_vec_Array_2_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T___l_int32_x3b_2_r_(t123, t124)
    var t126 [2]int32 = [2]int32{20, 21}
    var t127 *_goml_vec_Array_2_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T___l_int32_x3b_2_r_(t125, t126)
    var holder__10 Holder = Holder{
        data: t122,
        vecs: t127,
    }
    var place_root61 Holder = holder__10
    var place62 [2]int32 = place_root61.data
    var index63 int32 = 0
    array_get__Array_2_5int32(place62, index63)
    var value65 int32 = 160
    var t128 [2]int32 = array_set__Array_2_5int32(place62, index63, value65)
    var t129 *_goml_vec_Array_2_5int32 = place_root61.vecs
    var t130 Holder = Holder{
        data: t128,
        vecs: t129,
    }
    holder__10 = t130
    var t132 [2]int32 = holder__10.data
    var t133 int32 = array_get__Array_2_5int32(t132, 0)
    println__T_int32(t133)
    var place_root68 Holder = holder__10
    var place69 *_goml_vec_Array_2_5int32 = place_root68.vecs
    var index70 int32 = 1
    var place71 [2]int32 = vec_get__Vec_14Array_2_5int32(place69, index70)
    var index72 int32 = 0
    array_get__Array_2_5int32(place71, index72)
    var value74 int32 = 200
    var t134 [2]int32 = array_set__Array_2_5int32(place71, index72, value74)
    vec_set__Vec_14Array_2_5int32(place69, index70, t134)
    var t136 *_goml_vec_Array_2_5int32 = holder__10.vecs
    var t137 [2]int32 = vec_get__Vec_14Array_2_5int32(t136, 1)
    var t138 int32 = array_get__Array_2_5int32(t137, 0)
    println__T_int32(t138)
    var t139 [2]int32 = [2]int32{22, 23}
    var r__11 *ref_Array_2_5int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T___l_int32_x3b_2_r_(t139)
    var place_ref77 *ref_Array_2_5int32_x = r__11
    var place_root78 [2]int32 = ref_get__Ref_14Array_2_5int32(place_ref77)
    var index79 int32 = 1
    array_get__Array_2_5int32(place_root78, index79)
    var value81 int32 = 230
    var t140 [2]int32 = array_set__Array_2_5int32(place_root78, index79, value81)
    ref_set__Ref_14Array_2_5int32(place_ref77, t140)
    var t142 [2]int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T___l_int32_x3b_2_r_(r__11)
    var t143 int32 = array_get__Array_2_5int32(t142, 1)
    println__T_int32(t143)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t145 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t145)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t148 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t148)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv151 *_goml_vec_int32
    var t152 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv151 = t152
    return retv151
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(self__99 *_goml_vec_int32, elem__100 int32) *_goml_vec_int32 {
    var retv154 *_goml_vec_int32
    var result__101 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    var index__102 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop156:
    for {
        var t157 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__102)
        var t158 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__99)
        var t159 bool = t157 < t158
        if t159 {
            var t160 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__102)
            var t161 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__99, t160)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(result__101, t161)
            var t162 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__102)
            var t163 int32 = t162 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__102, t163)
            continue
        } else {
            break Loop_loop156
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(result__101, elem__100)
    retv154 = result__101
    return retv154
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(self__109 *_goml_vec_int32, start__110 int32, end__111 int32) []int32 {
    var retv165 []int32
    var t166 []int32 = self__109.items[start__110:end__111]
    retv165 = t166
    return retv165
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32() *hashmap_string_int32_x {
    var retv168 *hashmap_string_int32_x
    var t169 *hashmap_string_int32_x = hashmap_new__HashMap_6string_5int32()
    retv168 = t169
    return retv168
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___l_int32_x3b_2_r_() *_goml_vec_Array_2_5int32 {
    var retv171 *_goml_vec_Array_2_5int32
    var t172 *_goml_vec_Array_2_5int32 = vec_new__Vec_14Array_2_5int32()
    retv171 = t172
    return retv171
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T___l_int32_x3b_2_r_(self__99 *_goml_vec_Array_2_5int32, elem__100 [2]int32) *_goml_vec_Array_2_5int32 {
    var retv174 *_goml_vec_Array_2_5int32
    var result__101 *_goml_vec_Array_2_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___l_int32_x3b_2_r_()
    var index__102 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop176:
    for {
        var t177 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__102)
        var t178 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___l_int32_x3b_2_r_(self__99)
        var t179 bool = t177 < t178
        if t179 {
            var t180 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__102)
            var t181 [2]int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T___l_int32_x3b_2_r_(self__99, t180)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___l_int32_x3b_2_r_(result__101, t181)
            var t182 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__102)
            var t183 int32 = t182 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__102, t183)
            continue
        } else {
            break Loop_loop176
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___l_int32_x3b_2_r_(result__101, elem__100)
    retv174 = result__101
    return retv174
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T___l_int32_x3b_2_r_(value__140 [2]int32) *ref_Array_2_5int32_x {
    var retv185 *ref_Array_2_5int32_x
    var t186 *ref_Array_2_5int32_x = ref__Ref_14Array_2_5int32(value__140)
    retv185 = t186
    return retv185
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T___l_int32_x3b_2_r_(self__141 *ref_Array_2_5int32_x) [2]int32 {
    var retv188 [2]int32
    var t189 [2]int32 = ref_get__Ref_14Array_2_5int32(self__141)
    retv188 = t189
    return retv188
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv191 string
    retv191 = self__9
    return retv191
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv193 string
    var t194 string = _goml_runtime_core_int32_to_string(self__13)
    retv193 = t194
    return retv193
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__140 int32) *ref_int32_x {
    var retv196 *ref_int32_x
    var t197 *ref_int32_x = ref__Ref_5int32(value__140)
    retv196 = t197
    return retv196
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__141 *ref_int32_x) int32 {
    var retv199 int32
    var t200 int32 = ref_get__Ref_5int32(self__141)
    retv199 = t200
    return retv199
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__108 *_goml_vec_int32) int32 {
    var retv202 int32
    var t203 int32 = vec_len__Vec_5int32(self__108)
    retv202 = t203
    return retv202
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__97 *_goml_vec_int32, elem__98 int32) struct{} {
    vec_push__Vec_5int32(self__97, elem__98)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__103 *_goml_vec_int32, index__104 int32) int32 {
    var retv207 int32
    var t208 int32 = vec_get__Vec_5int32(self__103, index__104)
    retv207 = t208
    return retv207
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__142 *ref_int32_x, value__143 int32) struct{} {
    ref_set__Ref_5int32(self__142, value__143)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___l_int32_x3b_2_r_(self__108 *_goml_vec_Array_2_5int32) int32 {
    var retv212 int32
    var t213 int32 = vec_len__Vec_14Array_2_5int32(self__108)
    retv212 = t213
    return retv212
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___l_int32_x3b_2_r_(self__97 *_goml_vec_Array_2_5int32, elem__98 [2]int32) struct{} {
    vec_push__Vec_14Array_2_5int32(self__97, elem__98)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T___l_int32_x3b_2_r_(self__103 *_goml_vec_Array_2_5int32, index__104 int32) [2]int32 {
    var retv217 [2]int32
    var t218 [2]int32 = vec_get__Vec_14Array_2_5int32(self__103, index__104)
    retv217 = t218
    return retv217
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__25 string, other__26 string) bool {
    var retv220 bool
    var t221 bool = self__25 == other__26
    retv220 = t221
    return retv220
}

func _goml_m_trait__impl_i_Hash_i_string_i_hash(self__51 string) uint64 {
    var retv223 uint64
    var t224 uint64 = _goml_runtime_core_string_hash(self__51)
    retv223 = t224
    return retv223
}

func main() {
    main0()
}
