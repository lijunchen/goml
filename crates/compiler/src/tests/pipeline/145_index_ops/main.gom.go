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
    hashes []uint64
    len int32
}

func hashmap_new__HashMap_6string_5int32() *hashmap_string_int32_x {
    return &hashmap_string_int32_x{
        buckets: make(map[uint64][]hashmap_string_int32_x_entry),
        len: 0,
        hashes: nil,
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
    var reuse_index int32 = -1
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_string_i_hash(key)
    var bucket []hashmap_string_int32_x_entry = m.buckets[h]
    if len(bucket) == 0 {
        m.hashes = append(m.hashes, h)
    }
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
        if !entry.active && reuse_index < 0 {
            reuse_index = i
        }
        i = i + 1
    }
    if reuse_index >= 0 {
        bucket[reuse_index] = hashmap_string_int32_x_entry{
            active: true,
            key: key,
            value: value,
        }
        m.len = m.len + 1
        return struct{}{}
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
        var x61 int32 = x__0.(Some)._0
        var v__1 int32 = x61
        println__T_int32(v__1)
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func main0() struct{} {
    var t128 [2]int32 = [2]int32{31, 32}
    var t129 int32 = array_get__Array_2_5int32(t128, 1)
    println__T_int32(t129)
    var arr__2 [3]int32 = [3]int32{1, 2, 3}
    var t130 int32 = array_get__Array_3_5int32(arr__2, 0)
    println__T_int32(t130)
    var arr2__3 [3]int32 = [3]int32{4, 5, 6}
    var place_root64 [3]int32 = arr2__3
    var index65 int32 = 1
    array_get__Array_3_5int32(place_root64, index65)
    var value67 int32 = 50
    var t131 [3]int32 = array_set__Array_3_5int32(place_root64, index65, value67)
    arr2__3 = t131
    var t133 int32 = array_get__Array_3_5int32(arr2__3, 1)
    println__T_int32(t133)
    var t134 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    var t135 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t134, 7)
    var t136 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t135, 8)
    var vec__4 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t136, 9)
    var t137 int32 = vec_get__Vec_5int32(vec__4, 2)
    println__T_int32(t137)
    var t138 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    var t139 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t138, 10)
    var t140 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t139, 11)
    var vec2__5 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t140, 12)
    var place_root71 *_goml_vec_int32 = vec2__5
    var index72 int32 = 0
    vec_get__Vec_5int32(place_root71, index72)
    var value74 int32 = 100
    vec_set__Vec_5int32(place_root71, index72, value74)
    var t142 int32 = vec_get__Vec_5int32(vec2__5, 0)
    println__T_int32(t142)
    var s__6 []int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(vec2__5, 0, 2)
    var t143 int32 = s__6[1]
    println__T_int32(t143)
    var map__7 *hashmap_string_int32_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32()
    var place_root78 *hashmap_string_int32_x = map__7
    var index79 string = "a"
    hashmap_get__HashMap_6string_5int32(place_root78, index79)
    var value81 int32 = 13
    hashmap_set__HashMap_6string_5int32(place_root78, index79, value81)
    var t145 Option__int32 = hashmap_get__HashMap_6string_5int32(map__7, "a")
    print_opt_int(t145)
    var t146 Option__int32 = hashmap_get__HashMap_6string_5int32(map__7, "missing")
    print_opt_int(t146)
    var t147 [2]int32 = [2]int32{1, 2}
    var t148 [2]int32 = [2]int32{3, 4}
    var matrix__8 [2][2]int32 = [2][2]int32{t147, t148}
    var place_root85 [2][2]int32 = matrix__8
    var index86 int32 = 1
    var place87 [2]int32 = array_get__Array_2_14Array_2_5int32(place_root85, index86)
    var index88 int32 = 0
    array_get__Array_2_5int32(place87, index88)
    var value90 int32 = 30
    var t149 [2]int32 = array_set__Array_2_5int32(place87, index88, value90)
    var t150 [2][2]int32 = array_set__Array_2_14Array_2_5int32(place_root85, index86, t149)
    matrix__8 = t150
    var t152 [2]int32 = array_get__Array_2_14Array_2_5int32(matrix__8, 1)
    var t153 int32 = array_get__Array_2_5int32(t152, 0)
    println__T_int32(t153)
    var t154 [2]int32 = [2]int32{14, 15}
    var pair__9 Tuple2_13Array2_5int32_5int32 = Tuple2_13Array2_5int32_5int32{
        _0: t154,
        _1: 0,
    }
    var place_root93 Tuple2_13Array2_5int32_5int32 = pair__9
    var place94 [2]int32 = place_root93._0
    var index95 int32 = 1
    array_get__Array_2_5int32(place94, index95)
    var value97 int32 = 150
    var t155 [2]int32 = array_set__Array_2_5int32(place94, index95, value97)
    var t156 int32 = place_root93._1
    var t157 Tuple2_13Array2_5int32_5int32 = Tuple2_13Array2_5int32_5int32{
        _0: t155,
        _1: t156,
    }
    pair__9 = t157
    var t159 [2]int32 = pair__9._0
    var t160 int32 = array_get__Array_2_5int32(t159, 1)
    println__T_int32(t160)
    var t161 [2]int32 = [2]int32{16, 17}
    var t162 *_goml_vec_Array_2_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___l_int32_x3b_2_r_()
    var t163 [2]int32 = [2]int32{18, 19}
    var t164 *_goml_vec_Array_2_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T___l_int32_x3b_2_r_(t162, t163)
    var t165 [2]int32 = [2]int32{20, 21}
    var t166 *_goml_vec_Array_2_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T___l_int32_x3b_2_r_(t164, t165)
    var holder__10 Holder = Holder{
        data: t161,
        vecs: t166,
    }
    var place_root100 Holder = holder__10
    var place101 [2]int32 = place_root100.data
    var index102 int32 = 0
    array_get__Array_2_5int32(place101, index102)
    var value104 int32 = 160
    var t167 [2]int32 = array_set__Array_2_5int32(place101, index102, value104)
    var t168 *_goml_vec_Array_2_5int32 = place_root100.vecs
    var t169 Holder = Holder{
        data: t167,
        vecs: t168,
    }
    holder__10 = t169
    var t171 [2]int32 = holder__10.data
    var t172 int32 = array_get__Array_2_5int32(t171, 0)
    println__T_int32(t172)
    var place_root107 Holder = holder__10
    var place108 *_goml_vec_Array_2_5int32 = place_root107.vecs
    var index109 int32 = 1
    var place110 [2]int32 = vec_get__Vec_14Array_2_5int32(place108, index109)
    var index111 int32 = 0
    array_get__Array_2_5int32(place110, index111)
    var value113 int32 = 200
    var t173 [2]int32 = array_set__Array_2_5int32(place110, index111, value113)
    vec_set__Vec_14Array_2_5int32(place108, index109, t173)
    var t175 *_goml_vec_Array_2_5int32 = holder__10.vecs
    var t176 [2]int32 = vec_get__Vec_14Array_2_5int32(t175, 1)
    var t177 int32 = array_get__Array_2_5int32(t176, 0)
    println__T_int32(t177)
    var t178 [2]int32 = [2]int32{22, 23}
    var r__11 *ref_Array_2_5int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T___l_int32_x3b_2_r_(t178)
    var place_ref116 *ref_Array_2_5int32_x = r__11
    var place_root117 [2]int32 = ref_get__Ref_14Array_2_5int32(place_ref116)
    var index118 int32 = 1
    array_get__Array_2_5int32(place_root117, index118)
    var value120 int32 = 230
    var t179 [2]int32 = array_set__Array_2_5int32(place_root117, index118, value120)
    ref_set__Ref_14Array_2_5int32(place_ref116, t179)
    var t181 [2]int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T___l_int32_x3b_2_r_(r__11)
    var t182 int32 = array_get__Array_2_5int32(t181, 1)
    println__T_int32(t182)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t184 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t184)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t187 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t187)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv190 *_goml_vec_int32
    var t191 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv190 = t191
    return retv190
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(self__125 *_goml_vec_int32, elem__126 int32) *_goml_vec_int32 {
    var retv193 *_goml_vec_int32
    var result__127 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    var index__128 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop195:
    for {
        var t196 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
        var t197 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__125)
        var t198 bool = t196 < t197
        if t198 {
            var t199 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
            var t200 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__125, t199)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(result__127, t200)
            var t201 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
            var t202 int32 = t201 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__128, t202)
            continue
        } else {
            break Loop_loop195
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(result__127, elem__126)
    retv193 = result__127
    return retv193
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(self__172 *_goml_vec_int32, start__173 int32, end__174 int32) []int32 {
    var retv204 []int32
    var t205 []int32 = self__172.items[start__173:end__174]
    retv204 = t205
    return retv204
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32() *hashmap_string_int32_x {
    var retv207 *hashmap_string_int32_x
    var t208 *hashmap_string_int32_x = hashmap_new__HashMap_6string_5int32()
    retv207 = t208
    return retv207
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___l_int32_x3b_2_r_() *_goml_vec_Array_2_5int32 {
    var retv210 *_goml_vec_Array_2_5int32
    var t211 *_goml_vec_Array_2_5int32 = vec_new__Vec_14Array_2_5int32()
    retv210 = t211
    return retv210
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T___l_int32_x3b_2_r_(self__125 *_goml_vec_Array_2_5int32, elem__126 [2]int32) *_goml_vec_Array_2_5int32 {
    var retv213 *_goml_vec_Array_2_5int32
    var result__127 *_goml_vec_Array_2_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___l_int32_x3b_2_r_()
    var index__128 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop215:
    for {
        var t216 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
        var t217 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___l_int32_x3b_2_r_(self__125)
        var t218 bool = t216 < t217
        if t218 {
            var t219 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
            var t220 [2]int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T___l_int32_x3b_2_r_(self__125, t219)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___l_int32_x3b_2_r_(result__127, t220)
            var t221 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
            var t222 int32 = t221 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__128, t222)
            continue
        } else {
            break Loop_loop215
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___l_int32_x3b_2_r_(result__127, elem__126)
    retv213 = result__127
    return retv213
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T___l_int32_x3b_2_r_(value__204 [2]int32) *ref_Array_2_5int32_x {
    var retv224 *ref_Array_2_5int32_x
    var t225 *ref_Array_2_5int32_x = ref__Ref_14Array_2_5int32(value__204)
    retv224 = t225
    return retv224
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T___l_int32_x3b_2_r_(self__205 *ref_Array_2_5int32_x) [2]int32 {
    var retv227 [2]int32
    var t228 [2]int32 = ref_get__Ref_14Array_2_5int32(self__205)
    retv227 = t228
    return retv227
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv230 string
    retv230 = self__37
    return retv230
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__41 int32) string {
    var retv232 string
    var t233 string = _goml_runtime_core_int32_to_string(self__41)
    retv232 = t233
    return retv232
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__204 int32) *ref_int32_x {
    var retv235 *ref_int32_x
    var t236 *ref_int32_x = ref__Ref_5int32(value__204)
    retv235 = t236
    return retv235
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__205 *ref_int32_x) int32 {
    var retv238 int32
    var t239 int32 = ref_get__Ref_5int32(self__205)
    retv238 = t239
    return retv238
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__134 *_goml_vec_int32) int32 {
    var retv241 int32
    var t242 int32 = vec_len__Vec_5int32(self__134)
    retv241 = t242
    return retv241
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__123 *_goml_vec_int32, elem__124 int32) struct{} {
    vec_push__Vec_5int32(self__123, elem__124)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__129 *_goml_vec_int32, index__130 int32) int32 {
    var retv246 int32
    var t247 int32 = vec_get__Vec_5int32(self__129, index__130)
    retv246 = t247
    return retv246
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__206 *ref_int32_x, value__207 int32) struct{} {
    ref_set__Ref_5int32(self__206, value__207)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___l_int32_x3b_2_r_(self__134 *_goml_vec_Array_2_5int32) int32 {
    var retv251 int32
    var t252 int32 = vec_len__Vec_14Array_2_5int32(self__134)
    retv251 = t252
    return retv251
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___l_int32_x3b_2_r_(self__123 *_goml_vec_Array_2_5int32, elem__124 [2]int32) struct{} {
    vec_push__Vec_14Array_2_5int32(self__123, elem__124)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T___l_int32_x3b_2_r_(self__129 *_goml_vec_Array_2_5int32, index__130 int32) [2]int32 {
    var retv256 [2]int32
    var t257 [2]int32 = vec_get__Vec_14Array_2_5int32(self__129, index__130)
    retv256 = t257
    return retv256
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__53 string, other__54 string) bool {
    var retv259 bool
    var t260 bool = self__53 == other__54
    retv259 = t260
    return retv259
}

func _goml_m_trait__impl_i_Hash_i_string_i_hash(self__79 string) uint64 {
    var retv262 uint64
    var t263 uint64 = _goml_runtime_core_string_hash(self__79)
    retv262 = t263
    return retv262
}

func main() {
    main0()
}
