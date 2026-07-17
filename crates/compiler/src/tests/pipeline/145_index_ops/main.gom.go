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
        var x58 int32 = x__0.(Some)._0
        var v__1 int32 = x58
        println__T_int32(v__1)
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func main0() struct{} {
    var t125 [2]int32 = [2]int32{31, 32}
    var t126 int32 = array_get__Array_2_5int32(t125, 1)
    println__T_int32(t126)
    var arr__2 [3]int32 = [3]int32{1, 2, 3}
    var t127 int32 = array_get__Array_3_5int32(arr__2, 0)
    println__T_int32(t127)
    var arr2__3 [3]int32 = [3]int32{4, 5, 6}
    var place_root61 [3]int32 = arr2__3
    var index62 int32 = 1
    array_get__Array_3_5int32(place_root61, index62)
    var value64 int32 = 50
    var t128 [3]int32 = array_set__Array_3_5int32(place_root61, index62, value64)
    arr2__3 = t128
    var t130 int32 = array_get__Array_3_5int32(arr2__3, 1)
    println__T_int32(t130)
    var t131 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    var t132 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t131, 7)
    var t133 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t132, 8)
    var vec__4 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t133, 9)
    var t134 int32 = vec_get__Vec_5int32(vec__4, 2)
    println__T_int32(t134)
    var t135 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    var t136 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t135, 10)
    var t137 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t136, 11)
    var vec2__5 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t137, 12)
    var place_root68 *_goml_vec_int32 = vec2__5
    var index69 int32 = 0
    vec_get__Vec_5int32(place_root68, index69)
    var value71 int32 = 100
    vec_set__Vec_5int32(place_root68, index69, value71)
    var t139 int32 = vec_get__Vec_5int32(vec2__5, 0)
    println__T_int32(t139)
    var s__6 []int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(vec2__5, 0, 2)
    var t140 int32 = s__6[1]
    println__T_int32(t140)
    var map__7 *hashmap_string_int32_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32()
    var place_root75 *hashmap_string_int32_x = map__7
    var index76 string = "a"
    hashmap_get__HashMap_6string_5int32(place_root75, index76)
    var value78 int32 = 13
    hashmap_set__HashMap_6string_5int32(place_root75, index76, value78)
    var t142 Option__int32 = hashmap_get__HashMap_6string_5int32(map__7, "a")
    print_opt_int(t142)
    var t143 Option__int32 = hashmap_get__HashMap_6string_5int32(map__7, "missing")
    print_opt_int(t143)
    var t144 [2]int32 = [2]int32{1, 2}
    var t145 [2]int32 = [2]int32{3, 4}
    var matrix__8 [2][2]int32 = [2][2]int32{t144, t145}
    var place_root82 [2][2]int32 = matrix__8
    var index83 int32 = 1
    var place84 [2]int32 = array_get__Array_2_14Array_2_5int32(place_root82, index83)
    var index85 int32 = 0
    array_get__Array_2_5int32(place84, index85)
    var value87 int32 = 30
    var t146 [2]int32 = array_set__Array_2_5int32(place84, index85, value87)
    var t147 [2][2]int32 = array_set__Array_2_14Array_2_5int32(place_root82, index83, t146)
    matrix__8 = t147
    var t149 [2]int32 = array_get__Array_2_14Array_2_5int32(matrix__8, 1)
    var t150 int32 = array_get__Array_2_5int32(t149, 0)
    println__T_int32(t150)
    var t151 [2]int32 = [2]int32{14, 15}
    var pair__9 Tuple2_13Array2_5int32_5int32 = Tuple2_13Array2_5int32_5int32{
        _0: t151,
        _1: 0,
    }
    var place_root90 Tuple2_13Array2_5int32_5int32 = pair__9
    var place91 [2]int32 = place_root90._0
    var index92 int32 = 1
    array_get__Array_2_5int32(place91, index92)
    var value94 int32 = 150
    var t152 [2]int32 = array_set__Array_2_5int32(place91, index92, value94)
    var t153 int32 = place_root90._1
    var t154 Tuple2_13Array2_5int32_5int32 = Tuple2_13Array2_5int32_5int32{
        _0: t152,
        _1: t153,
    }
    pair__9 = t154
    var t156 [2]int32 = pair__9._0
    var t157 int32 = array_get__Array_2_5int32(t156, 1)
    println__T_int32(t157)
    var t158 [2]int32 = [2]int32{16, 17}
    var t159 *_goml_vec_Array_2_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___l_int32_x3b_2_r_()
    var t160 [2]int32 = [2]int32{18, 19}
    var t161 *_goml_vec_Array_2_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T___l_int32_x3b_2_r_(t159, t160)
    var t162 [2]int32 = [2]int32{20, 21}
    var t163 *_goml_vec_Array_2_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T___l_int32_x3b_2_r_(t161, t162)
    var holder__10 Holder = Holder{
        data: t158,
        vecs: t163,
    }
    var place_root97 Holder = holder__10
    var place98 [2]int32 = place_root97.data
    var index99 int32 = 0
    array_get__Array_2_5int32(place98, index99)
    var value101 int32 = 160
    var t164 [2]int32 = array_set__Array_2_5int32(place98, index99, value101)
    var t165 *_goml_vec_Array_2_5int32 = place_root97.vecs
    var t166 Holder = Holder{
        data: t164,
        vecs: t165,
    }
    holder__10 = t166
    var t168 [2]int32 = holder__10.data
    var t169 int32 = array_get__Array_2_5int32(t168, 0)
    println__T_int32(t169)
    var place_root104 Holder = holder__10
    var place105 *_goml_vec_Array_2_5int32 = place_root104.vecs
    var index106 int32 = 1
    var place107 [2]int32 = vec_get__Vec_14Array_2_5int32(place105, index106)
    var index108 int32 = 0
    array_get__Array_2_5int32(place107, index108)
    var value110 int32 = 200
    var t170 [2]int32 = array_set__Array_2_5int32(place107, index108, value110)
    vec_set__Vec_14Array_2_5int32(place105, index106, t170)
    var t172 *_goml_vec_Array_2_5int32 = holder__10.vecs
    var t173 [2]int32 = vec_get__Vec_14Array_2_5int32(t172, 1)
    var t174 int32 = array_get__Array_2_5int32(t173, 0)
    println__T_int32(t174)
    var t175 [2]int32 = [2]int32{22, 23}
    var r__11 *ref_Array_2_5int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T___l_int32_x3b_2_r_(t175)
    var place_ref113 *ref_Array_2_5int32_x = r__11
    var place_root114 [2]int32 = ref_get__Ref_14Array_2_5int32(place_ref113)
    var index115 int32 = 1
    array_get__Array_2_5int32(place_root114, index115)
    var value117 int32 = 230
    var t176 [2]int32 = array_set__Array_2_5int32(place_root114, index115, value117)
    ref_set__Ref_14Array_2_5int32(place_ref113, t176)
    var t178 [2]int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T___l_int32_x3b_2_r_(r__11)
    var t179 int32 = array_get__Array_2_5int32(t178, 1)
    println__T_int32(t179)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t181 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t181)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t184 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t184)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv187 *_goml_vec_int32
    var t188 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv187 = t188
    return retv187
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(self__122 *_goml_vec_int32, elem__123 int32) *_goml_vec_int32 {
    var retv190 *_goml_vec_int32
    var result__124 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    var index__125 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop192:
    for {
        var t193 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__125)
        var t194 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__122)
        var t195 bool = t193 < t194
        if t195 {
            var t196 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__125)
            var t197 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__122, t196)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(result__124, t197)
            var t198 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__125)
            var t199 int32 = t198 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__125, t199)
            continue
        } else {
            break Loop_loop192
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(result__124, elem__123)
    retv190 = result__124
    return retv190
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(self__168 *_goml_vec_int32, start__169 int32, end__170 int32) []int32 {
    var retv201 []int32
    var t202 []int32 = self__168.items[start__169:end__170]
    retv201 = t202
    return retv201
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32() *hashmap_string_int32_x {
    var retv204 *hashmap_string_int32_x
    var t205 *hashmap_string_int32_x = hashmap_new__HashMap_6string_5int32()
    retv204 = t205
    return retv204
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___l_int32_x3b_2_r_() *_goml_vec_Array_2_5int32 {
    var retv207 *_goml_vec_Array_2_5int32
    var t208 *_goml_vec_Array_2_5int32 = vec_new__Vec_14Array_2_5int32()
    retv207 = t208
    return retv207
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T___l_int32_x3b_2_r_(self__122 *_goml_vec_Array_2_5int32, elem__123 [2]int32) *_goml_vec_Array_2_5int32 {
    var retv210 *_goml_vec_Array_2_5int32
    var result__124 *_goml_vec_Array_2_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___l_int32_x3b_2_r_()
    var index__125 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop212:
    for {
        var t213 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__125)
        var t214 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___l_int32_x3b_2_r_(self__122)
        var t215 bool = t213 < t214
        if t215 {
            var t216 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__125)
            var t217 [2]int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T___l_int32_x3b_2_r_(self__122, t216)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___l_int32_x3b_2_r_(result__124, t217)
            var t218 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__125)
            var t219 int32 = t218 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__125, t219)
            continue
        } else {
            break Loop_loop212
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___l_int32_x3b_2_r_(result__124, elem__123)
    retv210 = result__124
    return retv210
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T___l_int32_x3b_2_r_(value__200 [2]int32) *ref_Array_2_5int32_x {
    var retv221 *ref_Array_2_5int32_x
    var t222 *ref_Array_2_5int32_x = ref__Ref_14Array_2_5int32(value__200)
    retv221 = t222
    return retv221
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T___l_int32_x3b_2_r_(self__201 *ref_Array_2_5int32_x) [2]int32 {
    var retv224 [2]int32
    var t225 [2]int32 = ref_get__Ref_14Array_2_5int32(self__201)
    retv224 = t225
    return retv224
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv227 string
    retv227 = self__34
    return retv227
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__38 int32) string {
    var retv229 string
    var t230 string = _goml_runtime_core_int32_to_string(self__38)
    retv229 = t230
    return retv229
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__200 int32) *ref_int32_x {
    var retv232 *ref_int32_x
    var t233 *ref_int32_x = ref__Ref_5int32(value__200)
    retv232 = t233
    return retv232
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__201 *ref_int32_x) int32 {
    var retv235 int32
    var t236 int32 = ref_get__Ref_5int32(self__201)
    retv235 = t236
    return retv235
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__131 *_goml_vec_int32) int32 {
    var retv238 int32
    var t239 int32 = vec_len__Vec_5int32(self__131)
    retv238 = t239
    return retv238
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__120 *_goml_vec_int32, elem__121 int32) struct{} {
    vec_push__Vec_5int32(self__120, elem__121)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__126 *_goml_vec_int32, index__127 int32) int32 {
    var retv243 int32
    var t244 int32 = vec_get__Vec_5int32(self__126, index__127)
    retv243 = t244
    return retv243
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__202 *ref_int32_x, value__203 int32) struct{} {
    ref_set__Ref_5int32(self__202, value__203)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___l_int32_x3b_2_r_(self__131 *_goml_vec_Array_2_5int32) int32 {
    var retv248 int32
    var t249 int32 = vec_len__Vec_14Array_2_5int32(self__131)
    retv248 = t249
    return retv248
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___l_int32_x3b_2_r_(self__120 *_goml_vec_Array_2_5int32, elem__121 [2]int32) struct{} {
    vec_push__Vec_14Array_2_5int32(self__120, elem__121)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T___l_int32_x3b_2_r_(self__126 *_goml_vec_Array_2_5int32, index__127 int32) [2]int32 {
    var retv253 [2]int32
    var t254 [2]int32 = vec_get__Vec_14Array_2_5int32(self__126, index__127)
    retv253 = t254
    return retv253
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__50 string, other__51 string) bool {
    var retv256 bool
    var t257 bool = self__50 == other__51
    retv256 = t257
    return retv256
}

func _goml_m_trait__impl_i_Hash_i_string_i_hash(self__76 string) uint64 {
    var retv259 uint64
    var t260 uint64 = _goml_runtime_core_string_hash(self__76)
    retv259 = t260
    return retv259
}

func main() {
    main0()
}
