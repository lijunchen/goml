package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_hash(s string) uint64 {
    var h uint64 = 14695981039346656037
    var i int = 0
    for {
        if i >= int(len(s)) {
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

func array_get__Array_2_3int(arr [2]int, index int) int {
    return arr[index]
}

func array_set__Array_2_3int(arr [2]int, index int, value int) [2]int {
    arr[index] = value
    return arr
}

func array_get__Array_3_3int(arr [3]int, index int) int {
    return arr[index]
}

func array_set__Array_3_3int(arr [3]int, index int, value int) [3]int {
    arr[index] = value
    return arr
}

func array_get__Array_2_5int32(arr [2]int32, index int) int32 {
    return arr[index]
}

func array_set__Array_2_5int32(arr [2]int32, index int, value int32) [2]int32 {
    arr[index] = value
    return arr
}

func array_get__Array_2_14Array_2_5int32(arr [2][2]int32, index int) [2]int32 {
    return arr[index]
}

func array_set__Array_2_14Array_2_5int32(arr [2][2]int32, index int, value [2]int32) [2][2]int32 {
    arr[index] = value
    return arr
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

func vec_get__Vec_5int32(vec *_goml_vec_int32, index int) int32 {
    return vec.items[index]
}

func vec_set__Vec_5int32(vec *_goml_vec_int32, index int, value int32) struct{} {
    vec.items[index] = value
    return struct{}{}
}

func vec_len__Vec_5int32(vec *_goml_vec_int32) int {
    return int(len(vec.items))
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

func vec_get__Vec_14Array_2_5int32(vec *_goml_vec_Array_2_5int32, index int) [2]int32 {
    return vec.items[index]
}

func vec_set__Vec_14Array_2_5int32(vec *_goml_vec_Array_2_5int32, index int, value [2]int32) struct{} {
    vec.items[index] = value
    return struct{}{}
}

func vec_len__Vec_14Array_2_5int32(vec *_goml_vec_Array_2_5int32) int {
    return int(len(vec.items))
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

type ref_int_x struct {
    value int
}

func ref__Ref_3int(value int) *ref_int_x {
    return &ref_int_x{
        value: value,
    }
}

func ref_get__Ref_3int(reference *ref_int_x) int {
    return reference.value
}

func ref_set__Ref_3int(reference *ref_int_x, value int) struct{} {
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
    len int
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
    var i int = 0
    for {
        if i >= int(len(bucket)) {
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
    var reuse_index int = -1
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_string_i_hash(key)
    var bucket []hashmap_string_int32_x_entry = m.buckets[h]
    if len(bucket) == 0 {
        m.hashes = append(m.hashes, h)
    }
    var i int = 0
    for {
        if i >= int(len(bucket)) {
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

type Tuple2_11Array2_3int_3int struct {
    _0 [2]int
    _1 int
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
        var x108 int32 = x__0.(Some)._0
        var v__1 int32 = x108
        println__T_int32(v__1)
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func main0() struct{} {
    var t175 [2]int = [2]int{31, 32}
    var t176 int = array_get__Array_2_3int(t175, 1)
    println__T_int(t176)
    var arr__2 [3]int = [3]int{1, 2, 3}
    var t177 int = array_get__Array_3_3int(arr__2, 0)
    println__T_int(t177)
    var arr2__3 [3]int = [3]int{4, 5, 6}
    var place_root111 [3]int = arr2__3
    var index112 int = 1
    array_get__Array_3_3int(place_root111, index112)
    var value114 int = 50
    var t178 [3]int = array_set__Array_3_3int(place_root111, index112, value114)
    arr2__3 = t178
    var t180 int = array_get__Array_3_3int(arr2__3, 1)
    println__T_int(t180)
    var t181 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int()
    var t182 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int(t181, 7)
    var t183 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int(t182, 8)
    var vec__4 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int(t183, 9)
    var t184 int = vec_get__Vec_3int(vec__4, 2)
    println__T_int(t184)
    var t185 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    var t186 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t185, 10)
    var t187 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t186, 11)
    var vec2__5 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t187, 12)
    var place_root118 *_goml_vec_int32 = vec2__5
    var index119 int = 0
    vec_get__Vec_5int32(place_root118, index119)
    var value121 int32 = 100
    vec_set__Vec_5int32(place_root118, index119, value121)
    var t189 int32 = vec_get__Vec_5int32(vec2__5, 0)
    println__T_int32(t189)
    var s__6 []int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(vec2__5, 0, 2)
    var t190 int32 = s__6[1]
    println__T_int32(t190)
    var map__7 *hashmap_string_int32_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32()
    var place_root125 *hashmap_string_int32_x = map__7
    var index126 string = "a"
    hashmap_get__HashMap_6string_5int32(place_root125, index126)
    var value128 int32 = 13
    hashmap_set__HashMap_6string_5int32(place_root125, index126, value128)
    var t192 Option__int32 = hashmap_get__HashMap_6string_5int32(map__7, "a")
    print_opt_int(t192)
    var t193 Option__int32 = hashmap_get__HashMap_6string_5int32(map__7, "missing")
    print_opt_int(t193)
    var t194 [2]int32 = [2]int32{1, 2}
    var t195 [2]int32 = [2]int32{3, 4}
    var matrix__8 [2][2]int32 = [2][2]int32{t194, t195}
    var place_root132 [2][2]int32 = matrix__8
    var index133 int = 1
    var place134 [2]int32 = array_get__Array_2_14Array_2_5int32(place_root132, index133)
    var index135 int = 0
    array_get__Array_2_5int32(place134, index135)
    var value137 int32 = 30
    var t196 [2]int32 = array_set__Array_2_5int32(place134, index135, value137)
    var t197 [2][2]int32 = array_set__Array_2_14Array_2_5int32(place_root132, index133, t196)
    matrix__8 = t197
    var t199 [2]int32 = array_get__Array_2_14Array_2_5int32(matrix__8, 1)
    var t200 int32 = array_get__Array_2_5int32(t199, 0)
    println__T_int32(t200)
    var t201 [2]int = [2]int{14, 15}
    var pair__9 Tuple2_11Array2_3int_3int = Tuple2_11Array2_3int_3int{
        _0: t201,
        _1: 0,
    }
    var place_root140 Tuple2_11Array2_3int_3int = pair__9
    var place141 [2]int = place_root140._0
    var index142 int = 1
    array_get__Array_2_3int(place141, index142)
    var value144 int = 150
    var t202 [2]int = array_set__Array_2_3int(place141, index142, value144)
    var t203 int = place_root140._1
    var t204 Tuple2_11Array2_3int_3int = Tuple2_11Array2_3int_3int{
        _0: t202,
        _1: t203,
    }
    pair__9 = t204
    var t206 [2]int = pair__9._0
    var t207 int = array_get__Array_2_3int(t206, 1)
    println__T_int(t207)
    var t208 [2]int32 = [2]int32{16, 17}
    var t209 *_goml_vec_Array_2_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___l_int32_x3b_2_r_()
    var t210 [2]int32 = [2]int32{18, 19}
    var t211 *_goml_vec_Array_2_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T___l_int32_x3b_2_r_(t209, t210)
    var t212 [2]int32 = [2]int32{20, 21}
    var t213 *_goml_vec_Array_2_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T___l_int32_x3b_2_r_(t211, t212)
    var holder__10 Holder = Holder{
        data: t208,
        vecs: t213,
    }
    var place_root147 Holder = holder__10
    var place148 [2]int32 = place_root147.data
    var index149 int = 0
    array_get__Array_2_5int32(place148, index149)
    var value151 int32 = 160
    var t214 [2]int32 = array_set__Array_2_5int32(place148, index149, value151)
    var t215 *_goml_vec_Array_2_5int32 = place_root147.vecs
    var t216 Holder = Holder{
        data: t214,
        vecs: t215,
    }
    holder__10 = t216
    var t218 [2]int32 = holder__10.data
    var t219 int32 = array_get__Array_2_5int32(t218, 0)
    println__T_int32(t219)
    var place_root154 Holder = holder__10
    var place155 *_goml_vec_Array_2_5int32 = place_root154.vecs
    var index156 int = 1
    var place157 [2]int32 = vec_get__Vec_14Array_2_5int32(place155, index156)
    var index158 int = 0
    array_get__Array_2_5int32(place157, index158)
    var value160 int32 = 200
    var t220 [2]int32 = array_set__Array_2_5int32(place157, index158, value160)
    vec_set__Vec_14Array_2_5int32(place155, index156, t220)
    var t222 *_goml_vec_Array_2_5int32 = holder__10.vecs
    var t223 [2]int32 = vec_get__Vec_14Array_2_5int32(t222, 1)
    var t224 int32 = array_get__Array_2_5int32(t223, 0)
    println__T_int32(t224)
    var t225 [2]int32 = [2]int32{22, 23}
    var r__11 *ref_Array_2_5int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T___l_int32_x3b_2_r_(t225)
    var place_ref163 *ref_Array_2_5int32_x = r__11
    var place_root164 [2]int32 = ref_get__Ref_14Array_2_5int32(place_ref163)
    var index165 int = 1
    array_get__Array_2_5int32(place_root164, index165)
    var value167 int32 = 230
    var t226 [2]int32 = array_set__Array_2_5int32(place_root164, index165, value167)
    ref_set__Ref_14Array_2_5int32(place_ref163, t226)
    var t228 [2]int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T___l_int32_x3b_2_r_(r__11)
    var t229 int32 = array_get__Array_2_5int32(t228, 1)
    println__T_int32(t229)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t231 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t231)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t234 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t234)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t237 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t237)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int() *_goml_vec_int {
    var retv240 *_goml_vec_int
    var t241 *_goml_vec_int = vec_new__Vec_3int()
    retv240 = t241
    return retv240
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int(self__128 *_goml_vec_int, elem__129 int) *_goml_vec_int {
    var retv243 *_goml_vec_int
    var result__130 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int()
    var index__131 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop245:
    for {
        var t246 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
        var t247 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int(self__128)
        var t248 bool = t246 < t247
        if t248 {
            var t249 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
            var t250 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(self__128, t249)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(result__130, t250)
            var t251 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
            var t252 int = t251 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__131, t252)
            continue
        } else {
            break Loop_loop245
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(result__130, elem__129)
    retv243 = result__130
    return retv243
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv254 *_goml_vec_int32
    var t255 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv254 = t255
    return retv254
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(self__128 *_goml_vec_int32, elem__129 int32) *_goml_vec_int32 {
    var retv257 *_goml_vec_int32
    var result__130 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    var index__131 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop259:
    for {
        var t260 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
        var t261 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__128)
        var t262 bool = t260 < t261
        if t262 {
            var t263 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
            var t264 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__128, t263)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(result__130, t264)
            var t265 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
            var t266 int = t265 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__131, t266)
            continue
        } else {
            break Loop_loop259
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(result__130, elem__129)
    retv257 = result__130
    return retv257
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(self__175 *_goml_vec_int32, start__176 int, end__177 int) []int32 {
    var retv268 []int32
    var t269 []int32 = self__175.items[start__176:end__177]
    retv268 = t269
    return retv268
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32() *hashmap_string_int32_x {
    var retv271 *hashmap_string_int32_x
    var t272 *hashmap_string_int32_x = hashmap_new__HashMap_6string_5int32()
    retv271 = t272
    return retv271
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___l_int32_x3b_2_r_() *_goml_vec_Array_2_5int32 {
    var retv274 *_goml_vec_Array_2_5int32
    var t275 *_goml_vec_Array_2_5int32 = vec_new__Vec_14Array_2_5int32()
    retv274 = t275
    return retv274
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T___l_int32_x3b_2_r_(self__128 *_goml_vec_Array_2_5int32, elem__129 [2]int32) *_goml_vec_Array_2_5int32 {
    var retv277 *_goml_vec_Array_2_5int32
    var result__130 *_goml_vec_Array_2_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___l_int32_x3b_2_r_()
    var index__131 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop279:
    for {
        var t280 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
        var t281 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___l_int32_x3b_2_r_(self__128)
        var t282 bool = t280 < t281
        if t282 {
            var t283 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
            var t284 [2]int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T___l_int32_x3b_2_r_(self__128, t283)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___l_int32_x3b_2_r_(result__130, t284)
            var t285 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
            var t286 int = t285 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__131, t286)
            continue
        } else {
            break Loop_loop279
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___l_int32_x3b_2_r_(result__130, elem__129)
    retv277 = result__130
    return retv277
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T___l_int32_x3b_2_r_(value__207 [2]int32) *ref_Array_2_5int32_x {
    var retv288 *ref_Array_2_5int32_x
    var t289 *ref_Array_2_5int32_x = ref__Ref_14Array_2_5int32(value__207)
    retv288 = t289
    return retv288
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T___l_int32_x3b_2_r_(self__208 *ref_Array_2_5int32_x) [2]int32 {
    var retv291 [2]int32
    var t292 [2]int32 = ref_get__Ref_14Array_2_5int32(self__208)
    retv291 = t292
    return retv291
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv294 string
    retv294 = self__38
    return retv294
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv296 string
    var t297 string = _goml_runtime_core_int32_to_string(self__43)
    retv296 = t297
    return retv296
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv299 string
    var t300 string = _goml_runtime_core_int_to_string(self__40)
    retv299 = t300
    return retv299
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv302 *ref_int_x
    var t303 *ref_int_x = ref__Ref_3int(value__207)
    retv302 = t303
    return retv302
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv305 int
    var t306 int = ref_get__Ref_3int(self__208)
    retv305 = t306
    return retv305
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int(self__137 *_goml_vec_int) int {
    var retv308 int
    var t309 int = vec_len__Vec_3int(self__137)
    retv308 = t309
    return retv308
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(self__126 *_goml_vec_int, elem__127 int) struct{} {
    vec_push__Vec_3int(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(self__132 *_goml_vec_int, index__133 int) int {
    var retv313 int
    var t314 int = vec_get__Vec_3int(self__132, index__133)
    retv313 = t314
    return retv313
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__137 *_goml_vec_int32) int {
    var retv318 int
    var t319 int = vec_len__Vec_5int32(self__137)
    retv318 = t319
    return retv318
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__126 *_goml_vec_int32, elem__127 int32) struct{} {
    vec_push__Vec_5int32(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__132 *_goml_vec_int32, index__133 int) int32 {
    var retv323 int32
    var t324 int32 = vec_get__Vec_5int32(self__132, index__133)
    retv323 = t324
    return retv323
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___l_int32_x3b_2_r_(self__137 *_goml_vec_Array_2_5int32) int {
    var retv326 int
    var t327 int = vec_len__Vec_14Array_2_5int32(self__137)
    retv326 = t327
    return retv326
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___l_int32_x3b_2_r_(self__126 *_goml_vec_Array_2_5int32, elem__127 [2]int32) struct{} {
    vec_push__Vec_14Array_2_5int32(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T___l_int32_x3b_2_r_(self__132 *_goml_vec_Array_2_5int32, index__133 int) [2]int32 {
    var retv331 [2]int32
    var t332 [2]int32 = vec_get__Vec_14Array_2_5int32(self__132, index__133)
    retv331 = t332
    return retv331
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__55 string, other__56 string) bool {
    var retv334 bool
    var t335 bool = self__55 == other__56
    retv334 = t335
    return retv334
}

func _goml_m_trait__impl_i_Hash_i_string_i_hash(self__83 string) uint64 {
    var retv337 uint64
    var t338 uint64 = _goml_runtime_core_string_hash(self__83)
    retv337 = t338
    return retv337
}

func main() {
    main0()
}
