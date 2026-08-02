package main

import (
    _goml_fmt "fmt"
    _goml_slices "slices"
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

func vec_with_capacity__Vec_3int(capacity int) *_goml_vec_int {
    return &_goml_vec_int{
        items: _goml_slices.Grow([]int{}, int(capacity)),
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

func vec_with_capacity__Vec_5int32(capacity int) *_goml_vec_int32 {
    return &_goml_vec_int32{
        items: _goml_slices.Grow([]int32{}, int(capacity)),
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

func vec_with_capacity__Vec_14Array_2_5int32(capacity int) *_goml_vec_Array_2_5int32 {
    return &_goml_vec_Array_2_5int32{
        items: _goml_slices.Grow([][2]int32{}, int(capacity)),
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
        return struct{}{}
    case Some:
        var x155 int32 = x__0.(Some)._0
        println__T_int32(x155)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t222 [2]int = [2]int{31, 32}
    var t223 int = array_get__Array_2_3int(t222, 1)
    println__T_int(t223)
    var arr__2 [3]int = [3]int{1, 2, 3}
    var t224 int = array_get__Array_3_3int(arr__2, 0)
    println__T_int(t224)
    var arr2__3 [3]int = [3]int{4, 5, 6}
    var place_root158 [3]int = arr2__3
    var index159 int = 1
    array_get__Array_3_3int(place_root158, index159)
    var value161 int = 50
    var t225 [3]int = array_set__Array_3_3int(place_root158, index159, value161)
    arr2__3 = t225
    var t227 int = array_get__Array_3_3int(arr2__3, 1)
    println__T_int(t227)
    var t228 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int()
    var t229 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int(t228, 7)
    var t230 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int(t229, 8)
    var vec__4 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int(t230, 9)
    var t231 int = vec_get__Vec_3int(vec__4, 2)
    println__T_int(t231)
    var t232 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    var t233 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t232, 10)
    var t234 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t233, 11)
    var vec2__5 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t234, 12)
    var index166 int = 0
    vec_get__Vec_5int32(vec2__5, index166)
    var value168 int32 = 100
    vec_set__Vec_5int32(vec2__5, index166, value168)
    var t236 int32 = vec_get__Vec_5int32(vec2__5, 0)
    println__T_int32(t236)
    var s__6 []int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(vec2__5, 0, 2)
    var t237 int32 = s__6[1]
    println__T_int32(t237)
    var map__7 *hashmap_string_int32_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32()
    var index173 string = "a"
    hashmap_get__HashMap_6string_5int32(map__7, index173)
    var value175 int32 = 13
    hashmap_set__HashMap_6string_5int32(map__7, index173, value175)
    var t239 Option__int32 = hashmap_get__HashMap_6string_5int32(map__7, "a")
    print_opt_int(t239)
    var t240 Option__int32 = hashmap_get__HashMap_6string_5int32(map__7, "missing")
    print_opt_int(t240)
    var t241 [2]int32 = [2]int32{1, 2}
    var t242 [2]int32 = [2]int32{3, 4}
    var matrix__8 [2][2]int32 = [2][2]int32{t241, t242}
    var place_root179 [2][2]int32 = matrix__8
    var index180 int = 1
    var place181 [2]int32 = array_get__Array_2_14Array_2_5int32(place_root179, index180)
    var index182 int = 0
    array_get__Array_2_5int32(place181, index182)
    var value184 int32 = 30
    var t243 [2]int32 = array_set__Array_2_5int32(place181, index182, value184)
    var t244 [2][2]int32 = array_set__Array_2_14Array_2_5int32(place_root179, index180, t243)
    matrix__8 = t244
    var t246 [2]int32 = array_get__Array_2_14Array_2_5int32(matrix__8, 1)
    var t247 int32 = array_get__Array_2_5int32(t246, 0)
    println__T_int32(t247)
    var t248 [2]int = [2]int{14, 15}
    var pair__9 Tuple2_11Array2_3int_3int = Tuple2_11Array2_3int_3int{
        _0: t248,
        _1: 0,
    }
    var place_root187 Tuple2_11Array2_3int_3int = pair__9
    var place188 [2]int = place_root187._0
    var index189 int = 1
    array_get__Array_2_3int(place188, index189)
    var value191 int = 150
    var t249 [2]int = array_set__Array_2_3int(place188, index189, value191)
    var t250 int = place_root187._1
    var t251 Tuple2_11Array2_3int_3int = Tuple2_11Array2_3int_3int{
        _0: t249,
        _1: t250,
    }
    pair__9 = t251
    var t253 [2]int = pair__9._0
    var t254 int = array_get__Array_2_3int(t253, 1)
    println__T_int(t254)
    var t255 [2]int32 = [2]int32{16, 17}
    var t256 *_goml_vec_Array_2_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___l_int32_x3b_2_r_()
    var t257 [2]int32 = [2]int32{18, 19}
    var t258 *_goml_vec_Array_2_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T___l_int32_x3b_2_r_(t256, t257)
    var t259 [2]int32 = [2]int32{20, 21}
    var t260 *_goml_vec_Array_2_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T___l_int32_x3b_2_r_(t258, t259)
    var holder__10 Holder = Holder{
        data: t255,
        vecs: t260,
    }
    var place_root194 Holder = holder__10
    var place195 [2]int32 = place_root194.data
    var index196 int = 0
    array_get__Array_2_5int32(place195, index196)
    var value198 int32 = 160
    var t261 [2]int32 = array_set__Array_2_5int32(place195, index196, value198)
    var t262 *_goml_vec_Array_2_5int32 = place_root194.vecs
    var t263 Holder = Holder{
        data: t261,
        vecs: t262,
    }
    holder__10 = t263
    var t265 [2]int32 = holder__10.data
    var t266 int32 = array_get__Array_2_5int32(t265, 0)
    println__T_int32(t266)
    var place_root201 Holder = holder__10
    var place202 *_goml_vec_Array_2_5int32 = place_root201.vecs
    var index203 int = 1
    var place204 [2]int32 = vec_get__Vec_14Array_2_5int32(place202, index203)
    var index205 int = 0
    array_get__Array_2_5int32(place204, index205)
    var value207 int32 = 200
    var t267 [2]int32 = array_set__Array_2_5int32(place204, index205, value207)
    vec_set__Vec_14Array_2_5int32(place202, index203, t267)
    var t269 *_goml_vec_Array_2_5int32 = holder__10.vecs
    var t270 [2]int32 = vec_get__Vec_14Array_2_5int32(t269, 1)
    var t271 int32 = array_get__Array_2_5int32(t270, 0)
    println__T_int32(t271)
    var t272 [2]int32 = [2]int32{22, 23}
    var r__11 *ref_Array_2_5int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T___l_int32_x3b_2_r_(t272)
    var place_root211 [2]int32 = ref_get__Ref_14Array_2_5int32(r__11)
    var index212 int = 1
    array_get__Array_2_5int32(place_root211, index212)
    var value214 int32 = 230
    var t273 [2]int32 = array_set__Array_2_5int32(place_root211, index212, value214)
    ref_set__Ref_14Array_2_5int32(r__11, t273)
    var t275 [2]int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T___l_int32_x3b_2_r_(r__11)
    var t276 int32 = array_get__Array_2_5int32(t275, 1)
    println__T_int32(t276)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t278 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t278)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t281 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t281)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t284 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t284)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int() *_goml_vec_int {
    var t288 *_goml_vec_int = vec_new__Vec_3int()
    return t288
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int(self__128 *_goml_vec_int, elem__129 int) *_goml_vec_int {
    var t291 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int(self__128)
    var t292 int = t291 + 1
    var result__130 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__int(t292)
    var index__131 int = 0
    Loop_loop294:
    for {
        var t295 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int(self__128)
        var t296 bool = index__131 < t295
        if t296 {
            var t297 int = vec_get__Vec_3int(self__128, index__131)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(result__130, t297)
            var compound_old38 int = index__131
            var compound_value39 int = 1
            var t298 int = compound_old38 + compound_value39
            index__131 = t298
            continue
        } else {
            break Loop_loop294
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(result__130, elem__129)
    return result__130
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var t302 *_goml_vec_int32 = vec_new__Vec_5int32()
    return t302
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(self__128 *_goml_vec_int32, elem__129 int32) *_goml_vec_int32 {
    var t305 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__128)
    var t306 int = t305 + 1
    var result__130 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__int32(t306)
    var index__131 int = 0
    Loop_loop308:
    for {
        var t309 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__128)
        var t310 bool = index__131 < t309
        if t310 {
            var t311 int32 = vec_get__Vec_5int32(self__128, index__131)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(result__130, t311)
            var compound_old38 int = index__131
            var compound_value39 int = 1
            var t312 int = compound_old38 + compound_value39
            index__131 = t312
            continue
        } else {
            break Loop_loop308
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(result__130, elem__129)
    return result__130
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(self__175 *_goml_vec_int32, start__176 int, end__177 int) []int32 {
    var t316 []int32 = self__175.items[start__176:end__177]
    return t316
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32() *hashmap_string_int32_x {
    var t319 *hashmap_string_int32_x = hashmap_new__HashMap_6string_5int32()
    return t319
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___l_int32_x3b_2_r_() *_goml_vec_Array_2_5int32 {
    var t322 *_goml_vec_Array_2_5int32 = vec_new__Vec_14Array_2_5int32()
    return t322
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T___l_int32_x3b_2_r_(self__128 *_goml_vec_Array_2_5int32, elem__129 [2]int32) *_goml_vec_Array_2_5int32 {
    var t325 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___l_int32_x3b_2_r_(self__128)
    var t326 int = t325 + 1
    var result__130 *_goml_vec_Array_2_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T___l_int32_x3b_2_r_(t326)
    var index__131 int = 0
    Loop_loop328:
    for {
        var t329 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___l_int32_x3b_2_r_(self__128)
        var t330 bool = index__131 < t329
        if t330 {
            var t331 [2]int32 = vec_get__Vec_14Array_2_5int32(self__128, index__131)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___l_int32_x3b_2_r_(result__130, t331)
            var compound_old38 int = index__131
            var compound_value39 int = 1
            var t332 int = compound_old38 + compound_value39
            index__131 = t332
            continue
        } else {
            break Loop_loop328
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___l_int32_x3b_2_r_(result__130, elem__129)
    return result__130
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T___l_int32_x3b_2_r_(value__207 [2]int32) *ref_Array_2_5int32_x {
    var t336 *ref_Array_2_5int32_x = ref__Ref_14Array_2_5int32(value__207)
    return t336
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T___l_int32_x3b_2_r_(self__208 *ref_Array_2_5int32_x) [2]int32 {
    var t339 [2]int32 = ref_get__Ref_14Array_2_5int32(self__208)
    return t339
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var t344 string = _goml_runtime_core_int32_to_string(self__43)
    return t344
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var t347 string = _goml_runtime_core_int_to_string(self__40)
    return t347
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int(self__137 *_goml_vec_int) int {
    var t350 int = vec_len__Vec_3int(self__137)
    return t350
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__int(capacity__125 int) *_goml_vec_int {
    var t353 *_goml_vec_int = vec_with_capacity__Vec_3int(capacity__125)
    return t353
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(self__126 *_goml_vec_int, elem__127 int) struct{} {
    vec_push__Vec_3int(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__137 *_goml_vec_int32) int {
    var t358 int = vec_len__Vec_5int32(self__137)
    return t358
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__int32(capacity__125 int) *_goml_vec_int32 {
    var t361 *_goml_vec_int32 = vec_with_capacity__Vec_5int32(capacity__125)
    return t361
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__126 *_goml_vec_int32, elem__127 int32) struct{} {
    vec_push__Vec_5int32(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___l_int32_x3b_2_r_(self__137 *_goml_vec_Array_2_5int32) int {
    var t366 int = vec_len__Vec_14Array_2_5int32(self__137)
    return t366
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T___l_int32_x3b_2_r_(capacity__125 int) *_goml_vec_Array_2_5int32 {
    var t369 *_goml_vec_Array_2_5int32 = vec_with_capacity__Vec_14Array_2_5int32(capacity__125)
    return t369
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___l_int32_x3b_2_r_(self__126 *_goml_vec_Array_2_5int32, elem__127 [2]int32) struct{} {
    vec_push__Vec_14Array_2_5int32(self__126, elem__127)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__55 string, other__56 string) bool {
    var t374 bool = self__55 == other__56
    return t374
}

func _goml_m_trait__impl_i_Hash_i_string_i_hash(self__83 string) uint64 {
    var t377 uint64 = _goml_runtime_core_string_hash(self__83)
    return t377
}

func main() {
    main0()
}
