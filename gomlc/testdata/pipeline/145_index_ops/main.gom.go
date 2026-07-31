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
    case Some:
        var x152 int32 = x__0.(Some)._0
        var v__1 int32 = x152
        println__T_int32(v__1)
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func main0() struct{} {
    var t219 [2]int = [2]int{31, 32}
    var t220 int = array_get__Array_2_3int(t219, 1)
    println__T_int(t220)
    var arr__2 [3]int = [3]int{1, 2, 3}
    var t221 int = array_get__Array_3_3int(arr__2, 0)
    println__T_int(t221)
    var arr2__3 [3]int = [3]int{4, 5, 6}
    var place_root155 [3]int = arr2__3
    var index156 int = 1
    array_get__Array_3_3int(place_root155, index156)
    var value158 int = 50
    var t222 [3]int = array_set__Array_3_3int(place_root155, index156, value158)
    arr2__3 = t222
    var t224 int = array_get__Array_3_3int(arr2__3, 1)
    println__T_int(t224)
    var t225 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int()
    var t226 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int(t225, 7)
    var t227 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int(t226, 8)
    var vec__4 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int(t227, 9)
    var t228 int = vec_get__Vec_3int(vec__4, 2)
    println__T_int(t228)
    var t229 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    var t230 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t229, 10)
    var t231 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t230, 11)
    var vec2__5 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t231, 12)
    var place_root162 *_goml_vec_int32 = vec2__5
    var index163 int = 0
    vec_get__Vec_5int32(place_root162, index163)
    var value165 int32 = 100
    vec_set__Vec_5int32(place_root162, index163, value165)
    var t233 int32 = vec_get__Vec_5int32(vec2__5, 0)
    println__T_int32(t233)
    var s__6 []int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(vec2__5, 0, 2)
    var t234 int32 = s__6[1]
    println__T_int32(t234)
    var map__7 *hashmap_string_int32_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32()
    var place_root169 *hashmap_string_int32_x = map__7
    var index170 string = "a"
    hashmap_get__HashMap_6string_5int32(place_root169, index170)
    var value172 int32 = 13
    hashmap_set__HashMap_6string_5int32(place_root169, index170, value172)
    var t236 Option__int32 = hashmap_get__HashMap_6string_5int32(map__7, "a")
    print_opt_int(t236)
    var t237 Option__int32 = hashmap_get__HashMap_6string_5int32(map__7, "missing")
    print_opt_int(t237)
    var t238 [2]int32 = [2]int32{1, 2}
    var t239 [2]int32 = [2]int32{3, 4}
    var matrix__8 [2][2]int32 = [2][2]int32{t238, t239}
    var place_root176 [2][2]int32 = matrix__8
    var index177 int = 1
    var place178 [2]int32 = array_get__Array_2_14Array_2_5int32(place_root176, index177)
    var index179 int = 0
    array_get__Array_2_5int32(place178, index179)
    var value181 int32 = 30
    var t240 [2]int32 = array_set__Array_2_5int32(place178, index179, value181)
    var t241 [2][2]int32 = array_set__Array_2_14Array_2_5int32(place_root176, index177, t240)
    matrix__8 = t241
    var t243 [2]int32 = array_get__Array_2_14Array_2_5int32(matrix__8, 1)
    var t244 int32 = array_get__Array_2_5int32(t243, 0)
    println__T_int32(t244)
    var t245 [2]int = [2]int{14, 15}
    var pair__9 Tuple2_11Array2_3int_3int = Tuple2_11Array2_3int_3int{
        _0: t245,
        _1: 0,
    }
    var place_root184 Tuple2_11Array2_3int_3int = pair__9
    var place185 [2]int = place_root184._0
    var index186 int = 1
    array_get__Array_2_3int(place185, index186)
    var value188 int = 150
    var t246 [2]int = array_set__Array_2_3int(place185, index186, value188)
    var t247 int = place_root184._1
    var t248 Tuple2_11Array2_3int_3int = Tuple2_11Array2_3int_3int{
        _0: t246,
        _1: t247,
    }
    pair__9 = t248
    var t250 [2]int = pair__9._0
    var t251 int = array_get__Array_2_3int(t250, 1)
    println__T_int(t251)
    var t252 [2]int32 = [2]int32{16, 17}
    var t253 *_goml_vec_Array_2_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___l_int32_x3b_2_r_()
    var t254 [2]int32 = [2]int32{18, 19}
    var t255 *_goml_vec_Array_2_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T___l_int32_x3b_2_r_(t253, t254)
    var t256 [2]int32 = [2]int32{20, 21}
    var t257 *_goml_vec_Array_2_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T___l_int32_x3b_2_r_(t255, t256)
    var holder__10 Holder = Holder{
        data: t252,
        vecs: t257,
    }
    var place_root191 Holder = holder__10
    var place192 [2]int32 = place_root191.data
    var index193 int = 0
    array_get__Array_2_5int32(place192, index193)
    var value195 int32 = 160
    var t258 [2]int32 = array_set__Array_2_5int32(place192, index193, value195)
    var t259 *_goml_vec_Array_2_5int32 = place_root191.vecs
    var t260 Holder = Holder{
        data: t258,
        vecs: t259,
    }
    holder__10 = t260
    var t262 [2]int32 = holder__10.data
    var t263 int32 = array_get__Array_2_5int32(t262, 0)
    println__T_int32(t263)
    var place_root198 Holder = holder__10
    var place199 *_goml_vec_Array_2_5int32 = place_root198.vecs
    var index200 int = 1
    var place201 [2]int32 = vec_get__Vec_14Array_2_5int32(place199, index200)
    var index202 int = 0
    array_get__Array_2_5int32(place201, index202)
    var value204 int32 = 200
    var t264 [2]int32 = array_set__Array_2_5int32(place201, index202, value204)
    vec_set__Vec_14Array_2_5int32(place199, index200, t264)
    var t266 *_goml_vec_Array_2_5int32 = holder__10.vecs
    var t267 [2]int32 = vec_get__Vec_14Array_2_5int32(t266, 1)
    var t268 int32 = array_get__Array_2_5int32(t267, 0)
    println__T_int32(t268)
    var t269 [2]int32 = [2]int32{22, 23}
    var r__11 *ref_Array_2_5int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T___l_int32_x3b_2_r_(t269)
    var place_ref207 *ref_Array_2_5int32_x = r__11
    var place_root208 [2]int32 = ref_get__Ref_14Array_2_5int32(place_ref207)
    var index209 int = 1
    array_get__Array_2_5int32(place_root208, index209)
    var value211 int32 = 230
    var t270 [2]int32 = array_set__Array_2_5int32(place_root208, index209, value211)
    ref_set__Ref_14Array_2_5int32(place_ref207, t270)
    var t272 [2]int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T___l_int32_x3b_2_r_(r__11)
    var t273 int32 = array_get__Array_2_5int32(t272, 1)
    println__T_int32(t273)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t275 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t275)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t278 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t278)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t281 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t281)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int() *_goml_vec_int {
    var retv284 *_goml_vec_int
    var t285 *_goml_vec_int = vec_new__Vec_3int()
    retv284 = t285
    return retv284
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int(self__128 *_goml_vec_int, elem__129 int) *_goml_vec_int {
    var retv287 *_goml_vec_int
    var t288 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int(self__128)
    var t289 int = t288 + 1
    var result__130 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__int(t289)
    var index__131 int = 0
    Loop_loop291:
    for {
        var t292 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int(self__128)
        var t293 bool = index__131 < t292
        if t293 {
            var t294 int = vec_get__Vec_3int(self__128, index__131)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(result__130, t294)
            var compound_old38 int = index__131
            var compound_value39 int = 1
            var t295 int = compound_old38 + compound_value39
            index__131 = t295
            continue
        } else {
            break Loop_loop291
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(result__130, elem__129)
    retv287 = result__130
    return retv287
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv298 *_goml_vec_int32
    var t299 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv298 = t299
    return retv298
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(self__128 *_goml_vec_int32, elem__129 int32) *_goml_vec_int32 {
    var retv301 *_goml_vec_int32
    var t302 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__128)
    var t303 int = t302 + 1
    var result__130 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__int32(t303)
    var index__131 int = 0
    Loop_loop305:
    for {
        var t306 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__128)
        var t307 bool = index__131 < t306
        if t307 {
            var t308 int32 = vec_get__Vec_5int32(self__128, index__131)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(result__130, t308)
            var compound_old38 int = index__131
            var compound_value39 int = 1
            var t309 int = compound_old38 + compound_value39
            index__131 = t309
            continue
        } else {
            break Loop_loop305
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(result__130, elem__129)
    retv301 = result__130
    return retv301
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(self__175 *_goml_vec_int32, start__176 int, end__177 int) []int32 {
    var retv312 []int32
    var t313 []int32 = self__175.items[start__176:end__177]
    retv312 = t313
    return retv312
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32() *hashmap_string_int32_x {
    var retv315 *hashmap_string_int32_x
    var t316 *hashmap_string_int32_x = hashmap_new__HashMap_6string_5int32()
    retv315 = t316
    return retv315
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___l_int32_x3b_2_r_() *_goml_vec_Array_2_5int32 {
    var retv318 *_goml_vec_Array_2_5int32
    var t319 *_goml_vec_Array_2_5int32 = vec_new__Vec_14Array_2_5int32()
    retv318 = t319
    return retv318
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T___l_int32_x3b_2_r_(self__128 *_goml_vec_Array_2_5int32, elem__129 [2]int32) *_goml_vec_Array_2_5int32 {
    var retv321 *_goml_vec_Array_2_5int32
    var t322 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___l_int32_x3b_2_r_(self__128)
    var t323 int = t322 + 1
    var result__130 *_goml_vec_Array_2_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T___l_int32_x3b_2_r_(t323)
    var index__131 int = 0
    Loop_loop325:
    for {
        var t326 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___l_int32_x3b_2_r_(self__128)
        var t327 bool = index__131 < t326
        if t327 {
            var t328 [2]int32 = vec_get__Vec_14Array_2_5int32(self__128, index__131)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___l_int32_x3b_2_r_(result__130, t328)
            var compound_old38 int = index__131
            var compound_value39 int = 1
            var t329 int = compound_old38 + compound_value39
            index__131 = t329
            continue
        } else {
            break Loop_loop325
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___l_int32_x3b_2_r_(result__130, elem__129)
    retv321 = result__130
    return retv321
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T___l_int32_x3b_2_r_(value__207 [2]int32) *ref_Array_2_5int32_x {
    var retv332 *ref_Array_2_5int32_x
    var t333 *ref_Array_2_5int32_x = ref__Ref_14Array_2_5int32(value__207)
    retv332 = t333
    return retv332
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T___l_int32_x3b_2_r_(self__208 *ref_Array_2_5int32_x) [2]int32 {
    var retv335 [2]int32
    var t336 [2]int32 = ref_get__Ref_14Array_2_5int32(self__208)
    retv335 = t336
    return retv335
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv338 string
    retv338 = self__38
    return retv338
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv340 string
    var t341 string = _goml_runtime_core_int32_to_string(self__43)
    retv340 = t341
    return retv340
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv343 string
    var t344 string = _goml_runtime_core_int_to_string(self__40)
    retv343 = t344
    return retv343
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__int(capacity__125 int) *_goml_vec_int {
    var retv346 *_goml_vec_int
    var t347 *_goml_vec_int = vec_with_capacity__Vec_3int(capacity__125)
    retv346 = t347
    return retv346
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int(self__137 *_goml_vec_int) int {
    var retv349 int
    var t350 int = vec_len__Vec_3int(self__137)
    retv349 = t350
    return retv349
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(self__126 *_goml_vec_int, elem__127 int) struct{} {
    vec_push__Vec_3int(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__int32(capacity__125 int) *_goml_vec_int32 {
    var retv354 *_goml_vec_int32
    var t355 *_goml_vec_int32 = vec_with_capacity__Vec_5int32(capacity__125)
    retv354 = t355
    return retv354
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__137 *_goml_vec_int32) int {
    var retv357 int
    var t358 int = vec_len__Vec_5int32(self__137)
    retv357 = t358
    return retv357
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__126 *_goml_vec_int32, elem__127 int32) struct{} {
    vec_push__Vec_5int32(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T___l_int32_x3b_2_r_(capacity__125 int) *_goml_vec_Array_2_5int32 {
    var retv362 *_goml_vec_Array_2_5int32
    var t363 *_goml_vec_Array_2_5int32 = vec_with_capacity__Vec_14Array_2_5int32(capacity__125)
    retv362 = t363
    return retv362
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___l_int32_x3b_2_r_(self__137 *_goml_vec_Array_2_5int32) int {
    var retv365 int
    var t366 int = vec_len__Vec_14Array_2_5int32(self__137)
    retv365 = t366
    return retv365
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___l_int32_x3b_2_r_(self__126 *_goml_vec_Array_2_5int32, elem__127 [2]int32) struct{} {
    vec_push__Vec_14Array_2_5int32(self__126, elem__127)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__55 string, other__56 string) bool {
    var retv370 bool
    var t371 bool = self__55 == other__56
    retv370 = t371
    return retv370
}

func _goml_m_trait__impl_i_Hash_i_string_i_hash(self__83 string) uint64 {
    var retv373 uint64
    var t374 uint64 = _goml_runtime_core_string_hash(self__83)
    retv373 = t374
    return retv373
}

func main() {
    main0()
}
