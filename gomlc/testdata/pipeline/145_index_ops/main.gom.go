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

func main0() struct{} {
    var t244 [2]int = [2]int{31, 32}
    var t245 int = array_get__Array_2_3int(t244, 1)
    println__T_int(t245)
    var arr__2 [3]int = [3]int{1, 2, 3}
    var t246 int = array_get__Array_3_3int(arr__2, 0)
    println__T_int(t246)
    var arr2__3 [3]int = [3]int{4, 5, 6}
    var place_root180 [3]int = arr2__3
    var index181 int = 1
    array_get__Array_3_3int(place_root180, index181)
    var value183 int = 50
    var t247 [3]int = array_set__Array_3_3int(place_root180, index181, value183)
    arr2__3 = t247
    var t249 int = array_get__Array_3_3int(arr2__3, 1)
    println__T_int(t249)
    var t250 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int()
    var t251 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int(t250, 7)
    var t252 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int(t251, 8)
    var vec__4 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int(t252, 9)
    var t253 int = vec_get__Vec_3int(vec__4, 2)
    println__T_int(t253)
    var t254 *_goml_vec_int32
    var inline451 *_goml_vec_int32 = vec_new__Vec_5int32()
    t254 = inline451
    var t255 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t254, 10)
    var t256 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t255, 11)
    var vec2__5 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t256, 12)
    var index188 int = 0
    vec_get__Vec_5int32(vec2__5, index188)
    var value190 int32 = 100
    vec_set__Vec_5int32(vec2__5, index188, value190)
    var t258 int32 = vec_get__Vec_5int32(vec2__5, 0)
    var inline448 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t258)
    _goml_runtime_core_string_println(inline448)
    var s__6 []int32
    var inline444 int = 0
    var inline445 int = 2
    var inline446 []int32 = vec2__5.items[inline444:inline445]
    s__6 = inline446
    var t259 int32 = s__6[1]
    var inline441 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t259)
    _goml_runtime_core_string_println(inline441)
    var map__7 *hashmap_string_int32_x
    var inline439 *hashmap_string_int32_x = hashmap_new__HashMap_6string_5int32()
    map__7 = inline439
    var index195 string = "a"
    hashmap_get__HashMap_6string_5int32(map__7, index195)
    var value197 int32 = 13
    hashmap_set__HashMap_6string_5int32(map__7, index195, value197)
    var t261 Option__int32 = hashmap_get__HashMap_6string_5int32(map__7, "a")
    switch t261.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline435 int32 = t261.(Some)._0
        println__T_int32(inline435)
    default:
        panic("non-exhaustive match")
    }
    var t262 Option__int32 = hashmap_get__HashMap_6string_5int32(map__7, "missing")
    switch t262.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline430 int32 = t262.(Some)._0
        println__T_int32(inline430)
    default:
        panic("non-exhaustive match")
    }
    var t263 [2]int32 = [2]int32{1, 2}
    var t264 [2]int32 = [2]int32{3, 4}
    var matrix__8 [2][2]int32 = [2][2]int32{t263, t264}
    var place_root201 [2][2]int32 = matrix__8
    var index202 int = 1
    var place203 [2]int32 = array_get__Array_2_14Array_2_5int32(place_root201, index202)
    var index204 int = 0
    array_get__Array_2_5int32(place203, index204)
    var value206 int32 = 30
    var t265 [2]int32 = array_set__Array_2_5int32(place203, index204, value206)
    var t266 [2][2]int32 = array_set__Array_2_14Array_2_5int32(place_root201, index202, t265)
    matrix__8 = t266
    var t268 [2]int32 = array_get__Array_2_14Array_2_5int32(matrix__8, 1)
    var t269 int32 = array_get__Array_2_5int32(t268, 0)
    var inline426 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t269)
    _goml_runtime_core_string_println(inline426)
    var t270 [2]int = [2]int{14, 15}
    var pair__9 Tuple2_11Array2_3int_3int = Tuple2_11Array2_3int_3int{
        _0: t270,
        _1: 0,
    }
    var place_root209 Tuple2_11Array2_3int_3int = pair__9
    var place210 [2]int = place_root209._0
    var index211 int = 1
    array_get__Array_2_3int(place210, index211)
    var value213 int = 150
    var t271 [2]int = array_set__Array_2_3int(place210, index211, value213)
    var t272 int = place_root209._1
    var t273 Tuple2_11Array2_3int_3int = Tuple2_11Array2_3int_3int{
        _0: t271,
        _1: t272,
    }
    pair__9 = t273
    var t275 [2]int = pair__9._0
    var t276 int = array_get__Array_2_3int(t275, 1)
    var inline423 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t276)
    _goml_runtime_core_string_println(inline423)
    var t277 [2]int32 = [2]int32{16, 17}
    var t278 *_goml_vec_Array_2_5int32
    var inline421 *_goml_vec_Array_2_5int32 = vec_new__Vec_14Array_2_5int32()
    t278 = inline421
    var t279 [2]int32 = [2]int32{18, 19}
    var t280 *_goml_vec_Array_2_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T___l_int32_x3b_2_r_(t278, t279)
    var t281 [2]int32 = [2]int32{20, 21}
    var t282 *_goml_vec_Array_2_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T___l_int32_x3b_2_r_(t280, t281)
    var holder__10 Holder = Holder{
        data: t277,
        vecs: t282,
    }
    var place_root216 Holder = holder__10
    var place217 [2]int32 = place_root216.data
    var index218 int = 0
    array_get__Array_2_5int32(place217, index218)
    var value220 int32 = 160
    var t283 [2]int32 = array_set__Array_2_5int32(place217, index218, value220)
    var t284 *_goml_vec_Array_2_5int32 = place_root216.vecs
    var t285 Holder = Holder{
        data: t283,
        vecs: t284,
    }
    holder__10 = t285
    var t287 [2]int32 = holder__10.data
    var t288 int32 = array_get__Array_2_5int32(t287, 0)
    var inline418 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t288)
    _goml_runtime_core_string_println(inline418)
    var place_root223 Holder = holder__10
    var place224 *_goml_vec_Array_2_5int32 = place_root223.vecs
    var index225 int = 1
    var place226 [2]int32 = vec_get__Vec_14Array_2_5int32(place224, index225)
    var index227 int = 0
    array_get__Array_2_5int32(place226, index227)
    var value229 int32 = 200
    var t289 [2]int32 = array_set__Array_2_5int32(place226, index227, value229)
    vec_set__Vec_14Array_2_5int32(place224, index225, t289)
    var t291 *_goml_vec_Array_2_5int32 = holder__10.vecs
    var t292 [2]int32 = vec_get__Vec_14Array_2_5int32(t291, 1)
    var t293 int32 = array_get__Array_2_5int32(t292, 0)
    var inline415 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t293)
    _goml_runtime_core_string_println(inline415)
    var t294 [2]int32 = [2]int32{22, 23}
    var r__11 *ref_Array_2_5int32_x
    var inline413 *ref_Array_2_5int32_x = ref__Ref_14Array_2_5int32(t294)
    r__11 = inline413
    var place_root233 [2]int32 = ref_get__Ref_14Array_2_5int32(r__11)
    var index234 int = 1
    array_get__Array_2_5int32(place_root233, index234)
    var value236 int32 = 230
    var t295 [2]int32 = array_set__Array_2_5int32(place_root233, index234, value236)
    ref_set__Ref_14Array_2_5int32(r__11, t295)
    var t297 [2]int32
    var inline411 [2]int32 = ref_get__Ref_14Array_2_5int32(r__11)
    t297 = inline411
    var t298 int32 = array_get__Array_2_5int32(t297, 1)
    var inline408 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t298)
    _goml_runtime_core_string_println(inline408)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t300 string
    t300 = value__31
    _goml_runtime_core_string_println(t300)
    return struct{}{}
}

func println__T_int32(value__31 int32) struct{} {
    var t303 string
    var inline454 string = _goml_runtime_core_int32_to_string(value__31)
    t303 = inline454
    _goml_runtime_core_string_println(t303)
    return struct{}{}
}

func println__T_int(value__31 int) struct{} {
    var t306 string
    var inline456 string = _goml_runtime_core_int_to_string(value__31)
    t306 = inline456
    _goml_runtime_core_string_println(t306)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int() *_goml_vec_int {
    var t310 *_goml_vec_int = vec_new__Vec_3int()
    return t310
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int(self__157 *_goml_vec_int, elem__158 int) *_goml_vec_int {
    var t313 int
    var inline466 int = vec_len__Vec_3int(self__157)
    t313 = inline466
    var t314 int = t313 + 1
    var result__159 *_goml_vec_int
    var inline464 *_goml_vec_int = vec_with_capacity__Vec_3int(t314)
    result__159 = inline464
    var index__160 int = 0
    Loop_loop316:
    for {
        var t317 int
        var inline460 int = vec_len__Vec_3int(self__157)
        t317 = inline460
        var t318 bool = index__160 < t317
        if t318 {
            var t319 int = vec_get__Vec_3int(self__157, index__160)
            vec_push__Vec_3int(result__159, t319)
            var compound_old60 int = index__160
            var compound_value61 int = 1
            var t320 int = compound_old60 + compound_value61
            index__160 = t320
            continue
        } else {
            break Loop_loop316
        }
    }
    vec_push__Vec_3int(result__159, elem__158)
    return result__159
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(self__157 *_goml_vec_int32, elem__158 int32) *_goml_vec_int32 {
    var t327 int
    var inline476 int = vec_len__Vec_5int32(self__157)
    t327 = inline476
    var t328 int = t327 + 1
    var result__159 *_goml_vec_int32
    var inline474 *_goml_vec_int32 = vec_with_capacity__Vec_5int32(t328)
    result__159 = inline474
    var index__160 int = 0
    Loop_loop330:
    for {
        var t331 int
        var inline470 int = vec_len__Vec_5int32(self__157)
        t331 = inline470
        var t332 bool = index__160 < t331
        if t332 {
            var t333 int32 = vec_get__Vec_5int32(self__157, index__160)
            vec_push__Vec_5int32(result__159, t333)
            var compound_old60 int = index__160
            var compound_value61 int = 1
            var t334 int = compound_old60 + compound_value61
            index__160 = t334
            continue
        } else {
            break Loop_loop330
        }
    }
    vec_push__Vec_5int32(result__159, elem__158)
    return result__159
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T___l_int32_x3b_2_r_(self__157 *_goml_vec_Array_2_5int32, elem__158 [2]int32) *_goml_vec_Array_2_5int32 {
    var t347 int
    var inline486 int = vec_len__Vec_14Array_2_5int32(self__157)
    t347 = inline486
    var t348 int = t347 + 1
    var result__159 *_goml_vec_Array_2_5int32
    var inline484 *_goml_vec_Array_2_5int32 = vec_with_capacity__Vec_14Array_2_5int32(t348)
    result__159 = inline484
    var index__160 int = 0
    Loop_loop350:
    for {
        var t351 int
        var inline480 int = vec_len__Vec_14Array_2_5int32(self__157)
        t351 = inline480
        var t352 bool = index__160 < t351
        if t352 {
            var t353 [2]int32 = vec_get__Vec_14Array_2_5int32(self__157, index__160)
            vec_push__Vec_14Array_2_5int32(result__159, t353)
            var compound_old60 int = index__160
            var compound_value61 int = 1
            var t354 int = compound_old60 + compound_value61
            index__160 = t354
            continue
        } else {
            break Loop_loop350
        }
    }
    vec_push__Vec_14Array_2_5int32(result__159, elem__158)
    return result__159
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t366 string = _goml_runtime_core_int32_to_string(self__72)
    return t366
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t369 string = _goml_runtime_core_int_to_string(self__69)
    return t369
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__84 string, other__85 string) bool {
    var t396 bool = self__84 == other__85
    return t396
}

func _goml_m_trait__impl_i_Hash_i_string_i_hash(self__112 string) uint64 {
    var t399 uint64 = _goml_runtime_core_string_hash(self__112)
    return t399
}

func main() {
    main0()
}
