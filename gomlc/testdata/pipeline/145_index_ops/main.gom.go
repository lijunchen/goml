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
        if entry.active && _goml_m_trait__impl_i_PartialEq_i_string_i_eq(entry.key, key) {
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
        if entry.active && _goml_m_trait__impl_i_PartialEq_i_string_i_eq(entry.key, key) {
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
    var t249 [2]int = [2]int{31, 32}
    var t250 int = array_get__Array_2_3int(t249, 1)
    println__T_int(t250)
    var arr__2 [3]int = [3]int{1, 2, 3}
    var t251 int = array_get__Array_3_3int(arr__2, 0)
    println__T_int(t251)
    var arr2__3 [3]int = [3]int{4, 5, 6}
    var place_root185 [3]int = arr2__3
    var index186 int = 1
    array_get__Array_3_3int(place_root185, index186)
    var value188 int = 50
    var t252 [3]int = array_set__Array_3_3int(place_root185, index186, value188)
    arr2__3 = t252
    var t254 int = array_get__Array_3_3int(arr2__3, 1)
    println__T_int(t254)
    var t255 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int()
    var t256 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int(t255, 7)
    var t257 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int(t256, 8)
    var vec__4 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int(t257, 9)
    var t258 int = vec_get__Vec_3int(vec__4, 2)
    println__T_int(t258)
    var t259 *_goml_vec_int32
    var inline482 *_goml_vec_int32 = vec_new__Vec_5int32()
    t259 = inline482
    var t260 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t259, 10)
    var t261 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t260, 11)
    var vec2__5 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t261, 12)
    var index193 int = 0
    vec_get__Vec_5int32(vec2__5, index193)
    var value195 int32 = 100
    vec_set__Vec_5int32(vec2__5, index193, value195)
    var t263 int32 = vec_get__Vec_5int32(vec2__5, 0)
    var inline479 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t263)
    _goml_runtime_core_string_println(inline479)
    var s__6 []int32
    var inline475 int = 0
    var inline476 int = 2
    var inline477 []int32 = vec2__5.items[inline475:inline476]
    s__6 = inline477
    var t264 int32 = s__6[1]
    var inline472 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t264)
    _goml_runtime_core_string_println(inline472)
    var map__7 *hashmap_string_int32_x
    var inline470 *hashmap_string_int32_x = hashmap_new__HashMap_6string_5int32()
    map__7 = inline470
    var index200 string = "a"
    hashmap_get__HashMap_6string_5int32(map__7, index200)
    var value202 int32 = 13
    hashmap_set__HashMap_6string_5int32(map__7, index200, value202)
    var t266 Option__int32 = hashmap_get__HashMap_6string_5int32(map__7, "a")
    switch t266.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline466 int32 = t266.(Some)._0
        println__T_int32(inline466)
    default:
        panic("non-exhaustive match")
    }
    var t267 Option__int32 = hashmap_get__HashMap_6string_5int32(map__7, "missing")
    switch t267.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline461 int32 = t267.(Some)._0
        println__T_int32(inline461)
    default:
        panic("non-exhaustive match")
    }
    var t268 [2]int32 = [2]int32{1, 2}
    var t269 [2]int32 = [2]int32{3, 4}
    var matrix__8 [2][2]int32 = [2][2]int32{t268, t269}
    var place_root206 [2][2]int32 = matrix__8
    var index207 int = 1
    var place208 [2]int32 = array_get__Array_2_14Array_2_5int32(place_root206, index207)
    var index209 int = 0
    array_get__Array_2_5int32(place208, index209)
    var value211 int32 = 30
    var t270 [2]int32 = array_set__Array_2_5int32(place208, index209, value211)
    var t271 [2][2]int32 = array_set__Array_2_14Array_2_5int32(place_root206, index207, t270)
    matrix__8 = t271
    var t273 [2]int32 = array_get__Array_2_14Array_2_5int32(matrix__8, 1)
    var t274 int32 = array_get__Array_2_5int32(t273, 0)
    var inline457 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t274)
    _goml_runtime_core_string_println(inline457)
    var t275 [2]int = [2]int{14, 15}
    var pair__9 Tuple2_11Array2_3int_3int = Tuple2_11Array2_3int_3int{
        _0: t275,
        _1: 0,
    }
    var place_root214 Tuple2_11Array2_3int_3int = pair__9
    var place215 [2]int = place_root214._0
    var index216 int = 1
    array_get__Array_2_3int(place215, index216)
    var value218 int = 150
    var t276 [2]int = array_set__Array_2_3int(place215, index216, value218)
    var t277 int = place_root214._1
    var t278 Tuple2_11Array2_3int_3int = Tuple2_11Array2_3int_3int{
        _0: t276,
        _1: t277,
    }
    pair__9 = t278
    var t280 [2]int = pair__9._0
    var t281 int = array_get__Array_2_3int(t280, 1)
    var inline454 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t281)
    _goml_runtime_core_string_println(inline454)
    var t282 [2]int32 = [2]int32{16, 17}
    var t283 *_goml_vec_Array_2_5int32
    var inline452 *_goml_vec_Array_2_5int32 = vec_new__Vec_14Array_2_5int32()
    t283 = inline452
    var t284 [2]int32 = [2]int32{18, 19}
    var t285 *_goml_vec_Array_2_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T___l_int32_x3b_2_r_(t283, t284)
    var t286 [2]int32 = [2]int32{20, 21}
    var t287 *_goml_vec_Array_2_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T___l_int32_x3b_2_r_(t285, t286)
    var holder__10 Holder = Holder{
        data: t282,
        vecs: t287,
    }
    var place_root221 Holder = holder__10
    var place222 [2]int32 = place_root221.data
    var index223 int = 0
    array_get__Array_2_5int32(place222, index223)
    var value225 int32 = 160
    var t288 [2]int32 = array_set__Array_2_5int32(place222, index223, value225)
    var t289 *_goml_vec_Array_2_5int32 = place_root221.vecs
    var t290 Holder = Holder{
        data: t288,
        vecs: t289,
    }
    holder__10 = t290
    var t292 [2]int32 = holder__10.data
    var t293 int32 = array_get__Array_2_5int32(t292, 0)
    var inline449 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t293)
    _goml_runtime_core_string_println(inline449)
    var place_root228 Holder = holder__10
    var place229 *_goml_vec_Array_2_5int32 = place_root228.vecs
    var index230 int = 1
    var place231 [2]int32 = vec_get__Vec_14Array_2_5int32(place229, index230)
    var index232 int = 0
    array_get__Array_2_5int32(place231, index232)
    var value234 int32 = 200
    var t294 [2]int32 = array_set__Array_2_5int32(place231, index232, value234)
    vec_set__Vec_14Array_2_5int32(place229, index230, t294)
    var t296 *_goml_vec_Array_2_5int32 = holder__10.vecs
    var t297 [2]int32 = vec_get__Vec_14Array_2_5int32(t296, 1)
    var t298 int32 = array_get__Array_2_5int32(t297, 0)
    var inline446 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t298)
    _goml_runtime_core_string_println(inline446)
    var t299 [2]int32 = [2]int32{22, 23}
    var r__11 *ref_Array_2_5int32_x
    var inline444 *ref_Array_2_5int32_x = ref__Ref_14Array_2_5int32(t299)
    r__11 = inline444
    var place_root238 [2]int32 = ref_get__Ref_14Array_2_5int32(r__11)
    var index239 int = 1
    array_get__Array_2_5int32(place_root238, index239)
    var value241 int32 = 230
    var t300 [2]int32 = array_set__Array_2_5int32(place_root238, index239, value241)
    ref_set__Ref_14Array_2_5int32(r__11, t300)
    var t302 [2]int32
    var inline442 [2]int32 = ref_get__Ref_14Array_2_5int32(r__11)
    t302 = inline442
    var t303 int32 = array_get__Array_2_5int32(t302, 1)
    var inline439 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t303)
    _goml_runtime_core_string_println(inline439)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t305 string
    t305 = value__1
    _goml_runtime_core_string_println(t305)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t308 string
    var inline485 string = _goml_runtime_core_int32_to_string(value__1)
    t308 = inline485
    _goml_runtime_core_string_println(t308)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t311 string
    var inline487 string = _goml_runtime_core_int_to_string(value__1)
    t311 = inline487
    _goml_runtime_core_string_println(t311)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int() *_goml_vec_int {
    var t315 *_goml_vec_int = vec_new__Vec_3int()
    return t315
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int(self__176 *_goml_vec_int, elem__177 int) *_goml_vec_int {
    var t318 int
    var inline497 int = vec_len__Vec_3int(self__176)
    t318 = inline497
    var t319 int = t318 + 1
    var result__178 *_goml_vec_int
    var inline495 *_goml_vec_int = vec_with_capacity__Vec_3int(t319)
    result__178 = inline495
    var index__179 int = 0
    Loop_loop321:
    for {
        var t322 int
        var inline491 int = vec_len__Vec_3int(self__176)
        t322 = inline491
        var t323 bool = index__179 < t322
        if t323 {
            var t324 int = vec_get__Vec_3int(self__176, index__179)
            vec_push__Vec_3int(result__178, t324)
            var compound_old80 int = index__179
            var compound_value81 int = 1
            var t325 int = compound_old80 + compound_value81
            index__179 = t325
            continue
        } else {
            break Loop_loop321
        }
    }
    vec_push__Vec_3int(result__178, elem__177)
    return result__178
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(self__176 *_goml_vec_int32, elem__177 int32) *_goml_vec_int32 {
    var t332 int
    var inline507 int = vec_len__Vec_5int32(self__176)
    t332 = inline507
    var t333 int = t332 + 1
    var result__178 *_goml_vec_int32
    var inline505 *_goml_vec_int32 = vec_with_capacity__Vec_5int32(t333)
    result__178 = inline505
    var index__179 int = 0
    Loop_loop335:
    for {
        var t336 int
        var inline501 int = vec_len__Vec_5int32(self__176)
        t336 = inline501
        var t337 bool = index__179 < t336
        if t337 {
            var t338 int32 = vec_get__Vec_5int32(self__176, index__179)
            vec_push__Vec_5int32(result__178, t338)
            var compound_old80 int = index__179
            var compound_value81 int = 1
            var t339 int = compound_old80 + compound_value81
            index__179 = t339
            continue
        } else {
            break Loop_loop335
        }
    }
    vec_push__Vec_5int32(result__178, elem__177)
    return result__178
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T___l_int32_x3b_2_r_(self__176 *_goml_vec_Array_2_5int32, elem__177 [2]int32) *_goml_vec_Array_2_5int32 {
    var t352 int
    var inline517 int = vec_len__Vec_14Array_2_5int32(self__176)
    t352 = inline517
    var t353 int = t352 + 1
    var result__178 *_goml_vec_Array_2_5int32
    var inline515 *_goml_vec_Array_2_5int32 = vec_with_capacity__Vec_14Array_2_5int32(t353)
    result__178 = inline515
    var index__179 int = 0
    Loop_loop355:
    for {
        var t356 int
        var inline511 int = vec_len__Vec_14Array_2_5int32(self__176)
        t356 = inline511
        var t357 bool = index__179 < t356
        if t357 {
            var t358 [2]int32 = vec_get__Vec_14Array_2_5int32(self__176, index__179)
            vec_push__Vec_14Array_2_5int32(result__178, t358)
            var compound_old80 int = index__179
            var compound_value81 int = 1
            var t359 int = compound_old80 + compound_value81
            index__179 = t359
            continue
        } else {
            break Loop_loop355
        }
    }
    vec_push__Vec_14Array_2_5int32(result__178, elem__177)
    return result__178
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t371 string = _goml_runtime_core_int32_to_string(self__70)
    return t371
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t374 string = _goml_runtime_core_int_to_string(self__67)
    return t374
}

func _goml_m_trait__impl_i_PartialEq_i_string_i_eq(self__97 string, other__98 string) bool {
    var t401 bool = self__97 == other__98
    return t401
}

func _goml_m_trait__impl_i_Hash_i_string_i_hash(self__125 string) uint64 {
    var t404 uint64 = _goml_runtime_core_string_hash(self__125)
    return t404
}

func main() {
    main0()
}
