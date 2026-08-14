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
    var t254 [2]int = [2]int{31, 32}
    var t255 int = array_get__Array_2_3int(t254, 1)
    println__T_int(t255)
    var arr__2 [3]int = [3]int{1, 2, 3}
    var t256 int = array_get__Array_3_3int(arr__2, 0)
    println__T_int(t256)
    var arr2__3 [3]int = [3]int{4, 5, 6}
    var place_root190 [3]int = arr2__3
    var index191 int = 1
    array_get__Array_3_3int(place_root190, index191)
    var value193 int = 50
    var t257 [3]int = array_set__Array_3_3int(place_root190, index191, value193)
    arr2__3 = t257
    var t259 int = array_get__Array_3_3int(arr2__3, 1)
    println__T_int(t259)
    var t260 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int()
    var t261 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int(t260, 7)
    var t262 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int(t261, 8)
    var vec__4 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int(t262, 9)
    var t263 int = vec_get__Vec_3int(vec__4, 2)
    println__T_int(t263)
    var t264 *_goml_vec_int32
    var inline487 *_goml_vec_int32 = vec_new__Vec_5int32()
    t264 = inline487
    var t265 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t264, 10)
    var t266 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t265, 11)
    var vec2__5 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t266, 12)
    var index198 int = 0
    vec_get__Vec_5int32(vec2__5, index198)
    var value200 int32 = 100
    vec_set__Vec_5int32(vec2__5, index198, value200)
    var t268 int32 = vec_get__Vec_5int32(vec2__5, 0)
    var inline484 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t268)
    _goml_runtime_core_string_println(inline484)
    var s__6 []int32
    var inline480 int = 0
    var inline481 int = 2
    var inline482 []int32 = vec2__5.items[inline480:inline481]
    s__6 = inline482
    var t269 int32 = s__6[1]
    var inline477 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t269)
    _goml_runtime_core_string_println(inline477)
    var map__7 *hashmap_string_int32_x
    var inline475 *hashmap_string_int32_x = hashmap_new__HashMap_6string_5int32()
    map__7 = inline475
    var index205 string = "a"
    hashmap_get__HashMap_6string_5int32(map__7, index205)
    var value207 int32 = 13
    hashmap_set__HashMap_6string_5int32(map__7, index205, value207)
    var t271 Option__int32 = hashmap_get__HashMap_6string_5int32(map__7, "a")
    switch t271.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline471 int32 = t271.(Some)._0
        println__T_int32(inline471)
    default:
        panic("non-exhaustive match")
    }
    var t272 Option__int32 = hashmap_get__HashMap_6string_5int32(map__7, "missing")
    switch t272.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline466 int32 = t272.(Some)._0
        println__T_int32(inline466)
    default:
        panic("non-exhaustive match")
    }
    var t273 [2]int32 = [2]int32{1, 2}
    var t274 [2]int32 = [2]int32{3, 4}
    var matrix__8 [2][2]int32 = [2][2]int32{t273, t274}
    var place_root211 [2][2]int32 = matrix__8
    var index212 int = 1
    var place213 [2]int32 = array_get__Array_2_14Array_2_5int32(place_root211, index212)
    var index214 int = 0
    array_get__Array_2_5int32(place213, index214)
    var value216 int32 = 30
    var t275 [2]int32 = array_set__Array_2_5int32(place213, index214, value216)
    var t276 [2][2]int32 = array_set__Array_2_14Array_2_5int32(place_root211, index212, t275)
    matrix__8 = t276
    var t278 [2]int32 = array_get__Array_2_14Array_2_5int32(matrix__8, 1)
    var t279 int32 = array_get__Array_2_5int32(t278, 0)
    var inline462 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t279)
    _goml_runtime_core_string_println(inline462)
    var t280 [2]int = [2]int{14, 15}
    var pair__9 Tuple2_11Array2_3int_3int = Tuple2_11Array2_3int_3int{
        _0: t280,
        _1: 0,
    }
    var place_root219 Tuple2_11Array2_3int_3int = pair__9
    var place220 [2]int = place_root219._0
    var index221 int = 1
    array_get__Array_2_3int(place220, index221)
    var value223 int = 150
    var t281 [2]int = array_set__Array_2_3int(place220, index221, value223)
    var t282 int = place_root219._1
    var t283 Tuple2_11Array2_3int_3int = Tuple2_11Array2_3int_3int{
        _0: t281,
        _1: t282,
    }
    pair__9 = t283
    var t285 [2]int = pair__9._0
    var t286 int = array_get__Array_2_3int(t285, 1)
    var inline459 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t286)
    _goml_runtime_core_string_println(inline459)
    var t287 [2]int32 = [2]int32{16, 17}
    var t288 *_goml_vec_Array_2_5int32
    var inline457 *_goml_vec_Array_2_5int32 = vec_new__Vec_14Array_2_5int32()
    t288 = inline457
    var t289 [2]int32 = [2]int32{18, 19}
    var t290 *_goml_vec_Array_2_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T___l_int32_x3b_2_r_(t288, t289)
    var t291 [2]int32 = [2]int32{20, 21}
    var t292 *_goml_vec_Array_2_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T___l_int32_x3b_2_r_(t290, t291)
    var holder__10 Holder = Holder{
        data: t287,
        vecs: t292,
    }
    var place_root226 Holder = holder__10
    var place227 [2]int32 = place_root226.data
    var index228 int = 0
    array_get__Array_2_5int32(place227, index228)
    var value230 int32 = 160
    var t293 [2]int32 = array_set__Array_2_5int32(place227, index228, value230)
    var t294 *_goml_vec_Array_2_5int32 = place_root226.vecs
    var t295 Holder = Holder{
        data: t293,
        vecs: t294,
    }
    holder__10 = t295
    var t297 [2]int32 = holder__10.data
    var t298 int32 = array_get__Array_2_5int32(t297, 0)
    var inline454 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t298)
    _goml_runtime_core_string_println(inline454)
    var place_root233 Holder = holder__10
    var place234 *_goml_vec_Array_2_5int32 = place_root233.vecs
    var index235 int = 1
    var place236 [2]int32 = vec_get__Vec_14Array_2_5int32(place234, index235)
    var index237 int = 0
    array_get__Array_2_5int32(place236, index237)
    var value239 int32 = 200
    var t299 [2]int32 = array_set__Array_2_5int32(place236, index237, value239)
    vec_set__Vec_14Array_2_5int32(place234, index235, t299)
    var t301 *_goml_vec_Array_2_5int32 = holder__10.vecs
    var t302 [2]int32 = vec_get__Vec_14Array_2_5int32(t301, 1)
    var t303 int32 = array_get__Array_2_5int32(t302, 0)
    var inline451 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t303)
    _goml_runtime_core_string_println(inline451)
    var t304 [2]int32 = [2]int32{22, 23}
    var r__11 *ref_Array_2_5int32_x
    var inline449 *ref_Array_2_5int32_x = ref__Ref_14Array_2_5int32(t304)
    r__11 = inline449
    var place_root243 [2]int32 = ref_get__Ref_14Array_2_5int32(r__11)
    var index244 int = 1
    array_get__Array_2_5int32(place_root243, index244)
    var value246 int32 = 230
    var t305 [2]int32 = array_set__Array_2_5int32(place_root243, index244, value246)
    ref_set__Ref_14Array_2_5int32(r__11, t305)
    var t307 [2]int32
    var inline447 [2]int32 = ref_get__Ref_14Array_2_5int32(r__11)
    t307 = inline447
    var t308 int32 = array_get__Array_2_5int32(t307, 1)
    var inline444 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t308)
    _goml_runtime_core_string_println(inline444)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t310 string
    t310 = value__1
    _goml_runtime_core_string_println(t310)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t313 string
    var inline490 string = _goml_runtime_core_int32_to_string(value__1)
    t313 = inline490
    _goml_runtime_core_string_println(t313)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t316 string
    var inline492 string = _goml_runtime_core_int_to_string(value__1)
    t316 = inline492
    _goml_runtime_core_string_println(t316)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int() *_goml_vec_int {
    var t320 *_goml_vec_int = vec_new__Vec_3int()
    return t320
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int(self__176 *_goml_vec_int, elem__177 int) *_goml_vec_int {
    var t323 int
    var inline502 int = vec_len__Vec_3int(self__176)
    t323 = inline502
    var t324 int = t323 + 1
    var result__178 *_goml_vec_int
    var inline500 *_goml_vec_int = vec_with_capacity__Vec_3int(t324)
    result__178 = inline500
    var index__179 int = 0
    Loop_loop326:
    for {
        var t327 int
        var inline496 int = vec_len__Vec_3int(self__176)
        t327 = inline496
        var t328 bool = index__179 < t327
        if t328 {
            var t329 int = vec_get__Vec_3int(self__176, index__179)
            vec_push__Vec_3int(result__178, t329)
            var compound_old80 int = index__179
            var compound_value81 int = 1
            var t330 int = compound_old80 + compound_value81
            index__179 = t330
            continue
        } else {
            break Loop_loop326
        }
    }
    vec_push__Vec_3int(result__178, elem__177)
    return result__178
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(self__176 *_goml_vec_int32, elem__177 int32) *_goml_vec_int32 {
    var t337 int
    var inline512 int = vec_len__Vec_5int32(self__176)
    t337 = inline512
    var t338 int = t337 + 1
    var result__178 *_goml_vec_int32
    var inline510 *_goml_vec_int32 = vec_with_capacity__Vec_5int32(t338)
    result__178 = inline510
    var index__179 int = 0
    Loop_loop340:
    for {
        var t341 int
        var inline506 int = vec_len__Vec_5int32(self__176)
        t341 = inline506
        var t342 bool = index__179 < t341
        if t342 {
            var t343 int32 = vec_get__Vec_5int32(self__176, index__179)
            vec_push__Vec_5int32(result__178, t343)
            var compound_old80 int = index__179
            var compound_value81 int = 1
            var t344 int = compound_old80 + compound_value81
            index__179 = t344
            continue
        } else {
            break Loop_loop340
        }
    }
    vec_push__Vec_5int32(result__178, elem__177)
    return result__178
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T___l_int32_x3b_2_r_(self__176 *_goml_vec_Array_2_5int32, elem__177 [2]int32) *_goml_vec_Array_2_5int32 {
    var t357 int
    var inline522 int = vec_len__Vec_14Array_2_5int32(self__176)
    t357 = inline522
    var t358 int = t357 + 1
    var result__178 *_goml_vec_Array_2_5int32
    var inline520 *_goml_vec_Array_2_5int32 = vec_with_capacity__Vec_14Array_2_5int32(t358)
    result__178 = inline520
    var index__179 int = 0
    Loop_loop360:
    for {
        var t361 int
        var inline516 int = vec_len__Vec_14Array_2_5int32(self__176)
        t361 = inline516
        var t362 bool = index__179 < t361
        if t362 {
            var t363 [2]int32 = vec_get__Vec_14Array_2_5int32(self__176, index__179)
            vec_push__Vec_14Array_2_5int32(result__178, t363)
            var compound_old80 int = index__179
            var compound_value81 int = 1
            var t364 int = compound_old80 + compound_value81
            index__179 = t364
            continue
        } else {
            break Loop_loop360
        }
    }
    vec_push__Vec_14Array_2_5int32(result__178, elem__177)
    return result__178
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t376 string = _goml_runtime_core_int32_to_string(self__70)
    return t376
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t379 string = _goml_runtime_core_int_to_string(self__67)
    return t379
}

func _goml_m_trait__impl_i_PartialEq_i_string_i_eq(self__97 string, other__98 string) bool {
    var t406 bool = self__97 == other__98
    return t406
}

func _goml_m_trait__impl_i_Hash_i_string_i_hash(self__125 string) uint64 {
    var t409 uint64 = _goml_runtime_core_string_hash(self__125)
    return t409
}

func main() {
    main0()
}
