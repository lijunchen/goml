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
    var t239 [2]int = [2]int{31, 32}
    var t240 int = array_get__Array_2_3int(t239, 1)
    println__T_int(t240)
    var arr__2 [3]int = [3]int{1, 2, 3}
    var t241 int = array_get__Array_3_3int(arr__2, 0)
    println__T_int(t241)
    var arr2__3 [3]int = [3]int{4, 5, 6}
    var place_root175 [3]int = arr2__3
    var index176 int = 1
    array_get__Array_3_3int(place_root175, index176)
    var value178 int = 50
    var t242 [3]int = array_set__Array_3_3int(place_root175, index176, value178)
    arr2__3 = t242
    var t244 int = array_get__Array_3_3int(arr2__3, 1)
    println__T_int(t244)
    var t245 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int()
    var t246 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int(t245, 7)
    var t247 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int(t246, 8)
    var vec__4 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int(t247, 9)
    var t248 int = vec_get__Vec_3int(vec__4, 2)
    println__T_int(t248)
    var t249 *_goml_vec_int32
    var inline472 *_goml_vec_int32 = vec_new__Vec_5int32()
    t249 = inline472
    var t250 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t249, 10)
    var t251 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t250, 11)
    var vec2__5 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t251, 12)
    var index183 int = 0
    vec_get__Vec_5int32(vec2__5, index183)
    var value185 int32 = 100
    vec_set__Vec_5int32(vec2__5, index183, value185)
    var t253 int32 = vec_get__Vec_5int32(vec2__5, 0)
    var inline469 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t253)
    _goml_runtime_core_string_println(inline469)
    var s__6 []int32
    var inline465 int = 0
    var inline466 int = 2
    var inline467 []int32 = vec2__5.items[inline465:inline466]
    s__6 = inline467
    var t254 int32 = s__6[1]
    var inline462 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t254)
    _goml_runtime_core_string_println(inline462)
    var map__7 *hashmap_string_int32_x
    var inline460 *hashmap_string_int32_x = hashmap_new__HashMap_6string_5int32()
    map__7 = inline460
    var index190 string = "a"
    hashmap_get__HashMap_6string_5int32(map__7, index190)
    var value192 int32 = 13
    hashmap_set__HashMap_6string_5int32(map__7, index190, value192)
    var t256 Option__int32 = hashmap_get__HashMap_6string_5int32(map__7, "a")
    switch t256.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline456 int32 = t256.(Some)._0
        println__T_int32(inline456)
    default:
        panic("non-exhaustive match")
    }
    var t257 Option__int32 = hashmap_get__HashMap_6string_5int32(map__7, "missing")
    switch t257.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline451 int32 = t257.(Some)._0
        println__T_int32(inline451)
    default:
        panic("non-exhaustive match")
    }
    var t258 [2]int32 = [2]int32{1, 2}
    var t259 [2]int32 = [2]int32{3, 4}
    var matrix__8 [2][2]int32 = [2][2]int32{t258, t259}
    var place_root196 [2][2]int32 = matrix__8
    var index197 int = 1
    var place198 [2]int32 = array_get__Array_2_14Array_2_5int32(place_root196, index197)
    var index199 int = 0
    array_get__Array_2_5int32(place198, index199)
    var value201 int32 = 30
    var t260 [2]int32 = array_set__Array_2_5int32(place198, index199, value201)
    var t261 [2][2]int32 = array_set__Array_2_14Array_2_5int32(place_root196, index197, t260)
    matrix__8 = t261
    var t263 [2]int32 = array_get__Array_2_14Array_2_5int32(matrix__8, 1)
    var t264 int32 = array_get__Array_2_5int32(t263, 0)
    var inline447 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t264)
    _goml_runtime_core_string_println(inline447)
    var t265 [2]int = [2]int{14, 15}
    var pair__9 Tuple2_11Array2_3int_3int = Tuple2_11Array2_3int_3int{
        _0: t265,
        _1: 0,
    }
    var place_root204 Tuple2_11Array2_3int_3int = pair__9
    var place205 [2]int = place_root204._0
    var index206 int = 1
    array_get__Array_2_3int(place205, index206)
    var value208 int = 150
    var t266 [2]int = array_set__Array_2_3int(place205, index206, value208)
    var t267 int = place_root204._1
    var t268 Tuple2_11Array2_3int_3int = Tuple2_11Array2_3int_3int{
        _0: t266,
        _1: t267,
    }
    pair__9 = t268
    var t270 [2]int = pair__9._0
    var t271 int = array_get__Array_2_3int(t270, 1)
    var inline444 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t271)
    _goml_runtime_core_string_println(inline444)
    var t272 [2]int32 = [2]int32{16, 17}
    var t273 *_goml_vec_Array_2_5int32
    var inline442 *_goml_vec_Array_2_5int32 = vec_new__Vec_14Array_2_5int32()
    t273 = inline442
    var t274 [2]int32 = [2]int32{18, 19}
    var t275 *_goml_vec_Array_2_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T___l_int32_x3b_2_r_(t273, t274)
    var t276 [2]int32 = [2]int32{20, 21}
    var t277 *_goml_vec_Array_2_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T___l_int32_x3b_2_r_(t275, t276)
    var holder__10 Holder = Holder{
        data: t272,
        vecs: t277,
    }
    var place_root211 Holder = holder__10
    var place212 [2]int32 = place_root211.data
    var index213 int = 0
    array_get__Array_2_5int32(place212, index213)
    var value215 int32 = 160
    var t278 [2]int32 = array_set__Array_2_5int32(place212, index213, value215)
    var t279 *_goml_vec_Array_2_5int32 = place_root211.vecs
    var t280 Holder = Holder{
        data: t278,
        vecs: t279,
    }
    holder__10 = t280
    var t282 [2]int32 = holder__10.data
    var t283 int32 = array_get__Array_2_5int32(t282, 0)
    var inline439 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t283)
    _goml_runtime_core_string_println(inline439)
    var place_root218 Holder = holder__10
    var place219 *_goml_vec_Array_2_5int32 = place_root218.vecs
    var index220 int = 1
    var place221 [2]int32 = vec_get__Vec_14Array_2_5int32(place219, index220)
    var index222 int = 0
    array_get__Array_2_5int32(place221, index222)
    var value224 int32 = 200
    var t284 [2]int32 = array_set__Array_2_5int32(place221, index222, value224)
    vec_set__Vec_14Array_2_5int32(place219, index220, t284)
    var t286 *_goml_vec_Array_2_5int32 = holder__10.vecs
    var t287 [2]int32 = vec_get__Vec_14Array_2_5int32(t286, 1)
    var t288 int32 = array_get__Array_2_5int32(t287, 0)
    var inline436 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t288)
    _goml_runtime_core_string_println(inline436)
    var t289 [2]int32 = [2]int32{22, 23}
    var r__11 *ref_Array_2_5int32_x
    var inline434 *ref_Array_2_5int32_x = ref__Ref_14Array_2_5int32(t289)
    r__11 = inline434
    var place_root228 [2]int32 = ref_get__Ref_14Array_2_5int32(r__11)
    var index229 int = 1
    array_get__Array_2_5int32(place_root228, index229)
    var value231 int32 = 230
    var t290 [2]int32 = array_set__Array_2_5int32(place_root228, index229, value231)
    ref_set__Ref_14Array_2_5int32(r__11, t290)
    var t292 [2]int32
    var inline432 [2]int32 = ref_get__Ref_14Array_2_5int32(r__11)
    t292 = inline432
    var t293 int32 = array_get__Array_2_5int32(t292, 1)
    var inline429 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t293)
    _goml_runtime_core_string_println(inline429)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t295 string
    t295 = value__31
    _goml_runtime_core_string_println(t295)
    return struct{}{}
}

func println__T_int32(value__31 int32) struct{} {
    var t298 string
    var inline475 string = _goml_runtime_core_int32_to_string(value__31)
    t298 = inline475
    _goml_runtime_core_string_println(t298)
    return struct{}{}
}

func println__T_int(value__31 int) struct{} {
    var t301 string
    var inline477 string = _goml_runtime_core_int_to_string(value__31)
    t301 = inline477
    _goml_runtime_core_string_println(t301)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int() *_goml_vec_int {
    var t305 *_goml_vec_int = vec_new__Vec_3int()
    return t305
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int(self__178 *_goml_vec_int, elem__179 int) *_goml_vec_int {
    var t308 int
    var inline487 int = vec_len__Vec_3int(self__178)
    t308 = inline487
    var t309 int = t308 + 1
    var result__180 *_goml_vec_int
    var inline485 *_goml_vec_int = vec_with_capacity__Vec_3int(t309)
    result__180 = inline485
    var index__181 int = 0
    Loop_loop311:
    for {
        var t312 int
        var inline481 int = vec_len__Vec_3int(self__178)
        t312 = inline481
        var t313 bool = index__181 < t312
        if t313 {
            var t314 int = vec_get__Vec_3int(self__178, index__181)
            vec_push__Vec_3int(result__180, t314)
            var compound_old80 int = index__181
            var compound_value81 int = 1
            var t315 int = compound_old80 + compound_value81
            index__181 = t315
            continue
        } else {
            break Loop_loop311
        }
    }
    vec_push__Vec_3int(result__180, elem__179)
    return result__180
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(self__178 *_goml_vec_int32, elem__179 int32) *_goml_vec_int32 {
    var t322 int
    var inline497 int = vec_len__Vec_5int32(self__178)
    t322 = inline497
    var t323 int = t322 + 1
    var result__180 *_goml_vec_int32
    var inline495 *_goml_vec_int32 = vec_with_capacity__Vec_5int32(t323)
    result__180 = inline495
    var index__181 int = 0
    Loop_loop325:
    for {
        var t326 int
        var inline491 int = vec_len__Vec_5int32(self__178)
        t326 = inline491
        var t327 bool = index__181 < t326
        if t327 {
            var t328 int32 = vec_get__Vec_5int32(self__178, index__181)
            vec_push__Vec_5int32(result__180, t328)
            var compound_old80 int = index__181
            var compound_value81 int = 1
            var t329 int = compound_old80 + compound_value81
            index__181 = t329
            continue
        } else {
            break Loop_loop325
        }
    }
    vec_push__Vec_5int32(result__180, elem__179)
    return result__180
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T___l_int32_x3b_2_r_(self__178 *_goml_vec_Array_2_5int32, elem__179 [2]int32) *_goml_vec_Array_2_5int32 {
    var t342 int
    var inline507 int = vec_len__Vec_14Array_2_5int32(self__178)
    t342 = inline507
    var t343 int = t342 + 1
    var result__180 *_goml_vec_Array_2_5int32
    var inline505 *_goml_vec_Array_2_5int32 = vec_with_capacity__Vec_14Array_2_5int32(t343)
    result__180 = inline505
    var index__181 int = 0
    Loop_loop345:
    for {
        var t346 int
        var inline501 int = vec_len__Vec_14Array_2_5int32(self__178)
        t346 = inline501
        var t347 bool = index__181 < t346
        if t347 {
            var t348 [2]int32 = vec_get__Vec_14Array_2_5int32(self__178, index__181)
            vec_push__Vec_14Array_2_5int32(result__180, t348)
            var compound_old80 int = index__181
            var compound_value81 int = 1
            var t349 int = compound_old80 + compound_value81
            index__181 = t349
            continue
        } else {
            break Loop_loop345
        }
    }
    vec_push__Vec_14Array_2_5int32(result__180, elem__179)
    return result__180
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t361 string = _goml_runtime_core_int32_to_string(self__72)
    return t361
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t364 string = _goml_runtime_core_int_to_string(self__69)
    return t364
}

func _goml_m_trait__impl_i_PartialEq_i_string_i_eq(self__99 string, other__100 string) bool {
    var t391 bool = self__99 == other__100
    return t391
}

func _goml_m_trait__impl_i_Hash_i_string_i_hash(self__127 string) uint64 {
    var t394 uint64 = _goml_runtime_core_string_hash(self__127)
    return t394
}

func main() {
    main0()
}
