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
    var t203 [2]int = [2]int{31, 32}
    var t204 int = array_get__Array_2_3int(t203, 1)
    println__T_int(t204)
    var arr__2 [3]int = [3]int{1, 2, 3}
    var t205 int = array_get__Array_3_3int(arr__2, 0)
    println__T_int(t205)
    var arr2__3 [3]int = [3]int{4, 5, 6}
    var place_root139 [3]int = arr2__3
    var index140 int = 1
    array_get__Array_3_3int(place_root139, index140)
    var value142 int = 50
    var t206 [3]int = array_set__Array_3_3int(place_root139, index140, value142)
    arr2__3 = t206
    var t208 int = array_get__Array_3_3int(arr2__3, 1)
    println__T_int(t208)
    var t209 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int()
    var t210 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int(t209, 7)
    var t211 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int(t210, 8)
    var vec__4 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int(t211, 9)
    var t212 int = vec_get__Vec_3int(vec__4, 2)
    println__T_int(t212)
    var t213 *_goml_vec_int32
    var inline410 *_goml_vec_int32 = vec_new__Vec_5int32()
    t213 = inline410
    var t214 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t213, 10)
    var t215 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t214, 11)
    var vec2__5 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t215, 12)
    var index147 int = 0
    vec_get__Vec_5int32(vec2__5, index147)
    var value149 int32 = 100
    vec_set__Vec_5int32(vec2__5, index147, value149)
    var t217 int32 = vec_get__Vec_5int32(vec2__5, 0)
    var inline407 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t217)
    _goml_runtime_core_string_println(inline407)
    var s__6 []int32
    var inline403 int = 0
    var inline404 int = 2
    var inline405 []int32 = vec2__5.items[inline403:inline404]
    s__6 = inline405
    var t218 int32 = s__6[1]
    var inline400 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t218)
    _goml_runtime_core_string_println(inline400)
    var map__7 *hashmap_string_int32_x
    var inline398 *hashmap_string_int32_x = hashmap_new__HashMap_6string_5int32()
    map__7 = inline398
    var index154 string = "a"
    hashmap_get__HashMap_6string_5int32(map__7, index154)
    var value156 int32 = 13
    hashmap_set__HashMap_6string_5int32(map__7, index154, value156)
    var t220 Option__int32 = hashmap_get__HashMap_6string_5int32(map__7, "a")
    switch t220.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline394 int32 = t220.(Some)._0
        println__T_int32(inline394)
    default:
        panic("non-exhaustive match")
    }
    var t221 Option__int32 = hashmap_get__HashMap_6string_5int32(map__7, "missing")
    switch t221.(type) {
    case None:
        println__T_string("none")
    case Some:
        var inline389 int32 = t221.(Some)._0
        println__T_int32(inline389)
    default:
        panic("non-exhaustive match")
    }
    var t222 [2]int32 = [2]int32{1, 2}
    var t223 [2]int32 = [2]int32{3, 4}
    var matrix__8 [2][2]int32 = [2][2]int32{t222, t223}
    var place_root160 [2][2]int32 = matrix__8
    var index161 int = 1
    var place162 [2]int32 = array_get__Array_2_14Array_2_5int32(place_root160, index161)
    var index163 int = 0
    array_get__Array_2_5int32(place162, index163)
    var value165 int32 = 30
    var t224 [2]int32 = array_set__Array_2_5int32(place162, index163, value165)
    var t225 [2][2]int32 = array_set__Array_2_14Array_2_5int32(place_root160, index161, t224)
    matrix__8 = t225
    var t227 [2]int32 = array_get__Array_2_14Array_2_5int32(matrix__8, 1)
    var t228 int32 = array_get__Array_2_5int32(t227, 0)
    var inline385 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t228)
    _goml_runtime_core_string_println(inline385)
    var t229 [2]int = [2]int{14, 15}
    var pair__9 Tuple2_11Array2_3int_3int = Tuple2_11Array2_3int_3int{
        _0: t229,
        _1: 0,
    }
    var place_root168 Tuple2_11Array2_3int_3int = pair__9
    var place169 [2]int = place_root168._0
    var index170 int = 1
    array_get__Array_2_3int(place169, index170)
    var value172 int = 150
    var t230 [2]int = array_set__Array_2_3int(place169, index170, value172)
    var t231 int = place_root168._1
    var t232 Tuple2_11Array2_3int_3int = Tuple2_11Array2_3int_3int{
        _0: t230,
        _1: t231,
    }
    pair__9 = t232
    var t234 [2]int = pair__9._0
    var t235 int = array_get__Array_2_3int(t234, 1)
    var inline382 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t235)
    _goml_runtime_core_string_println(inline382)
    var t236 [2]int32 = [2]int32{16, 17}
    var t237 *_goml_vec_Array_2_5int32
    var inline380 *_goml_vec_Array_2_5int32 = vec_new__Vec_14Array_2_5int32()
    t237 = inline380
    var t238 [2]int32 = [2]int32{18, 19}
    var t239 *_goml_vec_Array_2_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T___l_int32_x3b_2_r_(t237, t238)
    var t240 [2]int32 = [2]int32{20, 21}
    var t241 *_goml_vec_Array_2_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T___l_int32_x3b_2_r_(t239, t240)
    var holder__10 Holder = Holder{
        data: t236,
        vecs: t241,
    }
    var place_root175 Holder = holder__10
    var place176 [2]int32 = place_root175.data
    var index177 int = 0
    array_get__Array_2_5int32(place176, index177)
    var value179 int32 = 160
    var t242 [2]int32 = array_set__Array_2_5int32(place176, index177, value179)
    var t243 *_goml_vec_Array_2_5int32 = place_root175.vecs
    var t244 Holder = Holder{
        data: t242,
        vecs: t243,
    }
    holder__10 = t244
    var t246 [2]int32 = holder__10.data
    var t247 int32 = array_get__Array_2_5int32(t246, 0)
    var inline377 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t247)
    _goml_runtime_core_string_println(inline377)
    var place_root182 Holder = holder__10
    var place183 *_goml_vec_Array_2_5int32 = place_root182.vecs
    var index184 int = 1
    var place185 [2]int32 = vec_get__Vec_14Array_2_5int32(place183, index184)
    var index186 int = 0
    array_get__Array_2_5int32(place185, index186)
    var value188 int32 = 200
    var t248 [2]int32 = array_set__Array_2_5int32(place185, index186, value188)
    vec_set__Vec_14Array_2_5int32(place183, index184, t248)
    var t250 *_goml_vec_Array_2_5int32 = holder__10.vecs
    var t251 [2]int32 = vec_get__Vec_14Array_2_5int32(t250, 1)
    var t252 int32 = array_get__Array_2_5int32(t251, 0)
    var inline374 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t252)
    _goml_runtime_core_string_println(inline374)
    var t253 [2]int32 = [2]int32{22, 23}
    var r__11 *ref_Array_2_5int32_x
    var inline372 *ref_Array_2_5int32_x = ref__Ref_14Array_2_5int32(t253)
    r__11 = inline372
    var place_root192 [2]int32 = ref_get__Ref_14Array_2_5int32(r__11)
    var index193 int = 1
    array_get__Array_2_5int32(place_root192, index193)
    var value195 int32 = 230
    var t254 [2]int32 = array_set__Array_2_5int32(place_root192, index193, value195)
    ref_set__Ref_14Array_2_5int32(r__11, t254)
    var t256 [2]int32
    var inline370 [2]int32 = ref_get__Ref_14Array_2_5int32(r__11)
    t256 = inline370
    var t257 int32 = array_get__Array_2_5int32(t256, 1)
    var inline367 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t257)
    _goml_runtime_core_string_println(inline367)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t259 string
    t259 = value__31
    _goml_runtime_core_string_println(t259)
    return struct{}{}
}

func println__T_int32(value__31 int32) struct{} {
    var t262 string
    var inline413 string = _goml_runtime_core_int32_to_string(value__31)
    t262 = inline413
    _goml_runtime_core_string_println(t262)
    return struct{}{}
}

func println__T_int(value__31 int) struct{} {
    var t265 string
    var inline415 string = _goml_runtime_core_int_to_string(value__31)
    t265 = inline415
    _goml_runtime_core_string_println(t265)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int() *_goml_vec_int {
    var t269 *_goml_vec_int = vec_new__Vec_3int()
    return t269
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int(self__153 *_goml_vec_int, elem__154 int) *_goml_vec_int {
    var t272 int
    var inline425 int = vec_len__Vec_3int(self__153)
    t272 = inline425
    var t273 int = t272 + 1
    var result__155 *_goml_vec_int
    var inline423 *_goml_vec_int = vec_with_capacity__Vec_3int(t273)
    result__155 = inline423
    var index__156 int = 0
    Loop_loop275:
    for {
        var t276 int
        var inline419 int = vec_len__Vec_3int(self__153)
        t276 = inline419
        var t277 bool = index__156 < t276
        if t277 {
            var t278 int = vec_get__Vec_3int(self__153, index__156)
            vec_push__Vec_3int(result__155, t278)
            var compound_old44 int = index__156
            var compound_value45 int = 1
            var t279 int = compound_old44 + compound_value45
            index__156 = t279
            continue
        } else {
            break Loop_loop275
        }
    }
    vec_push__Vec_3int(result__155, elem__154)
    return result__155
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(self__153 *_goml_vec_int32, elem__154 int32) *_goml_vec_int32 {
    var t286 int
    var inline435 int = vec_len__Vec_5int32(self__153)
    t286 = inline435
    var t287 int = t286 + 1
    var result__155 *_goml_vec_int32
    var inline433 *_goml_vec_int32 = vec_with_capacity__Vec_5int32(t287)
    result__155 = inline433
    var index__156 int = 0
    Loop_loop289:
    for {
        var t290 int
        var inline429 int = vec_len__Vec_5int32(self__153)
        t290 = inline429
        var t291 bool = index__156 < t290
        if t291 {
            var t292 int32 = vec_get__Vec_5int32(self__153, index__156)
            vec_push__Vec_5int32(result__155, t292)
            var compound_old44 int = index__156
            var compound_value45 int = 1
            var t293 int = compound_old44 + compound_value45
            index__156 = t293
            continue
        } else {
            break Loop_loop289
        }
    }
    vec_push__Vec_5int32(result__155, elem__154)
    return result__155
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T___l_int32_x3b_2_r_(self__153 *_goml_vec_Array_2_5int32, elem__154 [2]int32) *_goml_vec_Array_2_5int32 {
    var t306 int
    var inline445 int = vec_len__Vec_14Array_2_5int32(self__153)
    t306 = inline445
    var t307 int = t306 + 1
    var result__155 *_goml_vec_Array_2_5int32
    var inline443 *_goml_vec_Array_2_5int32 = vec_with_capacity__Vec_14Array_2_5int32(t307)
    result__155 = inline443
    var index__156 int = 0
    Loop_loop309:
    for {
        var t310 int
        var inline439 int = vec_len__Vec_14Array_2_5int32(self__153)
        t310 = inline439
        var t311 bool = index__156 < t310
        if t311 {
            var t312 [2]int32 = vec_get__Vec_14Array_2_5int32(self__153, index__156)
            vec_push__Vec_14Array_2_5int32(result__155, t312)
            var compound_old44 int = index__156
            var compound_value45 int = 1
            var t313 int = compound_old44 + compound_value45
            index__156 = t313
            continue
        } else {
            break Loop_loop309
        }
    }
    vec_push__Vec_14Array_2_5int32(result__155, elem__154)
    return result__155
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t325 string = _goml_runtime_core_int32_to_string(self__72)
    return t325
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t328 string = _goml_runtime_core_int_to_string(self__69)
    return t328
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__99 string, other__100 string) bool {
    var t355 bool = self__99 == other__100
    return t355
}

func _goml_m_trait__impl_i_Hash_i_string_i_hash(self__127 string) uint64 {
    var t358 uint64 = _goml_runtime_core_string_hash(self__127)
    return t358
}

func main() {
    main0()
}
