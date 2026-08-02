package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
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

func vec_set__Vec_3int(vec *_goml_vec_int, index int, value int) struct{} {
    vec.items[index] = value
    return struct{}{}
}

func vec_len__Vec_3int(vec *_goml_vec_int) int {
    return int(len(vec.items))
}

type ref_string_x struct {
    value string
}

func ref__Ref_6string(value string) *ref_string_x {
    return &ref_string_x{
        value: value,
    }
}

func ref_get__Ref_6string(reference *ref_string_x) string {
    return reference.value
}

func ref_set__Ref_6string(reference *ref_string_x, value string) struct{} {
    reference.value = value
    return struct{}{}
}

type hashmap_string_int_x_entry struct {
    active bool
    key string
    value int
}

type hashmap_string_int_x struct {
    buckets map[uint64][]hashmap_string_int_x_entry
    hashes []uint64
    len int
}

func hashmap_new__HashMap_6string_3int() *hashmap_string_int_x {
    return &hashmap_string_int_x{
        buckets: make(map[uint64][]hashmap_string_int_x_entry),
        len: 0,
        hashes: nil,
    }
}

func hashmap_len__HashMap_6string_3int(m *hashmap_string_int_x) int {
    if m == nil {
        return 0
    }
    return m.len
}

func hashmap_lookup__HashMap_6string_3int(m *hashmap_string_int_x, key string) (int, bool) {
    if m == nil {
        var zero int
        return zero, false
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_string_i_hash(key)
    var bucket []hashmap_string_int_x_entry = m.buckets[h]
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_string_int_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_Eq_i_string_i_eq(entry.key, key) {
            return entry.value, true
        }
        i = i + 1
    }
    var zero int
    return zero, false
}

func hashmap_get__HashMap_6string_3int(m *hashmap_string_int_x, key string) Option__int {
    var value int
    var ok bool
    value, ok = hashmap_lookup__HashMap_6string_3int(m, key)
    if ok {
        return Some{
            _0: value,
        }
    }
    return None{}
}

func hashmap_set__HashMap_6string_3int(m *hashmap_string_int_x, key string, value int) struct{} {
    var reuse_index int = -1
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_string_i_hash(key)
    var bucket []hashmap_string_int_x_entry = m.buckets[h]
    if len(bucket) == 0 {
        m.hashes = append(m.hashes, h)
    }
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_string_int_x_entry = bucket[i]
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
        bucket[reuse_index] = hashmap_string_int_x_entry{
            active: true,
            key: key,
            value: value,
        }
        m.len = m.len + 1
        return struct{}{}
    }
    bucket = append(bucket, hashmap_string_int_x_entry{
        active: true,
        key: key,
        value: value,
    })
    m.buckets[h] = bucket
    m.len = m.len + 1
    return struct{}{}
}

type Tuple2_3int_3int struct {
    _0 int
    _1 int
}

type Point struct {
    x int
    y int
}

type Option__int interface {
    isOption__int()
}

type None struct {}

func (_ None) isOption__int() {}

type Some struct {
    _0 int
}

func (_ Some) isOption__int() {}

func record(log__0 *ref_string_x, label__1 string, value__2 int) int {
    var t201 string
    var inline328 string = ref_get__Ref_6string(log__0)
    t201 = inline328
    var t202 string = t201 + label__1
    ref_set__Ref_6string(log__0, t202)
    return value__2
}

func record_point(log__3 *ref_string_x, label__4 string, value__5 Point) Point {
    var t205 string
    var inline332 string = ref_get__Ref_6string(log__3)
    t205 = inline332
    var t206 string = t205 + label__4
    ref_set__Ref_6string(log__3, t206)
    return value__5
}

func record_vec(log__6 *ref_string_x, label__7 string, value__8 *_goml_vec_int) *_goml_vec_int {
    var t209 string
    var inline336 string = ref_get__Ref_6string(log__6)
    t209 = inline336
    var t210 string = t209 + label__7
    ref_set__Ref_6string(log__6, t210)
    return value__8
}

func main0() struct{} {
    var number__9 int = 5
    var compound_old158 int = number__9
    var compound_value159 int = 3
    var t212 int = compound_old158 + compound_value159
    number__9 = t212
    var compound_old161 int = number__9
    var compound_value162 int = 2
    var t214 int = compound_old161 * compound_value162
    number__9 = t214
    var compound_old164 int = number__9
    var compound_value165 int = 1
    var t216 int = compound_old164 >> compound_value165
    number__9 = t216
    var t218 string = _goml_m_inherent_i_int_i_int_i_to__string(number__9)
    println__T_string(t218)
    var direct__10 Point = Point{
        x: 3,
        y: 4,
    }
    var place_root168 Point = direct__10
    var place169 int = place_root168.x
    var value170 int = 5
    var t219 int = place169 + value170
    var t220 int = place_root168.y
    var t221 Point = Point{
        x: t219,
        y: t220,
    }
    direct__10 = t221
    var t223 int = direct__10.x
    var t224 string = _goml_m_inherent_i_int_i_int_i_to__string(t223)
    var t225 string = "" + t224
    var t226 string = t225 + ","
    var t227 int = direct__10.y
    var t228 string = _goml_m_inherent_i_int_i_int_i_to__string(t227)
    var t229 string = t226 + t228
    println__T_string(t229)
    var pair__11 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: 2,
        _1: 9,
    }
    var place_root173 Tuple2_3int_3int = pair__11
    var place174 int = place_root173._0
    var value175 int = 3
    var t230 int = place174 * value175
    var t231 int = place_root173._1
    var t232 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: t230,
        _1: t231,
    }
    pair__11 = t232
    var t234 int = pair__11._0
    var t235 string = _goml_m_inherent_i_int_i_int_i_to__string(t234)
    var t236 string = "" + t235
    var t237 string = t236 + ","
    var t238 int = pair__11._1
    var t239 string = _goml_m_inherent_i_int_i_int_i_to__string(t238)
    var t240 string = t237 + t239
    println__T_string(t240)
    var log__12 *ref_string_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string("")
    var base__13 Point = Point{
        x: 1,
        y: 2,
    }
    var struct_update_field__780__0 int = record(log__12, "F", 7)
    var struct_update_base__780 Point = record_point(log__12, "B", base__13)
    var t241 int = struct_update_base__780.y
    var t243 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    println__T_string(t243)
    var t245 string = _goml_m_inherent_i_int_i_int_i_to__string(struct_update_field__780__0)
    var t246 string = "" + t245
    var t247 string = t246 + ","
    var t249 string = _goml_m_inherent_i_int_i_int_i_to__string(t241)
    var t250 string = t247 + t249
    println__T_string(t250)
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__12, "")
    var vec_literal__967 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int()
    var t251 int = record(log__12, "A", 10)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__967, t251)
    var t252 int = record(log__12, "B", 20)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__967, t252)
    var place_root183 *_goml_vec_int = record_vec(log__12, "R", vec_literal__967)
    var index184 int = record(log__12, "I", 1)
    var place185 int = vec_get__Vec_3int(place_root183, index184)
    var value186 int = record(log__12, "V", 5)
    var t253 int = place185 + value186
    vec_set__Vec_3int(place_root183, index184, t253)
    var t255 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    println__T_string(t255)
    var t256 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(vec_literal__967, 0)
    var t257 string = _goml_m_inherent_i_int_i_int_i_to__string(t256)
    var t258 string = "" + t257
    var t259 string = t258 + ","
    var t260 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(vec_literal__967, 1)
    var t261 string = _goml_m_inherent_i_int_i_int_i_to__string(t260)
    var t262 string = t259 + t261
    println__T_string(t262)
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__12, "")
    var hashmap_literal__1226 *hashmap_string_int_x
    var inline392 *hashmap_string_int_x = hashmap_new__HashMap_6string_3int()
    hashmap_literal__1226 = inline392
    var t263 string = "" + "k"
    var t264 int = record(log__12, "K", 1)
    var t265 string
    var inline390 string = _goml_runtime_core_int_to_string(t264)
    t265 = inline390
    var t266 string = t263 + t265
    var t267 int
    var inline384 string = "V"
    var inline385 int = 11
    var inline386 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    var inline387 string = inline386 + inline384
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__12, inline387)
    t267 = inline385
    hashmap_set__HashMap_6string_3int(hashmap_literal__1226, t266, t267)
    var t268 int
    var inline376 string = "A"
    var inline377 int = 1
    var inline378 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    var inline379 string = inline378 + inline376
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__12, inline379)
    t268 = inline377
    var inline373 string = "same"
    hashmap_set__HashMap_6string_3int(hashmap_literal__1226, inline373, t268)
    var t269 int
    var inline367 string = "B"
    var inline368 int = 2
    var inline369 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    var inline370 string = inline369 + inline367
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__12, inline370)
    t269 = inline368
    var inline364 string = "same"
    hashmap_set__HashMap_6string_3int(hashmap_literal__1226, inline364, t269)
    var t270 string
    var inline362 string = ref_get__Ref_6string(log__12)
    t270 = inline362
    var inline359 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t270)
    _goml_runtime_core_string_println(inline359)
    var mtmp195 Option__int
    var inline356 string = "same"
    var inline357 Option__int = hashmap_get__HashMap_6string_3int(hashmap_literal__1226, inline356)
    mtmp195 = inline357
    var jp272 string
    switch mtmp195.(type) {
    case None:
        jp272 = "missing"
    case Some:
        var x196 int = mtmp195.(Some)._0
        var inline338 string = _goml_runtime_core_int_to_string(x196)
        jp272 = inline338
    default:
        panic("non-exhaustive match")
    }
    var inline353 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp272)
    _goml_runtime_core_string_println(inline353)
    var vec_literal__1570 *_goml_vec_int
    var inline351 *_goml_vec_int = vec_new__Vec_3int()
    vec_literal__1570 = inline351
    var hashmap_literal__1623 *hashmap_string_int_x
    var inline349 *hashmap_string_int_x = hashmap_new__HashMap_6string_3int()
    hashmap_literal__1623 = inline349
    var t273 string = "" + "empty="
    var t274 int
    var inline347 int = vec_len__Vec_3int(vec_literal__1570)
    t274 = inline347
    var t275 int
    var inline345 int = hashmap_len__HashMap_6string_3int(hashmap_literal__1623)
    t275 = inline345
    var t276 int = t274 + t275
    var t277 string
    var inline343 string = _goml_runtime_core_int_to_string(t276)
    t277 = inline343
    var t278 string = t273 + t277
    var t279 string = t278 + " {ok}"
    var inline340 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t279)
    _goml_runtime_core_string_println(inline340)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(self__208 *ref_string_x) string {
    var t283 string = ref_get__Ref_6string(self__208)
    return t283
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(self__209 *ref_string_x, value__210 string) struct{} {
    ref_set__Ref_6string(self__209, value__210)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t287 string
    t287 = value__1
    _goml_runtime_core_string_println(t287)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var t291 string = _goml_runtime_core_int_to_string(self__5)
    return t291
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string(value__207 string) *ref_string_x {
    var t294 *ref_string_x = ref__Ref_6string(value__207)
    return t294
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int() *_goml_vec_int {
    var t297 *_goml_vec_int = vec_new__Vec_3int()
    return t297
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(self__126 *_goml_vec_int, elem__127 int) struct{} {
    vec_push__Vec_3int(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(self__132 *_goml_vec_int, index__133 int) int {
    var t302 int = vec_get__Vec_3int(self__132, index__133)
    return t302
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__55 string, other__56 string) bool {
    var t321 bool = self__55 == other__56
    return t321
}

func _goml_m_trait__impl_i_Hash_i_string_i_hash(self__83 string) uint64 {
    var t324 uint64 = _goml_runtime_core_string_hash(self__83)
    return t324
}

func main() {
    main0()
}
