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
    var t223 string
    var inline350 string = ref_get__Ref_6string(log__0)
    t223 = inline350
    var t224 string = t223 + label__1
    ref_set__Ref_6string(log__0, t224)
    return value__2
}

func record_point(log__3 *ref_string_x, label__4 string, value__5 Point) Point {
    var t227 string
    var inline354 string = ref_get__Ref_6string(log__3)
    t227 = inline354
    var t228 string = t227 + label__4
    ref_set__Ref_6string(log__3, t228)
    return value__5
}

func record_vec(log__6 *ref_string_x, label__7 string, value__8 *_goml_vec_int) *_goml_vec_int {
    var t231 string
    var inline358 string = ref_get__Ref_6string(log__6)
    t231 = inline358
    var t232 string = t231 + label__7
    ref_set__Ref_6string(log__6, t232)
    return value__8
}

func main0() struct{} {
    var number__9 int = 5
    var compound_old180 int = number__9
    var compound_value181 int = 3
    var t234 int = compound_old180 + compound_value181
    number__9 = t234
    var compound_old183 int = number__9
    var compound_value184 int = 2
    var t236 int = compound_old183 * compound_value184
    number__9 = t236
    var compound_old186 int = number__9
    var compound_value187 int = 1
    var t238 int = compound_old186 >> compound_value187
    number__9 = t238
    var t240 string = _goml_m_inherent_i_int_i_int_i_to__string(number__9)
    println__T_string(t240)
    var direct__10 Point = Point{
        x: 3,
        y: 4,
    }
    var place_root190 Point = direct__10
    var place191 int = place_root190.x
    var value192 int = 5
    var t241 int = place191 + value192
    var t242 int = place_root190.y
    var t243 Point = Point{
        x: t241,
        y: t242,
    }
    direct__10 = t243
    var t245 int = direct__10.x
    var t246 string = _goml_m_inherent_i_int_i_int_i_to__string(t245)
    var t247 string = "" + t246
    var t248 string = t247 + ","
    var t249 int = direct__10.y
    var t250 string = _goml_m_inherent_i_int_i_int_i_to__string(t249)
    var t251 string = t248 + t250
    println__T_string(t251)
    var pair__11 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: 2,
        _1: 9,
    }
    var place_root195 Tuple2_3int_3int = pair__11
    var place196 int = place_root195._0
    var value197 int = 3
    var t252 int = place196 * value197
    var t253 int = place_root195._1
    var t254 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: t252,
        _1: t253,
    }
    pair__11 = t254
    var t256 int = pair__11._0
    var t257 string = _goml_m_inherent_i_int_i_int_i_to__string(t256)
    var t258 string = "" + t257
    var t259 string = t258 + ","
    var t260 int = pair__11._1
    var t261 string = _goml_m_inherent_i_int_i_int_i_to__string(t260)
    var t262 string = t259 + t261
    println__T_string(t262)
    var log__12 *ref_string_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string("")
    var base__13 Point = Point{
        x: 1,
        y: 2,
    }
    var struct_update_field__780__0 int = record(log__12, "F", 7)
    var struct_update_base__780 Point = record_point(log__12, "B", base__13)
    var t263 int = struct_update_base__780.y
    var t265 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    println__T_string(t265)
    var t267 string = _goml_m_inherent_i_int_i_int_i_to__string(struct_update_field__780__0)
    var t268 string = "" + t267
    var t269 string = t268 + ","
    var t271 string = _goml_m_inherent_i_int_i_int_i_to__string(t263)
    var t272 string = t269 + t271
    println__T_string(t272)
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__12, "")
    var vec_literal__967 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int()
    var t273 int = record(log__12, "A", 10)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__967, t273)
    var t274 int = record(log__12, "B", 20)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__967, t274)
    var place_root205 *_goml_vec_int = record_vec(log__12, "R", vec_literal__967)
    var index206 int = record(log__12, "I", 1)
    var place207 int = vec_get__Vec_3int(place_root205, index206)
    var value208 int = record(log__12, "V", 5)
    var t275 int = place207 + value208
    vec_set__Vec_3int(place_root205, index206, t275)
    var t277 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    println__T_string(t277)
    var t278 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(vec_literal__967, 0)
    var t279 string = _goml_m_inherent_i_int_i_int_i_to__string(t278)
    var t280 string = "" + t279
    var t281 string = t280 + ","
    var t282 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(vec_literal__967, 1)
    var t283 string = _goml_m_inherent_i_int_i_int_i_to__string(t282)
    var t284 string = t281 + t283
    println__T_string(t284)
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__12, "")
    var hashmap_literal__1226 *hashmap_string_int_x
    var inline414 *hashmap_string_int_x = hashmap_new__HashMap_6string_3int()
    hashmap_literal__1226 = inline414
    var t285 string = "" + "k"
    var t286 int = record(log__12, "K", 1)
    var t287 string
    var inline412 string = _goml_runtime_core_int_to_string(t286)
    t287 = inline412
    var t288 string = t285 + t287
    var t289 int
    var inline406 string = "V"
    var inline407 int = 11
    var inline408 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    var inline409 string = inline408 + inline406
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__12, inline409)
    t289 = inline407
    hashmap_set__HashMap_6string_3int(hashmap_literal__1226, t288, t289)
    var t290 int
    var inline398 string = "A"
    var inline399 int = 1
    var inline400 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    var inline401 string = inline400 + inline398
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__12, inline401)
    t290 = inline399
    var inline395 string = "same"
    hashmap_set__HashMap_6string_3int(hashmap_literal__1226, inline395, t290)
    var t291 int
    var inline389 string = "B"
    var inline390 int = 2
    var inline391 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    var inline392 string = inline391 + inline389
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__12, inline392)
    t291 = inline390
    var inline386 string = "same"
    hashmap_set__HashMap_6string_3int(hashmap_literal__1226, inline386, t291)
    var t292 string
    var inline384 string = ref_get__Ref_6string(log__12)
    t292 = inline384
    var inline381 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t292)
    _goml_runtime_core_string_println(inline381)
    var mtmp217 Option__int
    var inline378 string = "same"
    var inline379 Option__int = hashmap_get__HashMap_6string_3int(hashmap_literal__1226, inline378)
    mtmp217 = inline379
    var jp294 string
    switch mtmp217.(type) {
    case None:
        jp294 = "missing"
    case Some:
        var x218 int = mtmp217.(Some)._0
        var inline360 string = _goml_runtime_core_int_to_string(x218)
        jp294 = inline360
    default:
        panic("non-exhaustive match")
    }
    var inline375 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp294)
    _goml_runtime_core_string_println(inline375)
    var vec_literal__1570 *_goml_vec_int
    var inline373 *_goml_vec_int = vec_new__Vec_3int()
    vec_literal__1570 = inline373
    var hashmap_literal__1623 *hashmap_string_int_x
    var inline371 *hashmap_string_int_x = hashmap_new__HashMap_6string_3int()
    hashmap_literal__1623 = inline371
    var t295 string = "" + "empty="
    var t296 int
    var inline369 int = vec_len__Vec_3int(vec_literal__1570)
    t296 = inline369
    var t297 int
    var inline367 int = hashmap_len__HashMap_6string_3int(hashmap_literal__1623)
    t297 = inline367
    var t298 int = t296 + t297
    var t299 string
    var inline365 string = _goml_runtime_core_int_to_string(t298)
    t299 = inline365
    var t300 string = t295 + t299
    var t301 string = t300 + " {ok}"
    var inline362 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t301)
    _goml_runtime_core_string_println(inline362)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(self__237 *ref_string_x) string {
    var t305 string = ref_get__Ref_6string(self__237)
    return t305
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(self__238 *ref_string_x, value__239 string) struct{} {
    ref_set__Ref_6string(self__238, value__239)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t309 string
    t309 = value__31
    _goml_runtime_core_string_println(t309)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__34 int) string {
    var t313 string = _goml_runtime_core_int_to_string(self__34)
    return t313
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string(value__236 string) *ref_string_x {
    var t316 *ref_string_x = ref__Ref_6string(value__236)
    return t316
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int() *_goml_vec_int {
    var t319 *_goml_vec_int = vec_new__Vec_3int()
    return t319
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(self__155 *_goml_vec_int, elem__156 int) struct{} {
    vec_push__Vec_3int(self__155, elem__156)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(self__161 *_goml_vec_int, index__162 int) int {
    var t324 int = vec_get__Vec_3int(self__161, index__162)
    return t324
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__84 string, other__85 string) bool {
    var t343 bool = self__84 == other__85
    return t343
}

func _goml_m_trait__impl_i_Hash_i_string_i_hash(self__112 string) uint64 {
    var t346 uint64 = _goml_runtime_core_string_hash(self__112)
    return t346
}

func main() {
    main0()
}
