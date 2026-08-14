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
        if entry.active && _goml_m_trait__impl_i_PartialEq_i_string_i_eq(entry.key, key) {
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
    var t233 string
    var inline360 string = ref_get__Ref_6string(log__0)
    t233 = inline360
    var t234 string = t233 + label__1
    ref_set__Ref_6string(log__0, t234)
    return value__2
}

func record_point(log__3 *ref_string_x, label__4 string, value__5 Point) Point {
    var t237 string
    var inline364 string = ref_get__Ref_6string(log__3)
    t237 = inline364
    var t238 string = t237 + label__4
    ref_set__Ref_6string(log__3, t238)
    return value__5
}

func record_vec(log__6 *ref_string_x, label__7 string, value__8 *_goml_vec_int) *_goml_vec_int {
    var t241 string
    var inline368 string = ref_get__Ref_6string(log__6)
    t241 = inline368
    var t242 string = t241 + label__7
    ref_set__Ref_6string(log__6, t242)
    return value__8
}

func main0() struct{} {
    var number__9 int = 5
    var compound_old190 int = number__9
    var compound_value191 int = 3
    var t244 int = compound_old190 + compound_value191
    number__9 = t244
    var compound_old193 int = number__9
    var compound_value194 int = 2
    var t246 int = compound_old193 * compound_value194
    number__9 = t246
    var compound_old196 int = number__9
    var compound_value197 int = 1
    var t248 int = compound_old196 >> compound_value197
    number__9 = t248
    var t250 string = _goml_m_inherent_i_int_i_int_i_to__string(number__9)
    println__T_string(t250)
    var direct__10 Point = Point{
        x: 3,
        y: 4,
    }
    var place_root200 Point = direct__10
    var place201 int = place_root200.x
    var value202 int = 5
    var t251 int = place201 + value202
    var t252 int = place_root200.y
    var t253 Point = Point{
        x: t251,
        y: t252,
    }
    direct__10 = t253
    var t255 int = direct__10.x
    var t256 string = _goml_m_inherent_i_int_i_int_i_to__string(t255)
    var t257 string = "" + t256
    var t258 string = t257 + ","
    var t259 int = direct__10.y
    var t260 string = _goml_m_inherent_i_int_i_int_i_to__string(t259)
    var t261 string = t258 + t260
    println__T_string(t261)
    var pair__11 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: 2,
        _1: 9,
    }
    var place_root205 Tuple2_3int_3int = pair__11
    var place206 int = place_root205._0
    var value207 int = 3
    var t262 int = place206 * value207
    var t263 int = place_root205._1
    var t264 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: t262,
        _1: t263,
    }
    pair__11 = t264
    var t266 int = pair__11._0
    var t267 string = _goml_m_inherent_i_int_i_int_i_to__string(t266)
    var t268 string = "" + t267
    var t269 string = t268 + ","
    var t270 int = pair__11._1
    var t271 string = _goml_m_inherent_i_int_i_int_i_to__string(t270)
    var t272 string = t269 + t271
    println__T_string(t272)
    var log__12 *ref_string_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string("")
    var base__13 Point = Point{
        x: 1,
        y: 2,
    }
    var struct_update_field__780__0 int = record(log__12, "F", 7)
    var struct_update_base__780 Point = record_point(log__12, "B", base__13)
    var t273 int = struct_update_base__780.y
    var t275 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    println__T_string(t275)
    var t277 string = _goml_m_inherent_i_int_i_int_i_to__string(struct_update_field__780__0)
    var t278 string = "" + t277
    var t279 string = t278 + ","
    var t281 string = _goml_m_inherent_i_int_i_int_i_to__string(t273)
    var t282 string = t279 + t281
    println__T_string(t282)
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__12, "")
    var vec_literal__967 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int()
    var t283 int = record(log__12, "A", 10)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__967, t283)
    var t284 int = record(log__12, "B", 20)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__967, t284)
    var place_root215 *_goml_vec_int = record_vec(log__12, "R", vec_literal__967)
    var index216 int = record(log__12, "I", 1)
    var place217 int = vec_get__Vec_3int(place_root215, index216)
    var value218 int = record(log__12, "V", 5)
    var t285 int = place217 + value218
    vec_set__Vec_3int(place_root215, index216, t285)
    var t287 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    println__T_string(t287)
    var t288 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(vec_literal__967, 0)
    var t289 string = _goml_m_inherent_i_int_i_int_i_to__string(t288)
    var t290 string = "" + t289
    var t291 string = t290 + ","
    var t292 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(vec_literal__967, 1)
    var t293 string = _goml_m_inherent_i_int_i_int_i_to__string(t292)
    var t294 string = t291 + t293
    println__T_string(t294)
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__12, "")
    var hashmap_literal__1226 *hashmap_string_int_x
    var inline424 *hashmap_string_int_x = hashmap_new__HashMap_6string_3int()
    hashmap_literal__1226 = inline424
    var t295 string = "" + "k"
    var t296 int = record(log__12, "K", 1)
    var t297 string
    var inline422 string = _goml_runtime_core_int_to_string(t296)
    t297 = inline422
    var t298 string = t295 + t297
    var t299 int
    var inline416 string = "V"
    var inline417 int = 11
    var inline418 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    var inline419 string = inline418 + inline416
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__12, inline419)
    t299 = inline417
    hashmap_set__HashMap_6string_3int(hashmap_literal__1226, t298, t299)
    var t300 int
    var inline408 string = "A"
    var inline409 int = 1
    var inline410 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    var inline411 string = inline410 + inline408
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__12, inline411)
    t300 = inline409
    var inline405 string = "same"
    hashmap_set__HashMap_6string_3int(hashmap_literal__1226, inline405, t300)
    var t301 int
    var inline399 string = "B"
    var inline400 int = 2
    var inline401 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    var inline402 string = inline401 + inline399
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__12, inline402)
    t301 = inline400
    var inline396 string = "same"
    hashmap_set__HashMap_6string_3int(hashmap_literal__1226, inline396, t301)
    var t302 string
    var inline394 string = ref_get__Ref_6string(log__12)
    t302 = inline394
    var inline391 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t302)
    _goml_runtime_core_string_println(inline391)
    var mtmp227 Option__int
    var inline388 string = "same"
    var inline389 Option__int = hashmap_get__HashMap_6string_3int(hashmap_literal__1226, inline388)
    mtmp227 = inline389
    var jp304 string
    switch mtmp227.(type) {
    case None:
        jp304 = "missing"
    case Some:
        var x228 int = mtmp227.(Some)._0
        var inline370 string = _goml_runtime_core_int_to_string(x228)
        jp304 = inline370
    default:
        panic("non-exhaustive match")
    }
    var inline385 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp304)
    _goml_runtime_core_string_println(inline385)
    var vec_literal__1570 *_goml_vec_int
    var inline383 *_goml_vec_int = vec_new__Vec_3int()
    vec_literal__1570 = inline383
    var hashmap_literal__1623 *hashmap_string_int_x
    var inline381 *hashmap_string_int_x = hashmap_new__HashMap_6string_3int()
    hashmap_literal__1623 = inline381
    var t305 string = "" + "empty="
    var t306 int
    var inline379 int = vec_len__Vec_3int(vec_literal__1570)
    t306 = inline379
    var t307 int
    var inline377 int = hashmap_len__HashMap_6string_3int(hashmap_literal__1623)
    t307 = inline377
    var t308 int = t306 + t307
    var t309 string
    var inline375 string = _goml_runtime_core_int_to_string(t308)
    t309 = inline375
    var t310 string = t305 + t309
    var t311 string = t310 + " {ok}"
    var inline372 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t311)
    _goml_runtime_core_string_println(inline372)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(self__274 *ref_string_x) string {
    var t315 string = ref_get__Ref_6string(self__274)
    return t315
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(self__275 *ref_string_x, value__276 string) struct{} {
    ref_set__Ref_6string(self__275, value__276)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t319 string
    t319 = value__1
    _goml_runtime_core_string_println(t319)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__32 int) string {
    var t323 string = _goml_runtime_core_int_to_string(self__32)
    return t323
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string(value__273 string) *ref_string_x {
    var t326 *ref_string_x = ref__Ref_6string(value__273)
    return t326
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int() *_goml_vec_int {
    var t329 *_goml_vec_int = vec_new__Vec_3int()
    return t329
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(self__174 *_goml_vec_int, elem__175 int) struct{} {
    vec_push__Vec_3int(self__174, elem__175)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(self__184 *_goml_vec_int, index__185 int) int {
    var t334 int = vec_get__Vec_3int(self__184, index__185)
    return t334
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_PartialEq_i_string_i_eq(self__97 string, other__98 string) bool {
    var t353 bool = self__97 == other__98
    return t353
}

func _goml_m_trait__impl_i_Hash_i_string_i_hash(self__125 string) uint64 {
    var t356 uint64 = _goml_runtime_core_string_hash(self__125)
    return t356
}

func main() {
    main0()
}
