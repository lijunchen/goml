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
    var t218 string
    var inline345 string = ref_get__Ref_6string(log__0)
    t218 = inline345
    var t219 string = t218 + label__1
    ref_set__Ref_6string(log__0, t219)
    return value__2
}

func record_point(log__3 *ref_string_x, label__4 string, value__5 Point) Point {
    var t222 string
    var inline349 string = ref_get__Ref_6string(log__3)
    t222 = inline349
    var t223 string = t222 + label__4
    ref_set__Ref_6string(log__3, t223)
    return value__5
}

func record_vec(log__6 *ref_string_x, label__7 string, value__8 *_goml_vec_int) *_goml_vec_int {
    var t226 string
    var inline353 string = ref_get__Ref_6string(log__6)
    t226 = inline353
    var t227 string = t226 + label__7
    ref_set__Ref_6string(log__6, t227)
    return value__8
}

func main0() struct{} {
    var number__9 int = 5
    var compound_old175 int = number__9
    var compound_value176 int = 3
    var t229 int = compound_old175 + compound_value176
    number__9 = t229
    var compound_old178 int = number__9
    var compound_value179 int = 2
    var t231 int = compound_old178 * compound_value179
    number__9 = t231
    var compound_old181 int = number__9
    var compound_value182 int = 1
    var t233 int = compound_old181 >> compound_value182
    number__9 = t233
    var t235 string = _goml_m_inherent_i_int_i_int_i_to__string(number__9)
    println__T_string(t235)
    var direct__10 Point = Point{
        x: 3,
        y: 4,
    }
    var place_root185 Point = direct__10
    var place186 int = place_root185.x
    var value187 int = 5
    var t236 int = place186 + value187
    var t237 int = place_root185.y
    var t238 Point = Point{
        x: t236,
        y: t237,
    }
    direct__10 = t238
    var t240 int = direct__10.x
    var t241 string = _goml_m_inherent_i_int_i_int_i_to__string(t240)
    var t242 string = "" + t241
    var t243 string = t242 + ","
    var t244 int = direct__10.y
    var t245 string = _goml_m_inherent_i_int_i_int_i_to__string(t244)
    var t246 string = t243 + t245
    println__T_string(t246)
    var pair__11 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: 2,
        _1: 9,
    }
    var place_root190 Tuple2_3int_3int = pair__11
    var place191 int = place_root190._0
    var value192 int = 3
    var t247 int = place191 * value192
    var t248 int = place_root190._1
    var t249 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: t247,
        _1: t248,
    }
    pair__11 = t249
    var t251 int = pair__11._0
    var t252 string = _goml_m_inherent_i_int_i_int_i_to__string(t251)
    var t253 string = "" + t252
    var t254 string = t253 + ","
    var t255 int = pair__11._1
    var t256 string = _goml_m_inherent_i_int_i_int_i_to__string(t255)
    var t257 string = t254 + t256
    println__T_string(t257)
    var log__12 *ref_string_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string("")
    var base__13 Point = Point{
        x: 1,
        y: 2,
    }
    var struct_update_field__780__0 int = record(log__12, "F", 7)
    var struct_update_base__780 Point = record_point(log__12, "B", base__13)
    var t258 int = struct_update_base__780.y
    var t260 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    println__T_string(t260)
    var t262 string = _goml_m_inherent_i_int_i_int_i_to__string(struct_update_field__780__0)
    var t263 string = "" + t262
    var t264 string = t263 + ","
    var t266 string = _goml_m_inherent_i_int_i_int_i_to__string(t258)
    var t267 string = t264 + t266
    println__T_string(t267)
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__12, "")
    var vec_literal__967 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int()
    var t268 int = record(log__12, "A", 10)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__967, t268)
    var t269 int = record(log__12, "B", 20)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__967, t269)
    var place_root200 *_goml_vec_int = record_vec(log__12, "R", vec_literal__967)
    var index201 int = record(log__12, "I", 1)
    var place202 int = vec_get__Vec_3int(place_root200, index201)
    var value203 int = record(log__12, "V", 5)
    var t270 int = place202 + value203
    vec_set__Vec_3int(place_root200, index201, t270)
    var t272 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    println__T_string(t272)
    var t273 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(vec_literal__967, 0)
    var t274 string = _goml_m_inherent_i_int_i_int_i_to__string(t273)
    var t275 string = "" + t274
    var t276 string = t275 + ","
    var t277 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(vec_literal__967, 1)
    var t278 string = _goml_m_inherent_i_int_i_int_i_to__string(t277)
    var t279 string = t276 + t278
    println__T_string(t279)
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__12, "")
    var hashmap_literal__1226 *hashmap_string_int_x
    var inline409 *hashmap_string_int_x = hashmap_new__HashMap_6string_3int()
    hashmap_literal__1226 = inline409
    var t280 string = "" + "k"
    var t281 int = record(log__12, "K", 1)
    var t282 string
    var inline407 string = _goml_runtime_core_int_to_string(t281)
    t282 = inline407
    var t283 string = t280 + t282
    var t284 int
    var inline401 string = "V"
    var inline402 int = 11
    var inline403 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    var inline404 string = inline403 + inline401
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__12, inline404)
    t284 = inline402
    hashmap_set__HashMap_6string_3int(hashmap_literal__1226, t283, t284)
    var t285 int
    var inline393 string = "A"
    var inline394 int = 1
    var inline395 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    var inline396 string = inline395 + inline393
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__12, inline396)
    t285 = inline394
    var inline390 string = "same"
    hashmap_set__HashMap_6string_3int(hashmap_literal__1226, inline390, t285)
    var t286 int
    var inline384 string = "B"
    var inline385 int = 2
    var inline386 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    var inline387 string = inline386 + inline384
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__12, inline387)
    t286 = inline385
    var inline381 string = "same"
    hashmap_set__HashMap_6string_3int(hashmap_literal__1226, inline381, t286)
    var t287 string
    var inline379 string = ref_get__Ref_6string(log__12)
    t287 = inline379
    var inline376 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t287)
    _goml_runtime_core_string_println(inline376)
    var mtmp212 Option__int
    var inline373 string = "same"
    var inline374 Option__int = hashmap_get__HashMap_6string_3int(hashmap_literal__1226, inline373)
    mtmp212 = inline374
    var jp289 string
    switch mtmp212.(type) {
    case None:
        jp289 = "missing"
    case Some:
        var x213 int = mtmp212.(Some)._0
        var inline355 string = _goml_runtime_core_int_to_string(x213)
        jp289 = inline355
    default:
        panic("non-exhaustive match")
    }
    var inline370 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp289)
    _goml_runtime_core_string_println(inline370)
    var vec_literal__1570 *_goml_vec_int
    var inline368 *_goml_vec_int = vec_new__Vec_3int()
    vec_literal__1570 = inline368
    var hashmap_literal__1623 *hashmap_string_int_x
    var inline366 *hashmap_string_int_x = hashmap_new__HashMap_6string_3int()
    hashmap_literal__1623 = inline366
    var t290 string = "" + "empty="
    var t291 int
    var inline364 int = vec_len__Vec_3int(vec_literal__1570)
    t291 = inline364
    var t292 int
    var inline362 int = hashmap_len__HashMap_6string_3int(hashmap_literal__1623)
    t292 = inline362
    var t293 int = t291 + t292
    var t294 string
    var inline360 string = _goml_runtime_core_int_to_string(t293)
    t294 = inline360
    var t295 string = t290 + t294
    var t296 string = t295 + " {ok}"
    var inline357 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t296)
    _goml_runtime_core_string_println(inline357)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(self__256 *ref_string_x) string {
    var t300 string = ref_get__Ref_6string(self__256)
    return t300
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(self__257 *ref_string_x, value__258 string) struct{} {
    ref_set__Ref_6string(self__257, value__258)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t304 string
    t304 = value__1
    _goml_runtime_core_string_println(t304)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__32 int) string {
    var t308 string = _goml_runtime_core_int_to_string(self__32)
    return t308
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string(value__255 string) *ref_string_x {
    var t311 *ref_string_x = ref__Ref_6string(value__255)
    return t311
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int() *_goml_vec_int {
    var t314 *_goml_vec_int = vec_new__Vec_3int()
    return t314
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(self__174 *_goml_vec_int, elem__175 int) struct{} {
    vec_push__Vec_3int(self__174, elem__175)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(self__180 *_goml_vec_int, index__181 int) int {
    var t319 int = vec_get__Vec_3int(self__180, index__181)
    return t319
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_PartialEq_i_string_i_eq(self__97 string, other__98 string) bool {
    var t338 bool = self__97 == other__98
    return t338
}

func _goml_m_trait__impl_i_Hash_i_string_i_hash(self__125 string) uint64 {
    var t341 uint64 = _goml_runtime_core_string_hash(self__125)
    return t341
}

func main() {
    main0()
}
