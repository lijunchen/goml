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
    var t228 string
    var inline355 string = ref_get__Ref_6string(log__0)
    t228 = inline355
    var t229 string = t228 + label__1
    ref_set__Ref_6string(log__0, t229)
    return value__2
}

func record_point(log__3 *ref_string_x, label__4 string, value__5 Point) Point {
    var t232 string
    var inline359 string = ref_get__Ref_6string(log__3)
    t232 = inline359
    var t233 string = t232 + label__4
    ref_set__Ref_6string(log__3, t233)
    return value__5
}

func record_vec(log__6 *ref_string_x, label__7 string, value__8 *_goml_vec_int) *_goml_vec_int {
    var t236 string
    var inline363 string = ref_get__Ref_6string(log__6)
    t236 = inline363
    var t237 string = t236 + label__7
    ref_set__Ref_6string(log__6, t237)
    return value__8
}

func main0() struct{} {
    var number__9 int = 5
    var compound_old185 int = number__9
    var compound_value186 int = 3
    var t239 int = compound_old185 + compound_value186
    number__9 = t239
    var compound_old188 int = number__9
    var compound_value189 int = 2
    var t241 int = compound_old188 * compound_value189
    number__9 = t241
    var compound_old191 int = number__9
    var compound_value192 int = 1
    var t243 int = compound_old191 >> compound_value192
    number__9 = t243
    var t245 string = _goml_m_inherent_i_int_i_int_i_to__string(number__9)
    println__T_string(t245)
    var direct__10 Point = Point{
        x: 3,
        y: 4,
    }
    var place_root195 Point = direct__10
    var place196 int = place_root195.x
    var value197 int = 5
    var t246 int = place196 + value197
    var t247 int = place_root195.y
    var t248 Point = Point{
        x: t246,
        y: t247,
    }
    direct__10 = t248
    var t250 int = direct__10.x
    var t251 string = _goml_m_inherent_i_int_i_int_i_to__string(t250)
    var t252 string = "" + t251
    var t253 string = t252 + ","
    var t254 int = direct__10.y
    var t255 string = _goml_m_inherent_i_int_i_int_i_to__string(t254)
    var t256 string = t253 + t255
    println__T_string(t256)
    var pair__11 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: 2,
        _1: 9,
    }
    var place_root200 Tuple2_3int_3int = pair__11
    var place201 int = place_root200._0
    var value202 int = 3
    var t257 int = place201 * value202
    var t258 int = place_root200._1
    var t259 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: t257,
        _1: t258,
    }
    pair__11 = t259
    var t261 int = pair__11._0
    var t262 string = _goml_m_inherent_i_int_i_int_i_to__string(t261)
    var t263 string = "" + t262
    var t264 string = t263 + ","
    var t265 int = pair__11._1
    var t266 string = _goml_m_inherent_i_int_i_int_i_to__string(t265)
    var t267 string = t264 + t266
    println__T_string(t267)
    var log__12 *ref_string_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string("")
    var base__13 Point = Point{
        x: 1,
        y: 2,
    }
    var struct_update_field__780__0 int = record(log__12, "F", 7)
    var struct_update_base__780 Point = record_point(log__12, "B", base__13)
    var t268 int = struct_update_base__780.y
    var t270 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    println__T_string(t270)
    var t272 string = _goml_m_inherent_i_int_i_int_i_to__string(struct_update_field__780__0)
    var t273 string = "" + t272
    var t274 string = t273 + ","
    var t276 string = _goml_m_inherent_i_int_i_int_i_to__string(t268)
    var t277 string = t274 + t276
    println__T_string(t277)
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__12, "")
    var vec_literal__967 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int()
    var t278 int = record(log__12, "A", 10)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__967, t278)
    var t279 int = record(log__12, "B", 20)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__967, t279)
    var place_root210 *_goml_vec_int = record_vec(log__12, "R", vec_literal__967)
    var index211 int = record(log__12, "I", 1)
    var place212 int = vec_get__Vec_3int(place_root210, index211)
    var value213 int = record(log__12, "V", 5)
    var t280 int = place212 + value213
    vec_set__Vec_3int(place_root210, index211, t280)
    var t282 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    println__T_string(t282)
    var t283 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(vec_literal__967, 0)
    var t284 string = _goml_m_inherent_i_int_i_int_i_to__string(t283)
    var t285 string = "" + t284
    var t286 string = t285 + ","
    var t287 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(vec_literal__967, 1)
    var t288 string = _goml_m_inherent_i_int_i_int_i_to__string(t287)
    var t289 string = t286 + t288
    println__T_string(t289)
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__12, "")
    var hashmap_literal__1226 *hashmap_string_int_x
    var inline419 *hashmap_string_int_x = hashmap_new__HashMap_6string_3int()
    hashmap_literal__1226 = inline419
    var t290 string = "" + "k"
    var t291 int = record(log__12, "K", 1)
    var t292 string
    var inline417 string = _goml_runtime_core_int_to_string(t291)
    t292 = inline417
    var t293 string = t290 + t292
    var t294 int
    var inline411 string = "V"
    var inline412 int = 11
    var inline413 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    var inline414 string = inline413 + inline411
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__12, inline414)
    t294 = inline412
    hashmap_set__HashMap_6string_3int(hashmap_literal__1226, t293, t294)
    var t295 int
    var inline403 string = "A"
    var inline404 int = 1
    var inline405 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    var inline406 string = inline405 + inline403
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__12, inline406)
    t295 = inline404
    var inline400 string = "same"
    hashmap_set__HashMap_6string_3int(hashmap_literal__1226, inline400, t295)
    var t296 int
    var inline394 string = "B"
    var inline395 int = 2
    var inline396 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    var inline397 string = inline396 + inline394
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__12, inline397)
    t296 = inline395
    var inline391 string = "same"
    hashmap_set__HashMap_6string_3int(hashmap_literal__1226, inline391, t296)
    var t297 string
    var inline389 string = ref_get__Ref_6string(log__12)
    t297 = inline389
    var inline386 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t297)
    _goml_runtime_core_string_println(inline386)
    var mtmp222 Option__int
    var inline383 string = "same"
    var inline384 Option__int = hashmap_get__HashMap_6string_3int(hashmap_literal__1226, inline383)
    mtmp222 = inline384
    var jp299 string
    switch mtmp222.(type) {
    case None:
        jp299 = "missing"
    case Some:
        var x223 int = mtmp222.(Some)._0
        var inline365 string = _goml_runtime_core_int_to_string(x223)
        jp299 = inline365
    default:
        panic("non-exhaustive match")
    }
    var inline380 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp299)
    _goml_runtime_core_string_println(inline380)
    var vec_literal__1570 *_goml_vec_int
    var inline378 *_goml_vec_int = vec_new__Vec_3int()
    vec_literal__1570 = inline378
    var hashmap_literal__1623 *hashmap_string_int_x
    var inline376 *hashmap_string_int_x = hashmap_new__HashMap_6string_3int()
    hashmap_literal__1623 = inline376
    var t300 string = "" + "empty="
    var t301 int
    var inline374 int = vec_len__Vec_3int(vec_literal__1570)
    t301 = inline374
    var t302 int
    var inline372 int = hashmap_len__HashMap_6string_3int(hashmap_literal__1623)
    t302 = inline372
    var t303 int = t301 + t302
    var t304 string
    var inline370 string = _goml_runtime_core_int_to_string(t303)
    t304 = inline370
    var t305 string = t300 + t304
    var t306 string = t305 + " {ok}"
    var inline367 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t306)
    _goml_runtime_core_string_println(inline367)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(self__271 *ref_string_x) string {
    var t310 string = ref_get__Ref_6string(self__271)
    return t310
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(self__272 *ref_string_x, value__273 string) struct{} {
    ref_set__Ref_6string(self__272, value__273)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t314 string
    t314 = value__1
    _goml_runtime_core_string_println(t314)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__32 int) string {
    var t318 string = _goml_runtime_core_int_to_string(self__32)
    return t318
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string(value__270 string) *ref_string_x {
    var t321 *ref_string_x = ref__Ref_6string(value__270)
    return t321
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int() *_goml_vec_int {
    var t324 *_goml_vec_int = vec_new__Vec_3int()
    return t324
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(self__174 *_goml_vec_int, elem__175 int) struct{} {
    vec_push__Vec_3int(self__174, elem__175)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(self__184 *_goml_vec_int, index__185 int) int {
    var t329 int = vec_get__Vec_3int(self__184, index__185)
    return t329
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_PartialEq_i_string_i_eq(self__97 string, other__98 string) bool {
    var t348 bool = self__97 == other__98
    return t348
}

func _goml_m_trait__impl_i_Hash_i_string_i_hash(self__125 string) uint64 {
    var t351 uint64 = _goml_runtime_core_string_hash(self__125)
    return t351
}

func main() {
    main0()
}
