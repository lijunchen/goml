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
    var t182 string
    var inline309 string = ref_get__Ref_6string(log__0)
    t182 = inline309
    var t183 string = t182 + label__1
    ref_set__Ref_6string(log__0, t183)
    return value__2
}

func record_point(log__3 *ref_string_x, label__4 string, value__5 Point) Point {
    var t186 string
    var inline313 string = ref_get__Ref_6string(log__3)
    t186 = inline313
    var t187 string = t186 + label__4
    ref_set__Ref_6string(log__3, t187)
    return value__5
}

func record_vec(log__6 *ref_string_x, label__7 string, value__8 *_goml_vec_int) *_goml_vec_int {
    var t190 string
    var inline317 string = ref_get__Ref_6string(log__6)
    t190 = inline317
    var t191 string = t190 + label__7
    ref_set__Ref_6string(log__6, t191)
    return value__8
}

func main0() struct{} {
    var number__9 int = 5
    var compound_old139 int = number__9
    var compound_value140 int = 3
    var t193 int = compound_old139 + compound_value140
    number__9 = t193
    var compound_old142 int = number__9
    var compound_value143 int = 2
    var t195 int = compound_old142 * compound_value143
    number__9 = t195
    var compound_old145 int = number__9
    var compound_value146 int = 1
    var t197 int = compound_old145 >> compound_value146
    number__9 = t197
    var t199 string = _goml_m_inherent_i_int_i_int_i_to__string(number__9)
    println__T_string(t199)
    var direct__10 Point = Point{
        x: 3,
        y: 4,
    }
    var place_root149 Point = direct__10
    var place150 int = place_root149.x
    var value151 int = 5
    var t200 int = place150 + value151
    var t201 int = place_root149.y
    var t202 Point = Point{
        x: t200,
        y: t201,
    }
    direct__10 = t202
    var t204 int = direct__10.x
    var t205 string = _goml_m_inherent_i_int_i_int_i_to__string(t204)
    var t206 string = "" + t205
    var t207 string = t206 + ","
    var t208 int = direct__10.y
    var t209 string = _goml_m_inherent_i_int_i_int_i_to__string(t208)
    var t210 string = t207 + t209
    println__T_string(t210)
    var pair__11 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: 2,
        _1: 9,
    }
    var place_root154 Tuple2_3int_3int = pair__11
    var place155 int = place_root154._0
    var value156 int = 3
    var t211 int = place155 * value156
    var t212 int = place_root154._1
    var t213 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: t211,
        _1: t212,
    }
    pair__11 = t213
    var t215 int = pair__11._0
    var t216 string = _goml_m_inherent_i_int_i_int_i_to__string(t215)
    var t217 string = "" + t216
    var t218 string = t217 + ","
    var t219 int = pair__11._1
    var t220 string = _goml_m_inherent_i_int_i_int_i_to__string(t219)
    var t221 string = t218 + t220
    println__T_string(t221)
    var log__12 *ref_string_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string("")
    var base__13 Point = Point{
        x: 1,
        y: 2,
    }
    var struct_update_field__780__0 int = record(log__12, "F", 7)
    var struct_update_base__780 Point = record_point(log__12, "B", base__13)
    var t222 int = struct_update_base__780.y
    var t224 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    println__T_string(t224)
    var t226 string = _goml_m_inherent_i_int_i_int_i_to__string(struct_update_field__780__0)
    var t227 string = "" + t226
    var t228 string = t227 + ","
    var t230 string = _goml_m_inherent_i_int_i_int_i_to__string(t222)
    var t231 string = t228 + t230
    println__T_string(t231)
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__12, "")
    var vec_literal__967 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int()
    var t232 int = record(log__12, "A", 10)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__967, t232)
    var t233 int = record(log__12, "B", 20)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__967, t233)
    var place_root164 *_goml_vec_int = record_vec(log__12, "R", vec_literal__967)
    var index165 int = record(log__12, "I", 1)
    var place166 int = vec_get__Vec_3int(place_root164, index165)
    var value167 int = record(log__12, "V", 5)
    var t234 int = place166 + value167
    vec_set__Vec_3int(place_root164, index165, t234)
    var t236 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    println__T_string(t236)
    var t237 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(vec_literal__967, 0)
    var t238 string = _goml_m_inherent_i_int_i_int_i_to__string(t237)
    var t239 string = "" + t238
    var t240 string = t239 + ","
    var t241 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(vec_literal__967, 1)
    var t242 string = _goml_m_inherent_i_int_i_int_i_to__string(t241)
    var t243 string = t240 + t242
    println__T_string(t243)
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__12, "")
    var hashmap_literal__1226 *hashmap_string_int_x
    var inline373 *hashmap_string_int_x = hashmap_new__HashMap_6string_3int()
    hashmap_literal__1226 = inline373
    var t244 string = "" + "k"
    var t245 int = record(log__12, "K", 1)
    var t246 string
    var inline371 string = _goml_runtime_core_int_to_string(t245)
    t246 = inline371
    var t247 string = t244 + t246
    var t248 int
    var inline365 string = "V"
    var inline366 int = 11
    var inline367 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    var inline368 string = inline367 + inline365
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__12, inline368)
    t248 = inline366
    hashmap_set__HashMap_6string_3int(hashmap_literal__1226, t247, t248)
    var t249 int
    var inline357 string = "A"
    var inline358 int = 1
    var inline359 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    var inline360 string = inline359 + inline357
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__12, inline360)
    t249 = inline358
    var inline354 string = "same"
    hashmap_set__HashMap_6string_3int(hashmap_literal__1226, inline354, t249)
    var t250 int
    var inline348 string = "B"
    var inline349 int = 2
    var inline350 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    var inline351 string = inline350 + inline348
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__12, inline351)
    t250 = inline349
    var inline345 string = "same"
    hashmap_set__HashMap_6string_3int(hashmap_literal__1226, inline345, t250)
    var t251 string
    var inline343 string = ref_get__Ref_6string(log__12)
    t251 = inline343
    var inline340 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t251)
    _goml_runtime_core_string_println(inline340)
    var mtmp176 Option__int
    var inline337 string = "same"
    var inline338 Option__int = hashmap_get__HashMap_6string_3int(hashmap_literal__1226, inline337)
    mtmp176 = inline338
    var jp253 string
    switch mtmp176.(type) {
    case None:
        jp253 = "missing"
    case Some:
        var x177 int = mtmp176.(Some)._0
        var inline319 string = _goml_runtime_core_int_to_string(x177)
        jp253 = inline319
    default:
        panic("non-exhaustive match")
    }
    var inline334 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp253)
    _goml_runtime_core_string_println(inline334)
    var vec_literal__1570 *_goml_vec_int
    var inline332 *_goml_vec_int = vec_new__Vec_3int()
    vec_literal__1570 = inline332
    var hashmap_literal__1623 *hashmap_string_int_x
    var inline330 *hashmap_string_int_x = hashmap_new__HashMap_6string_3int()
    hashmap_literal__1623 = inline330
    var t254 string = "" + "empty="
    var t255 int
    var inline328 int = vec_len__Vec_3int(vec_literal__1570)
    t255 = inline328
    var t256 int
    var inline326 int = hashmap_len__HashMap_6string_3int(hashmap_literal__1623)
    t256 = inline326
    var t257 int = t255 + t256
    var t258 string
    var inline324 string = _goml_runtime_core_int_to_string(t257)
    t258 = inline324
    var t259 string = t254 + t258
    var t260 string = t259 + " {ok}"
    var inline321 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t260)
    _goml_runtime_core_string_println(inline321)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(self__216 *ref_string_x) string {
    var t264 string = ref_get__Ref_6string(self__216)
    return t264
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(self__217 *ref_string_x, value__218 string) struct{} {
    ref_set__Ref_6string(self__217, value__218)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t268 string
    t268 = value__31
    _goml_runtime_core_string_println(t268)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__34 int) string {
    var t272 string = _goml_runtime_core_int_to_string(self__34)
    return t272
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string(value__215 string) *ref_string_x {
    var t275 *ref_string_x = ref__Ref_6string(value__215)
    return t275
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int() *_goml_vec_int {
    var t278 *_goml_vec_int = vec_new__Vec_3int()
    return t278
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(self__134 *_goml_vec_int, elem__135 int) struct{} {
    vec_push__Vec_3int(self__134, elem__135)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(self__140 *_goml_vec_int, index__141 int) int {
    var t283 int = vec_get__Vec_3int(self__140, index__141)
    return t283
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__84 string, other__85 string) bool {
    var t302 bool = self__84 == other__85
    return t302
}

func _goml_m_trait__impl_i_Hash_i_string_i_hash(self__112 string) uint64 {
    var t305 uint64 = _goml_runtime_core_string_hash(self__112)
    return t305
}

func main() {
    main0()
}
