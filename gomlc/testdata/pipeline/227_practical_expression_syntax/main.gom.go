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
    var retv197 int
    var t198 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__0)
    var t199 string = t198 + label__1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__0, t199)
    retv197 = value__2
    return retv197
}

func record_point(log__3 *ref_string_x, label__4 string, value__5 Point) Point {
    var retv201 Point
    var t202 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__3)
    var t203 string = t202 + label__4
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__3, t203)
    retv201 = value__5
    return retv201
}

func record_vec(log__6 *ref_string_x, label__7 string, value__8 *_goml_vec_int) *_goml_vec_int {
    var retv205 *_goml_vec_int
    var t206 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__6)
    var t207 string = t206 + label__7
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__6, t207)
    retv205 = value__8
    return retv205
}

func main0() struct{} {
    var number__9 int = 5
    var compound_old155 int = number__9
    var compound_value156 int = 3
    var t209 int = compound_old155 + compound_value156
    number__9 = t209
    var compound_old158 int = number__9
    var compound_value159 int = 2
    var t211 int = compound_old158 * compound_value159
    number__9 = t211
    var compound_old161 int = number__9
    var compound_value162 int = 1
    var t213 int = compound_old161 >> compound_value162
    number__9 = t213
    var t215 string = _goml_m_inherent_i_int_i_int_i_to__string(number__9)
    _goml_runtime_core_string_println(t215)
    var direct__10 Point = Point{
        x: 3,
        y: 4,
    }
    var place_root165 Point = direct__10
    var place166 int = place_root165.x
    var value167 int = 5
    var t216 int = place166 + value167
    var t217 int = place_root165.y
    var t218 Point = Point{
        x: t216,
        y: t217,
    }
    direct__10 = t218
    var t220 int = direct__10.x
    var t221 string = _goml_m_inherent_i_int_i_int_i_to__string(t220)
    var t222 string = "" + t221
    var t223 string = t222 + ","
    var t224 int = direct__10.y
    var t225 string = _goml_m_inherent_i_int_i_int_i_to__string(t224)
    var t226 string = t223 + t225
    _goml_runtime_core_string_println(t226)
    var pair__11 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: 2,
        _1: 9,
    }
    var place_root170 Tuple2_3int_3int = pair__11
    var place171 int = place_root170._0
    var value172 int = 3
    var t227 int = place171 * value172
    var t228 int = place_root170._1
    var t229 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: t227,
        _1: t228,
    }
    pair__11 = t229
    var t231 int = pair__11._0
    var t232 string = _goml_m_inherent_i_int_i_int_i_to__string(t231)
    var t233 string = "" + t232
    var t234 string = t233 + ","
    var t235 int = pair__11._1
    var t236 string = _goml_m_inherent_i_int_i_int_i_to__string(t235)
    var t237 string = t234 + t236
    _goml_runtime_core_string_println(t237)
    var log__12 *ref_string_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string("")
    var base__13 Point = Point{
        x: 1,
        y: 2,
    }
    var struct_update_field__801__0 int = record(log__12, "F", 7)
    var struct_update_base__801 Point = record_point(log__12, "B", base__13)
    var t238 int = struct_update_base__801.y
    var t239 Point = Point{
        x: struct_update_field__801__0,
        y: t238,
    }
    var updated__14 Point = t239
    var t240 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    _goml_runtime_core_string_println(t240)
    var t241 int = updated__14.x
    var t242 string = _goml_m_inherent_i_int_i_int_i_to__string(t241)
    var t243 string = "" + t242
    var t244 string = t243 + ","
    var t245 int = updated__14.y
    var t246 string = _goml_m_inherent_i_int_i_int_i_to__string(t245)
    var t247 string = t244 + t246
    _goml_runtime_core_string_println(t247)
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__12, "")
    var vec_literal__1002 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int()
    var t248 int = record(log__12, "A", 10)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__1002, t248)
    var t249 int = record(log__12, "B", 20)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__1002, t249)
    var values__15 *_goml_vec_int = vec_literal__1002
    var place_root180 *_goml_vec_int = record_vec(log__12, "R", values__15)
    var index181 int = record(log__12, "I", 1)
    var place182 int = vec_get__Vec_3int(place_root180, index181)
    var value183 int = record(log__12, "V", 5)
    var t250 int = place182 + value183
    vec_set__Vec_3int(place_root180, index181, t250)
    var t252 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    _goml_runtime_core_string_println(t252)
    var t253 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(values__15, 0)
    var t254 string = _goml_m_inherent_i_int_i_int_i_to__string(t253)
    var t255 string = "" + t254
    var t256 string = t255 + ","
    var t257 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(values__15, 1)
    var t258 string = _goml_m_inherent_i_int_i_int_i_to__string(t257)
    var t259 string = t256 + t258
    _goml_runtime_core_string_println(t259)
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__12, "")
    var hashmap_literal__1275 *hashmap_string_int_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int()
    var t260 string = "" + "k"
    var t261 int = record(log__12, "K", 1)
    var t262 string = _goml_m_inherent_i_int_i_int_i_to__string(t261)
    var t263 string = t260 + t262
    var t264 int = record(log__12, "V", 11)
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int(hashmap_literal__1275, t263, t264)
    var t265 int = record(log__12, "A", 1)
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int(hashmap_literal__1275, "same", t265)
    var t266 int = record(log__12, "B", 2)
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int(hashmap_literal__1275, "same", t266)
    var table__16 *hashmap_string_int_x = hashmap_literal__1275
    var t267 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    _goml_runtime_core_string_println(t267)
    var mtmp192 Option__int = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__string____V__int(table__16, "same")
    var jp269 string
    switch mtmp192.(type) {
    case None:
        jp269 = "missing"
    case Some:
        var x193 int = mtmp192.(Some)._0
        var value__17 int = x193
        var t277 string = _goml_m_inherent_i_int_i_int_i_to__string(value__17)
        jp269 = t277
    default:
        panic("non-exhaustive match")
    }
    _goml_runtime_core_string_println(jp269)
    var vec_literal__1633 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int()
    var empty_values__18 *_goml_vec_int = vec_literal__1633
    var hashmap_literal__1686 *hashmap_string_int_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int()
    var empty_table__19 *hashmap_string_int_x = hashmap_literal__1686
    var t270 string = "" + "empty="
    var t271 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int(empty_values__18)
    var t272 int = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__string____V__int(empty_table__19)
    var t273 int = t271 + t272
    var t274 string = _goml_m_inherent_i_int_i_int_i_to__string(t273)
    var t275 string = t270 + t274
    var t276 string = t275 + " {ok}"
    _goml_runtime_core_string_println(t276)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(self__208 *ref_string_x) string {
    var retv279 string
    var t280 string = ref_get__Ref_6string(self__208)
    retv279 = t280
    return retv279
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(self__209 *ref_string_x, value__210 string) struct{} {
    ref_set__Ref_6string(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv284 string
    var t285 string = _goml_runtime_core_int_to_string(self__5)
    retv284 = t285
    return retv284
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string(value__207 string) *ref_string_x {
    var retv287 *ref_string_x
    var t288 *ref_string_x = ref__Ref_6string(value__207)
    retv287 = t288
    return retv287
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int() *_goml_vec_int {
    var retv290 *_goml_vec_int
    var t291 *_goml_vec_int = vec_new__Vec_3int()
    retv290 = t291
    return retv290
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(self__126 *_goml_vec_int, elem__127 int) struct{} {
    vec_push__Vec_3int(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(self__132 *_goml_vec_int, index__133 int) int {
    var retv295 int
    var t296 int = vec_get__Vec_3int(self__132, index__133)
    retv295 = t296
    return retv295
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int() *hashmap_string_int_x {
    var retv298 *hashmap_string_int_x
    var t299 *hashmap_string_int_x = hashmap_new__HashMap_6string_3int()
    retv298 = t299
    return retv298
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int(self__198 *hashmap_string_int_x, key__199 string, value__200 int) struct{} {
    hashmap_set__HashMap_6string_3int(self__198, key__199, value__200)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__string____V__int(self__196 *hashmap_string_int_x, key__197 string) Option__int {
    var retv303 Option__int
    var t304 Option__int = hashmap_get__HashMap_6string_3int(self__196, key__197)
    retv303 = t304
    return retv303
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int(self__137 *_goml_vec_int) int {
    var retv306 int
    var t307 int = vec_len__Vec_3int(self__137)
    retv306 = t307
    return retv306
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__string____V__int(self__203 *hashmap_string_int_x) int {
    var retv309 int
    var t310 int = hashmap_len__HashMap_6string_3int(self__203)
    retv309 = t310
    return retv309
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__55 string, other__56 string) bool {
    var retv312 bool
    var t313 bool = self__55 == other__56
    retv312 = t313
    return retv312
}

func _goml_m_trait__impl_i_Hash_i_string_i_hash(self__83 string) uint64 {
    var retv315 uint64
    var t316 uint64 = _goml_runtime_core_string_hash(self__83)
    retv315 = t316
    return retv315
}

func main() {
    main0()
}
