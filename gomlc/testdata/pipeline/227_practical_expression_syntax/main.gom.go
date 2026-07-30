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
    var retv153 int
    var t154 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__0)
    var t155 string = t154 + label__1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__0, t155)
    retv153 = value__2
    return retv153
}

func record_point(log__3 *ref_string_x, label__4 string, value__5 Point) Point {
    var retv157 Point
    var t158 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__3)
    var t159 string = t158 + label__4
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__3, t159)
    retv157 = value__5
    return retv157
}

func record_vec(log__6 *ref_string_x, label__7 string, value__8 *_goml_vec_int) *_goml_vec_int {
    var retv161 *_goml_vec_int
    var t162 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__6)
    var t163 string = t162 + label__7
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__6, t163)
    retv161 = value__8
    return retv161
}

func main0() struct{} {
    var number__9 int = 5
    var compound_old111 int = number__9
    var compound_value112 int = 3
    var t165 int = compound_old111 + compound_value112
    number__9 = t165
    var compound_old114 int = number__9
    var compound_value115 int = 2
    var t167 int = compound_old114 * compound_value115
    number__9 = t167
    var compound_old117 int = number__9
    var compound_value118 int = 1
    var t169 int = compound_old117 >> compound_value118
    number__9 = t169
    var t171 string = _goml_m_inherent_i_int_i_int_i_to__string(number__9)
    _goml_runtime_core_string_println(t171)
    var direct__10 Point = Point{
        x: 3,
        y: 4,
    }
    var place_root121 Point = direct__10
    var place122 int = place_root121.x
    var value123 int = 5
    var t172 int = place122 + value123
    var t173 int = place_root121.y
    var t174 Point = Point{
        x: t172,
        y: t173,
    }
    direct__10 = t174
    var t176 int = direct__10.x
    var t177 string = _goml_m_inherent_i_int_i_int_i_to__string(t176)
    var t178 string = "" + t177
    var t179 string = t178 + ","
    var t180 int = direct__10.y
    var t181 string = _goml_m_inherent_i_int_i_int_i_to__string(t180)
    var t182 string = t179 + t181
    _goml_runtime_core_string_println(t182)
    var pair__11 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: 2,
        _1: 9,
    }
    var place_root126 Tuple2_3int_3int = pair__11
    var place127 int = place_root126._0
    var value128 int = 3
    var t183 int = place127 * value128
    var t184 int = place_root126._1
    var t185 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: t183,
        _1: t184,
    }
    pair__11 = t185
    var t187 int = pair__11._0
    var t188 string = _goml_m_inherent_i_int_i_int_i_to__string(t187)
    var t189 string = "" + t188
    var t190 string = t189 + ","
    var t191 int = pair__11._1
    var t192 string = _goml_m_inherent_i_int_i_int_i_to__string(t191)
    var t193 string = t190 + t192
    _goml_runtime_core_string_println(t193)
    var log__12 *ref_string_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string("")
    var base__13 Point = Point{
        x: 1,
        y: 2,
    }
    var struct_update_field__801__0 int = record(log__12, "F", 7)
    var struct_update_base__801 Point = record_point(log__12, "B", base__13)
    var t194 int = struct_update_base__801.y
    var t195 Point = Point{
        x: struct_update_field__801__0,
        y: t194,
    }
    var updated__14 Point = t195
    var t196 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    _goml_runtime_core_string_println(t196)
    var t197 int = updated__14.x
    var t198 string = _goml_m_inherent_i_int_i_int_i_to__string(t197)
    var t199 string = "" + t198
    var t200 string = t199 + ","
    var t201 int = updated__14.y
    var t202 string = _goml_m_inherent_i_int_i_int_i_to__string(t201)
    var t203 string = t200 + t202
    _goml_runtime_core_string_println(t203)
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__12, "")
    var vec_literal__1002 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int()
    var t204 int = record(log__12, "A", 10)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__1002, t204)
    var t205 int = record(log__12, "B", 20)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__1002, t205)
    var values__15 *_goml_vec_int = vec_literal__1002
    var place_root136 *_goml_vec_int = record_vec(log__12, "R", values__15)
    var index137 int = record(log__12, "I", 1)
    var place138 int = vec_get__Vec_3int(place_root136, index137)
    var value139 int = record(log__12, "V", 5)
    var t206 int = place138 + value139
    vec_set__Vec_3int(place_root136, index137, t206)
    var t208 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    _goml_runtime_core_string_println(t208)
    var t209 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(values__15, 0)
    var t210 string = _goml_m_inherent_i_int_i_int_i_to__string(t209)
    var t211 string = "" + t210
    var t212 string = t211 + ","
    var t213 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(values__15, 1)
    var t214 string = _goml_m_inherent_i_int_i_int_i_to__string(t213)
    var t215 string = t212 + t214
    _goml_runtime_core_string_println(t215)
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__12, "")
    var hashmap_literal__1275 *hashmap_string_int_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int()
    var t216 string = "" + "k"
    var t217 int = record(log__12, "K", 1)
    var t218 string = _goml_m_inherent_i_int_i_int_i_to__string(t217)
    var t219 string = t216 + t218
    var t220 int = record(log__12, "V", 11)
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int(hashmap_literal__1275, t219, t220)
    var t221 int = record(log__12, "A", 1)
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int(hashmap_literal__1275, "same", t221)
    var t222 int = record(log__12, "B", 2)
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int(hashmap_literal__1275, "same", t222)
    var table__16 *hashmap_string_int_x = hashmap_literal__1275
    var t223 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    _goml_runtime_core_string_println(t223)
    var mtmp148 Option__int = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__string____V__int(table__16, "same")
    var jp225 string
    switch mtmp148.(type) {
    case None:
        jp225 = "missing"
    case Some:
        var x149 int = mtmp148.(Some)._0
        var value__17 int = x149
        var t233 string = _goml_m_inherent_i_int_i_int_i_to__string(value__17)
        jp225 = t233
    default:
        panic("non-exhaustive match")
    }
    _goml_runtime_core_string_println(jp225)
    var vec_literal__1633 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int()
    var empty_values__18 *_goml_vec_int = vec_literal__1633
    var hashmap_literal__1686 *hashmap_string_int_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int()
    var empty_table__19 *hashmap_string_int_x = hashmap_literal__1686
    var t226 string = "" + "empty="
    var t227 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int(empty_values__18)
    var t228 int = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__string____V__int(empty_table__19)
    var t229 int = t227 + t228
    var t230 string = _goml_m_inherent_i_int_i_int_i_to__string(t229)
    var t231 string = t226 + t230
    var t232 string = t231 + " {ok}"
    _goml_runtime_core_string_println(t232)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(self__208 *ref_string_x) string {
    var retv235 string
    var t236 string = ref_get__Ref_6string(self__208)
    retv235 = t236
    return retv235
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(self__209 *ref_string_x, value__210 string) struct{} {
    ref_set__Ref_6string(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv240 string
    var t241 string = _goml_runtime_core_int_to_string(self__5)
    retv240 = t241
    return retv240
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string(value__207 string) *ref_string_x {
    var retv243 *ref_string_x
    var t244 *ref_string_x = ref__Ref_6string(value__207)
    retv243 = t244
    return retv243
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int() *_goml_vec_int {
    var retv246 *_goml_vec_int
    var t247 *_goml_vec_int = vec_new__Vec_3int()
    retv246 = t247
    return retv246
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(self__126 *_goml_vec_int, elem__127 int) struct{} {
    vec_push__Vec_3int(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(self__132 *_goml_vec_int, index__133 int) int {
    var retv251 int
    var t252 int = vec_get__Vec_3int(self__132, index__133)
    retv251 = t252
    return retv251
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int() *hashmap_string_int_x {
    var retv254 *hashmap_string_int_x
    var t255 *hashmap_string_int_x = hashmap_new__HashMap_6string_3int()
    retv254 = t255
    return retv254
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int(self__198 *hashmap_string_int_x, key__199 string, value__200 int) struct{} {
    hashmap_set__HashMap_6string_3int(self__198, key__199, value__200)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__string____V__int(self__196 *hashmap_string_int_x, key__197 string) Option__int {
    var retv259 Option__int
    var t260 Option__int = hashmap_get__HashMap_6string_3int(self__196, key__197)
    retv259 = t260
    return retv259
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int(self__137 *_goml_vec_int) int {
    var retv262 int
    var t263 int = vec_len__Vec_3int(self__137)
    retv262 = t263
    return retv262
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__string____V__int(self__203 *hashmap_string_int_x) int {
    var retv265 int
    var t266 int = hashmap_len__HashMap_6string_3int(self__203)
    retv265 = t266
    return retv265
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__55 string, other__56 string) bool {
    var retv268 bool
    var t269 bool = self__55 == other__56
    retv268 = t269
    return retv268
}

func _goml_m_trait__impl_i_Hash_i_string_i_hash(self__83 string) uint64 {
    var retv271 uint64
    var t272 uint64 = _goml_runtime_core_string_hash(self__83)
    retv271 = t272
    return retv271
}

func main() {
    main0()
}
