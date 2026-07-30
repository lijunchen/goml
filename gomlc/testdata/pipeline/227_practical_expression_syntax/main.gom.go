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
    var retv113 int
    var t114 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__0)
    var t115 string = t114 + label__1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__0, t115)
    retv113 = value__2
    return retv113
}

func record_point(log__3 *ref_string_x, label__4 string, value__5 Point) Point {
    var retv117 Point
    var t118 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__3)
    var t119 string = t118 + label__4
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__3, t119)
    retv117 = value__5
    return retv117
}

func record_vec(log__6 *ref_string_x, label__7 string, value__8 *_goml_vec_int) *_goml_vec_int {
    var retv121 *_goml_vec_int
    var t122 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__6)
    var t123 string = t122 + label__7
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__6, t123)
    retv121 = value__8
    return retv121
}

func main0() struct{} {
    var number__9 int = 5
    var compound_old71 int = number__9
    var compound_value72 int = 3
    var t125 int = compound_old71 + compound_value72
    number__9 = t125
    var compound_old74 int = number__9
    var compound_value75 int = 2
    var t127 int = compound_old74 * compound_value75
    number__9 = t127
    var compound_old77 int = number__9
    var compound_value78 int = 1
    var t129 int = compound_old77 >> compound_value78
    number__9 = t129
    var t131 string = _goml_m_inherent_i_int_i_int_i_to__string(number__9)
    _goml_runtime_core_string_println(t131)
    var direct__10 Point = Point{
        x: 3,
        y: 4,
    }
    var place_root81 Point = direct__10
    var place82 int = place_root81.x
    var value83 int = 5
    var t132 int = place82 + value83
    var t133 int = place_root81.y
    var t134 Point = Point{
        x: t132,
        y: t133,
    }
    direct__10 = t134
    var t136 int = direct__10.x
    var t137 string = _goml_m_inherent_i_int_i_int_i_to__string(t136)
    var t138 string = "" + t137
    var t139 string = t138 + ","
    var t140 int = direct__10.y
    var t141 string = _goml_m_inherent_i_int_i_int_i_to__string(t140)
    var t142 string = t139 + t141
    _goml_runtime_core_string_println(t142)
    var pair__11 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: 2,
        _1: 9,
    }
    var place_root86 Tuple2_3int_3int = pair__11
    var place87 int = place_root86._0
    var value88 int = 3
    var t143 int = place87 * value88
    var t144 int = place_root86._1
    var t145 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: t143,
        _1: t144,
    }
    pair__11 = t145
    var t147 int = pair__11._0
    var t148 string = _goml_m_inherent_i_int_i_int_i_to__string(t147)
    var t149 string = "" + t148
    var t150 string = t149 + ","
    var t151 int = pair__11._1
    var t152 string = _goml_m_inherent_i_int_i_int_i_to__string(t151)
    var t153 string = t150 + t152
    _goml_runtime_core_string_println(t153)
    var log__12 *ref_string_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string("")
    var base__13 Point = Point{
        x: 1,
        y: 2,
    }
    var struct_update_field__801__0 int = record(log__12, "F", 7)
    var struct_update_base__801 Point = record_point(log__12, "B", base__13)
    var t154 int = struct_update_base__801.y
    var t155 Point = Point{
        x: struct_update_field__801__0,
        y: t154,
    }
    var updated__14 Point = t155
    var t156 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    _goml_runtime_core_string_println(t156)
    var t157 int = updated__14.x
    var t158 string = _goml_m_inherent_i_int_i_int_i_to__string(t157)
    var t159 string = "" + t158
    var t160 string = t159 + ","
    var t161 int = updated__14.y
    var t162 string = _goml_m_inherent_i_int_i_int_i_to__string(t161)
    var t163 string = t160 + t162
    _goml_runtime_core_string_println(t163)
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__12, "")
    var vec_literal__1002 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int()
    var t164 int = record(log__12, "A", 10)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__1002, t164)
    var t165 int = record(log__12, "B", 20)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__1002, t165)
    var values__15 *_goml_vec_int = vec_literal__1002
    var place_root96 *_goml_vec_int = record_vec(log__12, "R", values__15)
    var index97 int = record(log__12, "I", 1)
    var place98 int = vec_get__Vec_3int(place_root96, index97)
    var value99 int = record(log__12, "V", 5)
    var t166 int = place98 + value99
    vec_set__Vec_3int(place_root96, index97, t166)
    var t168 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    _goml_runtime_core_string_println(t168)
    var t169 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(values__15, 0)
    var t170 string = _goml_m_inherent_i_int_i_int_i_to__string(t169)
    var t171 string = "" + t170
    var t172 string = t171 + ","
    var t173 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(values__15, 1)
    var t174 string = _goml_m_inherent_i_int_i_int_i_to__string(t173)
    var t175 string = t172 + t174
    _goml_runtime_core_string_println(t175)
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__12, "")
    var hashmap_literal__1275 *hashmap_string_int_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int()
    var t176 string = "" + "k"
    var t177 int = record(log__12, "K", 1)
    var t178 string = _goml_m_inherent_i_int_i_int_i_to__string(t177)
    var t179 string = t176 + t178
    var t180 int = record(log__12, "V", 11)
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int(hashmap_literal__1275, t179, t180)
    var t181 int = record(log__12, "A", 1)
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int(hashmap_literal__1275, "same", t181)
    var t182 int = record(log__12, "B", 2)
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int(hashmap_literal__1275, "same", t182)
    var table__16 *hashmap_string_int_x = hashmap_literal__1275
    var t183 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    _goml_runtime_core_string_println(t183)
    var mtmp108 Option__int = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__string____V__int(table__16, "same")
    var jp185 string
    switch mtmp108.(type) {
    case None:
        jp185 = "missing"
    case Some:
        var x109 int = mtmp108.(Some)._0
        var value__17 int = x109
        var t193 string = _goml_m_inherent_i_int_i_int_i_to__string(value__17)
        jp185 = t193
    default:
        panic("non-exhaustive match")
    }
    _goml_runtime_core_string_println(jp185)
    var vec_literal__1633 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int()
    var empty_values__18 *_goml_vec_int = vec_literal__1633
    var hashmap_literal__1686 *hashmap_string_int_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int()
    var empty_table__19 *hashmap_string_int_x = hashmap_literal__1686
    var t186 string = "" + "empty="
    var t187 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int(empty_values__18)
    var t188 int = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__string____V__int(empty_table__19)
    var t189 int = t187 + t188
    var t190 string = _goml_m_inherent_i_int_i_int_i_to__string(t189)
    var t191 string = t186 + t190
    var t192 string = t191 + " {ok}"
    _goml_runtime_core_string_println(t192)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(self__208 *ref_string_x) string {
    var retv195 string
    var t196 string = ref_get__Ref_6string(self__208)
    retv195 = t196
    return retv195
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(self__209 *ref_string_x, value__210 string) struct{} {
    ref_set__Ref_6string(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv200 string
    var t201 string = _goml_runtime_core_int_to_string(self__5)
    retv200 = t201
    return retv200
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string(value__207 string) *ref_string_x {
    var retv203 *ref_string_x
    var t204 *ref_string_x = ref__Ref_6string(value__207)
    retv203 = t204
    return retv203
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int() *_goml_vec_int {
    var retv206 *_goml_vec_int
    var t207 *_goml_vec_int = vec_new__Vec_3int()
    retv206 = t207
    return retv206
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(self__126 *_goml_vec_int, elem__127 int) struct{} {
    vec_push__Vec_3int(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(self__132 *_goml_vec_int, index__133 int) int {
    var retv211 int
    var t212 int = vec_get__Vec_3int(self__132, index__133)
    retv211 = t212
    return retv211
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int() *hashmap_string_int_x {
    var retv214 *hashmap_string_int_x
    var t215 *hashmap_string_int_x = hashmap_new__HashMap_6string_3int()
    retv214 = t215
    return retv214
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int(self__198 *hashmap_string_int_x, key__199 string, value__200 int) struct{} {
    hashmap_set__HashMap_6string_3int(self__198, key__199, value__200)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_get____K__string____V__int(self__196 *hashmap_string_int_x, key__197 string) Option__int {
    var retv219 Option__int
    var t220 Option__int = hashmap_get__HashMap_6string_3int(self__196, key__197)
    retv219 = t220
    return retv219
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int(self__137 *_goml_vec_int) int {
    var retv222 int
    var t223 int = vec_len__Vec_3int(self__137)
    retv222 = t223
    return retv222
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_len____K__string____V__int(self__203 *hashmap_string_int_x) int {
    var retv225 int
    var t226 int = hashmap_len__HashMap_6string_3int(self__203)
    retv225 = t226
    return retv225
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__55 string, other__56 string) bool {
    var retv228 bool
    var t229 bool = self__55 == other__56
    retv228 = t229
    return retv228
}

func _goml_m_trait__impl_i_Hash_i_string_i_hash(self__83 string) uint64 {
    var retv231 uint64
    var t232 uint64 = _goml_runtime_core_string_hash(self__83)
    retv231 = t232
    return retv231
}

func main() {
    main0()
}
