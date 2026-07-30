package main

import (
    _goml_fmt "fmt"
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

type ref_int_x struct {
    value int
}

func ref__Ref_3int(value int) *ref_int_x {
    return &ref_int_x{
        value: value,
    }
}

func ref_get__Ref_3int(reference *ref_int_x) int {
    return reference.value
}

func ref_set__Ref_3int(reference *ref_int_x, value int) struct{} {
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

func print_opt_int(x__0 Option__int32) struct{} {
    switch x__0.(type) {
    case None:
        println__T_string("none")
    case Some:
        var x68 int32 = x__0.(Some)._0
        var v__1 int32 = x68
        println__T_int32(v__1)
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func main0() struct{} {
    var t135 [2]int = [2]int{31, 32}
    var t136 int = array_get__Array_2_3int(t135, 1)
    println__T_int(t136)
    var arr__2 [3]int = [3]int{1, 2, 3}
    var t137 int = array_get__Array_3_3int(arr__2, 0)
    println__T_int(t137)
    var arr2__3 [3]int = [3]int{4, 5, 6}
    var place_root71 [3]int = arr2__3
    var index72 int = 1
    array_get__Array_3_3int(place_root71, index72)
    var value74 int = 50
    var t138 [3]int = array_set__Array_3_3int(place_root71, index72, value74)
    arr2__3 = t138
    var t140 int = array_get__Array_3_3int(arr2__3, 1)
    println__T_int(t140)
    var t141 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int()
    var t142 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int(t141, 7)
    var t143 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int(t142, 8)
    var vec__4 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int(t143, 9)
    var t144 int = vec_get__Vec_3int(vec__4, 2)
    println__T_int(t144)
    var t145 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    var t146 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t145, 10)
    var t147 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t146, 11)
    var vec2__5 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t147, 12)
    var place_root78 *_goml_vec_int32 = vec2__5
    var index79 int = 0
    vec_get__Vec_5int32(place_root78, index79)
    var value81 int32 = 100
    vec_set__Vec_5int32(place_root78, index79, value81)
    var t149 int32 = vec_get__Vec_5int32(vec2__5, 0)
    println__T_int32(t149)
    var s__6 []int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(vec2__5, 0, 2)
    var t150 int32 = s__6[1]
    println__T_int32(t150)
    var map__7 *hashmap_string_int32_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32()
    var place_root85 *hashmap_string_int32_x = map__7
    var index86 string = "a"
    hashmap_get__HashMap_6string_5int32(place_root85, index86)
    var value88 int32 = 13
    hashmap_set__HashMap_6string_5int32(place_root85, index86, value88)
    var t152 Option__int32 = hashmap_get__HashMap_6string_5int32(map__7, "a")
    print_opt_int(t152)
    var t153 Option__int32 = hashmap_get__HashMap_6string_5int32(map__7, "missing")
    print_opt_int(t153)
    var t154 [2]int32 = [2]int32{1, 2}
    var t155 [2]int32 = [2]int32{3, 4}
    var matrix__8 [2][2]int32 = [2][2]int32{t154, t155}
    var place_root92 [2][2]int32 = matrix__8
    var index93 int = 1
    var place94 [2]int32 = array_get__Array_2_14Array_2_5int32(place_root92, index93)
    var index95 int = 0
    array_get__Array_2_5int32(place94, index95)
    var value97 int32 = 30
    var t156 [2]int32 = array_set__Array_2_5int32(place94, index95, value97)
    var t157 [2][2]int32 = array_set__Array_2_14Array_2_5int32(place_root92, index93, t156)
    matrix__8 = t157
    var t159 [2]int32 = array_get__Array_2_14Array_2_5int32(matrix__8, 1)
    var t160 int32 = array_get__Array_2_5int32(t159, 0)
    println__T_int32(t160)
    var t161 [2]int = [2]int{14, 15}
    var pair__9 Tuple2_11Array2_3int_3int = Tuple2_11Array2_3int_3int{
        _0: t161,
        _1: 0,
    }
    var place_root100 Tuple2_11Array2_3int_3int = pair__9
    var place101 [2]int = place_root100._0
    var index102 int = 1
    array_get__Array_2_3int(place101, index102)
    var value104 int = 150
    var t162 [2]int = array_set__Array_2_3int(place101, index102, value104)
    var t163 int = place_root100._1
    var t164 Tuple2_11Array2_3int_3int = Tuple2_11Array2_3int_3int{
        _0: t162,
        _1: t163,
    }
    pair__9 = t164
    var t166 [2]int = pair__9._0
    var t167 int = array_get__Array_2_3int(t166, 1)
    println__T_int(t167)
    var t168 [2]int32 = [2]int32{16, 17}
    var t169 *_goml_vec_Array_2_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___l_int32_x3b_2_r_()
    var t170 [2]int32 = [2]int32{18, 19}
    var t171 *_goml_vec_Array_2_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T___l_int32_x3b_2_r_(t169, t170)
    var t172 [2]int32 = [2]int32{20, 21}
    var t173 *_goml_vec_Array_2_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T___l_int32_x3b_2_r_(t171, t172)
    var holder__10 Holder = Holder{
        data: t168,
        vecs: t173,
    }
    var place_root107 Holder = holder__10
    var place108 [2]int32 = place_root107.data
    var index109 int = 0
    array_get__Array_2_5int32(place108, index109)
    var value111 int32 = 160
    var t174 [2]int32 = array_set__Array_2_5int32(place108, index109, value111)
    var t175 *_goml_vec_Array_2_5int32 = place_root107.vecs
    var t176 Holder = Holder{
        data: t174,
        vecs: t175,
    }
    holder__10 = t176
    var t178 [2]int32 = holder__10.data
    var t179 int32 = array_get__Array_2_5int32(t178, 0)
    println__T_int32(t179)
    var place_root114 Holder = holder__10
    var place115 *_goml_vec_Array_2_5int32 = place_root114.vecs
    var index116 int = 1
    var place117 [2]int32 = vec_get__Vec_14Array_2_5int32(place115, index116)
    var index118 int = 0
    array_get__Array_2_5int32(place117, index118)
    var value120 int32 = 200
    var t180 [2]int32 = array_set__Array_2_5int32(place117, index118, value120)
    vec_set__Vec_14Array_2_5int32(place115, index116, t180)
    var t182 *_goml_vec_Array_2_5int32 = holder__10.vecs
    var t183 [2]int32 = vec_get__Vec_14Array_2_5int32(t182, 1)
    var t184 int32 = array_get__Array_2_5int32(t183, 0)
    println__T_int32(t184)
    var t185 [2]int32 = [2]int32{22, 23}
    var r__11 *ref_Array_2_5int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T___l_int32_x3b_2_r_(t185)
    var place_ref123 *ref_Array_2_5int32_x = r__11
    var place_root124 [2]int32 = ref_get__Ref_14Array_2_5int32(place_ref123)
    var index125 int = 1
    array_get__Array_2_5int32(place_root124, index125)
    var value127 int32 = 230
    var t186 [2]int32 = array_set__Array_2_5int32(place_root124, index125, value127)
    ref_set__Ref_14Array_2_5int32(place_ref123, t186)
    var t188 [2]int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T___l_int32_x3b_2_r_(r__11)
    var t189 int32 = array_get__Array_2_5int32(t188, 1)
    println__T_int32(t189)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t191 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t191)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t194 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t194)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t197 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t197)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int() *_goml_vec_int {
    var retv200 *_goml_vec_int
    var t201 *_goml_vec_int = vec_new__Vec_3int()
    retv200 = t201
    return retv200
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int(self__128 *_goml_vec_int, elem__129 int) *_goml_vec_int {
    var retv203 *_goml_vec_int
    var result__130 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int()
    var index__131 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop205:
    for {
        var t206 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
        var t207 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int(self__128)
        var t208 bool = t206 < t207
        if t208 {
            var t209 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
            var t210 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(self__128, t209)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(result__130, t210)
            var t211 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
            var t212 int = t211 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__131, t212)
            continue
        } else {
            break Loop_loop205
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(result__130, elem__129)
    retv203 = result__130
    return retv203
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv214 *_goml_vec_int32
    var t215 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv214 = t215
    return retv214
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(self__128 *_goml_vec_int32, elem__129 int32) *_goml_vec_int32 {
    var retv217 *_goml_vec_int32
    var result__130 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    var index__131 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop219:
    for {
        var t220 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
        var t221 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__128)
        var t222 bool = t220 < t221
        if t222 {
            var t223 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
            var t224 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__128, t223)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(result__130, t224)
            var t225 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
            var t226 int = t225 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__131, t226)
            continue
        } else {
            break Loop_loop219
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(result__130, elem__129)
    retv217 = result__130
    return retv217
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(self__175 *_goml_vec_int32, start__176 int, end__177 int) []int32 {
    var retv228 []int32
    var t229 []int32 = self__175.items[start__176:end__177]
    retv228 = t229
    return retv228
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32() *hashmap_string_int32_x {
    var retv231 *hashmap_string_int32_x
    var t232 *hashmap_string_int32_x = hashmap_new__HashMap_6string_5int32()
    retv231 = t232
    return retv231
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___l_int32_x3b_2_r_() *_goml_vec_Array_2_5int32 {
    var retv234 *_goml_vec_Array_2_5int32
    var t235 *_goml_vec_Array_2_5int32 = vec_new__Vec_14Array_2_5int32()
    retv234 = t235
    return retv234
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T___l_int32_x3b_2_r_(self__128 *_goml_vec_Array_2_5int32, elem__129 [2]int32) *_goml_vec_Array_2_5int32 {
    var retv237 *_goml_vec_Array_2_5int32
    var result__130 *_goml_vec_Array_2_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___l_int32_x3b_2_r_()
    var index__131 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop239:
    for {
        var t240 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
        var t241 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___l_int32_x3b_2_r_(self__128)
        var t242 bool = t240 < t241
        if t242 {
            var t243 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
            var t244 [2]int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T___l_int32_x3b_2_r_(self__128, t243)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___l_int32_x3b_2_r_(result__130, t244)
            var t245 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__131)
            var t246 int = t245 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__131, t246)
            continue
        } else {
            break Loop_loop239
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___l_int32_x3b_2_r_(result__130, elem__129)
    retv237 = result__130
    return retv237
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T___l_int32_x3b_2_r_(value__207 [2]int32) *ref_Array_2_5int32_x {
    var retv248 *ref_Array_2_5int32_x
    var t249 *ref_Array_2_5int32_x = ref__Ref_14Array_2_5int32(value__207)
    retv248 = t249
    return retv248
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T___l_int32_x3b_2_r_(self__208 *ref_Array_2_5int32_x) [2]int32 {
    var retv251 [2]int32
    var t252 [2]int32 = ref_get__Ref_14Array_2_5int32(self__208)
    retv251 = t252
    return retv251
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv254 string
    retv254 = self__38
    return retv254
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv256 string
    var t257 string = _goml_runtime_core_int32_to_string(self__43)
    retv256 = t257
    return retv256
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv259 string
    var t260 string = _goml_runtime_core_int_to_string(self__40)
    retv259 = t260
    return retv259
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv262 *ref_int_x
    var t263 *ref_int_x = ref__Ref_3int(value__207)
    retv262 = t263
    return retv262
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv265 int
    var t266 int = ref_get__Ref_3int(self__208)
    retv265 = t266
    return retv265
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int(self__137 *_goml_vec_int) int {
    var retv268 int
    var t269 int = vec_len__Vec_3int(self__137)
    retv268 = t269
    return retv268
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(self__126 *_goml_vec_int, elem__127 int) struct{} {
    vec_push__Vec_3int(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(self__132 *_goml_vec_int, index__133 int) int {
    var retv273 int
    var t274 int = vec_get__Vec_3int(self__132, index__133)
    retv273 = t274
    return retv273
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__137 *_goml_vec_int32) int {
    var retv278 int
    var t279 int = vec_len__Vec_5int32(self__137)
    retv278 = t279
    return retv278
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__126 *_goml_vec_int32, elem__127 int32) struct{} {
    vec_push__Vec_5int32(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__132 *_goml_vec_int32, index__133 int) int32 {
    var retv283 int32
    var t284 int32 = vec_get__Vec_5int32(self__132, index__133)
    retv283 = t284
    return retv283
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___l_int32_x3b_2_r_(self__137 *_goml_vec_Array_2_5int32) int {
    var retv286 int
    var t287 int = vec_len__Vec_14Array_2_5int32(self__137)
    retv286 = t287
    return retv286
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___l_int32_x3b_2_r_(self__126 *_goml_vec_Array_2_5int32, elem__127 [2]int32) struct{} {
    vec_push__Vec_14Array_2_5int32(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T___l_int32_x3b_2_r_(self__132 *_goml_vec_Array_2_5int32, index__133 int) [2]int32 {
    var retv291 [2]int32
    var t292 [2]int32 = vec_get__Vec_14Array_2_5int32(self__132, index__133)
    retv291 = t292
    return retv291
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__55 string, other__56 string) bool {
    var retv294 bool
    var t295 bool = self__55 == other__56
    retv294 = t295
    return retv294
}

func _goml_m_trait__impl_i_Hash_i_string_i_hash(self__83 string) uint64 {
    var retv297 uint64
    var t298 uint64 = _goml_runtime_core_string_hash(self__83)
    retv297 = t298
    return retv297
}

func main() {
    main0()
}
