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
        var x64 int32 = x__0.(Some)._0
        var v__1 int32 = x64
        println__T_int32(v__1)
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func main0() struct{} {
    var t131 [2]int = [2]int{31, 32}
    var t132 int = array_get__Array_2_3int(t131, 1)
    println__T_int(t132)
    var arr__2 [3]int = [3]int{1, 2, 3}
    var t133 int = array_get__Array_3_3int(arr__2, 0)
    println__T_int(t133)
    var arr2__3 [3]int = [3]int{4, 5, 6}
    var place_root67 [3]int = arr2__3
    var index68 int = 1
    array_get__Array_3_3int(place_root67, index68)
    var value70 int = 50
    var t134 [3]int = array_set__Array_3_3int(place_root67, index68, value70)
    arr2__3 = t134
    var t136 int = array_get__Array_3_3int(arr2__3, 1)
    println__T_int(t136)
    var t137 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int()
    var t138 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int(t137, 7)
    var t139 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int(t138, 8)
    var vec__4 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int(t139, 9)
    var t140 int = vec_get__Vec_3int(vec__4, 2)
    println__T_int(t140)
    var t141 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    var t142 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t141, 10)
    var t143 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t142, 11)
    var vec2__5 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t143, 12)
    var place_root74 *_goml_vec_int32 = vec2__5
    var index75 int = 0
    vec_get__Vec_5int32(place_root74, index75)
    var value77 int32 = 100
    vec_set__Vec_5int32(place_root74, index75, value77)
    var t145 int32 = vec_get__Vec_5int32(vec2__5, 0)
    println__T_int32(t145)
    var s__6 []int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(vec2__5, 0, 2)
    var t146 int32 = s__6[1]
    println__T_int32(t146)
    var map__7 *hashmap_string_int32_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32()
    var place_root81 *hashmap_string_int32_x = map__7
    var index82 string = "a"
    hashmap_get__HashMap_6string_5int32(place_root81, index82)
    var value84 int32 = 13
    hashmap_set__HashMap_6string_5int32(place_root81, index82, value84)
    var t148 Option__int32 = hashmap_get__HashMap_6string_5int32(map__7, "a")
    print_opt_int(t148)
    var t149 Option__int32 = hashmap_get__HashMap_6string_5int32(map__7, "missing")
    print_opt_int(t149)
    var t150 [2]int32 = [2]int32{1, 2}
    var t151 [2]int32 = [2]int32{3, 4}
    var matrix__8 [2][2]int32 = [2][2]int32{t150, t151}
    var place_root88 [2][2]int32 = matrix__8
    var index89 int = 1
    var place90 [2]int32 = array_get__Array_2_14Array_2_5int32(place_root88, index89)
    var index91 int = 0
    array_get__Array_2_5int32(place90, index91)
    var value93 int32 = 30
    var t152 [2]int32 = array_set__Array_2_5int32(place90, index91, value93)
    var t153 [2][2]int32 = array_set__Array_2_14Array_2_5int32(place_root88, index89, t152)
    matrix__8 = t153
    var t155 [2]int32 = array_get__Array_2_14Array_2_5int32(matrix__8, 1)
    var t156 int32 = array_get__Array_2_5int32(t155, 0)
    println__T_int32(t156)
    var t157 [2]int = [2]int{14, 15}
    var pair__9 Tuple2_11Array2_3int_3int = Tuple2_11Array2_3int_3int{
        _0: t157,
        _1: 0,
    }
    var place_root96 Tuple2_11Array2_3int_3int = pair__9
    var place97 [2]int = place_root96._0
    var index98 int = 1
    array_get__Array_2_3int(place97, index98)
    var value100 int = 150
    var t158 [2]int = array_set__Array_2_3int(place97, index98, value100)
    var t159 int = place_root96._1
    var t160 Tuple2_11Array2_3int_3int = Tuple2_11Array2_3int_3int{
        _0: t158,
        _1: t159,
    }
    pair__9 = t160
    var t162 [2]int = pair__9._0
    var t163 int = array_get__Array_2_3int(t162, 1)
    println__T_int(t163)
    var t164 [2]int32 = [2]int32{16, 17}
    var t165 *_goml_vec_Array_2_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___l_int32_x3b_2_r_()
    var t166 [2]int32 = [2]int32{18, 19}
    var t167 *_goml_vec_Array_2_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T___l_int32_x3b_2_r_(t165, t166)
    var t168 [2]int32 = [2]int32{20, 21}
    var t169 *_goml_vec_Array_2_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T___l_int32_x3b_2_r_(t167, t168)
    var holder__10 Holder = Holder{
        data: t164,
        vecs: t169,
    }
    var place_root103 Holder = holder__10
    var place104 [2]int32 = place_root103.data
    var index105 int = 0
    array_get__Array_2_5int32(place104, index105)
    var value107 int32 = 160
    var t170 [2]int32 = array_set__Array_2_5int32(place104, index105, value107)
    var t171 *_goml_vec_Array_2_5int32 = place_root103.vecs
    var t172 Holder = Holder{
        data: t170,
        vecs: t171,
    }
    holder__10 = t172
    var t174 [2]int32 = holder__10.data
    var t175 int32 = array_get__Array_2_5int32(t174, 0)
    println__T_int32(t175)
    var place_root110 Holder = holder__10
    var place111 *_goml_vec_Array_2_5int32 = place_root110.vecs
    var index112 int = 1
    var place113 [2]int32 = vec_get__Vec_14Array_2_5int32(place111, index112)
    var index114 int = 0
    array_get__Array_2_5int32(place113, index114)
    var value116 int32 = 200
    var t176 [2]int32 = array_set__Array_2_5int32(place113, index114, value116)
    vec_set__Vec_14Array_2_5int32(place111, index112, t176)
    var t178 *_goml_vec_Array_2_5int32 = holder__10.vecs
    var t179 [2]int32 = vec_get__Vec_14Array_2_5int32(t178, 1)
    var t180 int32 = array_get__Array_2_5int32(t179, 0)
    println__T_int32(t180)
    var t181 [2]int32 = [2]int32{22, 23}
    var r__11 *ref_Array_2_5int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T___l_int32_x3b_2_r_(t181)
    var place_ref119 *ref_Array_2_5int32_x = r__11
    var place_root120 [2]int32 = ref_get__Ref_14Array_2_5int32(place_ref119)
    var index121 int = 1
    array_get__Array_2_5int32(place_root120, index121)
    var value123 int32 = 230
    var t182 [2]int32 = array_set__Array_2_5int32(place_root120, index121, value123)
    ref_set__Ref_14Array_2_5int32(place_ref119, t182)
    var t184 [2]int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T___l_int32_x3b_2_r_(r__11)
    var t185 int32 = array_get__Array_2_5int32(t184, 1)
    println__T_int32(t185)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t187 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t187)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t190 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t190)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t193 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t193)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int() *_goml_vec_int {
    var retv196 *_goml_vec_int
    var t197 *_goml_vec_int = vec_new__Vec_3int()
    retv196 = t197
    return retv196
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int(self__130 *_goml_vec_int, elem__131 int) *_goml_vec_int {
    var retv199 *_goml_vec_int
    var result__132 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int()
    var index__133 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop201:
    for {
        var t202 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__133)
        var t203 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int(self__130)
        var t204 bool = t202 < t203
        if t204 {
            var t205 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__133)
            var t206 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(self__130, t205)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(result__132, t206)
            var t207 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__133)
            var t208 int = t207 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__133, t208)
            continue
        } else {
            break Loop_loop201
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(result__132, elem__131)
    retv199 = result__132
    return retv199
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32() *_goml_vec_int32 {
    var retv210 *_goml_vec_int32
    var t211 *_goml_vec_int32 = vec_new__Vec_5int32()
    retv210 = t211
    return retv210
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(self__130 *_goml_vec_int32, elem__131 int32) *_goml_vec_int32 {
    var retv213 *_goml_vec_int32
    var result__132 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int32()
    var index__133 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop215:
    for {
        var t216 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__133)
        var t217 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__130)
        var t218 bool = t216 < t217
        if t218 {
            var t219 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__133)
            var t220 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__130, t219)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(result__132, t220)
            var t221 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__133)
            var t222 int = t221 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__133, t222)
            continue
        } else {
            break Loop_loop215
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(result__132, elem__131)
    retv213 = result__132
    return retv213
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_slice____T__int32(self__177 *_goml_vec_int32, start__178 int, end__179 int) []int32 {
    var retv224 []int32
    var t225 []int32 = self__177.items[start__178:end__179]
    retv224 = t225
    return retv224
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32() *hashmap_string_int32_x {
    var retv227 *hashmap_string_int32_x
    var t228 *hashmap_string_int32_x = hashmap_new__HashMap_6string_5int32()
    retv227 = t228
    return retv227
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___l_int32_x3b_2_r_() *_goml_vec_Array_2_5int32 {
    var retv230 *_goml_vec_Array_2_5int32
    var t231 *_goml_vec_Array_2_5int32 = vec_new__Vec_14Array_2_5int32()
    retv230 = t231
    return retv230
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T___l_int32_x3b_2_r_(self__130 *_goml_vec_Array_2_5int32, elem__131 [2]int32) *_goml_vec_Array_2_5int32 {
    var retv233 *_goml_vec_Array_2_5int32
    var result__132 *_goml_vec_Array_2_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T___l_int32_x3b_2_r_()
    var index__133 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop235:
    for {
        var t236 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__133)
        var t237 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___l_int32_x3b_2_r_(self__130)
        var t238 bool = t236 < t237
        if t238 {
            var t239 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__133)
            var t240 [2]int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T___l_int32_x3b_2_r_(self__130, t239)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___l_int32_x3b_2_r_(result__132, t240)
            var t241 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__133)
            var t242 int = t241 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__133, t242)
            continue
        } else {
            break Loop_loop235
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___l_int32_x3b_2_r_(result__132, elem__131)
    retv233 = result__132
    return retv233
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T___l_int32_x3b_2_r_(value__209 [2]int32) *ref_Array_2_5int32_x {
    var retv244 *ref_Array_2_5int32_x
    var t245 *ref_Array_2_5int32_x = ref__Ref_14Array_2_5int32(value__209)
    retv244 = t245
    return retv244
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T___l_int32_x3b_2_r_(self__210 *ref_Array_2_5int32_x) [2]int32 {
    var retv247 [2]int32
    var t248 [2]int32 = ref_get__Ref_14Array_2_5int32(self__210)
    retv247 = t248
    return retv247
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv250 string
    retv250 = self__38
    return retv250
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv252 string
    var t253 string = _goml_runtime_core_int32_to_string(self__43)
    retv252 = t253
    return retv252
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv255 string
    var t256 string = _goml_runtime_core_int_to_string(self__40)
    retv255 = t256
    return retv255
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__209 int) *ref_int_x {
    var retv258 *ref_int_x
    var t259 *ref_int_x = ref__Ref_3int(value__209)
    retv258 = t259
    return retv258
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__210 *ref_int_x) int {
    var retv261 int
    var t262 int = ref_get__Ref_3int(self__210)
    retv261 = t262
    return retv261
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int(self__139 *_goml_vec_int) int {
    var retv264 int
    var t265 int = vec_len__Vec_3int(self__139)
    retv264 = t265
    return retv264
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(self__128 *_goml_vec_int, elem__129 int) struct{} {
    vec_push__Vec_3int(self__128, elem__129)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(self__134 *_goml_vec_int, index__135 int) int {
    var retv269 int
    var t270 int = vec_get__Vec_3int(self__134, index__135)
    retv269 = t270
    return retv269
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__211 *ref_int_x, value__212 int) struct{} {
    ref_set__Ref_3int(self__211, value__212)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__139 *_goml_vec_int32) int {
    var retv274 int
    var t275 int = vec_len__Vec_5int32(self__139)
    retv274 = t275
    return retv274
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__128 *_goml_vec_int32, elem__129 int32) struct{} {
    vec_push__Vec_5int32(self__128, elem__129)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__134 *_goml_vec_int32, index__135 int) int32 {
    var retv279 int32
    var t280 int32 = vec_get__Vec_5int32(self__134, index__135)
    retv279 = t280
    return retv279
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___l_int32_x3b_2_r_(self__139 *_goml_vec_Array_2_5int32) int {
    var retv282 int
    var t283 int = vec_len__Vec_14Array_2_5int32(self__139)
    retv282 = t283
    return retv282
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T___l_int32_x3b_2_r_(self__128 *_goml_vec_Array_2_5int32, elem__129 [2]int32) struct{} {
    vec_push__Vec_14Array_2_5int32(self__128, elem__129)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T___l_int32_x3b_2_r_(self__134 *_goml_vec_Array_2_5int32, index__135 int) [2]int32 {
    var retv287 [2]int32
    var t288 [2]int32 = vec_get__Vec_14Array_2_5int32(self__134, index__135)
    retv287 = t288
    return retv287
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__55 string, other__56 string) bool {
    var retv290 bool
    var t291 bool = self__55 == other__56
    retv290 = t291
    return retv290
}

func _goml_m_trait__impl_i_Hash_i_string_i_hash(self__83 string) uint64 {
    var retv293 uint64
    var t294 uint64 = _goml_runtime_core_string_hash(self__83)
    retv293 = t294
    return retv293
}

func main() {
    main0()
}
